{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module EVM.SymExec where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.Spawn (parMapIO, pool)
import Control.Concurrent.STM (atomically, TVar, readTVarIO, readTVar, newTVarIO, writeTVar)
import Control.Monad.Operational qualified as Operational
import Control.Monad.ST (RealWorld, stToIO, ST)
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.DoubleWord (Word256)
import Data.List (foldl', sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, isSubsetOf, size)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Tree.Zipper qualified as Zipper
import Data.Tuple (swap)
import EVM (makeVm, initialContract, getCodeLocation, isValidJumpDest)
import EVM.Exec
import EVM.Fetch qualified as Fetch
import EVM.ABI
import EVM.Expr qualified as Expr
import EVM.Format (formatExpr, formatPartial)
import EVM.SMT (SMTCex(..), SMT2(..), assertProps, formatSMT2)
import EVM.SMT qualified as SMT
import EVM.Solvers
import EVM.Stepper (Stepper)
import EVM.Stepper qualified as Stepper
import EVM.Traversals
import EVM.Types
import EVM.Concrete (createAddress)
import EVM.FeeSchedule qualified as FeeSchedule
import EVM.Format (indent, formatBinary)
import GHC.Conc (getNumProcessors)
import GHC.Generics (Generic)
import Optics.Core
import Options.Generic (ParseField, ParseFields, ParseRecord)
import Witch (into, unsafeInto)

-- | A method name, and the (ordered) types of it's arguments
data Sig = Sig Text [AbiType]

data LoopHeuristic
  = Naive
  | StackBased
  deriving (Eq, Show, Read, ParseField, ParseFields, ParseRecord, Generic)

data ProofResult a b c = Qed a | Cex b | Timeout c
  deriving (Show, Eq)
type VerifyResult = ProofResult () (Expr End, SMTCex) (Expr End)
type EquivResult = ProofResult () (SMTCex) ()

isTimeout :: ProofResult a b c -> Bool
isTimeout (Timeout _) = True
isTimeout _ = False

isCex :: ProofResult a b c -> Bool
isCex (Cex _) = True
isCex _ = False

isQed :: ProofResult a b c -> Bool
isQed (Qed _) = True
isQed _ = False

data VeriOpts = VeriOpts
  { simp :: Bool
  , debug :: Bool
  , maxIter :: Maybe Integer
  , askSmtIters :: Integer
  , loopHeuristic :: LoopHeuristic
  , rpcInfo :: Fetch.RpcInfo
  }
  deriving (Eq, Show)

defaultVeriOpts :: VeriOpts
defaultVeriOpts = VeriOpts
  { simp = True
  , debug = False
  , maxIter = Nothing
  , askSmtIters = 1
  , loopHeuristic = StackBased
  , rpcInfo = Nothing
  }

rpcVeriOpts :: (Fetch.BlockNumber, Text) -> VeriOpts
rpcVeriOpts info = defaultVeriOpts { rpcInfo = Just info }

debugVeriOpts :: VeriOpts
debugVeriOpts = defaultVeriOpts { debug = True }

extractCex :: VerifyResult -> Maybe (Expr End, SMTCex)
extractCex (Cex c) = Just c
extractCex _ = Nothing

bool :: Expr EWord -> Prop
bool e = POr (PEq e (Lit 1)) (PEq e (Lit 0))

-- | Abstract calldata argument generation
symAbiArg :: Text -> AbiType -> CalldataFragment
symAbiArg name = \case
  AbiUIntType n ->
    if n `mod` 8 == 0 && n <= 256
    then let v = Var name in St [Expr.inRange n v] v
    else internalError "bad type"
  AbiIntType n ->
    if n `mod` 8 == 0 && n <= 256
    -- TODO: is this correct?
    then let v = Var name in St [Expr.inRange n v] v
    else internalError "bad type"
  AbiBoolType -> let v = Var name in St [bool v] v
  AbiAddressType -> let v = Var name in St [Expr.inRange 160 v] v
  AbiBytesType n ->
    if n > 0 && n <= 32
    then let v = Var name in St [Expr.inRange (n * 8) v] v
    else internalError "bad type"
  AbiArrayType sz tp ->
    Comp $ fmap (\n -> symAbiArg (name <> n) tp) [T.pack (show n) | n <- [0..sz-1]]
  t -> internalError $ "TODO: symbolic abi encoding for " <> show t

data CalldataFragment
  = St [Prop] (Expr EWord)
  | Dy [Prop] (Expr EWord) (Expr Buf)
  | Comp [CalldataFragment]
  deriving (Show, Eq)

-- | Generates calldata matching given type signature, optionally specialized
-- with concrete arguments.
-- Any argument given as "<symbolic>" or omitted at the tail of the list are
-- kept symbolic.
symCalldata :: Text -> [AbiType] -> [String] -> Expr Buf -> (Expr Buf, [Prop])
symCalldata sig typesignature concreteArgs base =
  let
    args = concreteArgs <> replicate (length typesignature - length concreteArgs) "<symbolic>"
    mkArg :: AbiType -> String -> Int -> CalldataFragment
    mkArg typ "<symbolic>" n = symAbiArg (T.pack $ "arg" <> show n) typ
    mkArg typ arg _ =
      case makeAbiValue typ arg of
        AbiUInt _ w -> St [] . Lit . into $ w
        AbiInt _ w -> St [] . Lit . unsafeInto $ w
        AbiAddress w -> St [] . Lit . into $ w
        AbiBool w -> St [] . Lit $ if w then 1 else 0
        _ -> internalError "TODO"
    calldatas = zipWith3 mkArg typesignature args [1..]
    (cdBuf, props) = combineFragments calldatas base
    withSelector = writeSelector cdBuf sig
    sizeConstraints
      = (Expr.bufLength withSelector .>= cdLen calldatas)
      .&& (Expr.bufLength withSelector .< (Lit (2 ^ (64 :: Integer))))
  in (withSelector, sizeConstraints : props)

cdLen :: [CalldataFragment] -> Expr EWord
cdLen = go (Lit 4)
  where
    go acc = \case
      [] -> acc
      (hd:tl) -> case hd of
                   St _ _ -> go (Expr.add acc (Lit 32)) tl
                   _ -> internalError "unsupported"

writeSelector :: Expr Buf -> Text -> Expr Buf
writeSelector buf sig =
  writeSel (Lit 0) $ writeSel (Lit 1) $ writeSel (Lit 2) $ writeSel (Lit 3) buf
  where
    sel = ConcreteBuf $ selector sig
    writeSel idx = Expr.writeByte idx (Expr.readByte idx sel)

combineFragments :: [CalldataFragment] -> Expr Buf -> (Expr Buf, [Prop])
combineFragments fragments base = go (Lit 4) fragments (base, [])
  where
    go :: Expr EWord -> [CalldataFragment] -> (Expr Buf, [Prop]) -> (Expr Buf, [Prop])
    go _ [] acc = acc
    go idx (f:rest) (buf, ps) =
      case f of
        St p w -> go (Expr.add idx (Lit 32)) rest (Expr.writeWord idx w buf, p <> ps)
        s -> internalError $ "unsupported cd fragment: " <> show s


abstractVM
  :: (Expr Buf, [Prop])
  -> ByteString
  -> Maybe (Precondition s)
  -> Expr Storage
  -> Bool
  -> ST s (VM s)
abstractVM cd contractCode maybepre store create = do
  vm' <- loadSymVM code store caller value (if create then mempty else cd) create
  let precond = case maybepre of
                Nothing -> []
                Just p -> [p vm']
  pure $ vm' & over #constraints (<> precond)
  where
    caller = Caller 0
    value = CallValue 0
    code = if create then InitCode contractCode mempty
           else RuntimeCode (ConcreteRuntimeCode contractCode)

loadSymVM
  :: ContractCode
  -> Expr Storage
  -> Expr EWord
  -> Expr EWord
  -> (Expr Buf, [Prop])
  -> Bool
  -> ST s (VM s)
loadSymVM x initStore addr callvalue cd create =
  (makeVm $ VMOpts
    { contract = initialContract x
    , calldata = cd
    , value = callvalue
    , initialStorage = initStore
    , address = createAddress ethrunAddress 1
    , caller = addr
    , origin = ethrunAddress --todo: generalize
    , coinbase = 0
    , number = 0
    , timestamp = Lit 0
    , blockGaslimit = 0
    , gasprice = 0
    , prevRandao = 42069
    , gas = 0xffffffffffffffff
    , gaslimit = 0xffffffffffffffff
    , baseFee = 0
    , priorityFee = 0
    , maxCodeSize = 0xffffffff
    , schedule = FeeSchedule.berlin
    , chainId = 1
    , create = create
    , txAccessList = mempty
    , allowFFI = False
    }) <&> set (#env % #contracts % at (createAddress ethrunAddress 1))
             (Just (initialContract x))

-- | Interpreter which explores all paths at branching points. Returns an
-- 'Expr End' representing the possible executions.
interpret
  :: Fetch.Fetcher RealWorld
  -> Maybe Integer -- max iterations
  -> Integer -- ask smt iterations
  -> LoopHeuristic
  -> VM RealWorld
  -> Stepper RealWorld (Expr End)
  -> IO (Expr End)
interpret fetcher maxIter askSmtIters heuristic vm =
  eval . Operational.view
  where
  eval
    :: Operational.ProgramView (Stepper.Action RealWorld) (Expr End)
    -> IO (Expr End)

  eval (Operational.Return x) = pure x

  eval (action Operational.:>>= k) =
    case action of
      Stepper.Exec -> do
        (r, vm') <- stToIO $ runStateT exec vm
        interpret fetcher maxIter askSmtIters heuristic vm' (k r)
      Stepper.IOAct q -> do
        r <- q
        interpret fetcher maxIter askSmtIters heuristic vm (k r)
      Stepper.Ask (PleaseChoosePath cond continue) -> do
        (a, b) <- concurrently
          (do
            (ra, vma) <- stToIO $ runStateT (continue True) vm { result = Nothing }
            interpret fetcher maxIter askSmtIters heuristic vma (k ra)
          )
          (do
            (rb, vmb) <- stToIO $ runStateT (continue False) vm { result = Nothing }
            interpret fetcher maxIter askSmtIters heuristic vmb (k rb)
          )
        pure $ ITE cond a b
      Stepper.Wait q -> do
        let performQuery = do
              m <- liftIO (fetcher q)
              (r, vm') <- stToIO $ runStateT m vm
              interpret fetcher maxIter askSmtIters heuristic vm' (k r)

        case q of
          PleaseAskSMT cond _ continue -> do
            case cond of
              -- is the condition concrete?
              Lit c ->
                -- have we reached max iterations, are we inside a loop?
                case (maxIterationsReached vm maxIter, isLoopHead heuristic vm) of
                  -- Yes. return a partial leaf
                  (Just _, Just True) ->
                    pure $ Partial vm.keccakEqs (Traces (Zipper.toForest vm.traces) vm.env.contracts) $ MaxIterationsReached vm.state.pc vm.state.contract
                  -- No. keep executing
                  _ -> do
                    (r, vm') <- stToIO $ runStateT (continue (Case (c > 0))) vm
                    interpret fetcher maxIter askSmtIters heuristic vm' (k r)

              -- the condition is symbolic
              _ ->
                -- are in we a loop, have we hit maxIters, have we hit askSmtIters?
                case (isLoopHead heuristic vm, askSmtItersReached vm askSmtIters, maxIterationsReached vm maxIter) of
                  -- we're in a loop and maxIters has been reached
                  (Just True, _, Just n) -> do
                    -- continue execution down the opposite branch than the one that
                    -- got us to this point and return a partial leaf for the other side
                    (r, vm') <- stToIO $ runStateT (continue (Case $ not n)) vm
                    a <- interpret fetcher maxIter askSmtIters heuristic vm' (k r)
                    pure $ ITE cond a (Partial vm.keccakEqs (Traces (Zipper.toForest vm.traces) vm.env.contracts) (MaxIterationsReached vm.state.pc vm.state.contract))
                  -- we're in a loop and askSmtIters has been reached
                  (Just True, True, _) ->
                    -- ask the smt solver about the loop condition
                    performQuery
                  -- otherwise just try both branches and don't ask the solver
                  _ -> do
                    (r, vm') <- stToIO $ runStateT (continue EVM.Types.Unknown) vm
                    interpret fetcher maxIter askSmtIters heuristic vm' (k r)

          _ -> performQuery

      Stepper.EVM m -> do
        (r, vm') <- stToIO $ runStateT m vm
        interpret fetcher maxIter askSmtIters heuristic vm' (k r)

maxIterationsReached :: VM s -> Maybe Integer -> Maybe Bool
maxIterationsReached _ Nothing = Nothing
maxIterationsReached vm (Just maxIter) =
  let codelocation = getCodeLocation vm
      (iters, _) = view (at codelocation % non (0, [])) vm.iterations
  in if unsafeInto maxIter <= iters
     then Map.lookup (codelocation, iters - 1) vm.cache.path
     else Nothing

askSmtItersReached :: VM s -> Integer -> Bool
askSmtItersReached vm askSmtIters = let
    codelocation = getCodeLocation vm
    (iters, _) = view (at codelocation % non (0, [])) vm.iterations
  in askSmtIters <= into iters

{- | Loop head detection heuristic

 The main thing we wish to differentiate between, are actual loop heads, and branch points inside of internal functions that are called multiple times.

 One way to do this is to observe that for internal functions, the compiler must always store a stack item representing the location that it must jump back to. If we compare the stack at the time of the previous visit, and the time of the current visit, and notice that this location has changed, then we can guess that the location is a jump point within an internal function instead of a loop (where such locations should be constant between iterations).

 This heuristic is not perfect, and can certainly be tricked, but should generally be good enough for most compiler generated and non pathological user generated loops.
 -}
isLoopHead :: LoopHeuristic -> VM s -> Maybe Bool
isLoopHead Naive _ = Just True
isLoopHead StackBased vm = let
    loc = getCodeLocation vm
    oldIters = Map.lookup loc vm.iterations
    isValid (Lit wrd) = wrd <= unsafeInto (maxBound :: Int) && isValidJumpDest vm (unsafeInto wrd)
    isValid _ = False
  in case oldIters of
       Just (_, oldStack) -> Just $ filter isValid oldStack == filter isValid vm.state.stack
       Nothing -> Nothing

type Precondition s = VM s -> Prop
type Postcondition s = VM s -> Expr End -> Prop

checkAssert
  :: SolverGroup
  -> [Word256]
  -> ByteString
  -> Maybe Sig
  -> [String]
  -> VeriOpts
  -> IO (Expr End, [VerifyResult])
checkAssert solvers errs c signature' concreteArgs opts =
  verifyContract solvers c signature' concreteArgs opts AbstractStore Nothing (Just $ checkAssertions errs)

{- | Checks if an assertion violation has been encountered

  hevm recognises the following as an assertion violation:

  1. the invalid opcode (0xfe) (solc < 0.8)
  2. a revert with a reason of the form `abi.encodeWithSelector("Panic(uint256)", code)`, where code is one of the following (solc >= 0.8):
    - 0x00: Used for generic compiler inserted panics.
    - 0x01: If you call assert with an argument that evaluates to false.
    - 0x11: If an arithmetic operation results in underflow or overflow outside of an unchecked { ... } block.
    - 0x12; If you divide or modulo by zero (e.g. 5 / 0 or 23 % 0).
    - 0x21: If you convert a value that is too big or negative into an enum type.
    - 0x22: If you access a storage byte array that is incorrectly encoded.
    - 0x31: If you call .pop() on an empty array.
    - 0x32: If you access an array, bytesN or an array slice at an out-of-bounds or negative index (i.e. x[i] where i >= x.length or i < 0).
    - 0x41: If you allocate too much memory or create an array that is too large.
    - 0x51: If you call a zero-initialized variable of internal function type.

  see: https://docs.soliditylang.org/en/v0.8.6/control-structures.html?highlight=Panic#panic-via-assert-and-error-via-require
-}
checkAssertions :: [Word256] -> Postcondition s
checkAssertions errs _ = \case
  Failure _ _ (Revert (ConcreteBuf msg)) -> PBool $ msg `notElem` (fmap panicMsg errs)
  Failure _ _ (Revert b) -> foldl' PAnd (PBool True) (fmap (PNeg . PEq b . ConcreteBuf . panicMsg) errs)
  _ -> PBool True

-- | By default hevm only checks for user-defined assertions
defaultPanicCodes :: [Word256]
defaultPanicCodes = [0x01]

allPanicCodes :: [Word256]
allPanicCodes = [0x00, 0x01, 0x11, 0x12, 0x21, 0x22, 0x31, 0x32, 0x41, 0x51]

-- | Produces the revert message for solc >=0.8 assertion violations
panicMsg :: Word256 -> ByteString
panicMsg err = selector "Panic(uint256)" <> encodeAbiValue (AbiUInt 256 err)

-- | Builds a buffer representing calldata from the provided method description
-- and concrete arguments
mkCalldata :: Maybe Sig -> [String] -> (Expr Buf, [Prop])
mkCalldata Nothing _ =
  ( AbstractBuf "txdata"
  -- assert that the length of the calldata is never more than 2^64
  -- this is way larger than would ever be allowed by the gas limit
  -- and avoids spurious counterexamples during abi decoding
  -- TODO: can we encode calldata as an array with a smaller length?
  , [Expr.bufLength (AbstractBuf "txdata") .< (Lit (2 ^ (64 :: Integer)))]
  )
mkCalldata (Just (Sig name types)) args =
  symCalldata name types args (AbstractBuf "txdata")

verifyContract
  :: SolverGroup
  -> ByteString
  -> Maybe Sig
  -> [String]
  -> VeriOpts
  -> Expr Storage
  -> Maybe (Precondition RealWorld)
  -> Maybe (Postcondition RealWorld)
  -> IO (Expr End, [VerifyResult])
verifyContract solvers theCode signature' concreteArgs opts initStore maybepre maybepost = do
  preState <- stToIO $ abstractVM (mkCalldata signature' concreteArgs) theCode maybepre initStore False
  verify solvers opts preState maybepost

-- | Stepper that parses the result of Stepper.runFully into an Expr End
runExpr :: Stepper.Stepper RealWorld (Expr End)
runExpr = do
  vm <- Stepper.runFully
  let asserts = vm.keccakEqs <> vm.constraints
  pure $ case vm.result of
    Just (VMSuccess buf) -> Success asserts (Traces (Zipper.toForest vm.traces) vm.env.contracts) buf vm.env.storage
    Just (VMFailure e) -> Failure asserts (Traces (Zipper.toForest vm.traces) vm.env.contracts) e
    Just (Unfinished p) -> Partial asserts (Traces (Zipper.toForest vm.traces) vm.env.contracts) p
    _ -> internalError "vm in intermediate state after call to runFully"

-- | Converts a given top level expr into a list of final states and the
-- associated path conditions for each state.
flattenExpr :: Expr End -> [Expr End]
flattenExpr = go []
  where
    go :: [Prop] -> Expr End -> [Expr End]
    go pcs = \case
      ITE c t f -> go (PNeg ((PEq c (Lit 0))) : pcs) t <> go (PEq c (Lit 0) : pcs) f
      Success ps trace msg store -> [Success (ps <> pcs) trace msg store]
      Failure ps trace e -> [Failure (ps <> pcs) trace e]
      Partial ps trace p -> [Partial (ps <> pcs) trace p]
      GVar _ -> internalError "cannot flatten an Expr containing a GVar"

-- | Strips unreachable branches from a given expr
-- Returns a list of executed SMT queries alongside the reduced expression for debugging purposes
-- Note that the reduced expression loses information relative to the original
-- one if jump conditions are removed. This restriction can be removed once
-- Expr supports attaching knowledge to AST nodes.
-- Although this algorithm currently parallelizes nicely, it does not exploit
-- the incremental nature of the task at hand. Introducing support for
-- incremental queries might let us go even faster here.
-- TODO: handle errors properly
reachable :: SolverGroup -> Expr End -> IO ([SMT2], Expr End)
reachable solvers e = do
  res <- go [] e
  pure $ second (fromMaybe (internalError "no reachable paths found")) res
  where
    {-
       Walk down the tree and collect pcs.
       Dispatch a reachability query at each leaf.
       If reachable return the expr wrapped in a Just. If not return Nothing.
       When walking back up the tree drop unreachable subbranches.
    -}
    go :: [Prop] -> Expr End -> IO ([SMT2], Maybe (Expr End))
    go pcs = \case
      ITE c t f -> do
        (tres, fres) <- concurrently
          (go (PEq (Lit 1) c : pcs) t)
          (go (PEq (Lit 0) c : pcs) f)
        let subexpr = case (snd tres, snd fres) of
              (Just t', Just f') -> Just $ ITE c t' f'
              (Just t', Nothing) -> Just t'
              (Nothing, Just f') -> Just f'
              (Nothing, Nothing) -> Nothing
        pure (fst tres <> fst fres, subexpr)
      leaf -> do
        let query = assertProps pcs
        res <- checkSat solvers query
        case res of
          Sat _ -> pure ([query], Just leaf)
          Unsat -> pure ([query], Nothing)
          r -> internalError $ "Invalid solver result: " <> show r

-- | Extract contraints stored in Expr End nodes
extractProps :: Expr End -> [Prop]
extractProps = \case
  ITE _ _ _ -> []
  Success asserts _ _ _ -> asserts
  Failure asserts _ _ -> asserts
  Partial asserts _ _ -> asserts
  GVar _ -> internalError "cannot extract props from a GVar"

isPartial :: Expr a -> Bool
isPartial (Partial _ _ _) = True
isPartial _ = False

getPartials :: [Expr End] -> [PartialExec]
getPartials = mapMaybe go
  where
    go :: Expr End -> Maybe PartialExec
    go = \case
      Partial _ _ p -> Just p
      _ -> Nothing

-- | Symbolically execute the VM and check all endstates against the
-- postcondition, if available.
verify
  :: SolverGroup
  -> VeriOpts
  -> VM RealWorld
  -> Maybe (Postcondition RealWorld)
  -> IO (Expr End, [VerifyResult])
verify solvers opts preState maybepost = do
  putStrLn "Exploring contract"

  exprInter <- interpret (Fetch.oracle solvers opts.rpcInfo) opts.maxIter opts.askSmtIters opts.loopHeuristic preState runExpr
  when opts.debug $ T.writeFile "unsimplified.expr" (formatExpr exprInter)

  putStrLn "Simplifying expression"
  expr <- if opts.simp then (pure $ Expr.simplify exprInter) else pure exprInter
  when opts.debug $ T.writeFile "simplified.expr" (formatExpr expr)

  putStrLn $ "Explored contract (" <> show (Expr.numBranches expr) <> " branches)"

  let flattened = flattenExpr expr
  when (any isPartial flattened) $ do
    T.putStrLn ""
    T.putStrLn "WARNING: hevm was only able to partially explore the given contract due to the following issues:"
    T.putStrLn ""
    T.putStrLn . T.unlines . fmap (indent 2 . ("- " <>)) . fmap formatPartial . getPartials $ flattened

  case maybepost of
    Nothing -> pure (expr, [Qed ()])
    Just post -> do
      let
        -- Filter out any leaves that can be statically shown to be safe
        canViolate = flip filter flattened $
          \leaf -> case Expr.evalProp (post preState leaf) of
            PBool True -> False
            _ -> True
        assumes = preState.constraints
        withQueries = canViolate <&> \leaf ->
          (assertProps (PNeg (post preState leaf) : assumes <> extractProps leaf), leaf)
      putStrLn $ "Checking for reachability of "
                   <> show (length withQueries)
                   <> " potential property violation(s)"

      when opts.debug $ forM_ (zip [(1 :: Int)..] withQueries) $ \(idx, (q, leaf)) -> do
        TL.writeFile
          ("query-" <> show idx <> ".smt2")
          ("; " <> (TL.pack $ show leaf) <> "\n\n" <> formatSMT2 q <> "\n\n(check-sat)")

      -- Dispatch the remaining branches to the solver to check for violations
      results <- flip mapConcurrently withQueries $ \(query, leaf) -> do
        res <- checkSat solvers query
        pure (res, leaf)
      let cexs = filter (\(res, _) -> not . isUnsat $ res) results
      pure $ if Prelude.null cexs then (expr, [Qed ()]) else (expr, fmap toVRes cexs)
  where
    toVRes :: (CheckSatResult, Expr End) -> VerifyResult
    toVRes (res, leaf) = case res of
      Sat model -> Cex (leaf, model)
      EVM.Solvers.Unknown -> Timeout leaf
      Unsat -> Qed ()
      Error e -> internalError $ "solver responded with error: " <> show e

type UnsatCache = TVar [Set Prop]

-- | Compares two contract runtimes for trace equivalence by running two VMs
-- and comparing the end states.
--
-- We do this by asking the solver to find a common input for each pair of
-- endstates that satisfies the path conditions for both sides and produces a
-- differing output. If we can find such an input, then we have a clear
-- equivalence break, and since we run this check for every pair of end states,
-- the check is exhaustive.
equivalenceCheck
  :: SolverGroup -> ByteString -> ByteString -> VeriOpts -> (Expr Buf, [Prop])
  -> IO [EquivResult]
equivalenceCheck solvers bytecodeA bytecodeB opts calldata = do
  case bytecodeA == bytecodeB of
    True -> do
      putStrLn "bytecodeA and bytecodeB are identical"
      pure [Qed ()]
    False -> do
      branchesA <- getBranches bytecodeA
      branchesB <- getBranches bytecodeB
      equivalenceCheck' solvers branchesA branchesB opts
  where
    -- decompiles the given bytecode into a list of branches
    getBranches :: ByteString -> IO [Expr End]
    getBranches bs = do
      let bytecode = if BS.null bs then BS.pack [0] else bs
      prestate <- stToIO $ abstractVM calldata bytecode Nothing AbstractStore False
      expr <- interpret (Fetch.oracle solvers Nothing) opts.maxIter opts.askSmtIters opts.loopHeuristic prestate runExpr
      let simpl = if opts.simp then (Expr.simplify expr) else expr
      pure $ flattenExpr simpl


equivalenceCheck' :: SolverGroup -> [Expr End] -> [Expr End] -> VeriOpts -> IO [EquivResult]
equivalenceCheck' solvers branchesA branchesB opts = do
      when (any isPartial branchesA || any isPartial branchesB) $ do
        putStrLn ""
        putStrLn "WARNING: hevm was only able to partially explore the given contract due to the following issues:"
        putStrLn ""
        T.putStrLn . T.unlines . fmap (indent 2 . ("- " <>)) . fmap formatPartial . nubOrd $ ((getPartials branchesA) <> (getPartials branchesB))

      let allPairs = [(a,b) | a <- branchesA, b <- branchesB]
      putStrLn $ "Found " <> show (length allPairs) <> " total pairs of endstates"

      when opts.debug $
        putStrLn $ "endstates in bytecodeA: " <> show (length branchesA)
                   <> "\nendstates in bytecodeB: " <> show (length branchesB)

      let differingEndStates = sortBySize (mapMaybe (uncurry distinct) allPairs)
      putStrLn $ "Asking the SMT solver for " <> (show $ length differingEndStates) <> " pairs"
      when opts.debug $ forM_ (zip differingEndStates [(1::Integer)..]) (\(x, i) ->
        T.writeFile ("prop-checked-" <> show i) (T.pack $ show x))

      knownUnsat <- newTVarIO []
      procs <- getNumProcessors
      results <- checkAll differingEndStates knownUnsat procs

      let useful = foldr (\(_, b) n -> if b then n+1 else n) (0::Integer) results
      putStrLn $ "Reuse of previous queries was Useful in " <> (show useful) <> " cases"
      case all isQed . fmap fst $ results of
        True -> pure [Qed ()]
        False -> pure $ filter (/= Qed ()) . fmap fst $ results
  where
    -- we order the sets by size because this gives us more cache hits when
    -- running our queries later on (since we rely on a subset check)
    sortBySize :: [Set a] -> [Set a]
    sortBySize = sortBy (\a b -> if size a > size b then Prelude.LT else Prelude.GT)

    -- returns True if a is a subset of any of the sets in b
    subsetAny :: Set Prop -> [Set Prop] -> Bool
    subsetAny a b = foldr (\bp acc -> acc || isSubsetOf a bp) False b

    -- checks for satisfiability of all the props in the provided set. skips
    -- the solver if we can determine unsatisfiability from the cache already
    -- the last element of the returned tuple indicates whether the cache was
    -- used or not
    check :: UnsatCache -> (Set Prop) -> Int -> IO (EquivResult, Bool)
    check knownUnsat props idx = do
      let smt = assertProps $ Set.toList props
      -- if debug is on, write the query to a file
      when opts.debug $ TL.writeFile
        ("equiv-query-" <> show idx <> ".smt2") (formatSMT2 smt <> "\n\n(check-sat)")

      ku <- readTVarIO knownUnsat
      res <- if subsetAny props ku
             then pure (True, Unsat)
             else (fmap ((False),) (checkSat solvers smt))
      case res of
        (_, Sat x) -> pure (Cex x, False)
        (quick, Unsat) ->
          case quick of
            True  -> pure (Qed (), quick)
            False -> do
              -- nb: we might end up with duplicates here due to a
              -- potential race, but it doesn't matter for correctness
              atomically $ readTVar knownUnsat >>= writeTVar knownUnsat . (props :)
              pure (Qed (), False)
        (_, EVM.Solvers.Unknown) -> pure (Timeout (), False)
        (_, Error txt) -> internalError $ "issue while running solver: `" <> T.unpack txt -- <> "` SMT file was: `" <> filename <> "`"

    -- Allows us to run it in parallel. Note that this (seems to) run it
    -- from left-to-right, and with a max of K threads. This is in contrast to
    -- mapConcurrently which would spawn as many threads as there are jobs, and
    -- run them in a random order. We ordered them correctly, though so that'd be bad
    checkAll :: [(Set Prop)] -> UnsatCache -> Int -> IO [(EquivResult, Bool)]
    checkAll input cache numproc = do
       wrap <- pool numproc
       parMapIO (wrap . (uncurry $ check cache)) $ zip input [1..]


    -- Takes two branches and returns a set of props that will need to be
    -- satisfied for the two branches to violate the equivalence check. i.e.
    -- for a given pair of branches, equivalence is violated if there exists an
    -- input that satisfies the branch conditions from both sides and produces
    -- a differing result in each branch
    distinct :: Expr End -> Expr End -> Maybe (Set Prop)
    distinct aEnd bEnd =
      let
        differingResults = case (aEnd, bEnd) of
          (Success _ _ aOut aStore, Success _ _ bOut bStore) ->
            if aOut == bOut && aStore == bStore
            then PBool False
            else aStore ./= bStore .|| aOut ./= bOut
          (Failure _ _ (Revert a), Failure _ _ (Revert b)) -> if a == b then PBool False else a ./= b
          (Failure _ _ a, Failure _ _ b) -> if a == b then PBool False else PBool True
          -- partial end states can't be compared to actual end states, so we always ignore them
          (Partial {}, _) -> PBool False
          (_, Partial {}) -> PBool False
          (ITE _ _ _, _) -> internalError "Expressions must be flattened"
          (_, ITE _ _ _) -> internalError "Expressions must be flattened"
          (a, b) -> if a == b
                    then PBool False
                    else PBool True
      in case differingResults of
        -- if the end states are the same, then they can never produce a
        -- different result under any circumstances
        PBool False -> Nothing
        -- if we can statically determine that the end states differ, then we
        -- ask the solver to find us inputs that satisfy both sets of branch
        -- conditions
        PBool True  -> Just . Set.fromList $ extractProps aEnd <> extractProps bEnd
        -- if we cannot statically determine whether or not the end states
        -- differ, then we ask the solver if the end states can differ if both
        -- sets of path conditions are satisfiable
        _ -> Just . Set.fromList $ differingResults : extractProps aEnd <> extractProps bEnd

both' :: (a -> b) -> (a, a) -> (b, b)
both' f (x, y) = (f x, f y)

produceModels :: SolverGroup -> Expr End -> IO [(Expr End, CheckSatResult)]
produceModels solvers expr = do
  let flattened = flattenExpr expr
      withQueries = fmap (\e -> (assertProps . extractProps $ e, e)) flattened
  results <- flip mapConcurrently withQueries $ \(query, leaf) -> do
    res <- checkSat solvers query
    pure (res, leaf)
  pure $ fmap swap $ filter (\(res, _) -> not . isUnsat $ res) results

showModel :: Expr Buf -> (Expr End, CheckSatResult) -> IO ()
showModel cd (expr, res) = do
  case res of
    Unsat -> pure () -- ignore unreachable branches
    Error e -> internalError $ "smt solver returned an error: " <> show e
    EVM.Solvers.Unknown -> do
      putStrLn "--- Branch ---"
      putStrLn ""
      putStrLn "Unable to produce a model for the following end state:"
      putStrLn ""
      T.putStrLn $ indent 2 $ formatExpr expr
      putStrLn ""
    Sat cex -> do
      putStrLn "--- Branch ---"
      putStrLn ""
      putStrLn "Inputs:"
      putStrLn ""
      T.putStrLn $ indent 2 $ formatCex cd cex
      putStrLn ""
      putStrLn "End State:"
      putStrLn ""
      T.putStrLn $ indent 2 $ formatExpr expr
      putStrLn ""


formatCex :: Expr Buf -> SMTCex -> Text
formatCex cd m@(SMTCex _ _ store blockContext txContext) = T.unlines $
  [ "Calldata:"
  , indent 2 cd'
  , ""
  ]
  <> storeCex
  <> txCtx
  <> blockCtx
  where
    -- we attempt to produce a model for calldata by substituting all variables
    -- and buffers provided by the model into the original calldata expression.
    -- If we have a concrete result then we diplay it, otherwise we diplay
    -- `Any`. This is a little bit of a hack (and maybe unsound?), but we need
    -- it for branches that do not refer to calldata at all (e.g. the top level
    -- callvalue check inserted by solidity in contracts that don't have any
    -- payable functions).
    cd' = prettyBuf . Expr.simplify . defaultSymbolicValues $ subModel m cd

    storeCex :: [Text]
    storeCex
      | Map.null store = []
      | otherwise =
          [ "Storage:"
          , indent 2 $ T.unlines $ Map.foldrWithKey (\key val acc ->
              ("Addr " <> (T.pack $ show (unsafeInto key :: Addr))
                <> ": " <> (T.pack $ show (Map.toList val))) : acc
            ) mempty store
          , ""
          ]

    txCtx :: [Text]
    txCtx
      | Map.null txContext = []
      | otherwise =
        [ "Transaction Context:"
        , indent 2 $ T.unlines $ Map.foldrWithKey (\key val acc ->
            (showTxCtx key <> ": " <> (T.pack $ show val)) : acc
          ) mempty (filterSubCtx txContext)
        , ""
        ]

    -- strips the frame arg from frame context vars to make them easier to read
    showTxCtx :: Expr EWord -> Text
    showTxCtx (CallValue _) = "CallValue"
    showTxCtx (Caller _) = "Caller"
    showTxCtx (Address _) = "Address"
    showTxCtx x = T.pack $ show x

    -- strips all frame context that doesn't come from the top frame
    filterSubCtx :: Map (Expr EWord) W256 -> Map (Expr EWord) W256
    filterSubCtx = Map.filterWithKey go
      where
        go :: Expr EWord -> W256 -> Bool
        go (CallValue x) _ = x == 0
        go (Caller x) _ = x == 0
        go (Address x) _ = x == 0
        go (Balance {}) _ = internalError "TODO: BALANCE"
        go (SelfBalance {}) _ = internalError "TODO: SELFBALANCE"
        go (Gas {}) _ = internalError "TODO: Gas"
        go _ _ = False

    blockCtx :: [Text]
    blockCtx
      | Map.null blockContext = []
      | otherwise =
        [ "Block Context:"
        , indent 2 $ T.unlines $ Map.foldrWithKey (\key val acc ->
            (T.pack $ show key <> ": " <> show val) : acc
          ) mempty txContext
        , ""
        ]

    prettyBuf :: Expr Buf -> Text
    prettyBuf (ConcreteBuf "") = "Empty"
    prettyBuf (ConcreteBuf bs) = formatBinary bs
    prettyBuf b = internalError $ "Unexpected symbolic buffer:\n" <> T.unpack (formatExpr b)

-- | If the expression contains any symbolic values, default them to some
-- concrete value The intuition here is that if we still have symbolic values
-- in our calldata expression after substituing in our cex, then they can have
-- any value and we can safely pick a random value. This is a bit unsatisfying,
-- we should really be doing smth like: https://github.com/ethereum/hevm/issues/334
-- but it's probably good enough for now
defaultSymbolicValues :: Expr a -> Expr a
defaultSymbolicValues e = subBufs (foldTerm symbufs mempty e)
                        . subVars (foldTerm symwords mempty e) $ e
  where
    symbufs :: Expr a -> Map (Expr Buf) ByteString
    symbufs = \case
      a@(AbstractBuf _) -> Map.singleton a ""
      _ -> mempty
    symwords :: Expr a -> Map (Expr EWord) W256
    symwords = \case
      a@(Var _) -> Map.singleton a 0
      a@Origin -> Map.singleton a 0
      a@Coinbase -> Map.singleton a 0
      a@Timestamp -> Map.singleton a 0
      a@BlockNumber -> Map.singleton a 0
      a@PrevRandao -> Map.singleton a 0
      a@GasLimit -> Map.singleton a 0
      a@ChainId -> Map.singleton a 0
      a@BaseFee -> Map.singleton a 0
      _ -> mempty

-- | Takes an expression and a Cex and replaces all abstract values in the buf with
-- concrete ones from the Cex.
subModel :: SMTCex -> Expr a -> Expr a
subModel c
  = subBufs (fmap forceFlattened c.buffers)
  . subStore c.store
  . subVars c.vars
  . subVars c.blockContext
  . subVars c.txContext
  where
    forceFlattened (SMT.Flat bs) = bs
    forceFlattened b@(SMT.Comp _) = forceFlattened $
      fromMaybe (internalError $ "cannot flatten buffer: " <> show b)
                (SMT.collapse b)

subVars :: Map (Expr EWord) W256 -> Expr a -> Expr a
subVars model b = Map.foldlWithKey subVar b model
  where
    subVar :: Expr a -> Expr EWord -> W256 -> Expr a
    subVar a var val = mapExpr go a
      where
        go :: Expr a -> Expr a
        go = \case
          v@(Var _) -> if v == var
                      then Lit val
                      else v
          e -> e

subBufs :: Map (Expr Buf) ByteString -> Expr a -> Expr a
subBufs model b = Map.foldlWithKey subBuf b model
  where
    subBuf :: Expr a -> Expr Buf -> ByteString -> Expr a
    subBuf x var val = mapExpr go x
      where
        go :: Expr a -> Expr a
        go = \case
          a@(AbstractBuf _) -> if a == var
                      then ConcreteBuf val
                      else a
          e -> e

subStore :: Map W256 (Map W256 W256) -> Expr a -> Expr a
subStore model b = mapExpr go b
  where
    go :: Expr a -> Expr a
    go = \case
      AbstractStore -> ConcreteStore model
      e -> e
