module Main where

import GHC.Natural
import Control.Monad
import System.Environment (getEnv)

import qualified Paths_hevm as Paths

import Test.Tasty (localOption, withResource)
import Test.Tasty.Bench
import Data.ByteString (ByteString)
import System.FilePath.Posix
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.FilePath.Find as Find
import qualified Data.ByteString.Lazy as LazyByteString

import EVM.SymExec
import EVM.Solidity
import EVM.Solvers
import EVM.Dapp
import EVM.Types
import EVM.Format
import qualified EVM.TTY as TTY
import qualified EVM.Stepper as Stepper
import qualified EVM.Fetch as Fetch

import EVM.Test.BlockchainTests qualified as BCTests
import qualified EVM.Transaction
import qualified EVM.Expr as Expr
import qualified EVM.FeeSchedule as FeeSchedule
import EVM (initialContract, makeVm)
import qualified Data.ByteString as ByteString
import Bytecodes

main :: IO ()
main = defaultMain
  [ mkbench erc20 "erc20" 0 [1]
  , mkbench (pure vat) "vat" 0 [4]
  , mkbench (pure deposit) "deposit" 32 [4]
  , mkbench (pure uniV2Pair) "uniV2" 10 [4]
  , withResource bcjsons (pure . const ()) blockchainTests
  ]

vmFromByteString :: ByteString -> VM
vmFromByteString bs = EVM.Transaction.initTx vm0
  where
    c = initialContract (RuntimeCode (ConcreteRuntimeCode (hexByteString "bytes" bs)))
    vm0 = makeVm $ VMOpts
      { contract      = c
      , calldata      = (ConcreteBuf "", [])
      , value         = Lit 0
      , address       = 0xacab
      , caller        = Expr.litAddr 0
      , origin        = 0
      , gas           = 0xffffffffffffffff
      , baseFee       = 0
      , priorityFee   = 0
      , gaslimit      = 0xffffffffffffffff
      , coinbase      = 0
      , number        = 0
      , timestamp     = Lit 0
      , blockGaslimit = 0xffffffffffffffff
      , gasprice      = 0
      , maxCodeSize   = 0xffffffff
      , prevRandao    = 0
      , schedule      = FeeSchedule.berlin
      , chainId       = 1
      , create        = False
      , initialStorage = EmptyStore
      , txAccessList  = mempty -- TODO: support me soon
      , allowFFI      = False
      }

benchCode :: ByteString -> Benchmark
benchCode bs = bench "interpret" $ nfIO $ do
  let vm0 = vmFromByteString bs
  result <- Stepper.interpret (Fetch.zero 0 Nothing) vm0 Stepper.execFully
  case result of
    Left _ -> pure False
    Right _ -> pure True

strip0xL :: LazyByteString.ByteString -> LazyByteString.ByteString
strip0xL bs = if "0x" `LazyByteString.isPrefixOf` bs
  then LazyByteString.drop 2 bs
  else bs

benchFile :: FilePath -> Benchmark
benchFile path = bench (show path) $ nfIO $ do
  bs <- strip0x <$> ByteString.readFile path
  let vm0 = vmFromByteString bs
  result <- Stepper.interpret (Fetch.zero 0 Nothing) vm0 Stepper.execFully
  case result of
    Left _ -> pure False
    Right _ -> pure True

-- bench every .bin file in a given folder
benchFolder :: FilePath -> IO [Benchmark]
benchFolder path = do
  files <- Find.find Find.always (Find.extension Find.==? ".bin") path
  pure $ map benchFile files

--- General State Tests ----------------------------------------------------------------------------


-- | loads and parses all blockchain test files
-- We pull this out into a separate stage to ensure that we only benchmark the
-- actual time spent executing tests, and not the IO & parsing overhead
bcjsons :: IO (Map.Map FilePath (Map.Map String BCTests.Case))
bcjsons = do
  repo <- getEnv "HEVM_ETHEREUM_TESTS_REPO"
  let testsDir = "BlockchainTests/GeneralStateTests"
      dir = repo </> testsDir
  jsons <- Find.find Find.always (Find.extension Find.==? ".json") dir
  Map.fromList <$> mapM parseSuite jsons
  where
    parseSuite path = do
      contents <- LazyByteString.readFile path
      case BCTests.parseBCSuite contents of
        Left e -> pure (path, mempty)
        Right tests -> pure (path, tests)

-- | executes all provided bc tests in sequence and accumulates a boolean value representing their success.
-- the accumulated value ensures that we actually have to execute all the tests as a part of this benchmark
blockchainTests :: IO (Map.Map FilePath (Map.Map String BCTests.Case)) -> Benchmark
blockchainTests ts = bench "blockchain-tests" $ nfIO $ do
  tests <- ts
  putStrLn "\n    executing tests:"
  let cases = concat . Map.elems . (fmap Map.toList) $ tests
      ignored = Map.keys BCTests.commonProblematicTests
  foldM (\acc (n, c) ->
      if n `elem` ignored
      then pure True
      else do
        res <- runBCTest c
        putStrLn $ "      " <> n
        pure $ acc && res
    ) True cases

-- | executes a single test case and returns a boolean value representing its success
runBCTest :: BCTests.Case -> IO Bool
runBCTest x =
 do
  let vm0 = BCTests.vmForCase x
  result <- Stepper.interpret (Fetch.zero 0 Nothing) vm0 Stepper.execFully
  case result of
    Left _ -> pure False
    Right _ -> pure True
  -- maybeReason <- BCTests.checkExpectation False x (_ result)
  -- pure $ isNothing maybeReason


--- Helpers ----------------------------------------------------------------------------------------


debugContract :: ByteString -> IO ()
debugContract c = withSolvers CVC5 4 Nothing $ \solvers -> do
  let prestate = abstractVM (mkCalldata Nothing []) c Nothing AbstractStore
  void $ TTY.runFromVM solvers Nothing Nothing emptyDapp prestate

findPanics :: Solver -> Natural -> Integer -> ByteString -> IO ()
findPanics solver count iters c = do
  (_, res) <- withSolvers solver count Nothing $ \s -> do
    let opts = defaultVeriOpts
          { maxIter = Just iters
          , askSmtIters = iters + 1
          }
    checkAssert s allPanicCodes c Nothing [] opts
  putStrLn "done"


-- constructs a benchmark suite that checks the given bytecode for reachable
-- assertion violations takes an iteration bound, as well as a list of solver
-- counts to benchmark, allowing us to construct benchmarks that compare the
-- performance impact of increasing solver parallelisation
mkbench :: IO ByteString -> String -> Integer -> [Natural] -> Benchmark
mkbench c name iters counts = localOption WallTime $ env c (bgroup name . bmarks)
  where
    bmarks c' = concat $ [
       [ bench ("cvc5-" <> show i) $ nfIO $ findPanics CVC5 i iters c'
       , bench ("z3-" <> show i) $ nfIO $ findPanics Z3 i iters c'
       ]
       | i <- counts
     ]
