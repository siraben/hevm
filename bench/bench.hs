{-# LANGUAGE DataKinds #-}
module Main where

import GHC.Natural
import Control.Monad
import System.Environment (getEnv)

-- import qualified Paths_hevm as Paths

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
import EVM.Test.Tracing
import Data.Aeson qualified as JSON
import System.Process (readProcessWithExitCode)
import GHC.IO.Exception
import System.Exit (ExitCode(..), exitWith, ExitCode(..))
import GHC.Word
import Data.Maybe
import EVM.Sign

-- import EVM.Types

-- benchmark hevm and EVM tool on the folder called benchmarks
-- using tasty-bench
main :: IO ()
main = do
  let path = "benchmarks"
  hevmBenches <- benchFolder path
  evmtoolBenches <- benchEVMToolFolder path
  defaultMain [ bgroup "hevm" hevmBenches
              , bgroup "evmtool" evmtoolBenches
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

-- bytecodeToOpContract :: ByteString -> OpContract
-- evmSetup :: OpContract -> ByteString -> Int -> (EVM.Transaction.Transaction, EVMToolEnv, EVMToolAlloc, Addr, Addr, Integer)
evmSetup' contr txData gaslimitExec = (txn, evmEnv, contrAlloc, fromAddress, toAddress, sk)
  where
    contrLits = contr
    -- toW8fromLitB :: Expr 'Byte -> Word8
    -- toW8fromLitB (LitByte a) = a
    -- toW8fromLitB _ = internalError "Cannot convert non-litB"

    bitcode = ByteString.pack $ contrLits
    contrAlloc = EVMToolAlloc{ balance = 0xffffffffffffff
                             , code = bitcode
                             , nonce = 0x48
                             }
    txn = EVM.Transaction.Transaction
      { txdata     = txData
      , gasLimit = fromIntegral gaslimitExec
      , gasPrice = Just 1
      , nonce    = 172
      , toAddr   = Just 0x8A8eAFb1cf62BfBeb1741769DAE1a9dd47996192
      , r        = 0 -- will be fixed when we sign
      , s        = 0 -- will be fixed when we sign
      , v        = 0 -- will be fixed when we sign
      , value    = 0 -- setting this > 0 fails because HEVM doesn't handle value sent in toplevel transaction
      , txtype     = EVM.Transaction.EIP1559Transaction
      , accessList = []
      , maxPriorityFeeGas =  Just 1
      , maxFeePerGas = Just 1
      , chainId = 1
      }
    evmEnv = EVMToolEnv { coinbase   =  0xff
                        , timestamp   =  Lit 0x3e8
                        , number      =  0x0
                        , prevRandao  =  0x0
                        , gasLimit    =  fromIntegral gaslimitExec
                        , baseFee     =  0x0
                        , maxCodeSize =  0xfffff
                        , schedule    =  FeeSchedule.berlin
                        , blockHashes =  blockHashesDefault
                        }
    sk = 0xDC38EE117CAE37750EB1ECC5CFD3DE8E85963B481B93E732C5D0CB66EE6B0C9D
    fromAddress :: Addr
    fromAddress = fromJust $ deriveAddr sk
    toAddress :: Addr
    toAddress = 0x8A8eAFb1cf62BfBeb1741769DAE1a9dd47996192



-- getEVMToolRet' :: OpContract -> ByteString -> Int -> IO (Maybe EVMToolResult)
runGeth contr = do
  let (txn, evmEnv, contrAlloc, fromAddress, toAddress, sk) = evmSetup' contr mempty 40000
      txs = [EVM.Transaction.sign sk txn]
      walletAlloc = EVMToolAlloc{ balance = 0x5ffd4878be161d74
                                , code = mempty -- ByteString.pack [ 0x60, 0x20 ]
                                , nonce = 0xac
                                }
      alloc :: Map.Map Addr EVMToolAlloc
      alloc = Map.fromList ([ (fromAddress, walletAlloc), (toAddress, contrAlloc)])
  JSON.encodeFile "txs.json" txs
  JSON.encodeFile "alloc.json" alloc
  JSON.encodeFile "env.json" evmEnv
  (exitCode, evmtoolStdout, evmtoolStderr) <- readProcessWithExitCode "evm" [ "transition"
                               ,"--input.alloc" , "alloc.json"
                               , "--input.env" , "env.json"
                               , "--input.txs" , "txs.json"
                               , "--output.alloc" , "alloc-out.json"
                               -- , "--trace.returndata=true"
                               , "--trace" , "trace.json"
                               , "--output.result", "result.json"
                               ] ""
  when (exitCode /= ExitSuccess) $ do
    putStrLn $ "evmtool exited with code " <> show exitCode
    putStrLn $ "evmtool stderr output:" <> show evmtoolStderr
    putStrLn $ "evmtool stdout output:" <> show evmtoolStdout
    exitWith (ExitFailure 2)
    

runGethOnFile path = do
  bs <- strip0x <$> ByteString.readFile path
  let contr = ByteString.unpack bs
  runGeth contr
benchEVMTool :: FilePath -> Benchmark
benchEVMTool path = bench (show path) $ nfIO $ runGethOnFile path

benchEVMToolFolder :: FilePath -> IO [Benchmark]
benchEVMToolFolder path = do
  files <- Find.find Find.always (Find.extension Find.==? ".bin") path
  pure $ map benchEVMTool files

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
        Left _ -> pure (path, mempty)
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
  (_, _) <- withSolvers solver count Nothing $ \s -> do
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
