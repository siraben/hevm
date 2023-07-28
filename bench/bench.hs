{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Aeson qualified as JSON
import Data.ByteString.UTF8 as BSU 
import Data.ByteString qualified as ByteString
import Data.Either
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String.Here
import EVM
import EVM.ABI
import EVM.Expr
import EVM.FeeSchedule qualified as FeeSchedule
import EVM.Fetch qualified as Fetch
import EVM.Format
import EVM.Sign
import EVM.Solidity
import EVM.Stepper qualified as Stepper
import EVM.Test.Tracing
import EVM.Transaction qualified
import EVM.Types
import EVM.UnitTest
import GHC.Word
import System.Directory (setCurrentDirectory)
import System.FilePath.Find qualified as Find
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import Test.Tasty.Bench

-- benchmark hevm and EVM tool on the folder called benchmarks
-- using tasty-bench

vmFromByteString :: ByteString -> VM
vmFromByteString bs =
  bs
    & hexByteString "bytes"
    & ConcreteRuntimeCode
    & RuntimeCode
    & initialContract
    & vm0
    & EVM.Transaction.initTx

vmFromRawByteString :: ByteString -> VM
vmFromRawByteString bs =
  bs
    & ConcreteRuntimeCode
    & RuntimeCode
    & initialContract
    & vm0
    & EVM.Transaction.initTx

vm0 :: Contract -> VM
vm0 c = makeVm $ vm0Opts c

vm0Opts :: Contract -> VMOpts
vm0Opts c =
  VMOpts
    { contract = c,
      calldata = (ConcreteBuf "", []),
      value = Lit 0xfffffffffffff, -- balance
      address = 0xacab,
      caller = litAddr 0,
      origin = 0,
      gas = 0xffffffffffffffff,
      baseFee = 0,
      priorityFee = 0,
      gaslimit = 0xffffffffffffffff,
      coinbase = 0,
      number = 0,
      timestamp = Lit 0,
      blockGaslimit = 0xffffffffffffffff,
      gasprice = 0,
      maxCodeSize = 0xffffffff,
      prevRandao = 0,
      schedule = FeeSchedule.berlin,
      chainId = 1,
      create = False,
      initialStorage = EmptyStore,
      txAccessList = mempty, -- TODO: support me soon
      allowFFI = False
    }

benchCode :: ByteString -> Benchmark
benchCode bs = bench "interpret" $ nfIO $ do
  isRight <$> Stepper.interpret (Fetch.zero 0 Nothing) (vmFromByteString bs) Stepper.execFully

benchFile :: FilePath -> Benchmark
benchFile path = bench (show path) $ nfIO $ do
  bs <- strip0x <$> ByteString.readFile path
  isRight <$> Stepper.interpret (Fetch.zero 0 Nothing) (vmFromByteString bs) Stepper.execFully

-- bench every .bin file in a given folder
benchFolder :: FilePath -> IO [Benchmark]
benchFolder path = map benchFile <$> Find.find Find.always (Find.extension Find.==? ".bin") path

vmOptsToTestVMParams :: VMOpts -> TestVMParams
vmOptsToTestVMParams v =
  TestVMParams
    { address = v.address,
      caller = fromJust $ exprToAddr $ v.caller,
      origin = v.origin,
      gasCreate = v.gas,
      gasCall = v.gas,
      baseFee = v.baseFee,
      priorityFee = v.priorityFee,
      balanceCreate = case v.value of
        Lit x -> x
        _ -> 0,
      coinbase = v.coinbase,
      number = v.number,
      timestamp = case v.timestamp of
        Lit x -> x
        _ -> 0,
      gaslimit = v.gaslimit,
      gasprice = v.gasprice,
      maxCodeSize = v.maxCodeSize,
      prevrandao = v.prevRandao,
      chainId = v.chainId
    }

callMainForBytecode bs = do
  let vm = vmFromRawByteString bs
  Stepper.interpret (Fetch.zero 0 Nothing) vm (Stepper.evm (abiCall (vmOptsToTestVMParams (vm0Opts (initialContract (RuntimeCode (ConcreteRuntimeCode bs))))) (Left ("main()", emptyAbi))) >> Stepper.execFully)


benchMain (name, bs) = bench name $ nfIO $ isRight <$> callMainForBytecode bs

benchBytecodes = map (\x -> bench "bytecode" $ nfIO $ isRight <$> (Stepper.interpret (Fetch.zero 0 Nothing) (vmFromRawByteString x) Stepper.execFully))

callMain :: TestVMParams -> EVM ()
callMain params = makeTxCall params (ConcreteBuf (abiMethod "main()" emptyAbi), [])

evmSetup' :: [Word8] -> ByteString -> (EVM.Transaction.Transaction, EVMToolEnv, EVMToolAlloc, Addr, Addr, Integer)
evmSetup' contr txData = (txn, evmEnv, contrAlloc, fromAddress, toAddress, sk)
  where
    bitcode = ByteString.pack contr
    contrAlloc =
      EVMToolAlloc
        { balance = 0xffffffffffffff,
          code = bitcode,
          nonce = 0x48
        }
    txn =
      EVM.Transaction.Transaction
        { txdata = txData,
          gasLimit = 0xffffffffffffff,
          gasPrice = Just 1,
          nonce = 172,
          toAddr = Just 0x8A8eAFb1cf62BfBeb1741769DAE1a9dd47996192,
          r = 0, -- will be fixed when we sign
          s = 0, -- will be fixed when we sign
          v = 0, -- will be fixed when we sign
          value = 0, -- setting this > 0 fails because HEVM doesn't handle value sent in toplevel transaction
          txtype = EVM.Transaction.EIP1559Transaction,
          accessList = [],
          maxPriorityFeeGas = Just 1,
          maxFeePerGas = Just 1,
          chainId = 1
        }
    evmEnv =
      EVMToolEnv
        { coinbase = 0xff,
          timestamp = Lit 0x3e8,
          number = 0x0,
          prevRandao = 0x0,
          gasLimit = 0xffffffffffffff,
          baseFee = 0x0,
          maxCodeSize = 0xfffff,
          schedule = FeeSchedule.berlin,
          blockHashes = blockHashesDefault
        }
    sk = 0xDC38EE117CAE37750EB1ECC5CFD3DE8E85963B481B93E732C5D0CB66EE6B0C9D
    fromAddress :: Addr
    fromAddress = fromJust $ deriveAddr sk
    toAddress :: Addr
    toAddress = 0x8A8eAFb1cf62BfBeb1741769DAE1a9dd47996192

runGethOnFile :: FilePath -> IO Benchmark
runGethOnFile path = do
  bs <- hexByteString "bytes" . strip0x <$> ByteString.readFile path
  let contr = ByteString.unpack bs
  let (txn, evmEnv, contrAlloc, fromAddress, toAddress, sk) = evmSetup' contr mempty
      txs = [EVM.Transaction.sign sk txn]
      walletAlloc =
        EVMToolAlloc
          { balance = 0x5ffd4878be161d74,
            code = mempty,
            nonce = 0xac
          }
      alloc :: Map.Map Addr EVMToolAlloc
      alloc = Map.fromList ([(fromAddress, walletAlloc), (toAddress, contrAlloc)])

  -- do each benchmark in a temporary directory
  pure $
    bench (show path) $
      nfIO $
        withTempDirectory
          "/tmp"
          "name"
          ( \dir -> do
              setCurrentDirectory dir
              JSON.encodeFile "txs.json" txs
              JSON.encodeFile "alloc.json" alloc
              JSON.encodeFile "env.json" evmEnv
              readProcessWithExitCode
                "evm"
                [ "transition",
                  "--input.alloc",
                  "alloc.json",
                  "--input.env",
                  "env.json",
                  "--input.txs",
                  "txs.json"
                ]
                ""
          )

benchEVMTool :: FilePath -> IO Benchmark
benchEVMTool path = runGethOnFile path

benchEVMToolFolder :: FilePath -> IO [Benchmark]
benchEVMToolFolder path = do
  files <- Find.find Find.always (Find.extension Find.==? ".bin") path
  mapM benchEVMTool files

main :: IO ()
main = do
  l <- mapM (\n -> (show n,) <$> simple_loop n)  [2 ^ n | n <- [1 .. 14]]
  p <- mapM (\n -> (show n,) <$> primes n)       [2 ^ n | n <- [1 .. 14]]
  long <- mapM (\n -> (show n,) <$> primes n)    [2 ^ n | n <- [1 .. 14]]
  h <- mapM (\n -> (show n,) <$> hashes n)       [2 ^ n | n <- [1 .. 14]]
  badmem <- mapM (\n -> (show n,) <$> hashmem n) [2 ^ n | n <- [1 .. 14]]
  balanceTransfer <- mapM (\n -> (show n,) <$> balanceTransfer n) [2 ^ n | n <- [1 .. 14]]
  funcCall <- mapM (\n -> (show n,) <$> funcCall n) [2 ^ n | n <- [1 .. 14]]
  defaultMain [bgroup "benches-loop" (benchMain <$> l),
               bgroup "benches-primes" (benchMain <$> p),
               bgroup "benches-long" (benchMain <$> long),
               bgroup "benches-hashes" (benchMain <$> h),
               bgroup "benches-hashmem" (benchMain <$> badmem),
               bgroup "benches-balance_transfer" (benchMain <$> balanceTransfer),
               bgroup "benches-func_call" (benchMain <$> funcCall)
              ]

-- Loop that adds up n numbers
simple_loop :: Int -> IO ByteString
simple_loop n = do
  let src =
        [i|
          contract A {
            function main() public {
              uint256 acc = 0;
              for (uint i = 0; i < ${n}; i++) {
                acc += i;
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

-- Computes prime numbers and stores them up to n.
primes :: Int -> IO ByteString
primes n = do
  let src =
        [i|
          contract A {
            mapping (uint => uint) public primes;
            function isPrime (uint n) public returns (bool) {
              if (n == 2) {
                return true;
              }
              if (n % 2 == 0) {
                return false;
              }
              for (uint i = 3; i * i < n; i += 2) {
                if (n % i == 0) {
                  return false;
                }
              }
              return true;
            }
            
            function main() public {
              uint n = 0;
              for (uint i = 0; i < ${n}; i++) {
                if (isPrime(i)) {
                  primes[n++] = i;
                }
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

-- Program that is as long as the input
longFile :: Int -> ByteString
longFile n = hexByteString "bytes" (BSU.fromString ("600a" ++ concat (replicate n ("600101"))))

-- Program that repeatedly hashes a value
hashes :: Int -> IO ByteString
hashes n = do
  let src =
        [i|
          contract A {
            function main() public {
              bytes32 h = 0;
              for (uint i = 0; i < ${n}; i++) {
                h = keccak256(abi.encode(h));
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

-- Program that repeatedly hashes a value and stores it in a map
hashmem :: Int -> IO ByteString
hashmem n = do
  let src =
        [i|
          contract A {
            mapping (uint256 => uint256) public map;
            function main() public {
              uint256 h = 0;
              for (uint i = 0; i < ${n}; i++) {
                uint256 x = h;
                h = uint256(keccak256(abi.encode(h)));
                map[x] = h;
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

balanceTransfer :: Int -> IO ByteString
balanceTransfer n = do
  let src =
        [i|
          contract A {
            function main() public {
              address payable to = payable(address(0x8A8eAFb1cf62BfBeb1741769DAE1a9dd47996192)); 
              for (uint i = 0; i < ${n}; i++) {
                to.transfer(1);
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

funcCall :: Int -> IO ByteString
funcCall n = do
  let src =
        [i|
          contract A {
            uint256 public acc;
            function f(uint256 x) public {
              acc += x;
            }
            function main() public {
              for (uint i = 0; i < ${n}; i++) {
                f(i);
              }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)
