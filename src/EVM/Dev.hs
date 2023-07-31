{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module: EVM.Dev
Description: Helpers for repl driven hevm hacking
-}
module EVM.Dev where

import Control.Monad.State.Strict hiding (state)
import Data.ByteString hiding (writeFile, zip)
import Data.Maybe (fromJust)
import Data.String.Here
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Typeable (Typeable)
import System.Directory (withCurrentDirectory)
import System.Exit (exitFailure)

import EVM
import EVM.Dapp (dappInfo)
import EVM.Expr (numBranches, simplify)
import EVM.Fetch qualified as Fetch
import EVM.FeeSchedule qualified as FeeSchedule
import EVM.Format (formatExpr)
import EVM.SMT
import EVM.Solvers
import EVM.Solidity
import EVM.SymExec
import EVM.Types
import EVM.UnitTest
import GHC.Conc
import Witch (unsafeInto)
import Control.Monad.ST (RealWorld, ST, stToIO)

checkEquiv :: (Typeable a) => Expr a -> Expr a -> IO ()
checkEquiv a b = withSolvers Z3 1 Nothing $ \s -> do
  let smt = assertProps [a ./= b]
  res <- checkSat s smt
  print res

runDappTest :: FilePath -> IO ()
runDappTest root =
  withCurrentDirectory root $ do
    cores <- unsafeInto <$> getNumProcessors
    let testFile = root <> "/out/dapp.sol.json"
    Right (BuildOutput contracts _) <- readSolc DappTools root testFile
    withSolvers Z3 cores Nothing $ \solvers -> do
      opts <- testOpts solvers root testFile
      res <- unitTest opts contracts Nothing
      unless res exitFailure

testOpts :: SolverGroup -> FilePath -> FilePath -> IO (UnitTestOptions RealWorld)
testOpts solvers root testFile = do
  srcInfo <- readSolc DappTools root testFile >>= \case
    Left e -> internalError e
    Right out ->
      pure $ dappInfo root out

  params <- getParametersFromEnvironmentVariables Nothing
  pure EVM.UnitTest.UnitTestOptions
    { EVM.UnitTest.solvers = solvers
    , EVM.UnitTest.rpcInfo = Nothing
    , EVM.UnitTest.maxIter = Nothing
    , EVM.UnitTest.askSmtIters = 1
    , EVM.UnitTest.smtTimeout = Nothing
    , EVM.UnitTest.smtDebug = False
    , EVM.UnitTest.solver = Nothing
    , EVM.UnitTest.covMatch = Nothing
    , EVM.UnitTest.verbose = Nothing
    , EVM.UnitTest.match = ".*"
    , EVM.UnitTest.maxDepth = Nothing
    , EVM.UnitTest.fuzzRuns = 100
    , EVM.UnitTest.replay = Nothing
    , EVM.UnitTest.vmModifier = id
    , EVM.UnitTest.testParams = params
    , EVM.UnitTest.dapp = srcInfo
    , EVM.UnitTest.ffiAllowed = True
    }

doTest :: IO ()
doTest = do
  c <- testContract
  reachable' False c
  --e <- simplify <$> buildExpr c
  --Prelude.putStrLn (formatExpr e)

analyzeDai :: IO ()
analyzeDai = do
  d <- dai
  reachable' False d

daiExpr :: IO (Expr End)
daiExpr = do
  d <- dai
  withSolvers Z3 1 Nothing $ \s -> buildExpr s d

analyzeVat :: IO ()
analyzeVat = do
  putStrLn "starting"
  v <- vat
  withSolvers Z3 1 Nothing $ \s -> do
    e <- buildExpr s v
    putStrLn $ "done (" <> show (numBranches e) <> " branches)"
    reachable' False v

analyzeDeposit :: IO ()
analyzeDeposit = do
  Just c <- solcRuntime "Deposit"
    [i|
    contract Deposit {
      function deposit(uint256 deposit_count) external pure {
        require(deposit_count < 2**32 - 1);
        ++deposit_count;
        bool found = false;
        for (uint height = 0; height < 32; height++) {
          if ((deposit_count & 1) == 1) {
            found = true;
            break;
          }
         deposit_count = deposit_count >> 1;
         }
        assert(found);
      }
     }
    |]
  withSolvers Z3 1 Nothing $ \s -> do
    putStrLn "Exploring Contract"
    e <- simplify <$> buildExpr s c
    putStrLn "Writing AST"
    T.writeFile "full.ast" (formatExpr e)


reachable' :: Bool -> ByteString -> IO ()
reachable' smtdebug c = do
  putStrLn "Exploring contract"
  withSolvers Z3 4 Nothing $ \s -> do
    full <- simplify <$> buildExpr s c
    putStrLn $ "Explored contract (" <> (show $ numBranches full) <> " branches)"
    --putStrLn $ formatExpr full
    T.writeFile "full.ast" $ formatExpr full
    putStrLn "Dumped to full.ast"
    putStrLn "Checking reachability"
    (qs, less) <- reachable s full
    putStrLn $ "Checked reachability (" <> (show $ numBranches less) <> " reachable branches)"
    T.writeFile "reachable.ast" $ formatExpr less
    putStrLn "Dumped to reachable.ast"
    --putStrLn $ formatExpr less
    when smtdebug $ do
      putStrLn "\n\nQueries\n\n"
      forM_ qs $ \q -> do
        putStrLn "\n\n-- Query --"
        TL.putStrLn $ formatSMT2 q


showExpr :: ByteString -> IO ()
showExpr c = do
  withSolvers Z3 1 Nothing $ \s -> do
    e <- buildExpr s c
    T.putStrLn $ formatExpr (simplify e)

summaryStore :: IO ByteString
summaryStore = do
  let src =
        [i|
          contract A {
            uint x;
            function f(uint256 y) public {
               unchecked {
                 x += y;
                 x += y;
               }
            }
          }
        |]
  fmap fromJust (solcRuntime "A" src)

safeAdd :: IO ByteString
safeAdd = do
  let src =
        [i|
          contract SafeAdd {
            function add(uint x, uint y) public pure returns (uint z) {
                 require((z = x + y) >= x);
            }
          }
        |]
  fmap fromJust (solcRuntime "SafeAdd" src)



testContract :: IO ByteString
testContract = do
  let src =
        [i|
          contract C {
            uint x;
            function set(uint v) public {
              x = v + v;
            }
          }
          |]
  fmap fromJust (solcRuntime "C" src)

vat :: IO ByteString
vat = do
  let src =
        [i|
          /// vat.sol -- Dai CDP database

          // Copyright (C) 2018 Rain <rainbreak@riseup.net>
          //
          // This program is free software: you can redistribute it and/or modify
          // it under the terms of the GNU Affero General Public License as published by
          // the Free Software Foundation, either version 3 of the License, or
          // (at your option) any later version.
          //
          // This program is distributed in the hope that it will be useful,
          // but WITHOUT ANY WARRANTY; without even the implied warranty of
          // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
          // GNU Affero General Public License for more details.
          //
          // You should have received a copy of the GNU Affero General Public License
          // along with this program.  If not, see <https://www.gnu.org/licenses/>.

          // FIXME: This contract was altered compared to the production version.
          // It doesn't use LibNote anymore.
          // New deployments of this contract will need to include custom events (TO DO).

          contract Vat {
              // --- Auth ---
              mapping (address => uint) public wards;
              function rely(address usr) external auth { require(live == 1, "Vat/not-live"); wards[usr] = 1; }
              function deny(address usr) external auth { require(live == 1, "Vat/not-live"); wards[usr] = 0; }
              modifier auth {
                  require(wards[msg.sender] == 1, "Vat/not-authorized");
                  _;
              }

              mapping(address => mapping (address => uint)) public can;
              function hope(address usr) external { can[msg.sender][usr] = 1; }
              function nope(address usr) external { can[msg.sender][usr] = 0; }
              function wish(address bit, address usr) internal view returns (bool) {
                  return either(bit == usr, can[bit][usr] == 1);
              }

              // --- Data ---
              struct Ilk {
                  uint256 Art;   // Total Normalised Debt     [wad]
                  uint256 rate;  // Accumulated Rates         [ray]
                  uint256 spot;  // Price with Safety Margin  [ray]
                  uint256 line;  // Debt Ceiling              [rad]
                  uint256 dust;  // Urn Debt Floor            [rad]
              }
              struct Urn {
                  uint256 ink;   // Locked Collateral  [wad]
                  uint256 art;   // Normalised Debt    [wad]
              }

              mapping (bytes32 => Ilk)                       public ilks;
              mapping (bytes32 => mapping (address => Urn )) public urns;
              mapping (bytes32 => mapping (address => uint)) public gem;  // [wad]
              mapping (address => uint256)                   public dai;  // [rad]
              mapping (address => uint256)                   public sin;  // [rad]

              uint256 public debt;  // Total Dai Issued    [rad]
              uint256 public vice;  // Total Unbacked Dai  [rad]
              uint256 public Line;  // Total Debt Ceiling  [rad]
              uint256 public live;  // Active Flag

              // --- Init ---
              constructor() public {
                  wards[msg.sender] = 1;
                  live = 1;
              }

              // --- Math ---
              function _add(uint x, int y) internal pure returns (uint z) {
                  z = x + uint(y);
                  require(y >= 0 || z <= x);
                  require(y <= 0 || z >= x);
              }
              function _sub(uint x, int y) internal pure returns (uint z) {
                  z = x - uint(y);
                  require(y <= 0 || z <= x);
                  require(y >= 0 || z >= x);
              }
              function _mul(uint x, int y) internal pure returns (int z) {
                  z = int(x) * y;
                  require(int(x) >= 0);
                  require(y == 0 || z / y == int(x));
              }
              function _add(uint x, uint y) internal pure returns (uint z) {
                  require((z = x + y) >= x);
              }
              function _sub(uint x, uint y) internal pure returns (uint z) {
                  require((z = x - y) <= x);
              }
              function _mul(uint x, uint y) internal pure returns (uint z) {
                  require(y == 0 || (z = x * y) / y == x);
              }

              // --- Administration ---
              function init(bytes32 ilk) external auth {
                  require(ilks[ilk].rate == 0, "Vat/ilk-already-init");
                  ilks[ilk].rate = 10 ** 27;
              }
              function file(bytes32 what, uint data) external auth {
                  require(live == 1, "Vat/not-live");
                  if (what == "Line") Line = data;
                  else revert("Vat/file-unrecognized-param");
              }
              function file(bytes32 ilk, bytes32 what, uint data) external auth {
                  require(live == 1, "Vat/not-live");
                  if (what == "spot") ilks[ilk].spot = data;
                  else if (what == "line") ilks[ilk].line = data;
                  else if (what == "dust") ilks[ilk].dust = data;
                  else revert("Vat/file-unrecognized-param");
              }
              function cage() external auth {
                  live = 0;
              }

              // --- Fungibility ---
              function slip(bytes32 ilk, address usr, int256 wad) external auth {
                  gem[ilk][usr] = _add(gem[ilk][usr], wad);
              }
              function flux(bytes32 ilk, address src, address dst, uint256 wad) external {
                  require(wish(src, msg.sender), "Vat/not-allowed");
                  gem[ilk][src] = _sub(gem[ilk][src], wad);
                  gem[ilk][dst] = _add(gem[ilk][dst], wad);
              }
              function move(address src, address dst, uint256 rad) external {
                  require(wish(src, msg.sender), "Vat/not-allowed");
                  dai[src] = _sub(dai[src], rad);
                  dai[dst] = _add(dai[dst], rad);
              }

              function either(bool x, bool y) internal pure returns (bool z) {
                  assembly{ z := or(x, y)}
              }
              function both(bool x, bool y) internal pure returns (bool z) {
                  assembly{ z := and(x, y)}
              }

              // --- CDP Confiscation ---
              function grab(bytes32 i, address u, address v, address w, int dink, int dart) external auth {
                  Urn storage urn = urns[i][u];
                  Ilk storage ilk = ilks[i];

                  urn.ink = _add(urn.ink, dink);
                  urn.art = _add(urn.art, dart);
                  ilk.Art = _add(ilk.Art, dart);

                  int dtab = _mul(ilk.rate, dart);

                  gem[i][v] = _sub(gem[i][v], dink);
                  sin[w]    = _sub(sin[w],    dtab);
                  vice      = _sub(vice,      dtab);
              }
          }
          |]
  fmap fromJust (solcRuntime "Vat" src)

initVm :: ByteString -> ST s (VM s)
initVm bs = vm
  where
    contractCode = RuntimeCode (ConcreteRuntimeCode bs)
    c = Contract
      { contractcode = contractCode
      , balance      = 0
      , nonce        = 0
      , codehash     = keccak (ConcreteBuf bs)
      , opIxMap      = mkOpIxMap contractCode
      , codeOps      = mkCodeOps contractCode
      , external     = False
      }
    vm = makeVm $ VMOpts
      { contract       = c
      , calldata       = (AbstractBuf "txdata", [])
      , value          = CallValue 0
      , address        = Addr 0xffffffffffffffff
      , caller         = Lit 0
      , origin         = Addr 0xffffffffffffffff
      , gas            = 0xffffffffffffffff
      , gaslimit       = 0xffffffffffffffff
      , initialStorage = AbstractStore
      , baseFee        = 0
      , priorityFee    = 0
      , coinbase       = 0
      , number         = 0
      , timestamp      = Var "timestamp"
      , blockGaslimit  = 0
      , gasprice       = 0
      , maxCodeSize    = 0xffffffff
      , prevRandao     = 420
      , schedule       = FeeSchedule.berlin
      , chainId        = 1
      , create         = False
      , txAccessList   = mempty
      , allowFFI       = False
      }


-- | Builds the Expr for the given evm bytecode object
buildExpr :: SolverGroup -> ByteString -> IO (Expr End)
buildExpr solvers bs = do
  vm <- stToIO $ initVm bs
  interpret (Fetch.oracle solvers Nothing) Nothing 1 Naive vm runExpr

dai :: IO ByteString
dai = do
  let src =
        [i|
        contract Dai {
            // --- Auth ---
            mapping (address => uint) public wards;
            function rely(address guy) external auth { wards[guy] = 1; }
            function deny(address guy) external auth { wards[guy] = 0; }
            modifier auth {
                require(wards[msg.sender] == 1, "Dai/not-authorized");
                _;
            }

            // --- ERC20 Data ---
            string  public constant name     = "Dai Stablecoin";
            string  public constant symbol   = "DAI";
            string  public constant version  = "1";
            uint8   public constant decimals = 18;
            uint256 public totalSupply;

            mapping (address => uint)                      public balanceOf;
            mapping (address => mapping (address => uint)) public allowance;

            event Approval(address indexed src, address indexed guy, uint wad);
            event Transfer(address indexed src, address indexed dst, uint wad);

            // --- Math ---
            function add(uint x, uint y) internal pure returns (uint z) {
                require((z = x + y) >= x);
            }
            function sub(uint x, uint y) internal pure returns (uint z) {
                require((z = x - y) <= x);
            }

            // --- EIP712 niceties ---
            constructor() public {
                wards[msg.sender] = 1;
            }

            // --- Token ---
            function transfer(address dst, uint wad) external returns (bool) {
                return transferFrom(msg.sender, dst, wad);
            }
            function transferFrom(address src, address dst, uint wad)
                public returns (bool)
            {
                require(balanceOf[src] >= wad, "Dai/insufficient-balance");
                if (src != msg.sender && allowance[src][msg.sender] != type(uint).max) {
                    require(allowance[src][msg.sender] >= wad, "Dai/insufficient-allowance");
                    allowance[src][msg.sender] = sub(allowance[src][msg.sender], wad);
                }
                balanceOf[src] = sub(balanceOf[src], wad);
                balanceOf[dst] = add(balanceOf[dst], wad);
                emit Transfer(src, dst, wad);
                return true;
            }
            function mint(address usr, uint wad) external auth {
                balanceOf[usr] = add(balanceOf[usr], wad);
                totalSupply    = add(totalSupply, wad);
                emit Transfer(address(0), usr, wad);
            }
            function burn(address usr, uint wad) external {
                require(balanceOf[usr] >= wad, "Dai/insufficient-balance");
                if (usr != msg.sender && allowance[usr][msg.sender] != type(uint).max) {
                    require(allowance[usr][msg.sender] >= wad, "Dai/insufficient-allowance");
                    allowance[usr][msg.sender] = sub(allowance[usr][msg.sender], wad);
                }
                balanceOf[usr] = sub(balanceOf[usr], wad);
                totalSupply    = sub(totalSupply, wad);
                emit Transfer(usr, address(0), wad);
            }
            function approve(address usr, uint wad) external returns (bool) {
                allowance[msg.sender][usr] = wad;
                emit Approval(msg.sender, usr, wad);
                return true;
            }

            // --- Alias ---
            function push(address usr, uint wad) external {
                transferFrom(msg.sender, usr, wad);
            }
            function pull(address usr, uint wad) external {
                transferFrom(usr, msg.sender, wad);
            }
            function move(address src, address dst, uint wad) external {
                transferFrom(src, dst, wad);
            }
        }
        |]
  fmap fromJust (solcRuntime "Dai" src)
