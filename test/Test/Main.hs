-- | Test codes for running @hpc-codecov@ executable.
module Test.Main (main) where

-- base
import           Control.Exception      (SomeException (..), bracket, try)
import           Control.Monad          (when)
import           Data.List              (isSubsequenceOf)
import           Data.Maybe             (isJust)
import           System.Environment     (getExecutablePath, lookupEnv,
                                         setEnv, unsetEnv, withArgs)
import           System.IO              (hClose, openTempFile)

-- filepath
import           System.FilePath        (takeFileName, (</>))

-- directory
import           System.Directory       (doesDirectoryExist,
                                         getTemporaryDirectory,
                                         listDirectory,
                                         removeDirectoryRecursive,
                                         removeFile)

-- process
import           System.Process         (callProcess)

-- tasty
import           Test.Tasty             (TestTree, defaultMain, testGroup,
                                         withResource)
import           Test.Tasty.HUnit       (assertFailure, testCase)

-- Internal
import qualified Trace.Hpc.Codecov.Main as HpcCodecov


-- ------------------------------------------------------------------------
--
-- Tests
--
-- ------------------------------------------------------------------------

main :: IO ()
main = do
  test_in_test <- isTestInTest
  tool <- getBuildTool
  let t = [cmdline , recipReport] ++
          [selfReportTest | not test_in_test, knownTool tool]
  when test_in_test $ putStr $ unlines
    [ ""
    , "==================================="
    , "Running test to generate hpc data  "
    , "==================================="
    , "" ]
  defaultMain $ testGroup "main" t

cmdline :: TestTree
cmdline = testGroup "cmdline"
  [ testCase "non-existing-option"
             (shouldFail (main' ["--foo"]))
  , testCase "non-existing-options"
             (shouldFail (main' ["--foo", "--bar", "--buzz"]))
  , testCase "mixdir-without-argument"
             (shouldFail (main' ["--mixdir"]))
  , testCase "no-tix-file"
             (shouldFail (main' []))
  , testCase "non-existing-tix"
             (shouldFail (main' ["no_such_file.tix"]))
  , testCase "help" (main' ["--help"])
  , testCase "version" (main' ["--version"])
  , testCase "numeric-version" (main' ["--numeric-version"])
  ]

recipReport :: TestTree
recipReport = testGroup "recip"
  [ testCase "recip-data-to-stdout"
             (main' ["--mix=test/data/reciprocal/.hpc"
                    ,"--src=test/data/reciprocal"
                    ,"--exclude=NoSuchModule"
                    ,"--verbose"
                    ,"test/data/reciprocal/reciprocal.tix"])
  , testCase "recip-data-no-src"
             (shouldFail
                (main' ["--mix=test/data/reciprocal/.hpc"
                       ,"--src=test/"
                       ,"--src=test/data"
                       ,"--verbose"
                       ,"test/data/reciprocal/reciprocal.tix"]))
  ]

-- Note: Running test to generate .mix and .tix of hpc-codecov package
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The test recursively runs itself to generate .mix and .tix files of
-- the hpc-codecov package itself.
--
-- To terminate the recursive run, the "main" function in this module
-- is looking up an environment variable to check whether the test is
-- running inside the test or not. Recursive run is done only for the
-- known build tool (e.g.: cabal install, stack).

selfReportTest :: TestTree
selfReportTest = withTempDir work1
  where
    work1 getDir =
      withResource (acquire getDir) release selfReport
    acquire getDir =
      setEnv testInTestKey "TRUE" >> getDir >>= getSelfReportArgs
    release _ =
      unsetEnv testInTestKey

-- | Self Report Arguments
data SRA =
  SRA { sra_tix      :: FilePath
      , sra_mixs     :: [FilePath]
      , sra_excludes :: [String]
      , sra_verbose  :: Bool
      , sra_out      :: Maybe FilePath
      }

emptySRA :: SRA
emptySRA = SRA { sra_tix = ""
               , sra_mixs = []
               , sra_excludes = []
               , sra_verbose = False
               , sra_out = Nothing }

data BuildTool
  = CabalV1
  | CabalV2
  | Stack
  | Unknown
  deriving (Eq, Show)

knownTool :: BuildTool -> Bool
knownTool t = case t of
  Unknown -> False
  _       -> True

getBuildTool :: IO BuildTool
getBuildTool = do
  path <- getExecutablePath
  if ".stack-work" `isSubsequenceOf` path
     then return Stack
     else if "dist-newstyle" `isSubsequenceOf` path
       then return CabalV2
       else if "dist" `isSubsequenceOf` path
         then return CabalV1
         else return Unknown

getSelfReportArgs :: FilePath -> IO SRA
getSelfReportArgs builddir = do
  tool <- getBuildTool
  case tool of
    Stack   -> getSelfReportStackArgs builddir
    CabalV1 -> getSelfReportCabalArgs setupV1 builddir
    CabalV2 -> getSelfReportCabalArgs setupV2 builddir
    _       -> error $ "Unsupported build tool: " ++ show tool

getSelfReportStackArgs :: FilePath -> IO SRA
getSelfReportStackArgs wd = do
  callProcess "stack" [ "--work-dir=" ++ wd, "test", "--coverage"]
  mb_tix <- findUnder (\p -> takeFileName p == "test-main.tix") wd
  mb_mix <- findUnder (\p -> takeFileName p == "hpc") (wd </> "dist")
  case (mb_tix, mb_mix) of
    (Just tix, Just mix) -> do
      return emptySRA { sra_tix = tix
                      , sra_mixs = [mix]
                      , sra_excludes = ["Paths_hpc_codecov"] }
    _ -> error "getting arguments for self test with stack failed"

setupV1 :: FilePath -> IO ()
setupV1 bd = bracket acquire release work
  where
    key = "GHC_PACKAGE_PATH"
    acquire = do
      mb_ghc_pkg_path <- lookupEnv key
      putStrLn $ "GHC_PACKAGE_PATH: " ++ show mb_ghc_pkg_path
      unsetEnv key
      pure mb_ghc_pkg_path

    release mb_ghc_pkg_path = do
      case mb_ghc_pkg_path of
        Just path -> setEnv key path
        Nothing   -> return ()

    work _ = do
      let setup args = callProcess "runhaskell" ("Setup.hs" : args)

      setup [ "configure", "--builddir=" ++ bd
            , "--enable-test", "--enable-coverage"
            , "--package-db=/build/package.conf.d" ]

      setup ["build", "--builddir=" ++ bd]
      setup ["test", "--builddir=" ++ bd]

setupV2 :: FilePath -> IO ()
setupV2 bd =
  callProcess "cabal" [ "test", "--builddir=" ++ bd, "--enable-coverage"]

getSelfReportCabalArgs :: (FilePath -> IO ()) -> FilePath -> IO SRA
getSelfReportCabalArgs setup bd = do
  setup bd
  mb_tix <- findUnder (\p -> takeFileName p == "test-main.tix") bd
  putStrLn $ "tix: " ++ show mb_tix
  mb_vanilla <- findUnder (\p -> takeFileName p == "vanilla") bd
  putStrLn $ "vanilla: " ++ show mb_vanilla
  case (mb_tix, mb_vanilla) of
    (Nothing, Nothing) -> error "failed to find tix and vanilla"
    (Nothing, _) -> error "failed to find tix"
    (_, Nothing) -> error "failed to find vanilla"
    (Just tix, Just vanilla) -> do
      let mixdir = vanilla </> "mix"
      mixs <- map (mixdir </>) <$> listDirectory mixdir
      return emptySRA { sra_tix = tix
                      , sra_mixs = mixs
                      , sra_excludes = ["Main", "Paths_hpc_codecov"] }

selfReport :: IO SRA -> TestTree
selfReport getArgs = testGroup "self"
  [ testCase "self-data-to-stdout"
             (getArgs >>= sraMain)
  , testCase "self-data-to-stdout-verbose"
             (do args <- getArgs
                 sraMain (args {sra_verbose=True}))
  , withTempFile
      (\getPath ->
         testCase "self-data-to-file-verbose"
                  (do path <- getPath
                      args <- getArgs
                      sraMain (args {sra_out=Just path
                                    ,sra_verbose=True})))
  , testCase "self-data-no-mix-none"
             (getArgs >>= shouldFail . sraMain . tixOnly)
  , testCase "self-data-no-mix-one"
             (do args <- getArgs
                 shouldFail (sraMain ((tixOnly args) {sra_mixs=["foo"]})))
  , testCase "self-data-no-all-two"
             (do args <- getArgs
                 let args' = (tixOnly args) { sra_mixs = ["foo", "bar"] }
                 shouldFail (sraMain args'))
  ]
  where
    tixOnly sra = emptySRA { sra_tix = sra_tix sra }


-- ------------------------------------------------------------------------
--
-- Auxiliary functions
--
-- ------------------------------------------------------------------------

isTestInTest :: IO Bool
isTestInTest = isJust <$> lookupEnv testInTestKey

testInTestKey :: String
testInTestKey = "HPC_CODECOV_TEST_IN_TEST"

findUnder :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findUnder test root = foldDir f Nothing [root]
  where
    f _    acc@(Just _) = return acc
    f path Nothing =
      if test path
         then return (Just path)
         else return Nothing

foldDir :: (FilePath -> a -> IO a) -> a -> [FilePath] -> IO a
foldDir f = go
  where
    go acc [] = return acc
    go acc (dir:dirs) | isIgnoredPattern dir = go acc dirs
    go acc (dir:dirs) = do
       acc' <- f dir acc
       is_dir <- doesDirectoryExist dir
       if not is_dir
          then go acc' dirs
          else do
             contents <- map (dir </>) <$> listDirectory dir
             acc'' <- go acc' contents
             go acc'' dirs

-- | Pattern to ignore when walking directory tree.
isIgnoredPattern :: FilePath -> Bool
isIgnoredPattern path = takeFileName path `elem` [".git", ".hg", "_darcs"]

-- | Wrapper to run 'Trace.Hpc.Codecov.Main.main' with given argument
-- strings.
main' :: [String] -> IO ()
main' args = withArgs args HpcCodecov.main

sraMain :: SRA -> IO ()
sraMain sra = main' args
  where
    args =
      map ("--mix=" ++) (sra_mixs sra) ++
      map ("--exclude=" ++) (sra_excludes sra) ++
      maybe [] (\p -> ["--out=" ++ p]) (sra_out sra) ++
      ["--verbose" | sra_verbose sra] ++
      [sra_tix sra]

-- | Run test with path to temporary file.
withTempFile :: (IO FilePath -> TestTree) -> TestTree
withTempFile = withResource acquire release
  where
    acquire = do (path,hdl) <- openTempFile "." "test.tmp"
                 hClose hdl
                 return path
    release path = removeFile path

withTempDir :: (IO FilePath -> TestTree) -> TestTree
withTempDir = withResource acquire release
  where
     acquire = do
       tool <- getBuildTool
       dir <- case tool of
         Stack   -> pure ".hpc_codecov_test_tmp_stack"
         CabalV1 -> do
           tmpdir <- getTemporaryDirectory
           pure (tmpdir </> "hpc_codecov_test_tmp_cabal_v1")
         CabalV2 -> pure ".hpc_codecov_test_tmp_cabal_v2"
         _       -> error $ "Unsupported tool: " ++ show tool
       exists <- doesDirectoryExist dir
       when exists $ removeDirectoryRecursive dir
       pure dir
     release _ = return ()

-- | Pass the HUnit test when an exception was thrown, otherwise a
-- test failure.
shouldFail :: IO a -> IO ()
shouldFail act =
  do et_err <- try act
     case et_err of
       Left (SomeException {}) -> return ()
       _                       -> assertFailure "should fail"
