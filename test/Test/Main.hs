{-# LANGUAGE CPP #-}
-- | Test codes for running @hpc-codecov@ executable.
module Test.Main (main) where

-- base
import           Control.Exception          (SomeException (..), try)
import           Control.Monad              (when)
import           Data.List                  (isSubsequenceOf)
import           Data.Maybe                 (fromMaybe, isJust)
import           System.Environment         (getExecutablePath, lookupEnv,
                                             setEnv, unsetEnv, withArgs)
import           System.Exit                (ExitCode)
import           System.IO                  (hClose, openTempFile)

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                ((<>))
#endif


-- filepath
import           System.FilePath            (takeFileName, (</>))

-- directory
import           System.Directory           (canonicalizePath,
                                             doesDirectoryExist,
                                             listDirectory,
                                             removeDirectoryRecursive,
                                             removeFile,
                                             withCurrentDirectory)

-- process
import           System.Process             (CreateProcess (..),
                                             callProcess, shell,
                                             waitForProcess,
                                             withCreateProcess)

-- tasty
import           Test.Tasty                 (TestTree, defaultMain,
                                             testGroup, withResource)
import           Test.Tasty.HUnit           (assertEqual, assertFailure,
                                             testCase)

-- Internal
import           Trace.Hpc.Codecov.Discover
import qualified Trace.Hpc.Codecov.Main     as HpcCodecov
import           Trace.Hpc.Codecov.Report


-- ------------------------------------------------------------------------
--
-- Tests
--
-- ------------------------------------------------------------------------

main :: IO ()
main = do
  test_in_test <- isTestInTest
  mb_tool <- getBuildTool

  when test_in_test $ putStr $ unlines
    [ ""
    , "================================="
    , "Running test to generate hpc data"
    , "================================="
    , "" ]

  defaultMain $ testGroup "main" $
    [reportTest , cmdline, recipReport] ++
    [selfReportTest | not test_in_test, isJust mb_tool] ++
    [discoverStackTest | not test_in_test, mb_tool == Just Stack] ++
    [discoverCabalTest | not test_in_test, mb_tool == Just Cabal]

reportTest :: TestTree
reportTest = testGroup "report"
  [ testCase "mempty" $ do
      shouldFail (print $ reportTix mempty)
      assertEqual "empty mix dirs" (reportMixDirs mempty) []
      assertEqual "empty src dirs" (reportSrcDirs mempty) []
      assertEqual "non verbose" (reportVerbose mempty) False
      let r1 = mempty { reportExcludes = ["M1"] }
          r2 = mempty { reportExcludes = ["M2", "M3"]
                      , reportVerbose = True }
          r3 = r1 <> r2
      assertEqual "<> for verbose" (reportVerbose r3) True
      assertEqual "<> for excludes" (reportExcludes r3) ["M1","M2","M3"]
  ]

cmdline :: TestTree
cmdline = testGroup "cmdline"
  [ testCase "non-existing-option"
             (shouldFail (main' ["--foo"]))
  , testCase "non-existing-options"
             (shouldFail (main' ["--foo", "--bar", "--buzz"]))
  , testCase "mixdir-without-argument"
             (shouldFail (main' ["--mix"]))
  , testCase "no-tix-file"
             (shouldFail (main' []))
  , testCase "non-existing-tix"
             (shouldFail (main' ["no_such_file.tix"]))
  , testCase "invalid-build-tool"
              (shouldFail (main' ["foo:tests"]))
  , testCase "invalid-test-suite"
              (shouldFail (main' ["cabal:no-such-test"]))
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

getBuildTool :: IO (Maybe BuildTool)
getBuildTool = do
  path <- getExecutablePath
  if ".stack-work" `isSubsequenceOf` path
     then return $ Just Stack
     else if "dist-newstyle" `isSubsequenceOf` path
       then return $ Just Cabal
       else return Nothing

getSelfReportArgs :: FilePath -> IO SRA
getSelfReportArgs builddir = do
  mb_tool <- getBuildTool
  case mb_tool of
    Just Stack -> getSelfReportStackArgs builddir
    Just Cabal -> getSelfReportCabalArgs setupV2 builddir
    _          -> error "Cannot determine build tool"

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

-- setupV1 :: FilePath -> IO ()
-- setupV1 bd = bracket acquire release work
--   where
--     -- Cabal v1 style build complains when "GHC_PACKAGE_PATH"
--     -- environment were set, manually removing the variable.
--     key = "GHC_PACKAGE_PATH"

--     acquire = do
--       mb_ghc_pkg_path <- lookupEnv key
--       putStrLn $ "GHC_PACKAGE_PATH: " ++ show mb_ghc_pkg_path
--       unsetEnv key
--       pure mb_ghc_pkg_path

--     release = mapM_ (setEnv key)

--     work _ = do
--       let setup args = callProcess "runhaskell" ("Setup.hs" : args)

--       setup [ "configure", "--builddir=" ++ bd
--             , "--enable-test", "--enable-coverage"
--             , "--package-db=/build/package.conf.d" ]

--       setup ["build", "--builddir=" ++ bd]
--       setup ["test", "--builddir=" ++ bd]

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
-- Discover tests
--
-- ------------------------------------------------------------------------

discoverStackTest :: TestTree
discoverStackTest =
  let t = buildAndTestWithStack
      withProject name act = case getAcquireAndRelease Stack name [] of
        (a,r) -> withResource a r (const act)
  in  testGroup "discover_stack"
        [ t "project1"
          [ "--root=" ++ testData "project1"
          , "--verbose"
          , "stack:project1-test"]
          []
        , t "project1"
          [ "--root=" ++ testData "project1"
          , "--verbose"
          , "--build=dot-stack-work"
          , "stack:project1-test.tix"]
          ["--work-dir=dot-stack-work"]

        , withProject "project1" $
            testCase "project1" $
              withCurrentDirectory (testData "project1") $ main'
                [ "--verbose", "stack:project1-test" ]

        , withProject "project1" $
            withResource
              (findUnder (\p -> takeFileName p == "project1-test.tix")
                         (testData "project1" </> ".stack-work"))
              (\_ -> pure ())
              (\getTixPath -> testCase "project1" $ do
                 tix_path <- fromMaybe "project1-test.tix" <$> getTixPath
                 canonical_tix_path <- canonicalizePath tix_path
                 putStrLn $
                   "tix_path: " ++ tix_path ++ "\n" ++
                   "canonical_tix_path: " ++ canonical_tix_path
                 main' [ "--verbose", "stack:" ++ canonical_tix_path ])
        ]

discoverCabalTest :: TestTree
discoverCabalTest =
  let t = buildAndTestWithCabal
      withProject name act = case getAcquireAndRelease Cabal name [] of
        (a,r) -> withResource a r (const act)
  in  testGroup "discover_cabal"
        [ t "project1"
          [ "--root=" ++ testData "project1"
          , "--verbose"
          , "-x", "Paths_project1"
          , "-X", "project1-exe"
          , "cabal:project1-test" ]
          []

        , withProject "project1" $
            withResource
              (findUnder (\p -> takeFileName p == "project1-test.tix")
                (testData "project1" </> "dist-newstyle"))
              (\_ -> pure ())
              (\getTixPath -> testCase "project1" $ do
                  tix_path <- fromMaybe "project1-test.tix" <$> getTixPath
                  canonical_tix_path <- canonicalizePath tix_path
                  main' [ "--verbose"
                        , "-x", "Main,Paths_project1"
                        , "cabal:" ++ canonical_tix_path])
        ]

buildAndTestWithStack :: String -> [String] -> [String] -> TestTree
buildAndTestWithStack = buildAndTestWith Stack

buildAndTestWithCabal :: String -> [String] -> [String] -> TestTree
buildAndTestWithCabal = buildAndTestWith Cabal

buildAndTestWith :: BuildTool -> String -> [String] -> [String] -> TestTree
buildAndTestWith tool name args tool_args = withResource acquire release work
  where
    (acquire, release) = getAcquireAndRelease tool name tool_args
    work _ec = testCase name $ main' args

getAcquireAndRelease
  :: BuildTool -> String -> [String] -> (IO ExitCode, ExitCode -> IO ())
getAcquireAndRelease tool name tool_args = (acquire, release)
  where
    dir = testData name

    acq_cmd = case tool of
      Stack -> ("stack", ["test", "--coverage"])
      Cabal -> ("cabal", ["test", "--enable-coverage"])

    call (cmd,args) = callProcessIn dir cmd (tool_args ++ args)

    acquire = do
      putStrLn $ "Testing " ++ name ++ " with " ++ show tool ++ " ..."
      ec <- call acq_cmd
      putStrLn $ "ec: " ++ show ec
      return ec

    release _ec = pure ()

callProcessIn :: FilePath -> String -> [String] -> IO ExitCode
callProcessIn dir cmd args = withCreateProcess cp f
  where
    f _ _ _ = waitForProcess
    cp = (shell (unwords (cmd : args))) {cwd = Just dir}


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

-- | Wrapper to run 'Trace.Hpc.Codecov.Main.main' with given argument
-- strings.
main' :: [String] -> IO ()
main' args = withArgs args HpcCodecov.defaultMain

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
       mb_tool <- getBuildTool
       dir <- case mb_tool of
         Just Stack -> pure ".hpc_codecov_test_tmp_stack"
         Just Cabal -> pure ".hpc_codecov_test_tmp_cabal_v2"
         _          -> error "Cannot determine build tool"
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
       Left SomeException {} -> return ()
       _                     -> assertFailure "should fail"

-- | Get directory under test data.
testData :: String -> FilePath
testData dir = "test" </> "data" </> dir
