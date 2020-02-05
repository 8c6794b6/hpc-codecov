-- | Test codes for running @hpc-codecov@ executable.
module Test.Main (main) where

-- base
import           Control.Exception         (SomeException (..), try)
import           System.Environment        (withArgs)
import           System.IO                 (hClose, openTempFile)

-- filepath
import           System.FilePath           (dropExtension, (<.>), (</>))

-- directory
import           System.Directory          (removeDirectoryRecursive,
                                            removeFile)

-- tar
import           Codec.Archive.Tar         (extract)

-- tasty
import           Test.Tasty                (TestTree, defaultMain,
                                            testGroup, withResource)
import           Test.Tasty.HUnit

-- Internal
import qualified Trace.Hpc.Codecov.Main    as HpcCodecov
import           Trace.Hpc.Codecov.Options


-- ------------------------------------------------------------------------
--
-- Tests
--
-- ------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "main"
            [ cmdline
            , withTestTarData "self.tar" selfReport
            , withTestTarData "reciprocal.tar" recipReport
            ]

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
  ]

selfReport :: TestTree
selfReport = testGroup "self"
  [ testCase "self-data-to-stdout"
             (main' selfHpcDataArgs)
  , testCase "self-data-to-stdout-verbose"
             (main' ("--verbose":selfHpcDataArgs))
  , withTempFile
      (\getPath ->
         testCase "self-data-to-file-verbose"
                  (do path <- getPath
                      main' (["--out=" ++ path, "--verbose"] ++
                             selfHpcDataArgs)))
  , testCase "self-data-no-mix-none"
             (shouldFail (main' [selfTix]))
  , testCase "self-data-no-mix-one"
             (shouldFail (main' ["--mix=foo", selfTix]))
  , testCase "self-data-no-all-two"
             (shouldFail (main' ["--mix=foo", "--mix=bar", selfTix]))
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

-- ------------------------------------------------------------------------
--
-- Auxiliary functions
--
-- ------------------------------------------------------------------------

-- | Wrapper to run 'Trace.Hpc.Codecov.Main.main' with given argument
-- strings.
main' :: [String] -> IO ()
main' args = withArgs args HpcCodecov.main

-- | Run given test with mix and tix files of tar file.
--
-- The mix and tix packages are archived as 'test/data/XXX.tar' and
-- added as extra source file in cabal configuration. Unpacked tar
-- contents are removed after running given tests.
withTestTarData :: String   -- ^ Tar file name under 'testDataDir'
                -> TestTree -- ^ Test using the tar contents
                -> TestTree
withTestTarData dot_tar tt = withResource acquire release (const tt)
  where
    acquire = do extract testDataDir (testDataDir </> dot_tar)
                 return (testDataDir </> dropExtension dot_tar)
    release = removeDirectoryRecursive

-- | Run test with path to temporary file.
withTempFile :: (IO FilePath -> TestTree) -> TestTree
withTempFile = withResource acquire release
  where
    acquire = do (path,hdl) <- openTempFile "." "test.tmp"
                 hClose hdl
                 return path
    release path = removeFile path

-- | My package name
me :: String
me = "hpc-codecov-" ++ versionString

-- | Directory path of extracted @self.tar@.
selfDir :: FilePath
selfDir = testDataDir </> "self"

-- | Relative path of tix file in @self.tar@ from project root.
selfTix :: FilePath
selfTix = selfDir </> "tix" </> me </> me <.> "tix"

-- | Arguments for running test with @self.tar@ contents.
selfHpcDataArgs :: [String]
selfHpcDataArgs =
  let mix = selfDir </> "mix" </> me
  in  ["--mix=" ++ mix, "--exclude=Paths_hpc_codecov", selfTix]

-- | Directory containing test data.
testDataDir :: FilePath
testDataDir = "test" </> "data"

-- | Pass the HUnit test when an exception was thrown, otherwise a
-- test failure.
shouldFail :: IO a -> IO ()
shouldFail act =
  do et_err <- try act
     case et_err of
       Left (SomeException {}) -> return ()
       _                       -> assertFailure "should fail"
