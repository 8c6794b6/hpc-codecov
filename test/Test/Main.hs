-- | Test codes for running @hpc-codecov@ executable.
module Test.Main (main) where

-- base
import           Control.Exception         (SomeException (..), try)
import           System.Environment        (withArgs)
import           System.IO                 (hClose, openTempFile)

-- filepath
import           System.FilePath           ((<.>), (</>))

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "run" [cmdline, withSelfHpcData report]

report :: TestTree
report = testGroup "main"
  [ testCase "self-data-to-stdout" (main' selfHpcDataArgs)
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

main' :: [String] -> IO ()
main' args = withArgs args HpcCodecov.main

-- Run given test with mix and tix files of hpc-codecov package.
--
-- The mix and tix packages are archived as 'test/data/self.tar' and
-- added as extra source file in cabal configuration. Unpacked tar
-- contents are removed after running given tests.
withSelfHpcData :: TestTree -> TestTree
withSelfHpcData tt = withResource acquire release (const tt)
  where
    acquire = do extract testDataDir (testDataDir </> "self.tar")
                 return (testDataDir </> "self")
    release = removeDirectoryRecursive

-- | Run test with path to temporary file.
withTempFile :: (IO FilePath -> TestTree) -> TestTree
withTempFile = withResource acquire release
  where
    acquire = do (path,hdl) <- openTempFile "." "test.tmp"
                 hClose hdl
                 return path
    release path = removeFile path

testDataDir :: FilePath
testDataDir = "test" </> "data"

-- My package name
me :: String
me = "hpc-codecov-" ++ versionString

selfDir :: FilePath
selfDir = testDataDir </> "self"

selfTix :: FilePath
selfTix = selfDir </> "tix" </> me </> me <.> "tix"

selfHpcDataArgs :: [String]
selfHpcDataArgs =
  let mix = selfDir </> "mix" </> me
  in  ["--mix=" ++ mix, "--exclude=Paths_hpc_codecov", selfTix]

-- | Pass the HUnit test when an exception was thrown, otherwise a
-- test failure.
shouldFail :: IO a -> IO ()
shouldFail act =
  do et_err <- try act
     case et_err of
       Left (SomeException {}) -> return ()
       _                       -> assertFailure "should fail"
