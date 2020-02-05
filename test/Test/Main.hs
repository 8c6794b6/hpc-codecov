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
import           Test.Tasty                (TestTree, testGroup,
                                            withResource)
import qualified Test.Tasty                as Tasty
import           Test.Tasty.HUnit

-- Internal
import           Trace.Hpc.Codecov.Main
import           Trace.Hpc.Codecov.Options

main :: IO ()
main = Tasty.defaultMain tests

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
  ]

cmdline :: TestTree
cmdline = testGroup "cmdline"
  [ testCase "non-existing-option"
             (shouldFail (main' ["--foo"]))
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
main' args = withArgs args defaultMain

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

selfHpcDataArgs :: [String]
selfHpcDataArgs =
  let self = testDataDir </> "self"
      me = "hpc-codecov-" ++ versionString
      tix = self </> "tix" </> me </> me <.> "tix"
      mix = self </> "mix" </> me
  in  ["--mix=" ++ mix, "--exclude=Paths_hpc_codecov", tix]

-- | Pass the HUnit test when an exception was thrown, otherwise a
-- test failure.
shouldFail :: IO a -> IO ()
shouldFail act =
  do et_err <- try act
     case et_err of
       Left (SomeException {}) -> return ()
       _                       -> assertFailure "should fail"
