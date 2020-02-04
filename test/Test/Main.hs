-- | Test codes for running @hpc-codecov@ executable.
module Test.Main (main) where

-- base
import           Control.Exception         (SomeException (..), try)
import           System.Environment        (withArgs)

-- filepath
import           System.FilePath           ((<.>), (</>))

-- directory
import           System.Directory          (removeDirectoryRecursive)

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
tests = withSelfHpcData (testGroup "run" [cmdline])

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

cmdline :: TestTree
cmdline = testGroup "cmdline"
  [ testCase "Passing non-existing option"
             (shouldFail (withArgs ["--foo"] defaultMain))
  , testCase "Passing tix without file"
             (shouldFail (withArgs ["--tix"] defaultMain))
  , testCase "Help command show help and exit"
             (withArgs ["--help"] defaultMain)
  , testCase "Version command show version and exit"
             (withArgs ["--version"] defaultMain)
  , testCase "Reading own data, print to stdout"
             (withArgs selfHpcDataArgs defaultMain)
  , testCase "Reading own data, print to file"
             (withArgs ("--out=reports" : selfHpcDataArgs) defaultMain)
  ]

testDataDir :: FilePath
testDataDir = "test" </> "data"

selfHpcDataArgs :: [String]
selfHpcDataArgs =
  let self = testDataDir </> "self"
      me = "hpc-codecov-" ++ versionString
      tix = self </> "tix" </> me </> me <.> "tix"
      mix = self </> "mix" </> me
  in  ["--tix=" ++ tix, "--mix=" ++ mix]

-- Auxiliary functions

-- | Pass the HUnit test when an exception was thrown, otherwise a
-- test failure.
shouldFail :: IO a -> IO ()
shouldFail act =
  do et_err <- try act
     case et_err of
       Left (SomeException {}) -> return ()
       _                       -> assertFailure "should fail"
