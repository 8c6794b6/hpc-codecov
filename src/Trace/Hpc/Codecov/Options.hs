{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module:     Trace.Hpc.Codecov.Options
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Command line options for generating Codecov test coverage report.

module Trace.Hpc.Codecov.Options
  (
    -- * The Options type and predefined values
    Options(..)
  , defaultOptions
  , emptyOptions

    -- * Command line parser for 'Options'
  , parseOptions

    -- * Converter
  , opt2rpt

    -- * Help message and version number
  , printHelp
  , printVersion
  , versionString
  ) where

-- base
import Control.Exception           (throw, throwIO)
import Data.Version                (showVersion)
import System.Console.GetOpt       (ArgDescr (..), ArgOrder (..),
                                    OptDescr (..), getOpt, usageInfo)
import System.Environment          (getProgName)

-- directory
import System.Directory            (doesFileExist)


-- Internal
import Paths_hpc_codecov           (version)
import Trace.Hpc.Codecov.Discover
import Trace.Hpc.Codecov.Exception
import Trace.Hpc.Codecov.Report

-- | Options for generating test coverage report.
data Options = Options
  { optTix         :: FilePath
    -- ^ Input tix file.
  , optMixDirs     :: [FilePath]
    -- ^ Directory containing mix files referred by the tix file.
  , optSrcDirs     :: [FilePath]
    -- ^ Directory containing source codes referred by the mix files.
  , optExcludes    :: [String]
    -- ^ Module name strings to exclude from coverage report.
  , optOutFile     :: Maybe FilePath
    -- ^ Output file to write JSON report, if given.

  , optVerbose     :: Bool
    -- ^ Flag for showing verbose message during coverage report
    -- generation.

  , optRootDir :: FilePath
    -- ^ Project root directory for the build tool.
  , optBuildDir    :: Maybe FilePath
    -- ^ Name of the build directory used by the build tool
  , optSkipDirs    :: [String]
    -- ^ Directories to ignore while discovering.

  , optShowVersion :: Bool
    -- ^ Flag for showing version.
  , optShowNumeric :: Bool
    -- ^ Flag for showing numeric version.
  , optShowHelp    :: Bool
    -- ^ Flag for showing help message.
  }

-- | Empty 'Options'.
emptyOptions :: Options
emptyOptions = Options
  { optTix = throw NoTarget
  , optMixDirs = []
  , optSrcDirs = []
  , optExcludes = []
  , optOutFile = Nothing
  , optVerbose = False
  , optRootDir = ""
  , optBuildDir = Nothing
  , optSkipDirs = []
  , optShowVersion = False
  , optShowNumeric = False
  , optShowHelp = False
  }

-- | The default 'Options'.
defaultOptions :: Options
defaultOptions = emptyOptions
  { optMixDirs = [".hpc"]
  , optSrcDirs = [""]
  }

-- | Commandline option oracle.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['m'] ["mixdir"]
           (ReqArg (\d o -> o {optMixDirs = uncommas d ++ optMixDirs o})
                   "DIR")
            ".mix file directory, can repeat\n\
            \(default: .hpc)"
  , Option ['s'] ["srcdir"]
           (ReqArg (\d o -> o {optSrcDirs = uncommas d ++ optSrcDirs o})
                   "DIR")
           "Source directory, can repeat\n\
           \(default: current directory)"
  , Option ['x'] ["exclude"]
           (ReqArg (\m o -> o {optExcludes = uncommas m ++ optExcludes o})
                   "MODULE")
           "Module name to exclude, can repeat"
  , Option ['o'] ["out"]
           (ReqArg (\p o -> o {optOutFile = Just p}) "FILE")
           "Output file\n\
           \(default: stdout)"

  , Option ['r'] ["rootdir"]
           (ReqArg (\d o -> o {optRootDir = d})
                   "DIR")
           "Project root directory for TOOL\n\
           \Usually the directory containing\n\
           \'stack.yaml' or 'cabal.project'\n\
           \(default: current directory)"
  , Option ['b'] ["builddir"]
           (ReqArg (\d o -> o {optBuildDir = Just d})
                   "DIR")
           "Name of directory made by the TOOL\n\
           \(default:\n\
           \ - '.stack-work' for stack\n\
           \ - 'dist-newstyle' for cabal)"
  , Option ['X'] ["skipdir"]
           (ReqArg (\d o -> o {optSkipDirs = uncommas d ++ optSkipDirs o})
                   "DIR")
           "Basename of directory to skip while\n\
           \searching data for TOOL, can repeat"

  , Option ['v'] ["verbose"]
           (NoArg (\o -> o {optVerbose = True}))
           "Show verbose output"
  , Option [] ["version"]
           (NoArg (\o -> o {optShowVersion = True}))
           "Show versoin and exit"
  , Option [] ["numeric-version"]
           (NoArg (\o -> o {optShowNumeric = True}))
           "Show numeric version and exit"
  , Option ['h'] ["help"]
           (NoArg (\o -> o {optShowHelp = True}))
           "Show this help and exit"
  ]

-- | Parse command line argument and return either error messages or
-- parsed 'Options'.
parseOptions :: [String] -- ^ Command line argument strings.
             -> Either [String] Options
parseOptions args =
  case getOpt Permute options args of
    (flags, rest, []) ->
      -- Not returning error messages with missing ".tix" file
      -- argument at this point, to show help and version messages
      -- without specifying ".tix" file.
      let opts0 = foldr ($) emptyOptions flags
          opts1 = fillDefaultIfNotGiven opts0
      in  case rest of
            []      -> Right opts1
            (tix:_) -> Right (opts1 {optTix = tix})
    (_, _, errs)  -> Left errs

fillDefaultIfNotGiven :: Options -> Options
fillDefaultIfNotGiven opts = opts
  { optMixDirs = fillIf null optMixDirs
  , optSrcDirs = fillIf null optSrcDirs
  }
 where
   fillIf test fld =
      let orig = fld opts
      in  if test orig
             then fld defaultOptions
             else orig

-- | Representation of @TARGET@ argument.
data Target
  = TixFile FilePath
  | TestSuite BuildTool String

parseTarget :: String -> IO Target
parseTarget str = do
  -- Detecting file existence before separating with ':', to support
  -- directory path containing ':' under Windows.
  file_found <- doesFileExist str
  if file_found
     then pure $ TixFile str
     else case break (== ':') str of
       ("cabal", ':':name) -> pure $ TestSuite Cabal name
       ("stack", ':':name) -> pure $ TestSuite Stack name
       (tool, ':':_)       -> throwIO $ InvalidBuildTool tool
       _                   -> pure $ TixFile str

uncommas :: String -> [String]
uncommas = go
  where
    go str = case break (== ',') str of
      (cs, ',':rest) -> cs : go rest
      (cs, _)        -> [cs]

-- | Make a 'Report' value from 'Optoins'.
opt2rpt :: Options -> IO Report
opt2rpt opt = do
  let rpt1 = mempty
        { reportMixDirs = optMixDirs opt
        , reportSrcDirs = optSrcDirs opt
        , reportExcludes = optExcludes opt
        , reportOutFile = optOutFile opt
        , reportVerbose = verbose
        }
      tix = optTix opt
      verbose = optVerbose opt
  target <- parseTarget tix
  case target of
    TixFile path -> pure (rpt1 {reportTix = path})
    TestSuite tool name -> do
      rpt2 <- discover DiscoverArgs
        { da_tool = tool
        , da_testsuite = name
        , da_rootdir = optRootDir opt
        , da_builddir = optBuildDir opt
        , da_skipdirs = optSkipDirs opt
        , da_verbose = verbose
        }
      pure $ rpt1 `mappend` rpt2

-- | Print help messages.
printHelp :: IO ()
printHelp = getProgName >>= putStrLn . helpMessage

-- | Print version number of this package.
printVersion :: IO ()
printVersion =
  do me <- getProgName
     putStrLn (me ++ " version " ++ versionString)

-- | Help message for command line output.
helpMessage :: String -- ^ Executable program name.
            -> String
helpMessage name = usageInfo header options ++ footer
  where
    header = "USAGE: " ++ name ++ " [OPTIONS] TARGET\n\
\\n\
\Generate Codecov JSON coverage report for Haskell source codes\n\
\from .tix and .mix files made with hpc.\n\
\\n\
\TARGET is either a path to .tix file or 'TOOL:TEST_SUITE'.\n\
\Supported TOOL values are 'stack' and 'cabal'. When the TOOL is\n\
\'stack' and building project with multiple packages, use 'all' as\n\
\TEST_SUITE value to refer the combined report.\n\
\\n\
\OPTIONS:\n"
    footer = "\
\\n\
\For more info, see:\n\
\\n\
\  https://github.com/8c6794b6/hpc-codecov#readme\n\
\\n"

-- | String representation of the version number of this package.
versionString :: String
versionString = showVersion version
