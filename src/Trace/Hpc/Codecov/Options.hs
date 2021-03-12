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
import Control.Exception        (throw)
import Data.Version             (showVersion)
import System.Console.GetOpt    (ArgDescr (..), ArgOrder (..),
                                 OptDescr (..), getOpt, usageInfo)
import System.Environment       (getProgName)

-- Internal
import Paths_hpc_codecov        (version)
import Trace.Hpc.Codecov.Error
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
  , optShowVersion :: Bool
    -- ^ Flag for showing version.
  , optShowNumeric :: Bool
    -- ^ Flag for showing numeric version.
  , optShowHelp    :: Bool
    -- ^ Flag for showing help message.
  } deriving (Eq, Show)

-- | Empty 'Options'.
emptyOptions :: Options
emptyOptions = Options
  { optTix = throw NoTixFile
  , optMixDirs = []
  , optSrcDirs = []
  , optExcludes = []
  , optOutFile = Nothing
  , optVerbose = False
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
            (ReqArg (\d o -> o {optMixDirs = d : optMixDirs o})
                    "DIR")
            ".mix file directory, can repeat\n\
            \default is .hpc"
  , Option ['s'] ["srcdir"]
           (ReqArg (\d o -> o {optSrcDirs = d : optSrcDirs o})
                   "DIR")
           "source directory, can repeat\n\
           \default is current directory"
  , Option ['x'] ["exclude"]
           (ReqArg (\m o -> o {optExcludes = m : optExcludes o})
                   "MODULE")
           "module name to exclude, can repeat"
  , Option ['o'] ["out"]
           (ReqArg (\p o -> o {optOutFile = Just p}) "FILE")
           "output file\n\
           \default is stdout"
  , Option ['v'] ["verbose"]
           (NoArg (\o -> o {optVerbose = True}))
           "show verbose output"
  , Option [] ["version"]
           (NoArg (\o -> o {optShowVersion = True}))
           "show versoin and exit"
  , Option [] ["numeric-version"]
           (NoArg (\o -> o {optShowNumeric = True}))
           "show numeric version and exit"
  , Option ['h'] ["help"]
           (NoArg (\o -> o {optShowHelp = True}))
           "show this help"
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

-- | Make a 'Report' value from 'Optoins'.
opt2rpt :: Options -> Report
opt2rpt opt = Report
  { reportTix = optTix opt
  , reportMixDirs = optMixDirs opt
  , reportSrcDirs = optSrcDirs opt
  , reportExcludes = optExcludes opt
  , reportOutFile = optOutFile opt
  , reportVerbose = optVerbose opt
  }

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
helpMessage name = usageInfo header options
  where
    header = "\
\Generate Codecov JSON coverage report from .tix and .mix files\n\
\\n\
\Usage: \n\
\\n\
\   " ++ name ++ " [OPTIONS] TIX_FILE\n\
\\n\
\Options:\n"

-- | String representation of the version number of this package.
versionString :: String
versionString = showVersion version
