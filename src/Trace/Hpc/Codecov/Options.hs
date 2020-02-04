-- |
-- Module:     Trace.Hpc.Codecov.Options
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Options for @hpc-codecov@.
--

module Trace.Hpc.Codecov.Options
  ( Options(..)
  , defaultOptions
  , parseOptions
  , helpMessage
  , versionString
  ) where

-- base
import Data.Version          (showVersion)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt, usageInfo)

-- Internal
import Paths_hpc_codecov     (version)

-- | Options for generating test coverage report.
data Options = Options
  { optTix         :: Maybe FilePath
  , optMixDirs     :: [FilePath]
  , optSrcDirs     :: [FilePath]
  , optExcludes    :: [String]
  , optOutFile     :: Maybe FilePath
  , optVerbose     :: Bool
  , optShowVersion :: Bool
  , optShowHelp    :: Bool
  } deriving (Eq, Show)

-- | The default 'Options'.
defaultOptions :: Options
defaultOptions = Options
  { optTix = Nothing
  , optMixDirs = []
  , optSrcDirs = [""]
  , optExcludes = []
  , optOutFile = Nothing
  , optVerbose = False
  , optShowVersion = False
  , optShowHelp = False
  }

-- | Commandline option oracle.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['m'] ["mixdir"]
            (ReqArg (\d opts -> opts {optMixDirs = d : optMixDirs opts})
                    "DIR")
            ".mix directory, allows repeats"
  , Option ['s'] ["srcdir"]
           (ReqArg (\d opts -> opts {optSrcDirs = d : optSrcDirs opts})
                   "DIR")
           "source directory, allows repeats"
  , Option ['x'] ["exclude"]
           (ReqArg (\m opts -> opts {optExcludes = m : optExcludes opts})
                   "MODULE")
           "module name to exclude, allows repeats"
  , Option ['o'] ["out"]
           (ReqArg (\p opts -> opts {optOutFile = Just p}) "FILE")
           "output file, default is stdout"
  , Option [] ["verbose"]
           (NoArg (\opts -> opts {optVerbose = True}))
           "show verbose output"
  , Option ['v'] ["version"]
           (NoArg (\opts -> opts {optShowVersion = True}))
           "show versoin and exit"
  , Option ['h'] ["help"]
           (NoArg (\opts -> opts {optShowHelp = True}))
           "show this help"
  ]

-- | Parse command line argument and return either error messages or
-- parsed 'Options'.
parseOptions :: [String] -> Either [String] Options
parseOptions args =
  case getOpt Permute options args of
    (flags, rest, []) ->
      -- Not returning error messages with missing ".tix" file
      -- argument at this point, to show help and version messages
      -- without specifying ".tix" file.
      let opts = foldr (.) id flags $ defaultOptions
      in  case rest of
            []      -> Right opts
            (tix:_) -> Right (opts {optTix = Just tix})
    (_, _, errs)  -> Left errs

-- | Help message for command line output.
helpMessage :: String -- ^ Executable program name.
            -> String
helpMessage name = usageInfo header options
  where
    header = "USAGE: " ++ name ++ " [OPTIONS] TIX_FILE\n\
\\n\
\  Generate codecov.io coverage report from hpc tix and mix files \n\
\\n\
\OPTIONS:\n"

-- | String representation of version number.
versionString :: String
versionString = showVersion version
