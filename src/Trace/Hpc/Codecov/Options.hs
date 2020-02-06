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
  , emptyOptions
  , parseOptions
  , printHelp
  , helpMessage
  , versionString
  ) where

-- base
import Data.Version          (showVersion)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt, usageInfo)
import System.Environment    (getProgName)

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

-- | Empty 'Options'.
emptyOptions :: Options
emptyOptions = Options
  { optTix = Nothing
  , optMixDirs = []
  , optSrcDirs = []
  , optExcludes = []
  , optOutFile = Nothing
  , optVerbose = False
  , optShowVersion = False
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
  , Option [] ["verbose"]
           (NoArg (\o -> o {optVerbose = True}))
           "show verbose output"
  , Option ['v'] ["version"]
           (NoArg (\o -> o {optShowVersion = True}))
           "show versoin and exit"
  , Option ['h'] ["help"]
           (NoArg (\o -> o {optShowHelp = True}))
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
      let opts0 = foldr (.) id flags $ emptyOptions
          opts1 = fillDefaultIfNotGiven opts0
      in  case rest of
            []      -> Right opts1
            (tix:_) -> Right (opts1 {optTix = Just tix})
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

-- | Print help message.
printHelp :: IO ()
printHelp = getProgName >>= putStrLn . helpMessage

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

-- | String representation of version number.
versionString :: String
versionString = showVersion version
