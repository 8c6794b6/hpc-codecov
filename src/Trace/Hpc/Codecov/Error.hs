-- |
-- Module:     Trace.Hpc.Codecov.Error
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Error and exception related codes.

module Trace.Hpc.Codecov.Error
  (
    -- * Exception data type and handler
    HpcCodecovError(..)
  , withBriefUsageOnError
  ) where

-- base
import Control.Exception  (Exception (..), handle)
import System.Environment (getProgName)
import System.Exit        (exitFailure)

-- | Run the given action with a handler for 'HpcCodecovError'.
--
-- The handler will show a brief usage and call 'exitFailure' when an
-- exception was caught.
withBriefUsageOnError :: IO a   -- ^ Action to perform.
                      -> IO a
withBriefUsageOnError = handle handler
  where
    handler :: HpcCodecovError -> IO a
    handler e =
      do putStr ("Error: " ++ displayException e)
         name <- getProgName
         putStrLn ("Run '" ++ name ++ " --help' for usage.")
         exitFailure

-- | Exceptions thrown from @hpc-codecov@.
data HpcCodecovError
  = NoTixFile
   -- ^ Tix file path was not given.
  | TixNotFound FilePath
   -- ^ Tix file path was given, but not found.
  | MixNotFound FilePath [FilePath]
   -- ^ Mix file not found. The first field is the path specified by a
   -- tix file. The second is the searched paths.
  | SrcNotFound FilePath [FilePath]
   -- ^ Like 'MixNotFound', but for source code specified by a mix
   -- file.
  | InvalidArgs [String]
   -- ^ Some errors in command line argument, e.g., required value not
   -- specified.
  deriving (Show)

instance Exception HpcCodecovError where
  displayException = hpcCodecovErrorMessage

hpcCodecovErrorMessage :: HpcCodecovError -> String
hpcCodecovErrorMessage e =
  case e of
    NoTixFile -> "no .tix file given\n"
    TixNotFound tix -> "cannot find tix: " ++ show tix ++ "\n"
    MixNotFound mix locs -> searchedLocations "mix" mix locs
    SrcNotFound src locs -> searchedLocations "src" src locs
    InvalidArgs msgs ->
      case msgs of
        [x] -> x
        _   -> '\n' : concatMap ("  - " ++) msgs

searchedLocations :: String -> FilePath -> [FilePath] -> String
searchedLocations what path locs =
  "cannot find " ++ what ++ ": " ++ show path ++ locs'
  where
    locs' =
      case locs of
       [_] -> searched ""
       _   -> searched "s"
    searched post =
      "\nsearched location" ++ post ++ ":\n" ++
      unlines (map ("  " ++) locs)
