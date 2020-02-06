-- |
-- Module:     Trace.Hpc.Codecov.Error
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Errors for @hpc-codecov@.
--

module Trace.Hpc.Codecov.Error
  (
    -- * Exception data type and handler
    HpcCodecovError(..)
  , withHpcCodecovHandler

    -- * Re-exports
  , throw
  , throwIO
  ) where

-- base
import Control.Exception  (Exception (..), handle, throw, throwIO)
import System.Environment (getProgName)
import System.Exit        (exitFailure)

-- | Run action with 'HpcCodecovError' handler.
--
-- Run the given action with a handler for 'HpcCodecovError'. The
-- handler will show a brief usage and call 'exitFailure' when an
-- exception was caught.
withHpcCodecovHandler :: IO a   -- ^ Action to perform.
                      -> IO a
withHpcCodecovHandler = handle handler
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
  | TixNotFound FilePath
  | MixNotFound FilePath [FilePath]
  | SrcNotFound FilePath [FilePath]
  | InvalidArgs [String]
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
