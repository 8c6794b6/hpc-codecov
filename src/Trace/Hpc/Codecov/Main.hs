-- |
-- Module:     Trace.Hpc.Codecov.Main
-- Copyright:  (c) 2022 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Main function for @hpc-codecov@.
--
module Trace.Hpc.Codecov.Main (defaultMain) where

-- base
import Control.Exception           (Exception (..), handle, throwIO)
import System.Environment          (getArgs, getProgName)
import System.Exit                 (exitFailure)

-- Internal
import Trace.Hpc.Codecov.Exception
import Trace.Hpc.Codecov.Options
import Trace.Hpc.Codecov.Report

-- | The main function for @hpc-codecov@ executable.
defaultMain :: IO ()
defaultMain = handle handler (getArgs >>= go)
  where
    go args =
      case parseOptions args of
        Right opts | optShowHelp opts    -> printHelp
                   | optShowVersion opts -> printVersion
                   | optShowNumeric opts -> printNumericVersion
                   | otherwise           -> opt2rpt opts >>= genReport
        Left errs -> throwIO (InvalidArgs errs)

    handler :: HpcCodecovError -> IO a
    handler e =
      do putStr ("Error: " ++ displayException e)
         name <- getProgName
         putStrLn ("Run '" ++ name ++ " --help' for usage.")
         exitFailure
