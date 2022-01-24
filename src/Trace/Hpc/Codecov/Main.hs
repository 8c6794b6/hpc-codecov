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
import Control.Exception           (throwIO)
import System.Environment          (getArgs)

-- Internal
import Trace.Hpc.Codecov.Exception
import Trace.Hpc.Codecov.Options
import Trace.Hpc.Codecov.Report

-- | The main function for @hpc-codecov@ executable.
defaultMain :: IO ()
defaultMain = withBriefUsageOnError (getArgs >>= go)
  where
    go args =
      case parseOptions args of
        Right opts | optShowHelp opts    -> printHelp
                   | optShowVersion opts -> printVersion
                   | optShowNumeric opts -> putStrLn versionString
                   | otherwise           -> opt2rpt opts >>= genReport
        Left errs -> throwIO (InvalidArgs errs)
