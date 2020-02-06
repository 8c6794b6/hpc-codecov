-- |
-- Module:     Trace.Hpc.Codecov.Main
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Main function for @hpc-codecov@.
--
module Trace.Hpc.Codecov.Main (main) where

-- base
import System.Environment        (getArgs)

-- Internal
import Trace.Hpc.Codecov.Error
import Trace.Hpc.Codecov.Options
import Trace.Hpc.Codecov.Report

-- | The main function for @hpc-codecov@ executable.
main :: IO ()
main = withHpcCodecovHandler (getArgs >>= go)
  where
    go args =
      case parseOptions args of
        Right opts | optShowHelp opts -> printHelp
                   | optShowVersion opts -> putStrLn versionString
                   | otherwise -> genReport opts
        Left errs -> throwIO (InvalidArgs errs)
