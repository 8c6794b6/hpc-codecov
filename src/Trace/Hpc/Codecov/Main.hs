-- |
-- Module:     Trace.Hpc.Codecov.Main
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Main function for @hpc-codecov@.
--
module Trace.Hpc.Codecov.Main
  ( defaultMain
  ) where

-- base
import System.Environment        (getArgs, getProgName)
import System.Exit               (exitFailure)

-- Internal
import Trace.Hpc.Codecov.Options
import Trace.Hpc.Codecov.Report

-- | The main function for @hpc-codecov@ executable.
defaultMain :: IO ()
defaultMain =
  do args <- getArgs
     let printHelp = getProgName >>= putStrLn . helpMessage
     case parseOptions args of
       Right opts | optShowHelp opts -> printHelp
                  | optShowVersion opts -> putStrLn versionString
                  | otherwise -> genReport opts
       Left errs  -> do putStrLn "Error: "
                        mapM_ putStr (map ("  " ++) errs)
                        exitFailure
