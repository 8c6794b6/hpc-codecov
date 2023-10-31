-- |
-- Module:     Trace.Hpc.Codecov.Report
-- Copyright:  (c) 2022 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Generate Codecov report data.

module Trace.Hpc.Codecov.Report
  ( -- * Types
    Report(..)
  , CoverageEntry(..)
  , Format(..)
  , LineHits
  , Hit(..)
  , FunctionHits
  , BranchHits

    -- * Functions
  , genReport
  , genCoverageEntries
  , emitCoverage
  ) where

-- base
import Control.Monad                  (when)
import System.IO                      (IOMode (..), hPutStrLn, stderr,
                                       stdout, withFile)

-- bytestring
import Data.ByteString.Builder        (hPutBuilder)

-- Internal
import Trace.Hpc.Codecov.Report.Emit
import Trace.Hpc.Codecov.Report.Entry


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Generate report data from options.
genReport :: Report -> IO ()
genReport rpt = do
  let mb_out = reportOutFile rpt
      oname = maybe "stdout" show mb_out
      say = when (reportVerbose rpt) . hPutStrLn stderr
  entries <- genCoverageEntries rpt
  say ("Writing report to " ++ oname)
  emitCoverage (reportFormat rpt) mb_out entries
  say "Done"

-- | Generate test coverage entries.
genCoverageEntries :: Report -> IO [CoverageEntry]
genCoverageEntries rpt = readTixFile rpt (reportTix rpt) >>= tixToCoverage rpt

-- | Emit simple coverage data.
emitCoverage
  :: Format
  -- ^ Format of the report.
  --
  -- @since 0.4.0.0
  -> Maybe FilePath
  -- ^ 'Just' output file name, or 'Nothing' for 'stdout'.
  -> [CoverageEntry]
  -- ^ Coverage entries to write.
  -> IO ()
emitCoverage fmt mb_outfile entries = wrap emit
  where
    wrap = maybe ($ stdout) (`withFile` WriteMode) mb_outfile
    emit = flip hPutBuilder (builder entries)
    builder = case fmt of
      Codecov   -> buildCodecov
      Lcov      -> buildLcov
      Cobertura -> buildCobertura
