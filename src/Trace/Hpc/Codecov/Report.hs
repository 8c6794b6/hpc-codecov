{-# LANGUAGE CPP #-}
-- |
-- Module:     Trace.Hpc.Codecov.Report
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Generate Codecov report data.

module Trace.Hpc.Codecov.Report
  ( -- * Types
    Report(..)
  , CoverageEntry(..)
  , LineHits
  , Hit(..)

    -- * Functions
  , genReport
  , genCoverageEntries
  , emitCoverageJSON
  ) where

-- base
import Control.Exception       (ErrorCall, handle, throw, throwIO)
import Control.Monad           (mplus, when)
import Control.Monad.ST        (ST)
import Data.Function           (on)
import Data.List               (foldl', intersperse)
import System.IO               (IOMode (..), hPutStrLn, stderr, stdout,
                                withFile)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid             ((<>))
#endif

-- array
import Data.Array.Base         (unsafeAt)
import Data.Array.IArray       (bounds, listArray, range, (!))
import Data.Array.MArray       (newArray, readArray, writeArray)
import Data.Array.ST           (STUArray, runSTUArray)
import Data.Array.Unboxed      (UArray)

-- bytestring
import Data.ByteString.Builder (Builder, char7, hPutBuilder, intDec,
                                string7, stringUtf8)

-- directory
import System.Directory        (doesFileExist)

-- filepath
import System.FilePath         ((<.>), (</>))

-- hpc
import Trace.Hpc.Mix           (BoxLabel (..), Mix (..), MixEntry, readMix)
import Trace.Hpc.Tix           (Tix (..), TixModule (..), readTix)
import Trace.Hpc.Util          (HpcPos, fromHpcPos)

-- Internal
import Trace.Hpc.Codecov.Error


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Data type to hold information for generating test coverage
-- report.
data Report = Report
 { reportTix      :: FilePath
   -- ^ Input tix file.
 , reportMixDirs  :: [FilePath]
   -- ^ Directories containing mix files referred by the tix file.
 , reportSrcDirs  :: [FilePath]
   -- ^ Directories containing source codes referred by the mix files.
 , reportExcludes :: [String]
   -- ^ Module name strings to exclude from coverage report.
 , reportOutFile  :: Maybe FilePath
   -- ^ Output file to write JSON report, if given.
 , reportVerbose  :: Bool
   -- ^ Flag for showing verbose message during report generation.
 } deriving (Eq, Show)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Report where
  (<>) = mappendReport
#endif

instance Monoid Report where
  mempty = emptyReport
  mappend = mappendReport

emptyReport :: Report
emptyReport = Report
  { reportTix = throw NoTarget
  , reportMixDirs = []
  , reportSrcDirs = []
  , reportExcludes = []
  , reportOutFile = Nothing
  , reportVerbose = False
  }

mappendReport :: Report -> Report -> Report
mappendReport r1 r2 =
  let extend f = ((<>) `on` f) r1 r2
  in  Report { reportTix = reportTix r2
             , reportMixDirs = extend reportMixDirs
             , reportSrcDirs = extend reportSrcDirs
             , reportExcludes = extend reportExcludes
             , reportOutFile = (mplus `on` reportOutFile) r1 r2
             , reportVerbose = ((||) `on` reportVerbose) r1 r2
             }

-- | Single file entry in coverage report.
--
-- See the
-- <https://docs.codecov.io/docs/codecov-custom-coverage-format Codecov documentation>
-- for detail.
data CoverageEntry =
  CoverageEntry { ce_filename :: FilePath -- ^ Source code file name.
                , ce_hits     :: LineHits -- ^ Line hits of the file.
                } deriving (Eq, Show)

-- | Pair of line number and hit tag.
type LineHits = [(Int, Hit)]

-- | Data type to represent coverage of source code line.
data Hit
  = Missed  -- ^ The line is not covered at all.
  | Partial -- ^ The line is partially covered.
  | Full    -- ^ The line is fully covered.
  deriving (Eq, Show)

-- | Generate report data from options.
genReport :: Report -> IO ()
genReport rpt =
  do entries <- genCoverageEntries rpt
     let mb_out = reportOutFile rpt
         oname = maybe "stdout" show mb_out
     say rpt ("Writing JSON report to " ++ oname)
     emitCoverageJSON mb_out entries
     say rpt "Done"

-- | Generate test coverage entries.
genCoverageEntries :: Report -> IO [CoverageEntry]
genCoverageEntries rpt =
  readTixFile rpt (reportTix rpt) >>= tixToCoverage rpt

-- | Emit simple coverage JSON data.
emitCoverageJSON ::
  Maybe FilePath -- ^ 'Just' output file name, or 'Nothing' for
                 -- 'stdout'.
  -> [CoverageEntry] -- ^ Coverage entries to write.
  -> IO ()
emitCoverageJSON mb_outfile entries = wrap emit
  where
    wrap = maybe ($ stdout) (`withFile` WriteMode) mb_outfile
    emit = flip hPutBuilder (buildJSON entries)


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

-- | Build simple JSON report from coverage entries.
buildJSON :: [CoverageEntry] -> Builder
buildJSON entries = contents
  where
    contents =
      braced (key (string7 "coverage") <>
              braced (listify (map report entries))) <>
      char7 '\n'
    report ce =
      key (stringUtf8 (ce_filename ce)) <>
      braced (listify (map hit (ce_hits ce)))
    key x = dquote x <> char7 ':'
    dquote x = char7 '"' <> x <> char7 '"'
    braced x = char7 '{' <> x <> char7 '}'
    listify xs = mconcat (intersperse comma xs)
    comma = char7 ','
    hit (n, tag) =
      case tag of
        Missed  -> k <> char7 '0'
        Partial -> k <> dquote (char7 '1' <> char7 '/' <> char7 '2')
        Full    -> k <> char7 '1'
      where
        k = key (intDec n)

tixToCoverage :: Report -> Tix -> IO [CoverageEntry]
tixToCoverage rpt (Tix tms) = mapM (tixModuleToCoverage rpt)
                                   (excludeModules rpt tms)

tixModuleToCoverage :: Report -> TixModule -> IO CoverageEntry
tixModuleToCoverage rpt tm@(TixModule name _hash _count _ixs) =
  do say rpt ("Search mix:   " ++ name)
     Mix path _ _ _ entries <- readMixFile (reportMixDirs rpt) tm
     say rpt ("Found mix:    "++ path)
     let Info _ min_line max_line hits = makeInfo tm entries
         lineHits = makeLineHits min_line max_line hits
     path' <- ensureSrcPath rpt path
     return (CoverageEntry { ce_filename = path'
                           , ce_hits = lineHits })

-- | Exclude modules specified in given 'Report'.
excludeModules :: Report -> [TixModule] -> [TixModule]
excludeModules rpt = filter exclude
  where
    exclude (TixModule pkg_slash_name _ _ _) =
      let modname = case break (== '/') pkg_slash_name of
                      (_, '/':name) -> name
                      (name, _)     -> name
      in  notElem modname (reportExcludes rpt)

-- | Read tix file from file path, return a 'Tix' data or throw
-- a 'TixNotFound' exception.
readTixFile :: Report -> FilePath -> IO Tix
readTixFile rpt path =
  do mb_tix <- readTix path
     case mb_tix of
       Nothing  -> throwIO (TixNotFound path)
       Just tix -> say rpt ("Found tix file: " ++ path) >> return tix

-- | Search mix file under given directories, return a 'Mix' data or
-- throw a 'MixNotFound' exception.
readMixFile :: [FilePath] -> TixModule -> IO Mix
readMixFile dirs tm@(TixModule name _h _c _i) =
  handle handler (readMix dirs (Right tm))
  where
    handler :: ErrorCall -> IO a
    handler _ = throwIO (MixNotFound name dirs')
    dirs' = map (</> (name <.> "mix")) dirs

-- | Ensure the given source file exist, return the ensured 'FilePath'
-- or throw a 'SrcNotFound' exception.
ensureSrcPath :: Report -> FilePath -> IO FilePath
ensureSrcPath rpt path = go [] (reportSrcDirs rpt)
  where
    go acc [] = throwIO (SrcNotFound path acc)
    go acc (dir:dirs) =
      do let path' = dir </> path
         exist <- doesFileExist path'
         if exist
            then do say rpt ("Found source: " ++ path')
                    return path'
            else go (path':acc) dirs

-- | Print given message to 'stderr' when the verbose flag is 'True'.
say :: Report -> String -> IO ()
say rpt msg = when (reportVerbose rpt) (hPutStrLn stderr msg)

-- | Internal type synonym to represent code line hit.
type Tick = Int

-- | Internal type used for accumulating mix entries.
data Info =
  Info {-# UNPACK #-} !Int -- ^ Index count
       {-# UNPACK #-} !Int -- ^ Min line number
       {-# UNPACK #-} !Int -- ^ Max line number
       ![(HpcPos, Tick)] -- ^ Pair of position and hit

-- | Make line hits from intermediate info.
makeLineHits :: Int -> Int -> [(HpcPos, Tick)] -> LineHits
makeLineHits min_line max_line hits = ticksToHits (runSTUArray work)
  where
    work =
      do arr <- newArray (min_line, max_line) ignored
         mapM_ (updateHit arr) hits
         return arr
    updateHit arr (pos, hit) =
      let (ls, _, _, _) = fromHpcPos pos
      in  updateOne arr hit ls
    updateOne :: STUArray s Int Int -> Tick -> Int -> ST s ()
    updateOne arr hit i =
      do prev <- readArray arr i
         writeArray arr i (mergeEntry prev hit)
    mergeEntry prev hit
      | isIgnored prev              = hit
      | isMissed prev, isMissed hit = missed
      | isFull prev, isFull hit     = full
      | otherwise                   = partial

-- | Convert array of ticks to list of hits.
ticksToHits :: UArray Int Tick -> LineHits
ticksToHits arr = foldr f [] (range (bounds arr))
  where
    f i acc =
      case arr ! i of
        tck | isIgnored tck -> acc
            | isMissed tck  -> (i, Missed) : acc
            | isFull tck    -> (i, Full) : acc
            | otherwise     -> (i, Partial) : acc

ignored, missed, partial, full :: Tick
ignored = -1
missed = 0
partial = 1
full = 2

isIgnored :: Int -> Bool
isIgnored = (== ignored)

isMissed :: Int -> Bool
isMissed = (== missed)

isFull :: Int -> Bool
isFull = (== full)

notTicked, tickedOnlyTrue, tickedOnlyFalse, ticked :: Tick
notTicked = missed
tickedOnlyTrue = partial
tickedOnlyFalse = partial
ticked = full

-- See also: "utils/hpc/HpcMarkup.hs" in "ghc" git repository.
makeInfo :: TixModule -> [MixEntry] -> Info
makeInfo tm = foldl' f z
  where
    z = Info 0 maxBound 0 []
    f (Info i min_line max_line acc) (pos, boxLabel) =
      let binBox = case (isTicked i, isTicked (i+1)) of
                     (False, False) -> acc
                     (True,  False) -> (pos, tickedOnlyTrue) : acc
                     (False, True)  -> (pos, tickedOnlyFalse) : acc
                     (True, True)   -> acc
          tickBox = if isTicked i
                       then (pos, ticked) : acc
                       else (pos, notTicked) : acc
          acc' = case boxLabel of
                   ExpBox {}      -> tickBox
                   TopLevelBox {} -> tickBox
                   LocalBox {}    -> tickBox
                   BinBox _ True  -> binBox
                   _              -> acc
          (ls, _, le, _) = fromHpcPos pos
      in Info (i+1) (min ls min_line) (max le max_line) acc'

    -- Hope that mix file does not contain out of bound index.
    isTicked n = unsafeAt arr_tix n /= 0

    arr_tix :: UArray Int Int
    arr_tix = listArray (0, size - 1) (map fromIntegral tixs)
    TixModule _name _hash size tixs = tm
