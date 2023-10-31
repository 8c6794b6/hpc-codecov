{-# LANGUAGE CPP #-}
-- |
-- Module:     Trace.Hpc.Codecov.Report.Entry
-- Copyright:  (c) 2023 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Codes for converting tix and mix files to 'CoverageEntry'.

module Trace.Hpc.Codecov.Report.Entry
  ( Report(..)
  , CoverageEntry(..)
  , Format(..)
  , LineHits
  , Hit(..)
  , FunctionHits
  , BranchHits
  , tixToCoverage
  , readTixFile
  ) where

-- base
import           Control.Applicative         ((<|>))
import           Control.Exception           (ErrorCall, handle, throw,
                                              throwIO)
import           Control.Monad               (when)
import           Control.Monad.ST            (ST)
import           Data.Function               (on)
import           Data.List                   (foldl', intercalate)
import           System.IO                   (hPutStrLn, stderr)

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                 ((<>))
#endif

-- array
import           Data.Array.Base             (unsafeAt)
import           Data.Array.IArray           (assocs, listArray)
import           Data.Array.MArray           (newArray, readArray,
                                              writeArray)
import           Data.Array.ST               (STArray, runSTArray)
import           Data.Array.Unboxed          (UArray)

-- containers
import qualified Data.IntMap                 as IntMap

-- directory
import           System.Directory            (doesFileExist)

-- filepath
import           System.FilePath             ((<.>), (</>))

-- hpc
import           Trace.Hpc.Mix               (BoxLabel (..), Mix (..),
                                              MixEntry)
import           Trace.Hpc.Tix               (Tix (..), TixModule (..))
import           Trace.Hpc.Util              (fromHpcPos)


-- Internal
import           Trace.Hpc.Codecov.Exception
import           Trace.Hpc.Codecov.Parser


-- ------------------------------------------------------------------------
--
-- Types
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
   -- ^ Output file to write report data, if given.
 , reportVerbose  :: Bool
   -- ^ Flag for showing verbose message during report generation.
 , reportFormat   :: Format
   -- ^ Format of the report output.
   --
   -- @since 0.4.0.0
 } deriving (Eq, Show)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Report where
  (<>) = mappendReport
#endif

instance Monoid Report where
  mempty = emptyReport
#if !MIN_VERSION_base(4,16,0)
  mappend = mappendReport
#endif

emptyReport :: Report
emptyReport = Report
  { reportTix = throw NoTarget
  , reportMixDirs = []
  , reportSrcDirs = []
  , reportExcludes = []
  , reportOutFile = Nothing
  , reportVerbose = False
  , reportFormat = Codecov
  }

mappendReport :: Report -> Report -> Report
mappendReport r1 r2 =
  let extend f g = (f `on` g) r1 r2
  in  Report { reportTix = reportTix r2
             , reportMixDirs = extend (<>) reportMixDirs
             , reportSrcDirs = extend (<>) reportSrcDirs
             , reportExcludes = extend (<>) reportExcludes
             , reportOutFile = extend (<|>) reportOutFile
             , reportVerbose = extend (||) reportVerbose
             , reportFormat = reportFormat r2
             }

-- | Single file entry in coverage report.
--
data CoverageEntry =
  CoverageEntry { ce_filename :: FilePath -- ^ Source code file name.
                , ce_hits     :: LineHits -- ^ Line hits of the file.
                , ce_fns      :: FunctionHits
                  -- ^ Function hits of the file.
                  --
                  -- @since 0.4.0.0
                , ce_branches :: BranchHits
                  -- ^ Branch hits of the file.
                  --
                  -- @since 0.4.0.0
                } deriving (Eq, Show)

-- | Pair of line number and hit tag.
type LineHits = [(Int, Hit)]

-- | Data type to represent coverage of source code line.
--
-- The 'Int' value in 'Partial' and 'Full' are the hit count.
data Hit
  = Missed  -- ^ The line is not covered at all.
  | Partial Int -- ^ The line is partially covered.
  | Full Int   -- ^ The line is fully covered.
  deriving (Eq, Show)

-- | Type synonym for tracking function enter count. Elements are
-- start line number, end line number, execution count, and function
-- name.
--
-- @since 0.4.0.0
type FunctionHits = [(Int, Int, Int, String)]

-- | Type synonym for tracking branch information. Elements are start
-- line number, branch block number, 'Bool' for the taken branch, and
-- execution count.
--
-- @since 0.4.0.0
type BranchHits = [(Int, Int, Bool, Int)]

-- | Data type for generated report format.
data Format
  = Codecov
  -- ^ Custom Codecov JSON format. See the
  -- <https://docs.codecov.io/docs/codecov-custom-coverage-format Codecov documentation>
  -- for detail.
  --
  -- @since 0.1.0.0
  | Lcov
  -- ^ LCOV tracefile format. See the
  -- <https://ltp.sourceforge.net/coverage/lcov/geninfo.1.php geninfo manpage>
  -- for detail.
  --
  -- @since 0.4.0.0
  | Cobertura
  -- ^ Cobertura XML file format. See the
  -- <https://cobertura.github.io/cobertura/ Cobertura> website for detail.
  --
  -- @since 0.5.0.0
  deriving (Eq, Show)


-- ------------------------------------------------------------------------
--
-- Tix to CoverageEntry
--
-- ------------------------------------------------------------------------

tixToCoverage :: Report -> Tix -> IO [CoverageEntry]
tixToCoverage rpt (Tix tms) =
  mapM (tixModuleToCoverage rpt) (excludeModules rpt tms)

tixModuleToCoverage :: Report -> TixModule -> IO CoverageEntry
tixModuleToCoverage rpt tm@(TixModule name _hash count ixs) = do
  say rpt ("Searching mix:  " ++ name)
  Mix path _ _ _ entries <- readMixFile (reportMixDirs rpt) tm
  say rpt ("Found mix:      " ++ path)
  let Info _ min_line max_line hits fns pre_brs = makeInfo count ixs entries
      lineHits = makeLineHits min_line max_line hits
  path' <- ensureSrcPath rpt path
  return (CoverageEntry { ce_filename = path'
                        , ce_hits = lineHits
                        , ce_fns = fns
                        , ce_branches = reBranch pre_brs })

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
readTixFile rpt path = do
  mb_tix <- {-# SCC "readTixFile.readTix'" #-} readTix' path
  case mb_tix of
    Nothing  -> throwIO (TixNotFound path)
    Just tix -> say rpt ("Found tix file: " ++ path) >> return tix

-- | Search mix file under given directories, return a 'Mix' data or
-- throw a 'MixNotFound' exception.
readMixFile :: [FilePath] -> TixModule -> IO Mix
readMixFile dirs tm@(TixModule name _h _c _i) = handle handler go
  where
    handler :: ErrorCall -> IO a
    handler _ = throwIO (MixNotFound name dirs')
    dirs' = map (</> (name <.> "mix")) dirs
    go = {-# SCC "readMixFile.readMix'" #-} readMix' dirs (Right tm)

-- | Ensure the given source file exist, return the ensured 'FilePath'
-- or throw a 'SrcNotFound' exception.
ensureSrcPath :: Report -> FilePath -> IO FilePath
ensureSrcPath rpt path = go [] (reportSrcDirs rpt)
  where
    go acc [] = throwIO (SrcNotFound path acc)
    go acc (dir:dirs) = do
      let path' = dir </> path
      exist <- doesFileExist path'
      if exist
        then say rpt ("Found source:   " ++ path') >> return path'
        else go (path':acc) dirs

-- | Arrange branch hit information.
--
-- LCOV tracefile seems like want to have a true branch before the
-- corresponding false branch, so arranging the order.
--
-- Also assigning sequential block numbers to the branch entries
-- starting with identical line number.
reBranch :: PreBranchHits -> BranchHits
reBranch = go mempty
  where
    go im0 ((lf,brf,nf) : (lt,brt,nt) : rest) =
      let (mb_i, im1) = IntMap.insertLookupWithKey f lf 0 im0
          f _key _new old = old + 1 :: Int
          i = maybe 0 succ mb_i
      in  (lt,i,brt,nt) : (lf,i,brf,nf) : go im1 rest
    go _ _ = []

-- | Print given message to 'stderr' when the verbose flag is 'True'.
say :: Report -> String -> IO ()
say rpt = when (reportVerbose rpt) . hPutStrLn stderr

-- | Internal type synonym to represent code line hit. Using 'Int' so
-- that unboxed arrays can use in its elements.
type Tick = Int

-- | Internal type synonym to represent line hit count.
type Count = Int

-- | Like 'BranchHits', but without branch block number.
type PreBranchHits = [(Int, Bool, Count)]

-- | Internal type used for accumulating mix entries.
data Info =
  Info {-# UNPACK #-} !Int -- ^ Index count
       {-# UNPACK #-} !Int -- ^ Min line number
       {-# UNPACK #-} !Int -- ^ Max line number
       [(Int, Tick, Count)] -- ^ Start line number, tick, and count.
       FunctionHits -- ^ For tracking function.
       PreBranchHits -- ^ For tracking branch.

-- | Make line hits from intermediate info.
makeLineHits :: Int -> Int -> [(Int, Tick, Count)] -> LineHits
makeLineHits min_line max_line hits = ticksToHits (assocs merged)
  where
    merged = runSTArray $ do
      arr <- newArray (min_line, max_line) (ignored, 0)
      mapM_ (updateOne arr) hits
      return arr

    updateOne :: STArray s Int (Tick, Count) -> (Int, Tick, Count) -> ST s ()
    updateOne arr (i, hit, count) = do
      (old_hit, old_count) <- readArray arr i
      writeArray arr i (mergeEntry old_hit hit, max old_count count)

    mergeEntry prev curr
      | isMissed prev, isFull curr = partial
      | isFull prev, isMissed curr = partial
      | isPartial prev = prev
      | otherwise = curr

-- | Convert array of ticks to list of hits.
ticksToHits :: [(Int, (Tick, Count))] -> LineHits
ticksToHits = foldr f []
  where
    f (i,(tck,n)) acc
      | isIgnored tck = acc
      | isMissed tck  = (i, Missed) : acc
      | isFull tck    = (i, Full n) : acc
      | otherwise     = (i, Partial n) : acc

ignored, missed, partial, full :: Tick
ignored = -1
missed = 0
partial = 1
full = 2

isIgnored :: Tick -> Bool
isIgnored = (== ignored)

isMissed :: Tick -> Bool
isMissed = (== missed)

isPartial :: Tick -> Bool
isPartial = (== partial)

isFull :: Tick -> Bool
isFull = (== full)

notTicked, ticked :: Tick
notTicked = missed
ticked = full

-- See also: "utils/hpc/HpcMarkup.hs" in "ghc" git repository.
makeInfo :: Int -> [Integer] -> [MixEntry] -> Info
makeInfo size tixs = foldl' f z
  where
    z = Info 0 maxBound 0 [] [] []
    f (Info i0 min_line max_line txs fns brs) (pos, boxLabel) =
      let binBox =
            case (isTicked i0, isTicked i1) of
              (False, False) -> txs
              (True,  False) -> (sl, partial, numTicked i0) : txs
              (False, True)  -> (sl, partial, numTicked i1) : txs
              (True, True)   -> txs
          tickBox =
            let t | isTicked i0 = ticked
                  | otherwise = notTicked
            in  (sl, t, numTicked i0) : txs
          tlBox ns = (sl, el, numTicked i0, intercalate "." ns) : fns
          br bool = (sl, bool, numTicked i0)
          (txs', fns', brs') =
            case boxLabel of
              ExpBox {}      -> (tickBox, fns, brs)
              TopLevelBox ns -> (tickBox, tlBox ns, brs)
              LocalBox {}    -> (tickBox, fns, brs)
              BinBox _ True  -> (binBox, fns, br True : brs)
              BinBox _ False -> (txs, fns, br False : brs)
          (sl, _, el, _) = fromHpcPos pos
          i1 = i0 + 1
      in Info i1 (min sl min_line) (max el max_line) txs' fns' brs'

    -- Hope that the mix file does not contain out of bound index.
    numTicked = unsafeAt arr_tix
    isTicked n = numTicked n /= 0

    arr_tix :: UArray Int Tick
    arr_tix = listArray (0, size - 1) (map fromIntegral tixs)
