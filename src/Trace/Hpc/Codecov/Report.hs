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
    CoverageEntry(..)
  , LineHits
  , HitTag

    -- * Functions
  , genReport
  , genCoverageEntries
  , emitCoverageJSON
  ) where

-- base
import Control.Exception         (ErrorCall, handle, throwIO)
import Control.Monad             (when)
import Control.Monad.ST          (ST)
import Data.List                 (foldl', intersperse)
import System.IO                 (IOMode (..), hPutStrLn, stderr, stdout,
                                  withFile)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid               ((<>))
#endif

-- array
import Data.Array.Base           (unsafeAt)
import Data.Array.IArray         (assocs, listArray)
import Data.Array.MArray         (newArray, readArray, writeArray)
import Data.Array.ST             (STUArray, runSTUArray)
import Data.Array.Unboxed        (UArray)

-- bytestring
import Data.ByteString.Builder   (Builder, char7, hPutBuilder, intDec,
                                  string7, stringUtf8)

-- directory
import System.Directory          (doesFileExist)

-- filepath
import System.FilePath           ((<.>), (</>))

-- hpc
import Trace.Hpc.Mix             (BoxLabel (..), Mix (..), MixEntry,
                                  readMix)
import Trace.Hpc.Tix             (Tix (..), TixModule (..), readTix)
import Trace.Hpc.Util            (HpcPos, fromHpcPos)

-- Internal
import Trace.Hpc.Codecov.Error
import Trace.Hpc.Codecov.Options


-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Single file entry in coverage report.
--
-- See the
-- <https://docs.codecov.io/reference#section-codecov-json-report-format Codecov API>
-- for detail.
data CoverageEntry =
  CoverageEntry { ce_filename :: FilePath -- ^ Source code file name.
                , ce_hits     :: LineHits -- ^ Line hits of the file.
                } deriving (Eq, Show)

-- | Pair of line number and hit tag.
--
type LineHits = [(Int, HitTag)]

-- | Hit tag:
--
-- * @-1@ for irrelevant line
-- * @0@ for missed line
-- * @1@ for partially covered line
-- * @2@ for fully covered line
--
type HitTag = Int

-- | Generate report data from options.
genReport :: Options -> IO ()
genReport opts =
  do entries <- genCoverageEntries opts
     let mb_out = optOutFile opts
         oname = maybe "stdout" show mb_out
     say opts ("Writing JSON report to " ++ oname)
     emitCoverageJSON mb_out entries
     say opts "Done"

-- | Generate test coverage entries.
genCoverageEntries :: Options -> IO [CoverageEntry]
genCoverageEntries opts =
  readTixFile opts (optTix opts) >>= tixToCoverage opts

-- | Emit simple coverage JSON data.
emitCoverageJSON ::
  Maybe FilePath -- ^ 'Just' output file name, or 'Nothing' for
                 -- 'stdout'.
  -> [CoverageEntry] -- ^ Coverage entries to write.
  -> IO ()
emitCoverageJSON mb_outfile entries = wrap emit
  where
    wrap = maybe ($ stdout) (flip withFile WriteMode) mb_outfile
    emit = flip hPutBuilder (buildJSON entries)


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
        0 -> k <> char7 '0'
        1 -> k <> dquote (char7 '1' <> char7 '/' <> char7 '2')
        2 -> k <> char7 '1'
        _ -> mempty
      where
        k = key (intDec n)

tixToCoverage :: Options -> Tix -> IO [CoverageEntry]
tixToCoverage opts (Tix tms) = mapM (tixModuleToCoverage opts)
                                    (excludeModules opts tms)

tixModuleToCoverage :: Options -> TixModule -> IO CoverageEntry
tixModuleToCoverage opts tm@(TixModule name _hash _count _ixs) =
  do say opts ("Search mix:   " ++ name)
     Mix path _ _ _ entries <- readMixFile (optMixDirs opts) tm
     say opts ("Found mix:    "++ path)
     let Info _ min_line max_line hits = makeInfo tm entries
         lineHits = makeLineHits min_line max_line hits
     path' <- ensureSrcPath opts path
     return (CoverageEntry { ce_filename = path'
                           , ce_hits = lineHits })


-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------

-- | Exclude modules specified in given 'Options'.
excludeModules :: Options -> [TixModule] -> [TixModule]
excludeModules opts tms = filter exclude tms
  where
    exclude (TixModule pkg_slash_name _ _ _) =
      let modname = case break (== '/') pkg_slash_name of
                      (_, '/':name) -> name
                      (name, _)     -> name
      in  notElem modname (optExcludes opts)

-- | Read tix file from file path, return a 'Tix' data or throw
-- a 'TixNotFound' exception.
readTixFile :: Options -> FilePath -> IO Tix
readTixFile opts path =
  do mb_tix <- readTix path
     case mb_tix of
       Nothing  -> throwIO (TixNotFound path)
       Just tix -> say opts ("Found tix file: " ++ path) >> return tix

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
ensureSrcPath :: Options -> FilePath -> IO FilePath
ensureSrcPath opts path = go [] (optSrcDirs opts)
  where
    go acc [] = throwIO (SrcNotFound path acc)
    go acc (dir:dirs) =
      do let path' = dir </> path
         exist <- doesFileExist path'
         if exist
            then do say opts ("Found source: " ++ path')
                    return path'
            else go (path':acc) dirs

-- | Print given message to 'stderr' when the verbose flag is 'True'.
say :: Options -> String -> IO ()
say opts msg = when (optVerbose opts) (hPutStrLn stderr msg)

-- | Internal data type to represent code coverage line hit.
data Tick
  = NotTicked
  | TickedOnlyTrue
  | TickedOnlyFalse
  | IsTicked
  deriving (Eq, Show)

-- | Internal type used for accumulating mix entries.
data Info =
  Info {-# UNPACK  #-} !Int -- ^ Index count
       {-# UNPACK  #-} !Int -- ^ Min line number
       {-# UNPACK  #-} !Int -- ^ Max line number
       ![(HpcPos, Tick)] -- ^ Pair of position and hit

-- | Make line hits from intermediate info.
makeLineHits :: Int -> Int -> [(HpcPos, Tick)] -> LineHits
makeLineHits min_line max_line hits =
  filter ((>= 0) . snd) $ assocs (runSTUArray work)
  where
    work :: ST s (STUArray s Int Int)
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
      | isIgnored prev               = hit'
      | isMissed prev, isMissed hit' = missed
      | isFull prev, isFull hit'     = full
      | otherwise                    = partial
      where
        hit' = fromHit hit

ignored, missed, partial, full :: Int
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

fromHit :: Tick -> Int
fromHit hit =
  case hit of
    NotTicked       -> missed
    TickedOnlyTrue  -> partial
    TickedOnlyFalse -> partial
    IsTicked        -> full

-- See also: "utils/hpc/HpcMarkup.hs" in "ghc" git repository.
makeInfo :: TixModule -> [MixEntry] -> Info
makeInfo tm = foldl' f z
  where
    z = Info 0 maxBound 0 []
    f (Info i min_line max_line acc) (pos, boxLabel) =
      let binBox = case (isTicked i, isTicked (i+1)) of
                     (False, False) -> acc
                     (True,  False) -> (pos, TickedOnlyTrue) : acc
                     (False, True)  -> (pos, TickedOnlyFalse) : acc
                     (True, True)   -> acc
          tickBox | isTicked i = (pos, IsTicked) : acc
                  | otherwise  = (pos, NotTicked) : acc
          acc' = case boxLabel of
                   ExpBox {}      -> tickBox
                   TopLevelBox {} -> tickBox
                   LocalBox {}    -> tickBox
                   BinBox _ True  -> binBox
                   _              -> acc
          (ls, _, le, _) = fromHpcPos pos
      in (Info (i+1) (min ls min_line) (max le max_line) acc')

    -- Hope that mix file does not contain out of bound index.
    isTicked n = (unsafeAt arr_tix n) /= 0

    arr_tix :: UArray Int Int
    arr_tix = listArray (0, size - 1) (map fromIntegral tixs)
    TixModule _name _hash size tixs = tm
