{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:     Trace.Hpc.Codecov.Report
-- Copyright:  (c) 2020 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Generate codecov.io report data
--

module Trace.Hpc.Codecov.Report (genReport) where

-- base
import Control.Exception         (bracket)
import Control.Monad.ST          (ST)
import Data.List                 (foldl', intersperse)
import System.Exit               (exitFailure)
import System.IO                 (IOMode (..), hClose, openFile, stdout)

-- array
import Data.Array.IArray         (assocs, listArray, (!))
import Data.Array.MArray         (newArray, readArray, writeArray)
import Data.Array.ST             (STUArray, runSTUArray)
import Data.Array.Unboxed        (UArray)

-- bytestring
import Data.ByteString.Builder   (Builder, char7, hPutBuilder, intDec,
                                  string7, stringUtf8)

-- directory
import System.Directory          (doesFileExist)

-- filepath
import System.FilePath           ((</>))

-- hpc
import Trace.Hpc.Mix             (BoxLabel (..), Mix (..), MixEntry,
                                  readMix)
import Trace.Hpc.Tix             (Tix (..), TixModule (..), readTix)
import Trace.Hpc.Util            (HpcPos, fromHpcPos)

-- Internal
import Trace.Hpc.Codecov.Options


-- ------------------------------------------------------------------------
--
-- Report generator function
--
-- ------------------------------------------------------------------------

-- | Generate report data from options.
genReport :: Options -> IO ()
genReport opts@Options{..} =
  do mb_tixs <- mapM readTixWithPath optTixs
     coverages <- fmap concat (mapM (tixToCoverage opts) mb_tixs)
     emitCoverageJSON opts coverages

-- | Read tix file from file path, return a pair of path and the read
-- tix data.
readTixWithPath :: FilePath -> IO (FilePath, Maybe Tix)
readTixWithPath path = readTix path >>= \mbt -> return (path, mbt)

-- | Emit simple coverage JSON data.
emitCoverageJSON :: Options -> [CoverageEntry] -> IO ()
emitCoverageJSON opts entries = bracket acquire cleanup work
  where
    (acquire, cleanup) =
      case optOutFile opts of
        Just path -> (openFile path WriteMode, hClose)
        Nothing   -> (return stdout, const (return ()))
    work hdl = hPutBuilder hdl (buildJSON entries)

-- | Build JSON report from coverage entries.
buildJSON :: [CoverageEntry] -> Builder
buildJSON entries = contents
  where
    contents =
      braced (key (string7 "coverage") <>
              braced (listify (map report entries))) <>
      char7 '\n'
    report ce =
      key (stringUtf8 (ce_path ce)) <>
      braced (listify (map hit (ce_hits ce)))
    key x = dquote x <> char7 ':'
    dquote x = char7 '"' <> x <> char7 '"'
    braced x = char7 '{' <> x <> char7 '}'
    listify xs = mconcat (intersperse comma xs)
    hit (n, tag) =
      case tag of
        0 -> k <> char7 '0'
        1 -> k <> dquote (char7 '1' <> char7 '/' <> char7 '2')
        2 -> k <> char7 '1'
        _ -> mempty
      where
        k = key (intDec n)
    comma = char7 ','

tixToCoverage :: Options -> (FilePath, Maybe Tix) -> IO [CoverageEntry]
tixToCoverage opts (path, mb_tix) =
  case mb_tix of
    Nothing        -> error ("Tix file \"" ++ path ++ "\" not found")
    Just (Tix tms) -> mapM (tixModuleToCoverage opts)
                           (excludeModules opts tms)

excludeModules :: Options -> [TixModule] -> [TixModule]
excludeModules opts tms = filter exclude tms
  where
    exclude (TixModule pkg_slash_name _ _ _) =
      let modname = case break (== '/') pkg_slash_name of
                      (_, '/':name) -> name
                      (name, _)     -> name
      in  notElem modname (optExcludes opts)

tixModuleToCoverage :: Options -> TixModule -> IO CoverageEntry
tixModuleToCoverage opts tm@(TixModule _name _hash _count _ixs) =
  do Mix path _ _ _ entries <- readMix (optMixDirs opts) (Right tm)
     let Info _ min_line max_line hits = makeInfo tm entries
         lineHits = makeLineHits min_line max_line hits
     path' <- ensureSrcPath opts path
     return (CoverageEntry { ce_path = path'
                           , ce_hits = lineHits })

ensureSrcPath :: Options -> FilePath -> IO FilePath
ensureSrcPath opts path = go [] (optSrcDirs opts)
  where
    go acc [] =
      do putStrLn ("Error: cannot find \"" ++ path ++ "\"")
         putStrLn "Searched:"
         mapM_ (putStrLn . ("  - " ++)) acc
         exitFailure
    go acc (dir:dirs) =
      do let path' = dir </> path
         exist <- doesFileExist path'
         if exist
            then return path'
            else go (path':acc) dirs


-- ------------------------------------------------------------------------
--
-- Internal works for line hits
--
-- ------------------------------------------------------------------------

-- | Entry of single file in coverage report.
data CoverageEntry =
  CoverageEntry { ce_path :: FilePath
                , ce_hits :: LineHits
                } deriving (Eq, Show)

-- | Pair of line number and hit tag.
type LineHits = [(Int, Int)]

-- | Data type to represent code coverage line hit.
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
      do arr <- newArray (min_line, max_line) (-1)
         mapM_ (updateHit arr) hits
         return arr
    updateHit arr (pos, hit) =
      let (ls, _, _, _) = fromHpcPos pos
      in  updateOne arr hit ls
    updateOne :: STUArray s Int Int -> Tick -> Int -> ST s ()
    updateOne arr hit i =
      do current <- readArray arr i
         writeArray arr i (mergeEntry current hit)
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
    isTicked n = (arr_tix ! n) /= 0
    arr_tix :: UArray Int Int
    arr_tix = listArray (0, size - 1) (map fromIntegral tixs)
    TixModule _name _hash size tixs = tm
