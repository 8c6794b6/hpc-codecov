{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Exception                 (ErrorCall, handle,
                                                    throw, throwIO)
import           Control.Monad                     (mplus, when)
import           Control.Monad.ST                  (ST)
import           Data.Char                         (isUpper)
import           Data.Function                     (on)
import           Data.List                         (foldl', intercalate,
                                                    intersperse)
import           System.IO                         (IOMode (..), hPutStrLn,
                                                    stderr, stdout,
                                                    withFile)
import           System.IO.Unsafe                  (unsafePerformIO)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                       ((<>))
#endif

-- array
import           Data.Array.Base                   (unsafeAt)
import           Data.Array.IArray                 (assocs, listArray)
import           Data.Array.MArray                 (newArray, readArray,
                                                    writeArray)
import           Data.Array.ST                     (STArray, runSTArray)
import           Data.Array.Unboxed                (UArray)

-- bytestring
import           Data.ByteString.Builder           (Builder, char7,
                                                    hPutBuilder, intDec,
                                                    string7, stringUtf8)
import           Data.ByteString.Builder.RealFloat (formatDouble,
                                                    standardDefaultPrecision)

-- containers
import qualified Data.IntMap                       as IntMap
import qualified Data.Map                          as Map

-- directory
import           System.Directory                  (doesFileExist)

-- filepath
import           System.FilePath                   (dropExtension,
                                                    splitDirectories,
                                                    takeDirectory, (<.>),
                                                    (</>))

-- hpc
import           Trace.Hpc.Mix                     (BoxLabel (..),
                                                    Mix (..), MixEntry)
import           Trace.Hpc.Tix                     (Tix (..),
                                                    TixModule (..))
import           Trace.Hpc.Util                    (fromHpcPos)

-- time
import           Data.Time.Clock.POSIX             (getPOSIXTime)

-- Internal
import           Trace.Hpc.Codecov.Exception
import           Trace.Hpc.Codecov.Parser


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
             , reportOutFile = extend mplus reportOutFile
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

-- | Generate report data from options.
genReport :: Report -> IO ()
genReport rpt = do
  entries <- genCoverageEntries rpt
  let mb_out = reportOutFile rpt
      oname = maybe "stdout" show mb_out
  say rpt ("Writing report to " ++ oname)
  emitCoverage (reportFormat rpt) mb_out entries
  say rpt "Done"

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


-- ------------------------------------------------------------------------
--
-- CoverageEntry to report
--
-- ------------------------------------------------------------------------

-- | Build simple Codecov JSON report from coverage entries.
buildCodecov :: [CoverageEntry] -> Builder
buildCodecov entries = contents
  where
    contents =
      braced (key (string7 "coverage") <>
              braced (listify (map report entries))) <>
      char7 '\n'
    report ce =
      key (stringUtf8 (ce_filename ce)) <>
      braced (listify (map hit (ce_hits ce)))
    key x = dquote x <> char7 ':'
    braced x = char7 '{' <> x <> char7 '}'
    listify xs = mconcat (intersperse comma xs)
    hit (n, tag) =
      case tag of
        Missed     -> k <> char7 '0'
        Partial {} -> k <> dquote (string7 "1/2")
        Full i     -> k <> intDec i
      where
        k = key (intDec n)

-- | Build simple lcov tracefile from coverage entries.
buildLcov :: [CoverageEntry] -> Builder
buildLcov = mconcat . map buildLcovEntry

buildLcovEntry :: CoverageEntry -> Builder
buildLcovEntry e =
  string7 "TN:" <> nl <>
  string7 "SF:" <> stringUtf8 (ce_filename e) <> nl <>
  fns_and_nl <>
  string7 "FNF:" <> intDec fnf <> nl <>
  string7 "FNH:" <> intDec fnh <> nl <>
  brdas_and_nl <>
  string7 "BRF:" <> intDec brf <> nl <>
  string7 "BRH:" <> intDec brh <> nl <>
  das_and_nl <>
  string7 "LF:" <> intDec lf <> nl <>
  string7 "LH:" <> intDec lh <> nl <>
  string7 "end_of_record" <> nl
  where
    fold_hits f xs =
      let (as, bs, nentry, nhit) = foldr f ([],[],0,0) xs
          res = as <> bs
          res_and_nl | null res = mempty
                     | otherwise = mconcat (intersperse nl res) <> nl
      in  (res_and_nl, nentry, nhit)

    (fns_and_nl, fnf, fnh) = fold_hits ffn (ce_fns e)
    ffn (sl, el, n, name) (fn_acc, fnda_acc, num_fns, num_hit_fns) =
      ( string7 "FN:" <> intDec sl <> comma <> intDec el <>
        comma <> name' : fn_acc
      , string7 "FNDA:" <> intDec n <> comma <> name' : fnda_acc
      , num_fns + 1
      , if n == 0 then num_hit_fns else num_hit_fns + 1 )
      where
        name' = stringUtf8 name

    (brdas_and_nl, brf, brh) = fold_hits fbr (ce_branches e)
    fbr (sl, blk, bool, n) (_, br, num_brs, num_hit_brs) =
      ( []
      , string7 "BRDA:" <> intDec sl <> comma <>
        intDec blk <> comma <>
        char7 (if bool then '0' else '1') <> comma <>
        intDec n : br
      , num_brs + 1
      , if n == 0 then num_hit_brs else num_hit_brs + 1 )

    (das_and_nl, lf, lh) = fold_hits fda (ce_hits e)
    fda (n, hit) (_, da, num_lines, num_hits) =
      case hit of
        Missed    -> ([], da0 n:da,   num_lines + 1, num_hits)
        Partial i -> ([], dai n i:da, num_lines + 1, num_hits + 1)
        Full i    -> ([], dai n i:da, num_lines + 1, num_hits + 1)
    da0 n = string7 "DA:" <> intDec n <> comma <> char7 '0'
    dai n i = string7 "DA:" <> intDec n <> comma <> intDec i

    nl = char7 '\n'

dquote :: Builder -> Builder
dquote x = char7 '"' <> x <> char7 '"'
{-# INLINABLE dquote #-}

comma :: Builder
comma = char7 ','
{-# INLINABLE comma #-}

formatStandardDouble :: Double -> Builder
formatStandardDouble = formatDouble standardDefaultPrecision
{-# INLINABLE formatStandardDouble #-}

-- Types and typeclass for Cobertura

class HasRate c e where
  numValid :: c e -> Int
  numCovered :: c e -> Int

newtype Lines a = Lines {unLines :: a}

newtype Branches a = Branches {unBranches :: a}

buildRateOf :: HasRate c e => c e -> Builder
buildRateOf x = buildRate (numCovered x) (numValid x)
{-# INLINABLE buildRateOf #-}

buildRate :: Int -> Int -> Builder
buildRate n d =
  if d == 0 then
    string7 "0.0"
  else
    formatStandardDouble (fromIntegral n / fromIntegral d)
{-# INLINABLE buildRate #-}

data Acc = Acc
  { acc_valid_lines      :: !Int
  , acc_covered_lines    :: !Int
  , acc_valid_branches   :: !Int
  , acc_covered_branches :: !Int
  }

accLinesAndBranches :: (HasRate Lines e, HasRate Branches e) => [e] -> Acc
accLinesAndBranches = foldl' f (Acc 0 0 0 0)
  where
    f acc e = acc
      { acc_valid_lines = acc_valid_lines acc + numValid (Lines e)
      , acc_covered_lines = acc_covered_lines acc + numCovered (Lines e)
      , acc_valid_branches = acc_valid_branches acc + numValid (Branches e)
      , acc_covered_branches = acc_covered_branches acc + numCovered (Branches e)
      }

data CoberturaPackage = CoberturaPackage
  { cp_name           :: String
  , cp_lines_valid    :: !Int
  , cp_lines_covered  :: !Int
  , cp_branch_valid   :: !Int
  , cp_branch_covered :: !Int
  , cp_classes        :: [CoberturaClass]
  }

instance HasRate Lines CoberturaPackage where
  numValid = cp_lines_valid . unLines
  {-# INLINE numValid #-}
  numCovered = cp_lines_covered . unLines
  {-# INLINE numCovered #-}

instance HasRate Branches CoberturaPackage where
  numValid = cp_branch_valid . unBranches
  {-# INLINE numValid #-}
  numCovered = cp_branch_covered . unBranches
  {-# INLINE numCovered #-}

data CoberturaClass = CoberturaClass
  { cc_filename       :: String
  , cc_lines_valid    :: !Int
  , cc_lines_covered  :: !Int
  , cc_branch_valid   :: !Int
  , cc_branch_covered :: !Int
  , cc_methods        :: [CoberturaMethod]
  , cc_lines          :: [CoberturaLine]
  }

instance HasRate Lines CoberturaClass where
  numValid = cc_lines_valid . unLines
  {-# INLINE numValid #-}
  numCovered = cc_lines_covered . unLines
  {-# INLINE numCovered #-}

instance HasRate Branches CoberturaClass where
  numValid = cc_branch_valid . unBranches
  {-# INLINE numValid #-}
  numCovered = cc_branch_covered . unBranches
  {-# INLINE numCovered #-}

data CoberturaMethod = CoberturaMethod
  { cm_line_num :: !Int
  , cm_name     :: String
  }

data CoberturaLine = CoberturaLine
  { cl_line_num    :: !Int
  , cl_num_hits    :: !Int
  , cl_branch_hits :: Map.Map (Int, Bool) Int
  }

-- | Build simple Cobertura XML report from coverage entries.
buildCobertura :: [CoverageEntry] -> Builder
buildCobertura es =
  string7 "<?xml version=\"1.0\" ?>" <>
  string7 "<!DOCTYPE coverage SYSTEM 'http://cobertura.sourceforge.nex/xml/coverage-0.4.dtd'>" <>
  xmlTagWith "coverage" coverage_attrs
  (xmlTag "sources" (xmlTag "source" (char7 '.')) <>
   xmlTag "packages" (mconcat (map buildCoberturaPackage pkgs))) <>
  char7 '\n'
  where
    coverage_attrs =
      [("branch-rate", buildRate bc bv)
      ,("branches-covered", intDec bc)
      ,("branches-valid", intDec bv)
      ,("complexity", char7 '0')
      ,("line-rate", buildRate lc lv)
      ,("lines-covered", intDec lc)
      ,("lines-valid", intDec lv)
      ,("timestamp", intDec $ fromInteger unsafeGetTimestamp)
      ,("version", string7 "2.0.3")]
    pkgs = toCoberturaPackages es
    Acc lv lc bv bc = accLinesAndBranches pkgs

-- XXX: Not sure whether this is the preferred timestamp format.
unsafeGetTimestamp :: Integer
unsafeGetTimestamp = round (unsafePerformIO getPOSIXTime)
{-# NOINLINE unsafeGetTimestamp #-}

buildCoberturaPackage :: CoberturaPackage -> Builder
buildCoberturaPackage cp =
  xmlTagWith "package" package_attrs
  (xmlTag "classes"
   (mconcat (map buildCoberturaClass (cp_classes cp))))
  where
    package_attrs =
      [("name", stringUtf8 $ cp_name cp)
      ,("line-rate", buildRateOf (Lines cp))
      ,("branch-rate", buildRateOf (Branches cp))
      ,("complexity", char7 '0')]

buildCoberturaClass :: CoberturaClass -> Builder
buildCoberturaClass cc =
  xmlTagWith "class" class_attrs
  (xmlTag "methods" (mconcat (map method (cc_methods cc))) <>
   xmlTag "lines" (mconcat (map line (cc_lines cc))))
  where
    class_attrs =
      [("branch-rate", buildRateOf (Branches cc))
      ,("complexity", char7 '0')
      ,("filename", stringUtf8 $ cc_filename cc)
      ,("line-rate", buildRateOf (Lines cc))
      ,("name", stringUtf8 $ toModuleName (cc_filename cc))]
    method cm =
      xmlTagWith "method" method_attrs
      (xmlTag "lines" (line_tag line_attrs))
      where
        method_attrs =
          [("name", stringUtf8 $ xmlEscape (cm_name cm))
          ,("signature", mempty)
          ,("line-rate", string7 "0.0")
          ,("branch-rate", string7 "0.0")]
        line_attrs =
          [("hits", char7 '0')
          ,("number", intDec (cm_line_num cm))
          ,("branch", string7 "false")]
    line cl = line_tag line_attrs
      where
        is_branch = not (null (cl_branch_hits cl))
        line_attrs =
          [("branch", string7 $ if is_branch then "true" else "false")
          ,("hits", intDec (cl_num_hits cl))
          ,("number", intDec (cl_line_num cl)) ] <>
          [("condition-coverage", buildConditionCoverage (cl_branch_hits cl))
          | is_branch ]
    line_tag attrs =
      string7 "<line" <> mconcat (map xmlAttr attrs) <> string7 "/>"

buildConditionCoverage :: Map.Map a Int -> Builder
buildConditionCoverage m = fmt
  where
    fmt = percentage <> char7 ' ' <> char7 '(' <> fraction <> char7 ')'
    percentage = intDec (round (100 * rate)) <> char7 '%'
    fraction = intDec covered <> char7 '/' <> intDec valid
    rate = fromIntegral covered / fromIntegral valid :: Double
    (covered, valid) = foldl' f (0, 0) m
    f (c, v) num_hits = (if 0 < num_hits then c + 1 else c, v + 1)

toCoberturaPackages :: [CoverageEntry] -> [CoberturaPackage]
toCoberturaPackages =
  Map.foldrWithKey' go [] . Map.fromListWith (<>) . map pair_with_pkgname
  where
    pair_with_pkgname ce =
      (toCoberturaPackageName (ce_filename ce), [toCoberturaClass ce])
    go pkg_name ces acc =
      let Acc lv lc bv bc = accLinesAndBranches ces
      in  CoberturaPackage { cp_name = pkg_name
                           , cp_lines_valid = lv
                           , cp_lines_covered = lc
                           , cp_branch_valid = bv
                           , cp_branch_covered = bc
                           , cp_classes = ces
                           } : acc

toCoberturaClass :: CoverageEntry -> CoberturaClass
toCoberturaClass ce = CoberturaClass
  { cc_filename = ce_filename ce
  , cc_lines_valid = lv
  , cc_lines_covered = lc
  , cc_branch_valid = bv
  , cc_branch_covered = bc
  , cc_methods = methods
  , cc_lines = IntMap.elems im1
  }
  where
    (lv, lc, im0) = foldl' acc_line (0, 0, mempty) (ce_hits ce)
    (bv, bc, im1) = foldl' acc_branch (0, 0, im0) (ce_branches ce)
    acc_line (lv0, lc0, im) (n, hit) =
      case hit of
        Missed    -> (lv0 + 1, lc0, IntMap.insert n (make_line n 0) im)
        Partial i -> (lv0 + 1, lc0 + 1, IntMap.insert n (make_line n i) im)
        Full i    -> (lv0 + 1, lc0 + 1, IntMap.insert n (make_line n i) im)
    make_line n i = CoberturaLine n i mempty
    make_branch n i bn bool = CoberturaLine n i (Map.singleton (bn,bool) i)
    merge_brs new_cl old_cl = CoberturaLine
      { cl_line_num = cl_line_num old_cl
      , cl_num_hits = max (cl_num_hits old_cl) (cl_num_hits new_cl)
      , cl_branch_hits =
        Map.unionWith (+) (cl_branch_hits old_cl) (cl_branch_hits new_cl)
      }
    acc_branch (bv0, bc0, im) (n, bn, bool, count) =
      let im' = IntMap.insertWith merge_brs n (make_branch n count bn bool) im
      in  (bv0 + 1, if count == 0 then bc0 else bc0 + 1, im')
    methods = map to_method (ce_fns ce)
    to_method (sl,_,_,name) = CoberturaMethod sl name

toCoberturaPackageName :: FilePath -> String
toCoberturaPackageName = intercalate "." . splitDirectories . takeDirectory

toModuleName :: FilePath -> String
toModuleName path = intercalate "." paths'
  where
    paths' = [p | p@(c:_) <- splitDirectories (dropExtension path), isUpper c ]

xmlTag :: String -> Builder -> Builder
xmlTag name = xmlTagWith name []

xmlTagWith :: String -> [(String, Builder)] -> Builder -> Builder
xmlTagWith name attrs body =
  char7 '<' <> string7 name <> attrs' <> char7 '>' <>
  body <>
  string7 "</" <> string7 name <> char7 '>'
  where
    attrs' =
      if null attrs then mempty else mconcat (map xmlAttr attrs)

xmlAttr :: (String, Builder) -> Builder
xmlAttr (name, val) = char7 ' ' <> string7 name <> char7 '=' <> dquote val

xmlEscape :: String -> String
xmlEscape = go
  where
    go [] = []
    go (c:rest) = case c of
      '>' -> "&gt;" <> go rest
      '<' -> "&lt;" <> go rest
      '"' -> "&quot;" <> go rest
      '&' -> "&amp;" <> go rest
      _   -> c:go rest


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
say rpt msg = when (reportVerbose rpt) (hPutStrLn stderr msg)

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
