{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module:     Trace.Hpc.Codecov.Report.Emit
-- Copyright:  (c) 2023 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Emit 'CoverageEntry' to other report format.

module Trace.Hpc.Codecov.Report.Emit
  ( buildCodecov
  , buildLcov
  , buildCobertura
  ) where

-- base
import           Data.Char                         (isUpper)
import           Data.List                         (intercalate,
                                                    intersperse)
import           System.IO.Unsafe                  (unsafePerformIO)

#if !MIN_VERSION_base(4,20,0)
import           Data.List                         (foldl')
#endif

#if !MIN_VERSION_bytestring(0,11,0)
import           Text.Printf                       (printf)
#endif

-- bytestring
import           Data.ByteString.Builder           (Builder, char7, intDec,
                                                    string7, stringUtf8)
#if MIN_VERSION_bytestring(0,11,0)
import           Data.ByteString.Builder.RealFloat (formatDouble,
                                                    standardDefaultPrecision)
#endif

-- containers
import qualified Data.IntMap                       as IntMap
import qualified Data.Map                          as Map

-- filepath
import           System.FilePath                   (dropExtension,
                                                    splitDirectories,
                                                    takeDirectory)

-- time
import           Data.Time.Clock.POSIX             (getPOSIXTime)

-- Internal
import           Trace.Hpc.Codecov.Report.Entry


-- ------------------------------------------------------------------------
-- Codecov
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


-- ------------------------------------------------------------------------
-- Lcov
-- ------------------------------------------------------------------------

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


-- ------------------------------------------------------------------------
-- Cobertura
-- ------------------------------------------------------------------------

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
-- Auxiliary
-- ------------------------------------------------------------------------

dquote :: Builder -> Builder
dquote x = char7 '"' <> x <> char7 '"'
{-# INLINABLE dquote #-}

comma :: Builder
comma = char7 ','
{-# INLINABLE comma #-}

formatStandardDouble :: Double -> Builder
#if MIN_VERSION_bytestring(0,11,0)
formatStandardDouble = formatDouble standardDefaultPrecision
#else
formatStandardDouble = string7 . printf "%f"
#endif
{-# INLINABLE formatStandardDouble #-}
