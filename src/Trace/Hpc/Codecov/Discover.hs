-- |
-- Module:     Trace.Hpc.Codecov.Discover
-- Copyright:  (c) 2021 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Walk through directories and find hpc data.

module Trace.Hpc.Codecov.Discover
  ( -- * Discover function and types
    discover
  , DiscoverArgs(..)
  , BuildTool(..)

    -- * Auxiliary
  , foldDir
  , defaultIgnored
  , foldDirWithIgnoring
  ) where

-- base
import Control.Exception           (throwIO)
import Control.Monad               (when)
import Data.Maybe                  (isNothing)
import System.IO                   (hPutStrLn, stderr)

-- directory
import System.Directory            (doesDirectoryExist, doesFileExist,
                                    listDirectory)

-- filepath
import System.FilePath             (splitFileName, takeExtension,
                                    takeFileName, (<.>), (</>))

-- Internal
import Trace.Hpc.Codecov.Exception
import Trace.Hpc.Codecov.Report


-- ------------------------------------------------------------------------
--
-- Types
--
-- ------------------------------------------------------------------------

-- | Data type to hold arguments of 'discover' function.
data DiscoverArgs = DiscoverArgs
  { da_tool      :: BuildTool
    -- ^ Tool used to build Haskell cabal package.
  , da_testsuite :: String
    -- ^ Test suite name to search @.tix@ file.
  , da_root      :: FilePath
    -- ^ The project root directory for the build tool.
  , da_builddir  :: Maybe String
    -- ^ Name of the temporary build directory made by the build tool.
  , da_skipdirs  :: [String]
    -- ^ Directories to skip while searching for scanning data.
  , da_verbose   :: Bool
    -- ^ Flag for shwoing verbose information.
  }

-- | Tool used for building Haskell package source codes.
data BuildTool
  = Cabal
  -- ^ For <https://www.haskell.org/cabal/index.html cabal-install>.
  | Stack
  -- ^ For <https://docs.haskellstack.org/en/stable/README/ stack>.
  deriving (Eq)

instance Show BuildTool where
  show tool = case tool of
    Cabal -> "cabal"
    Stack -> "stack"

-- | Walk thorugh directory and search for @.mix@ directories, Haskell
-- source code directories, and @.tix@ file.
discover :: DiscoverArgs -> IO Report
discover da = do
  let build_dir = case da_builddir da of
        Nothing  -> defaultBuildDirName (da_tool da)
        Just dir -> dir
      list_msg = concatMap (\p -> "    - " ++ p ++ "\n")
      skipped_dirs_msg =
        if null (da_skipdirs da)
           then "No directory specified to skip during discover"
           else "Skipping directories: " ++ unwords (da_skipdirs da)

  say da $
    "Starting discover for " ++ show (da_tool da) ++ "\n" ++
    "Scanning under \"" ++ da_root da ++ "\"" ++
    " for .cabal files and \"" ++ build_dir ++ "\"\n" ++
    skipped_dirs_msg

  (src_dirs, build_dirs) <- findSrcDirsAndBuildDirs da build_dir
  say da $
    "Scanned:\n" ++
    "  Directories containing .cabal files:\n" ++ list_msg src_dirs ++
    "  Build dirs:\n" ++ list_msg build_dirs

  tix_path <- parseTixish (da_testsuite da)
  (mb_tix, mixs) <- findTixAndMix da tix_path build_dirs

  found_tix_path <- case mb_tix of
    Just tix -> pure tix
    Nothing  -> throwIO $ TestSuiteNotFound (da_testsuite da)

  say da $
    "Discovered:\n" ++
    "  Tix file: \n" ++ list_msg [found_tix_path] ++
    "  Mix dirs: \n" ++ list_msg mixs

  return $ mempty
    { reportTix = found_tix_path
    , reportMixDirs = mixs
    , reportSrcDirs = src_dirs
    }

data TixPath
  = UnresolvedTixPath FilePath
  | ResolvedTixPath FilePath

parseTixish :: String -> IO TixPath
parseTixish str = do
  let tix1 = if takeExtension str == ".tix"
                then str
                else str <.> "tix"
  tix1_found <- doesFileExist tix1
  if tix1_found
     then return $ ResolvedTixPath tix1
     else return $ UnresolvedTixPath tix1

defaultBuildDirName :: BuildTool -> String
defaultBuildDirName tool = case tool of
  Cabal -> "dist-newstyle"
  Stack -> ".stack-work"

-- | Show mssage to 'stdrr'.
say :: DiscoverArgs -> String -> IO ()
say da msg = when (da_verbose da) $ hPutStrLn stderr msg

findSrcDirsAndBuildDirs
  :: DiscoverArgs -> String -> IO ([FilePath], [FilePath])
findSrcDirsAndBuildDirs da build_dir = do
    ds <- if null $ da_root da
        then listDirectory "."
        else pure [da_root da]
    foldDirWithIgnoring ignored f z ds
  where
    z = ([], [])
    f p acc@(src_dirs, dirs)
      | takeExtension p_file == ".cabal" = pure (p_dir:src_dirs, dirs)
      | p_file == build_dir  = pure (src_dirs, p:dirs)
      | otherwise = pure acc
      where
        (p_dir, p_file) = splitFileName p
    ignored = build_dir : (defaultIgnored ++ da_skipdirs da)

findTixAndMix
  :: DiscoverArgs -> TixPath -> [FilePath]
  -> IO (Maybe FilePath, [FilePath])
findTixAndMix da tixish build_dirs = case da_tool da of
  Stack -> findForStack excludes tixish build_dirs
  Cabal -> findForCabal excludes tixish build_dirs
  where
    excludes = defaultIgnored ++ da_skipdirs da


-- ------------------------------------------------------------------------
--
-- Searching mix and tix for stack
--
-- ------------------------------------------------------------------------

findForStack
  :: [String] -> TixPath -> [FilePath] -> IO (Maybe FilePath, [FilePath])
findForStack excludes tx dirs = do
  mb_tix <- case tx of
    ResolvedTixPath path   -> pure $ Just path
    UnresolvedTixPath name -> findStackTix excludes name dirs
  mixs <- findStackMix excludes dirs
  pure (mb_tix, mixs)

findStackMix :: [String] -> [FilePath] -> IO [FilePath]
findStackMix ignored dirs = foldDirWithIgnoring ignored f [] dist_dirs
  where
    dist_dirs = map (</> "dist") dirs
    f p acc =
        pure $ if takeFileName p == "hpc"
                 then p : acc
                 else acc

findStackTix :: [String] -> String -> [FilePath] -> IO (Maybe FilePath)
findStackTix ignored tix_name dirs = go f Nothing install_dirs
  where
    go = foldDirWithIgnoring ignored
    install_dirs = map (</> "install") dirs
    f _ (Just tix) = return (Just tix)
    f p Nothing = if takeFileName p == tix_name
        then return (Just p)
        else return Nothing


-- ------------------------------------------------------------------------
--
-- Searching mix and tix for cabal-install
--
-- ------------------------------------------------------------------------

findForCabal
  :: [String] -> TixPath -> [FilePath] -> IO (Maybe FilePath, [FilePath])
findForCabal ignored tx = foldDirWithIgnoring ignored f z
  where
    f = case tx of
      ResolvedTixPath _ -> findVanilla ignored
      UnresolvedTixPath tix_name -> \p acc@(mb_tix, dirs) -> do
        if isNothing mb_tix && takeFileName p == tix_name
          then return (Just p, dirs)
          else findVanilla ignored p acc

    z = case tx of
      ResolvedTixPath path -> (Just path, [])
      _                    -> (Nothing, [])

findVanilla
  :: [String] -> FilePath -> (Maybe FilePath, [FilePath])
  -> IO (Maybe FilePath, [FilePath])
findVanilla ignored p acc@(mb_tix, dirs) = do
  if takeFileName p == "vanilla"
    then do
      let mix = p </> "mix"
      mix_exist <- doesDirectoryExist mix
      if mix_exist
         then do
           let f xs = [mix </> x| x <- xs, x `notElem` ignored]
           contents <- f <$> listDirectory mix
           return (mb_tix, contents ++ dirs)
         else return (mb_tix, dirs)
    else return acc


-- ------------------------------------------------------------------------
--
-- Simple directory walker
--
-- ------------------------------------------------------------------------

-- | Variant of 'foldDirWithIgnoring' with 'defaultIgnored'.
foldDir :: (FilePath -> a -> IO a) -> a -> [FilePath] -> IO a
foldDir = foldDirWithIgnoring defaultIgnored

-- | Default directory base names to ignore.
defaultIgnored :: [String]
defaultIgnored = [".git", ".github"]

-- | Fold under given directory.
foldDirWithIgnoring
  :: [String]
  -- ^ Directory base names to skip.
  -> (FilePath -> a -> IO a)
  -- ^ Accumulator function.
  -> a
  -- ^ Initial accumulator value.
  -> [FilePath]
  -- ^ Directories to walk through.
  -> IO a
foldDirWithIgnoring ignored f = go
  where
    go acc0 [] = return acc0
    go acc0 (dir:dirs) = do
      acc1 <- f dir acc0
      if takeFileName dir `elem` ignored
         then go acc1 dirs
         else do
           is_dir <- doesDirectoryExist dir
           if not is_dir
             then go acc1 dirs
             else do
               contents <- map (dir </>) <$> listDirectory dir
               acc2 <- go acc1 contents
               go acc2 dirs
