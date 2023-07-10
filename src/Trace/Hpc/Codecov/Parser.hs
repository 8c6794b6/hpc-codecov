{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module:     Trace.Hpc.Codecov.Parser
-- Copyright:  (c) 2023 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Simple bytestring parser for reading @.mix@ and @.tix@ files.
--
-- @since 0.4.0.0

module Trace.Hpc.Codecov.Parser
  ( readTix'
  , readMix'
  ) where

-- base
import           Control.Applicative            (Alternative (..))
import           Data.Functor                   (($>))
import           Prelude                        hiding (takeWhile)

-- bytestring
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as BS

-- filepath
import           System.FilePath                ((<.>), (</>))

-- hpc
import           Trace.Hpc.Mix                  (BoxLabel (..),
                                                 CondBox (..), Mix (..),
                                                 MixEntry)
import           Trace.Hpc.Tix                  (Tix (..), TixModule (..),
                                                 tixModuleName)
import           Trace.Hpc.Util                 (HpcHash (..), HpcPos,
                                                 catchIO, toHpcPos)

-- time
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Data.Time.Clock                (UTCTime (..))


-- ------------------------------------------------------------------------
-- Exported
-- ------------------------------------------------------------------------

-- | Read a @.tix@ File.
--
-- This function is similar to 'Trace.Hpc.Tix.readTix', but internally
-- uses 'ByteString' to improve performance.
readTix' :: FilePath -> IO (Maybe Tix)
readTix' path =
  (runMaybeP parseTix <$> BS.readFile path) `catchIO` const (pure Nothing)

-- | Read a @.mix@ file.
--
-- This function is similar to 'Trace.Hpc.Mix.readMix', but internally
-- uses 'ByteString' to improve performance.
--
-- __NOTE__: At the moment, the 'UTCTime' field in the parsed 'Mix' is
-- constantly filled with dummy value, to avoid parsing date time.
readMix'
  :: [String] -- ^ Dir names
  -> Either String TixModule -- ^ module wanted
  -> IO Mix
readMix' dirs et_tm = go dirs
  where
    mixname = either id tixModuleName et_tm <.> "mix"
    handler _ = pure (Left "err ...")
    parse path = runEitherP parseMix <$> BS.readFile path
    go [] = error "Cannot find mix file"
    go (d:ds) = do
      et_mix <- parse (d </> mixname) `catchIO` handler
      case et_mix of
        Right mix -> pure mix
        Left _err -> go ds


-- ------------------------------------------------------------------------
-- The parser
-- ------------------------------------------------------------------------

newtype P a =
  P {runP :: forall r. (String -> r) -- On error
          -> (a -> ByteString -> r)  -- On success
          -> ByteString              -- Input to consume
          -> r}

instance Functor P where
  fmap f p = P (\err ok -> runP p err (ok . f))
  {-# INLINE fmap #-}

instance Applicative P where
  pure x = P (\_ ok -> ok x)
  {-# INLINE pure #-}

  pf <*> pa = P (\err ok -> runP pf err (\f -> runP (fmap f pa) err ok))
  {-# INLINE (<*>) #-}

instance Monad P where
  m >>= k = P (\err ok -> runP m err (\x -> runP (k x) err ok))
  {-# INLINE (>>=) #-}

instance Alternative P where
  empty = P (\err _ _ -> err "Alternative.empty")
  {-# INLINE empty #-}

  p1 <|> p2 = P (\err go bs -> runP p1 (\_ -> runP p2 err go bs) go bs)
  {-# INLINE (<|>) #-}

runEitherP :: P a -> ByteString -> Either String a
runEitherP p = runP p Left (\a _ -> Right a)

runMaybeP :: P a -> ByteString -> Maybe a
runMaybeP p = runP p (const Nothing) (\a _ -> Just a)

char :: Char -> P ()
char c =
  P (\err ok bs ->
       case BS.uncons bs of
         Just (c', bs') | c == c' -> ok () bs'
         _                        -> err ("char: failed to get " <> show c))
{-# INLINABLE char #-}

bytes :: ByteString -> P ()
bytes target =
  P (\err ok bs ->
        case BS.splitAt (BS.length target) bs of
          (pre, post) | pre == target -> ok () post
          _ -> err ("bytes: failed to parse `" <> show target <> "'"))
{-# INLINABLE bytes #-}

int :: P Int
int =
  P (\err ok bs ->
       case BS.readInt bs of
         Just (n, bs') -> ok n bs'
         _             -> err "int: failed")
{-# INLINABLE int #-}

integer :: P Integer
integer = fmap fromIntegral int
{-# INLINEABLE integer #-}

spaces :: P ()
spaces = P (\_ ok bs -> ok () (BS.dropSpace bs))
{-# INLINABLE spaces #-}

takeWhile :: (Char -> Bool) -> P ByteString
takeWhile test =
  P (\_ ok bs -> case BS.span test bs of (pre, post) -> ok pre post)
{-# INLINABLE takeWhile #-}

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy a s = sepBy1 a s <|> pure []
{-# INLINEABLE sepBy #-}

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 a s = go
  where
    go = (:) <$> a <*> ((s *> go) <|> pure [])
{-# INLINABLE sepBy1 #-}

doubleQuoted :: P a -> P a
doubleQuoted p = char '"' *> p <* char '"'
{-# INLINEABLE doubleQuoted #-}

bracketed :: P a -> P a
bracketed p = char '[' *> p <* char ']'
{-# INLINABLE bracketed #-}

parenthesized :: P a -> P a
parenthesized p = char '(' *> p <* char ')'
{-# INLINABLE parenthesized #-}

comma :: P ()
comma = char ','
{-# INLINABLE comma #-}

bool :: P Bool
bool = true <|> false
  where
    true = bytes "True" $> True
    false = bytes "False" $> False
{-# INLINABLE bool #-}

string :: P String
string = BS.unpack <$> doubleQuoted (takeWhile (/= '"'))
{-# INLINABLE string #-}


-- ------------------------------------------------------------------------
-- Tix parser
-- ------------------------------------------------------------------------

parseTix :: P Tix
parseTix = do
  bytes "Tix" *> spaces
  tix_modules <- bracketed (sepBy tixModule comma)
  pure (Tix tix_modules)

tixModule :: P TixModule
tixModule = do
  spaces *> bytes "TixModule" *> spaces
  name <- string <* spaces
  hash <- fmap toHash int <* spaces
  size <- int <* spaces
  ticks <- bracketed (sepBy integer comma)
  pure (TixModule name hash size ticks)


-- ------------------------------------------------------------------------
-- Mix parser
-- ------------------------------------------------------------------------

parseMix :: P Mix
parseMix = do
  bytes "Mix" *> spaces
  path <- string <* spaces
  _year <- takeWhile (/= ' ') <* spaces
  _time <- takeWhile (/= ' ') <* spaces
  _zone <- takeWhile (/= ' ') <* spaces
  hash <- fmap toHash int <* spaces
  tabstop <- int <* spaces
  let dummy_date = UTCTime (fromOrdinalDate 1900 1) 0
  Mix path dummy_date hash tabstop <$> mixEntries

mixEntries :: P [MixEntry]
mixEntries = bracketed (sepBy mixEntry comma)
{-# INLINABLE mixEntries #-}

mixEntry :: P MixEntry
mixEntry = parenthesized $ do
  pos <- hpcPos
  comma
  box <- boxLabel
  pure (pos, box)
{-# INLINABLE mixEntry #-}

hpcPos :: P HpcPos
hpcPos = do
  sl <- int
  char ':'
  sc <- int
  char '-'
  el <- int
  char ':'
  ec <- int
  pure (toHpcPos (sl, sc, el, ec))
{-# INLINABLE hpcPos #-}

boxLabel :: P BoxLabel
boxLabel = expBox <|> topLevelBox <|> localBox <|> binBox
  where
    expBox = bytes "ExpBox" *> spaces *> fmap ExpBox bool
    topLevelBox = bytes "TopLevelBox" *> spaces *> fmap TopLevelBox names
    localBox = bytes "LocalBox" *> spaces *> fmap LocalBox names
    binBox = bytes "BinBox" *> spaces *>
             (BinBox <$> (condBox <* spaces) <*> bool)
{-# INLINABLE boxLabel #-}

names :: P [String]
names = bracketed (sepBy string comma)
{-# INLINABLE names #-}

condBox :: P CondBox
condBox = guard <|> cond <|> qual
  where
    guard = bytes "GuardBinBox" $> GuardBinBox
    cond = bytes "CondBinBox" $> CondBinBox
    qual = bytes "QualBinBox" $> QualBinBox
{-# INLINABLE condBox #-}
