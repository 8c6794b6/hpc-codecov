{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

data R = R
  { field1 :: Bool
  , field2 :: Bool
  }

pm01 :: Maybe Int -> Int
pm01 (Just n) =
  n + 1
pm01 Nothing =
  12345

br01 :: Int -> Int -> Int
br01 a b
  | a < b = b
  | a == 0 = b
  | b < 0 = a + b
  | otherwise = 12345

newtype A f x = A {unA :: f x}
  deriving (Functor, Applicative, Monad)

newtype B f x = B {unB :: f x}

instance Functor f => Functor (B f) where
  fmap f (B fx) = B (fmap f fx)

br02 :: Int -> Int -> Int -> Int
br02 a b c
  | a < b = b
  | a == 0 = b
  | b < c = a + b
  | otherwise = 12345

main :: IO ()
main = do
  case R True False of
    R t f -> print (t && f)
  print $ pm01 Nothing
  print $ br01 2 1
  print $ br02 0 (-1) 0
