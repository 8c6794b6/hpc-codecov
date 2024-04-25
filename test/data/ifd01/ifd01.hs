module Main where

pre :: R -> IO ()
pre (R f1 f2)
  | f1 = putStrLn "=== ifd01.hs: f1 ==="
  | f2 = putStrLn "=== ifd01.hs: f2 ==="
  | otherwise = putStrLn "=== ifd01.hs ==="

data R = R
  { field1 :: Bool
  , field2 :: Bool
  } deriving (Show)

post :: R -> IO ()
post (R f1 f2)
  | f1 = putStrLn "=== the end: f1 ==="
  | f2 = putStrLn "=== the end: f2 ==="
  | otherwise = putStrLn "=== the end ==="

main :: IO ()
main = do
  pre $ R True False
  print $ R True False
  post $ R False True
