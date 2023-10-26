module Lib.Foo where

yetAnotherFunc :: Int -> Int -> Int -> IO ()
yetAnotherFunc a b c
  | a <= b, b <= c = putStrLn "b in the middle"
  | b <= a, a <= c = putStrLn "a in the middle"
  | otherwise = pure ()
