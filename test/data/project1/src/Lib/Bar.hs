module Lib.Bar
  ( module Lib.Bar
  , module Lib.Bar.Buzz
  ) where

import Lib.Bar.Buzz

theNewFunc :: Int -> Int -> Int -> IO ()
theNewFunc a b c
  | even a = print (a * b + c)
  | even b = print (a * c + b)
  | odd a, odd b = print (b * c + a)
  | otherwise = pure ()
