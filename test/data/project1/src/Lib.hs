module Lib
    ( someFunc
    , anotherFunc
    , yetAnotherFunc
    , theNewFunc
    , theReturnedFunc
    ) where

import Lib.Bar
import Lib.Foo

someFunc :: Int -> IO ()
someFunc n =
    if even n
      then putStrLn "got even number"
      else putStrLn "not a even number"

anotherFunc :: Int -> Int -> Int -> IO ()
anotherFunc a b c
  | 0 < a, 0 < b, 0 < c = putStrLn "all positive"
  | even a, even b, even c = putStrLn "all even"
  | otherwise = pure ()

