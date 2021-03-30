module Lib
    ( someFunc
    ) where

someFunc :: Int -> IO ()
someFunc n =
    if even n
      then putStrLn "got even number"
      else putStrLn "not a even number"
