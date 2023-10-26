module Lib.Bar.Buzz where

theReturnedFunc :: Int -> Int -> Int -> IO ()
theReturnedFunc a b c = print (a + b + c)

(<&&>) :: Bool -> Bool -> Bool
a <&&> b = a && b
