{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH01

$fooDecl

$barDecl

main :: IO ()
main = do
  print foo
  print bar

