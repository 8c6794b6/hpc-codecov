{-# LANGUAGE TemplateHaskell #-}

module TH01 where

import Language.Haskell.TH

fooDecl :: Q [Dec]
fooDecl = [d| foo = 42; foo :: Int |]

barDecl :: Q [Dec]
barDecl = [d| bar :: Int; bar = 42 |]
