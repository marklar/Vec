{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude hiding (return)
import Vec

main ∷ IO ()
main = do
  let v0 = 1 :# 3 :# 5 :# Nil
  print $ v0
  print $ insert 2 v0
