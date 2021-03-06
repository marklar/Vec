{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

-- {-# OPTIONS_GHC -freduction-depth=0 #-}

import Test.Hspec

-- import GHC.TypeLits
import VecLit
import Unfoldable
import Control.Applicative
import Data.Foldable
import Data.Proxy

main :: IO ()
main =
  hspec $ do

  describe "vhead" $ do
    it "returns the head of a non-nil Vec" $ do
      vhead ('a' :# Nil) `shouldBe` 'a'
      vhead ('a' :# 'b' :# 'c' :# Nil) `shouldBe` 'a'
      vhead (1 :# 2 :# 3 :# Nil) `shouldBe` 1

    -- doesn't compile :-)
    -- it "shouldn't compile if attempted on a nil Vec" $ do
      -- vhead Nil `shouldBe` 'a'

  describe "vtail" $ do
    it "works with non-nil Vecs" $ do
      vtail ('a' :# Nil) `shouldBe` Nil
      vtail ('a' :# 'b' :# Nil) `shouldBe` 'b' :# Nil

  describe "append" $ do
    it "works when both Vecs are nil" $ do
      -- (Nil ∷ Vec 0 Int) +# Nil `shouldBe` Nil
      Nil +# Nil `shouldBe` (Nil ∷ Vec 0 Int)
    it "works when one Vec is nil" $ do
      Nil +# (1 :# Nil) `shouldBe` 1 :# Nil
      (1 :# Nil) +# Nil `shouldBe` 1 :# Nil
    it "works with both Vecs non-nil" $ do
      (1 :# 2 :# Nil) +# (11 :# 12 :# Nil) `shouldBe` 1 :# 2 :# 11 :# 12 :# Nil

  describe "insert" $ do
    it "works with nil Vec" $ do
      insert 1 Nil `shouldBe` 1 :# Nil
    it "sticks LT val at front" $ do
      insert 1 (2 :# 3 :# Nil) `shouldBe` 1 :# 2 :# 3 :# Nil
    it "inserts middling val in middle" $ do
      insert 2 (1 :# 3 :# Nil) `shouldBe` 1 :# 2 :# 3 :# Nil
    it "puts GT val at end" $ do
      insert 3 (1 :# 2 :# Nil) `shouldBe` 1 :# 2 :# 3 :# Nil
    it "includes both vals if EQ" $ do
      insert 2 (1 :# 2 :# Nil) `shouldBe` 1 :# 2 :# 2 :# Nil

  -- Unfoldable
  describe "unfold" $ do
    it "stops at the right 'n'" $ do
      -- Why can't use `≡` ?
      -- ((unfold (\x → (x `mod` 3 ≡ 2, x ^ 2 - 1)) 2) ∷ Vec 3 Bool) `shouldBe` True :# False :# True :# Nil
      ((unfold (\x → (x `mod` 3 == 2, x ^ 2 - 1)) 2) ∷ Vec 3 Bool) `shouldBe` True :# False :# True :# Nil
      ((unfold (\x → (x `mod` 3 == 2, x ^ 2 - 1)) 2) ∷ Vec 2 Bool) `shouldBe` True :# False :# Nil

  describe "iterateU" $ do
    it "stops at right n" $ do
      (iterateU succ 1 :: Vec 3 Int) `shouldBe` 1 :# 2 :# 3 :# Nil
      (iterateU succ 1 :: Vec 10 Int) `shouldBe` 1 :# 2 :# 3 :# 4 :# 5 :# 6 :# 7 :# 8 :# 9 :# 10 :# Nil

  describe "replicateU" $ do
    it "stops at right n" $ do
      (replicateU 'a' :: Vec 4 Char) `shouldBe` 'a' :# 'a' :# 'a' :# 'a' :# Nil
   

  -- Functor
  describe "fmap" $ do
    it "can be mapped over" $ do
      fmap (*3) (1 :# 2 :# 3 :# Nil) `shouldBe` (3 :# 6 :# 9 :# Nil)

  -- Foldable
  -- Without type annotation we get: Reduction stack overflow.
  describe "foldr'" $ do
    it "folds" $ do
      foldr' (:) [] ((1 :# 2 :# 3 :# Nil) ∷ Vec 3 Int) `shouldBe` [1, 2, 3]

  describe "toList" $ do
    it "can be toList-ed" $ do
      toList ((1 :# 2 :# 3 :# Nil) ∷ Vec 3 Int) `shouldBe` [1,2,3]

  describe "applicative" $ do
    it "can work w/ liftA2 and pure" $ do
      (pure 10 :: Vec 2 Int) `shouldBe` 10 :# 10 :# Nil
      (liftA2 (+) ((1 :# 2 :# 3 :# Nil) ∷ Vec 3 Int) ((100 :# 200 :# 300 :# Nil) ∷ Vec 3 Int)) `shouldBe` 101 :# 202 :# 303 :# Nil

  -- Traversable
  describe "sequence" $ do
    it "can be sequenced" $ do
      sequence ((Just 1 :# Just 2 :# Just 3 :# Nil) ∷ Vec 3 (Maybe Int)) `shouldBe` Just (1 :# 2 :# 3 :# Nil)

  describe "index" $ do
    it "can be indexed into" $ do
      index (Proxy ∷ Proxy 2) ((1 :# 2 :# 3 :# Nil) ∷ Vec 3 Int) `shouldBe` 3

