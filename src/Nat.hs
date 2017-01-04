{-# LANGUAGE
  DataKinds
, GADTs
, TypeFamilies
, TypeOperators
, UndecidableInstances
, UnicodeSyntax
#-}

{-|
Type to encode numbers.
-}
module Nat where

-- ℕ - term: type | type: kind
-- Z - term: val ctor | type: type (of kind: ℕ)
-- S - term: val ctor | type: type ctor (of kind: ℕ → ℕ)
data ℕ ∷ * where
  Z ∷ ℕ
  S ∷ ℕ → ℕ
  deriving (Show)

-- Or just…
-- data ℕ = Z | S ℕ
--   deriving (Show)

-- When referring to the promoted things (i.e. types),
-- we really oughta say 'Z and 'S.


-- Type-level fn
type family (n ∷ ℕ) + (m ∷ ℕ) where
  'Z   + m = m
  'S n + m = 'S (n + m)


-- Type-level fn
type family (n ∷ ℕ) * (m ∷ ℕ) where
  'Z   * m = 'Z
  'S n * m = m + (n * m)


-- infixr 6 :+
-- type family   (n ∷ ℕ) :+ (m ∷ ℕ) ∷ ℕ
-- type instance Z     :+ m = m
-- type instance (S n) :+ m = S (n :+ m)

-- infixr 7 :*
-- type family   (n ∷ ℕ) :* (m ∷ ℕ) ∷ ℕ
-- type instance Z     :* m = Z
-- type instance (S n) :* m = (n :* m) :+ m

