{-# LANGUAGE
  ConstraintKinds
, DataKinds
, FlexibleInstances
, GADTs
, StandaloneDeriving
, TypeFamilies
, TypeOperators
, UndecidableInstances
, UnicodeSyntax
#-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module VecLit where

import Control.Applicative  -- liftA2
import Data.Foldable
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import Unfoldable
import qualified GHC.Exts as L (IsList(..))


{-| The TypeLit comparison operators aren't convenient. So we define our
own, based on theirs.

TypeLit provides the type family `CmpNat x y` which returns an
`Ordering` (of `LT`, `EQ`, or `GT`).

`type x < y` makes `1 < 2` a *constraint*.
-}
type x > y = CmpNat x y ~ 'GT
-- type x < y = CmpNat x y ~ 'LT


infixr 5 :#

data Vec ∷ Nat → * → * where
  Nil  ∷ Vec 0 α
  -- Be consistent!
  -- For inductive "proofs", always use
  -- n-1 on the left and n on the right.
  (:#) ∷ α → Vec (n - 1) α → Vec n α

deriving instance Show α ⇒ Show (Vec n α)
deriving instance Eq α ⇒ Eq (Vec n α)


vhead ∷ (n > 0) ⇒ Vec n α → α
vhead (x :# _) = x


vtail ∷ (n > 0) ⇒ Vec n α → Vec (n - 1) α
vtail (_ :# xs) = xs


-- append
(+#) ∷ Vec n α → Vec m α → Vec (n + m) α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)


-- assumes ys is sorted
insert ∷ (Ord α) ⇒ α → Vec n α → Vec (n+1) α
insert x Nil = x :# Nil
insert x (y :# ys)
  | x < y     = x :# y :# ys
  | otherwise = y :# (insert x ys)


-----------------------

{-|
type synomyms:
https://mail.haskell.org/pipermail/glasgow-haskell-users/2008-October/015794.html
instance (Unfoldable (Vec n), Vec (n + 1) ~ t) => Unfoldable t where
-}

instance Unfoldable (Vec 0) where
  unfold _ _ = Nil

-- Need: -XIncoherentInstances for this. Why?
instance (n > 0, Unfoldable (Vec (n - 1))) => Unfoldable (Vec n) where
  unfold f x0 =
    let (y, x1) = f x0
    in y :# unfold f x1


instance Foldable (Vec 0) where
  foldMap _ Nil = mempty

instance (n > 0, Foldable (Vec (n - 1))) ⇒ Foldable (Vec n) where
  -- <>: mappend
  foldMap f (x :# xs) = f x <> (foldMap f xs)


instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :# xs) = f x :# fmap f xs

------

-- class (Functor f) => Applicative f where
--   -- pure ~ return
--   pure :: a -> f a
--   -- <*> ~ fmap (but first extracting fn from inside f (functor)
--   -- somewhat ~ to >>=
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative (Vec 0) where
  pure _    = Nil
  Nil <*> _ = Nil

-- For this, need: -freduction-depth=0
instance (n > 0, Applicative (Vec (n - 1))) ⇒ Applicative (Vec n) where
  pure x = x :# pure x
  (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)

------

instance Traversable (Vec 0) where
  traverse _ Nil = pure Nil

-- instance (n > 0, Traversable (Vec (n - 1)), Vec n ~ t) ⇒ Traversable t where
instance (n > 0, Traversable (Vec (n - 1))) ⇒ Traversable (Vec n) where
  traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)


--------------

{-| Typeclass Index.

An ad-hoc typeclass. It's specifically for providing a single fn on
the family of Vec types.

(Another alternative: singletons. You replace typeclasses with type
families and more parametrized types.

Parametrized off two types (`n` and `v`).  The pair of them has an
instance of Index if they match the constraints below.

To be a proper index, 'n' must always be less than 'm'.
  + 'n' - index into Vec
  + 'm' - Vec len

Need -XScopedTypeVariables to say that `n` in the type signature is
the same as the `n` in `Proxy x`.
-}

class Index (n ∷ Nat) v where
  index ∷ Proxy n → v α → α

instance (m > 0) ⇒ Index 0 (Vec m) where
  index _ (x :# _) = x

instance ∀ n m. (n > 0, m > 0, Index (n-1) (Vec (m-1))) ⇒ Index n (Vec m) where
  index _ (_ :# xs) = index (Proxy ∷ Proxy (n - 1)) xs


--------------


{-| In order to use list syntax w/ -XOverloadedLists.
-}

instance (Unfoldable (Vec n), Traversable (Vec n)) ⇒ L.IsList (Vec n α) where
  type Item (Vec n α) = α
  fromList xs = case fromListU xs of
    Nothing → error "List too short"   -- can produce runtime error
    Just ys → ys
  toList = Data.Foldable.toList
