{-# LANGUAGE
  DataKinds
, FlexibleInstances
, GADTs
, StandaloneDeriving
, TypeFamilies
, TypeOperators
, UndecidableInstances
, UnicodeSyntax
#-}

module Vec where

import Control.Applicative  -- liftA2
import Data.Monoid
import GHC.Exts as L (IsList, Item, fromList, toList)
import Data.Foldable

import Nat
import Unfoldable

infixr 5 :#

-- Either way will work.
-- data Vec (n ∷ ℕ) α where
data Vec ∷ ℕ → * → * where
  Nil  ∷ Vec Z α
  (:#) ∷ α → Vec n α → Vec (S n) α

--------------------
-- typeclasses
--------------------

-- Must do standalone, as Vec isn't expressable in normal Hs.
deriving instance Show α ⇒ Show (Vec n α)
deriving instance Eq α ⇒ Eq (Vec n α)


{-|
Unfoldable
Once we fill up the vector, the unfolding stops.
(-XFlexibleInstances allows us to use 'Z here.)

We need separate instances of the typeclass, one for each.
Vec 'Z is a diff type from Vec (S n).
-}

instance Unfoldable (Vec 'Z) where
  unfold _ _ = Nil

{-|
Vec (S n) is unfoldable iff Vec n is unfoldable.
Inductive declaration. (If n is an instance, so is (S n).)
-}
instance Unfoldable (Vec n) ⇒ Unfoldable (Vec (S n)) where
  unfold f x0 =
    let (y, x1) = f x0
    in y :# unfold f x1

----------------------

instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :# xs) = f x :# fmap f xs

----------------------

{-|
Applicative
A Functor w/ application, providing ops to:
  + embed (lift) pure expressions (`pure`)
    pure :: a -> f a
  + sequence computations and combine their results (`<*>`)
    (<*>) :: f (a -> b) -> f a -> f b
-}
instance Applicative (Vec Z) where
  pure _    = Nil
  Nil <*> _ = Nil

{-|
`Vec (S n)` is Applicative iff `Vec n` is.
Gives us an Applicative instance for *any* type `Vec n`.
-}
instance Applicative (Vec n) ⇒ Applicative (Vec (S n)) where
  pure x =
    -- Knows when to stop based on Vec len
    x :# pure x
  (f :# fs) <*> (x :# xs) =
    -- Do fs and xs have to be of same len?
    -- Kinda like ZipList.
    f x :# (fs <*> xs)

{-|

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
Lift a binary function to actions.

> import Control.Applicative
> liftA2 (+) (1 :# 2 :# 3 :# Nil) (100 :# 201 :# 302 :# Nil)
101 :# 203 :# 305 :# Nil

-}

----------------------

{-| A container - not necessarily a Functor - whose items can be folded
into a summary value.

Min. def: `foldMap` | `foldr`
Inductive: `Vec (S n)` if Foldable iff `Vec n` also is.

Monoid has methods:
  + `mempty` and
  + `mappend`, aka: (<>)
-}
instance Foldable (Vec Z) where
  foldMap _ Nil = mempty

instance Foldable (Vec n) ⇒ Foldable (Vec (S n)) where
  foldMap f (x :# xs) = f x <> (foldMap f xs)

{-|
We don't need to define `toList` directly on `Vec`.
Instead, we make `Vec` Foldable, and we get `toList` for free.

> import Data.Foldable
> toList $ 1 :# 2 :# 3 :# Nil
[1,2,3]
>
-}

----------------------

{-| A container. Go thru structure (left to right), processing elements,
and *keeping* the shape of the structure - i.e. putting new values in.

Min. def: traverse | sequenceA

methods:
  + `traverse` and `sequenceA` are for `Applicative`s
  + `mapM` and `sequence` are for `Monad`s.

traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  1. Map each elem of structure to an action.
  2. Eval actions L → R.
  3. Collect results.
-}
instance Traversable (Vec Z) where
  -- `pure`: wraps in Applicative
  traverse _ Nil = pure Nil
  
instance Traversable (Vec n) ⇒ Traversable (Vec (S n)) where
  traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)

{-|

https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor-Identity.html

> import Data.Functor.Identity
> traverse Identity $ 1 :# 2 :# 3 :# Nil
Identity (1 :# (2 :# (3 :# Nil)))
>
> -- We have a Vec n (IO ())
> sequence_ $ putStrLn "hello" :# putStrLn "world" :# Nil
"hello"
"world"
>
> sequence $ Just 1 :# Just 2 :# Nil
Just (1 :# (2 :# Nil))
> sequence $ Just 1 :# Nothing :# Nil
Nothing
>

-}

----------------------

{-|
If you'd like to use list syntax,
this typeclass instance, together with -XOverloadedLists,
will allow doing so. But it may introduce runtime errors.

L.IsList asks for a type family to return
the type of the element in the container
from the container type.
-}
instance (Unfoldable (Vec n), Traversable (Vec n)) ⇒ L.IsList (Vec n α) where
  type Item (Vec n α) = α
  fromList xs = case fromListU xs of   -- fromListU defined below
    Nothing → error "Demanded Vec from a too-short List"
    Just ys → ys
  toList = Data.Foldable.toList

----------------------

singleton ∷ α → Vec (S Z) α
singleton x = x :# Nil


-- Not partial!
vhead ∷ Vec (S n) α → α
vhead (x :# _) = x


-- Not partial!
vtail ∷ Vec (S n) α → Vec n α
vtail (_ :# xs) = xs


-- append
(+#) ∷ Vec n α → Vec m α → Vec (n + m) α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)


-- assumes ys is sorted
insert ∷ (Ord α) ⇒ α → Vec n α → Vec (S n) α
insert x Nil       = singleton x
insert x (y :# ys)
  | x < y     = x :# y :# ys
  | otherwise = y :# (insert x ys)



{-|

With `Vec` being Traversable, we can use `sequence` on it.
  sequence :: Monad m => t (m a) -> m (t a)
  Evaluate each monadic action in structure L → R. Collect the results.

With `Vec` being Unfoldable, we can use `fromListMaybes` on it.
  fromListMaybes ∷ Unfoldable t ⇒ [α] → t (Maybe α)
  For `Vec`…
  Fill up vec with elems from list:
    + Nothing, if list is exhausted
    + Just x, otherwise

You have:
  + after applying `fromListMaybes`: Vec n (Maybe α)
  + after applying `sequence`:       Maybe (Vec n α)

> -- Building a Vec shorter than the list works.
> fromListU [1,2,3] :: Maybe (Vec (S (S Z)) Int)
Just (1 :# (2 :# Nil))
>
> -- Building a Vec of the same length as the list works, too.
> fromListU [1,2,3] :: Maybe (Vec (S (S (S Z))) Int)
Just (1 :# (2 :# (3 :# Nil)))
>
> -- Trying to create a Vec longer than the list gives you Nothing.
> fromListU [1,2,3] :: Maybe (Vec (S (S (S (S Z)))) Int)
Nothing
>

-}
fromListU ∷ (Unfoldable t, Traversable t) ⇒ [α] → Maybe (t α)
fromListU = sequence . fromListMaybes
