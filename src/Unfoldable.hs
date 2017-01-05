{-# LANGUAGE
  DataKinds
, GADTs
, StandaloneDeriving
, TypeFamilies
, TypeOperators
, UndecidableInstances
, UnicodeSyntax
#-}

{- |

An inductive way to build up values.

't' (type ctor) is Unfoldable if an instance of it can be built from:
  + an unfolding fn  ∷ β → (α, β)
  + an initial state ∷ β

Each instance's impl of `unfold` should behave thusly…
  + Run fn on init state (β); get the 1st item (α) and a new state (β).
  + Run fn on new state (β);  get the 2nd item (α) and the next state (β).
  + etc.

The fns `replicateU`, `iterateU`, and `fromListMaybes` all use the
`unfold` method. And they do so polymorphically.

As such, the context must provide the concrete type for the Unfoldable.
That way, Hs knows which impl of `unfold` to use.

In the case that the Unfoldable is one of the Vec family of types,
that type's impl of `unfold` will determine when to stop unfolding.

-}
module Unfoldable where


class Unfoldable t where
  unfold ∷ (β → (α, β)) → β → t α


replicateU ∷ Unfoldable t ⇒ α → t α
replicateU = unfold (\x → (x,x))


iterateU ∷ Unfoldable t ⇒ (α → α) → α → t α
iterateU f = unfold (\x → (x, f x))


fromListMaybes ∷ Unfoldable t ⇒ [α] → t (Maybe α)
fromListMaybes =
  unfold $ \list → case list of
                     []   → (Nothing, [])
                     x:xs → (Just x,  xs)
  

fromListU ∷ (Unfoldable t, Traversable t) ⇒ [α] → Maybe (t α)
fromListU = sequence . fromListMaybes


----------------------

-- List instance of Unfoldable.
-- (Build up a list from a single init value and a fn.)
instance Unfoldable [] where
  unfold f x0 =
    let (y, x1) = f x0
    in  y : unfold f x1

-- e.g.
-- > take 5 $ unfold (\x -> (x `mod` 3 ≡ 2, x² - 1)) 2
-- [True, False, True, False, True]
