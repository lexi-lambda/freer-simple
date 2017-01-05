{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Open.Union.Internal where

data Union (r :: [ * -> * ]) v where
  UNow  :: t v -> Union (t ': r) v
  UNext :: Union (t ': r) v -> Union (any ': t ': r) v

data Nat = S Nat | Z
data P (n :: Nat) = P

-- injecting/projecting at a specified position P n
class Member' t r (n :: Nat) where
  inj' :: P n -> t v -> Union r v
  prj' :: P n -> Union r v -> Maybe (t v)

instance (r ~ (t ': r')) => Member' t r 'Z where
  inj' _ = UNow
  prj' _ (UNow x) = Just x
  prj' _ _        = Nothing

instance (r ~ (t' ': r' ': rs'), Member' t (r' ': rs') n) => Member' t r ('S n) where
  inj' _ = UNext . inj' (P::P n)
  prj' _ (UNow _)  = Nothing
  prj' _ (UNext x) = prj' (P::P n) x

-- Find an index of an element in a `list'
-- The element must exist
-- This closed type family disambiguates otherwise overlapping
-- instances
type family FindElem (t :: * -> *) r :: Nat where
  FindElem t (t ': r)    = 'Z
  FindElem t (any ': r)  = 'S (FindElem t r)

type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = 'True
  EQU a b = 'False

type family Head (xs :: [x]) :: x where
    Head (x ': _) = x

type family Tail (xs :: [x]) :: [x] where
    Tail (_ ': xs) = xs

--------------------------------------------------------------------------------
                           -- Interface --
--------------------------------------------------------------------------------

{-# INLINE decomp #-}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (UNow x)  = Right x
decomp (UNext v) = Left v

{-# INLINE weaken #-}
weaken :: Union (t ': r) w -> Union (any ': t ': r) w
weaken = UNext

{-# INLINE extract #-}
extract :: Union '[t] v -> t v
extract (UNow x)  = x


class (Member' t r (FindElem t r), r ~ (Head r ': Tail r)) => Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

instance (Member' t r (FindElem t r), r ~ (Head r ': Tail r)) => Member t r where
  inj = inj' (P :: P (FindElem t r))
  prj = prj' (P :: P (FindElem t r))

instance (Functor f) => Functor (Union '[f]) where
  fmap f = inj . fmap f . extract
instance (Functor f1, Functor (Union (f2 ': fs))) =>
         Functor (Union (f1 ': f2 ': fs)) where
  fmap f = either (weaken . fmap f) (inj . fmap f) . decomp
