{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:       Data.OpenUnion.Internal
-- Description:  Open unions (type-indexed co-products) for extensible effects.
--
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- These are internal definitions and should be used with caution. There are no
-- guarantees that the API of this module will be preserved between minor
-- versions of this package.
module Data.OpenUnion.Internal
  where

import Data.Bool (Bool(False, True))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right), either)
import Data.Function ((.))
import Data.Functor (Functor(fmap))


data Union (r :: [ * -> * ]) v where
  UNow  :: t v -> Union (t ': r) v
  UNext :: Union (t ': r) v -> Union (any ': t ': r) v

-- | Type level naturals are used to disambiguate otherwise overlapping
-- instances when iterating through a type list.
data Nat = S Nat | Z

-- | Represents position @(n :: 'Nat')@ in a type list.
data P (n :: Nat) = P

-- | Injecting\/projecting at a specified position @P n@.
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

-- | Find an index of an element in a type list. The element must exist.
--
-- This closed type family disambiguates otherwise overlapping instances.
type family FindElem (t :: * -> *) r :: Nat where
    FindElem t (t ': r)    = 'Z
    FindElem t (any ': r)  = 'S (FindElem t r)

type family EQU (a :: k) (b :: k) :: Bool where
    EQU a a = 'True
    EQU a b = 'False

type family Head (xs :: [x]) :: x where
    Head (x ': xs) = x

type family Tail (xs :: [x]) :: [x] where
    Tail (x ': xs) = xs

--------------------------------------------------------------------------------
                           -- Interface --
--------------------------------------------------------------------------------

decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (UNow x)  = Right x
decomp (UNext v) = Left v
{-# INLINE decomp #-}

weaken :: Union (t ': r) w -> Union (any ': t ': r) w
weaken = UNext
{-# INLINE weaken #-}

extract :: Union '[t] v -> t v
extract (UNow x)  = x
{-# INLINE extract #-}

class (Member' t r (FindElem t r), r ~ (Head r ': Tail r)) => Member t r where
    inj :: t v -> Union r v
    prj :: Union r v -> Maybe (t v)

instance (Member' t r (FindElem t r), r ~ (Head r ': Tail r)) => Member t r
  where
    inj = inj' (P :: P (FindElem t r))
    prj = prj' (P :: P (FindElem t r))

instance (Functor f) => Functor (Union '[f]) where
    fmap f = inj . fmap f . extract

instance
    ( Functor f1, Functor (Union (f2 ': fs))
    ) => Functor (Union (f1 ': f2 ': fs))
  where
    fmap f = either (weaken . fmap f) (inj . fmap f) . decomp
