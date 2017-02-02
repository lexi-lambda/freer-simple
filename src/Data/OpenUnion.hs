{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
-- Module:       Data.OpenUnion
-- Description:  Open unions (type-indexed co-products) for extensible effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  POSIX
--
-- This implementation relies on _closed_ type families added to GHC 7.8. It
-- has NO overlapping instances and NO @Typeable@. Alas, the absence of
-- @Typeable@ means the projections and injections generally take linear time.
-- The code illustrate how to use closed type families to disambiguate
-- otherwise overlapping instances.
--
-- The data constructors of 'Union' are not exported. Essentially, the nested
-- 'Either' data type.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/OpenUnion41.hs> as a starting
-- point.
module Data.OpenUnion
    (
    -- * Open Union
      Union

    -- * Open Union Operations
    , decomp
    , weaken
    , extract

    -- * Open Union Membership Constraints
    , Member(..)
    , Members

    -- * Re-exported
    , Functor(..)
    )
  where

import Data.Functor (Functor(..))

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

import Data.OpenUnion.Internal
    ( Member(inj, prj)
    , Union
    , decomp
    , extract
    , weaken
    )


type family Members m r :: Constraint where
    Members (t ': c) r = (Member t r, Members c r)
    Members '[] r = ()
