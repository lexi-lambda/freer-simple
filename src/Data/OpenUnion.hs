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
-- Portability:  GHC specific language extensions.
--
-- Open unions (type-indexed co-products, i.e. type-indexed sums) for
-- extensible effects All operations are constant-time.
module Data.OpenUnion
    (
    -- * Open Union
      Union

    -- * Open Union Operations
    , Weakens(..)
    , (:++:)
    , decomp
    , weaken
    , extract

    -- * Open Union Membership Constraints
    , Member(..)
    , Members
    , LastMember
    )
  where

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

import Data.OpenUnion.Internal
    ( Member(inj, prj)
    , Union
    , Weakens(weakens)
    , (:++:)
    , decomp
    , extract
    , weaken
    )


type family Members m r :: Constraint where
    Members (t ': c) r = (Member t r, Members c r)
    Members '[] r = ()

type family Last effs where
  Last (eff ': '[]) = eff
  Last (_ ': effs) = Last effs

class (Member m effs, m ~ Last effs) => LastMember m effs
instance (Member m effs, m ~ Last effs) => LastMember m effs
