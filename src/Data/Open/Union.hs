{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.Open.Union
Description : Open unions (type-indexed co-products) for extensible effects.
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

This implementation relies on _closed_ type families added to GHC
7.8. It has NO overlapping instances and NO Typeable. Alas, the
absence of Typeable means the projections and injections generally
take linear time.  The code illustrate how to use closed type families
to disambiguate otherwise overlapping instances.

The data constructors of Union are not exported. Essentially, the
nested Either data type.

Using <http://okmij.org/ftp/Haskell/extensible/OpenUnion41.hs> as a
starting point.

-}

module Data.Open.Union (
  module Data.Open.Union,
  Union,
  Member(..),
  decomp,
  weaken,
  extract,
  Functor(..)
) where

import GHC.Exts
import Data.Open.Union.Internal

--------------------------------------------------------------------------------
                           -- Interface --
--------------------------------------------------------------------------------
type family Members m r :: Constraint where
  Members (t ': c) r = (Member t r, Members c r)
  Members '[] r = ()

