{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.NonDet
-- Description:  Non deterministic effects
-- Copyright:    2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'NonDet' effects.
module Control.Monad.Freer.NonDet
    ( NonDet(..)
    , makeChoiceA
    , msplit
    )
  where

import Control.Applicative (Alternative, (<|>), empty, pure)
import Control.Monad (liftM2, msum, return)
import Data.Bool (Bool(False, True))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing))

import Control.Monad.Freer.Internal
    ( Eff(E, Val)
    , Member
    , NonDet(MPlus, MZero)
    , handleRelay
    , prj
    , qApp
    , qComp
    , tsingleton
    )


-- | A handler for nondeterminstic effects.
makeChoiceA
    :: Alternative f
    => Eff (NonDet ': effs) a
    -> Eff effs (f a)
makeChoiceA = handleRelay (return . pure) $ \m k ->
    case m of
        MZero -> return empty
        MPlus -> liftM2 (<|>) (k True) (k False)

msplit
    :: Member NonDet effs
    => Eff effs a
    -> Eff effs (Maybe (a, Eff effs a))
msplit = loop []
  where
    loop jq (Val x) = return (Just (x, msum jq))
    loop jq (E u q) = case prj u of
        Just MZero -> case jq of
            []      -> return Nothing
            (j:jq') -> loop jq' j
        Just MPlus -> loop (qApp q False : jq) (qApp q True)
        Nothing    -> E u (tsingleton k)
      where
        k = qComp q (loop jq)
