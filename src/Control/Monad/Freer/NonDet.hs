-- |
-- Module:       Control.Monad.Freer.NonDet
-- Description:  Non deterministic effects
-- Copyright:    2017 Ixperta Solutions s.r.o.; 2017 Alexis King
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
  ) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (msum)

import Control.Monad.Freer.Internal
  ( Eff(..)
  , Member
  , NonDet(..)
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
makeChoiceA = handleRelay (pure . pure) $ \m k ->
  case m of
    MZero -> pure empty
    MPlus -> (<|>) <$> k True <*> k False

msplit
  :: Member NonDet effs
  => Eff effs a
  -> Eff effs (Maybe (a, Eff effs a))
msplit = loop []
  where
    loop jq (Val x) = pure (Just (x, msum jq))
    loop jq (E u q) = case prj u of
        Just MZero -> case jq of
          []      -> pure Nothing
          (j:jq') -> loop jq' j
        Just MPlus -> loop (qApp q False : jq) (qApp q True)
        Nothing    -> E u (tsingleton k)
      where
        k = qComp q (loop jq)
