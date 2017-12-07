-- |
-- Module:       Control.Monad.Freer.StateRW
-- Description:  State effects in terms of Reader and Writer.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'State' effects in terms of 'Reader' and 'Writer'
-- effects. This module is more a tutorial on how to compose handlers. It is
-- slightly slower than a dedicated 'State' handler.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.StateRW
  ( runStateR
  , Reader
  , Writer
  , tell
  , ask
  ) where

import Control.Monad.Freer.Reader (Reader(..), ask)
import Control.Monad.Freer.Writer (Writer(..), tell)
import Control.Monad.Freer.Internal (Eff(..), decomp, qComp, tsingleton)

-- | State handler, using 'Reader' and 'Writer' effects.
runStateR :: s -> Eff (Writer s ': Reader s ': effs) a -> Eff effs (a, s)
runStateR s' (Val x) = return (x, s')
runStateR s' (E u q) = case decomp u of
    Right (Tell o) -> k o ()
    Left  u'  -> case decomp u' of
      Right Ask -> k s' s'
      Left u'' -> E u'' (tsingleton (k s'))
  where
    k s'' = qComp q (runStateR s'')
