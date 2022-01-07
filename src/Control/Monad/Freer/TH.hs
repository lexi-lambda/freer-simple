{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Originally ported from code written by Sandy Maguire (@isovector), available
-- at https://github.com/IxpertaSolutions/freer-effects/pull/28.

{-|
This module provides Template Haskell functions for automatically generating
effect operation functions (that is, functions that use 'send') from a given
effect algebra. For example, using the @FileSystem@ effect from the example in
the module documentation for "Control.Monad.Freer", we can write the following:

@
data FileSystem r where
  ReadFile :: 'FilePath' -> FileSystem 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
'makeEffect' ''FileSystem
@

This will automatically generate the following functions:

@
readFile :: 'Member' FileSystem effs => 'FilePath' -> 'Eff' effs 'String'
readFile a = 'send' (ReadFile a)

writeFile :: 'Member' FileSystem effs => 'FilePath' -> 'String' -> 'Eff' effs ()
writeFile a b = 'send' (WriteFile a b)
@
-}
module Control.Monad.Freer.TH
  ( makeEffect
  , makeEffect_
  )
where

import Control.Monad (forM, unless)
import Control.Monad.Freer (send, Member, Eff)
import Data.Char (toLower)
import Language.Haskell.TH
import Prelude


-- | If @T@ is a GADT representing an effect algebra, as described in the module
-- documentation for "Control.Monad.Freer", @$('makeEffect' ''T)@ automatically
-- generates a function that uses 'send' with each operation. For more
-- information, see the module documentation for "Control.Monad.Freer.TH".
makeEffect :: Name -> Q [Dec]
makeEffect = genFreer True

-- | Like 'makeEffect', but does not provide type signatures. This can be used
-- to attach Haddock comments to individual arguments for each generated
-- function.
--
-- @
-- data Lang x where
--   Output :: String -> Lang ()
--
-- makeEffect_ ''Lang
--
-- -- | Output a string.
-- output :: Member Lang effs
--        => String    -- ^ String to output.
--        -> Eff effs ()  -- ^ No result.
-- @
--
-- Note that 'makeEffect_' must be used /before/ the explicit type signatures.
makeEffect_ :: Name -> Q [Dec]
makeEffect_ = genFreer False

-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Eff' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer makeSigs tcName = do
  -- The signatures for the generated definitions require FlexibleContexts.
  isExtEnabled FlexibleContexts
    >>= flip unless (fail "makeEffect requires FlexibleContexts to be enabled")

  reify tcName >>= \case
    TyConI (DataD _ _ _ _ cons _) -> do
      sigs <- filter (const makeSigs) <$> mapM genSig cons
      decs <- mapM genDecl cons
      return $ sigs ++ decs

    _ -> fail "makeEffect expects a type constructor"

-- | Given the name of a GADT constructor, return the name of the corresponding
-- lifted function.
getDeclName :: Name -> Name
getDeclName = mkName . overFirst toLower . nameBase
 where
  overFirst f (a : as) = f a : as
  overFirst _ as       = as

-- | Builds a function definition of the form @x a b c = send $ X a b c@.
genDecl :: Con -> Q Dec
genDecl (ForallC _       _     con) = genDecl con
genDecl (GadtC   [cName] tArgs _  ) = do
  let fnName = getDeclName cName
  let arity  = length tArgs - 1
  dTypeVars <- forM [0 .. arity] $ const $ newName "a"
  return $ FunD fnName . pure $ Clause
    (VarP <$> dTypeVars)
    (NormalB . AppE (VarE 'send) $ foldl
      (\b -> AppE b . VarE)
      (ConE cName)
      dTypeVars
    )
    []
genDecl _ = fail "genDecl expects a GADT constructor"

-- | Generates a function type from the corresponding GADT type constructor
-- @x :: Member (Effect e) effs => a -> b -> c -> Eff effs r@.
genType :: Con -> Q Type
genType (ForallC tyVarBindings conCtx con)
  = ForallT tyVarBindings conCtx <$> genType con
genType (GadtC   _ tArgs' (AppT eff tRet)) = do
  effs <- newName "effs"
  let
    tArgs            = fmap snd tArgs'
    memberConstraint = ConT ''Member `AppT` eff `AppT` VarT effs
    resultType       = ConT ''Eff `AppT` VarT effs `AppT` tRet

  return
#if MIN_VERSION_template_haskell(2,17,0)
    .  ForallT [PlainTV effs SpecifiedSpec] [memberConstraint]
#else
    .  ForallT [PlainTV effs] [memberConstraint]
#endif
    .  foldArrows
    $  tArgs
    ++ [resultType]
-- TODO: Although this should never happen, we obviously need a better error message below.
genType _       = fail "genSig expects a GADT constructor"

-- | Turn all (KindedTV tv StarT) into (PlainTV tv) in the given type
-- This can prevent the need for KindSignatures
simplifyBndrs :: Type -> Type
simplifyBndrs (ForallT bndrs tcxt t) = ForallT (map simplifyBndr bndrs) tcxt (simplifyBndrs t)
simplifyBndrs (AppT t1 t2) = AppT (simplifyBndrs t1) (simplifyBndrs t2)
simplifyBndrs (SigT t k) = SigT (simplifyBndrs t) k
simplifyBndrs (InfixT t1 n t2) = InfixT (simplifyBndrs t1) n (simplifyBndrs t2)
simplifyBndrs (UInfixT t1 n t2) = InfixT (simplifyBndrs t1) n (simplifyBndrs t2)
simplifyBndrs (ParensT t) = ParensT (simplifyBndrs t)
simplifyBndrs t = t

-- | Turn TvVarBndrs of the form (KindedTV tv StarT) into (PlainTV tv)
-- This can prevent the need for KindSignatures
#if MIN_VERSION_template_haskell(2,17,0)
simplifyBndr :: TyVarBndrSpec -> TyVarBndrSpec
simplifyBndr (KindedTV tv f StarT) = PlainTV tv f
#else
simplifyBndr :: TyVarBndr -> TyVarBndr
simplifyBndr (KindedTV tv StarT) = PlainTV tv
#endif
simplifyBndr bndr = bndr

-- | Generates a type signature of the form
-- @x :: Member (Effect e) effs => a -> b -> c -> Eff effs r@.
genSig :: Con -> Q Dec
genSig con = do
  let
    getConName (ForallC _ _ c) = getConName c
    getConName (GadtC [n] _ _) = pure n
    getConName c = fail $ "failed to get GADT name from " ++ show c
  conName <- getConName con
  SigD (getDeclName conName) <$> simplifyBndrs <$> genType con

-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 (AppT . AppT ArrowT)
