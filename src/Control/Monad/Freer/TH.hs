{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Originally ported from code written by Sandy Maguire (@isovector), available
-- at https://github.com/IxpertaSolutions/freer-effects/pull/28.

-- | Automatic generation of freer monadic actions.
module Control.Monad.Freer.TH
  ( makeEffect
  , makeEffect_
  )
where

import Control.Monad (forM, unless)
import Control.Monad.Freer (send, Member, Eff)
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Prelude


-- | @$('makeEffect' ''T)@ provides freer monadic actions for the constructors of
-- the given GADT @T@.
makeEffect :: Name -> Q [Dec]
makeEffect = genFreer True

-- | Like 'makeEffect', but does not provide type signatures.
-- This can be used to attach Haddock comments to individual arguments
-- for each generated function.
--
-- @
-- data Lang x where
--   Output :: String -> Lang ()
--
-- makeEffect_ 'Lang
--
-- -- | Output a string.
-- output :: Member Lang effs
--        => String    -- ^ String to output.
--        -> Eff effs ()  -- ^ No result.
-- @
--
-- 'makeEffect_' must be called *before* the explicit type signatures.
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

-- | Generates a type signature of the form
-- @x :: Member (Effect e) effs => a -> b -> c -> Eff effs r@.
genSig :: Con -> Q Dec
genSig (ForallC _       _      con                    ) = genSig con
genSig (GadtC   [cName] tArgs' ctrType@(AppT eff tRet)) = do
  effs <- newName "effs"
  let
    fnName    = getDeclName cName
    tArgs     = fmap snd tArgs'
    otherVars = unapply ctrType
    quantifiedVars =
      fmap PlainTV . nub $ effs : mapMaybe freeVarName (tArgs ++ otherVars)
    memberConstraint = ConT ''Member `AppT` eff `AppT` VarT effs
    resultType       = ConT ''Eff `AppT` VarT effs `AppT` tRet

  return
    .  SigD fnName
    .  ForallT quantifiedVars [memberConstraint]
    .  foldArrows
    $  tArgs
    ++ [resultType]
-- TODO: Although this should never happen, we obviously need a better error message below.
genSig GadtC{} = fail "genSig can only look at applications (AppT)"
genSig _       = fail "genSig expects a GADT constructor"

-- | Gets the name of the free variable in the 'Type', if it exists.
freeVarName :: Type -> Maybe Name
freeVarName (VarT n) = Just n
freeVarName _        = Nothing

-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 (AppT . AppT ArrowT)

-- | Unfolds a type into any types which were applied together.
unapply :: Type -> [Type]
unapply (AppT a b) = unapply a ++ unapply b
unapply a          = [a]
