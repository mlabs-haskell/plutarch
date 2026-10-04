{-# LANGUAGE TemplateHaskell #-}

-- | @since 2.0.0
module Plutarch.Helpers.TH
  ( checkTyName,
    bindToName,
    fullTypeName,
    mkContextOf,
    mkContextOfExtended,
    mkUncons,
    toSomeTermE,
    punsafeCaseE,
    punsafeConstrE,
    punsafeConstantE,
    pequalsIntegerE,
    plam'E,
    punsafeCoerceE,
    punConstrDataE,
    punListDataE,
    pnilDataE,
    pmkConsE,
    pconstrDataE,
    pequalsDataE,
    pheadTailListE,
    punIDataE,
    mkPLam,
    mkAnonPLam,
    PType (..),
    PTypeSum (..),
    PTypeProduct (..),
    consToPTypeSum,
    unwrapField,
    isTypeRecursive,
    checkAndMark,
    terminateList,
  )
where

import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.Kind qualified as GHC
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH
  ( Bang,
    BndrVis,
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec,
    Exp (AppE, LamE, VarE),
    Info (TyConI),
    Name,
    Pat (VarP, WildP),
    Q,
    TyVarBndr (KindedTV, PlainTV),
    Type (AppT, ConT, InfixT, TupleT, VarT),
    newName,
    reify,
  )
import Plutarch.Backend.Term (Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun
  ( pconstrData,
    pequalsData,
    pequalsInteger,
    pmkCons,
    pnilData,
    punConstrData,
    punIData,
    punListData,
  )
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil))
import Plutarch.Primitive.DerivedFun (pheadTailList)
import Plutarch.Primitive.Singular (PAsData, (:-->))
import Plutarch.Primitive.Term
  ( plam',
    punsafeCase,
    punsafeCoerce,
    punsafeConstant,
    punsafeConstr,
  )

-- | Return the declaration of a type given its name, or error out if the name
-- does not name a type.
--
-- @since 2.0.0
checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

-- | Given a binder, yield the name it binds.
--
-- @since 2.0.0
bindToName :: TyVarBndr BndrVis -> Name
bindToName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

-- | Given a \'base\' type name, and a list of type variable binds, construct
-- the type name with all those binds applied.
--
-- @since 2.0.0
fullTypeName ::
  forall (f :: GHC.Type -> GHC.Type).
  (Foldable f) => Name -> f (TyVarBndr BndrVis) -> Type
fullTypeName tyName = foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName)

-- | Given a 'Vector' of type variables, and a list of single-parameter type
-- class names, construct a context asserting all of the type variables are
-- instances of all of these type classes.
--
-- @since 2.0.0
mkContextOf :: [Name] -> Vector (TyVarBndr BndrVis) -> Type
mkContextOf tyClassNames tyVars =
  let varLen = Vector.length tyVars
      classesLen = length tyClassNames
      varNames = [AppT (ConT tcn) . VarT . bindToName $ bn | tcn <- tyClassNames, bn <- Vector.toList tyVars]
   in foldl' AppT (TupleT (varLen * classesLen)) varNames

-- | @since 2.0.0
mkContextOfExtended :: [Name] -> Vector (TyVarBndr BndrVis) -> Type -> Type
mkContextOfExtended tyClassNames tyVars fieldTy =
  let varLen = Vector.length tyVars
      classesLen = length tyClassNames
      varNames = [AppT (ConT tcn) . VarT . bindToName $ bn | tcn <- tyClassNames, bn <- Vector.toList tyVars]
      fieldTyNames = [AppT (ConT tcn) fieldTy | tcn <- tyClassNames]
   in foldl' AppT (TupleT (varLen * classesLen + 1)) $ fieldTyNames <> varNames

-- | Given a name @ell@ corresponding to a @PDList@ variable, constructs the
-- equivalent of:
--
-- @
-- punsafeCase ell . NEVector.singleton . toSomeTerm $ f headName tailName
-- @
--
-- @headName@ and @tailName@ are passed to the continuation (being generated
-- locally).
--
-- @since 2.0.0
mkUncons :: Exp -> (Name -> Name -> Q Exp) -> Q Exp
mkUncons listE f = do
  hName <- newName "h"
  tName <- newName "t"
  body <- f hName tName
  let innerLam = AppE (VarE 'plam') . LamE [VarP tName] $ body
  let outerLam = AppE (VarE 'plam') . LamE [VarP hName] $ innerLam
  [e|pheadTailList $(pure listE) $(pure outerLam)|]

-- | @since 2.0.0
toSomeTermE :: Q Exp
toSomeTermE = pure $ VarE 'toSomeTerm

-- | @since 2.0.0
punsafeCaseE :: Q Exp
punsafeCaseE = pure $ VarE 'punsafeCase

-- | @since 2.0.0
punsafeConstrE :: Q Exp
punsafeConstrE = pure $ VarE 'punsafeConstr

-- | @since 2.0.0
punsafeConstantE :: Q Exp
punsafeConstantE = pure $ VarE 'punsafeConstant

-- | @since 2.0.0
pequalsIntegerE :: Q Exp
pequalsIntegerE = pure $ VarE 'pequalsInteger

-- | @since 2.0.0
plam'E :: Q Exp
plam'E = pure $ VarE 'plam'

-- | @since 2.0.0
punsafeCoerceE :: Q Exp
punsafeCoerceE = pure $ VarE 'punsafeCoerce

-- | @since 2.0.0
punConstrDataE :: Q Exp
punConstrDataE = pure $ VarE 'punConstrData

-- | @since 2.0.0
punListDataE :: Q Exp
punListDataE = pure $ VarE 'punListData

-- | @since 2.0.0
pnilDataE :: Q Exp
pnilDataE = pure $ VarE 'pnilData

-- | @since 2.0.0
pmkConsE :: Q Exp
pmkConsE = pure $ VarE 'pmkCons

-- | @since 2.0.0
pconstrDataE :: Q Exp
pconstrDataE = pure $ VarE 'pconstrData

-- | @since 2.0.0
pequalsDataE :: Q Exp
pequalsDataE = pure $ VarE 'pequalsData

-- | @since 2.0.0
punIDataE :: Q Exp
punIDataE = pure $ VarE 'punIData

-- | @since 2.0.0
pheadTailListE :: Q Exp
pheadTailListE = pure $ VarE 'pheadTailList

-- | Given a \'handler\' taking the named argument, constructs a new @plam'@
-- with a fresh argument.
--
-- @since 2.0.0
mkPLam :: (Name -> Q Exp) -> Q Exp
mkPLam f = do
  argName <- newName "x"
  body <- f argName
  pure . AppE (VarE 'plam') . LamE [VarP argName] $ body

-- | As 'mkPLam', but does not name a fresh argument.
--
-- @since 2.0.0
mkAnonPLam :: Q Exp -> Q Exp
mkAnonPLam f = AppE (VarE 'plam') . LamE [WildP] <$> f

-- | A \'classifier type\' for Plutarch types.
--
-- @since 2.0.0
data PType
  = -- | 'PAsData' wrapped.
    PTypeData Type
  | -- | A function type.
    PTypeFunction Type
  | -- | Something else.
    PTypeNotData Type

-- | A sum of 'PType' products.
--
-- @since 2.0.0
newtype PTypeSum = PTypeSum (NonEmptyVector (Name, PTypeProduct))

-- | A 'PType' product.
--
-- @since 2.0.0
newtype PTypeProduct = PTypeProduct (Vector PType)

-- | Given a 'Vector' of constructors, produce the corresponding 'PTypeSum'.
-- Errors if given an empty 'Vector'.
--
-- @since 2.0.0
consToPTypeSum :: Vector Con -> Q PTypeSum
consToPTypeSum v = case Vector.uncons v of
  Nothing -> fail "Cannot derive for nullary types."
  Just (c, cs) -> PTypeSum <$> traverse conToPTypeProduct (NEVector.consV c cs)

-- | Produce the \'inner type\' (removing the 'PAsData' wrapper) for a 'PType'
-- that represents @'PAsData' t@ for some @t@, erroring otherwise.
--
-- @since 2.0.0
unwrapField :: PType -> Q Type
unwrapField = \case
  PTypeData t -> pure t
  _ -> fail "Cannot use this derivation if a field is not wrapped in 'PAsData'."

-- | Verify whether a given 'PType' is recursive with the given 'Name' or not.
--
-- @since 2.0.0
isTypeRecursive :: Name -> PType -> Bool
isTypeRecursive name = \case
  PTypeData t -> go t
  PTypeNotData t -> go t
  PTypeFunction t -> go t
  where
    go :: Type -> Bool
    go = \case
      ConT name' -> name' == name
      AppT x y -> go x || go y
      InfixT t name' u -> name' == name || go t || go u
      _ -> False

-- | Check whether the given 'PType' is either recursive or a function type. If the
-- first argument is 'True' (indicating the intent to derive 'PEq' or 'PDebug'),
-- if the given 'PType' is a function type, error. Otherwise, produce:
--
-- * An indication of the type's recursivity given the 'Name' argument ('True'
-- means \'recursive\'); and
-- * The 'Type' as it would be originally (restoring 'PAsData' if it was there).
--
-- @since 2.0.0
checkAndMark :: Bool -> Name -> PType -> Q (Bool, Type)
checkAndMark deriveEqDebug name t = case t of
  PTypeFunction t' ->
    if deriveEqDebug
      then fail "Functions cannot have derivations for PEq."
      -- Function types are never recursive
      else pure (False, t')
  PTypeData t' -> pure (isTypeRecursive name t, AppT (ConT ''PAsData) t')
  PTypeNotData t' -> pure (isTypeRecursive name t, t')

-- | @since 2.0.0
terminateList :: Exp -> Name -> Q Exp
terminateList ellE contName =
  [e|
    pmatch $(pure ellE) $ \case
      PBNil -> $(pure . VarE $ contName)
      PBCons _ _ -> perror
    |]

-- Helpers

conToPTypeProduct :: Con -> Q (Name, PTypeProduct)
conToPTypeProduct = \case
  NormalC name fields -> addName name <$> (traverse (go name) . Vector.fromList $ fields)
  RecC name fields -> addName name <$> (traverse (goNamed name) . Vector.fromList $ fields)
  InfixC lhs name rhs -> do
    let pieces = Vector.fromListN 2 [lhs, rhs]
    addName name <$> traverse (go name) pieces
  ForallC {} -> fail "Derivation does not work on nested foralls."
  GadtC {} -> fail "Derivation does not work on GADTs."
  RecGadtC {} -> fail "Derivation does not work on GADTs."
  where
    addName :: Name -> Vector PType -> (Name, PTypeProduct)
    addName name p = (name, PTypeProduct p)
    go :: Name -> (Bang, Type) -> Q PType
    go conName (_, t) =
      let errMsg = "Constructor " <> show conName <> " has a field whose type is not wrapped in 'Term'."
       in dig errMsg t
    goNamed :: Name -> (Name, Bang, Type) -> Q PType
    goNamed conName (fieldName, _, t) =
      let errMsg = "Constructor " <> show conName <> " has a field " <> show fieldName <> " whose type is not wrapped in 'Term'."
       in dig errMsg t
    dig :: String -> Type -> Q PType
    dig errMsg = \case
      AppT (AppT (ConT n) _) x -> do
        unless (n == ''Term) (fail errMsg)
        classifyType x
      _ -> fail errMsg

classifyType :: Type -> Q PType
classifyType t = case t of
  AppT (ConT t') x ->
    if t' == ''PAsData
      then
        if isTypePFunction x
          then fail "PAsData wrapping a Plutarch function makes no sense."
          else pure $ PTypeData x
      else pure $ PTypeNotData t
  _ ->
    pure $
      if isTypePFunction t
        then PTypeFunction t
        else PTypeNotData t

isTypePFunction :: Type -> Bool
isTypePFunction = \case
  InfixT _ name _ -> name == ''(:-->)
  _ -> False
