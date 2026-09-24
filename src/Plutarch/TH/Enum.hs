{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.Enum
  ( deriveEnum,
  )
where

import Data.Foldable (foldrM)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH
  ( BndrVis,
    Body (NormalB),
    Dec,
    Exp (CaseE, ConE, LitE, VarE),
    Lit (IntegerL, StringL),
    Match (Match),
    Name,
    Pat (ConP),
    Q,
    TyVarBndr,
    Type,
    nameBase,
  )
import Plutarch.Helpers.TH
  ( PTypeProduct (PTypeProduct),
    PTypeSum (PTypeSum),
    fullTypeName,
    pequalsIntegerE,
    plam'E,
    punsafeCaseE,
    punsafeConstantE,
    toSomeTermE,
  )
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))
import Plutarch.Primitive.Singular (PInteger, PString)
import PlutusCore qualified as PLC

-- | @since 2.0.0
deriveEnum ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  PTypeSum ->
  Bool ->
  Q [Dec]
deriveEnum tvbs name (PTypeSum typeStructure) deriveEqDebug = do
  skeleton <- traverse asEmptyProduct typeStructure
  plutarchTypeDec <- derivePlutarchType tvbs name
  pmatchDec <- derivePMatch tvbs name skeleton
  pconDec <- derivePCon tvbs name skeleton
  peqDec <- if deriveEqDebug then derivePEq tvbs name else pure []
  pdebugDec <- if deriveEqDebug then derivePDebug tvbs name skeleton else pure []
  pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec <> pdebugDec

-- Helpers

asEmptyProduct :: (Name, PTypeProduct) -> Q Name
asEmptyProduct (name, PTypeProduct fields) = case Vector.uncons fields of
  Nothing -> pure name
  Just (_, _) -> fail "Cannot derive using Enum strategy for types with fields in any 'arm'."

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance PlutarchType $name where
      type PRepresentation $name = PInteger
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Name -> Q [Dec]
derivePMatch tyVars tyName constructorNames =
  [d|
    instance PMatch $name where
      pmatch' x f = $punsafeCaseE x $(mkHandlers 'f)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    mkHandlers :: Name -> Q Exp
    mkHandlers contName = do
      let (namesRest, nameLast) = NEVector.unsnoc constructorNames
      handlerLast <- mkHandler contName nameLast
      handlersRest <- traverse (mkHandler contName) namesRest
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> Name -> Q Exp
    mkHandler contName conName = [e|$(pure (VarE contName)) $(pure (ConE conName))|]

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Name -> Q [Dec]
derivePCon tyVars tyName constructorNames =
  [d|
    instance PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    matches :: Name -> Q Exp
    matches bindName = do
      matchVec <- NEVector.imapM mkMatch constructorNames
      pure . CaseE (VarE bindName) . NEVector.toList $ matchVec
    mkMatch :: Int -> Name -> Q Match
    mkMatch conIx conName = do
      let conMatchPat = ConP conName [] []
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrE <- [e|$punsafeConstantE (PLC.someValue @Integer $(pure constrIx))|]
      pure . Match conMatchPat (NormalB constrE) $ []

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance PEq $name where
      peq = $plam'E $ \x -> $plam'E $ \y -> $pequalsIntegerE # pfundamental x # pfundamental y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars

derivePDebug :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Name -> Q [Dec]
derivePDebug tyVars tyName constructorNames =
  [d|
    instance PDebug $name where
      pdebug t f = f (pmatch t $ \x -> $(matches 'x))
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    matches :: Name -> Q Exp
    matches bindName = do
      matchVec <- NEVector.mapM mkMatch constructorNames
      pure . CaseE (VarE bindName) . NEVector.toList $ matchVec
    mkMatch :: Name -> Q Match
    mkMatch conName = do
      let conMatchPat = ConP conName [] []
      let conNameLit = LitE . StringL . nameBase $ conName
      constantE <- [e|pconstant @PString $(pure conNameLit)|]
      pure . Match conMatchPat (NormalB constantE) $ []
