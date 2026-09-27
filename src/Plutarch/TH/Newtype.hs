{-# LANGUAGE TemplateHaskellQuotes #-}

module Plutarch.TH.Newtype (deriveNewtype) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH
  ( BndrVis,
    Body (NormalB),
    Dec,
    Exp (AppE, AppTypeE, CaseE, ConE, LamE, VarE),
    Match (Match),
    Name,
    Pat (ConP, VarP),
    Q,
    TyVarBndr,
    Type,
    newName,
  )
import Plutarch.Helpers.TH
  ( PTypeProduct (PTypeProduct),
    PTypeSum (PTypeSum),
    checkAndMark,
    fullTypeName,
    mkContextOfExtended,
  )
import Plutarch.Primitive.CanData (PCanData (pfromData, ptoData))
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))

-- | @since 2.0.0
deriveNewtype ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  PTypeSum ->
  Bool ->
  Bool ->
  Q [Dec]
deriveNewtype tvbs name (PTypeSum typeStructure) deriveEqDebug deriveCanData =
  case NEVector.unsnoc typeStructure of
    (_, (conName, PTypeProduct conFields)) -> do
      -- As we know at this point that we have a `newtype`, we can just assume
      -- that there's exactly one field.
      markedFields <- traverse (checkAndMark deriveEqDebug name) conFields
      let soleField = markedFields Vector.! 0
      plutarchTypeDec <- derivePlutarchType tvbs name soleField
      pmatchDec <- derivePMatch tvbs name conName soleField
      pconDec <- derivePCon tvbs name conName soleField
      peqDebugDec <-
        if deriveEqDebug
          then do
            peqDec <- derivePEq tvbs name conName soleField
            pdebugDec <- derivePDebug tvbs name conName soleField
            pure $ peqDec <> pdebugDec
          else pure []
      pcanDataDec <-
        if deriveCanData
          then derivePCanData tvbs name conName soleField
          else pure []
      pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDebugDec <> pcanDataDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> (Bool, Type) -> Q [Dec]
derivePlutarchType tyVars tyName (_, fieldTy) =
  [d|
    instance ($ctx) => PlutarchType $name where
      type PRepresentation $name = PRepresentation $(pure fieldTy)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PlutarchType] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars

derivePMatch ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Name ->
  (Bool, Type) ->
  Q [Dec]
derivePMatch tyVars tyName conName (_, fieldTy) =
  [d|
    instance ($ctx) => PMatch $name where
      pmatch' t f = f ($constr . punsafeSecondary $ t)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PlutarchType] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    constr :: Q Exp
    constr = pure . ConE $ conName

derivePCon ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Name ->
  (Bool, Type) ->
  Q [Dec]
derivePCon tyVars tyName conName (_, fieldTy) =
  [d|
    instance ($ctx) => PCon $name where
      pcon' t = $(mkMatch 't)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PlutarchType] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    mkMatch :: Name -> Q Exp
    mkMatch bindName = do
      fieldName <- newName "f"
      let conMatchPat = ConP conName [] [VarP fieldName]
      matchBody <- [e|pfundamental $(pure . VarE $ fieldName)|]
      pure . CaseE (VarE bindName) $ [Match conMatchPat (NormalB matchBody) []]

derivePEq ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Name ->
  (Bool, Type) ->
  Q [Dec]
derivePEq tyVars tyName conName (_, fieldTy) =
  [d|
    instance ($ctx) => PEq $name where
      peq = plam' $ \x -> plam' $ \y -> $(mkEq 'x 'y)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PEq] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    mkEq :: Name -> Name -> Q Exp
    mkEq xBind yBind = do
      xFieldName <- newName "x"
      yFieldName <- newName "y"
      eqE <- [e|peq # $(pure . VarE $ xFieldName) # $(pure . VarE $ yFieldName)|]
      let yMatchPat = ConP conName [] [VarP yFieldName]
      let yLam = LamE [yMatchPat] eqE
      yE <- [e|pmatch $(pure . VarE $ yBind) $ $(pure yLam)|]
      let xMatchPat = ConP conName [] [VarP xFieldName]
      let xLam = LamE [xMatchPat] yE
      [e|pmatch $(pure . VarE $ xBind) $ $(pure xLam)|]

derivePDebug ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Name ->
  (Bool, Type) ->
  Q [Dec]
derivePDebug tyVars tyName conName (_, fieldTy) =
  [d|
    instance ($ctx) => PDebug $name where
      pdebug t f = pmatch t $ $(mkDebug 'f)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PDebug] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    mkDebug :: Name -> Q Exp
    mkDebug contName = do
      fieldName <- newName "f"
      let conMatchPat = ConP conName [] [VarP fieldName]
      lamBody <- [e|pdebug $(pure . VarE $ fieldName) $(pure . VarE $ contName)|]
      pure . LamE [conMatchPat] $ lamBody

derivePCanData ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Name ->
  (Bool, Type) ->
  Q [Dec]
derivePCanData tyVars tyName conName (_, fieldTy) =
  [d|
    instance ($ctx) => PCanData $name where
      pfromData t = pcon $(mkFrom 't)
      ptoData t = pmatch t $ $(mkTo)
    |]
  where
    ctx :: Q Type
    ctx = pure . mkContextOfExtended [''PCanData] tyVars $ fieldTy
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    mkFrom :: Name -> Q Exp
    mkFrom bindName = do
      pfdE <- [e|pfromData|]
      pcE <- [e|punsafeCoerce|]
      pure . AppE (ConE conName) . AppE (AppTypeE pfdE fieldTy) . AppE pcE . VarE $ bindName
    mkTo :: Q Exp
    mkTo = do
      fieldName <- newName "f"
      let conMatchPat = ConP conName [] [VarP fieldName]
      lamBody <- [e|punsafeCoerce . ptoData $ $(pure . VarE $ fieldName)|]
      pure . LamE [conMatchPat] $ lamBody
