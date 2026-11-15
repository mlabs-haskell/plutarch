{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.Ledger
  ( deriveAsEquivalent,
  )
where

import Data.Foldable (foldl')
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH
  ( BndrVis,
    Dec (DataD),
    Exp,
    Name,
    Q,
    TyVarBndr,
    Type (AppT, ConT, TupleT, VarT),
  )
import Plutarch.Helpers.TH
  ( bindToName,
    checkTyName,
    fullTypeName,
    mkContextOf,
  )
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData

-- | @since 2.0.0
deriveAsEquivalent :: Name -> Name -> Q [Dec]
deriveAsEquivalent plutTyName plinthTyName = do
  tyDecPlut <- checkTyName plutTyName
  tyDecPlinth <- checkTyName plinthTyName
  case tyDecPlut of
    DataD _ namePlut tyVarBindsPlut _ _ _ -> case tyDecPlinth of
      DataD _ namePlinth tyVarBindsPlinth _ _ _ -> do
        let tvbPlinthVec = Vector.fromList tyVarBindsPlinth
        let tvbPlutVec = Vector.fromList tyVarBindsPlut
        plutusRepresentableDec <- derivePlutusRepresentable namePlinth tvbPlinthVec
        pliftableDec <- derivePLiftable namePlut tvbPlutVec namePlinth tvbPlinthVec
        pure $ plutusRepresentableDec <> pliftableDec
      _ -> fail "Only supports types defined using `data`."
    _ -> fail "Only supports types defined using `data`."

derivePlutusRepresentable :: Name -> Vector (TyVarBndr BndrVis) -> Q [Dec]
derivePlutusRepresentable tyNamePlinth tvbsPlinth =
  [d|
    instance ($ctx) => PlutusRepresentable $name where
      type AsPlutus $name = PLA.Data
      haskToRepr = PTxData.toData
      reprToHask d = case PTxData.fromData d of
        Nothing -> Left . UserSpecified $ $errStr
        Just x -> pure x
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyNamePlinth $ tvbsPlinth
    ctx :: Q Type
    ctx = do
      let tvbCtxes = mkContextOf [''PTxData.ToData, ''PTxData.FromData] tvbsPlinth
      let fullName = fullTypeName tyNamePlinth tvbsPlinth
      let toDataCtx = AppT (ConT ''PTxData.ToData) fullName
      let fromDataCtx = AppT (ConT ''PTxData.FromData) fullName
      pure . foldl' AppT (TupleT 3) $ [tvbCtxes, toDataCtx, fromDataCtx]
    errStr :: Q Exp
    errStr = [e|"Could not decode " <> show tyNamePlinth <> "from Data."|]

derivePLiftable ::
  Name ->
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Vector (TyVarBndr BndrVis) ->
  Q [Dec]
derivePLiftable tyNamePlut tvbsPlut tyNamePlinth tvbsPlinth =
  [d|
    instance ($ctx) => PLiftable $plutName where
      type AsHaskell $plutName = $plinthName
      plutToRepr t = plutToRepr (pfundamental t)
      reprToPlut = punsafeSecondary . pconstant
    |]
  where
    plutName :: Q Type
    plutName = pure . fullTypeName tyNamePlut $ tvbsPlut
    plinthName :: Q Type
    plinthName = pure . fullTypeName tyNamePlinth $ tvbsPlinth
    ctx :: Q Type
    ctx = do
      let tvbCtxes = mkContextOf [''PLiftable, ''PCanData] tvbsPlut
      let fullName = fullTypeName tyNamePlinth tvbsPlinth
      let toDataCtx = AppT (ConT ''PTxData.ToData) fullName
      let fromDataCtx = AppT (ConT ''PTxData.FromData) fullName
      let plinthCtxes = mkContextOfHaskell [''PTxData.ToData, ''PTxData.FromData] tvbsPlinth
      pure . foldl' AppT (TupleT 4) $ [tvbCtxes, toDataCtx, fromDataCtx, plinthCtxes]

-- Helpers

mkContextOfHaskell :: [Name] -> Vector (TyVarBndr BndrVis) -> Type
mkContextOfHaskell tyClassNames tyVars =
  let varLen = Vector.length tyVars
      classesLen = length tyClassNames
      varNames =
        [ AppT (ConT tcn) . AppT (ConT ''AsHaskell) . VarT . bindToName $ bn
        | tcn <- tyClassNames,
          bn <- Vector.toList tyVars
        ]
   in foldl' AppT (TupleT (varLen * classesLen)) varNames
