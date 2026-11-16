{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 2.0.0
module Plutarch.LedgerApi.Tx
  ( PTxId (..),
    PTxOutRef (..),
    PDatum (..),
    PDatumHash (..),
    POutputDatum (..),
    PTxOut (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.Address (PAddress)
import Plutarch.LedgerApi.Credential (PScriptHash)
import Plutarch.LedgerApi.MaybeData (PMaybeData)
import Plutarch.Primitive.BuiltinFun
  ( pappendString,
    plengthOfByteString,
    plistData,
    punListData,
  )
import Plutarch.Primitive.CanData (PCanData, pfromData)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    ReprError (UserSpecified),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( pfundamental,
    punsafeSecondary,
    (#),
  )
import Plutarch.Primitive.Singular
  ( PAsData,
    PBValue,
    PByteString,
    PData,
    PInteger,
  )
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
    pvalidateData,
  )
import Plutarch.TH.Strategy
  ( Strategy (DataList, DataPlutus, Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData
import PlutusTx.Prelude qualified as PTx

-- | @since 2.0.0
newtype PTxId = PTxId (Term PByteString)

deriveFor ''PTxId Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.TxId where
  type AsPlutus PLA.TxId = ByteString
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinByteString . coerce
  reprToHask bs = case BS.length bs of
    32 -> Right . PLA.TxId . PTx.toBuiltin $ bs
    n -> Left . UserSpecified $ "Invalid length for TxId: " <> show n

-- | @since 2.0.0
instance PLiftable PTxId where
  type AsHaskell PTxId = PLA.TxId
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = pcon . PTxId . reprToPlut

-- | @since 2.0.0
instance PCanData PTxId

-- | Verifies that we have a @Data@-encoded 'PByteString' whose length is
-- exactly 32.
--
-- @since 2.0.0
instance PValidateData PTxId where
  pwithValidated d res =
    let bs = pfromData (pvalidateData d)
     in pif
          ((plengthOfByteString # bs) #== pconstant 32)
          res
          perror

-- | @since 2.0.0
data PTxOutRef = PTxOutRef
  { ptxOutRefId :: Term (PAsData PTxId),
    ptxOutRefIdx :: Term (PAsData PInteger)
  }

deriveFor ''PTxOutRef DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.TxOutRef where
  type AsPlutus PLA.TxOutRef = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode TxOutRef from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PTxOutRef where
  type AsHaskell PTxOutRef = PLA.TxOutRef
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)

-- | @since 2.0.0
newtype PDatum = PDatum (Term PData)

deriveFor ''PDatum Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.Datum where
  type AsPlutus PLA.Datum = PLA.Data
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinData . coerce
  reprToHask = Right . PLA.Datum . PTx.toBuiltin

-- | @since 2.0.0
instance PLiftable PDatum where
  type AsHaskell PDatum = PLA.Datum
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = pcon . PDatum . reprToPlut

-- | @since 2.0.0
instance PCanData PDatum

-- | Essentially performs no validation: nothing can be said about a 'PDatum' in
-- general.
--
-- @since 2.0.0
instance PValidateData PDatum where
  pwithValidated = pwithValidated @PData

-- | @since 2.0.0
newtype PDatumHash = PDatumHash (Term PByteString)

deriveFor ''PDatumHash Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.DatumHash where
  type AsPlutus PLA.DatumHash = ByteString
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinByteString . coerce
  reprToHask bs = case BS.length bs of
    32 -> Right . PLA.DatumHash . PTx.toBuiltin $ bs
    n -> Left . UserSpecified $ "Invalid length for DatumHash: " <> show n

-- | @since 2.0.0
instance PLiftable PDatumHash where
  type AsHaskell PDatumHash = PLA.DatumHash
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = pcon . PDatumHash . reprToPlut

-- | @since 2.0.0
instance PCanData PDatumHash

-- | Verifies that we have a @Data@-encoded 'PByteString' whose length is
-- exactly 32.
--
-- @since 2.0.0
instance PValidateData PDatumHash where
  pwithValidated d res =
    let bs = pfromData (pvalidateData d)
     in pif
          ((plengthOfByteString # bs) #== pconstant 32)
          res
          perror

-- | @since 2.0.0
data POutputDatum
  = PNoOutputDatum
  | POutputDatumHash (Term (PAsData PDatumHash))
  | POutputDatum (Term (PAsData PDatum))

deriveFor ''POutputDatum DataPlutus AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.OutputDatum where
  type AsPlutus PLA.OutputDatum = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode OutputDatum from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable POutputDatum where
  type AsHaskell POutputDatum = PLA.OutputDatum
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant

-- | @since 2.0.0
data PTxOut = PTxOut
  { ptxOutAddress :: Term (PAsData PAddress),
    ptxOutValue :: Term (PAsData PBValue),
    ptxOutDatum :: Term (PAsData POutputDatum),
    ptxOutReferenceScript :: Term (PAsData (PMaybeData PScriptHash))
  }

deriveFor ''PTxOut DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.TxOut where
  type AsPlutus PLA.TxOut = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode TxOut from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PTxOut where
  type AsHaskell PTxOut = PLA.TxOut
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)
