{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Address
  ( PAccountId (..),
    PAddress (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.Credential (PCredential)
import Plutarch.LedgerApi.MaybeData (PMaybeData)
import Plutarch.Primitive.BuiltinFun (pappendString)
import Plutarch.Primitive.CanData (PCanData (pfromData))
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.Liftable
  ( PLiftable,
    PLiftableData (PLiftableData),
    PLiftableDataList (PLiftableDataList),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    PlutusRepresentableData (PlutusRepresentableData),
    ReprError (UserSpecified),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( pfundamental,
    (#),
  )
import Plutarch.Primitive.Singular (PAsData)
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
  )
import Plutarch.TH.Strategy
  ( Strategy (DataList, Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData

-- | @since 2.0.0
newtype PAccountId = PAccountId (Term PCredential)

deriveFor ''PAccountId Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.AccountId where
  type AsPlutus PLA.AccountId = AsPlutus PLA.Credential
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode AccountId from Data"
    Just x -> pure x

-- | @since 2.0.0
deriving via
  PLiftableData PAccountId PLA.AccountId
  instance
    PLiftable PAccountId

-- | @since 2.0.0
instance PCanData PAccountId

-- | @since 2.0.0
instance PValidateData PAccountId where
  pwithValidated = pwithValidated @PCredential

-- | @since 2.0.0
data PAddress = PAddress
  { paddressCredential :: Term (PAsData PCredential),
    paddressStakingAccountId :: Term (PAsData (PMaybeData PAccountId))
  }

deriveFor ''PAddress DataList AllInstances

-- | @since 2.0.0
deriving via
  PlutusRepresentableData PLA.Address
  instance
    PlutusRepresentable PLA.Address

-- | @since 2.0.0
deriving via
  (PLiftableDataList PAddress PLA.Address)
  instance
    PLiftable PAddress
