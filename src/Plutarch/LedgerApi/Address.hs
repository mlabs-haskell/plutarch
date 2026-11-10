{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Address
  ( PAccountId (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.Credential (PCredential)
import Plutarch.Primitive.CanData (PCanData)
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
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
  )
import Plutarch.TH.Strategy
  ( Strategy (Newtype),
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
instance PLiftable PAccountId where
  type AsHaskell PAccountId = PLA.AccountId
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant

-- | @since 2.0.0
instance PCanData PAccountId

-- | @since 2.0.0
instance PValidateData PAccountId where
  pwithValidated = pwithValidated @PCredential
