{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 2.0.0
module Plutarch.LedgerApi.Contexts
  ( PGovernanceActionId (..),
    PProtocolVersion (..),
    PConstitution (..),
    PColdCommitteeCredential (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.Credential (PCredential, PScriptHash)
import Plutarch.LedgerApi.MaybeData (PMaybeData)
import Plutarch.LedgerApi.Tx (PTxId)
import Plutarch.Primitive.BuiltinFun
  ( pappendString,
    plistData,
    punListData,
  )
import Plutarch.Primitive.CanData (PCanData (pfromData))
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
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
    PData,
    PInteger,
  )
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))
import Plutarch.TH.Strategy
  ( Strategy (DataList, Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData

-- | @since 2.0.0
data PGovernanceActionId = PGovernanceActionId
  { pgaidTxId :: Term (PAsData PTxId),
    pgaidGovActionIx :: Term (PAsData PInteger)
  }

deriveFor ''PGovernanceActionId DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.GovernanceActionId where
  type AsPlutus PLA.GovernanceActionId = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode GovernanceActionId from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PGovernanceActionId where
  type AsHaskell PGovernanceActionId = PLA.GovernanceActionId
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)

-- | @since 2.0.0
data PProtocolVersion = PProtocolVersion
  { ppvMajor :: Term (PAsData PInteger),
    ppvMinor :: Term (PAsData PInteger)
  }

deriveFor ''PProtocolVersion DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.ProtocolVersion where
  type AsPlutus PLA.ProtocolVersion = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode ProtocolVersion from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PProtocolVersion where
  type AsHaskell PProtocolVersion = PLA.ProtocolVersion
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)

-- | @since 2.0.0
newtype PConstitution = PConstitution (Term (PAsData (PMaybeData PScriptHash)))

deriveFor ''PConstitution DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.Constitution where
  type AsPlutus PLA.Constitution = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode Constitution from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PConstitution where
  type AsHaskell PConstitution = PLA.Constitution
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)

-- | @since 2.0.0
newtype PColdCommitteeCredential = PColdCommitteeCredential (Term PCredential)

deriveFor ''PColdCommitteeCredential Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.ColdCommitteeCredential where
  type AsPlutus PLA.ColdCommitteeCredential = AsPlutus PLA.Credential
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode ColdCommitteeCredential from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PColdCommitteeCredential where
  type AsHaskell PColdCommitteeCredential = AsHaskell PCredential
  plutToRepr t = plutToRepr (pmatch t $ \(PColdCommitteeCredential t') -> t')
  reprToPlut t = pcon . PColdCommitteeCredential $ reprToPlut t

-- | @since 2.0.0
instance PCanData PColdCommitteeCredential

-- | @since 2.0.0
instance PValidateData PColdCommitteeCredential where
  pwithValidated = pwithValidated @PCredential
