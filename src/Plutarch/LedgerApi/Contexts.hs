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
    PCommittee (..),
    PChangedParameters (..),
    PGovernanceAction (..),
    PProposalProcedure (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.AssocMap (PMapData)
import Plutarch.LedgerApi.Credential (PCredential, PScriptHash)
import Plutarch.LedgerApi.MaybeData (PMaybeData)
import Plutarch.LedgerApi.Ratio (PRationalData)
import Plutarch.LedgerApi.Tx (PTxId)
import Plutarch.LedgerApi.Value (PLovelace)
import Plutarch.Primitive.BuiltinFun (pappendString)
import Plutarch.Primitive.CanData (PCanData (pfromData))
import Plutarch.Primitive.Composite (PBList)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PLiftableData (PLiftableData),
    PLiftableDataList (PLiftableDataList),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    PlutusRepresentableData (PlutusRepresentableData),
    ReprError (UserSpecified),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation (pfundamental, (#))
import Plutarch.Primitive.Singular (PAsData, PData, PInteger)
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))
import Plutarch.TH.Strategy
  ( Strategy (DataList, DataPlutus, Newtype),
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
deriving via
  (PlutusRepresentableData PLA.GovernanceActionId)
  instance
    PlutusRepresentable PLA.GovernanceActionId

-- | @since 2.0.0
deriving via
  (PLiftableDataList PGovernanceActionId PLA.GovernanceActionId)
  instance
    PLiftable PGovernanceActionId

-- | @since 2.0.0
data PProtocolVersion = PProtocolVersion
  { ppvMajor :: Term (PAsData PInteger),
    ppvMinor :: Term (PAsData PInteger)
  }

deriveFor ''PProtocolVersion DataList AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.ProtocolVersion)
  instance
    PlutusRepresentable PLA.ProtocolVersion

-- | @since 2.0.0
deriving via
  (PLiftableDataList PProtocolVersion PLA.ProtocolVersion)
  instance
    PLiftable PProtocolVersion

-- | @since 2.0.0
newtype PConstitution = PConstitution (Term (PAsData (PMaybeData PScriptHash)))

deriveFor ''PConstitution DataList AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.Constitution)
  instance
    PlutusRepresentable PLA.Constitution

-- | @since 2.0.0
deriving via
  (PLiftableDataList PConstitution PLA.Constitution)
  instance
    PLiftable PConstitution

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

-- | @since 2.0.0
data PCommittee = PCommittee (Term (PAsData (PMapData PColdCommitteeCredential PInteger))) (Term (PAsData PRationalData))

deriveFor ''PCommittee DataList AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.Committee)
  instance
    PlutusRepresentable PLA.Committee

-- | @since 2.0.0
deriving via
  (PLiftableDataList PCommittee PLA.Committee)
  instance
    PLiftable PCommittee

-- | @since 2.0.0
newtype PChangedParameters = PChangedParameters (Term PData)

deriveFor ''PChangedParameters Newtype AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.ChangedParameters)
  instance
    PlutusRepresentable PLA.ChangedParameters

-- | @since 2.0.0
instance PLiftable PChangedParameters where
  type AsHaskell PChangedParameters = AsHaskell PData
  plutToRepr t = plutToRepr (pmatch t $ \(PChangedParameters t') -> t')
  reprToPlut t = pcon . PChangedParameters $ reprToPlut t

-- | @since 2.0.0
instance PCanData PChangedParameters

-- | Essentially does nothing.
--
-- @since 2.0.0
instance PValidateData PChangedParameters where
  pwithValidated = pwithValidated @PData

-- | @since 2.0.0
data PGovernanceAction
  = PParameterChange (Term (PAsData (PMaybeData PGovernanceActionId))) (Term (PAsData PChangedParameters)) (Term (PAsData (PMaybeData PScriptHash)))
  | PHardForkInitiation (Term (PAsData (PMaybeData PGovernanceActionId))) (Term (PAsData PProtocolVersion))
  | PTreasuryWithdrawals (Term (PAsData (PMapData PCredential PLovelace))) (Term (PAsData (PMaybeData PScriptHash)))
  | PNoConfidence (Term (PAsData (PMaybeData PGovernanceActionId)))
  | PUpdateCommittee
      (Term (PAsData (PMaybeData PGovernanceActionId)))
      (Term (PAsData (PBList (PAsData PColdCommitteeCredential))))
      (Term (PAsData (PMapData PColdCommitteeCredential PInteger)))
      (Term (PAsData PRationalData))
  | PNewConstitution (Term (PAsData (PMaybeData PGovernanceActionId))) (Term (PAsData PConstitution))
  | PInfoAction

deriveFor ''PGovernanceAction DataPlutus AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.GovernanceAction)
  instance
    PlutusRepresentable PLA.GovernanceAction

-- | @since 2.0.0
deriving via
  (PLiftableData PGovernanceAction PLA.GovernanceAction)
  instance
    PLiftable PGovernanceAction

-- | @since 2.0.0
data PProposalProcedure
  = PProposalProcedure
      (Term (PAsData PLovelace))
      (Term (PAsData PCredential))
      (Term (PAsData PGovernanceAction))

deriveFor ''PProposalProcedure DataList AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.ProposalProcedure)
  instance
    PlutusRepresentable PLA.ProposalProcedure

-- | @since 2.0.0
deriving via
  (PLiftableDataList PProposalProcedure PLA.ProposalProcedure)
  instance
    PLiftable PProposalProcedure
