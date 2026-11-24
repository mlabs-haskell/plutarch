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
    PAccountBalanceInterval (..),
    PAccountBalanceIntervals (..),
    PHotCommitteeCredential (..),
    PDRepCredential (..),
    PDRep (..),
    PDelegatee (..),
    PTxCert (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.Address (PAccountId)
import Plutarch.LedgerApi.AssocMap (PMapData)
import Plutarch.LedgerApi.Credential (PCredential, PPubKeyHash, PScriptHash)
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
  ( Strategy (DataList, DataPlutus, DataPlutusCompact, Newtype),
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

-- | @since 2.0.0
data PAccountBalanceInterval
  = PAccountBalanceLowerBound (Term (PAsData PLovelace))
  | PAccountBalanceUpperBound (Term (PAsData PLovelace))
  | PAccountBalanceBothBounds (Term (PAsData PLovelace)) (Term (PAsData PLovelace))
  | PAccountBalanceExact (Term (PAsData PLovelace))

deriveFor ''PAccountBalanceInterval DataPlutusCompact AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.AccountBalanceInterval)
  instance
    PlutusRepresentable PLA.AccountBalanceInterval

-- | @since 2.0.0
deriving via
  (PLiftableData PAccountBalanceInterval PLA.AccountBalanceInterval)
  instance
    PLiftable PAccountBalanceInterval

-- | @since 2.0.0
newtype PAccountBalanceIntervals
  = PAccountBalanceIntervals (Term (PMapData PAccountId PAccountBalanceInterval))

deriveFor ''PAccountBalanceIntervals Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.AccountBalanceIntervals where
  type AsPlutus PLA.AccountBalanceIntervals = AsPlutus (PLA.Map PLA.AccountId PLA.AccountBalanceInterval)
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode AccountBalanceIntervals from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PAccountBalanceIntervals where
  type AsHaskell PAccountBalanceIntervals = PLA.AccountBalanceIntervals
  plutToRepr t = plutToRepr (pmatch t $ \(PAccountBalanceIntervals t') -> t')
  reprToPlut t = pcon . PAccountBalanceIntervals $ reprToPlut t

-- | @since 2.0.0
instance PCanData PAccountBalanceIntervals

-- | @since 2.0.0
instance PValidateData PAccountBalanceIntervals where
  pwithValidated = pwithValidated @(PMapData PAccountId PAccountBalanceInterval)

-- | @since 2.0.0
newtype PHotCommitteeCredential = PHotCommitteeCredential (Term PCredential)

deriveFor ''PHotCommitteeCredential Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.HotCommitteeCredential where
  type AsPlutus PLA.HotCommitteeCredential = AsPlutus PLA.Credential
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode HotCommitteeCredential from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PHotCommitteeCredential where
  type AsHaskell PHotCommitteeCredential = AsHaskell PCredential
  plutToRepr t = plutToRepr (pmatch t $ \(PHotCommitteeCredential t') -> t')
  reprToPlut t = pcon . PHotCommitteeCredential $ reprToPlut t

-- | @since 2.0.0
instance PCanData PHotCommitteeCredential

-- | @since 2.0.0
instance PValidateData PHotCommitteeCredential where
  pwithValidated = pwithValidated @PCredential

-- | @since 2.0.0
newtype PDRepCredential = PDRepCredential (Term PCredential)

deriveFor ''PDRepCredential Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.DRepCredential where
  type AsPlutus PLA.DRepCredential = AsPlutus PLA.Credential
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode DRepCredential from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PDRepCredential where
  type AsHaskell PDRepCredential = AsHaskell PCredential
  plutToRepr t = plutToRepr (pmatch t $ \(PDRepCredential t') -> t')
  reprToPlut t = pcon . PDRepCredential $ reprToPlut t

-- | @since 2.0.0
instance PCanData PDRepCredential

-- | @since 2.0.0
instance PValidateData PDRepCredential where
  pwithValidated = pwithValidated @PCredential

-- | @since 2.0.0
data PDRep
  = PDRep (Term (PAsData PDRepCredential))
  | PDRepAlwaysAbstain
  | PDRepAlwaysNoConfidence

deriveFor ''PDRep DataPlutus AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.DRep)
  instance
    PlutusRepresentable PLA.DRep

-- | @since 2.0.0
deriving via
  (PLiftableData PDRep PLA.DRep)
  instance
    PLiftable PDRep

-- | @since 2.0.0
data PDelegatee
  = PDelegStake (Term (PAsData PPubKeyHash))
  | PDelegVote (Term (PAsData PDRep))
  | PDelegStakeVote (Term (PAsData PPubKeyHash)) (Term (PAsData PDRep))

deriveFor ''PDelegatee DataPlutusCompact AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.Delegatee)
  instance
    PlutusRepresentable PLA.Delegatee

-- | @since 2.0.0
deriving via
  (PLiftableData PDelegatee PLA.Delegatee)
  instance
    PLiftable PDelegatee

-- | @since 2.0.0
data PTxCert
  = PTxCertRegAccount (Term (PAsData PAccountId)) (Term (PAsData PLovelace))
  | PTxCertUnRegAccount (Term (PAsData PAccountId)) (Term (PAsData PLovelace))
  | PTxCertDelegAccount (Term (PAsData PAccountId)) (Term (PAsData PDelegatee))
  | PTxCertRegAccountDeleg (Term (PAsData PAccountId)) (Term (PAsData PDelegatee)) (Term (PAsData PLovelace))
  | PTxCertRegDRep (Term (PAsData PDRepCredential)) (Term (PAsData PLovelace))
  | PTxCertUpdateDRep (Term (PAsData PDRepCredential))
  | PTxCertUnRegDRep (Term (PAsData PDRepCredential)) (Term (PAsData PLovelace))
  | PTxCertPoolRegister (Term (PAsData PPubKeyHash)) (Term (PAsData PPubKeyHash))
  | PTxCertPoolRetire (Term (PAsData PPubKeyHash)) (Term (PAsData PInteger))
  | PTxCertAuthHotCommittee (Term (PAsData PColdCommitteeCredential)) (Term (PAsData PHotCommitteeCredential))
  | PTxCertResignColdCommittee (Term (PAsData PColdCommitteeCredential))

deriveFor ''PTxCert DataPlutusCompact AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.TxCert)
  instance
    PlutusRepresentable PLA.TxCert

-- | @since 2.0.0
deriving via
  (PLiftableData PTxCert PLA.TxCert)
  instance
    PLiftable PTxCert
