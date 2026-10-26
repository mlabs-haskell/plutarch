{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphanage () where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import GHC.TypeNats (KnownNat, Nat, natVal)
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusLedgerApi.V4 qualified as V4
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Arbitrary1 (liftArbitrary, liftShrink),
    Gen,
    NonEmptyList (NonEmpty),
    NonNegative (NonNegative),
    NonZero (NonZero),
    Positive (Positive),
    chooseBoundedIntegral,
    chooseInt,
    elements,
    frequency,
    getNonEmpty,
    getNonNegative,
    getNonZero,
    getPositive,
    oneof,
    resize,
    scale,
    sized,
    vectorOf,
  )
import Test.QuickCheck.Instances ()

deriving via Blake2b224Hash instance Arbitrary V4.ScriptHash

deriving via Blake2b224Hash instance Arbitrary V4.PubKeyHash

-- Does not shrink.
instance Arbitrary V4.Credential where
  arbitrary = oneof [V4.PubKeyCredential <$> arbitrary, V4.ScriptCredential <$> arbitrary]

deriving via V4.Credential instance Arbitrary V4.AccountId

deriving via Integer instance Arbitrary V4.Lovelace

instance Arbitrary V4.AccountBalanceInterval where
  arbitrary =
    oneof
      [ V4.AccountBalanceLowerBound <$> arbitrary,
        V4.AccountBalanceUpperBound <$> arbitrary,
        do
          lb <- arbitrary
          delta <- getPositive <$> arbitrary
          pure . V4.AccountBalanceBothBounds lb $ lb + delta,
        V4.AccountBalanceExact <$> arbitrary
      ]
  shrink = \case
    V4.AccountBalanceLowerBound b -> V4.AccountBalanceLowerBound <$> shrink b
    V4.AccountBalanceUpperBound b -> V4.AccountBalanceUpperBound <$> shrink b
    V4.AccountBalanceBothBounds lb ub -> do
      lb' <- shrink lb
      ub' <- shrink ub
      guard (lb' < ub')
      pure . V4.AccountBalanceBothBounds lb' $ ub'
    V4.AccountBalanceExact b -> V4.AccountBalanceExact <$> shrink b

instance (Ord k, Arbitrary k) => Arbitrary1 (V4.Map k) where
  liftArbitrary genVal =
    V4.unsafeFromList <$> do
      keyList <- Set.toList <$> arbitrary
      traverse (\k -> (k,) <$> genVal) keyList
  liftShrink shrinkVal aMap =
    V4.unsafeFromList <$> do
      let asList = AssocMap.toList aMap
      liftShrink (\(k, v) -> (k,) <$> shrinkVal v) asList

instance (Arbitrary v, Arbitrary k, Ord k) => Arbitrary (V4.Map k v) where
  arbitrary = liftArbitrary arbitrary
  shrink = liftShrink shrink

deriving via
  (V4.Map V4.AccountId V4.AccountBalanceInterval)
  instance
    Arbitrary V4.AccountBalanceIntervals

deriving via V4.Credential instance Arbitrary V4.ColdCommitteeCredential

deriving via V4.Credential instance Arbitrary V4.HotCommitteeCredential

deriving via V4.Credential instance Arbitrary V4.DRepCredential

instance Arbitrary V4.DRep where
  arbitrary =
    oneof
      [ V4.DRep <$> arbitrary,
        pure V4.DRepAlwaysAbstain,
        pure V4.DRepAlwaysNoConfidence
      ]

instance Arbitrary V4.Delegatee where
  arbitrary =
    oneof
      [ V4.DelegStake <$> arbitrary,
        V4.DelegVote <$> arbitrary,
        V4.DelegStakeVote <$> arbitrary <*> arbitrary
      ]

instance Arbitrary V4.TxCert where
  arbitrary =
    oneof
      [ V4.TxCertRegAccount <$> arbitrary <*> arbitrary,
        V4.TxCertUnRegAccount <$> arbitrary <*> arbitrary,
        V4.TxCertDelegAccount <$> arbitrary <*> arbitrary,
        V4.TxCertRegAccountDeleg <$> arbitrary <*> arbitrary <*> arbitrary,
        V4.TxCertRegDRep <$> arbitrary <*> arbitrary,
        V4.TxCertUpdateDRep <$> arbitrary,
        V4.TxCertUnRegDRep <$> arbitrary <*> arbitrary,
        V4.TxCertPoolRegister <$> arbitrary <*> arbitrary,
        V4.TxCertPoolRetire <$> arbitrary <*> arbitrary,
        V4.TxCertAuthHotCommittee <$> arbitrary <*> arbitrary,
        V4.TxCertResignColdCommittee <$> arbitrary
      ]
  shrink = \case
    V4.TxCertRegAccount aid ll -> V4.TxCertRegAccount aid <$> shrink ll
    V4.TxCertUnRegAccount aid ll -> V4.TxCertUnRegAccount aid <$> shrink ll
    V4.TxCertDelegAccount _ _ -> [] -- neither field shrinks
    V4.TxCertRegAccountDeleg aid d ll -> V4.TxCertRegAccountDeleg aid d <$> shrink ll
    V4.TxCertRegDRep cred ll -> V4.TxCertRegDRep cred <$> shrink ll
    V4.TxCertUpdateDRep _ -> [] -- credentials don't shrink
    V4.TxCertUnRegDRep cred ll -> V4.TxCertUnRegDRep cred <$> shrink ll
    V4.TxCertPoolRegister _ _ -> [] -- pubkeyhashes don't shrink
    V4.TxCertPoolRetire pkh i -> V4.TxCertPoolRetire pkh <$> shrink i
    V4.TxCertAuthHotCommittee _ _ -> [] -- credentials don't shrink
    V4.TxCertResignColdCommittee _ -> [] -- credentials don't shrink

instance Arbitrary V4.Voter where
  arbitrary =
    oneof
      [ V4.CommitteeVoter <$> arbitrary,
        V4.DRepVoter <$> arbitrary,
        V4.StakePoolVoter <$> arbitrary
      ]

instance Arbitrary V4.Vote where
  arbitrary = elements [V4.VoteYes, V4.VoteNo, V4.Abstain]

deriving via Blake2b224Hash instance Arbitrary V4.TxId

instance Arbitrary V4.GovernanceActionId where
  arbitrary = V4.GovernanceActionId <$> arbitrary <*> (getNonNegative <$> arbitrary)
  shrink (V4.GovernanceActionId tid ix) = do
    ix' <- getNonNegative <$> shrink (NonNegative ix)
    pure . V4.GovernanceActionId tid $ ix'

instance Arbitrary V4.Committee where
  arbitrary = do
    committee <- liftArbitrary (getPositive <$> arbitrary)
    -- We can't have a quorum of 0
    num <- chooseInt (1, 100)
    let quorum = V4.unsafeRatio (fromIntegral num) 100
    pure . V4.Committee committee $ quorum
  shrink (V4.Committee committee quorum) = do
    committee' <- liftShrink (fmap getPositive . shrink . Positive) committee
    guard (not . AssocMap.null $ committee')
    pure . V4.Committee committee' $ quorum

deriving via (Maybe V4.ScriptHash) instance Arbitrary V4.Constitution

instance Arbitrary V4.ProtocolVersion where
  arbitrary = do
    NonNegative major <- arbitrary
    NonNegative minor <- arbitrary
    pure . V4.ProtocolVersion major $ minor
  shrink (V4.ProtocolVersion major minor) = do
    NonNegative major' <- shrink (NonNegative major)
    NonNegative minor' <- shrink (NonNegative minor)
    pure . V4.ProtocolVersion major' $ minor'

instance Arbitrary PlutusTx.BuiltinByteString where
  arbitrary = PlutusTx.toBuiltin @ByteString <$> arbitrary
  shrink = fmap (PlutusTx.toBuiltin @ByteString) . shrink . PlutusTx.fromBuiltin

instance Arbitrary PlutusTx.BuiltinData where
  arbitrary = sized $ \size -> go size
    where
      scaleDown :: forall (a :: Type). Gen a -> Gen a
      scaleDown = scale (`quot` 4)
      go :: Int -> Gen PlutusTx.BuiltinData
      go size
        | size <= 0 = oneof [genB, genI]
        | otherwise = oneof [genB, genI, genConstr, genList, genMap]
      genB :: Gen PlutusTx.BuiltinData
      genB = Builtins.mkB <$> arbitrary
      genI :: Gen PlutusTx.BuiltinData
      genI = Builtins.mkI <$> arbitrary
      genConstr :: Gen PlutusTx.BuiltinData
      genConstr =
        Builtins.mkConstr . getNonNegative
          <$> arbitrary
          <*> scaleDown (liftArbitrary arbitrary)
      genList :: Gen PlutusTx.BuiltinData
      genList =
        Builtins.mkList <$> scaleDown (liftArbitrary arbitrary)
      genMap :: Gen PlutusTx.BuiltinData
      genMap =
        Builtins.mkMap <$> scaleDown (liftArbitrary ((,) <$> arbitrary <*> arbitrary))
  shrink dat =
    Builtins.matchData
      dat
      shrinkConstr
      shrinkMap
      shrinkList
      (fmap (Builtins.mkI . getNonNegative) . shrink . NonNegative)
      (fmap Builtins.mkB . shrink)
    where
      shrinkConstr :: Integer -> [PlutusTx.BuiltinData] -> [PlutusTx.BuiltinData]
      shrinkConstr ix dats = do
        NonNegative ix' <- shrink (NonNegative ix)
        dats' <- shrink dats
        pure . Builtins.mkConstr ix' $ dats'
      shrinkMap :: [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)] -> [PlutusTx.BuiltinData]
      shrinkMap kvs = Builtins.mkMap <$> shrink kvs
      shrinkList :: [PlutusTx.BuiltinData] -> [PlutusTx.BuiltinData]
      shrinkList ell = Builtins.mkList <$> shrink ell

instance Arbitrary V4.ChangedParameters where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    V4.ChangedParameters . Builtins.mkMap <$> do
      keyList <- liftArbitrary (chooseInt (0, 33))
      let keySet = Set.fromList keyList
      traverse (\k -> (Builtins.mkI . fromIntegral $ k,) <$> arbitrary) . Set.toList $ keySet

instance Arbitrary V4.GovernanceAction where
  arbitrary =
    oneof
      [ V4.ParameterChange <$> arbitrary <*> arbitrary <*> arbitrary,
        V4.HardForkInitiation <$> arbitrary <*> arbitrary,
        V4.TreasuryWithdrawals <$> arbitrary <*> arbitrary,
        V4.NoConfidence <$> arbitrary,
        V4.UpdateCommittee
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> (V4.unsafeRatio . fromIntegral <$> chooseInt (1, 100) <*> pure 100),
        V4.NewConstitution <$> arbitrary <*> arbitrary,
        pure V4.InfoAction
      ]
  shrink = \case
    V4.ParameterChange mgid cp msh -> do
      mgid' <- shrink mgid
      msh' <- shrink msh
      pure . V4.ParameterChange mgid' cp $ msh'
    V4.HardForkInitiation mgid v ->
      V4.HardForkInitiation
        <$> shrink mgid
        <*> shrink v
    V4.TreasuryWithdrawals wdrls msh ->
      V4.TreasuryWithdrawals
        <$> shrink wdrls
        <*> shrink msh
    V4.NoConfidence msh -> V4.NoConfidence <$> shrink msh
    -- No quorum shrinking
    V4.UpdateCommittee mgid creds mems quorum -> do
      mgid' <- shrink mgid
      creds' <- shrink creds
      mems' <- shrink mems
      pure . V4.UpdateCommittee mgid' creds' mems' $ quorum
    V4.NewConstitution mgid c -> V4.NewConstitution <$> shrink mgid <*> shrink c
    _ -> []

instance Arbitrary V4.ProposalProcedure where
  arbitrary = V4.ProposalProcedure <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (V4.ProposalProcedure dep raddr ga) =
    V4.ProposalProcedure <$> shrink dep <*> pure raddr <*> shrink ga

-- A `CurrencySymbol` is either a BLAKE2b-244 hash or empty (representing the
-- Ada symbol). In a fully-fair generator, this makes it vanishingly unlikely
-- that the Ada symbol will be produced naturally (1 in 2^8^28 = 2^244) odds.
-- QuickCheck doesn't give us the ability to represent these odds faithfully:
-- thus, we merely make the Ada symbol as unlikely as we can. If you want to
-- ensure that the Ada symbol is covered by your tests, you need to make
-- dedicated tests for this. For this reason, we also don't shrink into the Ada
-- symbol (indeed, we don't shrink at all).
instance Arbitrary V4.CurrencySymbol where
  arbitrary =
    V4.CurrencySymbol
      <$> frequency
        [ (1, pure ""),
          (maxBound, getSizedByteString . getBlake2b244Hash <$> arbitrary)
        ]

deriving via PlutusTx.BuiltinData instance Arbitrary V4.Datum

instance Arbitrary V4.TxOutRef where
  arbitrary = do
    refId <- arbitrary
    refIdX <- getNonNegative <$> arbitrary
    pure . V4.TxOutRef refId $ refIdX
  shrink (V4.TxOutRef tid ix) =
    V4.TxOutRef tid <$> (fmap getNonNegative . shrink . NonNegative $ ix)

instance Arbitrary V4.Address where
  arbitrary = V4.Address <$> arbitrary <*> arbitrary
  shrink (V4.Address cred mAccId) = do
    mAccId' <- shrink mAccId
    pure . V4.Address cred $ mAccId'

deriving via Blake2b256Hash instance Arbitrary V4.DatumHash

instance Arbitrary V4.OutputDatum where
  arbitrary =
    oneof
      [ pure V4.NoOutputDatum,
        V4.OutputDatumHash <$> arbitrary,
        V4.OutputDatum <$> arbitrary
      ]

  -- We only shrink the OutputDatum case, since the others wouldn't shrink
  -- anyway.
  shrink = \case
    V4.OutputDatum d -> V4.OutputDatum <$> shrink d
    _ -> []

instance Arbitrary V4.TxOut where
  arbitrary =
    V4.TxOut
      <$> arbitrary
      <*> (getUtxoValue <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
  shrink (V4.TxOut addr val outD refScript) = do
    addr' <- shrink addr
    UTxOValue val' <- shrink (UTxOValue val)
    outD' <- shrink outD
    refScript' <- shrink refScript
    let shrink1 = V4.TxOut addr val outD refScript'
    let shrink2 = V4.TxOut addr val outD' refScript
    let shrink3 = V4.TxOut addr val' outD refScript
    let shrink4 = V4.TxOut addr' val outD refScript
    foldl' (<|>) [shrink1] [[shrink2], [shrink3], [shrink4]]

instance Arbitrary V4.TxInInfo where
  arbitrary = V4.TxInInfo <$> arbitrary <*> arbitrary
  shrink (V4.TxInInfo outRef resolved) = do
    outRef' <- shrink outRef
    resolved' <- shrink resolved
    (pure . V4.TxInInfo outRef $ resolved')
      <|> (pure . V4.TxInInfo outRef' $ resolved)

instance Arbitrary V4.MintValue where
  arbitrary = do
    -- Generate a set of currency symbols that aren't Ada
    keySet <- Set.fromList <$> liftArbitrary (V4.CurrencySymbol . getSizedByteString . getBlake2b244Hash <$> arbitrary)
    let keyList = Set.toList keySet
    -- For each key, generate a list of token name keys that aren't Ada
    keyVals <- traverse (scale (`quot` 8) . mkInner) keyList
    -- It is possible to generate both a positive and a negative quantity of the
    -- same asset, so we have to prune zeros, despite using the `NonZero`
    -- generator.
    pure
      . UnsafeMintValue
      . coerce
      . pruneZeroes
      . foldMap (\(cs, vals) -> foldMap (uncurry (V4.singleton cs)) vals)
      $ keyVals
    where
      mkInner :: V4.CurrencySymbol -> Gen (V4.CurrencySymbol, [(V4.TokenName, Integer)])
      mkInner cs =
        (cs,)
          . Set.toList
          . Set.fromList
          . getNonEmpty
          <$> liftArbitrary ((,) <$> genNonAdaTokenName <*> (getNonZero <$> arbitrary))
      genNonAdaTokenName :: Gen V4.TokenName
      genNonAdaTokenName = fmap (V4.TokenName . PlutusTx.toBuiltin @ByteString . BS.pack) . sized $ \size -> do
        len <- resize size . chooseInt $ (1, 32)
        vectorOf len . chooseBoundedIntegral $ (33, 126)
  shrink (UnsafeMintValue v) =
    UnsafeMintValue <$> do
      -- To ensure we don't break anything, we shrink in only two ways:
      --
      -- 1. Dropping keys (outer or inner); and
      -- 2. Shrinking amounts.
      --
      -- To make this a bit easier on ourselves, we first 'unpack' the `MintValue`
      -- completely, shrink the resulting (nested) list, then 'repack'. As neither
      -- of these changes affect order or uniqueness, we're safe.
      let asList = fmap AssocMap.toList <$> AssocMap.toList v
      shrunk <- liftShrink (\(cs, inner) -> (cs,) <$> liftShrink (\(tn, amount) -> (tn,) . getNonZero <$> shrink (NonZero amount)) inner) asList
      pure . AssocMap.unsafeFromList . fmap (fmap AssocMap.unsafeFromList) $ shrunk

deriving via Integer instance Arbitrary V4.POSIXTime

instance Arbitrary V4.POSIXTimeRange where
  arbitrary =
    oneof
      [ -- always
        pure . V4.POSIXTimeRange Nothing $ Nothing,
        -- lower bound only
        V4.POSIXTimeRange . Just <$> arbitrary <*> pure Nothing,
        -- upper bound only
        V4.POSIXTimeRange Nothing . Just <$> arbitrary,
        -- lower and upper
        do
          range <- arbitrary
          start <- arbitrary
          pure . V4.POSIXTimeRange (Just start) . Just $ start + range
      ]

deriving via PlutusTx.BuiltinData instance Arbitrary V4.Redeemer

instance Arbitrary V4.ScriptPurpose where
  arbitrary =
    oneof
      [ V4.Minting <$> arbitrary <*> arbitrary,
        V4.Spending <$> arbitrary <*> arbitrary,
        V4.Withdrawing <$> arbitrary <*> arbitrary,
        V4.Certifying <$> arbitrary <*> arbitrary <*> arbitrary,
        V4.Voting <$> arbitrary <*> arbitrary,
        V4.Proposing <$> arbitrary <*> arbitrary <*> arbitrary,
        V4.Guarding <$> arbitrary <*> arbitrary
      ]
  shrink = \case
    V4.Minting sh cs -> do
      sh' <- shrink sh
      cs' <- shrink cs
      (pure . V4.Minting sh $ cs')
        <|> (pure . V4.Minting sh' $ cs)
    V4.Spending sh tor -> do
      sh' <- shrink sh
      tor' <- shrink tor
      (pure . V4.Spending sh $ tor')
        <|> (pure . V4.Spending sh' $ tor)
    V4.Withdrawing sh cred -> do
      sh' <- shrink sh
      cred' <- shrink cred
      (pure . V4.Withdrawing sh $ cred')
        <|> (pure . V4.Withdrawing sh' $ cred)
    V4.Certifying sh i cert -> do
      sh' <- shrink sh
      i' <- shrink i
      cert' <- shrink cert
      (pure . V4.Certifying sh i $ cert')
        <|> (pure . V4.Certifying sh i' $ cert)
        <|> (pure . V4.Certifying sh' i $ cert)
    V4.Voting sh v -> do
      sh' <- shrink sh
      v' <- shrink v
      (pure . V4.Voting sh $ v')
        <|> (pure . V4.Voting sh' $ v)
    V4.Proposing sh i pp -> do
      sh' <- shrink sh
      i' <- shrink i
      pp' <- shrink pp
      (pure . V4.Proposing sh i $ pp')
        <|> (pure . V4.Proposing sh i' $ pp)
        <|> (pure . V4.Proposing sh' i $ pp)
    V4.Guarding sh i -> do
      sh' <- shrink sh
      i' <- shrink i
      (pure . V4.Guarding sh $ i')
        <|> (pure . V4.Guarding sh' $ i)

instance Arbitrary V4.TxInfo where
  arbitrary = do
    iid <- arbitrary
    subTxIx <- arbitrary
    ins <- getNonEmpty <$> arbitrary
    refIns <- arbitrary
    outs <- getNonEmpty <$> arbitrary
    mint <- arbitrary
    certs <- arbitrary
    wdrls <- arbitrary
    directs <- arbitrary
    abints <- arbitrary
    valid <- arbitrary
    guards <- arbitrary
    reqGuards <- arbitrary
    reds <- arbitrary
    dats <- arbitrary
    votes <- arbitrary
    pps <- arbitrary
    currT <- arbitrary
    tDonation <- arbitrary
    pure
      . V4.TxInfo
        iid
        subTxIx
        ins
        refIns
        outs
        mint
        certs
        wdrls
        directs
        abints
        valid
        guards
        reqGuards
        reds
        dats
        votes
        pps
        currT
      $ tDonation
  shrink (V4.TxInfo iid subTxIx ins refIns outs mint certs wdrls directs abints valid guards reqGuards reds dats votes pps currT tDonation) = do
    ins' <- getNonEmpty <$> shrink (NonEmpty ins)
    outs' <- getNonEmpty <$> shrink (NonEmpty outs)
    V4.TxInfo
      <$> shrink iid
      <*> shrink subTxIx
      <*> pure ins'
      <*> shrink refIns
      <*> pure outs'
      <*> shrink mint
      <*> shrink certs
      <*> shrink wdrls
      <*> shrink directs
      <*> shrink abints
      <*> shrink valid
      <*> shrink guards
      <*> shrink reqGuards
      <*> shrink reds
      <*> shrink dats
      <*> shrink votes
      <*> shrink pps
      <*> shrink currT
      <*> shrink tDonation

instance Arbitrary V4.TopTxInfoSimplified where
  arbitrary = do
    ids <- arbitrary
    ins <- getNonEmpty <$> arbitrary
    refIns <- arbitrary
    outs <- getNonEmpty <$> arbitrary
    mints <- arbitrary
    burns <- arbitrary
    certs <- arbitrary
    wdrls <- arbitrary
    dds <- arbitrary
    valid <- arbitrary
    guards <- arbitrary
    reqGuards <- arbitrary
    redHashes <- arbitrary
    datas <- arbitrary
    votes <- arbitrary
    pps <- arbitrary
    currT <- arbitrary
    tDonations <- arbitrary
    pure
      . V4.TopTxInfoSimplified
        ids
        ins
        refIns
        outs
        mints
        burns
        certs
        wdrls
        dds
        valid
        guards
        reqGuards
        redHashes
        datas
        votes
        pps
        currT
      $ tDonations
  shrink (V4.TopTxInfoSimplified ids ins refIns outs mints burns certs wdrls dds valid guards reqGuards reds datas votes pps currT tDonations) = do
    ins' <- getNonEmpty <$> shrink (NonEmpty ins)
    outs' <- getNonEmpty <$> shrink (NonEmpty outs)
    V4.TopTxInfoSimplified
      <$> shrink ids
      <*> pure ins'
      <*> shrink refIns
      <*> pure outs'
      <*> shrink mints
      <*> shrink burns
      <*> shrink certs
      <*> shrink wdrls
      <*> shrink dds
      <*> shrink valid
      <*> shrink guards
      <*> shrink reqGuards
      <*> shrink reds
      <*> shrink datas
      <*> shrink votes
      <*> shrink pps
      <*> shrink currT
      <*> shrink tDonations

instance Arbitrary V4.TopTxInfo where
  arbitrary = do
    trans <- arbitrary
    dats <- arbitrary
    starts <- arbitrary
    simple <- arbitrary
    pure . V4.TopTxInfo trans dats starts $ simple
  shrink (V4.TopTxInfo trans dats starts simple) =
    V4.TopTxInfo <$> shrink trans <*> shrink dats <*> shrink starts <*> shrink simple

instance Arbitrary V4.ScriptInfo where
  arbitrary =
    oneof
      [ V4.MintingScript <$> arbitrary,
        V4.SpendingScript <$> arbitrary <*> arbitrary,
        V4.WithdrawingScript <$> arbitrary,
        V4.CertifyingScript <$> arbitrary <*> arbitrary,
        V4.VotingScript <$> arbitrary,
        V4.ProposingScript <$> arbitrary <*> arbitrary,
        V4.GuardingScript <$> arbitrary <*> arbitrary
      ]
  shrink = \case
    V4.MintingScript cs -> V4.MintingScript <$> shrink cs
    V4.SpendingScript outRef mDat -> do
      outRef' <- shrink outRef
      mDat' <- shrink mDat
      (pure . V4.SpendingScript outRef $ mDat')
        <|> (pure . V4.SpendingScript outRef' $ mDat)
    V4.WithdrawingScript aid -> V4.WithdrawingScript <$> shrink aid
    V4.CertifyingScript i txCert -> do
      i' <- shrink i
      txCert' <- shrink txCert
      (pure . V4.CertifyingScript i $ txCert')
        <|> (pure . V4.CertifyingScript i' $ txCert)
    V4.VotingScript v -> V4.VotingScript <$> shrink v
    V4.ProposingScript i pp -> do
      i' <- shrink i
      pp' <- shrink pp
      (pure . V4.ProposingScript i $ pp')
        <|> (pure . V4.ProposingScript i' $ pp)
    V4.GuardingScript i mTxInfo -> do
      i' <- shrink i
      mTxInfo' <- shrink mTxInfo
      (pure . V4.GuardingScript i $ mTxInfo')
        <|> (pure . V4.GuardingScript i' $ mTxInfo)

instance Arbitrary V4.ScriptContext where
  arbitrary = V4.ScriptContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (V4.ScriptContext info red si sh) =
    V4.ScriptContext <$> shrink info <*> shrink red <*> shrink si <*> shrink sh

-- Helpers

newtype Blake2b224Hash = Blake2b224Hash {getBlake2b244Hash :: SizedByteString 28}
  deriving (Eq, Ord, Arbitrary) via (SizedByteString 28)
  deriving stock (Show)

newtype Blake2b256Hash = Blake2b256Hash {getBlake2b256Hash :: SizedByteString 32}
  deriving (Eq, Ord, Arbitrary) via (SizedByteString 32)
  deriving stock (Show)

newtype SizedByteString (n :: Nat) = UnsafeSizedByteString {getSizedByteString :: PlutusTx.BuiltinByteString}
  deriving (Eq, Ord) via PlutusTx.BuiltinByteString
  deriving stock (Show)

type role SizedByteString nominal

instance (KnownNat n) => Arbitrary (SizedByteString n) where
  arbitrary =
    UnsafeSizedByteString <$> do
      let len = fromIntegral . natVal $ Proxy @n
      asList <- vectorOf len arbitrary
      pure . PlutusTx.toBuiltin @ByteString . BS.pack $ asList

-- A 'PLA.Value' suitable for 'PLA.TxOut'. Specifically:
--

-- * The `PLA.Value` is sorted by both keys (meaning 'PLA.CurrencySymbol' and

--   'PLA.TokenName');

-- * There exists an Ada amount; and

-- * All amounts are positive.

--
-- = Note
--
-- This is designed to act as a modifier, and thus, we expose the constructor
-- even though it preserves invariants. If you use the constructor directly,
-- be /very/ certain that the Value being wrapped satisfies the invariants
-- described above: failing to do so means all guarantees of this type are off
-- the table.
newtype UTxOValue = UTxOValue V4.Value
  deriving (Eq) via V4.Value
  deriving stock (Show)

instance Arbitrary UTxOValue where
  arbitrary =
    UTxOValue <$> do
      Positive adaQuantity <- arbitrary
      -- Set of non-Ada currency symbols
      csSet <- Set.fromList <$> liftArbitrary (V4.CurrencySymbol . getSizedByteString . getBlake2b244Hash <$> arbitrary)
      let cses = Set.toList csSet
      -- For each key, generate a set of token names that aren't Ada, and a
      -- positive value
      table <- traverse (scale (`quot` 8) . mkInner) cses
      -- Jam the Ada value in there
      let table' = (V4.adaSymbol, [(V4.adaToken, adaQuantity)]) : table
      pure . pruneZeroes . V4.Value . AssocMap.unsafeFromList . fmap (fmap AssocMap.unsafeFromList) $ table'
    where
      mkInner :: V4.CurrencySymbol -> Gen (V4.CurrencySymbol, [(V4.TokenName, Integer)])
      mkInner cs =
        (cs,) <$> do
          -- Set of non-Ada token names
          tnSet <- Set.fromList <$> liftArbitrary genNonAdaTokenName
          let asList = Set.toList tnSet
          traverse (\tn -> (tn,) . getPositive <$> arbitrary) asList
      genNonAdaTokenName :: Gen V4.TokenName
      genNonAdaTokenName =
        V4.TokenName . PlutusTx.toBuiltin @ByteString . BS.pack <$> do
          len <- chooseInt (1, 32)
          -- ASCII printable range
          vectorOf len . chooseBoundedIntegral $ (33, 126)
  shrink (UTxOValue (V4.Value v)) =
    UTxOValue . V4.Value <$> do
      -- To ensure we don't break anything, we shrink in only two ways:
      --
      -- 1. Dropping keys (outer or inner)
      -- 2. Shrinking amounts
      --
      -- To make this a bit easier on ourselves, we first 'unpack' the Value
      -- completely, shrink the resulting (nested) list, then 'repack'. As neither
      -- of these changes affect order or uniqueness, we're safe.
      let asList = fmap AssocMap.toList <$> AssocMap.toList v
      shrunk <- liftShrink (\(cs, inner) -> (cs,) <$> liftShrink (\(tn, amount) -> (tn,) . getPositive <$> shrink (Positive amount)) inner) asList
      pure . AssocMap.unsafeFromList . fmap (fmap AssocMap.unsafeFromList) $ shrunk

getUtxoValue :: UTxOValue -> V4.Value
getUtxoValue = coerce

pruneZeroes :: V4.Value -> V4.Value
pruneZeroes (V4.Value assets) =
  V4.Value $
    AssocMap.unsafeFromList $
      filter (not . AssocMap.null . snd) $ -- After removing tokens now we may have empty currency list, so clear that as well
        AssocMap.toList (AssocMap.mapMaybe (assocMapNonEmpty . filter ((/= 0) . snd) . AssocMap.toList) assets) -- Remove all zero tokens
  where
    assocMapNonEmpty :: [(k, v)] -> Maybe (AssocMap.Map k v)
    assocMapNonEmpty [] = Nothing
    assocMapNonEmpty lst = Just $ AssocMap.unsafeFromList lst
