{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Credential
  ( PScriptHash (..),
    PPubKeyHash (..),
    PCredential (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun
  ( pappendString,
    plengthOfByteString,
  )
import Plutarch.Primitive.CanData (PCanData (pfromData))
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PLiftableData (PLiftableData),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    PlutusRepresentableData (PlutusRepresentableData),
    ReprError (UserSpecified),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation (pfundamental, (#))
import Plutarch.Primitive.Singular
  ( PAsData,
    PByteString,
    PNatural,
  )
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
    pvalidateData,
  )
import Plutarch.TH.Strategy
  ( Strategy (DataPlutusCompact, Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.Prelude qualified as PTx

-- Note (Koz, 18/11/2026): For some utterly incomprehensible reason, putting
-- these definitions at the _bottom_ of the source file makes GHC think they
-- don't exist in the instances that use them. What is this, C?
--
-- I suspect TH might be be cause of this.

hashLen :: Int
hashLen = 28

phashLen :: Term PNatural
phashLen = pconstant 28

-- | @since 2.0.0
newtype PScriptHash = PScriptHash (Term PByteString)

deriveFor ''PScriptHash Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.ScriptHash where
  type AsPlutus PLA.ScriptHash = ByteString
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinByteString . coerce
  reprToHask bs
    | BS.length bs == hashLen = Right . PLA.ScriptHash . PTx.toBuiltin $ bs
    | otherwise = Left . UserSpecified $ "Invalid length for ScriptHash."

-- | @since 2.0.0
instance PLiftable PScriptHash where
  type AsHaskell PScriptHash = PLA.ScriptHash
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = pcon . PScriptHash . reprToPlut

-- | @since 2.0.0
instance PCanData PScriptHash

-- | Verifies that we have a @Data@-encoded 'PByteString' whose length is
-- exactly 28.
--
-- @since 2.0.0
instance PValidateData PScriptHash where
  pwithValidated d res =
    let bs = pfromData (pvalidateData d)
     in pif
          ((plengthOfByteString # bs) #== phashLen)
          res
          perror

-- | @since 2.0.0
newtype PPubKeyHash = PPubKeyHash (Term PByteString)

deriveFor ''PPubKeyHash Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.PubKeyHash where
  type AsPlutus PLA.PubKeyHash = ByteString
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinByteString . coerce
  reprToHask bs
    | BS.length bs == hashLen = Right . PLA.PubKeyHash . PTx.toBuiltin $ bs
    | otherwise = Left . UserSpecified $ "Invalid length for PubKeyHash."

-- | @since 2.0.0
instance PLiftable PPubKeyHash where
  type AsHaskell PPubKeyHash = PLA.PubKeyHash
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = pcon . PPubKeyHash . reprToPlut

-- | @since 2.0.0
instance PCanData PPubKeyHash

-- | Verifies that we have a @Data@-encoded 'PByteString' whose length is
-- exactly 28.
--
-- @since 2.0.0
instance PValidateData PPubKeyHash where
  pwithValidated d res =
    let bs = pfromData (pvalidateData d)
     in pif
          ((plengthOfByteString # bs) #== phashLen)
          res
          perror

-- | @since 2.0.0
data PCredential
  = PPubKeyCredential (Term (PAsData PPubKeyHash))
  | PScriptCredential (Term (PAsData PScriptHash))

deriveFor ''PCredential DataPlutusCompact AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData PLA.Credential)
  instance
    PlutusRepresentable PLA.Credential

-- | @since 2.0.0
deriving via
  (PLiftableData PCredential PLA.Credential)
  instance
    PLiftable PCredential
