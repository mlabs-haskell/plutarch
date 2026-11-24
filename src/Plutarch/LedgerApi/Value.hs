{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Value
  ( PLovelace (..),
    PCurrencySymbol (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Additive
  ( PAdditiveGroup,
    PAdditiveMonoid,
    PAdditiveSemigroup,
  )
import Plutarch.Primitive.BuiltinFun (plengthOfByteString, punBData)
import Plutarch.Primitive.CanData (PCanData)
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
import Plutarch.Primitive.Singular (PByteString, PInteger)
import Plutarch.Primitive.Term (perror)
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
  )
import Plutarch.TH.Strategy
  ( Strategy (Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.Prelude qualified as PTx

-- | @since 2.0.0
newtype PLovelace = PLovelace (Term PInteger)

deriveFor ''PLovelace Newtype AllInstances

-- | @since 2.0.0
instance PAdditiveSemigroup PLovelace

-- | @since 2.0.0
instance PAdditiveMonoid PLovelace

-- | @since 2.0.0
instance PAdditiveGroup PLovelace

-- | @since 2.0.0
instance PlutusRepresentable PLA.Lovelace where
  type AsPlutus PLA.Lovelace = Integer
  haskToRepr = coerce
  reprToHask = Right . coerce

-- | @since 2.0.0
instance PLiftable PLovelace where
  type AsHaskell PLovelace = PLA.Lovelace
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant

-- | @since 2.0.0
instance PCanData PLovelace

-- | @since 2.0.0
instance PValidateData PLovelace where
  pwithValidated = pwithValidated @PInteger

-- | @since 2.0.0
newtype PCurrencySymbol = PCurrencySymbol (Term PByteString)

deriveFor ''PCurrencySymbol Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.CurrencySymbol where
  type AsPlutus PLA.CurrencySymbol = ByteString
  haskToRepr = PTx.fromBuiltin @PTx.BuiltinByteString . coerce
  reprToHask bs
    | BS.null bs = Right . PLA.CurrencySymbol . PTx.toBuiltin $ bs
    | BS.length bs == 28 = Right . PLA.CurrencySymbol . PTx.toBuiltin $ bs
    | otherwise = Left . UserSpecified $ "Invalid length for CurrencySymbol"

-- | @since 2.0.0
instance PLiftable PCurrencySymbol where
  type AsHaskell PCurrencySymbol = PLA.CurrencySymbol
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant

-- | @since 2.0.0
instance PCanData PCurrencySymbol

-- | @sicne 2.0.0
instance PValidateData PCurrencySymbol where
  pwithValidated d r =
    let asBS = punBData # d
     in pif
          ((plengthOfByteString # asBS) #== pconstant 0)
          r
          ( pif
              ((plengthOfByteString # asBS) #== pconstant 28)
              r
              perror
          )
