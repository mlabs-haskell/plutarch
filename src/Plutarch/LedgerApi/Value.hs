{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Value
  ( PLovelace (..),
  )
where

import Data.Coerce (coerce)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Additive
  ( PAdditiveGroup,
    PAdditiveMonoid,
    PAdditiveSemigroup,
  )
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( pfundamental,
    punsafeSecondary,
    (#),
  )
import Plutarch.Primitive.Singular (PInteger)
import Plutarch.Primitive.Validate
  ( PValidateData (pwithValidated),
  )
import Plutarch.TH.Strategy
  ( Strategy (Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA

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
