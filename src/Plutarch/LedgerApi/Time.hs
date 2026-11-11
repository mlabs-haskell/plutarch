{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Time
  ( PPosixTime (..),
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
newtype PPosixTime = PPosixTime (Term PInteger)

deriveFor ''PPosixTime Newtype AllInstances

-- | @since 2.0.0
instance PAdditiveSemigroup PPosixTime

-- | @since 2.0.0
instance PAdditiveMonoid PPosixTime

-- | @since 2.0.0
instance PAdditiveGroup PPosixTime

-- | @since 2.0.0
instance PlutusRepresentable PLA.POSIXTime where
  type AsPlutus PLA.POSIXTime = Integer
  haskToRepr = coerce
  reprToHask = Right . coerce

-- | @since 2.0.0
instance PLiftable PPosixTime where
  type AsHaskell PPosixTime = PLA.POSIXTime
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant

-- | @since 2.0.0
instance PCanData PPosixTime

-- | @since 2.0.0
instance PValidateData PPosixTime where
  pwithValidated = pwithValidated @PInteger
