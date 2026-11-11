{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Time
  ( PPosixTime (..),
    PPosixTimeRange (..),
  )
where

import Data.Coerce (coerce)
import Plutarch.Backend.Term (Term)
import Plutarch.LedgerApi.MaybeData (PMaybeData)
import Plutarch.Primitive.Additive
  ( PAdditiveGroup,
    PAdditiveMonoid,
    PAdditiveSemigroup,
  )
import Plutarch.Primitive.BuiltinFun
  ( pappendString,
    plistData,
    punListData,
  )
import Plutarch.Primitive.CanData (PCanData, pfromData)
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
import Plutarch.Primitive.Singular (PAsData, PData, PInteger)
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

-- | = Note
--
-- The 'PValidateData' instance for 'PPosixTimeRange' only checks that the right
-- fields, of the right types, and in the right amount, are present in the
-- encoding. Importantly, it /does not/ verify that, if both fields are
-- @PJustData@, that the lower bound is not greater than the upper bound. If
-- this is important to you, ensure you check this yourself.
--
-- @since 2.0.0
data PPosixTimeRange = PPosixTimeRange
  { pfromInclusive :: Term (PAsData (PMaybeData PPosixTime)),
    ptoInclusive :: Term (PAsData (PMaybeData PPosixTime))
  }

deriveFor ''PPosixTimeRange DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.POSIXTimeRange where
  type AsPlutus PLA.POSIXTimeRange = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode POSIXTimeRange from Data"
    Just x -> pure x

-- | @since 2.0.0
instance PLiftable PPosixTimeRange where
  type AsHaskell PPosixTimeRange = PLA.POSIXTimeRange
  plutToRepr t = plutToRepr . pfundamental $ (plistData # pfundamental t)
  reprToPlut d = punsafeSecondary (punListData # pconstant @PData d)
