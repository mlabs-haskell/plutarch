{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.MaybeData
  ( PMaybeData (..),
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun (pappendString)
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
import Plutarch.Primitive.Singular (PAsData)
import Plutarch.Primitive.Term (perror)
import Plutarch.TH.Strategy
  ( Strategy (DataPlutus),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData
import PlutusTx.Prelude qualified as PTx

-- | @since 2.0.0
data PMaybeData (a :: Type)
  = PJustData (Term (PAsData a))
  | PNothingData

deriveFor ''PMaybeData DataPlutus AllInstances

-- | @since 2.0.0
instance (PTx.ToData a, PTx.FromData a) => PlutusRepresentable (PTx.Maybe a) where
  type AsPlutus (PTx.Maybe a) = PLA.Data
  haskToRepr = PTxData.toData
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode Maybe from Data"
    Just x -> pure x

-- | @since 2.0.0
instance
  (PLiftable a, PCanData a, PTx.ToData (AsHaskell a), PTx.FromData (AsHaskell a)) =>
  PLiftable (PMaybeData a)
  where
  type AsHaskell (PMaybeData a) = PTx.Maybe (AsHaskell a)
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . pconstant
