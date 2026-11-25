{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.AssocMap
  ( PMapData (..),
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun (pmapData, punMapData)
import Plutarch.Primitive.CanData (PCanData (pfromData, ptoData))
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil), PBPair (PBPair))
import Plutarch.Primitive.Con (PCon (pcon'), pcon)
import Plutarch.Primitive.Debug (PDebug (pdebug), pshow)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell),
    PLiftableData (PLiftableData),
    PlutusRepresentable,
    PlutusRepresentableData (PlutusRepresentableData),
  )
import Plutarch.Primitive.Match (PMatch (pmatch'), pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    punsafeSecondary,
    (#),
  )
import Plutarch.Primitive.Singular (PAsData, PData)
import Plutarch.Primitive.Term (pfix, plam', punsafeCoerce)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData
import Type.Reflection (Typeable)

-- | @since 2.0.0
newtype PMapData (k :: Type) (v :: Type)
  = PMapData (Term (PBList (PBPair (PAsData k) (PAsData v))))

-- | @since 2.0.0
instance PlutarchType (PMapData k v) where
  type PRepresentation (PMapData k v) = PData

-- | @since 2.0.0
instance PMatch (PMapData k v) where
  pmatch' t f =
    let asList = punsafeSecondary $ punMapData # t
     in f (PMapData asList)

-- | @since 2.0.0
instance PCon (PMapData k v) where
  pcon' (PMapData asList) = pfundamental (pmapData # asList)

-- | @since 2.0.0
instance PDebug (PMapData k v) where
  pdebug t f = pmatch t $ \(PMapData inner) -> f (pshow inner)

-- | @since 2.0.0
instance PCanData (PMapData k v) where
  pfromData t = pcon . PMapData . punsafeSecondary $ punMapData # pfundamental t
  ptoData t = pmatch t $ \(PMapData asList) -> punsafeCoerce $ pmapData # asList

-- | Checks that we have a @Map@ constructor of @Data@, that every entry is a
-- @Data@-encoded pair, that every pair validates as a pair of @k@ and @v@. As
-- Plutus assoc maps are guaranteed to be neither ordered nor have unique keys,
-- this is all we can do.
--
-- @since 2.0.0
instance (PValidateData k, PValidateData v) => PValidateData (PMapData k v) where
  pwithValidated :: Term PData -> (forall (r :: Type). (PlutarchType r) => Term r -> Term r)
  pwithValidated d r =
    let asList = punMapData # d
     in go # asList
    where
      go = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
        PBNil -> r
        PBCons x xs -> pmatch x $ \(PBPair k v) ->
          pwithValidated @k k . pwithValidated @v v $ self # xs

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData (PLA.Map k v))
  instance
    ( PTxData.ToData k,
      PTxData.ToData v,
      Typeable k,
      Typeable v,
      PTxData.FromData k,
      PTxData.FromData v
    ) =>
    PlutusRepresentable (PLA.Map k v)

-- | @since 2.0.0
deriving via
  (PLiftableData (PMapData k v) (PLA.Map (AsHaskell k) (AsHaskell v)))
  instance
    ( PTxData.ToData (AsHaskell k),
      PTxData.ToData (AsHaskell v),
      Typeable (AsHaskell k),
      Typeable (AsHaskell v),
      PTxData.FromData (AsHaskell k),
      PTxData.FromData (AsHaskell v)
    ) =>
    PLiftable (PMapData k v)
