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
  ( PLiftable (AsHaskell),
    PLiftableData (PLiftableData),
    PlutusRepresentable,
    PlutusRepresentableData (PlutusRepresentableData),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation (pfundamental, (#))
import Plutarch.Primitive.Singular (PAsData)
import Plutarch.Primitive.Term (perror)
import Plutarch.TH.Strategy
  ( Strategy (DataPlutus),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusTx.Prelude qualified as PTx
import Type.Reflection (Typeable)

-- | @since 2.0.0
data PMaybeData (a :: Type)
  = PJustData (Term (PAsData a))
  | PNothingData

deriveFor ''PMaybeData DataPlutus AllInstances

-- | @since 2.0.0
deriving via
  (PlutusRepresentableData (PTx.Maybe a))
  instance
    (PTx.ToData a, PTx.FromData a, Typeable a) => PlutusRepresentable (PTx.Maybe a)

-- | @since 2.0.0
deriving via
  (PLiftableData (PMaybeData a) (PTx.Maybe (AsHaskell a)))
  instance
    ( PLiftable a,
      PCanData a,
      PTx.ToData (AsHaskell a),
      PTx.FromData (AsHaskell a),
      Typeable (AsHaskell a)
    ) =>
    PLiftable (PMaybeData a)
