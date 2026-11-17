{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.Poly
  ( PA (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PlutusRepresentable
      ( AsPlutus,
        haskToRepr,
        reprToHask
      ),
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( pfundamental,
    punsafeSecondary,
    (#),
  )
import Plutarch.Primitive.Singular (PInteger)
import Plutarch.TH.Strategy
  ( Strategy (Newtype),
    WhichInstances (AllInstances),
    deriveFor,
  )
import Test.QuickCheck.Poly (A (A, unA))

-- | Analog to 'A' from @Test.QuickCheck.Poly@. Can be used to test polymorphic
-- functions or data types at the Plutarch level.
--
-- @since 2.0.0
newtype PA = PA (Term PInteger)

deriveFor ''PA Newtype AllInstances

-- | @since 2.0.0
instance PlutusRepresentable A where
  type AsPlutus A = Integer
  haskToRepr = unA
  reprToHask = Right . A

-- | @since 2.0.0
instance PCanData PA

-- | @since 2.0.0
instance PLiftable PA where
  type AsHaskell PA = A
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . reprToPlut
