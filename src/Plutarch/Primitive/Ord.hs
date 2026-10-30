{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.Ord
  ( -- * Type class
    POrd (..),

    -- * Functions
    (#<=),
    (#<),
    (#>=),
    (#>),
    pmin,
    pmax,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.DerivedFun (pfalse, pif, ptrue)
import Plutarch.Primitive.Eq (PEq)
import Plutarch.Primitive.Representation (PRepresentation, (#))
import Plutarch.Primitive.Singular (PBool, (:~>))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (plam', punsafeCoerce)

-- | @since 2.0.0
class (PEq a) => POrd (a :: Type) where
  plt :: Term (a :~> a :~> PBool)
  default plt ::
    (POrd (PRepresentation a)) => Term (a :~> a :~> PBool)
  plt = punsafeCoerce (plt @(PRepresentation a))
  ple :: Term (a :~> a :~> PBool)
  default ple ::
    (POrd (PRepresentation a)) => Term (a :~> a :~> PBool)
  ple = punsafeCoerce (ple @(PRepresentation a))

-- | @since 2.0.0
instance POrd PBool where
  plt = plam' $ \x -> plam' $ \y -> pif x pfalse y
  ple = plam' $ \x -> plam' $ \y -> pif x y ptrue

-- | @since 2.0.0
instance POrd S.PInteger where
  plt = BF.plessThanInteger
  ple = BF.plessThanEqualsInteger

-- | @since 2.0.0
instance POrd S.PNatural

-- | @since 2.0.0
instance POrd S.PPositive

-- | @since 2.0.0
instance POrd S.PByte

-- | @since 2.0.0
instance POrd S.PByteString where
  plt = BF.plessThanByteString
  ple = BF.plessThanEqualsByteString

-- | @since 2.0.0
(#<=) ::
  forall (a :: Type).
  (POrd a) =>
  Term a -> Term a -> Term PBool
x #<= y = ple # x # y

infix 4 #<=

-- | @since 2.0.0
(#<) ::
  forall (a :: Type).
  (POrd a) =>
  Term a -> Term a -> Term PBool
x #< y = plt # x # y

infix 4 #<

-- | @since 2.0.0
(#>=) ::
  forall (a :: Type).
  (POrd a) =>
  Term a -> Term a -> Term PBool
x #>= y = y #<= x

infix 4 #>=

-- | @since 2.0.0
(#>) ::
  forall (a :: Type).
  (POrd a) =>
  Term a -> Term a -> Term PBool
x #> y = y #< x

infix 4 #>

-- | @since 2.0.0
pmin ::
  forall (a :: Type).
  (POrd a) => Term a -> Term a -> Term a
pmin x y = pif (x #<= y) x y

-- | @since 2.0.0
pmax ::
  forall (a :: Type).
  (POrd a) => Term a -> Term a -> Term a
pmax x y = pif (x #<= y) y x
