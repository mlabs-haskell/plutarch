{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.Multiplicative
  ( -- * Type classes
    PMultiplicativeSemigroup (..),
    PMultiplicativeMonoid (..),

    -- * Functions
    (#*),
    pproduct,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)
import Plutarch.Helpers.Numeric (pbySquaring)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    punsafeSecondary,
    punsafeTranslate,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular
  ( PInteger,
    PNatural,
    PPositive,
    (:~>),
  )
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pfix, plam', punsafeCoerce)

-- | @since 2.0.0
class (PlutarchType a) => PMultiplicativeSemigroup (a :: Type) where
  pmultiply :: Term (a :~> a :~> a)
  default pmultiply ::
    (PMultiplicativeSemigroup (PRepresentation a)) =>
    Term (a :~> a :~> a)
  pmultiply = punsafeCoerce (pmultiply @(PRepresentation a))
  ppowPositive :: Term (PPositive :~> a :~> a)
  ppowPositive = plam' $ \e -> plam' $ pbySquaring pmultiply (pfundamental e)

-- | @since 2.0.0
instance PMultiplicativeSemigroup PInteger where
  pmultiply = BF.pmultiplyInteger

-- | @since 2.0.0
instance PMultiplicativeSemigroup PNatural

-- | @since 2.0.0
instance PMultiplicativeSemigroup PPositive

-- | @since 2.0.0
instance PMultiplicativeSemigroup S.PBLS12_381_MlResult where
  pmultiply = BF.pbls12_381_mulMlResult

-- | @since 2.0.0
class (PMultiplicativeSemigroup a) => PMultiplicativeMonoid (a :: Type) where
  pone :: Term a
  default pone ::
    (PMultiplicativeMonoid (PRepresentation a)) =>
    Term a
  pone = punsafeSecondary (pone @(PRepresentation a))
  ppowNatural :: Term (PNatural :~> a :~> a)
  ppowNatural = plam' $ \e -> plam' $ \b ->
    pif
      (e #== pconstant 1)
      pone
      (ppowPositive # punsafeTranslate e # b)

-- | @since 2.0.0
instance PMultiplicativeMonoid PInteger where
  pone = pconstant 1

-- | @since 2.0.0
instance PMultiplicativeMonoid PNatural

-- | @since 2.0.0
instance PMultiplicativeMonoid PPositive

-- | @since 2.0.0
(#*) ::
  forall (a :: Type).
  (PMultiplicativeSemigroup a) =>
  Term a -> Term a -> Term a
x #* y = pmultiply # x # y

infix 6 #*

-- | @since 2.0.0
pproduct ::
  forall (a :: Type).
  (PMultiplicativeMonoid a) =>
  Term (C.PBList a :~> a)
pproduct = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
  C.PBNil -> pone
  C.PBCons x xs -> pmultiply # x #$ self # xs
