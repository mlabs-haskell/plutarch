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
import Plutarch.Backend.Term (S, Term)
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
    (:-->),
  )
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pfix, plam', punsafeCoerce)

-- | @since 2.0.0
class (PlutarchType a) => PMultiplicativeSemigroup (a :: S -> Type) where
  pmultiply :: forall (s :: S). Term s (a :--> a :--> a)
  default pmultiply ::
    forall (s :: S).
    (PMultiplicativeSemigroup (PRepresentation a)) =>
    Term s (a :--> a :--> a)
  pmultiply = punsafeCoerce (pmultiply @(PRepresentation a))
  ppowPositive :: forall (s :: S). Term s (PPositive :--> a :--> a)
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
class (PMultiplicativeSemigroup a) => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a
  default pone ::
    forall (s :: S).
    (PMultiplicativeMonoid (PRepresentation a)) =>
    Term s a
  pone = punsafeSecondary (pone @(PRepresentation a))
  ppowNatural :: forall (s :: S). Term s (PNatural :--> a :--> a)
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
  forall (a :: S -> Type) (s :: S).
  (PMultiplicativeSemigroup a) =>
  Term s a -> Term s a -> Term s a
x #* y = pmultiply # x # y

infix 6 #*

-- | @since 2.0.0
pproduct ::
  forall (a :: S -> Type) (s :: S).
  (PMultiplicativeMonoid a) =>
  Term s (C.PBList a :--> a)
pproduct = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
  C.PBNil -> pone
  C.PBCons x xs -> pmultiply # x #$ self # xs
