{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.Additive
  ( -- * Type classes
    PAdditiveSemigroup (..),
    PAdditiveMonoid (..),
    PAdditiveGroup (..),

    -- * Functions
    (#+),
    (#-),
    psum,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Helpers.Numeric (pbySquaring)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun (pif, pisNegative)
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
import PlutusCore.Crypto.BLS12_381.G1 as G1
import PlutusCore.Crypto.BLS12_381.G2 as G2

-- | @since 2.0.0
class (PlutarchType a) => PAdditiveSemigroup (a :: S -> Type) where
  padd :: forall (s :: S). Term s (a :--> a :--> a)
  default padd ::
    forall (s :: S).
    (PAdditiveSemigroup (PRepresentation a)) =>
    Term s (a :--> a :--> a)
  padd = punsafeCoerce (padd @(PRepresentation a))
  pscalePositive :: forall (s :: S). Term s (PPositive :--> a :--> a)
  pscalePositive = plam' $ \e -> plam' $ pbySquaring padd (pfundamental e)

-- | @since 2.0.0
instance PAdditiveSemigroup PInteger where
  padd = BF.paddInteger
  pscalePositive = punsafeCoerce BF.pmultiplyInteger

-- | @since 2.0.0
instance PAdditiveSemigroup PNatural

-- | @since 2.0.0
instance PAdditiveSemigroup PPositive

-- | @since 2.0.0
instance PAdditiveSemigroup S.PBLS12_381_G1_Element where
  padd = BF.pbls12_381_G1_add
  pscalePositive = punsafeCoerce BF.pbls12_381_G1_scalarMul

-- | @since 2.0.0
instance PAdditiveSemigroup S.PBLS12_381_G2_Element where
  padd = BF.pbls12_381_G2_add
  pscalePositive = punsafeCoerce BF.pbls12_381_G2_scalarMul

-- | @since 2.0.0
class (PAdditiveSemigroup a) => PAdditiveMonoid (a :: S -> Type) where
  pzero :: forall (s :: S). Term s a
  default pzero ::
    forall (s :: S).
    (PAdditiveMonoid (PRepresentation a)) =>
    Term s a
  pzero = punsafeSecondary (pzero @(PRepresentation a))
  pscaleNatural :: forall (s :: S). Term s (PNatural :--> a :--> a)
  pscaleNatural = plam' $ \e -> plam' $ \b ->
    pif
      (e #== pconstant 0)
      pzero
      (pscalePositive # punsafeTranslate e # b)

-- | @since 2.0.0
instance PAdditiveMonoid PInteger where
  pzero = pconstant 0
  pscaleNatural = punsafeCoerce BF.pmultiplyInteger

-- | @since 2.0.0
instance PAdditiveMonoid PNatural

-- | @since 2.0.0
instance PAdditiveMonoid S.PBLS12_381_G1_Element where
  pzero = pconstant G1.offchain_zero
  pscaleNatural = punsafeCoerce BF.pbls12_381_G1_scalarMul

-- | @since 2.0.0
instance PAdditiveMonoid S.PBLS12_381_G2_Element where
  pzero = pconstant G2.offchain_zero
  pscaleNatural = punsafeCoerce BF.pbls12_381_G2_scalarMul

-- | @since 2.0.0
class (PAdditiveMonoid a) => PAdditiveGroup (a :: S -> Type) where
  psubtract :: Term s (a :--> a :--> a)
  psubtract = plam' $ \x -> plam' $ \y -> padd # x #$ pnegate # y
  pnegate :: Term s (a :--> a)
  pnegate = plam' $ \x -> psubtract # pzero # x
  pscaleInteger :: forall (s :: S). Term s (PInteger :--> a :--> a)
  pscaleInteger = plam' $ \e -> plam' $ \b ->
    pif
      (pisNegative e)
      (pnegate #$ pscaleNatural # punsafeSecondary (BF.psubtractInteger # pzero # e) # b)
      (pscaleNatural # punsafeSecondary e # b)

-- | @since 2.0.0
instance PAdditiveGroup PInteger where
  psubtract = BF.psubtractInteger
  pscaleInteger = punsafeCoerce BF.pmultiplyInteger

-- | @since 2.0.0
instance PAdditiveGroup S.PBLS12_381_G1_Element where
  pnegate = BF.pbls12_381_G1_neg
  pscaleInteger = BF.pbls12_381_G1_scalarMul

-- | @since 2.0.0
instance PAdditiveGroup S.PBLS12_381_G2_Element where
  pnegate = BF.pbls12_381_G2_neg
  pscaleInteger = BF.pbls12_381_G2_scalarMul

-- | @since 2.0.0
(#+) ::
  forall (a :: S -> Type) (s :: S).
  (PAdditiveSemigroup a) =>
  Term s a -> Term s a -> Term s a
x #+ y = padd # x # y

infix 6 #+

-- | @since 2.0.0
psum ::
  forall (a :: S -> Type) (s :: S).
  (PAdditiveMonoid a) =>
  Term s (C.PBList a :--> a)
psum = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
  C.PBNil -> pzero
  C.PBCons x xs -> padd # x #$ self # xs

-- | @since 2.0.0
(#-) ::
  forall (a :: S -> Type) (s :: S).
  (PAdditiveGroup a) =>
  Term s a -> Term s a -> Term s a
x #- y = psubtract # x # y

infix 6 #-
