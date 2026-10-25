{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.Eq
  ( PEq (..),
    (#==),
    (#/=),
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun qualified as F
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun (pfalse, pif, pnot, ptrue)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    (#),
  )
import Plutarch.Primitive.Singular (PBool, (:-->))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term
  ( pfix,
    plam',
    punsafeCase,
    punsafeCoerce,
    punsafeConstant,
  )
import PlutusCore qualified as PLC

-- | @since 2.0.0
class (PlutarchType a) => PEq (a :: S -> Type) where
  peq :: forall (s :: S). Term s (a :--> a :--> PBool)
  default peq ::
    forall (s :: S).
    (PEq (PRepresentation a)) => Term s (a :--> a :--> PBool)
  peq = punsafeCoerce (peq @(PRepresentation a))

-- | @since 2.0.0
instance PEq PBool where
  peq = plam' $ \x -> plam' $ \y ->
    pif x y (pnot y)

-- | @since 2.0.0
instance PEq S.PInteger where
  peq = F.pequalsInteger

-- | @since 2.0.0
instance PEq S.PNatural

-- | @since 2.0.0
instance PEq S.PPositive

-- | @since 2.0.0
instance PEq S.PByte

-- | @since 2.0.0
instance PEq S.PBLS12_381_G1_Element where
  peq = F.pbls12_381_G1_equal

-- | @since 2.0.0
instance PEq S.PBLS12_381_G2_Element where
  peq = F.pbls12_381_G2_equal

-- | @since 2.0.0
instance PEq S.PByteString where
  peq = F.pequalsByteString

-- | @since 2.0.0
instance PEq S.PData where
  peq = F.pequalsData

-- | @since 2.0.0
instance PEq (S.PAsData a)

-- | @since 2.0.0
instance PEq S.PString where
  peq = F.pequalsString

-- | @since 2.0.0
instance PEq S.PBValue where
  peq = plam' $ \v1 -> plam' $ \v2 ->
    F.pequalsData # (F.pvalueData # v1) # (F.pvalueData # v2)

-- | @since 2.0.0
instance (PEq a) => PEq (S.PBArray a) where
  peq = plam' $ \xs -> plam' $ \ys ->
    pif
      (peq # (F.plengthOfArray # xs) # (F.plengthOfArray # ys))
      (go xs ys (F.plengthOfArray # xs) # pconstant 0)
      pfalse
    where
      go ::
        forall (s :: S).
        Term s (S.PBArray a) ->
        Term s (S.PBArray a) ->
        Term s S.PNatural ->
        Term s (S.PNatural :--> PBool)
      go xs ys limit = pfix $ \self -> plam' $ \i ->
        pif
          (peq # i # limit)
          ptrue
          ( pif
              (peq # (F.pindexArray # xs # i) # (F.pindexArray # ys # i))
              (self # punsafeCoerce (F.paddInteger # pfundamental i # pconstant 1))
              pfalse
          )

-- | @since 2.0.0
instance (PEq a) => PEq (C.PBList a) where
  peq = pfix $ \self -> plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \case
      C.PBNil -> pmatch ys $ \case
        C.PBNil -> punsafeConstant . PLC.someValue @Bool $ True
        C.PBCons _ _ -> punsafeConstant . PLC.someValue @Bool $ False
      C.PBCons x' xs' -> pmatch ys $ \case
        C.PBNil -> punsafeConstant . PLC.someValue @Bool $ False
        C.PBCons y' ys' ->
          punsafeCase (peq # x' # y')
            . NEVector.cons (toSomeTerm . punsafeConstant . PLC.someValue @Bool $ False)
            . NEVector.singleton
            . toSomeTerm
            $ self # xs' # ys'

-- | @since 2.0.0
instance (PEq a, PEq b) => PEq (C.PBPair a b) where
  peq = plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \(C.PBPair x1 x2) ->
      pmatch ys $ \(C.PBPair y1 y2) ->
        pif
          (peq # x1 # y1)
          (peq # x2 # y2)
          pfalse

-- | @since 2.0.0
(#==) ::
  forall (a :: S -> Type) (s :: S).
  (PEq a) =>
  Term s a -> Term s a -> Term s PBool
x #== y = peq # x # y

-- | @since 2.0.0
infix 4 #==

-- | @since 2.0.0
(#/=) ::
  forall (a :: S -> Type) (s :: S).
  (PEq a) =>
  Term s a -> Term s a -> Term s PBool
x #/= y = pnot (peq # x # y)

-- | @since 2.0.0
infix 4 #/=
