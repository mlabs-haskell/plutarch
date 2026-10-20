module Constr
  ( goldens,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), debugGolden)
import Plutarch.Unsafe
  ( punsafeCase,
    punsafeSecondary,
    punsafeUncons,
    toSomeTerm,
  )
import Test.Tasty (TestTree, testGroup)

data PMaybeOld (a :: S -> Type) (s :: S)
  = PNothingOld
  | PJustOld (Term s (PAsData a))

instance (PlutarchType a) => PlutarchType (PMaybeOld a) where
  type PRepresentation (PMaybeOld a) = PData

instance (PlutarchType a) => PMatch (PMaybeOld a) where
  pmatch' t f = pmatch (BF.punConstrData # t) $ \(PBPair tag fields) ->
    punsafeCase tag
      . NEVector.cons (toSomeTerm $ f PNothingOld)
      . NEVector.singleton
      . toSomeTerm
      $ f (PJustOld . punsafeSecondary $ BF.pheadList # fields)

instance (PlutarchType a) => PCon (PMaybeOld a) where
  pcon' = \case
    PNothingOld -> BF.pconstrData # pconstant 0 # pcon PBNil
    PJustOld t -> BF.pconstrData # pconstant 1 # pcon (PBCons (pfundamental t) . pcon $ PBNil)

data PMaybeNew (a :: S -> Type) (s :: S)
  = PNothingNew
  | PJustNew (Term s (PAsData a))

instance (PlutarchType a) => PlutarchType (PMaybeNew a) where
  type PRepresentation (PMaybeNew a) = PData

instance (PlutarchType a) => PMatch (PMaybeNew a) where
  pmatch' t f =
    punsafeCase t
      . NEVector.cons (toSomeTerm $ plam1 $ \_ -> f PNothingNew)
      . NEVector.singleton
      . toSomeTerm
      $ plam1
      $ \fields -> f . PJustNew . punsafeSecondary $ BF.pheadList # fields

instance (PlutarchType a) => PCon (PMaybeNew a) where
  pcon' = \case
    PNothingNew -> BF.pconstrData # pconstant 0 # pcon PBNil
    PJustNew t -> BF.pconstrData # pconstant 1 # pcon (PBCons (pfundamental t) . pcon $ PBNil)

data PTheseNew (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisNew (Term s (PAsData a))
  | PThatNew (Term s (PAsData b))
  | PTheseNew (Term s (PAsData a)) (Term s (PAsData b))

instance (PlutarchType a, PlutarchType b) => PlutarchType (PTheseNew a b) where
  type PRepresentation (PTheseNew a b) = PData

instance (PlutarchType a, PlutarchType b) => PMatch (PTheseNew a b) where
  pmatch' t f =
    punsafeCase t
      . NEVector.cons (toSomeTerm $ plam1 $ \fields -> f (PThisNew . punsafeSecondary $ BF.pheadList # fields))
      . NEVector.cons (toSomeTerm $ plam1 $ \fields -> f (PThatNew . punsafeSecondary $ BF.pheadList # fields))
      . NEVector.singleton
      . toSomeTerm
      $ plam1
      $ \fields -> punsafeUncons fields $ plam2 $ \f1 t1 ->
        f (PTheseNew (punsafeSecondary f1) . punsafeSecondary $ BF.pheadList # t1)

instance (PlutarchType a, PlutarchType b) => PCon (PTheseNew a b) where
  pcon' = \case
    PThisNew x -> BF.pconstrData # pconstant 0 # (pcon . PBCons (pfundamental x) $ pcon PBNil)
    PThatNew y -> BF.pconstrData # pconstant 1 # (pcon . PBCons (pfundamental y) $ pcon PBNil)
    PTheseNew x y -> BF.pconstrData # pconstant 2 # (pcon . PBCons (pfundamental x) . pcon . PBCons (pfundamental y) $ pcon PBNil)

data PTheseOld (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisOld (Term s (PAsData a))
  | PThatOld (Term s (PAsData b))
  | PTheseOld (Term s (PAsData a)) (Term s (PAsData b))

instance (PlutarchType a, PlutarchType b) => PlutarchType (PTheseOld a b) where
  type PRepresentation (PTheseOld a b) = PData

instance (PlutarchType a, PlutarchType b) => PMatch (PTheseOld a b) where
  pmatch' t f = pmatch (BF.punConstrData # t) $ \(PBPair tag fields) ->
    punsafeUncons fields $ plam2 $ \f1 t1 ->
      punsafeCase tag
        . NEVector.cons (toSomeTerm $ f (PThisOld . punsafeSecondary $ f1))
        . NEVector.cons (toSomeTerm $ f (PThatOld . punsafeSecondary $ f1))
        . NEVector.singleton
        . toSomeTerm
        . punsafeUncons t1
        $ plam2
        $ \f2 _ -> f (PTheseOld (punsafeSecondary f1) (punsafeSecondary f2))

instance (PlutarchType a, PlutarchType b) => PCon (PTheseOld a b) where
  pcon' = \case
    PThisOld x -> BF.pconstrData # pconstant 0 # (pcon . PBCons (pfundamental x) $ pcon PBNil)
    PThatOld y -> BF.pconstrData # pconstant 1 # (pcon . PBCons (pfundamental y) $ pcon PBNil)
    PTheseOld x y -> BF.pconstrData # pconstant 2 # (pcon . PBCons (pfundamental x) . pcon . PBCons (pfundamental y) $ pcon PBNil)

goldens :: TestTree
goldens =
  testGroup
    "Constr Data"
    [ debugGolden EvaluateUPLC "Old-style Data Maybe" "Constr Case 1" case1,
      debugGolden EvaluateUPLC "New-style Data Maybe" "Constr Case 2" case2,
      debugGolden EvaluateUPLC "Old-style These Maybe" "Constr Case 3" case3,
      debugGolden EvaluateUPLC "New-style These Maybe" "Constr Case 4" case4
    ]

case1 :: forall (s :: S). Term s PInteger
case1 = pmatch (pcon . PJustOld . ptoData . pconstant $ 2) $ \case
  PNothingOld -> pconstant (-1)
  PJustOld t -> pfromData t

case2 :: forall (s :: S). Term s PInteger
case2 = pmatch (pcon . PJustNew . ptoData . pconstant $ 2) $ \case
  PNothingNew -> pconstant (-1)
  PJustNew t -> pfromData t

case3 :: forall (s :: S). Term s PInteger
case3 = pmatch (pcon . PTheseOld (ptoData . pconstant $ 1) . ptoData . pconstant $ 2) $ \case
  PThisOld x -> pfromData x
  PThatOld y -> pfromData y
  PTheseOld x y -> pfromData x #+ pfromData y

case4 :: forall (s :: S). Term s PInteger
case4 = pmatch (pcon . PTheseNew (ptoData . pconstant $ 1) . ptoData . pconstant $ 2) $ \case
  PThisNew x -> pfromData x
  PThatNew y -> pfromData y
  PTheseNew x y -> pfromData x #+ pfromData y
