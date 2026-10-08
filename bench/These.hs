module These
  ( PThese (..),
    PTheseNow (..),
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Unsafe
  ( punsafeCase,
    punsafeSecondary,
    punsafeUncons,
    toSomeTerm,
  )

data PThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThis (Term s (PAsData a))
  | PThat (Term s (PAsData b))
  | PThese (Term s (PAsData a)) (Term s (PAsData b))

instance (PlutarchType a, PlutarchType b) => PlutarchType (PThese a b) where
  type PRepresentation (PThese a b) = PData

instance (PlutarchType a, PlutarchType b) => PMatch (PThese a b) where
  pmatch' t f =
    punsafeCase t
      . NEVector.cons (toSomeTerm $ plam' $ \fields -> f (PThis . punsafeSecondary $ BF.pheadList # fields))
      . NEVector.cons (toSomeTerm $ plam' $ \fields -> f (PThat . punsafeSecondary $ BF.pheadList # fields))
      . NEVector.singleton
      . toSomeTerm
      $ plam'
      $ \fields -> punsafeUncons fields $ plam' $ \f1 -> plam' $ \t1 ->
        f (PThese (punsafeSecondary f1) . punsafeSecondary $ BF.pheadList # t1)

instance (PlutarchType a, PlutarchType b) => PCon (PThese a b) where
  pcon' = \case
    PThis x -> BF.pconstrData # pconstant 0 # (pcon . PBCons (pfundamental x) $ pcon PBNil)
    PThat y -> BF.pconstrData # pconstant 1 # (pcon . PBCons (pfundamental y) $ pcon PBNil)
    PThese x y -> BF.pconstrData # pconstant 2 # (pcon . PBCons (pfundamental x) . pcon . PBCons (pfundamental y) $ pcon PBNil)

data PTheseNow (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisNow (Term s (PAsData a))
  | PThatNow (Term s (PAsData b))
  | PTheseNow (Term s (PAsData a)) (Term s (PAsData b))

instance (PlutarchType a, PlutarchType b) => PlutarchType (PTheseNow a b) where
  type PRepresentation (PTheseNow a b) = PData

instance (PlutarchType a, PlutarchType b) => PMatch (PTheseNow a b) where
  pmatch' t f = pmatch (BF.punConstrData # t) $ \(PBPair tag fields) ->
    punsafeUncons fields $ plam' $ \f1 -> plam' $ \t1 ->
      punsafeCase tag
        . NEVector.cons (toSomeTerm $ f (PThisNow . punsafeSecondary $ f1))
        . NEVector.cons (toSomeTerm $ f (PThatNow . punsafeSecondary $ f1))
        . NEVector.singleton
        . toSomeTerm
        . punsafeUncons t1
        $ plam'
        $ \f2 -> plam' $ \_ -> f (PTheseNow (punsafeSecondary f1) (punsafeSecondary f2))

instance (PlutarchType a, PlutarchType b) => PCon (PTheseNow a b) where
  pcon' = \case
    PThisNow x -> BF.pconstrData # pconstant 0 # (pcon . PBCons (pfundamental x) $ pcon PBNil)
    PThatNow y -> BF.pconstrData # pconstant 1 # (pcon . PBCons (pfundamental y) $ pcon PBNil)
    PTheseNow x y -> BF.pconstrData # pconstant 2 # (pcon . PBCons (pfundamental x) . pcon . PBCons (pfundamental y) $ pcon PBNil)
