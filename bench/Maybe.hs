{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Maybe
  ( PSOPMaybe (..),
    PMSMaybe (..),
    PMaybeOld (..),
    PMaybeNew (..),
    pfindSOP,
    pfindMS,
    pfindCPS,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Unsafe (punsafeCase, punsafeSecondary, toSomeTerm)

data PSOPMaybe (a :: Type)
  = PSOPNothing
  | PSOPJust (Term a)

deriveFor ''PSOPMaybe SOP AllInstances

data PMSMaybe (a :: Type)
  = PMSNothing
  | PMSJust (Term a)

deriveFor ''PMSMaybe MogensenScott AllInstances

data PMaybeOld (a :: Type)
  = PNothingOld
  | PJustOld (Term (PAsData a))

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

data PMaybeNew (a :: Type)
  = PNothingNew
  | PJustNew (Term (PAsData a))

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

pfindSOP ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (a :~> PBool) ->
  Term (PBList a) ->
  Term (PSOPMaybe a)
pfindSOP p ell = go # ell
  where
    go :: Term (PBList a :~> PSOPMaybe a)
    go = pfix $ \self -> plam1 $ \xs -> pmatch xs $ \case
      PBNil -> pcon PSOPNothing
      PBCons x xs' -> pif (p # x) (pcon . PSOPJust $ x) (self # xs')

pfindMS ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (a :~> PBool) ->
  Term (PBList a) ->
  Term (PMSMaybe a)
pfindMS p ell = go # ell
  where
    go :: Term (PBList a :~> PMSMaybe a)
    go = pfix $ \self -> plam1 $ \xs -> pmatch xs $ \case
      PBNil -> pcon PMSNothing
      PBCons x xs' -> pif (p # x) (pcon . PMSJust $ x) (self # xs')

pfindCPS ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (a :~> PBool) ->
  Term (PBList a) ->
  Term b ->
  Term (a :~> b) ->
  Term b
pfindCPS p ell whenMissing whenFound = go # ell
  where
    go :: Term (PBList a :~> b)
    go = pfix $ \self -> plam1 $ \xs -> pmatch xs $ \case
      PBNil -> whenMissing
      PBCons x xs' ->
        pif
          (p # x)
          (whenFound # x)
          (self # xs')
