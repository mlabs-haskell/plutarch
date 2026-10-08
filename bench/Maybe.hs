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

data PSOPMaybe (a :: S -> Type) (s :: S)
  = PSOPNothing
  | PSOPJust (Term s a)

deriveFor ''PSOPMaybe SOP AllInstances

data PMSMaybe (a :: S -> Type) (s :: S)
  = PMSNothing
  | PMSJust (Term s a)

deriveFor ''PMSMaybe MogensenScott AllInstances

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
      . NEVector.cons (toSomeTerm $ plam' $ \_ -> f PNothingNew)
      . NEVector.singleton
      . toSomeTerm
      $ plam'
      $ \fields -> f . PJustNew . punsafeSecondary $ BF.pheadList # fields

instance (PlutarchType a) => PCon (PMaybeNew a) where
  pcon' = \case
    PNothingNew -> BF.pconstrData # pconstant 0 # pcon PBNil
    PJustNew t -> BF.pconstrData # pconstant 1 # pcon (PBCons (pfundamental t) . pcon $ PBNil)

pfindSOP ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> PBool) ->
  Term s (PBList a) ->
  Term s (PSOPMaybe a)
pfindSOP p ell = go # ell
  where
    go :: Term s (PBList a :--> PSOPMaybe a)
    go = pfix $ \self -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> pcon PSOPNothing
      PBCons x xs' -> pif (p # x) (pcon . PSOPJust $ x) (self # xs')

pfindMS ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> PBool) ->
  Term s (PBList a) ->
  Term s (PMSMaybe a)
pfindMS p ell = go # ell
  where
    go :: Term s (PBList a :--> PMSMaybe a)
    go = pfix $ \self -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> pcon PMSNothing
      PBCons x xs' -> pif (p # x) (pcon . PMSJust $ x) (self # xs')

pfindCPS ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> PBool) ->
  Term s (PBList a) ->
  Term s b ->
  Term s (a :--> b) ->
  Term s b
pfindCPS p ell whenMissing whenFound = go # ell
  where
    go :: Term s (PBList a :--> b)
    go = pfix $ \self -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> whenMissing
      PBCons x xs' ->
        pif
          (p # x)
          (whenFound # x)
          (self # xs')
