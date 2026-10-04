{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Maybe
  ( PSOPMaybe (..),
    PMSMaybe (..),
    pfindSOP,
    pfindMS,
    pfindCPS,
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

newtype OurPMaybe (a :: S -> Type) (s :: S) = OurPMaybe (forall (r :: S -> Type). Term s r -> Term s (a :--> r) -> Term s r)

instance (PlutarchType a) => PlutarchType (OurPMaybe a) where
  type PRepresentation (OurPMaybe a) = OurPMaybe (PRepresentation a)

instance (PlutarchType a) => PMatch (OurPMaybe a) where
  pmatch' = _

data PSOPMaybe (a :: S -> Type) (s :: S)
  = PSOPNothing
  | PSOPJust (Term s a)

deriveFor ''PSOPMaybe SOP AllInstances

data PMSMaybe (a :: S -> Type) (s :: S)
  = PMSNothing
  | PMSJust (Term s a)

deriveFor ''PMSMaybe MogensenScott AllInstances

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
