module Plutarch.Prelude.List
  ( pfoldr,
    pfoldl,
    plength,
    pindex,
    pfind,
    pcount,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil))
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType,
    pfundamental,
    punsafeSecondary,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular ((:-->))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pfix, plam')

-- | @since 2.0.0
pcount ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> S.PBool) ->
  Term s (PBList a) ->
  Term s S.PNatural
pcount p =
  pfoldr
    ( plam' $ \x -> plam' $ \acc ->
        pif
          (p # x)
          (punsafeSecondary $ BF.paddInteger # pfundamental acc # pconstant 1)
          acc
    )
    (pconstant 0)

-- | @since 2.0.0
pfind ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> S.PBool) ->
  Term s (PBList a) ->
  Term s b ->
  Term s (a :--> b) ->
  Term s b
pfind p ell whenMissing whenFound = go # ell
  where
    go :: Term s (PBList a :--> b)
    go = pfix $ \self -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> whenMissing
      PBCons x xs' ->
        pif
          (p # x)
          (whenFound # x)
          (self # xs')

-- | @since 2.0.0
pindex ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (PBList a) ->
  Term s S.PNatural ->
  -- | Result when the index is out of bounds
  Term s b ->
  -- | What to do with the element at an in-bounds index
  Term s (a :--> b) ->
  Term s b
pindex ell ix whenMissing whenPresent = go # pfundamental ix # ell
  where
    go :: Term s (S.PInteger :--> PBList a :--> b)
    go = pfix $ \self -> plam' $ \currIx -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> whenMissing
      PBCons x xs' ->
        pif
          (ix #== pconstant 0)
          (whenPresent # x)
          (self # (BF.psubtractInteger # currIx # pconstant 1) # xs')

-- | @since 2.0.0
plength ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBList a) ->
  Term s S.PNatural
plength = punsafeSecondary . pfoldr (plam' $ \_ -> BF.paddInteger # pconstant 1) (pconstant 0)

-- | @since 2.0.0
pfoldr ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b :--> b) ->
  Term s b ->
  Term s (PBList a) ->
  Term s b
pfoldr f acc xs = go # xs
  where
    go :: Term s (PBList a :--> b)
    go = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
      PBNil -> acc
      PBCons h t -> f # h #$ self # t

-- | @since 2.0.0
pfoldl ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (b :--> a :--> b) ->
  Term s b ->
  Term s (PBList a) ->
  Term s b
pfoldl f acc xs = go # xs
  where
    go :: Term s (PBList a :--> b)
    go = pfix $ \self -> plam' $ \ell -> pmatch ell $ \case
      PBNil -> acc
      PBCons h t -> f # (self # t) # h
