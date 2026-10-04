-- Needed because otherwise punsafeUncons is asymmetric with regard to pmatch
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Prelude.List
  ( pfoldr,
    pfoldl,
    plength,
    pindex,
    pfind,
    pcount,
    punsafeIndex,
    punsafeHead,
    punsafeTail,
    punsafeUncons,
    punsafeDrop,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Prelude.Maybe (PMaybe, pjust, pnothing)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil))
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.DerivedFun qualified as DF
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
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> S.PBool) ->
  Term s (PBList a) ->
  Term s (PMaybe a)
pfind p ell = go # ell
  where
    go :: Term s (PBList a :--> PMaybe a)
    go = pfix $ \self -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> pnothing
      PBCons x xs' ->
        pif
          (p # x)
          (pjust x)
          (self # xs')

-- | @since 2.0.0
pindex ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBList a) ->
  Term s S.PNatural ->
  Term s (PMaybe a)
pindex ell ix = go # pfundamental ix # ell
  where
    go :: Term s (S.PInteger :--> PBList a :--> PMaybe a)
    go = pfix $ \self -> plam' $ \currIx -> plam' $ \xs -> pmatch xs $ \case
      PBNil -> pnothing
      PBCons x xs' ->
        pif
          (ix #== pconstant 0)
          (pjust x)
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

-- | Retrieves the element at the given (zero-indexed) position in the 'PBList'
-- argument. This involves only two builtin calls, which makes this more
-- efficient than using a list crawl if you need an index higher than @0@.
--
-- = Note
--
-- This operation is unsafe: if given an index that is out of bounds, it will
-- error. We provide this operation for efficiency, but use it with care.
--
-- @since 2.0.0
punsafeIndex ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBList a) ->
  Term s S.PNatural ->
  Term s a
punsafeIndex ell i = BF.pheadList #$ BF.pdropList # i # ell

-- | Retrieves the first element of the argument. Faster than indexing or a
-- 'pmatch' if you don't need any other elements.
--
-- = Note
--
-- This operation is unsafe, as it will error on an empty argument.
--
-- @since 2.0.0
punsafeHead ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBList a) ->
  Term s a
punsafeHead ell = BF.pheadList # ell

-- | As @'punsafeDrop' 1@, but faster and less code. This operation is unsafe
-- for the same reason as 'punsafeDrop' is.
--
-- @since 2.0.0
punsafeTail ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBList a) ->
  Term s (PBList a)
punsafeTail ell = BF.ptailList # ell

-- | Get both the first element and all but the first element of the argument,
-- and pass it to the continuation given. Less code than a 'pmatch' if you know
-- that the argument is non-empty (and don't mind an error if it is).
--
-- @since 2.0.0
punsafeUncons ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (PBList a) ->
  Term s (a :--> PBList a :--> b) ->
  Term s b
punsafeUncons = DF.pheadTailList

-- | Discards the given number of items from the argument list and returns
-- whatever is left.
--
-- = Note
--
-- This operation is unsafe: if the number of items to discard is greater than
-- the length of the list argument, this will error.
--
-- @since 2.0.0
punsafeDrop ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s S.PNatural ->
  Term s (PBList a) ->
  Term s (PBList a)
punsafeDrop howMany ell = BF.pdropList # howMany # ell
