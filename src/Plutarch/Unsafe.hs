-- Needed because otherwise punsafeUncons is asymmetric with regard to pmatch
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | @since 2.0.0
module Plutarch.Unsafe
  ( -- * 'Term' wrapper
    BackendTerm.SomeTerm,
    BackendTerm.toSomeTerm,

    -- * Unsafe 'Term' construction
    PrimTerm.punsafeCase,
    PrimTerm.punsafeConstr,
    PrimTerm.punsafeCoerce,

    -- * Unsafe representation conversions
    Representation.punsafeSecondary,

    -- * Unsafe 'PBList' operations
    punsafeHead,
    punsafeTail,
    punsafeUncons,
    punsafeIndex,
    punsafeDrop,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite (PBList)
import Plutarch.Primitive.DerivedFun qualified as DF
import Plutarch.Primitive.Representation (PlutarchType, (#), (#$))
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular ((:-->))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term qualified as PrimTerm

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
