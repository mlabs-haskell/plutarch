-- Needed for 'safety constraints' on several functions.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Representation
  ( -- * Type class
    PlutarchType (..),

    -- * Functions
    (#),
    (#$),
    pfundamental,
    punsafeSecondary,
    punsafeTranslate,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Term (papp, punsafeCoerce)
import Plutarch.Primitive.Types qualified as T

-- | Designates that @a@ is a representative of something that can be onchain.
-- More precisely, @a@ corresponds to the result of some onchain computation,
-- and every inhabitant of @a@ has a sensible representation using some onchain
-- value.
--
-- TODO: Describe fundamental versus secondary types
--
-- = Laws
--
-- The laws are stated in terms of '#' and '#$' for brevity, but can be
-- rephrased equivalently using 'papply' instead, as both '#' and '#$' are
-- defined in terms of 'papply'.
--
-- 1. @f '#$' g '#' x@ @=@ @'pcompose' f g '#' x@
--
-- = Important note
--
-- When defining instances of 'PlutarchType', you almost never want to define
-- 'papply', as the default implementation works in 99% of cases. Normally, the
-- choice is merely between defining 'PRepresentation' or using the default.
--
-- The default assumes that you are defining a fundamental type. If you want to
-- define a secondary type, define the appropriate 'PRepresentation' associated
-- type manually.
--
-- @PRepresentation a = a@
--
-- @since 2.0.0
class
  (PlutarchType (PRepresentation a), PRepresentation a ~ PRepresentation (PRepresentation a)) =>
  PlutarchType (a :: S -> Type)
  where
  type PRepresentation a :: S -> Type
  type PRepresentation a = a
  papply ::
    forall (b :: S -> Type) (s :: S).
    (PlutarchType b) =>
    Term s (PRepresentation a :--> PRepresentation b) -> Term s a -> Term s b
  papply f x = punsafeSecondary (papp (punsafeCoerce f) (pfundamental x))

-- | @since 2.0.0
instance PlutarchType T.PBool

-- | @since 2.0.0
instance PlutarchType T.PInteger

-- | @since 2.0.0
instance PlutarchType T.PNatural where
  type PRepresentation T.PNatural = T.PInteger

-- | @since 2.0.0
instance PlutarchType T.PPositive where
  type PRepresentation T.PPositive = T.PInteger

-- | @since 2.0.0
instance PlutarchType T.PNZInteger where
  type PRepresentation T.PNZInteger = T.PInteger

-- | @since 2.0.0
instance PlutarchType T.PByte where
  type PRepresentation T.PByte = T.PInteger

-- | @since 2.0.0
instance PlutarchType T.PByteString

-- | @since 2.0.0
instance PlutarchType T.PString

-- | @since 2.0.0
instance PlutarchType T.PBLS12_381_G1_Element

-- | @since 2.0.0
instance PlutarchType T.PBLS12_381_G2_Element

-- | @since 2.0.0
instance PlutarchType T.PBLS12_381_MlResult

-- | @since 2.0.0
instance (PlutarchType a) => PlutarchType (T.PBArray a) where
  type PRepresentation (T.PBArray a) = T.PBArray (PRepresentation a)

-- | @since 2.0.0
instance PlutarchType T.PBValue

-- | @since 2.0.0
instance PlutarchType T.PSOP

-- | @since 2.0.0
instance PlutarchType T.PData

-- | @since 2.0.0
instance PlutarchType (T.PAsData a) where
  type PRepresentation (T.PAsData a) = T.PData

-- | Any Plutarch type can \'forget\' any additional structure it may have, and
-- revert to being its fundamental representation. This is safe, and has zero runtime cost.
--
-- @since 2.0.0
pfundamental ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) => Term s a -> Term s (PRepresentation a)
pfundamental = punsafeCoerce

-- | Given a fundamental representation of some type @a@, declare unconditionally
-- that it is a term of type @a@. This is not safe: @a@ may impose additional
-- structure beyond what its direct representation requires, which can break
-- Plutarch type system guarantees. It is zero runtime cost, and thus exists for
-- performance: check carefully that you aren't violating any structural
-- requirements of @a@!
--
-- @since 2.0.0
punsafeSecondary ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) => Term s (PRepresentation a) -> Term s a
punsafeSecondary = punsafeCoerce

-- | A combination of 'pfundamental' and 'punsafeSecondary'. Given two types @a@
-- and @b@, if their fundamental representations match, it is possible to
-- convert between them, essentially by re-tagging. This is unsafe for the same
-- reasons that 'punsafeSecondary' is, and should be used with the same caution.
--
-- Much like 'punsafeSecondary' and 'fundamental', 'punsafeTranslate' has zero
-- runtime cost.
--
-- @since 2.0.0
punsafeTranslate ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b, PRepresentation a ~ PRepresentation b) =>
  Term s a -> Term s b
punsafeTranslate = punsafeCoerce

-- | Infix synonym of 'papply', without the additional \'noise\' of
-- 'PRepresentation's on the Plutarch function being applied.
--
-- = Note
--
-- Prefer this to 'papp' whenever possible, as 'papp' is unable to check if the
-- application is sensible.
--
-- @since 2.0.0
(#) ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b) -> Term s a -> Term s b
f # x = papply (punsafeCoerce f) x

infixl 8 #

-- | As '#', but with lower precedence. This allows writing \'chains\' of
-- applications more easily. Instead of
--
-- @
-- f # x # (g # y # z)
-- @
--
-- you can instead write
--
-- @
-- f # x #$ g # x # y
-- @
--
-- in a similar way to what '$' enables for regular Haskell terms.
--
-- @since 2.0.0
(#$) ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b) -> Term s a -> Term s b
(#$) = (#)

infixr 0 #$
