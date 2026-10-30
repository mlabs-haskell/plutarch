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
import Plutarch.Backend.Term (PDelayed, Term)
import Plutarch.Primitive.Composite as C
import Plutarch.Primitive.Singular ((:~>))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (papp, punsafeCoerce)

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
  PlutarchType (a :: Type)
  where
  type PRepresentation a :: Type
  type PRepresentation a = a
  papply ::
    forall (b :: Type).
    (PlutarchType b) =>
    Term (PRepresentation a :~> PRepresentation b) -> Term a -> Term b
  papply f x = punsafeSecondary (papp (punsafeCoerce f) (pfundamental x))

-- | @since 2.0.0
instance (PlutarchType a, PlutarchType b) => PlutarchType (a :~> b) where
  type PRepresentation (a :~> b) = a :~> PRepresentation b

-- | @since 2.0.0
instance (PlutarchType a) => PlutarchType (PDelayed a) where
  type PRepresentation (PDelayed a) = PDelayed (PRepresentation a)

-- | @since 2.0.0
instance PlutarchType S.PBool

-- | @since 2.0.0
instance PlutarchType S.PInteger

-- | @since 2.0.0
instance PlutarchType S.PNatural where
  type PRepresentation S.PNatural = S.PInteger

-- | @since 2.0.0
instance PlutarchType S.PPositive where
  type PRepresentation S.PPositive = S.PInteger

-- | @since 2.0.0
instance PlutarchType S.PByte where
  type PRepresentation S.PByte = S.PInteger

-- | @since 2.0.0
instance PlutarchType S.PByteString

-- | @since 2.0.0
instance PlutarchType S.PString

-- | @since 2.0.0
instance PlutarchType S.PBLS12_381_G1_Element

-- | @since 2.0.0
instance PlutarchType S.PBLS12_381_G2_Element

-- | @since 2.0.0
instance PlutarchType S.PBLS12_381_MlResult

-- | @since 2.0.0
instance (PlutarchType a) => PlutarchType (S.PBArray a) where
  type PRepresentation (S.PBArray a) = S.PBArray (PRepresentation a)

-- | @since 2.0.0
instance PlutarchType S.PBValue

-- | @since 2.0.0
instance PlutarchType S.PSOP

-- | @since 2.0.0
instance PlutarchType S.PData

-- | @since 2.0.0
instance PlutarchType (S.PAsData a) where
  type PRepresentation (S.PAsData a) = S.PData

-- | @since 2.0.0
instance (PlutarchType a) => PlutarchType (C.PBList a) where
  type PRepresentation (C.PBList a) = C.PBList (PRepresentation a)

-- | @since 2.0.0
instance (PlutarchType a, PlutarchType b) => PlutarchType (C.PBPair a b) where
  type PRepresentation (C.PBPair a b) = C.PBPair (PRepresentation a) (PRepresentation b)

-- | Any Plutarch type can \'forget\' any additional structure it may have, and
-- revert to being its fundamental representation. This is safe, and has zero runtime cost.
--
-- @since 2.0.0
pfundamental ::
  forall (a :: Type).
  (PlutarchType a) => Term a -> Term (PRepresentation a)
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
  forall (a :: Type).
  (PlutarchType a) => Term (PRepresentation a) -> Term a
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
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b, PRepresentation a ~ PRepresentation b) =>
  Term a -> Term b
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
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (a :~> b) -> Term a -> Term b
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
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (a :~> b) -> Term a -> Term b
(#$) = (#)

infixr 0 #$
