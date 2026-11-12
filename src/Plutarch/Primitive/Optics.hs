{-# LANGUAGE DataKinds #-}

module Plutarch.Primitive.Optics
  ( pview,
    pviews,
    pover,
    pset,
  )
where

import Data.Kind (Type)
import Optics.Getter (A_Getter, view)
import Optics.Optic (Is, Optic, Optic')
import Optics.Setter (A_Setter, over, set)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Match (PMatch, pmatch)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular ((:~>))

-- | @since 2.0.0
pview ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (a :: Type).
  (Is k A_Getter, PlutarchType a, PMatch s) =>
  Optic' k is s (Term a) ->
  Term s ->
  Term a
pview o t = pmatch t $ view o

-- | @since 2.0.0
pviews ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (a :: Type) (r :: Type).
  (Is k A_Getter, PlutarchType a, PMatch s, PlutarchType r) =>
  Optic' k is s (Term a) ->
  Term (a :~> r) ->
  Term s ->
  Term r
pviews o f t = pmatch t $ \x -> f # view o x

-- | @since 2.0.0
pover ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (t :: Type) (a :: Type) (b :: Type).
  (Is k A_Setter, PMatch s, PlutarchType b, PlutarchType a, PlutarchType t) =>
  Optic k is s (Term t) (Term a) (Term b) ->
  Term (a :~> b) ->
  Term s ->
  Term t
pover o f t = pmatch t (over o (f #))

-- | @since 2.0.0
pset ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (t :: Type) (a :: Type) (b :: Type).
  (Is k A_Setter, PMatch s, PlutarchType t) =>
  Optic k is s (Term t) (Term a) (Term b) ->
  Term b ->
  Term s ->
  Term t
pset o x t = pmatch t (set o x)
