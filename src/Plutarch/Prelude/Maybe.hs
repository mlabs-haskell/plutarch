{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Prelude.Maybe
  ( PMaybe (..),
    pnothing,
    pjust,
    pisNothing,
    pisJust,
    punsafeFromJust,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.BuiltinFun (pappendString)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pfalse, ptrue)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PBool)
import Plutarch.Primitive.Term (perror)
import Plutarch.TH.Strategy
  ( Strategy (SOP),
    WhichInstances (AllInstances),
    deriveFor,
  )

-- | @since 2.0.0
data PMaybe (a :: S -> Type) (s :: S)
  = PNothing
  | PJust (Term s a)

deriveFor ''PMaybe SOP AllInstances

-- | @since 2.0.0
pnothing ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PMaybe a)
pnothing = pcon PNothing

-- | @since 2.0.0
pjust ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s a ->
  Term s (PMaybe a)
pjust = pcon . PJust

-- | @since 2.0.0
pisNothing ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PMaybe a) ->
  Term s PBool
pisNothing t = pmatch t $ \case
  PNothing -> ptrue
  PJust _ -> pfalse

-- | @since 2.0.0
pisJust ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PMaybe a) ->
  Term s PBool
pisJust t = pmatch t $ \case
  PNothing -> pfalse
  PJust _ -> ptrue

-- | @since 2.0.0
punsafeFromJust ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PMaybe a) ->
  Term s a
punsafeFromJust t = pmatch t $ \case
  PNothing -> perror
  PJust x -> x
