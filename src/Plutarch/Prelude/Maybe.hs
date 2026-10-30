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
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun (pappendString)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pfalse, ptrue)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Ord (POrd (ple, plt))
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PBool)
import Plutarch.Primitive.Term (perror, plam')
import Plutarch.TH.Strategy
  ( Strategy (SOP),
    WhichInstances (AllInstances),
    deriveFor,
  )

-- | @since 2.0.0
data PMaybe (a :: Type)
  = PNothing
  | PJust (Term a)

deriveFor ''PMaybe SOP AllInstances

-- | @since 2.0.0
instance (POrd a) => POrd (PMaybe a) where
  plt = plam' $ \x -> plam' $ \y -> pmatch x $ \case
    PNothing -> pmatch y $ \case
      PNothing -> pfalse
      PJust _ -> ptrue
    PJust x' -> pmatch y $ \case
      PNothing -> pfalse
      PJust y' -> plt # x' # y'
  ple = plam' $ \x -> plam' $ \y -> pmatch x $ \case
    PNothing -> ptrue
    PJust x' -> pmatch y $ \case
      PNothing -> pfalse
      PJust y' -> ple # x' # y'

-- | @since 2.0.0
pnothing ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PMaybe a)
pnothing = pcon PNothing

-- | @since 2.0.0
pjust ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term a ->
  Term (PMaybe a)
pjust = pcon . PJust

-- | @since 2.0.0
pisNothing ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PMaybe a) ->
  Term PBool
pisNothing t = pmatch t $ \case
  PNothing -> ptrue
  PJust _ -> pfalse

-- | @since 2.0.0
pisJust ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PMaybe a) ->
  Term PBool
pisJust t = pmatch t $ \case
  PNothing -> pfalse
  PJust _ -> ptrue

-- | @since 2.0.0
punsafeFromJust ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PMaybe a) ->
  Term a
punsafeFromJust t = pmatch t $ \case
  PNothing -> perror
  PJust x -> x
