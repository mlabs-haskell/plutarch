{-# LANGUAGE DataKinds #-}

-- | @since 2.0.0
module Plutarch.Primitive.Con
  ( PCon (..),
    pcon,
    inTerm,
    pover,
  )
where

import Data.Kind (Type)
import Optics.Getter (view)
import Optics.Lens (Lens, lens)
import Optics.Optic (Is, Optic)
import Optics.ReadOnly (getting)
import Optics.Setter (A_Setter, over, set)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun (pmkCons, pmkPairData)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Liftable (AsPlutus, PLiftable (AsHaskell))
import Plutarch.Primitive.Match (PMatch, pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    (#),
  )
import Plutarch.Primitive.Singular (PAsData, (:~>))
import Plutarch.Primitive.Term (punsafeCoerce, punsafeConstant)
import PlutusCore qualified as PLC

-- | = Laws
--
-- 1. @'pmatch' ('pcon' x) f@ @=@ @f x@
-- 2. @'pmatch' x 'pcon'@ @=@ @x@
--
-- @since 2.0.0
class (PMatch a) => PCon (a :: Type) where
  pcon' :: a -> Term (PRepresentation a)

-- | @since 2.0.0
instance (PLiftable a) => PCon (C.PBList a) where
  pcon' = \case
    C.PBNil -> punsafeConstant . PLC.someValue @[AsPlutus (AsHaskell a)] $ []
    C.PBCons x xs -> pmkCons # pfundamental x # pfundamental xs

-- | @since 2.0.0
instance (PCanData a, PCanData b) => PCon (C.PBPair (PAsData a) (PAsData b)) where
  pcon' (C.PBPair x y) = pmkPairData # pfundamental x # pfundamental y

-- | @since 2.0.0
pcon ::
  forall (a :: Type).
  (PCon a) => a -> Term a
pcon = punsafeCoerce . pcon'

-- | \'Promotes\' a 'Lens' that \'starts with\' a non-'Term' but operates on
-- 'Term's to \'start with\' and \'end with\' a 'Term'. Useful when composing
-- labelled optics produced for composite Plutarch types expressed as records.
--
-- @since 2.0.0
inTerm ::
  forall (s :: Type) (t :: Type) (a :: Type) (b :: Type).
  (PlutarchType a, PMatch s, PCon t) =>
  Lens s t (Term a) (Term b) ->
  Lens (Term s) (Term t) (Term a) (Term b)
inTerm o =
  lens
    (\s -> pmatch s ((view . getting) o))
    (\s b -> pmatch s (pcon . set o b))

-- | As 'over', but for applying a Plutarch arrow instead of a Haskell function.
--
-- @since 2.0.0
pover ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (t :: Type) (a :: Type) (b :: Type).
  (Is k A_Setter, PMatch s, PCon t, PlutarchType a, PlutarchType b) =>
  Optic k is s t (Term a) (Term b) ->
  Term (a :~> b) ->
  Term s ->
  Term t
pover o f s = pmatch s (pcon . over o (f #))
