-- | @since 2.0.0
module Plutarch.Primitive.Con
  ( PCon (..),
    pcon,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.BuiltinFun (pmkCons)
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Liftable (AsPlutus, PLiftable (AsHaskell))
import Plutarch.Primitive.Match (PMatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    (#),
  )
import Plutarch.Primitive.Term (punsafeCoerce, punsafeConstant)
import PlutusCore qualified as PLC

-- | = Laws
--
-- 1. @'pmatch' ('pcon' x) f@ @=@ @f x@
-- 2. @'pmatch' x 'pcon'@ @=@ @x@
--
-- @since 2.0.0
class (PMatch a) => PCon (a :: S -> Type) where
  pcon' :: forall (s :: S). a s -> Term s (PRepresentation a)

-- | @since 2.0.0
instance (PLiftable a) => PCon (C.PBList a) where
  pcon' = \case
    C.PBNil -> punsafeConstant . PLC.someValue @[AsPlutus (AsHaskell a)] $ []
    C.PBCons x xs -> pmkCons # pfundamental x # pfundamental xs

-- | @since 2.0.0
pcon ::
  forall (a :: S -> Type) (s :: S).
  (PCon a) => a s -> Term s a
pcon = punsafeCoerce . pcon'
