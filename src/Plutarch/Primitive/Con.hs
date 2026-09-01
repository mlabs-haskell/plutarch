-- | @since 2.0.0
module Plutarch.Primitive.Con
  ( PCon (..),
    pcon,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Match (PMatch)
import Plutarch.Primitive.Representation (PRepresentation)
import Plutarch.Primitive.Term (punsafeCoerce)

-- | = Laws
--
-- 1. @'pmatch' ('pcon' x) f@ @=@ @f x@
-- 2. @'pmatch' x 'pcon'@ @=@ @x@
--
-- @since 2.0.0
class (PMatch a) => PCon (a :: S -> Type) where
  pcon' :: forall (s :: S). a s -> Term s (PRepresentation a)

-- | @since 2.0.0
pcon ::
  forall (a :: S -> Type) (s :: S).
  (PCon a) => a s -> Term s a
pcon = punsafeCoerce . pcon'
