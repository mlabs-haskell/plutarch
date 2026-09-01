module Plutarch.Primitive.Match
  ( PMatch (..),
    pmatch,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
  )

-- | = Laws
--
-- The laws are stated in terms of 'pmatch' for clarity. This is not an issue,
-- as they can be restated in terms of @pmatch'@, only with more syntactic
-- noise.
--
-- 1. @'pmatch' ('pmatch' x f) g@ @=@ @'pmatch' x (\x' -> 'pmatch' (f x) g)@
-- 2. @f '#' ('pmatch' x g)@ @=@ @'pmatch' x (\x' -> f # g x')@
--
-- @since 2.0.0
class (PlutarchType a) => PMatch a where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    (PlutarchType b) =>
    Term s (PRepresentation a) -> (a s -> Term s b) -> Term s b

-- | @since 2.0.0
pmatch ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PMatch a, PlutarchType b) =>
  Term s a -> (a s -> Term s b) -> Term s b
pmatch x = pmatch' (pfundamental x)
