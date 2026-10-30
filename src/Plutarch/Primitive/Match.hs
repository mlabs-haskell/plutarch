module Plutarch.Primitive.Match
  ( PMatch (..),
    pmatch,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (SomeTerm, Term, toSomeTerm)
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil), PBPair (PBPair))
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
  )
import Plutarch.Primitive.Singular ((:~>))
import Plutarch.Primitive.Term (plam', punsafeCase)

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
    forall (b :: Type).
    (PlutarchType b) =>
    Term (PRepresentation a) -> (a -> Term b) -> Term b

-- | @since 2.0.0
instance (PlutarchType a) => PMatch (PBList a) where
  pmatch' ::
    forall (b :: Type).
    Term (PRepresentation (PBList a)) -> (PBList a -> Term b) -> Term b
  pmatch' x f = punsafeCase x handlers
    where
      handlers :: NonEmptyVector SomeTerm
      handlers = NEVector.cons (toSomeTerm whenCons) . NEVector.singleton . toSomeTerm $ whenNil
      whenNil :: Term b
      whenNil = f PBNil
      whenCons :: Term (a :~> PBList a :~> b)
      whenCons = plam' $ \h -> plam' $ \t -> f (PBCons h t)

-- | @since 2.0.0
instance (PlutarchType a, PlutarchType b) => PMatch (PBPair a b) where
  pmatch' p f = punsafeCase p . NEVector.singleton . toSomeTerm $ plam' $ \x -> plam' $ \y ->
    f (PBPair x y)

-- | @since 2.0.0
pmatch ::
  forall (a :: Type) (b :: Type).
  (PMatch a, PlutarchType b) =>
  Term a -> (a -> Term b) -> Term b
pmatch x = pmatch' (pfundamental x)
