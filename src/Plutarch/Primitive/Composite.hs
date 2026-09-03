-- | @since 2.0.0
module Plutarch.Primitive.Composite
  ( PBList (..),
    PBPair (..),
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)

-- | @since 2.0.0
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since 2.0.0
data PBPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBPair (Term s a) (Term s b)
