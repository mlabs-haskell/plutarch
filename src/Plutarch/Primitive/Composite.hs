-- | @since 2.0.0
module Plutarch.Primitive.Composite
  ( PBList (..),
    PBPair (..),
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)

-- | @since 2.0.0
data PBList (a :: Type)
  = PBNil
  | PBCons (Term a) (Term (PBList a))

-- | @since 2.0.0
data PBPair (a :: Type) (b :: Type)
  = PBPair (Term a) (Term b)
