-- | @since 2.0.0
module Plutarch.Unsafe
  ( -- * 'Term' wrapper
    BackendTerm.SomeTerm,
    BackendTerm.toSomeTerm,

    -- * Unsafe 'Term' construction
    PrimTerm.punsafeCase,
    PrimTerm.punsafeConstr,
    PrimTerm.punsafeCoerce,

    -- * Unsafe representation conversions
    Representation.punsafeSecondary,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Term qualified as PrimTerm
