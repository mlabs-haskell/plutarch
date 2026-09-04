-- | @since 2.0.0
module Plutarch.Unsafe
  ( -- * 'Term' wrapper
    BackendTerm.SomeTerm,
    BackendTerm.toSomeTerm,

    -- * Unsafe 'Term' construction
    PrimTerm.punsafeCase,
    PrimTerm.punsafeConstr,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.Term qualified as PrimTerm
