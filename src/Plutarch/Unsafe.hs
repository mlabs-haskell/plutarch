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

    -- * Unsafe 'PBList' operations
    List.punsafeHead,
    List.punsafeTail,
    List.punsafeUncons,
    List.punsafeIndex,
    List.punsafeDrop,

    -- * Unsafe 'PMaybe' operations
    Maybe.punsafeFromJust,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Prelude.List qualified as List
import Plutarch.Prelude.Maybe qualified as Maybe
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Term qualified as PrimTerm
