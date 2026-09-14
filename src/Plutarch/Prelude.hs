module Plutarch.Prelude
  ( -- * DSL types
    BackendTerm.S,
    BackendTerm.Term,
    BackendTerm.TracingMode (..),
    BackendTerm.OptimizationMode (..),
    BackendTerm.TermEnv (..),

    -- * DSL construction operations
    PrimTerm.plam',
    (Representation.#),
    (Representation.#$),
    PrimTerm.pforce,
    PrimTerm.pdelay,
    PrimTerm.plet,
    PrimTerm.pfix,
    PrimTerm.perror,
    PrimTerm.pcompiled,
    PrimTerm.pcompose,

    -- * Lifting
    Lift.pconstant,

    -- * Target language

    -- ** Functions
    (S.:-->),

    -- ** Bool

    -- *** Type marker
    S.PBool,

    -- *** Canonical values
    DF.ptrue,
    DF.pfalse,

    -- *** Operations
    DF.pif,
    DF.por,
    DF.pand,
    DF.pnot,

    -- ** Integer

    -- *** Type marker
    S.PInteger,

    -- *** Primitive operations
    BF.pequalsInteger,
    BF.paddInteger,
    BF.psubtractInteger,
    BF.pmultiplyInteger,

    -- ** String

    -- *** Type marker
    S.PString,

    -- ** Builtin list

    -- *** Type marker
    C.PBList (..),

    -- ** Builtin pair

    -- *** Type marker
    C.PBPair (..),

    -- ** Tracing
    Trace.ptraceDebug,
    Trace.ptraceError,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun qualified as DF
import Plutarch.Primitive.Liftable qualified as Lift
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term qualified as PrimTerm
import Plutarch.Primitive.Trace qualified as Trace
