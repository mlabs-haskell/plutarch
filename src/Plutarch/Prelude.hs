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

    -- * Basic operations
    Representation.PlutarchType (..),
    Representation.pfundamental,
    Match.PMatch (..),
    Match.pmatch,
    Con.PCon (..),
    Con.pcon,
    CanData.PCanData (..),
    Validate.PValidateData (..),
    Validate.pvalidateData,

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

    -- ** Byte

    -- *** Type marker
    S.PByte,

    -- ** Integer

    -- *** Type marker
    S.PInteger,

    -- *** Primitive operations
    BF.pequalsInteger,
    BF.paddInteger,
    BF.psubtractInteger,
    BF.pmultiplyInteger,

    -- ** ByteString

    -- *** Type marker
    S.PByteString,

    -- ** String

    -- *** Type marker
    S.PString,

    -- *** Primitive operations
    BF.pappendString,

    -- ** Builtin list

    -- *** Type marker
    C.PBList (..),

    -- ** Builtin pair

    -- *** Type marker
    C.PBPair (..),

    -- ** Tracing
    Trace.ptraceDebug,
    Trace.ptraceError,

    -- ** Debug
    Debug.PDebug (..),
    Debug.pshow,

    -- ** Data

    -- *** Type markers
    S.PData,
    S.PAsData,

    -- *** Primitive operations
    BF.piData,
    BF.plistData,

    -- ** Equality
    Eq.PEq (..),
    (Eq.#==),
    (Eq.#/=),

    -- * Derivations
    Strategy.Strategy (..),
    Strategy.WhichInstances (..),
    Strategy.deriveFor,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.CanData qualified as CanData
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Con qualified as Con
import Plutarch.Primitive.Debug qualified as Debug
import Plutarch.Primitive.DerivedFun qualified as DF
import Plutarch.Primitive.Eq qualified as Eq
import Plutarch.Primitive.Liftable qualified as Lift
import Plutarch.Primitive.Match qualified as Match
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term qualified as PrimTerm
import Plutarch.Primitive.Trace qualified as Trace
import Plutarch.Primitive.Validate qualified as Validate
import Plutarch.TH.Strategy qualified as Strategy
