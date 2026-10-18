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
    BF.plessThanEqualsInteger,

    -- ** Natural

    -- *** Type marker
    S.PNatural,

    -- ** Positive

    -- *** Type marker
    S.PPositive,

    -- ** Non-zero integer

    -- *** Type marker
    S.PNZInteger,

    -- ** BLS12-381

    -- *** Type markers
    S.PBLS12_381_G1_Element,
    S.PBLS12_381_G2_Element,
    S.PBLS12_381_MlResult,

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

    -- *** Operations
    List.pindex,
    List.plength,
    List.pfind,
    List.pcount,
    List.pfoldr,
    List.pfoldl,

    -- ** Builtin pair

    -- *** Type marker
    C.PBPair (..),

    -- ** Builtin array

    -- *** Type marker
    S.PBArray,

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

    -- ** Maybe

    -- *** Type marker
    Maybe.PMaybe (..),

    -- *** Operations
    Maybe.pnothing,
    Maybe.pjust,
    Maybe.pisNothing,
    Maybe.pisJust,

    -- ** Equality
    Eq.PEq (..),
    (Eq.#==),
    (Eq.#/=),

    -- ** Numeric

    -- *** Additive
    Additive.PAdditiveSemigroup (..),
    Additive.PAdditiveMonoid (..),
    Additive.PAdditiveGroup (..),
    (Additive.#+),
    Additive.psum,
    (Additive.#-),

    -- *** Multiplicative
    Multiplicative.PMultiplicativeSemigroup (..),
    Multiplicative.PMultiplicativeMonoid (..),
    (Multiplicative.#*),
    Multiplicative.pproduct,

    -- * Derivations
    Strategy.Strategy (..),
    Strategy.WhichInstances (..),
    Strategy.deriveFor,
  )
where

import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Prelude.List qualified as List
import Plutarch.Prelude.Maybe qualified as Maybe
import Plutarch.Primitive.Additive qualified as Additive
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.CanData qualified as CanData
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Con qualified as Con
import Plutarch.Primitive.Debug qualified as Debug
import Plutarch.Primitive.DerivedFun qualified as DF
import Plutarch.Primitive.Eq qualified as Eq
import Plutarch.Primitive.Liftable qualified as Lift
import Plutarch.Primitive.Match qualified as Match
import Plutarch.Primitive.Multiplicative qualified as Multiplicative
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term qualified as PrimTerm
import Plutarch.Primitive.Trace qualified as Trace
import Plutarch.Primitive.Validate qualified as Validate
import Plutarch.TH.Strategy qualified as Strategy
