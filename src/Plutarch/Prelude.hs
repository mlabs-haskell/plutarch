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
    ptrue,
    pfalse,

    -- *** Operations
    pif,
    por,
    pand,
    pnot,

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

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, Term, toSomeTerm)
import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Liftable qualified as Lift
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular (PBool)
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (punsafeCase, punsafeConstant)
import Plutarch.Primitive.Term qualified as PrimTerm
import Plutarch.Primitive.Trace qualified as Trace
import PlutusCore qualified as PLC

-- | Canonical true 'Term'.
--
-- @since 2.0.0
ptrue :: forall (s :: S). Term s PBool
ptrue = punsafeConstant . PLC.someValue @Bool $ True

-- | Canonical false 'Term'.
--
-- @since 2.0.0
pfalse :: forall (s :: S). Term s PBool
pfalse = punsafeConstant . PLC.someValue @Bool $ False

-- | Lazy if-then-else: only the branch that gets taken will be evaluated.
--
-- @since 2.0.0
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif cond ifT ifF =
  let tAsSome = toSomeTerm ifT
      fAsSome = toSomeTerm ifF
      -- This is backwards in UPLC `case`s: false first.
      handlers = NEVector.cons fAsSome $ NEVector.singleton tAsSome
   in punsafeCase cond handlers

-- | Lazy inclusive or: if the first argument evaluates as true, second argument
-- will not evaluate.
--
-- @since 2.0.0
por ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
por x = pif x ptrue

-- | Lazy inclusive and: if the first argument evaluates as false, second
-- argument will not evaluate.
--
-- @since 2.0.0
pand ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pand x y = pif x y pfalse

-- | Logical negation.
--
-- @since 2.0.0
pnot ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool
pnot x = pif x pfalse ptrue
