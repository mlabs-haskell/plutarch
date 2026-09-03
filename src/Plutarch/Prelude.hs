module Plutarch.Prelude
  ( -- * DSL types
    BackendTerm.S,
    BackendTerm.Term,

    -- * DSL construction operations
    PrimTerm.plam',
    (Representation.#),
    (Representation.#$),
    PrimTerm.pforce,
    PrimTerm.pdelay,
    PrimTerm.plet,
    PrimTerm.pfix,

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

    -- ** Integer

    -- *** Type marker
    S.PInteger,

    -- *** Primitive operations
    BF.pequalsInteger,
    BF.paddInteger,
    BF.pmultiplyInteger,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, Term, toSomeTerm)
import Plutarch.Backend.Term qualified as BackendTerm
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Liftable qualified as Lift
import Plutarch.Primitive.Representation qualified as Representation
import Plutarch.Primitive.Singular (PBool)
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (punsafeCase, punsafeConstant)
import Plutarch.Primitive.Term qualified as PrimTerm
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
