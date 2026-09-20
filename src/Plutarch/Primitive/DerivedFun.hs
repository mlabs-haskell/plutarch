-- | @since 2.0.0
module Plutarch.Primitive.DerivedFun
  ( -- * 'PBool'
    ptrue,
    pfalse,
    pif,
    pnot,
    por,
    pand,

    -- * Numeric
    pisNegative,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    (#),
  )
import Plutarch.Primitive.Singular (PBool, PInteger)
import Plutarch.Primitive.Term (punsafeCase, punsafeConstant)
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

-- | Checks if a given numeric type is negative. This is slightly more efficient
-- than a direct comparison to zero.
--
-- @since 2.0.0
pisNegative ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a, PRepresentation a ~ PInteger) =>
  Term s a ->
  Term s PBool
pisNegative t = BF.plessThanEqualsInteger # pfundamental t # pconstant (-1)
