-- | An 'in-construction' abstract syntax tree for Plutarch. This is designed to
-- be easy to work with at the front end, specifically for tasks like
-- prettyprinting.
--
-- More specifically, this corresponds to the @Structure@ described in the
-- /Hashing Modulo Alpha-Equivalence/ paper. Specifically, variables are all
-- anonymous in this representation, and any variables bound are tracked by
-- means of 'PosTree's.
--
-- = Links
--
-- - [The original paper](https://arxiv.org/pdf/2105.02856)
--
-- @since 2.0.0
module Plutarch.Backend.RawTerm
  ( VarTag (..),
    RawTerm (..),
    getRawTermAnn,
  )
where

import Data.Hashable
  ( Hashable (hash, hashWithSalt),
    defaultHashWithSalt,
  )
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Word (Word64)
import Plutarch.Backend.PosTree (PosTree)
import Plutarch.Backend.UPLC (UPLCTerm)
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

-- | Represents how a variable is used. The options are:
--
-- - As an argument to a lambda;
-- - As the name of a @let@ binding;
-- - As a recursive \'self\' argument in a fixpoint functional.
--
-- These are only for prettyprinting and code analysis.
--
-- @since 2.0.0
data VarTag
  = Argument
  | LetBinding
  | Self
  deriving stock
    ( -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Hashable VarTag where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINEABLE hash #-}
  hash =
    hash @Int . \case
      Argument -> 0
      LetBinding -> 1
      Self -> 2

-- | Plutarch-specific version of the @Structure@ definition from the paper
-- _Hashing Modulo Alpha-Equivalence_. We provide the ability to annotate these.
--
-- @since 2.0.0
data RawTerm (ann :: Type)
  = -- | A variable of some kind.
    --
    --    @since 2.0.0
    RVar ann VarTag
  | -- | A (possibly curried) lambda.
    --
    --    @since
    RLamAbs ann (Maybe PosTree) (RawTerm ann)
  | -- | An (possibly curried) application.
    --
    --    @since 2.0.0
    RApply ann (RawTerm ann) (RawTerm ann)
  | -- | A UPLC @force@.
    --
    --    @since 2.0.0
    RForce ann (RawTerm ann)
  | -- | A UPLC @delay@.
    --
    --    @since 2.0.0
    RDelay ann (RawTerm ann)
  | -- | A constant of something from the Plutus default universe.
    --
    --    @since 2.0.0
    RConstant ann (Some (ValueOf PLC.DefaultUni))
  | -- | A Plutus Core builtin.
    --
    --    @since 2.0.0
    RBuiltin ann PLC.DefaultFun
  | -- | A chunk of opaque, already-compiled code. This is
    --    essentially ignored by most analysis and optimization, and exists
    --    mainly to support precompilation for testing.
    --
    --    @since 2.0.0
    RCompiled ann UPLCTerm
  | -- | The canonical UPLC @error@.
    --
    --    @since 2.0.0
    RError ann
  | -- | A placeholder, used exclusively for record encodings.
    --
    --    @since 2.0.0
    RPlaceholder ann Integer
  | -- | A UPLC @constr@.
    --
    --    @since 2.0.0
    RConstr ann Word64 (Vector (RawTerm ann))
  | -- | A UPLC @case@.
    --
    --    @since 2.0.0
    RCase ann (RawTerm ann) (NonEmptyVector (RawTerm ann))
  | -- | An explicit @let@ binding. These do not directly translate to UPLC,
    --    but exist to aid prettyprinting and code analysis.
    --
    --    @Structure@-ally speaking, this is a hybrid between a lambda and an
    --    application, both arity 1.
    --
    --    @since 2.0.0
    RLet ann (Maybe PosTree) (RawTerm ann) (RawTerm ann)
  | -- | An explicit fixpoint of a functional. This does not directly translate
    --    to UPLC, but exists to aid prettyprinting, code analysis and generation.
    --
    --    @Structure@-ally speaking, this is a hybrid between a lambda and an
    --    application, though the fixpoint combinator is implicit.
    --
    --    @since 2.0.0
    RFix ann PosTree (RawTerm ann)
  | -- | A composition of two or more things that can have an argument applied.
    --
    --    @since 2.0.0
    RCompose ann (NonEmptyVector (RawTerm ann))
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Functor
    )

-- | Get the annotation for any 'RawTerm'.
--
-- @since 2.0.0
getRawTermAnn ::
  forall (ann :: Type).
  RawTerm ann -> ann
getRawTermAnn = \case
  RVar x _ -> x
  RLamAbs x _ _ -> x
  RApply x _ _ -> x
  RForce x _ -> x
  RDelay x _ -> x
  RConstant x _ -> x
  RBuiltin x _ -> x
  RCompiled x _ -> x
  RError x -> x
  RPlaceholder x _ -> x
  RConstr x _ _ -> x
  RCase x _ _ -> x
  RLet x _ _ _ -> x
  RFix x _ _ -> x
  RCompose x _ -> x
