module Plutarch.Primitive.Trace
  ( ptraceDebug,
    ptraceError,
  )
where

import Control.Monad.Reader (ask)
import Data.Kind (Type)
import Plutarch.Backend.Term
  ( S,
    Term (Term),
    TermEnv (TermEnv),
    TracingMode (DebugTracing, NoTracing),
    asRawTerm,
  )
import Plutarch.Primitive.BuiltinFun (ptrace)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PString)

-- | @since wip
ptraceDebug ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s PString ->
  Term s a ->
  Term s a
ptraceDebug msg t = Term $ do
  TermEnv mode _ <- ask
  asRawTerm $ case mode of
    DebugTracing -> ptrace # msg # t
    _ -> t

-- | @since wip
ptraceError ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s PString ->
  Term s a ->
  Term s a
ptraceError msg t = Term $ do
  TermEnv mode _ <- ask
  asRawTerm $ case mode of
    NoTracing -> t
    _ -> ptrace # msg # t
