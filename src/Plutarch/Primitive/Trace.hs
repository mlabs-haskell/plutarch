module Plutarch.Primitive.Trace
  ( ptraceDebug,
    ptraceError,
  )
where

import Control.Monad.Reader (ask)
import Data.Kind (Type)
import Plutarch.Backend.Term
  ( Term (Term),
    TermEnv (TermEnv),
    TracingMode (DebugTracing, NoTracing),
    asRawTerm,
  )
import Plutarch.Primitive.BuiltinFun (ptrace)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PString)

-- | @since wip
ptraceDebug ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term PString ->
  Term a ->
  Term a
ptraceDebug msg t = Term $ do
  TermEnv mode _ <- ask
  asRawTerm $ case mode of
    DebugTracing -> ptrace # msg # t
    _ -> t

-- | @since wip
ptraceError ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term PString ->
  Term a ->
  Term a
ptraceError msg t = Term $ do
  TermEnv mode _ <- ask
  asRawTerm $ case mode of
    NoTracing -> t
    _ -> ptrace # msg # t
