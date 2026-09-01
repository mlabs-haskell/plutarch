module Plutarch.Backend.Evaluate
  ( EvalError (..),
    peval,
  )
where

import Control.Monad.Except (throwError)
import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term, TermError, releaseTermEnv)
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Helpers.Compile (termToUPLC)
import Plutarch.Helpers.Evaluate (evalUPLC, maxBudget)
import PlutusCore qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

-- | @since 2.0.0
data EvalError
  = ATermError TermError
  | AnExecError (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
peval ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either EvalError (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
peval t = case termToUPLC releaseTermEnv t of
  Left err -> throwError . ATermError $ err
  Right t' -> case evalUPLC maxBudget t' of
    (res, _, _) -> case res of
      Left err -> throwError . AnExecError $ err
      Right res' -> pure res'
