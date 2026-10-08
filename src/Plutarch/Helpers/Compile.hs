module Plutarch.Helpers.Compile
  ( compileTerm,
    termToUPLC,
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Plutarch.Backend.ANF (analyzeDeps, fromHashedAST, fullPipeline)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.RawTerm (RawTerm)
import Plutarch.Backend.Term
  ( OptimizationMode (InternalExternal, OnlyInternal),
    S,
    Term (Term),
    TermEnv (TermEnv),
    TermError,
  )
import Plutarch.Backend.UPLC (UPLCTerm)

-- | @since 2.0.0
compileTerm ::
  forall (a :: S -> Type).
  TermEnv ->
  (forall (s :: S). Term s a) ->
  Either TermError (RawTerm ())
compileTerm env (Term comp) = (\(x, _, _) -> fmap snd x) . runRWS (runExceptT comp) env $ 0

-- | @since 2.0.0
termToUPLC ::
  forall (a :: S -> Type).
  TermEnv ->
  (forall (s :: S). Term s a) ->
  Either TermError UPLCTerm
termToUPLC env@(TermEnv _ opt) t = do
  let optAsBool = case opt of OnlyInternal -> False; InternalExternal -> True
  toUPLCTerm optAsBool . fullPipeline . analyzeDeps . fromHashedAST . fromRawTerm <$> compileTerm env t
