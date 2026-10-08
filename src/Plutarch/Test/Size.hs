{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | @since 2.0.0
module Plutarch.Test.Size
  ( -- * For 'Term'
    noLargerThan,

    -- * For 'UPLCTerm'
    noLargerThanUPLC,
  )
where

import Data.Bifunctor (first)
import Data.ByteString.Short qualified as Short
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import Plutarch.Backend.ANF (analyzeDeps, fromHashedAST, fullPipeline)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.RawTerm (RawTerm)
import Plutarch.Backend.Term (S, Term, releaseTermEnv)
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))
import Plutarch.Helpers.Compile (compileTerm)
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (serialiseUPLC)
import Test.Tasty (TestTree)
import Test.Tasty.Providers
  ( IsTest (run, testOptions),
    singleTest,
    testFailed,
    testPassed,
  )
import Type.Reflection (Typeable)
import UntypedPlutusCore qualified as UPLC

-- | Given a closed computation of interest, and another closed computation
-- whose size acts as an upper bound, check that the computation of interest is
-- bounded by said upper bound, and what margin remains (if any). If the
-- computation of interest overshoots, this test will fail, and indicate the
-- overshoot.
--
-- If either 'Term' fails to compile, this test will also fail; in that
-- situation, no sizes will be indicated (as this is not possible).
--
-- @since 2.0.0
noLargerThan ::
  forall (a :: S -> Type) (b :: S -> Type).
  (Typeable a, Typeable b) =>
  String ->
  -- | Computation of interest
  (forall (s :: S). Term s a) ->
  -- | Computation whose size is an upper bound
  (forall (s :: S). Term s b) ->
  TestTree
noLargerThan name t bound = singleTest name (CmpTermSize t bound)

-- | Similar to 'noLargerThan', but for 'UPLCTerm's instead. Prefer using the
-- 'Term' version whenever possible: 'noLargerThanUPLC' exists mostly for
-- compiler testing.
--
-- @since 2.0.0
noLargerThanUPLC ::
  String ->
  -- | Computation of interest
  UPLCTerm ->
  -- | Computation whose size is an upper bound
  UPLCTerm ->
  TestTree
noLargerThanUPLC name t bound = singleTest name (CmpUPLCSize t bound)

-- Helpers

data CmpTermSize :: Type where
  CmpTermSize ::
    (Typeable a, Typeable b) =>
    (forall (s :: S). Term s a) -> (forall (s :: S). Term s b) -> CmpTermSize
  CmpUPLCSize :: UPLCTerm -> UPLCTerm -> CmpTermSize

instance IsTest CmpTermSize where
  testOptions = Tagged []
  run _ cmpSize _ = pure $ case go of
    Left err -> testFailed err
    Right stats -> testPassed stats
    where
      go :: Either String String
      go = do
        (UPLCTerm uplcT, UPLCTerm uplcBound) <- case cmpSize of
          CmpTermSize t bound -> do
            t' <- first (\e -> "Term of interest failed to compile: " <> show e) . compileTerm releaseTermEnv $ t
            bound' <- first (\e -> "Bounding Term failed to compile" <> show e) . compileTerm releaseTermEnv $ bound
            pure (pipeline t', pipeline bound')
          CmpUPLCSize t bound -> pure (t, bound)
        -- Throw away names, as we can only serialise DeBruijn scripts
        uplcT' <- first (const "Unexpected error in Term of interest: please report this!") . toDB $ uplcT
        uplcBound' <- first (const "Unexpected error in bounding Term: please report this!") . toDB $ uplcBound
        let tSize = uplcSize uplcT'
        let boundSize = uplcSize uplcBound'
        let scored = score tSize boundSize
        if tSize <= boundSize
          then Right scored
          else Left scored

pipeline :: RawTerm () -> UPLCTerm
pipeline = toUPLCTerm True . fullPipeline . analyzeDeps . fromHashedAST . fromRawTerm

toDB ::
  UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun () ->
  Either UPLC.FreeVariableError (UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
toDB t = PLC.runQuoteT $ do
  deBruijnd <- UPLC.deBruijnTerm t
  pure . UPLC.termMapNames UPLC.unNameDeBruijn $ deBruijnd

uplcSize :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> Int
uplcSize = Short.length . serialiseUPLC . UPLC.Program () (UPLC.Version 1 1 0)

score :: Int -> Int -> String
score tSize boundSize =
  "Score:\nTerm: "
    <> show tSize
    <> " bytes\nBound: "
    <> show boundSize
    <> " bytes\nDelta: "
    <> show (boundSize - tSize)
    <> " bytes (negative means we overshot the bound)"
