{-# LANGUAGE OverloadedStrings #-}

module Trace (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), plutarchGoldenWith)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Trace"
    [ plutarchGoldenWith EvaluateUPLC noTraceEnv "10" "Trace Case 1 no trace" case1,
      plutarchGoldenWith EvaluateUPLC noTraceEnv "traceDebug \"foo\" 10" "Trace Case 2 no trace" case2,
      plutarchGoldenWith EvaluateUPLC noTraceEnv "traceError \"foo\" 10" "Trace Case 3 no trace" case3,
      plutarchGoldenWith EvaluateUPLC errorEnv "10" "Trace Case 1 error trace" case1,
      plutarchGoldenWith EvaluateUPLC errorEnv "traceDebug \"foo\" 10" "Trace Case 2 error trace" case2,
      plutarchGoldenWith EvaluateUPLC errorEnv "traceError \"foo\" 10" "Trace Case 3 error trace" case3,
      plutarchGoldenWith EvaluateUPLC debugEnv "10" "Trace Case 1 debug trace" case1,
      plutarchGoldenWith EvaluateUPLC debugEnv "traceDebug \"foo\" 10" "Trace Case 2 debug trace" case2,
      plutarchGoldenWith EvaluateUPLC debugEnv "traceError \"foo\" 10" "Trace Case 3 debug trace" case3
    ]

-- Cases

case1 :: Term PInteger
case1 = pconstant 10

case2 :: Term PInteger
case2 = ptraceDebug (pconstant @PString "foo") case1

case3 :: Term PInteger
case3 = ptraceError (pconstant @PString "foo") case1

-- Helpers

noTraceEnv :: TermEnv
noTraceEnv = TermEnv NoTracing OnlyInternal

errorEnv :: TermEnv
errorEnv = TermEnv ErrorTracing OnlyInternal

debugEnv :: TermEnv
debugEnv = TermEnv DebugTracing OnlyInternal
