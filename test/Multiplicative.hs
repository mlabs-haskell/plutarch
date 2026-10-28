module Multiplicative (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Multiplicative"
    [ releaseGolden EvaluateUPLC "PPositive pmultiply" "Multiplicative Case 1" (pmultiply @PPositive),
      releaseGolden EvaluateUPLC "PNatural pmultiply" "Multiplicative Case 2" (pmultiply @PNatural),
      releaseGolden EvaluateUPLC "PInteger pmultiply" "Multiplicative Case 3" (pmultiply @PInteger),
      releaseGolden EvaluateUPLC "PPositive ppowPositive" "Multiplicative Case 4" (ppowPositive @PPositive),
      releaseGolden EvaluateUPLC "PNatural pproduct" "Multiplicative Case 5" (pproduct @PNatural)
    ]
