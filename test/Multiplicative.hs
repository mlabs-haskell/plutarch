module Multiplicative (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Multiplicative"
    [ releaseGolden EvaluateUPLC "PPositive pmultiply" "Multiplicative Case 1" (pmultiply @PPositive),
      releaseGolden EvaluateUPLC "PNZInteger pmultiply" "Multiplicative Case 2" (pmultiply @PNZInteger),
      releaseGolden EvaluateUPLC "PNatural pmultiply" "Multiplicative Case 3" (pmultiply @PNatural),
      releaseGolden EvaluateUPLC "PInteger pmultiply" "Multiplicative Case 4" (pmultiply @PInteger),
      releaseGolden EvaluateUPLC "PPositive ppowPositive" "Multiplicative Case 5" (ppowPositive @PPositive),
      releaseGolden EvaluateUPLC "PNatural pproduct" "Multiplicative Case 6" (pproduct @PNatural)
    ]
