module Additive (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Additive"
    [ releaseGolden EvaluateUPLC "PPositive padd" "Additive Case 1" (padd @PPositive),
      releaseGolden EvaluateUPLC "PNatural padd" "Additive Case 2" (padd @PNatural),
      releaseGolden EvaluateUPLC "PInteger padd" "Additive Case 3" (padd @PInteger),
      releaseGolden EvaluateUPLC "G1 padd" "Additive Case 4" (padd @PBLS12_381_G1_Element),
      releaseGolden EvaluateUPLC "G2 padd" "Additive Case 5" (padd @PBLS12_381_G2_Element),
      releaseGolden EvaluateUPLC "PNatural pzero" "Additive Case 6" (pzero @PNatural),
      releaseGolden EvaluateUPLC "PInteger pzero" "Additive Case 7" (pzero @PInteger),
      releaseGolden EvaluateUPLC "G1 pzero" "Additive Case 8" (pzero @PBLS12_381_G1_Element),
      releaseGolden EvaluateUPLC "G2 pzero" "Additive Case 9" (pzero @PBLS12_381_G2_Element),
      releaseGolden EvaluateUPLC "PInteger psubtract" "Additive Case 10" (psubtract @PInteger),
      releaseGolden EvaluateUPLC "G1 psubtract" "Additive Case 11" (psubtract @PBLS12_381_G1_Element),
      releaseGolden EvaluateUPLC "G2 psubtract" "Additive Case 12" (psubtract @PBLS12_381_G2_Element),
      releaseGolden EvaluateUPLC "PPositive pscalePositive" "Additive Case 13" (pscalePositive @PPositive),
      releaseGolden EvaluateUPLC "PNatural pscalePositive" "Additive Case 14" (pscalePositive @PNatural),
      releaseGolden EvaluateUPLC "PInteger pscalePositive" "Additive Case 15" (pscalePositive @PInteger),
      releaseGolden EvaluateUPLC "PInteger psum" "Additive Case 16" (psum @PInteger)
    ]
