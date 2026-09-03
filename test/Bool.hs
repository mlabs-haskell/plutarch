module Bool (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Bool"
    [ releaseGolden EvaluateUPLC "\\x y -> or (not x) y" "Bool Case 1" case1
    ]

-- Cases

-- Case 1: \x y -> por (pnot x) y
case1 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
case1 = plam' $ \x -> plam' $ \y -> por (pnot x) y
