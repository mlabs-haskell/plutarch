module Bool (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Plutarch.Test.Size (noLargerThan)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Bool"
    [ releaseGolden EvaluateUPLC "\\x y -> or (not x) y" "Bool Case 1" case1,
      releaseGolden EvaluateUPLC "a v (b v (c v d))" "Bool Case 2" case2,
      releaseGolden EvaluateUPLC "(a v b) v (c v d)" "Bool Case 3" case3,
      noLargerThan "a v (b v (c v d)) vs (a v b) v (c v d)" case2 case3,
      releaseGolden EvaluateUPLC "a ^ (b ^ (c ^ d))" "Bool Case 4" case4,
      releaseGolden EvaluateUPLC "(a ^ b) ^ (c ^ d)" "Bool Case 5" case5,
      noLargerThan "a ^ (b ^ (c ^ d)) vs (a ^ b) ^ (c ^ d)" case4 case5
    ]

-- Cases

-- Case 1: \x y -> por (pnot x) y
case1 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
case1 = plam' $ \x -> plam' $ \y -> por (pnot x) y

-- Case 2: \a b c d -> por a (por b (por c d))
case2 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool :--> PBool :--> PBool)
case2 = plam' $ \x1 -> plam' $ \x2 -> plam' $ \x3 -> plam' $ \x4 ->
  por x1 (por x2 (por x3 x4))

-- Case 3: \a b c d -> pand (pand a b) (pand c d)
case3 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool :--> PBool :--> PBool)
case3 = plam' $ \x1 -> plam' $ \x2 -> plam' $ \x3 -> plam' $ \x4 ->
  por (por x1 x2) (por x3 x4)

-- Case 4: \a b c d -> pand a (pand b (pand c d))
case4 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool :--> PBool :--> PBool)
case4 = plam' $ \x1 -> plam' $ \x2 -> plam' $ \x3 -> plam' $ \x4 ->
  pand x1 (pand x2 (pand x3 x4))

-- Case 5: \a b c d -> pand (pand a b) (pand c d)
case5 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool :--> PBool :--> PBool)
case5 = plam' $ \x1 -> plam' $ \x2 -> plam' $ \x3 -> plam' $ \x4 ->
  pand (pand x1 x2) (pand x3 x4)
