{-# LANGUAGE OverloadedLists #-}

module PullArray (goldens) where

import Plutarch.Array (PPullArray)
import Plutarch.Array qualified as Array
import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (EvaluateUPLC),
    releaseGolden,
  )
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Pull arrays"
    [ releaseGolden EvaluateUPLC "pmap f . pmap g" "Pull Arrays Case 1" case1,
      releaseGolden EvaluateUPLC "pmap (f . g)" "Pull Arrays Case 2" case2,
      releaseGolden EvaluateUPLC "pzipWith f (pmap g1 x) (pmap g2 y)" "Pull Arrays Case 3" case3,
      releaseGolden EvaluateUPLC "pzipWith (\\a1 a2 -> f (g1 a1) (g2 a2) x y" "Pull Arrays Case 4" case4,
      releaseGolden EvaluateUPLC "pzipWith f x x" "Pull Arrays Case 5" case5,
      releaseGolden EvaluateUPLC "pmap (\\y -> f y y) x" "Pull Arrays Case 6" case6,
      releaseGolden EvaluateUPLC "pzipWith f (pmap g x) (pmap g x)" "Pull Arrays Case 7" case7,
      releaseGolden EvaluateUPLC "pmap (\\y -> let z = g y in f z z) x" "Pull Arrays Case 8" case8,
      releaseGolden EvaluateUPLC "pfoldr f x (pmap g y)" "Pull Arrays Case 9" case9,
      releaseGolden EvaluateUPLC "pfoldr (\\z acc -> f (g z) acc) x y" "Pull Arrays Case 10" case10
    ]

-- Cases

case1 :: forall (s :: S). Term s (PPullArray PInteger)
case1 = Array.pmap psquare . Array.pmap pinc $ parr

case2 :: forall (s :: S). Term s (PPullArray PInteger)
case2 = Array.pmap (pcompose psquare pinc) parr

case3 :: forall (s :: S). Term s (PPullArray PInteger)
case3 =
  Array.pzipWith
    paddInteger
    (Array.pmap pinc parr)
    (Array.pmap psquare parr2)

case4 :: forall (s :: S). Term s (PPullArray PInteger)
case4 =
  Array.pzipWith
    (plam' $ \x1 -> plam' $ \x2 -> paddInteger # (pinc # x1) # (psquare # x2))
    parr
    parr2

case5 :: forall (s :: S). Term s (PPullArray PInteger)
case5 = Array.pzipWith paddInteger parr parr

case6 :: forall (s :: S). Term s (PPullArray PInteger)
case6 = Array.pmap (plam' $ \x -> paddInteger # x # x) parr

case7 :: forall (s :: S). Term s (PPullArray PInteger)
case7 =
  Array.pzipWith
    paddInteger
    (Array.pmap pinc parr)
    (Array.pmap pinc parr)

case8 :: forall (s :: S). Term s (PPullArray PInteger)
case8 = Array.pmap (plam' $ \x -> paddInteger # (pinc # x) # (pinc # x)) parr

case9 :: forall (s :: S). Term s PInteger
case9 = Array.pfoldr paddInteger (pconstant 5) . Array.pmap psquare $ parr

case10 :: forall (s :: S). Term s PInteger
case10 = Array.pfoldr (plam' $ \z -> plam' $ \acc -> paddInteger # (psquare # z) # acc) (pconstant 5) parr

-- Helpers

psquare :: forall (s :: S). Term s (PInteger :--> PInteger)
psquare = plam' $ \x -> pmultiplyInteger # x # x

pinc :: forall (s :: S). Term s (PInteger :--> PInteger)
pinc = plam' $ \x -> paddInteger # x # pconstant 1

parr :: forall (s :: S). Term s (PPullArray PInteger)
parr = Array.pfromArray $ pconstant @(PBArray PInteger) [1, 2, 3]

parr2 :: forall (s :: S). Term s (PPullArray PInteger)
parr2 = Array.pfromArray $ pconstant @(PBArray PInteger) [4, 6, 8]
