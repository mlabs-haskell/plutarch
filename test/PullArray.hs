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
      releaseGolden EvaluateUPLC "pzipWith (\a1 a2 -> f (g1 a1) (g2 a2) x y" "Pull Arrays Case 4" case4
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

-- Helpers

psquare :: forall (s :: S). Term s (PInteger :--> PInteger)
psquare = plam' $ \x -> pmultiplyInteger # x # x

pinc :: forall (s :: S). Term s (PInteger :--> PInteger)
pinc = plam' $ \x -> paddInteger # x # pconstant 1

parr :: forall (s :: S). Term s (PPullArray PInteger)
parr = Array.pfromArray $ pconstant @(PBArray PInteger) [1, 2, 3]

parr2 :: forall (s :: S). Term s (PPullArray PInteger)
parr2 = Array.pfromArray $ pconstant @(PBArray PInteger) [4, 6, 8]
