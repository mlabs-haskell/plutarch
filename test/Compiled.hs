module Compiled (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Compiled"
    [ releaseGolden
        EvaluateUPLC
        "\\cond ifT ifF -> (compiled (\\cond' ifT' ifF' -> if cond' ifT' ifF') cond ifT ifF"
        "Compiled Case 1"
        case1
    ]

-- Cases

-- Case 1: \cond ifT ifF -> (compiled (\cond' ifT' ifF' -> pif cond' ifT' ifF') cond ifT ifF
case1 ::
  Term (PBool :~> PInteger :~> PInteger :~> PInteger)
case1 = plam3 $ \cond ifT ifF -> pcompiled go # cond # ifT # ifF
  where
    go ::
      Term (PBool :~> PInteger :~> PInteger :~> PInteger)
    go = plam3 $ \cond' ifT' ifF' -> pif cond' ifT' ifF'
