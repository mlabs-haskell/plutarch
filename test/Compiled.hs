module Compiled (goldens) where

import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import TH.Data (PTheseData)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Compiled"
    [ releaseGolden
        EvaluateUPLC
        "\\cond ifT ifF -> (compiled (\\cond' ifT' ifF' -> if cond' ifT' ifF') cond ifT ifF"
        "Compiled Case 1"
        case1,
      releaseGolden
        EvaluateUPLC
        "Builtin call over case"
        "Compiled Case 2"
        case2
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

case2 ::
  Term (PAsData (PTheseData PByteString PInteger))
case2 =
  let fields = pcon . PBCons (pfundamental $ BF.piData # pconstant 42) . pcon $ PBNil
   in pvalidateData (BF.pconstrData # pconstant 1 # fields)
