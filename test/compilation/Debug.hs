{-# LANGUAGE OverloadedStrings #-}

module Debug (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (EvaluateUPLC),
    debugGolden,
    releaseGolden,
  )
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Debug"
    [ debugGolden EvaluateUPLC "pshow PBool" "Debug Case 1" case1,
      releaseGolden EvaluateUPLC "pshow PBool" "Debug Case 1 tracing off" case1,
      debugGolden EvaluateUPLC "pshow PInteger" "Debug Case 2" case2,
      debugGolden EvaluateUPLC "pshow PByte" "Debug Case 3" case3,
      debugGolden EvaluateUPLC "pshow PByteString" "Debug Case 4" case4,
      debugGolden EvaluateUPLC "pshow [PInteger]" "Debug Case 5" case5,
      debugGolden EvaluateUPLC "pshow (PInteger, PByteString)" "Debug Case 6" case6
    ]

-- Cases

case1 :: Term PString
case1 = pshow ptrue

case2 :: Term PString
case2 = pshow . pconstant @PInteger $ -12345

case3 :: Term PString
case3 = pshow . pconstant @PByte $ 0x1F

case4 :: Term PString
case4 = pshow . pconstant @PByteString $ "cat goes nya"

case5 :: Term PString
case5 = pshow . pconstant @(PBList PInteger) $ [1, 2, 3, 4]

case6 :: Term PString
case6 = pshow . pconstant @(PBPair PInteger PByteString) $ (10, "nyaaa")
