module Pretty (goldens) where

import Data.Text qualified as Text
import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.String (renderString)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Pretty"
    [ releaseGolden EvaluateUPLC "[[(2, 3)]]" "Pretty Case 1" case1,
      releaseGolden
        EvaluateUPLC
        "\\f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)"
        "Pretty Case 2"
        case2
    ]

-- Cases

-- Case 1: [[2]] (for pretty printing)
case1 :: Term (PBList (PBList (PBPair PInteger PInteger)))
case1 = pconstant [[(2, 3)]]

-- Case 2: \f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)
case2 :: Term ((PBool :~> PBool) :~> PBool :~> PString)
case2 = plam2 $ \f x ->
  let lol = toPrettyString (f # x)
   in pconstant (Text.pack . show $ lol)

toPrettyString :: forall a. (Pretty a) => a -> String
toPrettyString = renderString . layoutSmart defaultLayoutOptions . pretty
