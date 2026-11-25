{-# LANGUAGE OverloadedStrings #-}

module Pretty (goldens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.UPLC qualified as UPLC
import Plutarch.Helpers.Pretty qualified as P
import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine),
    Pretty (pretty),
    defaultLayoutOptions,
    hardline,
    indent,
    layoutSmart,
  )
import Prettyprinter.Render.String (renderString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

goldens :: TestTree
goldens =
  testGroup
    "Pretty"
    [ releaseGolden EvaluateUPLC "[[(2, 3)]]" "Pretty Case 1" case1,
      releaseGolden
        EvaluateUPLC
        "\\f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)"
        "Pretty Case 2"
        case2,
      layoutTests,
      testCase "packed application and match differ only in display intent" $ do
        let terms = NEVector.singleton UPLC.uplcError
            packed = UPLC.uplcPackedApply UPLC.uplcError terms
            match = UPLC.uplcCase (UPLC.uplcConstr 0 (NEVector.toVector terms)) terms
        assertBool "display tags must participate in equality" (packed /= match)
        UPLC.eraseUPLC packed @?= UPLC.eraseUPLC match
    ]

layoutTests :: TestTree
layoutTests =
  testGroup
    "Printer layout"
    [ check
        "application fits exactly"
        8
        (app "f" ["xx"])
        "(f # xx)",
      check
        "one column narrower"
        7
        (app "f" ["xx"])
        "( f\n  # xx\n)",
      check
        "broken outer call preserves flat inner separators"
        12
        (app "f" [app "g" ["x"], "argument"])
        "( f\n  # (g # x)\n  # argument\n)",
      check
        "overlong head leaves opener alone"
        8
        (app "abcdefg" ["x"])
        "(\n  abcdefg\n  # x\n)",
      check
        "overlong argument does not displace a short head"
        7
        (app "f" ["argument"])
        "( f\n  # argument\n)",
      check
        "multiline head does not share opener"
        40
        (app ("a" <> hardline <> "b") ["x"])
        "(\n  a\n  b\n  # x\n)",
      check
        "indented closing paren aligns with opener"
        10
        (indent 4 (app "f" ["xx"]))
        "    ( f\n      # xx\n    )",
      check
        "unparenthesized application breaks without padding"
        8
        (P.appTemplate P.PrintDefault (NEVector.fromNonEmpty ("f" :| ["xx", "y"])))
        "f\n# xx\n# y",
      check
        "compact composition retains spaces"
        7
        composition
        "(f . g)",
      check
        "broken composition retains spaces"
        6
        composition
        "( f\n  . g\n)",
      check
        "small case shares opener even when its arms break"
        12
        smallCase
        "( case x\n    [ yes\n    , no\n    ]\n)",
      check
        "indented case aligns its parentheses"
        17
        (indent 4 smallCase)
        "    ( case x\n        [yes, no]\n    )",
      check
        "large multiline scrutinee gets its own block"
        40
        (P.caseTemplate P.PrintAtomic False ("a" <> hardline <> "b") ["k"])
        "(\n  case\n    a\n    b\n    [k]\n)",
      check
        "flat application preserves escaped newline"
        40
        newlineApp
        "(f # \"\\n\" :: String)",
      check
        "broken application preserves escaped newline"
        18
        newlineApp
        "( f\n  # \"\\n\" :: String\n)"
    ]
  where
    app :: Doc () -> [Doc ()] -> Doc ()
    app f xs = P.appTemplate P.PrintAtomic (NEVector.fromNonEmpty (f :| xs))

    composition :: Doc ()
    composition = P.composeTemplate P.PrintAtomic (NEVector.fromNonEmpty ("f" :| ["g"]))

    smallCase :: Doc ()
    smallCase = P.caseTemplate P.PrintAtomic True "x" ["yes", "no"]

    newlineApp :: Doc ()
    newlineApp = app "f" [pretty (pconstant @PString "\n")]

    check :: String -> Int -> Doc () -> String -> TestTree
    check label width doc expected =
      testCase label $
        renderString (layoutSmart (LayoutOptions (AvailablePerLine width 1)) doc) @?= expected

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
