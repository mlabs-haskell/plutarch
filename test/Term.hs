module Term (goldens) where

import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (Don'tEvaluateUPLC, EvaluateUPLC),
    releaseGolden,
  )
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Term"
    [ releaseGolden EvaluateUPLC "\\x -> (\\y -> y) ((\\z -> z) x)" "Term Case 1" case1,
      releaseGolden Don'tEvaluateUPLC "\\x -> force (delay x)" "Term Case 2" case2,
      releaseGolden Don'tEvaluateUPLC "(pif false 1 2) + (pif true 1 2)" "Term Case 3" case3,
      releaseGolden Don'tEvaluateUPLC "(pif false 1 2) + (pif true 1 2)" "Term Case 3 plet" case3Plet,
      releaseGolden EvaluateUPLC "pif a then (b c) else (d e)" "Term Case 4" case4,
      releaseGolden EvaluateUPLC "sum 0 .. 9" "Term Case 5" case5,
      releaseGolden Don'tEvaluateUPLC "parallel let" "Term Case 6" case6,
      releaseGolden Don'tEvaluateUPLC "serial let" "Term Case 7" case7
    ]

-- Cases

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam' $ \x -> plam' id # (plam' id # x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam' $ \x -> pforce (pdelay x)

case3 :: forall (s :: S). Term s PInteger
case3 =
  let f = plam' $ \x -> pif x (pconstant 1) (pconstant 2)
   in (f # ptrue) #+ (f # pfalse)

case3Plet :: forall (s :: S). Term s PInteger
case3Plet = plet (plam' $ \x -> pif x (pconstant 1) (pconstant 2)) $ \f ->
  (f # ptrue) #+ (f # pfalse)

case4 :: forall (s :: S). Term s (PBool :--> PInteger)
case4 =
  let ten = pconstant 10
      twenty = pconstant 20
   in plam' $ \b -> pif b (squares # ten # twenty) (sums # ten # twenty)
  where
    squares :: Term s (PInteger :--> PInteger :--> PInteger)
    squares = plam' $ \i -> plam' $ \j -> (square # i) #+ (square # j)
    sums :: Term s (PInteger :--> PInteger :--> PInteger)
    sums = plam' $ \i -> plam' $ \j -> (double # i) #+ (double # j)
    square :: Term s (PInteger :--> PInteger)
    square = plam' $ \x -> x #* x
    double :: Term s (PInteger :--> PInteger)
    double = plam' $ \x -> x #+ x

case5 :: forall (s :: S). Term s PInteger
case5 = go # pconstant 0
  where
    go :: Term s (PInteger :--> PInteger)
    go = pfix $ \self -> plam' $ \i ->
      pif
        (pequalsInteger # i # pconstant 10)
        (pconstant 0)
        (i #+ (self # (i #+ pconstant 1)))

case6 :: forall (s :: S). Term s (PInteger :--> PInteger)
case6 = plam' $ \x ->
  let squared = x #* x
      doubled = x #+ x
   in (doubled #+ squared) #+ (squared #+ doubled)

case7 :: forall (s :: S). Term s (PInteger :--> PInteger)
case7 = plam' $ \x ->
  let squareX = x #* x
      doubleSquareX = squareX #+ squareX
   in doubleSquareX #+ doubleSquareX
