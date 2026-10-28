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
      releaseGolden Don'tEvaluateUPLC "serial let" "Term Case 7" case7,
      releaseGolden Don'tEvaluateUPLC "two-let parallelizable" "Term Case 8" case8,
      releaseGolden Don'tEvaluateUPLC "two-let not parallelizable" "Term Case 9" case9
    ]

-- Cases

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam1 $ \x -> plam1 id # (plam1 id # x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam1 $ \x -> pforce (pdelay x)

case3 :: forall (s :: S). Term s PInteger
case3 =
  let f = plam1 $ \x -> pif x (pconstant 1) (pconstant 2)
   in (f # ptrue) #+ (f # pfalse)

case3Plet :: forall (s :: S). Term s PInteger
case3Plet = plet (plam1 $ \x -> pif x (pconstant 1) (pconstant 2)) $ \f ->
  (f # ptrue) #+ (f # pfalse)

case4 :: forall (s :: S). Term s (PBool :--> PInteger)
case4 =
  let ten = pconstant 10
      twenty = pconstant 20
   in plam1 $ \b -> pif b (squares # ten # twenty) (sums # ten # twenty)
  where
    squares :: Term s (PInteger :--> PInteger :--> PInteger)
    squares = plam2 $ \i j -> (square # i) #+ (square # j)
    sums :: Term s (PInteger :--> PInteger :--> PInteger)
    sums = plam2 $ \i j -> (double # i) #+ (double # j)
    square :: Term s (PInteger :--> PInteger)
    square = plam1 $ \x -> x #* x
    double :: Term s (PInteger :--> PInteger)
    double = plam1 $ \x -> x #+ x

case5 :: forall (s :: S). Term s PInteger
case5 = go # pconstant 0
  where
    go :: Term s (PInteger :--> PInteger)
    go = pfix $ \self -> plam1 $ \i ->
      pif
        (i #== pconstant 10)
        (pconstant 0)
        (i #+ (self # (i #+ pconstant 1)))

case6 :: forall (s :: S). Term s (PInteger :--> PInteger)
case6 = plam1 $ \x ->
  let squared = x #* x
      doubled = x #+ x
   in (doubled #+ squared) #+ (squared #+ doubled)

case7 :: forall (s :: S). Term s (PInteger :--> PInteger)
case7 = plam1 $ \x ->
  let squareX = x #* x
      doubleSquareX = squareX #+ squareX
   in doubleSquareX #+ doubleSquareX

case8 :: forall (s :: S). Term s (PInteger :--> PInteger)
case8 = plam1 $ \x -> plet (x #* x) $ \squared ->
  plet (x #+ x) $ \doubled ->
    (doubled #+ squared) #+ (squared #+ doubled)

case9 :: forall (s :: S). Term s (PInteger :--> PInteger)
case9 = plam1 $ \x -> plet (x #* x) $ \squareX ->
  plet (squareX #+ squareX) $ \doubleSquareX ->
    doubleSquareX #+ doubleSquareX
