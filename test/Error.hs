{-# LANGUAGE OverloadedLists #-}

module Error (goldens) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import Plutarch.Unsafe (punsafeCase, punsafeConstr, toSomeTerm)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Error"
    [ releaseGolden EvaluateUPLC "\\x y -> addInteger x error (addInteger x error)" "Error Case 1" case1,
      releaseGolden EvaluateUPLC "\\x -> constr 0 [x, error]" "Error Case 2" case2,
      releaseGolden EvaluateUPLC "\\x -> case error of [x]" "Error Case 3" case3
    ]

-- Cases

-- Case 1: \x -> addInteger error (addInteger x error)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam' $ \x -> paddInteger # perror #$ paddInteger # x # perror

-- Case 2: \x -> constr 0 [x, error]
case2 :: forall (a :: S -> Type) (b :: S -> Type) (s :: S). Term s (a :--> b)
case2 = plam' $ \x -> punsafeConstr 0 [toSomeTerm x, toSomeTerm perror]

-- Case 3: \x -> case error of [x]
case3 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case3 = plam' $ \x -> punsafeCase perror . NEVector.singleton . toSomeTerm $ x
