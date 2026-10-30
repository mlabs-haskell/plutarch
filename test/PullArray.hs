{-# LANGUAGE OverloadedLists #-}

module PullArray (goldens) where

import Data.Kind (Type)
import Plutarch.Array (PPullArray)
import Plutarch.Array qualified as Array
import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (EvaluateUPLC),
    releaseGolden,
  )
import Plutarch.Unsafe (punsafeSecondary)
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
      releaseGolden EvaluateUPLC "pfoldr (\\z acc -> f (g z) acc) x y" "Pull Arrays Case 10" case10,
      releaseGolden EvaluateUPLC "pimap f x" "Pull Arrays Case 11" case11,
      releaseGolden EvaluateUPLC "pzipWith f (piota n) x" "Pull Arrays Case 12" case12,
      releaseGolden EvaluateUPLC "pzipWith3 in terms of pzipWith" "Pull Arrays Case 13" case13,
      releaseGolden EvaluateUPLC "pzipWith3 directly" "Pull Arrays Case 14" case14
    ]

-- Cases

case1 :: Term (PPullArray PInteger)
case1 = Array.pmap psquare . Array.pmap pinc $ parr

case2 :: Term (PPullArray PInteger)
case2 = Array.pmap (pcompose psquare pinc) parr

case3 :: Term (PPullArray PInteger)
case3 =
  Array.pzipWith
    padd
    (Array.pmap pinc parr)
    (Array.pmap psquare parr2)

case4 :: Term (PPullArray PInteger)
case4 =
  Array.pzipWith
    (plam2 $ \x1 x2 -> (pinc # x1) #+ (psquare # x2))
    parr
    parr2

case5 :: Term (PPullArray PInteger)
case5 = Array.pzipWith padd parr parr

case6 :: Term (PPullArray PInteger)
case6 = Array.pmap (plam1 $ \x -> x #+ x) parr

case7 :: Term (PPullArray PInteger)
case7 =
  Array.pzipWith
    padd
    (Array.pmap pinc parr)
    (Array.pmap pinc parr)

case8 :: Term (PPullArray PInteger)
case8 = Array.pmap (plam1 $ \x -> (pinc # x) #+ (pinc # x)) parr

case9 :: Term PInteger
case9 = Array.pfoldr padd (pconstant 5) . Array.pmap psquare $ parr

case10 :: Term PInteger
case10 = Array.pfoldr (plam2 $ \z acc -> (psquare # z) #+ acc) (pconstant 5) parr

case11 :: Term (PPullArray PInteger)
case11 = Array.pimap pscale parr

case12 :: Term (PPullArray PInteger)
case12 = Array.pzipWith pscale (Array.piota len) parr
  where
    len :: Term PNatural
    len = punsafeSecondary $ pconstant @PInteger 3

case13 :: Term (PPullArray PInteger)
case13 = pzipWith3Naive pfma parr parr2 parr3

case14 :: Term (PPullArray PInteger)
case14 = pzipWith3Fused pfma parr parr2 parr3

-- Helpers

pfma :: Term (PInteger :~> PInteger :~> PInteger :~> PInteger)
pfma = plam3 $ \x y z -> x #* (y #+ z)

pscale :: Term (PNatural :~> PInteger :~> PInteger)
pscale = plam2 $ \n i -> pfundamental n #* i

psquare :: Term (PInteger :~> PInteger)
psquare = plam1 $ \x -> x #* x

pinc :: Term (PInteger :~> PInteger)
pinc = plam1 $ \x -> x #+ pconstant 1

parr :: Term (PPullArray PInteger)
parr = Array.pfromArray $ pconstant @(PBArray PInteger) [1, 2, 3]

parr2 :: Term (PPullArray PInteger)
parr2 = Array.pfromArray $ pconstant @(PBArray PInteger) [4, 6, 8]

parr3 :: Term (PPullArray PInteger)
parr3 = Array.pfromArray $ pconstant @(PBArray PInteger) [3, 6, 9]

pzipWith3Naive ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (PlutarchType a, PlutarchType b, PlutarchType c, PlutarchType d) =>
  Term (a :~> b :~> c :~> d) ->
  Term (PPullArray a) ->
  Term (PPullArray b) ->
  Term (PPullArray c) ->
  Term (PPullArray d)
pzipWith3Naive f xs ys =
  Array.pzipWith (plam2 $ \f' z -> f' # z) (Array.pzipWith (plam2 $ \x y -> f # x # y) xs ys)

pzipWith3Fused ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (PlutarchType a, PlutarchType b, PlutarchType c, PlutarchType d) =>
  Term (a :~> b :~> c :~> d) ->
  Term (PPullArray a) ->
  Term (PPullArray b) ->
  Term (PPullArray c) ->
  Term (PPullArray d)
pzipWith3Fused f xs ys zs = pmatch xs $ \(Array.PPullArray len1 g1) ->
  pmatch ys $ \(Array.PPullArray len2 g2) ->
    pmatch zs $ \(Array.PPullArray len3 g3) ->
      let len1' = pfundamental len1
          len2' = pfundamental len2
          len3' = pfundamental len3
          min12 = pmin len1' len2'
          len = punsafeSecondary (pmin min12 len3')
       in pcon . Array.PPullArray len $ plam1 $ \i -> f # (g1 # i) # (g2 # i) # (g3 # i)
