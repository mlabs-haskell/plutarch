{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Optics.Getter (view)
import Optics.Iso (Iso')
import Optics.Lens (Lens, Lens')
import Optics.Review (review)
import Optics.Setter (set)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Liftable (plift)
import Plutarch.Test.Poly (PA)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    forAllShrink,
    (===),
  )
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Type.Reflection (Typeable, typeRep)

-- Helpers

pisEven :: Term (PInteger :~> PBool)
pisEven = plam1 $ \i -> (BF.pmodInteger # i # pconstant 2) #== pconstant 0

ptoEnum :: Term (PBool :~> PNatural)
ptoEnum = plam1 $ \b -> pif b (pconstant 1) (pconstant 0)

pid :: forall (a :: Type). Term (a :~> a)
pid = plam1 id

typeName :: forall (a :: Type). (Typeable a) => String
typeName = show $ typeRep @a

data PRepeat (a :: Type) = PRepeat
  { pvalue :: Term a,
    pnumber :: Term PNatural
  }

deriveFor ''PRepeat MogensenScott AllInstances

makeFieldLabelsNoPrefix ''PRepeat

-- Main

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Optics" $
    [ testIso @PA pencoded,
      testPOverId @PA #pvalue,
      testPOverId @PA #pnumber,
      testPOverCompose @PInteger pisEven ptoEnum,
      testInTermLaws @PA @PA #pvalue,
      testInTermLaws @PA @PNatural #pnumber
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

-- Properties

testInTermLaws ::
  forall (s :: Type) (a :: Type).
  ( Typeable s,
    Typeable a,
    PLiftable s,
    PLiftable a,
    Arbitrary (AsHaskell s),
    Arbitrary (AsHaskell a),
    Show (AsHaskell s),
    Show (AsHaskell a),
    Eq (AsHaskell a),
    PEq s
  ) =>
  Lens (PRepeat s) (PRepeat s) (Term a) (Term a) ->
  TestTree
testInTermLaws ell =
  testGroup
    groupName
    [ testProperty "View-set" propGetPut,
      testProperty "Set-view" propPutGet,
      testProperty "Set-set" propPutPut
    ]
  where
    groupName :: String
    groupName = "Lens law consistency for inTerm for " <> typeName @(Lens (PRepeat s) (PRepeat s) (Term a) (Term a))
    promoted :: Lens (Term (PRepeat s)) (Term (PRepeat s)) (Term a) (Term a)
    promoted = inTerm ell
    propGetPut :: Property
    propGetPut = forAllShrink arbitrary shrink $ \(x, n) ->
      let input = pcon $ PRepeat {pvalue = pconstant @s x, pnumber = pconstant n}
       in plift (set promoted (view promoted input) input #== input)
    propPutGet :: Property
    propPutGet = forAllShrink arbitrary shrink $ \(x, y, n) ->
      let input = pcon $ PRepeat {pvalue = pconstant @s x, pnumber = pconstant n}
          modifier = pconstant @a y
       in plift (view promoted (set promoted modifier input)) === y
    propPutPut :: Property
    propPutPut = forAllShrink arbitrary shrink $ \(x, y, z, n) ->
      let input = pcon $ PRepeat {pvalue = pconstant @s x, pnumber = pconstant n}
          modifier1 = pconstant @a y
          modifier2 = pconstant @a z
       in plift ((set promoted modifier2 . set promoted modifier1 $ input) #== set promoted modifier2 input)

testIso ::
  forall (a :: Type) (b :: Type).
  ( Show (AsHaskell a),
    Arbitrary (AsHaskell a),
    Eq (AsHaskell a),
    Typeable b,
    Typeable a,
    PLiftable a
  ) =>
  Iso' (Term a) (Term b) ->
  TestTree
testIso o = testProperty testName . forAllShrink (arbitrary @(AsHaskell a)) shrink $ \x ->
  let x' = pconstant @a x
   in plift (review o . view o $ x') === x
  where
    testName :: String
    testName = "Iso laws for Iso' (" <> typeName @(Term a) <> ") (" <> typeName @(Term b) <> ")"

testPOverId ::
  forall (a :: Type) (b :: Type).
  ( Arbitrary (AsHaskell a),
    Show (AsHaskell a),
    Typeable a,
    Typeable b,
    PLiftable a,
    PCon (PRepeat a),
    PlutarchType b,
    PEq (PRepeat a)
  ) =>
  Lens' (PRepeat a) (Term b) ->
  TestTree
testPOverId ell = testProperty testName . forAllShrink (arbitrary @(AsHaskell a, Natural)) shrink $ \(x, n) ->
  let input = pcon $ PRepeat {pvalue = pconstant @a x, pnumber = pconstant n}
   in plift (input #== pover ell pid input)
  where
    testName :: String
    testName =
      "pover o pid = pid for Lens' ("
        <> typeName @(PRepeat a)
        <> ") ("
        <> typeName @(Term b)
        <> ")"

testPOverCompose ::
  forall (a :: Type) (b :: Type) (c :: Type).
  ( Arbitrary (AsHaskell a),
    Show (AsHaskell a),
    PCon (PRepeat a),
    PLiftable a,
    PlutarchType b,
    PEq c,
    Typeable a,
    Typeable b,
    Typeable c
  ) =>
  Term (a :~> b) ->
  Term (b :~> c) ->
  TestTree
testPOverCompose f g = testProperty testName . forAllShrink (arbitrary @(AsHaskell a, Natural)) shrink $ \(x, n) ->
  let input = pcon $ PRepeat {pvalue = pconstant @a x, pnumber = pconstant n}
   in plift (pover #pvalue (pcompose g f) input #== (pover #pvalue g . pover #pvalue f $ input))
  where
    testName :: String
    testName =
      "pover o (pcompose g f) = pover o g . pover o f for Lens ("
        <> typeName @(PRepeat a)
        <> ") ("
        <> typeName @(PRepeat c)
        <> ") ("
        <> typeName @(Term a)
        <> ") ("
        <> typeName @(Term c)
        <> ") by way of "
        <> typeName @(Term b)
