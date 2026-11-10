{-# LANGUAGE AllowAmbiguousTypes #-}

-- | @since 2.0.0
module Plutarch.Test.Laws
  ( testPlutusRepresentableLaws,
    testPLiftableLaws,
  )
where

import Data.Kind (Type)
import Plutarch.Primitive.Liftable
  ( PLiftable (AsHaskell, plutToRepr, reprToPlut),
    PlutusRepresentable
      ( AsPlutus,
        haskToRepr,
        reprToHask
      ),
  )
import Test.QuickCheck (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    forAllShrink,
    property,
    testProperty,
    (===),
  )
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

-- | @since 2.0.0
testPlutusRepresentableLaws ::
  forall (a :: Type).
  ( PlutusRepresentable a,
    Typeable a,
    Show a,
    Arbitrary a,
    Eq a
  ) =>
  TestTree
testPlutusRepresentableLaws =
  testGroup
    ("PlutusRepresentable laws for " <> tyConName (typeRepTyCon @a typeRep))
    [ testProperty "reprToHask . haskToRepr = Right" prop1
    ]
  where
    prop1 :: Property
    prop1 = forAllShrink (arbitrary @a) shrink $ \x ->
      (reprToHask . haskToRepr $ x) === Right x

-- | @since 2.0.0
testPLiftableLaws ::
  forall (a :: Type).
  ( PLiftable a,
    Typeable a,
    Show (AsPlutus (AsHaskell a)),
    Arbitrary (AsPlutus (AsHaskell a)),
    Eq (AsPlutus (AsHaskell a))
  ) =>
  TestTree
testPLiftableLaws =
  testGroup
    ("PLiftable laws for " <> tyConName (typeRepTyCon @a typeRep))
    [ testProperty "plutToRepr . reprToPlut = Right" prop1
    ]
  where
    prop1 :: Property
    prop1 = forAllShrink (arbitrary @(AsPlutus (AsHaskell a))) shrink $ \x ->
      case plutToRepr @a . reprToPlut $ x of
        Left _ -> property False
        Right res -> res === x
