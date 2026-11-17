{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Data.Kind (Type)
import Optics.Getter (view)
import Optics.Iso (Iso')
import Optics.Review (review)
import Plutarch.Prelude
import Plutarch.Primitive.Liftable (plift)
import Plutarch.Test.Poly (PA)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAllShrink, (===))
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Optics" $
    [ testIso @PA pencoded
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

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

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a
