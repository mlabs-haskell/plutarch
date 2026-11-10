{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Data.Kind (Type)
import Orphanage ()
import Plutarch.Prelude
import Plutarch.Primitive.Liftable (plift)
import Plutarch.Test.Laws
  ( testPLiftableLaws,
    testPlutusRepresentableLaws,
  )
import PlutusTx.IsData.Class qualified as PTx
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    forAllShrink,
    (===),
  )
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Type.Reflection
  ( Typeable,
    tyConName,
    typeRep,
    typeRepTyCon,
  )

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Ledger" $
    [ testLedger @PScriptHash,
      testLedger @PPubKeyHash,
      testLedger @PCredential,
      testLedger @PAccountId
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

-- Helpers

testLedger ::
  forall (a :: Type).
  ( PLiftable a,
    Typeable a,
    Typeable (AsHaskell a),
    Show (AsHaskell a),
    Arbitrary (AsHaskell a),
    Eq (AsHaskell a),
    PValidateData a,
    PTx.ToData (AsHaskell a),
    PTx.FromData (AsHaskell a)
  ) =>
  TestTree
testLedger =
  testGroup
    (tyConName (typeRepTyCon @a typeRep))
    [ testPlutusRepresentableLaws @(AsHaskell a),
      testPLiftableLaws @a,
      testPValidateData @a
    ]

-- Checks that if we generate a Plinth ledger type and lift it, its `Data`
-- encoding will validate.
testPValidateData ::
  forall (a :: Type).
  ( PValidateData a,
    Show (AsHaskell a),
    Arbitrary (AsHaskell a),
    PTx.ToData (AsHaskell a),
    PTx.FromData (AsHaskell a),
    Eq (AsHaskell a)
  ) =>
  TestTree
testPValidateData = testProperty "Valid Plinth encodings validate with pvalidateData"
  . forAllShrink (arbitrary @(AsHaskell a)) shrink
  $ \x ->
    let input = pconstant @PData . PTx.toData $ x
        result = PTx.fromData (plift (pfundamental . pvalidateData @a $ input))
     in result === Just x
