{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Data.Kind (Type)
import Orphanage ()
import Plutarch.Prelude
import Plutarch.Test.Laws
  ( testPLiftableLaws,
    testPlutusRepresentableLaws,
  )
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)
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
      testLedger @PCredential
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Helpers

testLedger ::
  forall (a :: Type).
  ( PLiftable a,
    Typeable a,
    Typeable (AsHaskell a),
    Show (AsHaskell a),
    Arbitrary (AsHaskell a),
    Eq (AsHaskell a)
  ) =>
  TestTree
testLedger =
  testGroup
    (tyConName (typeRepTyCon @a typeRep))
    [ testPlutusRepresentableLaws @(AsHaskell a),
      testPLiftableLaws @a
    ]
