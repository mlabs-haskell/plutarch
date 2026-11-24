{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Primitive.Liftable (plift)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    NonZero (NonZero),
    forAllShrink,
  )
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Type.Reflection (Typeable, typeRep)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Numeric" $
    [ testAdditiveSemigroupLaws mkRatio,
      testAdditiveMonoidLaws mkRatio,
      testAdditiveGroupLaws mkRatio,
      testMultiplicativeSemigroupLaws mkRatio,
      testMultiplicativeMonoidLaws mkRatio,
      testRecipLaw mkNZRatio,
      testDoubleRecip mkNZRatio
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 400

-- Helpers

testRecipLaw ::
  ((NonZero Integer, Positive) -> Term PRational) ->
  TestTree
testRecipLaw f = testProperty "x #* precip x = pone"
  . forAllShrink arbitrary shrink
  $ \x ->
    let x' = f x
     in plift ((x' #* precip x') #== pone)

testDoubleRecip ::
  ((NonZero Integer, Positive) -> Term PRational) ->
  TestTree
testDoubleRecip f = testProperty "precip . precip = id"
  . forAllShrink arbitrary shrink
  $ \x ->
    let x' = f x
     in plift ((precip . precip $ x') #== x')

testAdditiveSemigroupLaws ::
  forall (a :: Type) (h :: Type).
  ( Typeable a,
    Arbitrary h,
    Show h,
    PAdditiveSemigroup a,
    PEq a
  ) =>
  (h -> Term a) ->
  TestTree
testAdditiveSemigroupLaws f =
  testGroup
    testName
    [ propComm,
      propAssoc,
      propScaleOne,
      propScaleSum,
      propScaleProd
    ]
  where
    testName :: String
    testName = "PAdditiveSemigroup laws for " <> typeName @a
    propComm :: TestTree
    propComm = testProperty "x #+ y = y #+ x" . forAllShrink arbitrary shrink $ \(x, y) ->
      let x' = f x
          y' = f y
       in plift ((x' #+ y') #== (y' #+ x'))
    propAssoc :: TestTree
    propAssoc = testProperty "x #+ (y #+ z) = (x #+ y) #+ z" . forAllShrink arbitrary shrink $ \(x, y, z) ->
      let x' = f x
          y' = f y
          z' = f z
       in plift ((x' #+ (y' #+ z')) #== ((x' #+ y') #+ z'))
    propScaleOne :: TestTree
    propScaleOne = testProperty "pscalePositive # pone # x = x" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((pscalePositive # pone # x') #== x')
    propScaleSum :: TestTree
    propScaleSum = testProperty "pscalePositive # (p1 #+ p2) # x = (pscalePositive # p1 # x) #+ (pscalePositive # p2 # x)"
      . forAllShrink arbitrary shrink
      $ \(x, p1, p2) ->
        let x' = f x
            p1' = pconstant p1
            p2' = pconstant p2
         in plift ((pscalePositive # (p1' #+ p2') # x') #== ((pscalePositive # p1' # x') #+ (pscalePositive # p2' # x')))
    propScaleProd :: TestTree
    propScaleProd = testProperty "pscalePositive # (p1 #* p2) # x = pscalePositive # p1 #$ pscalePositive # p2 # x"
      . forAllShrink arbitrary shrink
      $ \(x, p1, p2) ->
        let x' = f x
            p1' = pconstant p1
            p2' = pconstant p2
         in plift ((pscalePositive # (p1' #* p2') # x') #== (pscalePositive # p1' #$ pscalePositive # p2' # x'))

testMultiplicativeSemigroupLaws ::
  forall (a :: Type) (h :: Type).
  ( Typeable a,
    Arbitrary h,
    Show h,
    PMultiplicativeSemigroup a,
    PEq a
  ) =>
  (h -> Term a) ->
  TestTree
testMultiplicativeSemigroupLaws f =
  testGroup
    testName
    [ propAssoc,
      propPowOne
      -- These tests currently exhaust the system even with severe constraints
      -- propPowSum,
      -- propPowProd
    ]
  where
    testName :: String
    testName = "PMultiplicativeSemigroup laws for " <> typeName @a
    propAssoc :: TestTree
    propAssoc = testProperty "x #* (y #* z) = (x #* y) #* z" . forAllShrink arbitrary shrink $ \(x, y, z) ->
      let x' = f x
          y' = f y
          z' = f z
       in plift ((x' #* (y' #* z')) #== ((x' #* y') #* z'))
    propPowOne :: TestTree
    propPowOne = testProperty "ppowPositive # pone # x = x" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((ppowPositive # pone # x') #== x')

{-
propPowSum :: TestTree
propPowSum = testProperty "ppowPositive # (p1 #+ p2) # x = (ppowPositive # p1 # x) #* (ppowPositive # p2 # x)"
  . forAllShrink arbitrary shrink
  $ \(x, p1, p2) ->
    let x' = f x
        p1' = pconstant p1
        p2' = pconstant p2
     in plift ((ppowPositive # (p1' #+ p2') # x') #== ((ppowPositive # p1' # x') #* (ppowPositive # p2' # x')))
propPowProd :: TestTree
propPowProd = testProperty "ppowPositive # (p1 #* p2) # x = ppowPositive # p1 #$ ppowPositive # p2 # x"
  . forAllShrink arbitrary shrink
  $ \(x, p1, p2) ->
    let x' = f x
        p1' = pconstant p1
        p2' = pconstant p2
     in plift ((ppowPositive # (p1' #* p2') # x') #== (ppowPositive # p1' #$ ppowPositive # p2' # x'))
-}

testAdditiveMonoidLaws ::
  forall (a :: Type) (h :: Type).
  ( PAdditiveMonoid a,
    Typeable a,
    PEq a,
    Arbitrary h,
    Show h
  ) =>
  (h -> Term a) ->
  TestTree
testAdditiveMonoidLaws f =
  testGroup
    testName
    [ propIdentity,
      propScaleZero
    ]
  where
    testName :: String
    testName = "PAdditiveMonoid laws for " <> typeName @a
    propIdentity :: TestTree
    propIdentity = testProperty "x #+ pzero = x" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((x' #+ pzero) #== x')
    propScaleZero :: TestTree
    propScaleZero = testProperty "pscaleNatural # pzero # x #== pzero" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((pscaleNatural # pzero # x') #== pzero)

testMultiplicativeMonoidLaws ::
  forall (a :: Type) (h :: Type).
  ( PMultiplicativeMonoid a,
    Typeable a,
    PEq a,
    Arbitrary h,
    Show h
  ) =>
  (h -> Term a) ->
  TestTree
testMultiplicativeMonoidLaws f =
  testGroup
    testName
    [ propIdentityRight,
      propIdentityLeft,
      propPowZero
    ]
  where
    testName :: String
    testName = "PMultiplicativeMonoid laws for " <> typeName @a
    propIdentityRight :: TestTree
    propIdentityRight = testProperty "x #* pone = x" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((x' #* pone) #== x')
    propIdentityLeft :: TestTree
    propIdentityLeft = testProperty "pone #* x = x" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((pone #* x') #== x')
    propPowZero :: TestTree
    propPowZero = testProperty "ppowNatural # pzero # x #== pone" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((ppowNatural # pzero # x') #== pone)

testAdditiveGroupLaws ::
  forall (a :: Type) (h :: Type).
  ( PAdditiveGroup a,
    Typeable a,
    PEq a,
    Arbitrary h,
    Show h
  ) =>
  (h -> Term a) ->
  TestTree
testAdditiveGroupLaws f =
  testGroup
    testName
    [ propNegate,
      propSubtract,
      propScaleNegative
    ]
  where
    testName :: String
    testName = "PAdditiveGroup laws for " <> typeName @a
    propNegate :: TestTree
    propNegate = testProperty "x #+ (pnegate # x) = pzero" . forAllShrink arbitrary shrink $ \x ->
      let x' = f x
       in plift ((x' #+ (pnegate # x')) #== pzero)
    propSubtract :: TestTree
    propSubtract = testProperty "x #- y = x #+ (pnegate # y)" . forAllShrink arbitrary shrink $ \(x, y) ->
      let x' = f x
          y' = f y
       in plift ((x' #- y') #== (x' #+ (pnegate # y')))
    propScaleNegative :: TestTree
    propScaleNegative = testProperty "pscaleInteger # (pnegate # i) # x = pnegate #$ pscaleInteger # i # x"
      . forAllShrink arbitrary shrink
      $ \(i, x) ->
        let x' = f x
            i' = pconstant i
         in plift ((pscaleInteger # (pnegate # i') # x') #== (pnegate #$ pscaleInteger # i' # x'))

mkRatio :: (Integer, Positive) -> Term PRational
mkRatio (n, d) = prational (pconstant n) (pconstant d)

mkNZRatio :: (NonZero Integer, Positive) -> Term PRational
mkNZRatio (NonZero n, d) = prational (pconstant n) (pconstant d)

typeName :: forall (a :: Type). (Typeable a) => String
typeName = show (typeRep @a)
