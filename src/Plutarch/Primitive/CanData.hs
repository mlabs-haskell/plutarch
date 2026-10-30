{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.CanData
  ( PCanData (..),
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (SomeTerm, Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun
  ( pbData,
    pconstrData,
    pfstPair,
    pheadList,
    piData,
    plistData,
    pmkCons,
    pmkPairData,
    pnilData,
    psndPair,
    punBData,
    punConstrData,
    punIData,
    punListData,
  )
import Plutarch.Primitive.Composite (PBList, PBPair (PBPair))
import Plutarch.Primitive.DerivedFun (pfalse, pif, ptrue)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    punsafeSecondary,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular
  ( PAsData,
    PBool,
    PByte,
    PByteString,
    PData,
    PInteger,
    PNatural,
    PPositive,
  )
import Plutarch.Primitive.Term
  ( plam',
    punsafeCase,
    punsafeCoerce,
  )

-- | @since 2.0.0
class (PlutarchType a) => PCanData (a :: Type) where
  pfromData ::
    Term (PAsData a) -> Term a
  default pfromData ::
    (PCanData (PRepresentation a)) =>
    Term (PAsData a) -> Term a
  pfromData = punsafeCoerce . pfromData @(PRepresentation a) . punsafeCoerce
  ptoData ::
    Term a -> Term (PAsData a)
  default ptoData ::
    (PCanData (PRepresentation a)) =>
    Term a -> Term (PAsData a)
  ptoData = punsafeCoerce . ptoData . pfundamental

-- | Booleans on chain are represented as @Constr 0 []@ for false, @Constr 1 []@
-- for true. Thus, the change between 'PBool' and 'PAsData PBool' is nontrivial.
--
-- @since 2.0.0
instance PCanData PBool where
  pfromData t = pmatch (punConstrData # pfundamental t) $ \(PBPair tag _) ->
    punsafeCase tag . NEVector.cons (toSomeTerm pfalse) . NEVector.singleton . toSomeTerm $ ptrue
  ptoData t = punsafeSecondary $ pconstrData # pif t (pconstant 0) (pconstant 1) # pnilData

-- | @since 2.0.0
instance PCanData PInteger where
  pfromData x = punIData # pfundamental x
  ptoData x = piData # x

-- | @since 2.0.0
instance PCanData PNatural

-- | @since 2.0.0
instance PCanData PPositive

-- | @since 2.0.0
instance PCanData PByte

-- | @since 2.0.0
instance PCanData PByteString where
  pfromData x = punBData # pfundamental x
  ptoData x = pbData # x

-- | @since 2.0.0
instance PCanData PData where
  pfromData = punsafeCoerce
  ptoData = punsafeCoerce

-- | @since 2.0.0
instance (PCanData a) => PCanData (PBList (PAsData a)) where
  pfromData x = punsafeCoerce (punListData # pfundamental x)
  ptoData x = punsafeCoerce (plistData @(PAsData a)) # x

-- | @since 2.0.0
instance PCanData (PBList PData) where
  pfromData x = punListData # pfundamental x
  ptoData x = punsafeCoerce plistData # x

-- | @since 2.0.0
instance (PCanData a, PCanData b) => PCanData (PBPair (PAsData a) (PAsData b)) where
  pfromData ::
    Term (PAsData (PBPair (PAsData a) (PAsData b))) ->
    Term (PBPair (PAsData a) (PAsData b))
  pfromData x =
    let asConstr = punConstrData # pfundamental x
        fields = psndPair # asConstr
     in punsafeCase fields handler
    where
      handler :: NonEmptyVector SomeTerm
      handler =
        let t = plam' $ \h1 -> plam' $ \rest ->
              pmkPairData # h1 #$ pheadList # rest
         in NEVector.singleton (toSomeTerm t)
  ptoData x =
    punsafeCoerce
      ( pconstrData
          # pconstant 0
          #$ pmkCons
          # pfundamental (pfstPair # x)
          #$ pmkCons
          # pfundamental (psndPair # x)
          # pnilData
      )

-- | @since 2.0.0
instance PCanData (PBPair PData PData) where
  pfromData ::
    Term (PAsData (PBPair PData PData)) ->
    Term (PBPair PData PData)
  pfromData x =
    let asConstr = punConstrData # pfundamental x
        fields = psndPair # asConstr
     in punsafeCase fields handler
    where
      handler :: NonEmptyVector SomeTerm
      handler =
        let t = plam' $ \h1 -> plam' $ \rest ->
              pmkPairData # h1 #$ pheadList # rest
         in NEVector.singleton (toSomeTerm t)
  ptoData x =
    punsafeCoerce
      ( pconstrData
          # pconstant 0
          #$ pmkCons
          # (pfstPair # x)
          #$ pmkCons
          # (psndPair # x)
          # pnilData
      )
