{-# LANGUAGE DefaultSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.CanData
  ( PCanData (..),
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, SomeTerm, Term, toSomeTerm)
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
    PInteger,
    PNZInteger,
    PNatural,
    PPositive,
  )
import Plutarch.Primitive.Term
  ( plam',
    punsafeCase,
    punsafeCoerce,
    punsafeConstant,
  )
import PlutusCore qualified as PLC

-- | @since 2.0.0
class (PlutarchType a) => PCanData (a :: S -> Type) where
  pfromData ::
    forall (s :: S).
    Term s (PAsData a) -> Term s a
  default pfromData ::
    forall (s :: S).
    (PCanData (PRepresentation a)) =>
    Term s (PAsData a) -> Term s a
  pfromData = punsafeCoerce . pfromData @(PRepresentation a) . punsafeCoerce
  ptoData ::
    forall (s :: S).
    Term s a -> Term s (PAsData a)
  default ptoData ::
    forall (s :: S).
    (PCanData (PRepresentation a)) =>
    Term s a -> Term s (PAsData a)
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
instance PCanData PNZInteger

-- | @since 2.0.0
instance PCanData PPositive

-- | @since 2.0.0
instance PCanData PByte

-- | @since 2.0.0
instance PCanData PByteString where
  pfromData x = punBData # pfundamental x
  ptoData x = pbData # x

-- | @since 2.0.0
instance (PCanData a) => PCanData (PBList (PAsData a)) where
  pfromData x = punsafeCoerce (punListData # pfundamental x)
  ptoData x = punsafeCoerce (plistData @(PAsData a)) # x

-- | @since 2.0.0
instance (PCanData a, PCanData b) => PCanData (PBPair (PAsData a) (PAsData b)) where
  pfromData ::
    forall (s :: S).
    Term s (PAsData (PBPair (PAsData a) (PAsData b))) ->
    Term s (PBPair (PAsData a) (PAsData b))
  pfromData x =
    let asConstr = punConstrData # pfundamental x
        fields = psndPair # asConstr
     in punsafeCase fields handlers
    where
      handlers :: NonEmptyVector (SomeTerm s)
      handlers =
        let t = plam' $ \h1 -> plam' $ \rest ->
              pmkPairData # h1 #$ pheadList # rest
         in NEVector.singleton (toSomeTerm t)
  ptoData x =
    punsafeCoerce
      (pconstrData # i0 #$ pmkCons # pfundamental (pfstPair # x) #$ pmkCons # pfundamental (psndPair # x) # pnilData)
    where
      i0 :: forall (s :: S). Term s PInteger
      i0 = punsafeConstant . PLC.someValue @Integer $ 0
