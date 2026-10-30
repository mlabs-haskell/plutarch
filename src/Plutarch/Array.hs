{-# LANGUAGE TemplateHaskell #-}

-- | = Note
--
-- This module should be imported qualified, as some of its identifiers will
-- clash with some Plutarch prelude operations.
--
-- @since 2.0.0
module Plutarch.Array
  ( -- * Type
    PPullArray (..),

    -- * Functions

    -- ** Introduction
    pfromArray,
    pfromByteString,
    pfromList,
    pgenerate,
    piota,

    -- ** Transformation
    pmap,
    pimap,
    pzipWith,
    pizipWith,

    -- ** Elimination
    plength,
    ptoList,
    ptoArray,
    pfoldr,
    pifoldr,
    pfoldl,
    pifoldl,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.DerivedFun (pif, pisNegative)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (PLiftable, pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType,
    pfundamental,
    punsafeSecondary,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular (PBArray, PNatural, (:~>))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pcompose, pfix, plam')
import Plutarch.TH.Strategy
  ( Strategy (MogensenScott),
    WhichInstances (NoEqDebug),
    deriveFor,
  )

-- | @since 2.0.0
data PPullArray (a :: Type)
  = PPullArray (Term PNatural) (Term (PNatural :~> a))

deriveFor ''PPullArray MogensenScott NoEqDebug

-- | @since 2.0.0
pfromArray ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PBArray a) -> Term (PPullArray a)
pfromArray t = pcon . PPullArray (BF.plengthOfArray # t) $ BF.pindexArray # t

-- | @since 2.0.0
pfromByteString ::
  Term S.PByteString -> Term (PPullArray S.PByte)
pfromByteString t =
  pcon . PPullArray (BF.plengthOfByteString # t) $
    BF.pindexByteString # t

-- | @since 2.0.0
pfromList ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (C.PBList a) -> Term (PPullArray a)
pfromList t = pfromArray (BF.plistToArray # t)

-- | @since 2.0.0
pgenerate ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term PNatural ->
  Term (PNatural :~> a) ->
  Term (PPullArray a)
pgenerate len = pcon . PPullArray len

-- | @since 2.0.0
piota ::
  Term PNatural -> Term (PPullArray PNatural)
piota len = pcon . PPullArray len . plam' $ id

-- | @since 2.0.0
ptoList ::
  forall (a :: Type).
  (PLiftable a) =>
  Term (PPullArray a) ->
  Term (C.PBList a)
ptoList = pfoldr (plam' $ \x -> plam' $ pcon . C.PBCons x) (pcon C.PBNil)

-- | @since 2.0.0
ptoArray ::
  forall (a :: Type).
  (PLiftable a) =>
  Term (PPullArray a) ->
  Term (PBArray a)
ptoArray t = BF.plistToArray # ptoList t

-- | @since 2.0.0
pfoldr ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (a :~> b :~> b) ->
  Term b ->
  Term (PPullArray a) ->
  Term b
pfoldr f acc t = pmatch t $ \(PPullArray len getIx) -> go (pfundamental len) getIx # pconstant 0
  where
    go ::
      Term S.PInteger ->
      Term (PNatural :~> a) ->
      Term (S.PInteger :~> b)
    go len getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (currIx #== len)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.paddInteger # currIx # pconstant 1
           in f # currE #$ self # nextIx
        )

-- | @since 2.0.0
pifoldr ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (PNatural :~> a :~> b :~> b) ->
  Term b ->
  Term (PPullArray a) ->
  Term b
pifoldr f acc t = pmatch t $ \(PPullArray len getIx) -> go (pfundamental len) getIx # pconstant 0
  where
    go ::
      Term S.PInteger ->
      Term (PNatural :~> a) ->
      Term (S.PInteger :~> b)
    go len getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (currIx #== len)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.paddInteger # currIx # pconstant 1
           in f # punsafeSecondary currIx # currE #$ self # nextIx
        )

-- | @since 2.0.0
pfoldl ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (b :~> a :~> b) ->
  Term b ->
  Term (PPullArray a) ->
  Term b
pfoldl f acc t = pmatch t $ \(PPullArray len getIx) ->
  let lastIx = BF.psubtractInteger # pfundamental len # pconstant 1
   in go getIx # lastIx
  where
    go ::
      Term (PNatural :~> a) ->
      Term (S.PInteger :~> b)
    go getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (pisNegative currIx)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.psubtractInteger # currIx # pconstant 1
           in f # (self # nextIx) # currE
        )

-- | @since 2.0.0
pifoldl ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (PNatural :~> b :~> a :~> b) ->
  Term b ->
  Term (PPullArray a) ->
  Term b
pifoldl f acc t = pmatch t $ \(PPullArray len getIx) ->
  let lastIx = BF.psubtractInteger # pfundamental len # pconstant 1
   in go getIx # lastIx
  where
    go ::
      Term (PNatural :~> a) ->
      Term (S.PInteger :~> b)
    go getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (pisNegative currIx)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.psubtractInteger # currIx # pconstant 1
           in f # punsafeSecondary currIx # (self # nextIx) # currE
        )

-- | @since 2.0.0
plength ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (PPullArray a) ->
  Term PNatural
plength t = pmatch t $ \(PPullArray len _) -> len

-- | @since 2.0.0
pmap ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (a :~> b) ->
  Term (PPullArray a) ->
  Term (PPullArray b)
pmap f t = pmatch t $ \(PPullArray len g) ->
  pcon . PPullArray len . pcompose f $ g

-- | @since 2.0.0
pimap ::
  forall (a :: Type) (b :: Type).
  (PlutarchType a, PlutarchType b) =>
  Term (PNatural :~> a :~> b) ->
  Term (PPullArray a) ->
  Term (PPullArray b)
pimap f t = pmatch t $ \(PPullArray len g) ->
  pcon . PPullArray len $ plam' $ \i -> f # i #$ g # i

-- | @since 2.0.0
pzipWith ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (PlutarchType a, PlutarchType b, PlutarchType c) =>
  Term (a :~> b :~> c) ->
  Term (PPullArray a) ->
  Term (PPullArray b) ->
  Term (PPullArray c)
pzipWith f t1 t2 = pmatch t1 $ \(PPullArray len1 g1) ->
  pmatch t2 $ \(PPullArray len2 g2) ->
    let len = pif (BF.plessThanEqualsInteger # pfundamental len1 # pfundamental len2) len1 len2
     in pcon . PPullArray len . plam' $ \i -> f # (g1 # i) # (g2 # i)

-- | @since 2.0.0
pizipWith ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (PlutarchType a, PlutarchType b, PlutarchType c) =>
  Term (PNatural :~> a :~> b :~> c) ->
  Term (PPullArray a) ->
  Term (PPullArray b) ->
  Term (PPullArray c)
pizipWith f t1 t2 = pmatch t1 $ \(PPullArray len1 g1) ->
  pmatch t2 $ \(PPullArray len2 g2) ->
    let len = pif (BF.plessThanEqualsInteger # pfundamental len1 # pfundamental len2) len1 len2
     in pcon . PPullArray len . plam' $ \i -> f # i # (g1 # i) # (g2 # i)
