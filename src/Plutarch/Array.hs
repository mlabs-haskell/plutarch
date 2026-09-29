{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Array
  ( -- * Type
    PPullArray (..),

    -- * Functions

    -- ** Introduction
    pfromArray,
    pfromByteString,
    pfromList,
    pgenerate,

    -- ** Transformation
    pmap,
    pzipWith,

    -- ** Elimination
    plength,
    ptoList,
    ptoArray,
    pfoldr,
    pfoldl,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
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
import Plutarch.Primitive.Singular (PBArray, PNatural, (:-->))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pcompose, pfix, plam')
import Plutarch.TH.Strategy
  ( Strategy (MogensenScott),
    WhichInstances (NoEqDebug),
    deriveFor,
  )

-- | @since 2.0.0
data PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (Term s PNatural) (Term s (PNatural :--> a))

deriveFor ''PPullArray MogensenScott NoEqDebug

-- | @since 2.0.0
pfromArray ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PBArray a) -> Term s (PPullArray a)
pfromArray t = pcon . PPullArray (BF.plengthOfArray # t) $ BF.pindexArray # t

-- | @since 2.0.0
pfromByteString ::
  forall (s :: S).
  Term s S.PByteString -> Term s (PPullArray S.PByte)
pfromByteString t =
  pcon . PPullArray (BF.plengthOfByteString # t) $
    BF.pindexByteString # t

-- | @since 2.0.0
pfromList ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (C.PBList a) -> Term s (PPullArray a)
pfromList t = pfromArray (BF.plistToArray # t)

-- | @since 2.0.0
pgenerate ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s PNatural ->
  Term s (PNatural :--> a) ->
  Term s (PPullArray a)
pgenerate len = pcon . PPullArray len

-- | @since 2.0.0
ptoList ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a) =>
  Term s (PPullArray a) ->
  Term s (C.PBList a)
ptoList = pfoldr (plam' $ \x -> plam' $ pcon . C.PBCons x) (pcon C.PBNil)

-- | @since 2.0.0
ptoArray ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a) =>
  Term s (PPullArray a) ->
  Term s (PBArray a)
ptoArray t = BF.plistToArray # ptoList t

-- | @since 2.0.0
pfoldr ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b :--> b) ->
  Term s b ->
  Term s (PPullArray a) ->
  Term s b
pfoldr f acc t = pmatch t $ \(PPullArray len getIx) -> go (pfundamental len) getIx # pconstant 0
  where
    go ::
      Term s S.PInteger ->
      Term s (PNatural :--> a) ->
      Term s (S.PInteger :--> b)
    go len getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (currIx #== len)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.paddInteger # currIx # pconstant 1
           in f # currE #$ self # nextIx
        )

-- | @since 2.0.0
pfoldl ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (b :--> a :--> b) ->
  Term s b ->
  Term s (PPullArray a) ->
  Term s b
pfoldl f acc t = pmatch t $ \(PPullArray len getIx) ->
  let lastIx = BF.psubtractInteger # pfundamental len # pconstant 1
   in go getIx # lastIx
  where
    go ::
      Term s (PNatural :--> a) ->
      Term s (S.PInteger :--> b)
    go getIx = pfix $ \self -> plam' $ \currIx ->
      pif
        (pisNegative currIx)
        acc
        ( let currE = getIx # punsafeSecondary currIx
              nextIx = BF.psubtractInteger # currIx # pconstant 1
           in f # (self # nextIx) # currE
        )

-- | @since 2.0.0
plength ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (PPullArray a) ->
  Term s PNatural
plength t = pmatch t $ \(PPullArray len _) -> len

-- | @since 2.0.0
pmap ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmap f t = pmatch t $ \(PPullArray len g) ->
  pcon . PPullArray len . pcompose f $ g

-- | @since 2.0.0
pzipWith ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b, PlutarchType c) =>
  Term s (a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pzipWith f t1 t2 = pmatch t1 $ \(PPullArray len1 g1) ->
  pmatch t2 $ \(PPullArray len2 g2) ->
    let len = pif (BF.plessThanEqualsInteger # pfundamental len1 # pfundamental len2) len1 len2
     in pcon . PPullArray len . plam' $ \i -> f # (g1 # i) # (g2 # i)
