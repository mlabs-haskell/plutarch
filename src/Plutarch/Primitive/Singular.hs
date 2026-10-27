-- | @since 2.0.0
module Plutarch.Primitive.Singular
  ( (:-->),
    PBool,
    PInteger,
    PNatural,
    PPositive,
    PByte,
    PByteString,
    PString,
    PBLS12_381_G1_Element,
    PBLS12_381_G2_Element,
    PBLS12_381_MlResult,
    PAsData,
    PData,
    PBArray,
    PBValue,
    PSOP,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S)

-- | @since 2.0.0
data (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)

type role (:-->) representational representational nominal

infixr 0 :-->

-- | @since 2.0.0
data PBool (s :: S)

type role PBool nominal

-- | @since 2.0.0
data PInteger (s :: S)

type role PInteger nominal

-- | @since 2.0.0
data PNatural (s :: S)

type role PNatural nominal

-- | @since 2.0.0
data PPositive (s :: S)

type role PPositive nominal

-- | @since 2.0.0
data PByte (s :: S)

type role PByte nominal

-- | @since 2.0.0
data PByteString (s :: S)

type role PByteString nominal

-- | @since 2.0.0
data PString (s :: S)

type role PString nominal

-- | @since 2.0.0
data PBLS12_381_G1_Element (s :: S)

type role PBLS12_381_G1_Element nominal

-- | @since 2.0.0
data PBLS12_381_G2_Element (s :: S)

type role PBLS12_381_G2_Element nominal

-- | @since 2.0.0
data PBLS12_381_MlResult (s :: S)

type role PBLS12_381_MlResult nominal

-- | @since 2.0.0
data PData (s :: S)

type role PData nominal

-- | @since 2.0.0
data PAsData (a :: S -> Type) (s :: S)

type role PAsData nominal nominal

-- | @since 2.0.0
data PBArray (a :: S -> Type) (s :: S)

type role PBArray representational nominal

-- | @since 2.0.0
data PBValue (s :: S)

type role PBValue nominal

-- | @since 2.0.0
data PSOP (s :: S)

type role PSOP nominal
