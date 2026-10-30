{-# LANGUAGE StandaloneKindSignatures #-}

-- | @since 2.0.0
module Plutarch.Primitive.Singular
  ( (:~>),
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

-- | @since 2.0.0
type (:~>) :: Type -> Type -> Type
data (:~>) a b

type role (:~>) representational representational

infixr 0 :~>

-- | @since 2.0.0
type PBool :: Type
data PBool

-- | @since 2.0.0
type PInteger :: Type
data PInteger

-- | @since 2.0.0
type PNatural :: Type
data PNatural

-- | @since 2.0.0
type PPositive :: Type
data PPositive

-- | @since 2.0.0
type PByte :: Type
data PByte

-- | @since 2.0.0
type PByteString :: Type
data PByteString

-- | @since 2.0.0
type PString :: Type
data PString

-- | @since 2.0.0
type PBLS12_381_G1_Element :: Type
data PBLS12_381_G1_Element

-- | @since 2.0.0
type PBLS12_381_G2_Element :: Type
data PBLS12_381_G2_Element

-- | @since 2.0.0
type PBLS12_381_MlResult :: Type
data PBLS12_381_MlResult

-- | @since 2.0.0
type PData :: Type
data PData

-- | @since 2.0.0
type PAsData :: Type -> Type
data PAsData a

type role PAsData nominal

-- | @since 2.0.0
type PBArray :: Type -> Type
data PBArray a

type role PBArray representational

-- | @since 2.0.0
type PBValue :: Type
data PBValue

-- | @since 2.0.0
type PSOP :: Type
data PSOP
