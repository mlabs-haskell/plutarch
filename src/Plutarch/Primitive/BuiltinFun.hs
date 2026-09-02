-- Needed for multiple safety constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | @since 2.0.0
module Plutarch.Primitive.BuiltinFun
  ( -- * Integers
    paddInteger,
    psubtractInteger,
    pmultiplyInteger,
    pdivideInteger,
    pquotientInteger,
    premainderInteger,
    pmodInteger,
    pequalsInteger,
    plessThanEqualsInteger,
    plessThanInteger,
    pexpModInteger,

    -- * Bytestrings
    pappendByteString,
    pconsByteString,
    psliceByteString,
    plengthOfByteString,
    pindexByteString,
    pequalsByteString,
    plessThanByteString,
    plessThanEqualsByteString,
    preplicateByte,

    -- * Cryptography and hashing
    psha2_256,
    psha3_256,
    pblake2b_256,
    pkeccak_256,
    pblake2b_224,
    pripemd_160,
    pverifyEd25519Signature,
    pverifyEcdsaSecp256k1Signature,
    pverifySchnorrSecp256k1Signature,

    -- * Strings
    pappendString,
    pequalsString,
    pencodeUtf8,
    pdecodeUtf8,

    -- * Data
    pconstrData,
    pmapData,
    plistData,
    piData,
    pbData,
    punConstrData,
    punMapData,
    punListData,
    punIData,
    punBData,
    pequalsData,
    pserialiseData,
    pchooseData,

    -- * Miscellaneous monomorphized constructors
    pmkPairData,
    pnilData,

    -- * BLS12-381 operations

    -- ** @G1@
    pbls12_381_G1_add,
    pbls12_381_G1_neg,
    pbls12_381_G1_scalarMul,
    pbls12_381_G1_equal,
    pbls12_381_G1_hashToGroup,
    pbls12_381_G1_compress,
    pbls12_381_G1_uncompress,
    pbls12_381_G1_multiScalarMul,

    -- ** @G2@
    pbls12_381_G2_add,
    pbls12_381_G2_neg,
    pbls12_381_G2_scalarMul,
    pbls12_381_G2_equal,
    pbls12_381_G2_hashToGroup,
    pbls12_381_G2_compress,
    pbls12_381_G2_uncompress,
    pbls12_381_G2_multiScalarMul,

    -- ** Pairing
    pbls12_381_millerLoop,
    pbls12_381_mulMlResult,
    pbls12_381_finalVerify,

    -- * Conversions
    pintegerToByteString,
    pbyteStringToInteger,
    plistToArray,

    -- * Logical
    pandByteString,
    porByteString,
    pxorByteString,
    pcomplementByteString,
    preadBit,
    pwriteBits,

    -- * Bitwise
    pshiftByteString,
    protateByteString,
    pcountSetBits,
    pfindFirstSetBit,

    -- * Value
    pinsertCoin,
    plookupCoin,
    punionValue,
    pvalueContains,
    pvalueData,
    punValueData,
    pscaleValue,

    -- * Bool
    pifThenElse,

    -- * Tracing
    ptrace,

    -- * Pairs
    pfstPair,
    psndPair,

    -- * Lists
    pchooseList,
    pmkCons,
    pheadList,
    ptailList,
    pnullList,
    pdropList,

    -- * Arrays
    plengthOfArray,
    pindexArray,
  )
where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Composite (PBList, PBPair)
import Plutarch.Primitive.Representation (PRepresentation)
import Plutarch.Primitive.Singular
  ( PAsData,
    PBArray,
    PBLS12_381_G1_Element,
    PBLS12_381_G2_Element,
    PBLS12_381_MlResult,
    PBValue,
    PBool,
    PByte,
    PByteString,
    PData,
    PInteger,
    PNatural,
    PString,
    (:-->),
  )
import Plutarch.Primitive.Term (pforce, punsafeBuiltin, punsafeConstant)
import PlutusCore qualified as PLC
import PlutusCore.Data (Data)

-- | @since 2.0.0
paddInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
paddInteger = punsafeBuiltin PLC.AddInteger

-- | @since 2.0.0
psubtractInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
psubtractInteger = punsafeBuiltin PLC.SubtractInteger

-- | @since 2.0.0
pmultiplyInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

-- | @since 2.0.0
pdivideInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdivideInteger = punsafeBuiltin PLC.DivideInteger

-- | @since 2.0.0
pquotientInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquotientInteger = punsafeBuiltin PLC.QuotientInteger

-- | @since 2.0.0
premainderInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
premainderInteger = punsafeBuiltin PLC.RemainderInteger

-- | @since 2.0.0
pmodInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmodInteger = punsafeBuiltin PLC.ModInteger

-- | @since 2.0.0
pequalsInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pequalsInteger = punsafeBuiltin PLC.EqualsInteger

-- | @since 2.0.0
plessThanInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
plessThanInteger = punsafeBuiltin PLC.LessThanInteger

-- | @since 2.0.0
plessThanEqualsInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
plessThanEqualsInteger = punsafeBuiltin PLC.LessThanEqualsInteger

-- | @since 2.0.0
pexpModInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PNatural :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger

-- | @since 2.0.0
pappendByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pappendByteString = punsafeBuiltin PLC.AppendByteString

-- | @since 2.0.0
pconsByteString ::
  forall (s :: S).
  Term s (PByte :--> PByteString :--> PByteString)
pconsByteString = punsafeBuiltin PLC.ConsByteString

-- | @since 2.0.0
psliceByteString ::
  forall (s :: S).
  Term s (PNatural :--> PNatural :--> PByteString :--> PByteString)
psliceByteString = punsafeBuiltin PLC.SliceByteString

-- | @since 2.0.0
plengthOfByteString ::
  forall (s :: S).
  Term s (PByteString :--> PNatural)
plengthOfByteString = punsafeBuiltin PLC.LengthOfByteString

-- | @since 2.0.0
pindexByteString ::
  forall (s :: S).
  Term s (PByteString :--> PNatural :--> PByte)
pindexByteString = punsafeBuiltin PLC.IndexByteString

-- | @since 2.0.0
pequalsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pequalsByteString = punsafeBuiltin PLC.EqualsByteString

-- | @since 2.0.0
plessThanByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
plessThanByteString = punsafeBuiltin PLC.LessThanByteString

-- | @since 2.0.0
plessThanEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
plessThanEqualsByteString = punsafeBuiltin PLC.LessThanEqualsByteString

-- | @since 2.0.0
preplicateByte :: forall (s :: S). Term s (PNatural :--> PByte :--> PByteString)
preplicateByte = punsafeBuiltin PLC.ReplicateByte

-- | @since 2.0.0
psha2_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
psha2_256 = punsafeBuiltin PLC.Sha2_256

-- | @since 2.0.0
psha3_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
psha3_256 = punsafeBuiltin PLC.Sha3_256

-- | @since 2.0.0
pblake2b_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
pblake2b_256 = punsafeBuiltin PLC.Blake2b_256

-- | @since 2.0.0
pkeccak_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
pkeccak_256 = punsafeBuiltin PLC.Keccak_256

-- | @since 2.0.0
pblake2b_224 :: forall (s :: S). Term s (PByteString :--> PByteString)
pblake2b_224 = punsafeBuiltin PLC.Blake2b_224

-- | @since 2.0.0
pripemd_160 :: forall (s :: S). Term s (PByteString :--> PByteString)
pripemd_160 = punsafeBuiltin PLC.Ripemd_160

-- | @since 2.0.0
pverifyEd25519Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

-- | @since 2.0.0
pverifyEcdsaSecp256k1Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEcdsaSecp256k1Signature = punsafeBuiltin PLC.VerifyEcdsaSecp256k1Signature

-- | @since 2.0.0
pverifySchnorrSecp256k1Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifySchnorrSecp256k1Signature = punsafeBuiltin PLC.VerifySchnorrSecp256k1Signature

-- | @since 2.0.0
pappendString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PString)
pappendString = punsafeBuiltin PLC.AppendString

-- | @since 2.0.0
pequalsString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PBool)
pequalsString = punsafeBuiltin PLC.EqualsString

-- | @since 2.0.0
pencodeUtf8 ::
  forall (s :: S).
  Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | @since 2.0.0
pdecodeUtf8 ::
  forall (s :: S).
  Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

-- | @since 2.0.0
piData ::
  forall (s :: S).
  Term s (PInteger :--> PAsData PInteger)
piData = punsafeBuiltin PLC.IData

-- | @since 2.0.0
pbData ::
  forall (s :: S).
  Term s (PByteString :--> PAsData PByteString)
pbData = punsafeBuiltin PLC.BData

-- | @since 2.0.0
pconstrData ::
  forall (s :: S).
  Term s (PInteger :--> PBList PData :--> PData)
pconstrData = punsafeBuiltin PLC.ConstrData

-- | @since 2.0.0
plistData ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PAsData (PBList (PAsData a)))
plistData = punsafeBuiltin PLC.ListData

-- | @since 2.0.0
pmapData ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBList (PBPair a b) :--> PAsData (PBList (PAsData (PBPair (PAsData a) (PAsData b)))))
pmapData = punsafeBuiltin PLC.MapData

-- | @since 2.0.0
punIData :: forall (s :: S). Term s (PData :--> PInteger)
punIData = punsafeBuiltin PLC.UnIData

-- | @since 2.0.0
punBData :: forall (s :: S). Term s (PData :--> PByteString)
punBData = punsafeBuiltin PLC.UnBData

-- | @since 2.0.0
punConstrData :: forall (s :: S). Term s (PData :--> PBPair PInteger (PBList PData))
punConstrData = punsafeBuiltin PLC.UnConstrData

-- | @since 2.0.0
punListData :: forall (s :: S). Term s (PData :--> PBList PData)
punListData = punsafeBuiltin PLC.UnListData

-- | @since 2.0.0
punMapData :: forall (s :: S). Term s (PData :--> PBList (PBPair PData PData))
punMapData = punsafeBuiltin PLC.UnMapData

-- | @since 2.0.0
pequalsData ::
  forall (a :: S -> Type) (s :: S).
  (PRepresentation a ~ PData) =>
  Term s (a :--> a :--> PBool)
pequalsData = punsafeBuiltin PLC.EqualsData

-- | @since 2.0.0
pserialiseData :: forall (s :: S). Term s (PData :--> PByteString)
pserialiseData = punsafeBuiltin PLC.SerialiseData

-- | @since 2.0.0
pchooseData ::
  forall (a :: S -> Type) (s :: S).
  Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
pchooseData = pforce . punsafeBuiltin $ PLC.ChooseData

-- | @since 2.0.0
pmkPairData ::
  forall (s :: S).
  Term s (PData :--> PData :--> PBPair PData PData)
pmkPairData = punsafeBuiltin PLC.MkPairData

-- | @since 2.0.0
pnilData ::
  forall (a :: S -> Type) (s :: S).
  (PRepresentation a ~ PData) =>
  Term s (PBList a)
pnilData = punsafeConstant . PLC.someValue @[Data] $ []

-- | @since 2.0.0
pbls12_381_G1_add ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

-- | @since 2.0.0
pbls12_381_G1_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

-- | @since 2.0.0
pbls12_381_G1_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

-- | @since 2.0.0
pbls12_381_G1_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBool)
pbls12_381_G1_equal = punsafeBuiltin PLC.Bls12_381_G1_equal

-- | @since 2.0.0
pbls12_381_G1_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G1_Element)
pbls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

-- | @since 2.0.0
pbls12_381_G1_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PByteString)
pbls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

-- | @since 2.0.0
pbls12_381_G1_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G1_Element)
pbls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

-- | @since 2.0.0
pbls12_381_G1_multiScalarMul ::
  forall (s :: S).
  Term s (PBList PInteger :--> PBList PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_multiScalarMul = punsafeBuiltin PLC.Bls12_381_G1_multiScalarMul

-- | @since 2.0.0
pbls12_381_G2_add ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

-- | @since 2.0.0
pbls12_381_G2_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

-- | @since 2.0.0
pbls12_381_G2_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

-- | @since 2.0.0
pbls12_381_G2_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBool)
pbls12_381_G2_equal = punsafeBuiltin PLC.Bls12_381_G2_equal

-- | @since 2.0.0
pbls12_381_G2_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G2_Element)
pbls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

-- | @since 2.0.0
pbls12_381_G2_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PByteString)
pbls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

-- | @since 2.0.0
pbls12_381_G2_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G2_Element)
pbls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

-- | @since 2.0.0
pbls12_381_G2_multiScalarMul ::
  forall (s :: S).
  Term s (PBList PInteger :--> PBList PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_multiScalarMul = punsafeBuiltin PLC.Bls12_381_G2_multiScalarMul

-- | @since 2.0.0
pbls12_381_millerLoop ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_MlResult)
pbls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

-- | @since 2.0.0
pbls12_381_mulMlResult ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBLS12_381_MlResult)
pbls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

-- | @since 2.0.0
pbls12_381_finalVerify ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBool)
pbls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify

-- | @since 2.0.0
pintegerToByteString ::
  forall (s :: S).
  Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteString = punsafeBuiltin PLC.IntegerToByteString

-- | @since 2.0.0
pbyteStringToInteger ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PInteger)
pbyteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

-- | @since 2.0.0
plistToArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBArray a)
plistToArray = pforce . punsafeBuiltin $ PLC.ListToArray

-- | @since 2.0.0
pandByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pandByteString = punsafeBuiltin PLC.AndByteString

-- | @since 2.0.0
porByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
porByteString = punsafeBuiltin PLC.OrByteString

-- | @since 2.0.0
pxorByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pxorByteString = punsafeBuiltin PLC.XorByteString

-- | @since 2.0.0
pcomplementByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pcomplementByteString = punsafeBuiltin PLC.ComplementByteString

-- | @since 2.0.0
preadBit ::
  forall (s :: S).
  Term s (PByteString :--> PNatural :--> PBool)
preadBit = punsafeBuiltin PLC.ReadBit

-- | @since 2.0.0
pwriteBits ::
  forall (s :: S).
  Term s (PByteString :--> PBList PInteger :--> PBool :--> PByteString)
pwriteBits = punsafeBuiltin PLC.WriteBits

-- | @since 2.0.0
pshiftByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
pshiftByteString = punsafeBuiltin PLC.ShiftByteString

-- | @since 2.0.0
protateByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
protateByteString = punsafeBuiltin PLC.RotateByteString

-- | @since 2.0.0
pcountSetBits ::
  forall (s :: S).
  Term s (PByteString :--> PNatural)
pcountSetBits = punsafeBuiltin PLC.CountSetBits

-- | @since 2.0.0
pfindFirstSetBit ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pfindFirstSetBit = punsafeBuiltin PLC.FindFirstSetBit

-- | @since 2.0.0
pinsertCoin ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PBValue :--> PBValue)
pinsertCoin = punsafeBuiltin PLC.InsertCoin

-- | @since 2.0.0
plookupCoin ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBValue :--> PInteger)
plookupCoin = punsafeBuiltin PLC.LookupCoin

-- | @since 2.0.0
punionValue ::
  forall (s :: S).
  Term s (PBValue :--> PBValue :--> PBValue)
punionValue = punsafeBuiltin PLC.UnionValue

-- | @since 2.0.0
pvalueContains ::
  forall (s :: S).
  Term s (PBValue :--> PBValue :--> PBool)
pvalueContains = punsafeBuiltin PLC.ValueContains

-- | @since 2.0.0
pvalueData ::
  forall (s :: S).
  Term s (PBValue :--> PAsData PBValue)
pvalueData = punsafeBuiltin PLC.ValueData

-- | @since 2.0.0
punValueData ::
  forall (s :: S).
  Term s (PData :--> PBValue)
punValueData = punsafeBuiltin PLC.UnValueData

-- | @since 2.0.0
pscaleValue ::
  forall (s :: S).
  Term s (PInteger :--> PBValue :--> PBValue)
pscaleValue = punsafeBuiltin PLC.ScaleValue

-- | @since 2.0.0
pifThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pifThenElse = pforce . punsafeBuiltin $ PLC.IfThenElse

-- | @since 2.0.0
ptrace ::
  forall (a :: S -> Type) (s :: S).
  Term s (PString :--> a :--> a)
ptrace = pforce . punsafeBuiltin $ PLC.Trace

-- | @since 2.0.0
pfstPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBPair a b :--> a)
pfstPair = pforce . pforce . punsafeBuiltin $ PLC.FstPair

-- | @since 2.0.0
psndPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBPair a b :--> b)
psndPair = pforce . pforce . punsafeBuiltin $ PLC.SndPair

-- | @since 2.0.0
pchooseList ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBList a :--> b :--> b :--> b)
pchooseList = pforce . pforce . punsafeBuiltin $ PLC.ChooseList

-- | @since 2.0.0
pmkCons ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PBList a :--> PBList a)
pmkCons = pforce $ punsafeBuiltin PLC.MkCons

-- | @since 2.0.0
pheadList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> a)
pheadList = pforce $ punsafeBuiltin PLC.HeadList

-- | @since 2.0.0
ptailList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBList a)
ptailList = pforce $ punsafeBuiltin PLC.TailList

-- | @since 2.0.0
pnullList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBool)
pnullList = pforce $ punsafeBuiltin PLC.NullList

-- | @since 2.0.0
pdropList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PNatural :--> PBList a :--> PBList a)
pdropList = pforce $ punsafeBuiltin PLC.DropList

-- | @since 2.0.0
plengthOfArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBArray a :--> PNatural)
plengthOfArray = pforce $ punsafeBuiltin PLC.LengthOfArray

-- | @since 2.0.0
pindexArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBArray a :--> PNatural :--> a)
pindexArray = pforce $ punsafeBuiltin PLC.IndexArray
