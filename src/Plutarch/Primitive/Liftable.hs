{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 2.0.0
module Plutarch.Primitive.Liftable
  ( -- * Type classes
    ReprError (..),
    PlutusRepresentable (..),
    LiftError (..),
    PLiftable (..),

    -- * Functions
    pconstant,
    plift,

    -- * Derivation helper
    PLiftableDirect (..),

    -- * Positive numbers in Haskell
    Positive (..),
    mkPositive,
    unsafeMkPositive,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Vector.Strict (Vector)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Plutarch.Backend.Evaluate (EvalError, peval)
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    punsafeSecondary,
  )
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (punsafeConstant)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (GEqL, geqL)
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS.G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as BLS.Pairing
import PlutusCore.Data as PLCData
import PlutusCore.Value as PLCValue

-- | Errors that can arise when converting from an onchain representation type
-- to its \'narrower\' equivalent.
--
-- @since 2.0.0
data ReprError
  = UnexpectedNegative Integer
  | ByteOutOfBounds Integer
  | UnexpectedZero
  | UserSpecified String
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | Errors that can arise when evaluating a computation designed to produce a
-- 'PLiftable' type.
--
-- @since 2.0.0
data LiftError
  = DidNotEvaluate EvalError
  | NotAConstant
  | WrongConstantType
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | Specifies that some Haskell-level type @a@ can be converted to and from
-- some other type @'AsPlutus' a@, where all of the following hold:
--
-- * @'AsPlutus' a@ is part of the Plutus default universe;
-- * @a@ can be converted unconditionally to @'AsPlutus' a@;
-- * @'AsPlutus' a@ can be converted conditionally to @a@; and
-- * The conversion should round-trip when starting from @a@.
--
-- Put another way, a @'PlutusRepresentable' a@ instance defines an embedding into
-- some member of the Plutus default universe.
--
-- = Laws
--
-- 1. @'reprToHask' '.' 'haskToRepr'@ @=@ @'Right'@
--
-- = Defaults
--
-- 'PlutusRepresentable' defaults are designed to make instances of types already
-- in the default Plutus universe efficient. You should (probably) not use these
-- yourself.
--
-- @since 2.0.0
class
  (PLC.DefaultUni `PLC.Contains` AsPlutus a) =>
  PlutusRepresentable (a :: Type)
  where
  type AsPlutus a :: Type
  type AsPlutus a = a
  haskToRepr :: a -> AsPlutus a
  default haskToRepr :: (a ~ AsPlutus a) => a -> AsPlutus a
  haskToRepr = id
  reprToHask :: AsPlutus a -> Either ReprError a
  default reprToHask :: (a ~ AsPlutus a) => AsPlutus a -> Either ReprError a
  reprToHask = Right

-- | @since 2.0.0
instance PlutusRepresentable Integer

-- | @since 2.0.0
instance PlutusRepresentable Bool

-- | @since 2.0.0
instance PlutusRepresentable ByteString

-- | @since 2.0.0
instance PlutusRepresentable Text

-- | @since 2.0.0
instance PlutusRepresentable BLS.G1.Element

-- | @since 2.0.0
instance PlutusRepresentable BLS.G2.Element

-- | @since 2.0.0
instance PlutusRepresentable BLS.Pairing.MlResult

-- | @since 2.0.0
instance PlutusRepresentable PLCValue.Value

-- | @since 2.0.0
instance PlutusRepresentable PLCData.Data

-- | @since 2.0.0
instance PlutusRepresentable Natural where
  type AsPlutus Natural = Integer
  haskToRepr = fromIntegral
  reprToHask i = case signum i of
    (-1) -> Left . UnexpectedNegative $ i
    _ -> Right . fromIntegral $ i

-- | @since 2.0.0
instance PlutusRepresentable Word8 where
  type AsPlutus Word8 = Integer
  haskToRepr = fromIntegral
  reprToHask i
    | i < 0 = Left . ByteOutOfBounds $ i
    | i > 255 = Left . ByteOutOfBounds $ i
    | otherwise = Right . fromIntegral $ i

-- | @since 2.0.0
instance (PlutusRepresentable a) => PlutusRepresentable [a] where
  type AsPlutus [a] = [AsPlutus a]
  haskToRepr = fmap haskToRepr
  reprToHask = traverse reprToHask

-- | @since 2.0.0
instance (PlutusRepresentable a) => PlutusRepresentable (Vector a) where
  type AsPlutus (Vector a) = Vector (AsPlutus a)
  haskToRepr = fmap haskToRepr
  reprToHask = traverse reprToHask

-- | @since 2.0.0
instance (PlutusRepresentable a, PlutusRepresentable b) => PlutusRepresentable (a, b) where
  type AsPlutus (a, b) = (AsPlutus a, AsPlutus b)
  haskToRepr = bimap haskToRepr haskToRepr
  reprToHask = bitraverse reprToHask reprToHask

-- | Designed as a Haskell-level equivalent to 'PPositive'. As this is designed
-- primarily for lifting, few if any instances are provided for it.
--
-- @since 2.0.0
newtype Positive = UnsafePositive Natural
  deriving
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Ord,
      -- | @since 2.0.0
      Show
    )
    via Natural

-- | @since 2.0.0
instance PlutusRepresentable Positive where
  type AsPlutus Positive = Integer
  haskToRepr (UnsafePositive n) = fromIntegral n
  reprToHask i = case compare i 0 of
    LT -> Left . UnexpectedNegative $ i
    EQ -> Left UnexpectedZero
    GT -> Right . UnsafePositive . fromIntegral $ i

-- | Safely construct a 'Positive'.
--
-- @since 2.0.0
mkPositive ::
  forall (a :: Type).
  (Integral a) => a -> Maybe Positive
mkPositive n = UnsafePositive (fromIntegral n) <$ guard (n > 0)

-- | Construct a 'Positive' directly from a 'Natural' with no checks. Use with
-- care.
--
-- @since 2.0.0
unsafeMkPositive :: Natural -> Positive
unsafeMkPositive = UnsafePositive

-- | Specifies that, for some Plutarch type @a@, there exists some Haskell type
-- @AsHaskell a@ such that its Plutus representation terms can be \'lifted\' as
-- constants into a Plutarch 'Term'. Additionally, specifies that any closed
-- Plutarch 'Term' producing an @a@ can be evaluated \'on the spot\' to produce
-- the equivalent Plutus representation of its result. Put another way, an
-- instance of @'PLiftable' a@ defines an embedding between some type in the
-- host language (Haskell) and scripts that Plutarch can produce.
--
-- = Important note
--
-- 'plutToRepr' is not something you should use in a script, as it requires
-- using the evaluator. It exists primarily for tests. 'reprToPlut' is fine to
-- use in scripts (as this ends up being embedded as a constant), though
-- 'pconstant' is usually more convenient.
--
-- = Laws
--
-- 1. @'reprToPlut' '.' 'plutToRepr'@ @=@ @'Right'@
--
-- @since 2.0.0
class
  (PlutusRepresentable (AsHaskell a), GEqL PLC.DefaultUni (AsPlutus (AsHaskell a)), PlutarchType a) =>
  -- Note (Koz, 26/06/2026): The second constraint here is needed to make
  -- `plutToRepr` work. The _short_ answer is that the machinery required to
  -- convert `Term` constants back into their (wrapped!) Haskell values involves
  -- a heterogenously-typed proof object comparison, for which there exists a
  -- dedicated, specialized type class (`GEqL` above). As this typeclass is
  -- standalone, and the type class universe is open, GHC _cannot_ infer the
  -- `GEqL` constraint from the containment constraint (despite the fact that it
  -- always holds and always should). While we could defer this problem to the
  -- specific instances that require it, doing so would create far too much
  -- typechecker noise, so we put it here.
  PLiftable (a :: S -> Type)
  where
  type AsHaskell a :: Type
  plutToRepr :: (forall (s :: S). Term s a) -> Either LiftError (AsPlutus (AsHaskell a))
  reprToPlut :: forall (s :: S). AsPlutus (AsHaskell a) -> Term s a

-- | @since 2.0.0
deriving via (PLiftableDirect S.PBool Bool) instance PLiftable S.PBool

-- | @since 2.0.0
deriving via (PLiftableDirect S.PInteger Integer) instance PLiftable S.PInteger

-- | @since 2.0.0
deriving via (PLiftableDirect S.PByteString ByteString) instance PLiftable S.PByteString

-- | @since 2.0.0
deriving via (PLiftableDirect S.PString Text) instance PLiftable S.PString

-- | @since 2.0.0
deriving via
  (PLiftableDirect S.PBLS12_381_G1_Element BLS.G1.Element)
  instance
    PLiftable S.PBLS12_381_G1_Element

-- | @since 2.0.0
deriving via
  (PLiftableDirect S.PBLS12_381_G2_Element BLS.G2.Element)
  instance
    PLiftable S.PBLS12_381_G2_Element

-- | @since 2.0.0
deriving via
  (PLiftableDirect S.PBLS12_381_MlResult BLS.Pairing.MlResult)
  instance
    PLiftable S.PBLS12_381_MlResult

-- | @since 2.0.0
deriving via
  (PLiftableDirect S.PBValue PLCValue.Value)
  instance
    PLiftable S.PBValue

-- | @since 2.0.0
deriving via
  (PLiftableDirect S.PData PLCData.Data)
  instance
    PLiftable S.PData

-- | @since 2.0.0
instance PLiftable S.PNatural where
  type AsHaskell S.PNatural = Natural
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . reprToPlut @S.PInteger

-- | @since 2.0.0
instance PLiftable S.PByte where
  type AsHaskell S.PByte = Word8
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . reprToPlut @S.PInteger

-- | @since 2.0.0
instance PLiftable S.PPositive where
  type AsHaskell S.PPositive = Positive
  plutToRepr t = plutToRepr (pfundamental t)
  reprToPlut = punsafeSecondary . reprToPlut @S.PInteger

-- | @since 2.0.0
instance (PLiftable a) => PLiftable (C.PBList a) where
  type AsHaskell (C.PBList a) = [AsHaskell a]
  plutToRepr t = case peval t of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof x) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @[AsPlutus (AsHaskell a)]
        case geqL expectedProof actualProof of
          PLC.EvaluationSuccess PLC.Refl -> pure x
          PLC.EvaluationFailure -> Left WrongConstantType
  reprToPlut = punsafeConstant . PLC.someValue @[AsPlutus (AsHaskell a)]

-- | @since 2.0.0
instance (PLiftable a, PLiftable b) => PLiftable (C.PBPair a b) where
  type AsHaskell (C.PBPair a b) = (AsHaskell a, AsHaskell b)
  plutToRepr t = case peval t of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof x) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @(AsPlutus (AsHaskell a), AsPlutus (AsHaskell b))
        case geqL expectedProof actualProof of
          PLC.EvaluationSuccess PLC.Refl -> pure x
          PLC.EvaluationFailure -> Left WrongConstantType
  reprToPlut = punsafeConstant . PLC.someValue @(AsPlutus (AsHaskell a), AsPlutus (AsHaskell b))

-- | @since 2.0.0
instance (PLiftable a) => PLiftable (S.PBArray a) where
  type AsHaskell (S.PBArray a) = Vector (AsHaskell a)
  plutToRepr t = case peval t of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof x) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @(Vector (AsPlutus (AsHaskell a)))
        case geqL expectedProof actualProof of
          PLC.EvaluationSuccess PLC.Refl -> pure x
          PLC.EvaluationFailure -> Left WrongConstantType
  reprToPlut = punsafeConstant . PLC.someValue @(Vector (AsPlutus (AsHaskell a)))

-- | Given the Haskell representation of a type, produce a 'Term' containing the
-- equivalent constant.
--
-- @since 2.0.0
pconstant ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a) =>
  AsHaskell a ->
  Term s a
pconstant = reprToPlut . haskToRepr

-- | Given a closed 'Term', evaluate it and produce the Haskell representation
-- of the result.
--
-- = Note
--
-- If the computation errors, or produces a result of the wrong type, this will
-- call @'error'@.
--
-- @since 2.0.0
plift ::
  forall (a :: S -> Type).
  (PLiftable a) =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case plutToRepr t of
  Left err -> error $ "lifting failed: " <> show err
  Right res -> case reprToHask res of
    Left err -> error $ "representation error: " <> show err
    Right res' -> res'

-- | A helper @newtype@ for deriving 'PLiftable' for Plutarch types which are
-- directly representable using something in the Plutus default universe. More
-- precisely, using 'PLiftableDirect' in a @via@ derivation specifies the
-- following:
--
-- * @a@'s Haskell equivalent is @h@; and
-- * @h@ is a part of the default Plutus universe.
--
-- @since 2.0.0
newtype PLiftableDirect (a :: S -> Type) (h :: Type) (s :: S) = PLiftableDirect (a s)

-- | @since 2.0.0
instance (PlutarchType a, PRepresentation a ~ a) => PlutarchType (PLiftableDirect a h) where
  type PRepresentation (PLiftableDirect a h) = a

-- | @since 2.0.0
instance
  (PlutarchType a, GEqL PLC.DefaultUni (AsPlutus h), PlutusRepresentable h, PRepresentation a ~ a) =>
  PLiftable (PLiftableDirect a h)
  where
  type AsHaskell (PLiftableDirect a h) = h
  plutToRepr t = case peval (pfundamental t) of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    -- Note (Koz, 26/06/2026): This sludge requires further explanation. When we
    -- get a constant back from the evaluator, we actually get two things:
    --
    -- 1. The constant itself (which is a Haskell value of some type); and
    -- 2. A proof of membership of said constant in the universe.
    --
    -- To ensure that what we have makes sense, we have to first verify that our
    -- proof of membership is what we expect: namely, that we got a proof
    -- specifically that `h` (and not some other type) is a member of the
    -- default universe. Doing this involves the use of `geqL`, as there is no
    -- guarantee that the proof will carry the same tag of what it's for as `h`.
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof x) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @(AsPlutus h)
        case geqL expectedProof actualProof of
          -- As the proofs match, in this branch, we know that x :: AsPlutus h
          PLC.EvaluationSuccess PLC.Refl -> pure x
          PLC.EvaluationFailure -> Left WrongConstantType
  reprToPlut = punsafeConstant . PLC.someValue @(AsPlutus h)
