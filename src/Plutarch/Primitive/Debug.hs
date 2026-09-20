{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 2.0.0
module Plutarch.Primitive.Debug
  ( PDebug (..),
    pshow,
  )
where

import Control.Monad.Reader (ask)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term
  ( S,
    Term (Term),
    TermEnv (TermEnv),
    TracingMode (NoTracing),
    asRawTerm,
    toSomeTerm,
  )
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun (pif, pisNegative)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
    punsafeSecondary,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular (PInteger, PNatural, PString, (:-->))
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (pfix, plam', punsafeCase)

-- | A way of displaying a value of a Plutarch type (for debugging purposes).
-- This is designed primarily for debugging: thus, release builds of scripts
-- will not include this logic.
--
-- = Why the CPS?
--
-- 'pdebug' is written in a continuation-passing style. This allows us to avoid
-- generating any code to produce debug representations when 'pshow' is used if
-- we're in a release build.
--
-- = Important note
--
-- /Do not/ use 'pdebug' directly! This is designed mostly as an implementation
-- detail. Use 'pshow' instead: not only is this easier, it also means that if
-- you accidentally leave in any 'pshow's, your scripts won't suffer too much.
--
-- @since 2.0.0
class (PlutarchType a) => PDebug (a :: S -> Type) where
  pdebug ::
    forall (r :: S -> Type) (s :: S).
    (PlutarchType r) =>
    Term s a -> (Term s PString -> Term s r) -> Term s r
  default pdebug ::
    forall (r :: S -> Type) (s :: S).
    (PlutarchType r, PDebug (PRepresentation a)) =>
    Term s a -> (Term s PString -> Term s r) -> Term s r
  pdebug t = pdebug (pfundamental t)

-- | @since 2.0.0
instance PDebug S.PBool where
  pdebug t f = f (pif t (pconstant "true") (pconstant "false"))

-- | @since 2.0.0
instance PDebug S.PInteger where
  pdebug t f =
    f
      ( pif
          (pisNegative t)
          -- Add a minus sign, absolute value
          (pconstant "-" #<|> pshow (punsafeSecondary @PNatural (BF.psubtractInteger # zero # t)))
          (pshow (punsafeSecondary @PNatural t))
      )

-- | @since 2.0.0
instance PDebug PNatural where
  pdebug t f =
    f
      ( pif
          (pfundamental t #== zero)
          (pconstant "0")
          ( let t' = pfundamental t
                q = BF.pquotientInteger # t' # ten
                r = BF.premainderInteger # t' # ten
             in renderNum # q # r
          )
      )

-- | @since 2.0.0
instance PDebug S.PPositive where
  pdebug t f =
    f
      ( let t' = pfundamental t
            q = BF.pquotientInteger # t' # ten
            r = BF.premainderInteger # t' # ten
         in renderNum # q # r
      )

-- | @since 2.0.0
instance PDebug S.PNZInteger

-- | Displays as hex.
--
-- @since 2.0.0
instance PDebug S.PByte where
  pdebug t f =
    f
      ( let t' = pfundamental t
            d1 = BF.pquotientInteger # t' # sixteen
            d2 = BF.premainderInteger # t' # sixteen
         in pconstant "0x" #<|> (renderHexDigit # d1) #<|> (renderHexDigit # d2)
      )

-- | Displays as a list of hex values.
--
-- @since 2.0.0
instance PDebug S.PByteString where
  pdebug t f =
    f
      ( let len = pfundamental (BF.plengthOfByteString # t)
         in pif
              (len #== zero)
              (pconstant "[]")
              ( let lim = BF.psubtractInteger # len # one
                 in pconstant "[" #<|> (go # t # lim # zero) #<|> pconstant "]"
              )
      )
    where
      go :: forall (s :: S). Term s (S.PByteString :--> PInteger :--> PInteger :--> PString)
      go = pfix $ \self -> plam' $ \bs -> plam' $ \lim -> plam' $ \i ->
        let rendered = pshow $ BF.pindexByteString # bs # punsafeSecondary i
         in pif
              (lim #== i)
              rendered
              (rendered #<|> pconstant ", " #<|> (self # bs # lim #$ BF.paddInteger # i # one))

-- | @since 2.0.0
instance (PDebug a) => PDebug (C.PBList a) where
  pdebug t f = f (formatList (plam' pshow) # t)

-- | @since 2.0.0
instance (PDebug a, PDebug b) => PDebug (C.PBPair a b) where
  pdebug t f = f (formatPair (plam' pshow) (plam' pshow) # t)

-- | @since 2.0.0
instance PDebug S.PData where
  pdebug ::
    forall (r :: S -> Type) (s :: S).
    Term s S.PData -> (Term s PString -> Term s r) -> Term s r
  pdebug t f = f (go # t)
    where
      go :: Term s (S.PData :--> PString)
      go = pfix $ \self -> plam' $ \dat ->
        BF.pchooseData
          # dat
          # (formatPair (plam' pshow) (formatList self) #$ BF.punConstrData # dat)
          # (formatList (formatPair self self) #$ BF.punMapData # dat)
          # (formatList self #$ BF.punListData # dat)
          # pshow (BF.punIData # dat)
          # pshow (BF.punBData # dat)

-- | Will display the 'S.PData' encoding rather than the wrapped type @a@.
--
-- @since 2.0.0
instance PDebug (S.PAsData a)

-- | Wraps in @"@.
--
-- @since 2.0.0
instance PDebug PString where
  pdebug t f = f (pconstant "\"" #<|> t #<|> pconstant "\"")

-- | A direct way of transforming something with a 'PDebug' instance into a
-- 'PString' representation.
--
-- = Important note
--
-- 'pshow' is extremely inefficient, both in terms of onchain resources and
-- script size. It should not be used for production - only debugging.
--
-- To help reduce the impact of accidental 'pshow' inclusion, if tracing is
-- disabled, 'pshow' will generate an empty string. This is not ideal, but at
-- least limits the impact of stray 'pshow's.
--
-- @since 2.0.0
pshow ::
  forall (a :: S -> Type) (s :: S).
  (PDebug a) =>
  Term s a -> Term s PString
pshow t = pdebug t $ \asString -> Term $ do
  TermEnv tracing _ <- ask
  asRawTerm $ case tracing of
    NoTracing -> pconstant ""
    _ -> asString

-- Helpers

-- Shorthand
(#<|>) :: forall (s :: S). Term s PString -> Term s PString -> Term s PString
t1 #<|> t2 = BF.pappendString # t1 # t2

renderNum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PString)
renderNum = pfix $ \self -> plam' $ \q -> plam' $ \r ->
  let d = renderDigit # r
   in pif
        (q #== zero)
        d
        ( let q' = BF.pquotientInteger # q # ten
              r' = BF.premainderInteger # q # ten
           in (self # q' # r') #<|> d
        )

renderDigit :: forall (s :: S). Term s (PInteger :--> PString)
renderDigit = plam' $ \i ->
  punsafeCase i
    . NEVector.generate1 10
    $ toSomeTerm . pconstant @PString . Text.pack . show

renderHexDigit :: forall (s :: S). Term s (PInteger :--> PString)
renderHexDigit = plam' $ \i ->
  punsafeCase i . NEVector.generate1 16 $
    toSomeTerm . pconstant @PString . \case
      0 -> "0"
      1 -> "1"
      2 -> "2"
      3 -> "3"
      4 -> "4"
      5 -> "5"
      6 -> "6"
      7 -> "7"
      8 -> "8"
      9 -> "9"
      10 -> "A"
      11 -> "B"
      12 -> "C"
      13 -> "D"
      14 -> "E"
      _ -> "F"

ten :: forall (s :: S). Term s PInteger
ten = pconstant 10

zero :: forall (s :: S). Term s PInteger
zero = pconstant 0

sixteen :: forall (s :: S). Term s PInteger
sixteen = pconstant 16

one :: forall (s :: S). Term s PInteger
one = pconstant 1

formatList ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> PString) ->
  Term s (C.PBList a :--> PString)
formatList f = plam' $ \ell -> pmatch ell $ \case
  C.PBNil -> pconstant "[]"
  C.PBCons x xs -> pconstant "[" #<|> (go # x # xs) #<|> pconstant "]"
  where
    go :: Term s (a :--> C.PBList a :--> PString)
    go = pfix $ \self -> plam' $ \x ->
      let rendered = f # x
       in plam' $ \xs -> pmatch xs $ \case
            C.PBNil -> rendered
            C.PBCons y ys -> rendered #<|> pconstant ", " #<|> (self # y # ys)

formatPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> PString) ->
  Term s (b :--> PString) ->
  Term s (C.PBPair a b :--> PString)
formatPair f g = plam' $ \p -> pmatch p $ \(C.PBPair x y) ->
  pconstant "(" #<|> (f # x) #<|> pconstant ", " #<|> (g # y) #<|> pconstant ")"
