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
import Plutarch.Backend.Term
  ( S,
    Term (Term),
    TermEnv (TermEnv),
    TracingMode (NoTracing),
    asRawTerm,
  )
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Representation
  ( PlutarchType (PRepresentation),
    pfundamental,
  )
import Plutarch.Primitive.Singular (PString)
import Plutarch.Primitive.Singular qualified as S

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
