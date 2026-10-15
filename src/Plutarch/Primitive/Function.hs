{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeAbstractions #-}

-- | @since 2.0.0
module Plutarch.Primitive.Function
  ( -- * Basic interface
    plam1,
    plam2,
    plam3,
    plam4,
    plam5,
    plam6,

    -- * Variadic
    PLamArgs (..),
    PlutarchArrow,
    plam,
  )
where

import Data.Kind (Type)
import Generics.SOP
  ( SListI,
    Shape (ShapeCons, ShapeNil),
    shape,
  )
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Singular ((:-->))
import Plutarch.Primitive.Term (plam')

-- | A type-heterogenous cons list of 'Term's. Designed to allow constructing a
-- Plutus lambda taking any number of arguments from a code transformation
-- taking the same number of arguments.
--
-- @since 2.0.0
data PLamArgs (ts :: [S -> Type]) (s :: S) where
  Done :: PLamArgs '[] s
  Arg ::
    forall (a :: S -> Type) (ts :: [S -> Type]) (s :: S).
    Term s a -> PLamArgs ts s -> PLamArgs (a ': ts) s

-- | Needed to successfully type 'plam'. Essentially, @'PlutarchArrow' [a_1,
-- a_2, ... , a_k] b@ is the same as @a_1 :--> a_2 :--> ... :--> a_k :--> b@.
--
-- @since 2.0.0
type family PlutarchArrow (ts :: [S -> Type]) (r :: S -> Type) where
  PlutarchArrow '[] r = r
  PlutarchArrow (a ': as) r = a :--> PlutarchArrow as r

-- | Given a code transformation involving any number of 'Term's to produce
-- another 'Term', construct a Plutus lambda taking the same number of arguments,
-- of the same types, and producing a result of the same type as the 'Term' that
-- the code transformation would produce.
--
-- = Note
--
-- You shouldn't need to use this directly very often: prefer the various
-- @plam*@ helpers. This is designed to act as a uniform \'back-end\' for
-- generating Plutus lambdas of arbitrary arity, shared across all helpers, and
-- for the rare case where you want a huge number of arguments.
--
-- @since 2.0.0
plam ::
  forall (ts :: [S -> Type]) (r :: S -> Type) (s :: S).
  (SListI ts) =>
  (PLamArgs ts s -> Term s r) -> Term s (PlutarchArrow ts r)
plam f = case shape @(S -> Type) @ts of
  ShapeNil -> f Done
  ShapeCons @ts' @a _ ->
    let g :: Term s a -> PLamArgs ts' s -> Term s r
        g x rest = f (Arg x rest)
     in plam' $ \arg -> plam (g arg)

-- | Construct an arity-1 Plutus lambda given a 'Term' transformation taking one
-- argument.
--
-- @since 2.0.0
plam1 ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s a -> Term s b) -> Term s (a :--> b)
plam1 = plam'

-- | As 'plam1', but for two arguments.
--
-- @since 2.0.0
plam2 ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  (Term s a -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
plam2 f = plam @[a, b] $ \case
  Arg t1 (Arg t2 Done) -> f t1 t2

-- | As 'plam1', but for three arguments.
--
-- @since 2.0.0
plam3 ::
  forall
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (d :: S -> Type)
    (s :: S).
  (Term s a -> Term s b -> Term s c -> Term s d) ->
  Term s (a :--> b :--> c :--> d)
plam3 f = plam @[a, b, c] $ \case
  Arg t1 (Arg t2 (Arg t3 Done)) -> f t1 t2 t3

-- | As 'plam1', but for four arguments.
--
-- @since 2.0.0
plam4 ::
  forall
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (d :: S -> Type)
    (e :: S -> Type)
    (s :: S).
  (Term s a -> Term s b -> Term s c -> Term s d -> Term s e) ->
  Term s (a :--> b :--> c :--> d :--> e)
plam4 f = plam @[a, b, c, d] $ \case
  Arg t1 (Arg t2 (Arg t3 (Arg t4 Done))) -> f t1 t2 t3 t4

-- | As 'plam1', but for five arguments.
--
-- @since 2.0.0
plam5 ::
  forall
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (d :: S -> Type)
    (e :: S -> Type)
    (f :: S -> Type)
    (s :: S).
  (Term s a -> Term s b -> Term s c -> Term s d -> Term s e -> Term s f) ->
  Term s (a :--> b :--> c :--> d :--> e :--> f)
plam5 f = plam @[a, b, c, d, e] $ \case
  Arg t1 (Arg t2 (Arg t3 (Arg t4 (Arg t5 Done)))) -> f t1 t2 t3 t4 t5

-- | As 'plam1', but for six arguments.
--
-- @since 2.0.0
plam6 ::
  forall
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (d :: S -> Type)
    (e :: S -> Type)
    (f :: S -> Type)
    (g :: S -> Type)
    (s :: S).
  (Term s a -> Term s b -> Term s c -> Term s d -> Term s e -> Term s f -> Term s g) ->
  Term s (a :--> b :--> c :--> d :--> e :--> f :--> g)
plam6 f = plam @[a, b, c, d, e, f] $ \case
  Arg t1 (Arg t2 (Arg t3 (Arg t4 (Arg t5 (Arg t6 Done))))) -> f t1 t2 t3 t4 t5 t6
