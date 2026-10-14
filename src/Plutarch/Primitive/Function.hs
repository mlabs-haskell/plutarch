{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Plutarch.Primitive.Function where

import Data.Kind (Type)
import Generics.SOP
  ( SListI,
    Shape (ShapeCons, ShapeNil),
    shape,
  )
import Plutarch.Backend.Term (S, Term)
import Plutarch.Primitive.Singular ((:-->))
import Plutarch.Primitive.Term (plam')

-- plam :: forall (ts :: [S -> Type]) (b :: S -> Type) (s :: S) .
--  (HVec s ts -> Term s b) -> Term s (PlutarchArrow ts b)
--
--  data HVec ts s where
--    HNil :: HVec [] s
--    HCons :: Term s a -> HVec ts s -> HVec (a : ts) s
--    HMCons :: a s -> HVec ts s -> HVec (a : ts) s
--
-- plam1 :: (Term s a -> Term s b) -> ...
--
-- plam2 ...
--
-- ...
--
-- plam1Matching :: (a s -> Term  s b) -> Term s (a :--> b)
--
-- plam2:
--
--  * (Term s a -> Term s b -> Term s b) -> Term s (a :--> b :--> c)
--  * (a s -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
--  * (Term s a -> b s -> Term s c) -> Term s (a :--> b :--> c)
--  * (a s -> b s -> Term s c) -> Term s (a :--> b :--> c)
--
-- plam2 :: (Term s a -> Term s b -> Term s b) -> Term s (a :--> b :--> c)
-- plam2 f = plam' $ \x -> plam' $ \y -> f x y
--
-- plam2 :: (a s -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
-- plam2 f = plam' $ \x -> plam' $ \y -> pmatch x $ \x' ->
--              f x' y
--
-- plam2 :: (a s -> b s -> Term s c) -> Term s (a :--> b :--> c)
-- plam2 f = plam' $ \x -> plam' $ \y -> pmatch x $ \x' ->
--              pmatch y $ \y' -> f x' y'

data HVec (ts :: [S -> Type]) (s :: S) where
  HNil :: HVec '[] s
  HCons ::
    forall (a :: S -> Type) (ts :: [S -> Type]) (s :: S).
    Term s a -> HVec ts s -> HVec (a ': ts) s

type family PlutarchArrow (ts :: [S -> Type]) (b :: S -> Type) where
  PlutarchArrow '[] b = b
  PlutarchArrow (a ': as) b = a :--> PlutarchArrow as b

plam ::
  forall (ts :: [S -> Type]) (b :: S -> Type) (s :: S).
  (SListI ts) =>
  (HVec ts s -> Term s b) -> Term s (PlutarchArrow ts b)
plam f = go (shape @(S -> Type) @ts)
  where
    go :: forall (ts' :: [S -> Type]). (SListI ts') => Shape ts' -> Term s (PlutarchArrow ts b)
    go = \case
      ShapeNil -> _
      ShapeCons t -> _
