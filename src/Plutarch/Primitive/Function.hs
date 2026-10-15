{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module Plutarch.Primitive.Function where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Generics.SOP
  ( All (cpara_SList),
    K (K),
    SListI,
    Shape (ShapeCons, ShapeNil),
    Top,
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

data HVec (s :: S) (f :: [S -> Type]) where
  HNil :: HVec s '[]
  HCons :: forall (a :: S -> Type) (b :: [S -> Type]) (s :: S). Term s a -> HVec s b -> HVec s (a ': b)

type family PlutarchArrow (ts :: [S -> Type]) (r :: S -> Type) where
  PlutarchArrow '[] b = b
  PlutarchArrow (a ': as) b = a :--> PlutarchArrow as b

data PContext :: (S -> Type) -> (S -> Type) -> Type -> Type where
  PCxt :: (Term s b -> t) -> Term s a -> PContext a b t

-- f :: (a -> b -> c -> d)

-- | c1 = PCxt (\_ -> f : () -> a -> (b -> c -> d)) () : PContext () () (a -> b -> c -> d) '[]
-- | |- x : a
-- |  | case c1 of PCxt g x ->
-- |     c2 = PCxt (g x a : b -> (c -> d)) a : PContext a b (b -> c -> d) '[a]
-- |     | y : b
-- |     case c2 of PCxt h a ->
-- |       c3 = PCxt (h y :: (c -> d)) b : PContext b c d '[a,b]

-- PContext almost works except it's very hard to see how we could construct it w/o
-- type class soup that we're trying to avoid! But I think we can make it work with a
-- type level list....

plam :: forall s xs r. (SListI xs) => (HVec s xs -> Term s r) -> Term s (PlutarchArrow xs r)
plam f = case shape @(S -> Type) @xs of
  ShapeNil -> f HNil
  ShapeCons @ys @y _rest ->
    let g :: Term s y -> HVec s ys -> Term s r
        g a ys = f (HCons a ys)
     in plam' $ \y -> plam (g y)

{-
type PLiftFn :: S -> Type -> Constraint
class PLiftFn s t where
  type ToPF s t :: (S -> Type)
  type PFArgs s t :: [S -> Type]
  type PFResult s t :: (S -> Type)

  pfunK :: t -> PFunK t (PFArgs s t) (PFResult s t) (ToPF s t) s

instance PLiftFn s (Term s a) where
  type ToPF s (Term s a) = a
  type PFArgs s (Term s a) = '[]
  type PFResult s (Term s a) = a

  pfunK  = PDoneK

instance PLiftFn s (Term s a -> Term s b) where
  type ToPF s (Term s a -> Term s b)
-}
{-
plam ::
  forall (ts :: [S -> Type]) (b :: S -> Type) (s :: S).
  (SListI ts) =>
  (HVec s ts -> Term s b) -> PFunK
plam f = mkP
-}
