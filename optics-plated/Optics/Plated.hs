{-# LANGUAGE DataKinds #-}

module Optics.Plated
  ( Plated (..),
    children,
  )
where

import Data.Kind (Type)
import Optics.Fold (toListOf)
import Optics.Traversal (Traversal', traversalVL)

class Plated (a :: Type) where
  plate :: Traversal' a a

instance Plated [a] where
  plate = traversalVL $ \f -> \case
    [] -> pure []
    (x : xs) -> (x :) <$> f xs

children ::
  forall (a :: Type).
  (Plated a) => a -> [a]
children = toListOf plate
