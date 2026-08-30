-- | @since 2.0.0
module Plutarch.Primitive.Function
  ( (:-->),
  )
where

import Data.Kind (Type)
import Plutarch.Backend.S (S)

-- | The type of a Plutarch lambda.
--
-- @since 2.0.0
data (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)

type role (:-->) nominal nominal nominal

infixr 0 :-->
