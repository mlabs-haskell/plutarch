{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TH.Newtype (PIdentity (..), PArrow (..)) where

import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce, punsafeSecondary)

newtype PIdentity (a :: Type) = PIdentity (Term a)

deriveFor ''PIdentity NewtypeData AllInstances

newtype PArrow (a :: Type) (b :: Type)
  = PArrow (Term (a :~> b))

deriveFor ''PArrow Newtype NoEqDebug
