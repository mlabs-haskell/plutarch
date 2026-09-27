{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TH.Newtype (PIdentity (..), PArrow (..)) where

import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce, punsafeSecondary)

newtype PIdentity (a :: S -> Type) (s :: S) = PIdentity (Term s a)

deriveFor ''PIdentity NewtypeData AllInstances

newtype PArrow (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PArrow (Term s (a :--> b))

deriveFor ''PArrow Newtype NoEqDebug
