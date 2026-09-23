{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Data (PTheseData (..), PTriple (..)) where

import Data.Kind (Type)
import Plutarch.Prelude

data PTheseData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisData (Term s (PAsData a))
  | PThatData (Term s (PAsData b))
  | PTheseData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PTheseData DataPlutus

data PTriple (a :: S -> Type) (s :: S)
  = PTriple (Term s (PAsData a)) (Term s (PAsData a)) (Term s (PAsData a))

deriveFor ''PTriple DataList
