{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Data
  ( PTheseData (..),
    PTriple (..),
    PPairData (..),
    PSmash (..),
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

data PTheseData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisData (Term s (PAsData a))
  | PThatData (Term s (PAsData b))
  | PTheseData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PTheseData DataPlutus AllInstances

instance (PCanData a, PCanData b) => PCanData (PTheseData a b)

data PTriple (a :: S -> Type) (s :: S)
  = PTriple (Term s (PAsData a)) (Term s (PAsData a)) (Term s (PAsData a))

deriveFor ''PTriple DataList AllInstances

instance (PCanData a) => PCanData (PTriple a)

data PPairData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PPairData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PPairData DataList NoEqDebug

data PSmash (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PNada
  | PSmash (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PSmash DataPlutus NoEqDebug
