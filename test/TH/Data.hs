{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Data
  ( PTheseData (..),
    PTriple (..),
    PPairData (..),
    PSmash (..),
    PListData (..),
    PNEList (..),
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

data PTheseData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisData (Term s (PAsData a))
  | PThatData (Term s (PAsData b))
  | PTheseData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PTheseData DataPlutusCompact AllInstances

data PTriple (a :: S -> Type) (s :: S)
  = PTriple (Term s (PAsData a)) (Term s (PAsData a)) (Term s (PAsData a))

deriveFor ''PTriple DataList AllInstances

data PPairData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PPairData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PPairData DataList NoEqDebug

data PSmash (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PNada
  | PSmash (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PSmash DataPlutusCompact NoEqDebug

data PListData (a :: S -> Type) (s :: S)
  = PNilData
  | PConsData (Term s (PAsData a)) (Term s (PAsData (PListData a)))

deriveFor ''PListData DataPlutus AllInstances

data PNEList (a :: S -> Type) (s :: S)
  = PSolo (Term s (PAsData a))
  | PCons (Term s (PAsData a)) (Term s (PAsData (PNEList a)))

deriveFor ''PNEList DataPlutusCompact AllInstances
