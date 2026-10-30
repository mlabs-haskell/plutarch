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

data PTheseData (a :: Type) (b :: Type)
  = PThisData (Term (PAsData a))
  | PThatData (Term (PAsData b))
  | PTheseData (Term (PAsData a)) (Term (PAsData b))

deriveFor ''PTheseData DataPlutusCompact AllInstances

data PTriple (a :: Type)
  = PTriple (Term (PAsData a)) (Term (PAsData a)) (Term (PAsData a))

deriveFor ''PTriple DataList AllInstances

data PPairData (a :: Type) (b :: Type)
  = PPairData (Term (PAsData a)) (Term (PAsData b))

deriveFor ''PPairData DataList NoEqDebug

data PSmash (a :: Type) (b :: Type)
  = PNada
  | PSmash (Term (PAsData a)) (Term (PAsData b))

deriveFor ''PSmash DataPlutusCompact NoEqDebug

data PListData (a :: Type)
  = PNilData
  | PConsData (Term (PAsData a)) (Term (PAsData (PListData a)))

deriveFor ''PListData DataPlutus AllInstances

data PNEList (a :: Type)
  = PSolo (Term (PAsData a))
  | PCons (Term (PAsData a)) (Term (PAsData (PNEList a)))

deriveFor ''PNEList DataPlutusCompact AllInstances
