{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.MS
  ( PTheseMS (..),
    PMSOp (..),
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

data PTheseMS (a :: Type) (b :: Type)
  = PThisMS (Term a)
  | PThatMS (Term b)
  | PTheseMS (Term a) (Term b)

deriveFor ''PTheseMS MogensenScott AllInstances

newtype PMSOp (a :: Type) (b :: Type)
  = PMSOp (Term (b :~> a))

deriveFor ''PMSOp MogensenScott NoEqDebug
