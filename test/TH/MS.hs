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

data PTheseMS (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisMS (Term s a)
  | PThatMS (Term s b)
  | PTheseMS (Term s a) (Term s b)

deriveFor ''PTheseMS MogensenScott AllInstances

newtype PMSOp (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PMSOp (Term s (b :--> a))

deriveFor ''PMSOp MogensenScott NoEqDebug
