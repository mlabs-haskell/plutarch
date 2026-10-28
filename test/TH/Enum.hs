{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Enum (PColour (..), PTimeOfDay (..)) where

import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Prelude qualified as P

type PColour :: Type
data PColour
  = PRed
  | PGreen
  | PBlue

deriveFor ''PColour P.Enum AllInstances

type PTimeOfDay :: Type
data PTimeOfDay
  = PDay
  | PNight

deriveFor ''PTimeOfDay P.Enum NoEqDebug
