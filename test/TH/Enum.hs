{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Enum (PColour (..), PTimeOfDay (..)) where

import Plutarch.Prelude
import Plutarch.Prelude qualified as P

data PColour (s :: S)
  = PRed
  | PGreen
  | PBlue

deriveFor ''PColour P.Enum AllInstances

instance PCanData PColour

data PTimeOfDay (s :: S)
  = PDay
  | PNight

deriveFor ''PTimeOfDay P.Enum NoEqDebug
