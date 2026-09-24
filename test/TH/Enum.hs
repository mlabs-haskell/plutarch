{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Enum (PColour (..)) where

import Plutarch.Prelude
import Plutarch.Prelude qualified as P

data PColour (s :: S)
  = PRed
  | PGreen
  | PBlue

deriveFor ''PColour P.Enum
