{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.SOP
  ( PThese (..),
    PEither (..),
    PSOPList (..),
    PSOPOp (..),
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

data PThese (a :: Type) (b :: Type)
  = PThis (Term a)
  | PThat (Term b)
  | PThese (Term a) (Term b)

deriveFor ''PThese SOP AllInstances

data PEither (a :: Type) (b :: Type)
  = PLeft (Term a)
  | PRight (Term b)

deriveFor ''PEither SOP AllInstances

data PSOPList (a :: Type)
  = PSOPNil
  | PSOPCons (Term a) (Term (PSOPList a))

deriveFor ''PSOPList SOP AllInstances

newtype PSOPOp (a :: Type) (b :: Type)
  = PSopOp (Term (b :~> a))

deriveFor ''PSOPOp SOP NoEqDebug
