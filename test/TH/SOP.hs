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

data PThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThis (Term s a)
  | PThat (Term s b)
  | PThese (Term s a) (Term s b)

deriveFor ''PThese SOP AllInstances

data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)

deriveFor ''PEither SOP AllInstances

data PSOPList (a :: S -> Type) (s :: S)
  = PSOPNil
  | PSOPCons (Term s a) (Term s (PSOPList a))

deriveFor ''PSOPList SOP AllInstances

newtype PSOPOp (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PSopOp (Term s (b :--> a))

deriveFor ''PSOPOp SOP NoEqDebug
