{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.SOP
  ( PThese (..),
    PEither (..),
    PSOPList (..),
  )
where

import Data.Kind (Type)
import Plutarch.Prelude

data PThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThis (Term s a)
  | PThat (Term s b)
  | PThese (Term s a) (Term s b)

deriveFor ''PThese SOP

data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)

deriveFor ''PEither SOP

data PSOPList (a :: S -> Type) (s :: S)
  = PSOPNil
  | PSOPCons (Term s a) (Term s (PSOPList a))

deriveFor ''PSOPList SOP
