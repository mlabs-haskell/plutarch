{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.Ratio
  ( PRationalData (..),
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.BuiltinFun (pappendString)
import Plutarch.Primitive.CanData (PCanData (pfromData))
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.Liftable
  ( PLiftable,
    PLiftableDataList (PLiftableDataList),
    PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
    ReprError (UserSpecified),
    pconstant,
  )
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation
  ( pfundamental,
    (#),
  )
import Plutarch.Primitive.Singular (PAsData, PInteger)
import Plutarch.Primitive.Term (perror)
import Plutarch.TH.Strategy
  ( Strategy (DataList),
    WhichInstances (AllInstances),
    deriveFor,
  )
import PlutusLedgerApi.V4 qualified as PLA
import PlutusTx.IsData.Class qualified as PTxData

-- | = Note
--
-- 'PRationalData' is not normalized, but is reduced. This means that @-2/4@ and
-- @2/-4@ are considered different even though they are not. This is in contrast
-- to @PRational@, which ensures that only @-2/4@ can exist.
--
-- @since 2.0.0
data PRationalData = PRationalData (Term (PAsData PInteger)) (Term (PAsData PInteger))

deriveFor ''PRationalData DataList AllInstances

-- | @since 2.0.0
instance PlutusRepresentable PLA.Rational where
  type AsPlutus PLA.Rational = PLA.Data
  haskToRepr = PTxData.toData

  -- For some reason, this won't autoderive the usual way, but writing it
  -- manually works?
  reprToHask d = case PTxData.fromData d of
    Nothing -> Left . UserSpecified $ "Could not decode Rational from Data."
    Just x -> pure x

-- | @since 2.0.0
deriving via
  (PLiftableDataList PRationalData PLA.Rational)
  instance
    PLiftable PRationalData
