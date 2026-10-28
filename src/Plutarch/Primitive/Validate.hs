{-# LANGUAGE AllowAmbiguousTypes #-}

-- | @since 2.0.0
module Plutarch.Primitive.Validate
  ( PValidateData (..),
    pvalidateData,
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Composite qualified as C
import Plutarch.Primitive.DerivedFun
  ( pheadTailList,
    pif,
    pisNegative,
    pisPositive,
  )
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PAsData, PData)
import Plutarch.Primitive.Singular qualified as S
import Plutarch.Primitive.Term (perror, plam', plet, punsafeCase, punsafeCoerce)

-- | @since 2.0.0
class (PCanData a) => PValidateData (a :: Type) where
  pwithValidated ::
    Term PData ->
    (forall (r :: Type). (PlutarchType r) => Term r -> Term r)

-- | @since 2.0.0
instance PValidateData S.PBool where
  pwithValidated d res = pmatch (BF.punConstrData # d) $ \(C.PBPair tag fields) ->
    pmatch fields $ \case
      C.PBNil ->
        punsafeCase tag
          . NEVector.cons (toSomeTerm res)
          . NEVector.singleton
          . toSomeTerm
          $ res
      C.PBCons _ _ -> perror

-- | @since 2.0.0
instance PValidateData S.PInteger where
  pwithValidated d res = plet (BF.punIData # d) $ const res

-- | @since 2.0.0
instance PValidateData S.PNatural where
  pwithValidated d res =
    let asI = BF.punIData # d
     in pif
          (pisNegative asI)
          perror
          res

-- | @since 2.0.0
instance PValidateData S.PPositive where
  pwithValidated d res =
    let asI = BF.punIData # d
     in pif
          (pisPositive asI)
          res
          perror

-- | @since 2.0.0
instance PValidateData S.PByte where
  pwithValidated d res =
    let asI = BF.punIData # d
     in pif
          (pisNegative asI)
          perror
          ( pif
              (BF.plessThanEqualsInteger # asI # pconstant 255)
              res
              perror
          )

-- | @since 2.0.0
instance PValidateData S.PByteString where
  pwithValidated d res = plet (BF.punBData # d) $ const res

-- | Effectively does nothing.
--
-- @since 2.0.0
instance PValidateData S.PData where
  pwithValidated = const id

-- | @since 2.0.0
instance PValidateData (C.PBList S.PData) where
  pwithValidated d res = plet (BF.punListData # d) $ const res

-- | @since 2.0.0
instance PValidateData (C.PBPair S.PData S.PData) where
  pwithValidated d res = pmatch (BF.punConstrData # d) $ \(C.PBPair tag fields) ->
    punsafeCase tag
      . NEVector.singleton
      . toSomeTerm
      . pheadTailList fields
      $ plam'
      $ \_ -> plam' $ \t ->
        pheadTailList t $
          plam' $ \_ -> plam' $ \t' -> pmatch t' $ \case
            C.PBNil -> res
            C.PBCons _ _ -> perror

-- | @since 2.0.0
pvalidateData ::
  forall (a :: Type).
  (PValidateData a) =>
  Term PData ->
  Term (PAsData a)
pvalidateData d = pwithValidated @a d (punsafeCoerce d)
