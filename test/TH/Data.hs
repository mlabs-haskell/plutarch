{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Data
  ( PTheseData (..),
    PTriple (..),
    PPairData (..),
    PSmash (..),
  )
where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Prelude
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Unsafe (SomeTerm, punsafeCase, toSomeTerm)

data PTheseData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisData (Term s (PAsData a))
  | PThatData (Term s (PAsData b))
  | PTheseData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PTheseData DataPlutus AllInstances

instance (PCanData a, PCanData b) => PCanData (PTheseData a b)

instance (PValidateData a, PValidateData b) => PValidateData (PTheseData a b) where
  pwithValidated ::
    forall (s :: S).
    Term s PData ->
    (forall (r :: S -> Type). (PlutarchType r) => Term s r -> Term s r)
  pwithValidated d res = pmatch (BF.punConstrData # d) $ \(PBPair tag fields) ->
    pheadTailList fields $ plam' $ \f1 -> plam' $ \t1 ->
      punsafeCase tag
        . NEVector.cons (whenThis f1 t1)
        . NEVector.cons (whenThat f1 t1)
        . NEVector.singleton
        $ whenThese f1 t1
    where
      whenThis :: Term s PData -> Term s (PBList PData) -> SomeTerm s
      whenThis f1 t1 = toSomeTerm $ pwithValidated @a f1 $ pmatch t1 $ \case
        PBNil -> res
        PBCons _ _ -> perror
      whenThat :: Term s PData -> Term s (PBList PData) -> SomeTerm s
      whenThat f1 t1 = toSomeTerm $ pwithValidated @b f1 $ pmatch t1 $ \case
        PBNil -> res
        PBCons _ _ -> perror
      whenThese :: Term s PData -> Term s (PBList PData) -> SomeTerm s
      whenThese f1 t1 = toSomeTerm $ pwithValidated @a f1 $ pheadTailList t1 $ plam' $ \f2 ->
        plam' $ \t2 -> pwithValidated @b f2 $ pmatch t2 $ \case
          PBNil -> res
          PBCons _ _ -> perror

data PTriple (a :: S -> Type) (s :: S)
  = PTriple (Term s (PAsData a)) (Term s (PAsData a)) (Term s (PAsData a))

deriveFor ''PTriple DataList AllInstances

{-
instance (PValidateData a) => PValidateData (PTriple a) where
  pwithValidated d res = pheadTailList (BF.punListData # d) $ plam' $ \f1 ->
    plam' $ \t1 -> pwithValidated @a f1 $ pheadTailList t1 $ plam' $ \f2 ->
      plam' $ \t2 -> pwithValidated @a f2 $ pheadTailList t2 $ plam' $ \f3 ->
        plam' $ \t3 -> pwithValidated @a f3 $ pmatch t3 $ \case
          PBNil -> res
          PBCons _ _ -> perror
-}

data PPairData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PPairData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PPairData DataList NoEqDebug

data PSmash (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PNada
  | PSmash (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PSmash DataPlutus NoEqDebug
