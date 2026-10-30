module Plutarch.Helpers.Numeric (pbySquaring) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PInteger, (:~>))
import Plutarch.Primitive.Term (pfix, plam', punsafeCase)

pbySquaring ::
  forall (a :: Type).
  (PlutarchType a) =>
  Term (a :~> a :~> a) ->
  Term PInteger ->
  Term a ->
  Term a
pbySquaring f e b = go # e
  where
    go :: Term (PInteger :~> a)
    go = pfix $ \self -> plam' $ \currE ->
      pif
        (e #== pconstant 1)
        b
        ( let halfE = BF.pquotientInteger # currE # pconstant 2
              rec = self # halfE
           in punsafeCase (BF.premainderInteger # currE # pconstant 2)
                . NEVector.cons (toSomeTerm $ f # rec # rec)
                . NEVector.singleton
                . toSomeTerm
                $ f # (f # rec # rec) # b
        )
