module Plutarch.Helpers.Numeric (pbySquaring) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.Term (S, Term, toSomeTerm)
import Plutarch.Primitive.BuiltinFun qualified as BF
import Plutarch.Primitive.DerivedFun (pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Representation (PlutarchType, (#))
import Plutarch.Primitive.Singular (PInteger, (:-->))
import Plutarch.Primitive.Term (pfix, plam', punsafeCase)

pbySquaring ::
  forall (a :: S -> Type) (s :: S).
  (PlutarchType a) =>
  Term s (a :--> a :--> a) ->
  Term s PInteger ->
  Term s a ->
  Term s a
pbySquaring f e b = go # e
  where
    go :: Term s (PInteger :--> a)
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
