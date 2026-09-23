{-# LANGUAGE OverloadedStrings #-}

module Derivation (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (EvaluateUPLC),
    debugGolden,
    releaseGolden,
  )
import TH.Data (PTriple (PTriple))
import TH.Enum (PColour (PRed))
import TH.MS (PTheseMS)
import TH.SOP (PEither (PLeft), PSOPList (PSOPCons, PSOPNil), PThese)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Derivation"
    [ releaseGolden
        EvaluateUPLC
        "peq @(PThese PInteger PInteger)"
        "Derivation Case 1"
        (peq @(PThese PInteger PInteger)),
      releaseGolden
        EvaluateUPLC
        "peq @(PEither PInteger PByteString)"
        "Derivation Case 2"
        (peq @(PEither PInteger PInteger)),
      releaseGolden
        EvaluateUPLC
        "peq @(PTheseMS PInteger PInteger)"
        "Derivation Case 3"
        (peq @(PTheseMS PInteger PInteger)),
      releaseGolden
        EvaluateUPLC
        "peq @(PSOPList PInteger)"
        "Derivation Case 4"
        case4,
      debugGolden
        EvaluateUPLC
        "pshow @PColour"
        "Derivation Case 5"
        (pshow @PColour . pcon $ PRed),
      debugGolden
        EvaluateUPLC
        "pshow @(PTriple PBool)"
        "Derivation Case 6"
        case6,
      debugGolden
        EvaluateUPLC
        "pshow @(PEither PInteger PByteString)"
        "Derivation Case 7"
        case7,
      debugGolden
        EvaluateUPLC
        "pshow @(PSOPList PInteger)"
        "Derivation Case 8"
        case8
    ]

-- Cases

case4 :: forall (s :: S). Term s PBool
case4 = peq # ell1 # ell2
  where
    ell1 :: Term s (PSOPList PInteger)
    ell1 = pcon PSOPNil
    ell2 :: Term s (PSOPList PInteger)
    ell2 = pcon sopList

case6 :: forall (s :: S). Term s PString
case6 =
  pshow . pcon $
    PTriple
      (ptoData pfalse)
      (ptoData pfalse)
      (ptoData pfalse)

case7 :: forall (s :: S). Term s PString
case7 =
  pshow
    . pcon @(PEither PInteger PByteString)
    . PLeft
    . pconstant @PInteger
    $ 2

case8 :: forall (s :: S). Term s PString
case8 = pshow . pcon $ sopList

sopList :: forall (s :: S). PSOPList PInteger s
sopList =
  PSOPCons
    (pconstant @PInteger 3)
    ( pcon
        ( PSOPCons
            (pconstant @PInteger 2)
            (pcon (PSOPCons (pconstant @PInteger 1) (pcon PSOPNil)))
        )
    )
