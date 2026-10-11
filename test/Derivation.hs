{-# LANGUAGE OverloadedStrings #-}

module Derivation (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden
  ( Evaluation (EvaluateUPLC),
    debugGolden,
    releaseGolden,
  )
import Plutarch.Unsafe (punsafeCoerce)
import TH.Data
  ( PListData (PConsData, PNilData),
    PTheseData (PTheseData),
    PTriple (PTriple),
  )
import TH.Enum (PColour (PRed))
import TH.MS (PTheseMS (PTheseMS))
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
        case8,
      debugGolden
        EvaluateUPLC
        "pshow @(PTheseData PInteger PByteString)"
        "Derivation Case 9"
        case9,
      debugGolden
        EvaluateUPLC
        "pshow @(PTheseMS PInteger PBool)"
        "Derivation Case 10"
        case10,
      releaseGolden
        EvaluateUPLC
        "pfromData @PColour"
        "Derivation Case 11"
        (pfromData @PColour . punsafeCoerce $ piData # pconstant 1),
      releaseGolden
        EvaluateUPLC
        "ptoData @PColour"
        "Derivation Case 12"
        (ptoData @PColour . pcon $ PRed),
      releaseGolden
        EvaluateUPLC
        "ptoData @(PTriple PBool)"
        "Derivation Case 13"
        (ptoData . pcon $ trip),
      releaseGolden
        EvaluateUPLC
        "ptoData @(PTheseData PInteger PByteString)"
        "Derivation Case 14"
        (ptoData pthese),
      debugGolden
        EvaluateUPLC
        "pvalidateData @PColour"
        "Derivation Case 15"
        (pvalidateData @PColour $ pfundamental (piData # pconstant 1)),
      debugGolden
        EvaluateUPLC
        "pvalidateData @(PTriple PBool)"
        "Derivation Case 16"
        (pvalidateData @(PTriple PBool) . pfundamental . ptoData . pcon $ trip),
      debugGolden
        EvaluateUPLC
        "pvalidateData @(PTheseData PInteger PByteString)"
        "Derivation Case 17"
        (pvalidateData @(PTheseData PInteger PByteString) . pfundamental $ pthese),
      debugGolden
        EvaluateUPLC
        "pvalidateData @(PListData PInteger)"
        "Derivation Case 18"
        (pvalidateData @(PListData PInteger) . pfundamental $ plist)
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
case6 = pshow . pcon $ trip

case7 :: forall (s :: S). Term s PString
case7 =
  pshow
    . pcon @(PEither PInteger PByteString)
    . PLeft
    . pconstant @PInteger
    $ 2

case8 :: forall (s :: S). Term s PString
case8 = pshow . pcon $ sopList

case9 :: forall (s :: S). Term s PString
case9 = pshow pthese

case10 :: forall (s :: S). Term s PString
case10 =
  pshow
    . pcon @(PTheseMS PInteger PBool)
    . PTheseMS (pconstant 2)
    $ pfalse

-- Helpers

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

trip :: forall (s :: S). PTriple PBool s
trip = PTriple (ptoData pfalse) (ptoData pfalse) (ptoData pfalse)

pthese :: forall (s :: S). Term s (PTheseData PInteger PByteString)
pthese =
  pcon @(PTheseData PInteger PByteString)
    . PTheseData (ptoData . pconstant $ 2)
    . ptoData
    . pconstant
    $ "foo"

plist :: forall (s :: S). Term s (PListData PInteger)
plist =
  pcon
    . PConsData (ptoData . pconstant $ 1)
    . ptoData
    . pcon
    . PConsData (ptoData . pconstant $ 2)
    . ptoData
    . pcon
    $ PNilData
