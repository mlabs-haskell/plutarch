module Derivation (goldens) where

import Plutarch.Prelude
import Plutarch.Test.Golden (Evaluation (EvaluateUPLC), releaseGolden)
import TH.MS (PTheseMS)
import TH.SOP (PEither, PSOPList (PSOPCons, PSOPNil), PThese)
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
        case4
    ]

-- Cases

case4 :: forall (s :: S). Term s PBool
case4 = peq # ell1 # ell2
  where
    ell1 :: Term s (PSOPList PInteger)
    ell1 = pcon PSOPNil
    ell2 :: Term s (PSOPList PInteger)
    ell2 =
      pcon
        ( PSOPCons
            (pconstant @PInteger 3)
            ( pcon
                ( PSOPCons
                    (pconstant @PInteger 2)
                    (pcon (PSOPCons (pconstant @PInteger 1) (pcon PSOPNil)))
                )
            )
        )
