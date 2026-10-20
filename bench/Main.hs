module Main (main) where

import Maybe
  ( PMSMaybe (PMSJust, PMSNothing),
    PMaybeNew (PJustNew, PNothingNew),
    PMaybeOld (PJustOld, PNothingOld),
    PSOPMaybe (PSOPJust, PSOPNothing),
    pfindCPS,
    pfindMS,
    pfindSOP,
  )
import Plutarch.Prelude
import Plutarch.Test.Size (noLargerThan)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import These
  ( PThese (PThat, PThese, PThis),
    PTheseNow (PThatNow, PTheseNow, PThisNow),
  )

main :: IO ()
main =
  defaultMain . testGroup "Benchmarks" $
    [ noLargerThan
        "SOP Maybe vs MS Maybe rep"
        (pcon . PSOPJust . pconstant @PInteger $ 3)
        (pcon . PMSJust . pconstant @PInteger $ 3),
      noLargerThan
        @(PInteger :--> PSOPMaybe PInteger)
        @(PInteger :--> PMSMaybe PInteger)
        "pcon SOP Maybe vs MS Maybe"
        (plam' $ pcon . PSOPJust)
        (plam' $ pcon . PMSJust),
      noLargerThan
        "pmatch SOP Maybe vs MS Maybe"
        ( plam' $ \x -> pmatch x $ \case
            PSOPNothing -> pconstant @PInteger 0
            PSOPJust y -> y
        )
        ( plam' $ \x -> pmatch x $ \case
            PMSNothing -> pconstant @PInteger 0
            PMSJust y -> y
        ),
      expectFail $
        noLargerThan
          "SOP pfind vs CPS pfind"
          ( plam' $ \ell -> pmatch (pfindSOP (plam' $ \e -> e #== pconstant 3) ell) $ \case
              PSOPNothing -> pconstant @PInteger (-1)
              PSOPJust x -> x
          )
          (plam' $ \ell -> pfindCPS (plam' $ \e -> e #== pconstant 3) ell (pconstant @PInteger (-1)) (plam' id)),
      expectFail $
        noLargerThan
          "MS pfind vs CPS pfind"
          ( plam' $ \ell -> pmatch (pfindMS (plam' $ \e -> e #== pconstant 3) ell) $ \case
              PMSNothing -> pconstant @PInteger (-1)
              PMSJust x -> x
          )
          (plam' $ \ell -> pfindCPS (plam' $ \e -> e #== pconstant 3) ell (pconstant @PInteger (-1)) (plam' id)),
      noLargerThan
        @(PMaybeNew PBool :--> PInteger :--> (PAsData PBool :--> PInteger) :--> PInteger)
        @(PMaybeOld PBool :--> PInteger :--> (PAsData PBool :--> PInteger) :--> PInteger)
        "case on Constr vs current (PMaybe)"
        ( plam' $ \t -> plam' $ \handleNothing -> plam' $ \handleJust -> pmatch t $ \case
            PNothingNew -> handleNothing
            PJustNew x -> handleJust # x
        )
        ( plam' $ \t -> plam' $ \handleNothing -> plam' $ \handleJust -> pmatch t $ \case
            PNothingOld -> handleNothing
            PJustOld x -> handleJust # x
        ),
      expectFail $
        noLargerThan
          @(PThese PBool PBool :--> (PAsData PBool :--> PInteger) :--> (PAsData PBool :--> PInteger) :--> PInteger)
          @(PTheseNow PBool PBool :--> (PAsData PBool :--> PInteger) :--> (PAsData PBool :--> PInteger) :--> PInteger)
          "case on Constr vs current (PThese)"
          ( plam' $ \t -> plam' $ \handleX -> plam' $ \handleY -> pmatch t $ \case
              PThis x -> handleX # x
              PThat y -> handleY # y
              PThese x y -> (handleX # x) #+ (handleY # y)
          )
          ( plam' $ \t -> plam' $ \handleX -> plam' $ \handleY -> pmatch t $ \case
              PThisNow x -> handleX # x
              PThatNow y -> handleY # y
              PTheseNow x y -> (handleX # x) #+ (handleY # y)
          )
    ]
