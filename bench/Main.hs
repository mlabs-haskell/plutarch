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
        (plam1 $ pcon . PSOPJust)
        (plam1 $ pcon . PMSJust),
      noLargerThan
        "pmatch SOP Maybe vs MS Maybe"
        ( plam1 $ \x -> pmatch x $ \case
            PSOPNothing -> pconstant @PInteger 0
            PSOPJust y -> y
        )
        ( plam1 $ \x -> pmatch x $ \case
            PMSNothing -> pconstant @PInteger 0
            PMSJust y -> y
        ),
      expectFail $
        noLargerThan
          "SOP pfind vs CPS pfind"
          ( plam1 $ \ell -> pmatch (pfindSOP (plam1 $ \e -> e #== pconstant 3) ell) $ \case
              PSOPNothing -> pconstant @PInteger (-1)
              PSOPJust x -> x
          )
          (plam1 $ \ell -> pfindCPS (plam1 $ \e -> e #== pconstant 3) ell (pconstant @PInteger (-1)) (plam1 id)),
      expectFail $
        noLargerThan
          "MS pfind vs CPS pfind"
          ( plam1 $ \ell -> pmatch (pfindMS (plam1 $ \e -> e #== pconstant 3) ell) $ \case
              PMSNothing -> pconstant @PInteger (-1)
              PMSJust x -> x
          )
          (plam1 $ \ell -> pfindCPS (plam1 $ \e -> e #== pconstant 3) ell (pconstant @PInteger (-1)) (plam1 id)),
      noLargerThan
        @(PMaybeNew PBool :--> PInteger :--> (PAsData PBool :--> PInteger) :--> PInteger)
        @(PMaybeOld PBool :--> PInteger :--> (PAsData PBool :--> PInteger) :--> PInteger)
        "case on Constr vs current (PMaybe)"
        ( plam3 $ \t handleNothing handleJust -> pmatch t $ \case
            PNothingNew -> handleNothing
            PJustNew x -> handleJust # x
        )
        ( plam3 $ \t handleNothing handleJust -> pmatch t $ \case
            PNothingOld -> handleNothing
            PJustOld x -> handleJust # x
        ),
      expectFail $
        noLargerThan
          @(PThese PBool PBool :--> (PAsData PBool :--> PInteger) :--> (PAsData PBool :--> PInteger) :--> PInteger)
          @(PTheseNow PBool PBool :--> (PAsData PBool :--> PInteger) :--> (PAsData PBool :--> PInteger) :--> PInteger)
          "case on Constr vs current (PThese)"
          ( plam3 $ \t handleX handleY -> pmatch t $ \case
              PThis x -> handleX # x
              PThat y -> handleY # y
              PThese x y -> (handleX # x) #+ (handleY # y)
          )
          ( plam3 $ \t handleX handleY -> pmatch t $ \case
              PThisNow x -> handleX # x
              PThatNow y -> handleY # y
              PTheseNow x y -> (handleX # x) #+ (handleY # y)
          )
    ]
