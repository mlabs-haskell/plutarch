module Main (main) where

import Maybe
  ( PMSMaybe (PMSJust, PMSNothing),
    PSOPMaybe (PSOPJust, PSOPNothing),
  )
import Plutarch.Prelude
import Plutarch.Test.Size (noLargerThan)
import Test.Tasty (defaultMain, testGroup)

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
        )
    ]
