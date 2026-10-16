module Main (main) where

import Additive qualified
import Bool qualified
import Compiled qualified
import Compose qualified
import Constr qualified
import Debug qualified
import Derivation qualified
import Error qualified
import Pretty qualified
import PullArray qualified
import Term qualified
import Test.Tasty (defaultMain, testGroup)
import Trace qualified

main :: IO ()
main =
  defaultMain . testGroup "Goldens" $
    [ Term.goldens,
      Bool.goldens,
      Additive.goldens,
      Compiled.goldens,
      Error.goldens,
      Compose.goldens,
      Pretty.goldens,
      Derivation.goldens,
      Trace.goldens,
      Debug.goldens,
      PullArray.goldens,
      Constr.goldens
    ]
