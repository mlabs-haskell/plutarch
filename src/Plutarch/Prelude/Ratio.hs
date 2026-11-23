{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | @since 2.0.0
module Plutarch.Prelude.Ratio
  ( PRational (PRational),
    prational,
    punsafeRational,
    precip,
  )
where

import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Additive
  ( PAdditiveGroup (pnegate, psubtract),
    PAdditiveMonoid (pzero),
    PAdditiveSemigroup (padd, pscalePositive),
    (#+),
    (#-),
  )
import Plutarch.Primitive.BuiltinFun (pappendString, pdivideInteger, pmodInteger)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pand, pif, pisPositive)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Multiplicative
  ( PMultiplicativeMonoid (pone),
    PMultiplicativeSemigroup (pmultiply, ppowPositive),
    (#*),
  )
import Plutarch.Primitive.Ord (POrd (ple, plt))
import Plutarch.Primitive.Representation
  ( pfundamental,
    punsafeSecondary,
    (#),
    (#$),
  )
import Plutarch.Primitive.Singular (PInteger, PPositive, (:~>))
import Plutarch.Primitive.Term (perror, pfix, plam')
import Plutarch.TH.Strategy
  ( Strategy (MogensenScott),
    WhichInstances (AllInstances),
    deriveFor,
  )

-- | @since 2.0.0
data PRational = PUnsafeRational (Term PInteger) (Term PPositive)

deriveFor ''PRational MogensenScott AllInstances

-- | @since 2.0.0
instance POrd PRational where
  plt = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) -> plt # (pscalePositive # yd # xn) #$ pscalePositive # xd # yn
  ple = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) -> ple # (pscalePositive # yd # xn) #$ pscalePositive # xd # yn

-- | @since 2.0.0
instance PAdditiveSemigroup PRational where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      let newN = (pscalePositive # yd # xn) #+ (pscalePositive # xd # yn)
          newD = xd #* yd
       in prational newN newD
  pscalePositive = plam' $ \p -> plam' $ \r -> pmatch r $ \(PRational n d) ->
    prational (pscalePositive # p # n) d

-- | @since 2.0.0
instance PAdditiveMonoid PRational where
  pzero = punsafeRational pzero pone

-- | @since 2.0.0
instance PAdditiveGroup PRational where
  pnegate = plam' $ \r -> pmatch r $ \(PRational n d) ->
    punsafeRational (pnegate # n) d
  psubtract = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      let newN = (pscalePositive # yd # xn) #- (pscalePositive # xd # yn)
          newD = xd #* yd
       in prational newN newD

-- | @since 2.0.0
instance PMultiplicativeSemigroup PRational where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      prational (xn #* yn) (xd #* yd)
  ppowPositive = plam' $ \p -> plam' $ \r -> pmatch r $ \(PRational n d) ->
    punsafeRational (ppowPositive # p # n) (ppowPositive # p # d)

-- | @since 2.0.0
instance PMultiplicativeMonoid PRational where
  pone = punsafeRational pone pone

-- | @since 2.0.0
pattern PRational :: Term PInteger -> Term PPositive -> PRational
pattern PRational n d <- PUnsafeRational n d

{-# COMPLETE PRational #-}

-- | Performs reduction.
--
-- @since 2.0.0
prational :: Term PInteger -> Term PPositive -> Term PRational
prational n d =
  let d' = pfundamental d
      -- Note (Koz, 23/11/2026): Because we know `d'` cannot be 0,
      -- we effectively force the first reduction step to happen
      -- immediately.
      reduction = pgcd # d' #$ pmodInteger # n # d'
      newN = pdivideInteger # n # reduction
      newD = punsafeSecondary $ pdivideInteger # d' # reduction
   in punsafeRational newN newD

-- | Does /not/ perform reduction - use with care.
--
-- @since 2.0.0
punsafeRational :: Term PInteger -> Term PPositive -> Term PRational
punsafeRational n = pcon . PUnsafeRational n

-- | Will error if asked for the reciprocal of 'pzero'.
--
-- @since 2.0.0
precip :: Term PRational -> Term PRational
precip r = pmatch r $ \(PRational n d) ->
  pif
    (n #== pzero)
    perror
    ( pif
        (pisPositive n)
        (punsafeRational (pfundamental d) . punsafeSecondary $ n)
        (punsafeRational (pnegate # pfundamental d) . punsafeSecondary $ pnegate # n)
    )

-- Helpers

pgcd :: Term (PInteger :~> PInteger :~> PInteger)
pgcd = pfix $ \self -> plam' $ \n -> plam' $ \d ->
  pif
    (d #== pzero)
    n
    (self # d #$ pmodInteger # n # d)
