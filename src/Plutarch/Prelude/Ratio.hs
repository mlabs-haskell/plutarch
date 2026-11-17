{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Prelude.Ratio
  ( PRational (PRational),
    prational,
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
import Plutarch.Primitive.BuiltinFun
  ( pappendString,
    pdivideInteger,
    pmodInteger,
  )
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.DerivedFun (pand, pif)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Multiplicative
  ( PMultiplicativeMonoid (pone),
    PMultiplicativeSemigroup (pmultiply, ppowPositive),
    (#*),
  )
import Plutarch.Primitive.Representation (pfundamental, punsafeSecondary, (#), (#$))
import Plutarch.Primitive.Singular (PInteger, PPositive, (:~>))
import Plutarch.Primitive.Term (perror, pfix, plam')
import Plutarch.TH.Strategy
  ( Strategy (MogensenScott),
    WhichInstances (AllInstances),
    deriveFor,
  )

-- | @since 2.0.0
data PRational = PRationalUnsafe (Term PInteger) (Term PPositive)

deriveFor ''PRational MogensenScott AllInstances

-- | @since 2.0.0
instance PAdditiveSemigroup PRational where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      let newDen = xd #* yd
          newNum = (pscalePositive # yd # xn) #+ (pscalePositive # xd # yn)
       in prational newNum newDen
  pscalePositive = plam' $ \p -> plam' $ \r -> pmatch r $ \(PRational xn xd) ->
    prational (pscalePositive # p # xn) xd

-- | @since 2.0.0
instance PAdditiveMonoid PRational where
  pzero = pcon . PRationalUnsafe pzero $ pone

-- | @since 2.0.0
instance PAdditiveGroup PRational where
  pnegate = plam' $ \r -> pmatch r $ \(PRational xn xd) ->
    pcon . PRationalUnsafe (pnegate # xn) $ xd
  psubtract = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      let newDen = xd #* yd
          newNum = (pscalePositive # yd # xn) #- (pscalePositive # xd # yn)
       in prational newNum newDen

-- | @since 2.0.0
instance PMultiplicativeSemigroup PRational where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational xn xd) ->
    pmatch y $ \(PRational yn yd) ->
      prational (xn #* yn) (xd #* yd)
  ppowPositive = plam' $ \p -> plam' $ \r -> pmatch r $ \(PRational xn xd) ->
    pcon . PRationalUnsafe (ppowPositive # p # xn) $ ppowPositive # p # xd

-- | @since 2.0.0
instance PMultiplicativeMonoid PRational where
  pone = pcon . PRationalUnsafe pone $ pone

-- | @since 2.0.0
pattern PRational :: Term PInteger -> Term PPositive -> PRational
pattern PRational n d <- PRationalUnsafe n d

{-# COMPLETE PRational #-}

-- | Performs reduction.
--
-- @since 2.0.0
prational :: Term PInteger -> Term PPositive -> Term PRational
prational n d =
  -- Note (Koz, 17/11/2026): We know that `d` cannot be 0, and we can thus save
  -- a comparison and loop step by essentially 'forcing' the first reduction of
  -- the Euclidean algorithm to happen straightaway.
  let d' = pfundamental d
      gcd' = getGcd # d' #$ pmodInteger # n # d'
   in pcon
        . PRationalUnsafe (pdivideInteger # n # gcd')
        . punsafeSecondary
        $ pdivideInteger # d' # gcd'

-- Helpers

getGcd :: Term (PInteger :~> PInteger :~> PInteger)
getGcd = pfix $ \self -> plam' $ \n -> plam' $ \d ->
  pif
    (d #== pconstant 0)
    n
    (self # d #$ pmodInteger # n # d)
