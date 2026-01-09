module Bell.Empirical where

import Bell.Config
import ProbabilityMonads


--- | Empirical Model for Bell-Mermin scenario
--- ================================================

--- | same choice -> perfectly correlated: 1/2 (T,T), 1/2 (F,F)
--- | diff choice -> 1/8 (T,T), 3/8 (T,F), 3/8 (F,T), 1/8 (F,F)

--- | parameterized/conditional empirical model : q(oa, ob | ia, ib)
emParamAB :: (IA, IB) -> Dist (Bool, Bool)
emParamAB (ia, ib) 
    | sameChoice ia ib 
        = uniform [ (tt, tt), (ff, ff) ]
    | otherwise       
        = Dist [ ( (tt, tt), 1/8 )
               , ( (tt, ff), 3/8 )
               , ( (ff, tt), 3/8 )
               , ( (ff, ff), 1/8 ) ]


--- | full empirical model : q(ia, ib, oa, ob) = q(ia, ib) * q(oa, ob|ia, ib)
em :: Dist ((IA, IB), (Bool, Bool))
em = jointFromCond qAB emParamAB

--- | rearranged full empirical model : q(ia, oa, ib, ob)
em' :: Dist ((IA, Bool), (IB, Bool))
em' = rearrange_em em

--- | marginal empirical model for A : q(ia, oa)
emA :: Dist (IA, Bool)
emA = fst <$> em'

--- | marginal empirical model for B : q(ib, ob)
emB :: Dist (IB, Bool)
emB = snd <$> em'

--- | parameterized marginal for A : q(oa | ia) = q(ia, oa) / q(ia)
emParamA :: IA -> Dist Bool
emParamA = condFromJoint emA

--- | parameterized marginal for B : q(ob | ib) = q(ib, ob) / q(ib)
emParamB :: IB -> Dist Bool
emParamB = condFromJoint emB


--- | failed attempt to restore emParamAB via `prodDist` (fully mixed)
--- | q(oa, ob | ia, ib) =? q(oa | ia) * q(ob | ib)
emParamAB_mixed :: (IA, IB) -> Dist (Bool, Bool)
emParamAB_mixed (ia, ib) = (emParamA ia) `prodDist` (emParamB ib)