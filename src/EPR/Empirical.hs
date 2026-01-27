module EPR.Empirical where

import Config
import EPR.Config
import ProbabilityMonads


--- | Empirical Model for EPR scenario
--- ================================================


--- | |01⟩ + |10⟩ state
emParamAB :: (IA, IB) -> Dist (Bool, Bool)
emParamAB (A, B) = uniform [ (tt, ff), (ff, tt) ]


--- | joint distribution q((ia, ib), (oa, ob))
em :: Dist ((IA, IB), (Bool, Bool))
em = jointFromCond qAB emParamAB

--- | marginal distribution q(ia, oa)
emA :: Dist (IA, Bool)
emA = rearrange_em em >>= return . fst

--- | marginal distribution q(ib, ob)
emB :: Dist (IB, Bool)
emB = rearrange_em em >>= return . snd


emParamA :: IA -> Dist Bool
emParamA = condFromJoint emA

emParamB :: IB -> Dist Bool
emParamB = condFromJoint emB


--- | failed attempt to recover the original joint distribution (fully mixed)
emParamAB_mixed :: (IA, IB) -> Dist (Bool, Bool)
emParamAB_mixed (A, B) = emParamA A `prodDist` emParamB B



--- | tests
--- ================================================


printEPR :: IO ()
printEPR = do
    print em


printMixed :: IO ()
printMixed = do
    print (jointFromCond qAB emParamAB_mixed)


