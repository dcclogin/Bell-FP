module EPR.LHV where

import EPR.Config
import EPR.Hidden
import ProbabilityMonads


--- | Attempt to realize EPR empirical model with local hidden-variables


--- | p(oa | ia, λ)
lhvmParamA :: Λ -> IA -> Dist Bool
lhvmParamA λ A = pure $ (fst (f λ)) A

--- | p(ob | ib, λ)
lhvmParamB :: Λ -> IB -> Dist Bool
lhvmParamB λ B = pure $ (snd (f λ)) B


--- | p(oa, ob | ia, ib, λ) = p(oa | ia, λ) * p(ob | ib, λ)
lhvmParamAB :: Λ -> (IA, IB) -> Dist (Bool, Bool)
lhvmParamAB λ (ia, ib) = (lhvmParamA λ ia) `prodDist` (lhvmParamB λ ib)


--- | empirical model from LHV model and distribution over Λ
getEmpirical :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpirical dΛ (A, B) = do
    λ <- dΛ
    lhvmParamAB λ (A, B)



--- | tests
--- ================================================

testUniform :: IO ()
testUniform = do
    print $ jointFromCond qAB (getEmpirical uniformΛ)