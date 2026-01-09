module Bell.LHV where

import Bell.Config
import Bell.Hidden
import ProbabilityMonads


--- | Attempt to realize Bell empirical model with local hidden-variables
--- | assuming (1) λ-independence and (2) locality

--- | attempt to realize Bell empirical model with LHV
--- ==================================================

--- | p(oa | ia, λ)
lhvmParamA :: Λ -> IA -> Dist Bool
lhvmParamA λ ia = pure $ (fst (f λ)) ia

--- | p(ob | ib, λ)
lhvmParamB :: Λ -> IB -> Dist Bool
lhvmParamB λ ib = pure $ (snd (f λ)) ib

--- | p(oa, ob | ia, ib, λ) = p(oa | ia, λ) * p(ob | ib, λ)
lhvmParamAB :: Λ -> (IA, IB) -> Dist (Bool, Bool)
lhvmParamAB λ (ia, ib) = (lhvmParamA λ ia) `prodDist` (lhvmParamB λ ib)

{--
--- | p(ia, oa | λ) = p(ia) * p(oa | ia, λ)
lhvmA :: Λ -> Dist (IA, Bool)
lhvmA λ = jointFromCond qA (lhvmParamA λ)

--- | p(ib, ob | λ) = p(ib) * p(ob | ib, λ)
lhvmB :: Λ -> Dist (IB, Bool)
lhvmB λ = jointFromCond qB (lhvmParamB λ)
--}

--- | p(ia, ib, oa, ob | λ) = p(ia, ib) * p(oa, ob | ia, ib, λ)
lhvmAB :: Dist (IA, IB) -> Λ -> Dist ((IA, IB), (Bool, Bool))
lhvmAB dAB λ = jointFromCond dAB (lhvmParamAB λ)

--- | full LHV model: p(λ, ia, ib, oa, ob) = p(λ) * p(ia, ib, oa, ob | λ)
lhvm :: Dist Λ ->  Dist (Λ, ((IA, IB), (Bool, Bool)))
lhvm dΛ = jointFromCond dΛ (lhvmAB qAB)

--- | realization of EM
--- | marginal LHV model: p(ia, ib, oa, ob) = Σλ p(λ, ia, ib, oa, ob)
lhvm2em :: Dist (Λ, ((IA, IB), (Bool, Bool))) -> Dist ((IA, IB), (Bool, Bool))
lhvm2em d = snd <$> d

lhvm2emParamAB :: Dist (Λ, ((IA, IB), (Bool, Bool))) -> (IA, IB) -> Dist (Bool, Bool)
lhvm2emParamAB d (ia, ib) = condFromJoint (lhvm2em d) (ia, ib)

--- | from distribution over Λ to the conditional empirical model
getEmpirical :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpirical dΛ = lhvm2emParamAB (lhvm dΛ)



--- | alternative formulation, without explicitly invoking qAB

--- | full LHV model: p(λ, oa, ob | ia, ib) = p(λ) * p(oa, ob | ia, ib, λ)
lhvm' :: Dist Λ -> (IA, IB) -> Dist (Λ, (Bool, Bool))
lhvm' dΛ (ia, ib) = jointFromCond dΛ (\λ -> lhvmParamAB λ (ia, ib))

--- | realization of EM (alternative)
--- | marginal LHV model: p(oa, ob | ia, ib) = Σλ p(λ, oa, ob | ia, ib)
lhvm2emParamAB' :: ((IA, IB) -> Dist (Λ, (Bool, Bool))) -> (IA, IB) -> Dist (Bool, Bool)
lhvm2emParamAB' f (ia, ib) = snd <$> f (ia, ib)

getEmpirical' :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpirical' dΛ = lhvm2emParamAB' (lhvm' dΛ)




--- | tests
--- ========================================    

testUniformSame :: IO ()
testUniformSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ (ia, ib))

testUniformDiff :: IO ()
testUniformDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ (ia, ib))

testLowerBoundSame :: IO ()
testLowerBoundSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ_lb (ia, ib))

--- | lowest agreement possible on diff choices
testLowerBoundDiff :: IO ()
testLowerBoundDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ_lb (ia, ib))

testUpperBoundSame :: IO ()
testUpperBoundSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ_ub (ia, ib))

--- | always agree on diff choices
testUpperBoundDiff :: IO ()
testUpperBoundDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ_ub (ia, ib))


--- | Alternative tests
--- ========================================

{--
testUniformSame' :: IO ()
testUniformSame' = do
    (ia, ib) <- randomAB_same
    print (lhvm2emParamAB' (lhvm' uniformΛ) (ia, ib))

testUniformDiff' :: IO ()
testUniformDiff' = do
    (ia, ib) <- randomAB_diff
    print (lhvm2emParamAB' (lhvm' uniformΛ) (ia, ib))
--}