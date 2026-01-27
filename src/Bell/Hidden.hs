module Bell.Hidden where

import Config
import Bell.Config
import ProbabilityMonads


data Λ = Λ (Bool, Bool, Bool) deriving (Show, Eq, Ord)

--- | determination functions, pure
δ1, δ2, δ3 :: Λ -> Bool
δ1 (Λ (l1, _, _)) = l1
δ2 (Λ (_, l2, _)) = l2
δ3 (Λ (_, _, l3)) = l3

--- | vector of determination functions
δ :: Λ -> (Bool, Bool, Bool)
δ λ = (δ1 λ, δ2 λ, δ3 λ)

--- | more general <instruction sets> 
--- | (see Abramsky "relational hidden variables" 12.4)
f :: Λ -> (IA -> Bool, IB -> Bool)
f λ = (fa, fb)
    where
        fa A1 = δ1 λ
        fa A2 = δ2 λ
        fa A3 = δ3 λ
        fb B1 = δ1 λ  -- not (δ1 λ) if anti-correlated
        fb B2 = δ2 λ  -- not (δ2 λ) if anti-correlated
        fb B3 = δ3 λ  -- not (δ3 λ) if anti-correlated


--- | λ-independence

--- | uniform distribution over all 8 possible λ
uniformΛ :: Dist Λ
uniformΛ = uniform [ Λ (l1, l2, l3) | l1 <- [tt, ff], l2 <- [tt, ff], l3 <- [tt, ff] ]

--- | lower bound: no (true, true, true) or (false, false, false)
uniformΛ_lb :: Dist Λ
uniformΛ_lb = Dist [ (Λ (l1, l2, l3), 1/6) 
    | l1 <- [tt, ff]
    , l2 <- [tt, ff]
    , l3 <- [tt, ff]
    , (l1, l2, l3) /= (tt, tt, tt)
    , (l1, l2, l3) /= (ff, ff, ff) ]

--- | upper bound: only (true, true, true) or (false, false, false)
uniformΛ_ub :: Dist Λ
uniformΛ_ub = uniform [ Λ (tt, tt, tt), Λ (ff, ff, ff) ]