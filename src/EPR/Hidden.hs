module EPR.Hidden where

import Config
import EPR.Config
import ProbabilityMonads


--- | only two points in the space of hidden variables
data Λ = Λ1 | Λ2 deriving (Show, Eq, Ord)


uniformΛ :: Dist Λ
uniformΛ = uniform [ Λ1, Λ2 ]


--- | determination functions, pure (producing anti-correlated outcomes)
f :: Λ -> (IA -> Bool, IB -> Bool)
f Λ1 = (fa, fb)
    where
        fa A = tt
        fb B = ff
f Λ2 = (fa, fb)
    where
        fa A = ff
        fb B = tt