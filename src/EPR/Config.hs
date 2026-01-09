module EPR.Config where

import System.Random hiding (uniform)
import ProbabilityMonads (Dist, uniform, prodDist)

--- | synonym for True and False
tt, ff :: Bool
tt = True
ff = False

--- | A = Alice, B = Bob
data IA = A deriving (Show, Eq, Ord, Enum, Bounded)
data IB = B deriving (Show, Eq, Ord, Enum, Bounded)


qA :: Dist IA
qA = pure A

qB :: Dist IB
qB = pure B

--- | joint distribution q(ia, ib)
qAB :: Dist (IA, IB)
qAB = qA `prodDist` qB


--- | helper to rearrange distributions
rearrange_em :: Dist ((IA, IB), (Bool, Bool)) -> Dist ((IA, Bool), (IB, Bool))
rearrange_em d = do
    ((ia, ib), (oa, ob)) <- d
    return ((ia, oa), (ib, ob))