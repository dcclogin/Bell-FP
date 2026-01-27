module Bell.Config where

import Config
import System.Random hiding (uniform)
import ProbabilityMonads (Dist, uniform, prodDist)


--- | A = Alice, B = Bob
data IA = A1 | A2 | A3 deriving (Show, Eq, Ord, Enum, Bounded)
data IB = B1 | B2 | B3 deriving (Show, Eq, Ord, Enum, Bounded)

instance Measurement IA where
    allMeasurements = [A1, A2, A3]
instance Measurement IB where
    allMeasurements = [B1, B2, B3]
instance (Measurement m1, Measurement m2) => Measurement (m1, m2) where
    allMeasurements = [ (m1, m2) | m1 <- allMeasurements, m2 <- allMeasurements ]


--- | distributions of measurement choices
--- ================================================

qA :: Dist IA
qA = uniform [ A1, A2, A3 ]

qB :: Dist IB
qB = uniform [ B1, B2, B3 ]

qAB :: Dist (IA, IB)
qAB = qA `prodDist` qB


--- | other helpers
--- ================================================

sameChoice :: IA -> IB -> Bool
sameChoice A1 B1 = True
sameChoice A2 B2 = True
sameChoice A3 B3 = True
sameChoice _  _  = False


--- | helper to rearrange distributions
rearrange_em :: Dist ((IA, IB), (Bool, Bool)) -> Dist ((IA, Bool), (IB, Bool))
rearrange_em d = do
    ((ia, ib), (oa, ob)) <- d
    return ((ia, oa), (ib, ob))


--- | generate a random (ia : IA)
randomA :: IO IA
randomA = do
    let choices = allMeasurements :: [IA]
    idx <- randomRIO (0, length choices - 1)
    return (choices !! idx)

--- | generate a random (ib : IB)
randomB :: IO IB
randomB = do
    let choices = allMeasurements :: [IB]
    idx <- randomRIO (0, length choices - 1)
    return (choices !! idx)

--- | generate a random (ia : IA, ib : IB)
randomAB :: IO (IA, IB)
randomAB = (,) <$> randomA <*> randomB


{--
--- | generate same choice (ia : IA, ib : IB)
randomAB_same :: IO (IA, IB)
randomAB_same = do
    ia <- randomA
    let ib = case ia of
            A1 -> B1
            A2 -> B2
            A3 -> B3
    return (ia, ib)
--}


suchThat :: IO a -> (a -> Bool) -> IO a
suchThat action pred = do
    r <- action
    if pred r
        then return r
        else suchThat action pred

--- | easy generate same choice (ia : IA, ib : IB)
randomAB_same :: IO (IA, IB)
randomAB_same = randomAB `suchThat` (\(ia, ib) -> sameChoice ia ib)

--- | easy generate different choice (ia : IA, ib : IB)
randomAB_diff :: IO (IA, IB)
randomAB_diff = randomAB `suchThat` (\(ia, ib) -> not (sameChoice ia ib))
