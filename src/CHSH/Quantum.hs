module CHSH.Quantum where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Monad.IO.Class
import System.Random (randomRIO)

------------------------------------------------------------
-- Quantum trial model (sampling from the correct joint distribution)

newtype Quantum a = Quantum { unQuantum :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runQuantum :: Quantum a -> IO a
runQuantum = unQuantum

-- Membrane: run the trial in IO (step index is irrelevant here).
runTrialQuantum :: RunTrial IO Quantum
runTrialQuantum _ = runQuantum

------------------------------------------------------------
-- Angles that maximize CHSH for E(a,b)=cos(2(a-b))
-- A0 = 0, A1 = π/4
-- B0 = π/8, B1 = -π/8
--
-- Then CHSH = 2√2 (in expectation).

piD :: Double
piD = pi

thetaA :: ASetting -> Double
thetaA A0 = 0
thetaA A1 = piD / 4

thetaB :: BSetting -> Double
thetaB B0 = piD / 8
thetaB B1 = -piD / 8

-- Correlation for the |Φ+> polarization-style model:
-- E = cos(2(θA - θB))
corr :: ASetting -> BSetting -> Double
corr a b = cos (2 * (thetaA a - thetaB b))

-- Given correlation E in [-1,1], generate (A,B) with:
--   P(A=B)   = (1+E)/2
--   P(A≠B)   = (1-E)/2
-- and unbiased marginals.
samplePairWithCorr :: Double -> IO (Outcome, Outcome)
samplePairWithCorr e = do
  base <- randomOutcome                 
  u    <- randomRIO (0.0, 1.0 :: Double)
  let pSame = (1 + e) / 2
  if u < pSame
    then pure (base, base)
    else pure (base, flipOutcome base)

instance TrialModel Quantum where
  localA _ = Quantum randomOutcome
  localB _ = Quantum randomOutcome
  -- Special jointAB: correlated sampling, depends on both settings,
  -- but keeps marginals uniform (no-signaling).
  jointAB a b = Quantum $ samplePairWithCorr (corr a b)

testQuantum_fixed :: IO Double
testQuantum_fixed = chsh 200000 fixedSchedule runTrialQuantum

testQuantum_random :: IO Double
testQuantum_random = chsh 200000 randomScheduleIO runTrialQuantum

noSignalingQuantum :: IO (Bool, String)
noSignalingQuantum = noSignalingReport 20000 0.02 runTrialQuantum

uniformMarginalsQuantum :: IO (Bool, String)
uniformMarginalsQuantum = do
  let n   = 20000
      eps = 0.03

  pA00 <- pAPlus n runTrialQuantum A0 B0
  pA01 <- pAPlus n runTrialQuantum A0 B1
  pA10 <- pAPlus n runTrialQuantum A1 B0
  pA11 <- pAPlus n runTrialQuantum A1 B1

  pB00 <- pBPlus n runTrialQuantum A0 B0
  pB10 <- pBPlus n runTrialQuantum A1 B0
  pB01 <- pBPlus n runTrialQuantum A0 B1
  pB11 <- pBPlus n runTrialQuantum A1 B1

  let closeHalf p = abs (p - 0.5) <= eps
      ok = and (map closeHalf [pA00,pA01,pA10,pA11,pB00,pB01,pB10,pB11])

      msg = unlines
        [ "Uniform marginal check (should be ~0.5)"
        , "--------------------------------------"
        , "Alice: P(+|A0,B0)=" ++ show pA00 ++ "  P(+|A0,B1)=" ++ show pA01
        , "       P(+|A1,B0)=" ++ show pA10 ++ "  P(+|A1,B1)=" ++ show pA11
        , "Bob:   P(+|A0,B0)=" ++ show pB00 ++ "  P(+|A1,B0)=" ++ show pB10
        , "       P(+|A0,B1)=" ++ show pB01 ++ "  P(+|A1,B1)=" ++ show pB11
        , "eps = " ++ show eps ++ "   n = " ++ show n
        ]

  pure (ok, msg)

testNoSignalQuantum :: IO ()
testNoSignalQuantum = do
  r <- noSignalingQuantum
  prettyReport r

testUniformQuantum :: IO ()
testUniformQuantum = do
  r <- uniformMarginalsQuantum
  prettyReport r

