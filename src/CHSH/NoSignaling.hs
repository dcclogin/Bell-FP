module CHSH.NoSignaling where

import CHSH.Util
import CHSH.Experiment

import Data.List (intercalate)
import Control.Monad.Reader (runReaderT)

------------------------------------------------------------
-- Estimators

-- Run n trials at fixed settings (a,b), returning the list of outcomes.
sampleAB
  :: (Monad e, TrialModel t)
  => Int
  -> RunTrial e t
  -> ASetting
  -> BSetting
  -> e [(Outcome, Outcome)]
sampleAB n runTrial a b =
  mapM (\i -> runReaderT (runTrial (jointAB a b)) i) [0 .. n-1]

pAPlus
  :: (Monad e, TrialModel t)
  => Int
  -> RunTrial e t
  -> ASetting
  -> BSetting
  -> e Double
pAPlus n runTrial a b = do
  xys <- sampleAB n runTrial a b
  let k = length [ () | (x,_) <- xys, x == Plus ]
  pure (fromIntegral k / fromIntegral n)

pBPlus
  :: (Monad e, TrialModel t)
  => Int
  -> RunTrial e t
  -> ASetting
  -> BSetting
  -> e Double
pBPlus n runTrial a b = do
  xys <- sampleAB n runTrial a b
  let k = length [ () | (_,y) <- xys, y == Plus ]
  pure (fromIntegral k / fromIntegral n)

------------------------------------------------------------
-- No-signaling checks

-- Alice no-signaling: P(A=Plus | a, B0) ~= P(A=Plus | a, B1)
checkNoSignalA
  :: (Monad e, TrialModel t)
  => Int
  -> Double           -- tolerance, e.g. 0.02 for n=20000
  -> RunTrial e t
  -> ASetting
  -> e (Bool, (Double, Double))
checkNoSignalA n eps runTrial a = do
  p0 <- pAPlus n runTrial a B0
  p1 <- pAPlus n runTrial a B1
  pure (abs (p0 - p1) <= eps, (p0, p1))

-- Bob no-signaling: P(B=Plus | A0, b) ~= P(B=Plus | A1, b)
checkNoSignalB
  :: (Monad e, TrialModel t)
  => Int
  -> Double
  -> RunTrial e t
  -> BSetting
  -> e (Bool, (Double, Double))
checkNoSignalB n eps runTrial b = do
  p0 <- pBPlus n runTrial A0 b
  p1 <- pBPlus n runTrial A1 b
  pure (abs (p0 - p1) <= eps, (p0, p1))

-- Convenience: check all four no-signaling equalities and return a small report.
noSignalingReport
  :: (Monad e, TrialModel t)
  => Int
  -> Double
  -> RunTrial e t
  -> e (Bool, String)
noSignalingReport n eps runTrial = do
  (okA0,(a00,a01)) <- checkNoSignalA n eps runTrial A0
  (okA1,(a10,a11)) <- checkNoSignalA n eps runTrial A1
  (okB0,(b00,b10)) <- checkNoSignalB n eps runTrial B0
  (okB1,(b01,b11)) <- checkNoSignalB n eps runTrial B1

  let ok = and [okA0, okA1, okB0, okB1]
      msg =
        intercalate "\n"
          [ "Alice A0: P(+|A0,B0)=" ++ show a00 ++ "  P(+|A0,B1)=" ++ show a01
          , "Alice A1: P(+|A1,B0)=" ++ show a10 ++ "  P(+|A1,B1)=" ++ show a11
          , "Bob   B0: P(+|A0,B0)=" ++ show b00 ++ "  P(+|A1,B0)=" ++ show b10
          , "Bob   B1: P(+|A0,B1)=" ++ show b01 ++ "  P(+|A1,B1)=" ++ show b11
          , "eps = " ++ show eps ++ "   n = " ++ show n
          ]
  pure (ok, msg)

------------------------------------------------------------