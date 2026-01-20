module CHSH.Util where

import Control.Monad
import Control.Monad.State.Strict
import System.Random (StdGen, randomR)

import CHSH.DSL

------------------------------------------------------------
-- Device runner for one-shot experiments
------------------------------------------------------------

type RunOneShot m = OneShot -> m (Outcome, Outcome)

------------------------------------------------------------
-- Random sampling in any StdGen state monad
------------------------------------------------------------

randomDouble :: (MonadState StdGen m) => m Double
randomDouble = state (randomR (0.0, 1.0 :: Double))

------------------------------------------------------------
-- Repeat one-shot N times using a given runner
------------------------------------------------------------

repeatOneShotWith
  :: (MonadState StdGen m)
  => RunOneShot m
  -> Int
  -> OneShot
  -> m [(Outcome, Outcome)]
repeatOneShotWith run n expr =
  replicateM n (run expr)

------------------------------------------------------------
-- Monte Carlo mean for a given (a,b) setting
------------------------------------------------------------

estimateMeanWith
  :: (MonadState StdGen m)
  => RunOneShot m
  -> Int        -- number of samples
  -> OneShot
  -> m Double
estimateMeanWith run n expr = do
  xs <- repeatOneShotWith run n expr
  let s = sum (map trialValue xs)
  pure (fromIntegral s / fromIntegral n)

------------------------------------------------------------
-- CHSH meta-protocol, parameterised by a device runner
------------------------------------------------------------

chshWith
  :: (MonadState StdGen m)
  => RunOneShot m   -- ^ how to run ONE (A,B) experiment
  -> Int            -- ^ samples per correlation
  -> m Double
chshWith run n = do
  e00 <- estimateMeanWith run n (MeasureAB A0 B0)
  e01 <- estimateMeanWith run n (MeasureAB A0 B1)
  e10 <- estimateMeanWith run n (MeasureAB A1 B0)
  e11 <- estimateMeanWith run n (MeasureAB A1 B1)
  pure (e00 + e01 + e10 - e11)