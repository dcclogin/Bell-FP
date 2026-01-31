module CHSH.Experiment where

import CHSH.Util

import Control.Applicative (liftA2)
import System.Random (randomRIO)

------------------------------------------------------------
-- Trial semantics: to be able to express the constraints
-- on the internals of the black boxes, we will use a monad
-- interface. Different choices of monads will express
-- different constraints on the black boxes.

class Monad t => TrialModel t where
  -- Alice’s response to her local setting
  localA :: ASetting -> t Outcome
  -- Bob’s response to his local setting
  localB :: BSetting -> t Outcome
  -- Compute joint response: default is the applicative pairing
  jointAB :: ASetting -> BSetting -> t (Outcome, Outcome)
  jointAB a b = liftA2 (,) (localA a) (localB b)

------------------------------------------------------------
-- Experiment semantics: to be able to express the constraints
-- across different trials, we will another monad.
-- To run each trial, we take the step index 'i', a
-- computation for the trial, and return a computation
-- in the experiment monad

type RunTrial e t = forall a. Int -> t a -> e a

------------------------------------------------------------
-- The experiment chooses a schedule, i.e. for each
-- step 'i', it chooses a setting (possibly using 
-- memory, random number generation, etc)

type Schedule e = Int -> e (ASetting, BSetting)

fixedSchedule :: Monad m => Schedule m
fixedSchedule i = pure $ case i `mod` 4 of
  0 -> (A0,B0)
  1 -> (A0,B1)
  2 -> (A1,B0)
  _ -> (A1,B1)

randomScheduleIO :: Schedule IO
randomScheduleIO _ = do
  ra <- randomRIO (0 :: Int, 1)
  rb <- randomRIO (0 :: Int, 1)
  pure (if ra == 0 then A0 else A1, if rb == 0 then B0 else B1)

-- CHSH correlation

chsh :: (Monad e, TrialModel t) =>
        Int -> Schedule e -> RunTrial e t -> e Double
chsh n sched runTrial =
  (*4) <$> mcAverageIx n trial
  where
    trial i = do
      (a,b) <- sched i
      xy    <- runTrial i $ jointAB a b
      let v = trialValue xy
      pure $ case (a,b) of
        (A1,B1) -> -v
        _       ->  v

------------------------------------------------------------
