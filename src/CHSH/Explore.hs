module CHSH.Explore where

import CHSH.Util
import CHSH.Experiment

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader

------------------------------------------------------------
-- Identity monad with fixed outcomes
-- CHSH = 2

instance TrialModel Identity where
  localA a = if a == A0 then pure Plus else pure Minus
  localB b = if b == B0 then pure Minus else pure Plus

-- Trial is pure Identity inside any experiment monad
liftIdentity :: Applicative e => RunTrial e Identity
liftIdentity ta = ReaderT (\_ -> pure (runIdentity ta))

test1_fixed :: Double
test1_fixed = runIdentity (chsh 20000 fixedSchedule liftIdentity)
-- expect 2.0 exactly (deterministic)

test1_random :: IO Double
test1_random = chsh 20000 randomScheduleIO liftIdentity
-- expect ~2.0

------------------------------------------------------------
-- Signaling model (within-trial communication)
-- We model Alice writing her setting and Bob reading it.
-- This violates locality within a trial, so it hits 4 no matter the schedule.

-- breaks parameter independence

data Shared = Shared { lastASetting :: Maybe ASetting }

instance TrialModel (State Shared) where
  localA a = do
    modify' (\s -> s { lastASetting = Just a })
    pure Minus
  localB b = do
    ma <- gets lastASetting
    pure $ case (ma, b) of
      (Just A1, B1) -> Plus
      _             -> Minus

runTrialState :: RunTrial Identity (State Shared)
runTrialState t = ReaderT (\_ -> Identity (evalState t (Shared Nothing)))

test2_fixed :: Double
test2_fixed = runIdentity (chsh 20000 fixedSchedule runTrialState) 
-- ~4.0

------------------------------------------------------------
-- Cross-trial loophole: trial can read a shared clock (step index)

newtype Clocked a = Clocked { unClocked :: Reader Int a }
  deriving (Functor, Applicative, Monad, MonadReader Int)

instance TrialModel Clocked where
  localA _ = pure Plus
  localB _ = do
    i <- ask
    pure $ if i `mod` 4 == 3 then Minus else Plus

-- Loophole open: reveal the clock to the trial (polymorphic)
runTrialClockLoose :: Applicative e => RunTrial e Clocked
runTrialClockLoose (Clocked r) = ReaderT (\i -> pure (runReader r i))

-- Loophole closed: hide the clock (always run trial at i = 0) (polymorphic)
runTrialClockTight :: Applicative e => RunTrial e Clocked
runTrialClockTight (Clocked r) = ReaderT (\_ -> pure (runReader r 0))

test3_loophole_open :: Double
test3_loophole_open =
  runIdentity (chsh 20000 fixedSchedule runTrialClockLoose)

test3_loophole_closed :: Double
test3_loophole_closed =
  runIdentity (chsh 20000 fixedSchedule runTrialClockTight)

test3_random_loophole_open :: IO Double
test3_random_loophole_open =
  chsh 20000 randomScheduleIO runTrialClockLoose

test3_random_loophole_closed :: IO Double
test3_random_loophole_closed =
  chsh 20000 randomScheduleIO runTrialClockTight

------------------------------------------------------------
