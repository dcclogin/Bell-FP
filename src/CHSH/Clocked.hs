module CHSH.Clocked where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import Control.Monad.Reader
import Control.Monad.Identity


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
-- exactly 4.0

test3_loophole_closed :: Double
test3_loophole_closed =
  runIdentity (chsh 20000 fixedSchedule runTrialClockTight)
-- exactly 2.0

test3_random_loophole_open :: IO Double
test3_random_loophole_open =
  chsh 20000 randomScheduleIO runTrialClockLoose
-- around 1.0

test3_random_loophole_closed :: IO Double
test3_random_loophole_closed =
  chsh 20000 randomScheduleIO runTrialClockTight
-- around 2.0

------------------------------------------------------------
-- Reports: the same four runs, with their no-signaling verdicts.
-- (Before, the clocked boxes were never checked; a separate check
-- at fixed settings would have passed them, since Bob then answers
-- Minus on every fourth step whatever the settings.)

report3_loophole_open :: Report
report3_loophole_open =
  runIdentity (chshReport 80000 fixedSchedule runTrialClockLoose)
-- CHSH 4.0; Bob answers Minus exactly in context (A1,B1), so his
-- marginal at B1 depends on Alice's setting

report3_loophole_closed :: Report
report3_loophole_closed =
  runIdentity (chshReport 80000 fixedSchedule runTrialClockTight)
-- CHSH 2.0, no-signaling

report3_random_loophole_open :: IO Report
report3_random_loophole_open =
  seeded 2 (chshReport 80000 randomScheduleIO runTrialClockLoose)
-- CHSH ~1.0, no-signaling: the clock is independent of the settings

report3_random_loophole_closed :: IO Report
report3_random_loophole_closed =
  seeded 3 (chshReport 80000 randomScheduleIO runTrialClockTight)
-- CHSH 2.0, no-signaling

------------------------------------------------------------
