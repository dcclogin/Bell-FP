module CHSH.Clocked where

import CHSH.Util
import CHSH.Experiment
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
