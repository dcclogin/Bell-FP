module CHSH.Explore where

import CHSH.Util
import CHSH.Experiment

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader

------------------------------------------------------------
-- This module corresponds to Section 2 of the paper, 
-- where we explore
-- (1) boxes as pure functions (Identity monad).
-- (2) boxes as shared state (State monad).
-- (3) a scheduler-induced loophole (Clocked).

------------------------------------------------------------
-- Identity monad with fixed outcomes
-- CHSH = 2

instance TrialModel Identity where
  localA _ = pure Plus
  localB _ = pure Plus

chshId :: Int
chshId = chshOf (\a b -> trialValue (runIdentity (jointAB a b)))

-- ghci> chshId
-- 2


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

chshST :: Int
chshST = chshOf (\a b -> trialValue (evalState (jointAB a b) (Shared Nothing)))

-- ghci> chshST
-- 4


------------------------------------------------------------
-- A schedule-induced, cross-trial loophole: 
-- trial can read a shared clock (step index)

newtype Clocked a = Clocked { unClocked :: Reader Int a }
  deriving (Functor, Applicative, Monad, MonadReader Int)

instance TrialModel Clocked where
  localA _ = pure Plus
  localB _ = do
    i <- ask
    pure $ if i `mod` 4 == 3 then Minus else Plus


runTrialCI, runTrialCZ :: Applicative e => RunTrial e Clocked
runTrialCI (Clocked r) = ReaderT (\i -> pure (runReader r i))
runTrialCZ (Clocked r) = ReaderT (\_ -> pure (runReader r 0))

test3CI, test3CZ :: Double
test3CI = runIdentity (chsh 20000 fixedSchedule runTrialCI)
test3CZ = runIdentity (chsh 20000 fixedSchedule runTrialCZ)

test3RCI, test3RCZ :: IO Double
test3RCI = chsh 20000 randomScheduleIO runTrialCI
test3RCZ = chsh 20000 randomScheduleIO runTrialCZ

-- expect 4 results to be ~1.0, ~2.0, ~2.0, and ~4.0, in some order
-- revealed in Clocked.hs test3* functions

------------------------------------------------------------

