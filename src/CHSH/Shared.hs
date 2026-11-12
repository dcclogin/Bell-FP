module CHSH.Shared where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Identity

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


runTrialState :: RunTrial Identity (State Shared)
runTrialState t = ReaderT (\_ -> Identity (evalState t (Shared Nothing)))

test2_fixed :: Double
test2_fixed = runIdentity (chsh 20000 fixedSchedule runTrialState)
-- 4.0

------------------------------------------------------------
-- Report: the run signals, as intended. (Before, this model was
-- never shown to the no-signaling checker; it is the negative
-- control the demo lacked.)

reportShared_fixed :: Report
reportShared_fixed = runIdentity (chshReport 80000 fixedSchedule runTrialState)
-- CHSH 4.0; Bob's marginal at B1 depends on Alice's setting
