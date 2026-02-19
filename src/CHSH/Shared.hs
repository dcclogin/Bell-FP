module CHSH.Shared where

import CHSH.Util
import CHSH.Experiment
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
chshST = evalState
  (do v00 <- val A0 B0
      v01 <- val A0 B1
      v10 <- val A1 B0
      v11 <- val A1 B1
      pure $ v00 + v01 + v10 - v11)
  (Shared Nothing)
    where val a b = do
            xy <- jointAB a b
            pure (trialValue xy)


runTrialState :: RunTrial Identity (State Shared)
runTrialState t = ReaderT (\_ -> Identity (evalState t (Shared Nothing)))

test2_fixed :: Double
test2_fixed = runIdentity (chsh 20000 fixedSchedule runTrialState) 
-- ~4.0