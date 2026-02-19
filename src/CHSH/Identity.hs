module CHSH.Identity where

import CHSH.Util
import CHSH.Experiment
import Control.Monad.Identity
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

testIdentity_fixed :: Double
testIdentity_fixed = runIdentity (chsh 20000 fixedSchedule liftIdentity)
-- expect 2.0 exactly (deterministic)

testIdentity_random :: IO Double
testIdentity_random = chsh 20000 randomScheduleIO liftIdentity
-- expect ~2.0
