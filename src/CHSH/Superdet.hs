module CHSH.Superdet where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling (noSignalingReport)
import CHSH.LHV (Lambda(..), LHV(..))

import Control.Monad.Identity
import Control.Monad.Reader

------------------------------------------------------------
-- Superdeterminism: lambda may depend on settings (a,b).
-- It saves the locality assumption (conjunction of parameter 
-- and outcome independence) at the cost of lambda-independence.
-- By allowing λ to depend on (a,b), we can "cheat" and achieve 
-- the algebraic maximum 4 for CHSH, even under random scheduling. 


-- "Illegal" sampler: lambda may depend on the chosen settings.
type BadSampler e = Int -> (ASetting, BSetting) -> e Lambda

-- Superdet CHSH runner: uses ONE schedule draw per trial,
-- and passes that same (a,b) to the bad sampler.
chshSuperdet
  :: Monad e
  => Int
  -> Schedule e
  -> BadSampler e
  -> e Double
chshSuperdet n sched badLam =
  (*4) <$> mcAverageIx n trial
  where
    trial i = do
      (a,b) <- runReaderT sched i
      lam   <- badLam i (a,b)
      let xy = runReader (unLHV (jointAB a b)) lam
          v  = trialValue xy
      pure $ case (a,b) of
        (A1,B1) -> -v
        _       ->  v

------------------------------------------------------------
-- A maximally cheating badLam: force the CHSH term to be +1 always.
-- For (A1,B1) we want trialValue = -1 so the "-" term becomes +1.
-- Otherwise we want trialValue = +1 directly.

cheatLam :: Applicative e => BadSampler e
cheatLam _ (a,b) =
  pure $ case (a,b) of
    (A1,B1) -> Lambda Plus Plus Plus Minus
    _       -> Lambda Plus Plus Plus Plus

-- Control: a "legal" sampler ignores (a,b); this is an ordinary LHV choice.
legalAllPlus :: Applicative e => BadSampler e
legalAllPlus _ _ = pure (Lambda Plus Plus Plus Plus)

------------------------------------------------------------
-- Membrane: embed a superdeterministic trial into the experiment 
-- monad Exp e, by sampling a λ for each trial using a provided 
-- illegal sampler that allows λ to depend on (a,b).

runTrialSuperdet :: Monad e => Schedule e -> BadSampler e -> RunTrial e LHV
runTrialSuperdet sched badLam (LHV r) =
  ReaderT $ \i -> do
    (a,b) <- runReaderT sched i
    lam   <- badLam i (a,b)
    pure (runReader r lam)

------------------------------------------------------------
-- CHSH Tests

testSuperdet_fixed :: Double
testSuperdet_fixed =
  runIdentity (chshSuperdet 20000 fixedSchedule cheatLam)
-- ~4.0

testSuperdet_random :: IO Double
testSuperdet_random =
  chshSuperdet 20000 randomScheduleIO cheatLam
-- ~4.0 (superdet correlations persist under randomization by definition)

testSuperdet_control :: Double
testSuperdet_control =
  runIdentity (chshSuperdet 20000 fixedSchedule legalAllPlus)
-- 2.0 exactly


------------------------------------------------------------
-- No-signaling checks

noSignalingSuperdet :: IO (Bool, String)
noSignalingSuperdet = 
  noSignalingReport 20000 0.02 (runTrialSuperdet fixedSchedule cheatLam)

noSignalingSuperdet_print :: IO ()
noSignalingSuperdet_print = prettyReport =<< noSignalingSuperdet
