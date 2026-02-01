module CHSH.Superdet where

import CHSH.Util
import CHSH.Experiment
import CHSH.LHV (Lambda(..), LHV(..))

import Control.Monad.Identity
import Control.Monad.Reader

------------------------------------------------------------
-- Superdeterminism: lambda may depend on (a,b)
-- This breaks lambda-independence (measurement independence).

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
-- Tests

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