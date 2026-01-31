module CHSH.Superdet where

-- Sampler looks at settings first
-- breaks lambda independence
-- contrast with LHV with has sampler but that do not depend
-- on settings

import CHSH.Syntax
import CHSH.LHV (Lambda(..), LHV(..))  
import CHSH.Explore (fixedSchedule, randomScheduleIO)

import Control.Monad.Identity
import Control.Monad.Reader

-- "Illegal" sampler: λ may depend on (a,b).
type BadSampler e = Int -> (ASetting,BSetting) -> e Lambda

-- An "illegal membrane" that feeds the chosen (a,b) into λ.
runTrialSuperdet
  :: Monad e
  => Schedule e
  -> BadSampler e
  -> RunTrial e LHV
runTrialSuperdet sched badLam i (LHV r) = do
  ab  <- sched i
  lam <- badLam i ab
  pure (runReader r lam)

-- A maximally cheating badLam: pick λ that forces the CHSH term to be +1 always.
-- This achieves CHSH = 4 classically.
cheatLam :: Applicative e => BadSampler e
cheatLam _ (a,b) =
  pure $ case (a,b) of
    (A1,B1) -> Lambda Plus Plus Plus Minus  -- make A*B = -1 so the "-" term becomes +1
    _       -> Lambda Plus Plus Plus Plus   -- make A*B = +1


-- runIdentity (chsh n fixedSchedule (runTrialSuperdet fixedSchedule cheatLam))
-- tends to 4 (no “special jointAB” needed; the leak is in λ-dependence)

------------------------------------------------------------
-- Tests

-- With superdeterministic λ(a,b), we can reach 4 even with applicative jointAB.
testSuperdet_fixed :: Double
testSuperdet_fixed =
  runIdentity (chsh 20000 fixedSchedule (runTrialSuperdet fixedSchedule cheatLam))

-- No cheating now
testSuperdet_random :: IO Double
testSuperdet_random =
  chsh 20000 randomScheduleIO (runTrialSuperdet randomScheduleIO cheatLam)

-- Control: if you replace cheatLam with a legal sampler (ignores a,b), you drop back ≤2.
-- For example, always pick all-plus λ:
legalAllPlus :: Applicative e => BadSampler e
legalAllPlus _ _ = pure (Lambda Plus Plus Plus Plus)

testSuperdet_control :: Double
testSuperdet_control =
  runIdentity (chsh 20000 fixedSchedule (runTrialSuperdet fixedSchedule legalAllPlus))
-- should be exactly 2

