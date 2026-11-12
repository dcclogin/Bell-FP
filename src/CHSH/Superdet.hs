module CHSH.Superdet where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.LHV (Lambda(..), LHV, Sampler, plus2, runTrialLHV)

import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

------------------------------------------------------------
-- Superdeterminism: lambda may depend on settings (a,b).
-- It saves the locality assumption (conjunction of parameter
-- and outcome independence) at the cost of lambda-independence.
-- By allowing λ to depend on (a,b), we can "cheat" and achieve
-- the algebraic maximum 4 for CHSH, even under random scheduling.


-- "Illegal" sampler: lambda may depend on the chosen settings.
-- Given the settings of the step, it is an instruction of the
-- experiment layer, like a legal sampler; a legal sampler is a bad
-- one that ignores the settings (const samp).
-- (Before: type BadSampler e = Int -> (ASetting, BSetting) -> e Lambda.)
type BadSampler e = Context -> Exp e Lambda

-- The membrane of a superdeterministic model at the settings s of
-- its trial: the LHV membrane, with λ drawn by the bad sampler at s.
-- The trial hands s over (CHSH.Experiment.trial), so the run goes
-- through the same trial as every other model.
-- (In revision 3, trialSuperdet ran the boxes without a membrane; in
-- the submission, chshSuperdet did, and a membrane of the same name,
-- runTrialSuperdet sched badLam, re-ran the schedule to find s.)
runTrialSuperdet :: Functor e => BadSampler e -> Context -> RunTrial e LHV
runTrialSuperdet bad s = runTrialLHV (bad s)

-- Superdet CHSH runner: the CHSH value of such a run.
chshSuperdet
  :: Monad e
  => Int
  -> Scheduler e
  -> BadSampler e
  -> e Double
chshSuperdet n sched bad =
  chshOf . correlator . empirical <$> experiment n (trial sched (runTrialSuperdet bad))

-- ... and the full report of such a run: same table, both verdicts.
reportSuperdet
  :: Monad e
  => Int
  -> Scheduler e
  -> BadSampler e
  -> e Report
reportSuperdet n sched bad = report n (trial sched (runTrialSuperdet bad))

------------------------------------------------------------
-- A maximally cheating bad sampler: force the CHSH term to be +1
-- always. For (A1,B1) we want trialValue = -1 so the "-" term
-- becomes +1. Otherwise we want trialValue = +1 directly.

cheatLam :: Applicative e => BadSampler e
cheatLam (a,b) =
  pure $ case (a,b) of
    (A1,B1) -> Lambda Plus Plus Plus Minus
    _       -> Lambda Plus Plus Plus Plus

-- Control: a legal sampler ignores (a,b); this is an ordinary LHV choice.
legalAllPlus :: Applicative e => BadSampler e
legalAllPlus _ = plus2

-- A sampler that sees the settings and still keeps both marginals
-- fair: one coin, handed to Bob flipped in context (A1,B1). Its
-- runs reproduce the PR box, CHSH value 4 included, and they are
-- no-signaling: no test on the table can detect this measurement
-- dependence.
cheatNS :: BadSampler IO
cheatNS (a, b) = liftIO $ do
  r <- randomOutcome
  let y = if a == A1 && b == B1 then flipOutcome r else r
  pure (Lambda r r y y)

------------------------------------------------------------
-- The step is shared. A sampler of the legal type may run the
-- schedule itself, at the same step, and pass the settings it gets
-- to a bad sampler. Nothing in its type says so.

peek :: Monad e => Scheduler e -> BadSampler e -> Sampler e
peek sched bad = sched >>= bad

-- runTrialLHV (peek sched bad) is the membrane runTrialSuperdet
-- sched badLam of the submission, which re-ran the schedule. Under
-- fixedSchedule, a function of the step, peek finds the settings of
-- the trial and the run is cheatLam's: CHSH 4. Under
-- randomScheduleIO it draws settings of its own, independent of the
-- trial's, and the cheat is gone: CHSH 2.

------------------------------------------------------------
-- CHSH Tests

testSuperdet_fixed :: Double
testSuperdet_fixed =
  runIdentity (chshSuperdet 20000 fixedSchedule cheatLam)
-- 4.0

testSuperdet_random :: IO Double
testSuperdet_random =
  chshSuperdet 20000 randomScheduleIO cheatLam
-- 4.0 (superdet correlations persist under randomization by definition)

testSuperdet_control :: Double
testSuperdet_control =
  runIdentity (chshSuperdet 20000 fixedSchedule legalAllPlus)
-- 2.0 exactly

------------------------------------------------------------
-- Reports

reportSuperdet_fixed :: Report
reportSuperdet_fixed = runIdentity (reportSuperdet 80000 fixedSchedule cheatLam)
-- CHSH 4.0, and Bob's marginal at B1 depends on Alice's setting:
-- at B1 he answers Plus next to A0 and Minus next to A1

reportSuperdet_random :: IO Report
reportSuperdet_random = seeded 4 (reportSuperdet 80000 randomScheduleIO cheatLam)
-- CHSH 4.0; signals likewise

reportSuperdet_control :: Report
reportSuperdet_control = runIdentity (reportSuperdet 80000 fixedSchedule legalAllPlus)
-- CHSH 2.0, no-signaling

reportSuperdet_cheatNS :: IO Report
reportSuperdet_cheatNS = seeded 10 (reportSuperdet 80000 randomScheduleIO cheatNS)
-- CHSH 4.0, no-signaling: the PR box, from a sampler that sees the settings

reportPeek_fixed :: Report
reportPeek_fixed =
  runIdentity (chshReport 80000 fixedSchedule (runTrialLHV (peek fixedSchedule cheatLam)))
-- CHSH 4.0, Bob at B1 signals: cheatLam's run, from a legal type

reportPeek_random :: IO Report
reportPeek_random =
  seeded 4 (chshReport 80000 randomScheduleIO (runTrialLHV (peek randomScheduleIO cheatLam)))
-- CHSH ~2.0, no-signaling: the peeked settings are not the trial's
