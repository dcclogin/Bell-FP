module CHSH.PRState where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.LHV (Lambda(..), Sampler, uniformAll, plus2)

import Control.Monad.State

------------------------------------------------------------
-- A nonlocal hidden-variable model of the PR box.
-- Instead of Reader Lambda, we use State Lambda, so that the
-- hidden variable can be modified on-the-fly to enforce the
-- PR constraint.

newtype PRHiddenState a = PRHiddenState { unPRHiddenState :: State Lambda a }
  deriving newtype (Functor, Applicative, Monad, MonadState Lambda)

-- Each box reads its own entry of λ, as in CHSH.LHV, and then
-- overwrites both of the other party's entries with its outcome,
-- flipped in one of them when it measured its second setting.
-- jointAB keeps its default, liftA2 (,) (localA a) (localB b):
-- Alice's box and then Bob's, so Bob reads what Alice wrote, and his
-- outcome is hers, flipped in context (A1,B1). Bob's box rewrites
-- Alice's entries in the same way, but she has already answered.
-- (Before: the local methods only read, and jointAB paired two
-- computations, nonlocalA and nonlocalB, that read and wrote.)
instance TrialModel PRHiddenState where
  localA A0 = do
    x <- gets a0
    modify (\lam -> lam { b0 = x, b1 = x })
    pure x
  localA A1 = do
    x <- gets a1
    modify (\lam -> lam { b0 = x, b1 = flipOutcome x })
    pure x
  localB B0 = do
    y <- gets b0
    modify (\lam -> lam { a0 = y, a1 = y })
    pure y
  localB B1 = do
    y <- gets b1
    modify (\lam -> lam { a0 = y, a1 = flipOutcome y })
    pure y

------------------------------------------------------------
-- Membrane: embed an PR trial into the experiment monad Exp e,
-- by sampling a λ for each trial using a provided sampler.

runTrialPRHiddenState :: Functor e => Sampler e -> RunTrial e PRHiddenState
runTrialPRHiddenState samp (PRHiddenState s) = evalState s <$> samp

------------------------------------------------------------
-- CHSH Tests

testPRHiddenState :: Int -> Sampler IO -> Scheduler IO -> IO Double
testPRHiddenState n samp sched =
  chsh n sched (runTrialPRHiddenState samp)

testPRHiddenState_fixed :: Int -> Sampler IO -> IO Double
testPRHiddenState_fixed n samp =
  testPRHiddenState n samp fixedSchedule

testPRHiddenState_random :: Int -> Sampler IO -> IO Double
testPRHiddenState_random n samp =
  testPRHiddenState n samp randomScheduleIO

testPRHiddenState_uniform_fixed :: IO Double
testPRHiddenState_uniform_fixed =
  testPRHiddenState 20000 uniformAll fixedSchedule
-- 4.0

testPRHiddenState_uniform_random :: IO Double
testPRHiddenState_uniform_random =
  testPRHiddenState 20000 uniformAll randomScheduleIO
-- 4.0

------------------------------------------------------------
-- Reports: the CHSH value and the no-signaling verdict of the same
-- run. (Before, noSignalingPRHiddenState ran separate experiments
-- with the uniform sampler.)

reportPRHiddenState_uniform_fixed :: IO Report
reportPRHiddenState_uniform_fixed =
  chshReport 80000 fixedSchedule (runTrialPRHiddenState uniformAll)
-- CHSH 4.0, no-signaling. The cycling sampler correlates Bob's
-- entries of λ with the settings, but nonlocalA overwrites them
-- before Bob reads them, so the table is the PR box.

reportPRHiddenState_uniform_random :: IO Report
reportPRHiddenState_uniform_random =
  seeded 11 (chshReport 80000 randomScheduleIO (runTrialPRHiddenState uniformAll))
-- CHSH 4.0, no-signaling

reportPRHiddenState_plus2_fixed :: IO Report
reportPRHiddenState_plus2_fixed =
  chshReport 80000 fixedSchedule (runTrialPRHiddenState plus2)
-- CHSH 4.0, but Bob's marginal at B1 depends on Alice's setting:
-- no-signaling is a property of the uniform sampler, not of the model
