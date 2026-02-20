module CHSH.PRState where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.LHV (Lambda(..), Sampler, uniformAll)

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

------------------------------------------------------------
-- A nonlocal hidden-variable model of the PR box.
-- Instead of Reader Lambda, we use State Lambda, so that the 
-- hidden variable can be modified on-the-fly to enforce the 
-- PR constraint.

newtype PRHiddenState a = PRHiddenState { unPRHiddenState :: State Lambda a }
  deriving newtype (Functor, Applicative, Monad, MonadState Lambda)

instance TrialModel PRHiddenState where
  localA A0 = gets a0
  localA A1 = gets a1
  localB B0 = gets b0
  localB B1 = gets b1

  jointAB a b = liftA2 (,) (nonlocalA a) (nonlocalB b)
    where
      nonlocalA :: ASetting -> PRHiddenState Outcome
      nonlocalA a = do
        x <- localA a
        modify (\lam -> case a of
          A0 -> lam { b0 = x, b1 = x }
          A1 -> lam { b0 = x, b1 = flipOutcome x })
        pure x
      nonlocalB :: BSetting -> PRHiddenState Outcome
      nonlocalB b = do
        y <- localB b
        modify (\lam -> case b of
          B0 -> lam { a0 = y, a1 = y }
          B1 -> lam { a0 = y, a1 = flipOutcome y })
        pure y

------------------------------------------------------------
-- Membrane: embed an PR trial into the experiment monad Exp e, 
-- by sampling a λ for each trial using a provided sampler.
  
runTrialPRHiddenState :: Monad e => Sampler e -> RunTrial e PRHiddenState
runTrialPRHiddenState sampleLam (PRHiddenState s) =
  ReaderT $ \i -> do
    lam <- sampleLam i
    pure $ evalState s lam

------------------------------------------------------------
-- CHSH Tests
  
testPRHiddenState :: Int -> Sampler IO -> Schedule IO -> IO Double
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
-- ~4.0

testPRHiddenState_uniform_random :: IO Double
testPRHiddenState_uniform_random = 
  testPRHiddenState 20000 uniformAll randomScheduleIO
-- ~4.0

------------------------------------------------------------
-- No-signaling checks

noSignalingPRHiddenState :: IO (Bool, String)
noSignalingPRHiddenState = 
  noSignalingReport 20000 0.02 (runTrialPRHiddenState uniformAll)

noSignalingPRHiddenState_print :: IO () 
noSignalingPRHiddenState_print = prettyReport =<< noSignalingPRHiddenState

  