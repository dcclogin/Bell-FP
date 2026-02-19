module CHSH.PRState where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.LHV (Lambda(..), Sampler, uniformAll)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative
import System.Random (randomRIO)

------------------------------------------------------------
-- A nonlocal hidden-variable model of the PR box.
-- Instead of Reader Lambda, we use State Lambda, so that the 
-- hidden variable can be modified on-the-fly to enforce the 
-- PR constraint.

newtype PR_StateHV a = PR_StateHV { unPR_StateHV :: State Lambda a }
  deriving newtype (Functor, Applicative, Monad, MonadState Lambda)

instance TrialModel PR_StateHV where
  localA A0 = gets a0
  localA A1 = gets a1
  localB B0 = gets b0
  localB B1 = gets b1

  -- Override jointAB to implement the PR constraint by 
  -- modifying the state on-the-fly.
  jointAB a b = liftA2 (,) (nonlocalA a) (nonlocalB b)
    where
      nonlocalA :: ASetting -> PR_StateHV Outcome
      nonlocalA a = do
        x <- localA a
        modify (\lam -> case a of
          A0 -> lam { b0 = x, b1 = x }
          A1 -> lam { b0 = x, b1 = flipOutcome x })
        pure x
      nonlocalB :: BSetting -> PR_StateHV Outcome
      nonlocalB b = do
        y <- localB b
        modify (\lam -> case b of
          B0 -> lam { a0 = y, a1 = y }
          B1 -> lam { a0 = y, a1 = flipOutcome y })
        pure y


allLambdas :: [Lambda]
allLambdas = 
  [ Lambda x0 x1 y0 y1 | 
    x0 <- allOutcomes
  , x1 <- allOutcomes
  , y0 <- allOutcomes
    , y1 <- allOutcomes 
  ]
  where allOutcomes = [Minus, Plus]

------------------------------------------------------------
-- Membrane: embed an PR trial into the experiment monad Exp e, 
-- by sampling a λ for each trial using a provided sampler.
  
runTrialPR_StateHV :: Monad e => Sampler e -> RunTrial e PR_StateHV
runTrialPR_StateHV sampleLam (PR_StateHV s) =
  ReaderT $ \i -> do
    lam <- sampleLam i
    pure $ evalState s lam

------------------------------------------------------------
-- CHSH Tests
  
testPR_StateHV :: Int -> Sampler IO -> Schedule IO -> IO Double
testPR_StateHV n samp sched =
  chsh n sched (runTrialPR_StateHV samp)

testPR_StateHV_fixed :: Int -> Sampler IO -> IO Double
testPR_StateHV_fixed n samp = 
  testPR_StateHV n samp fixedSchedule

testPR_StateHV_random :: Int -> Sampler IO -> IO Double
testPR_StateHV_random n samp = 
  testPR_StateHV n samp randomScheduleIO

testPR_StateHV_uniform_fixed :: IO Double
testPR_StateHV_uniform_fixed = 
  testPR_StateHV 20000 uniformAll fixedSchedule
-- ~4.0

testPR_StateHV_uniform_random :: IO Double
testPR_StateHV_uniform_random = 
  testPR_StateHV 20000 uniformAll randomScheduleIO
-- ~4.0

------------------------------------------------------------
-- No-signaling checks

noSignalingPR_StateHV :: IO (Bool, String)
noSignalingPR_StateHV = 
  noSignalingReport 20000 0.02 (runTrialPR_StateHV uniformAll)

noSignalingPR_StateHV_print :: IO () 
noSignalingPR_StateHV_print = prettyReport =<< noSignalingPR_StateHV

  