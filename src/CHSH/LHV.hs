module CHSH.LHV where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Monad.Reader
import Control.Monad.Identity
import System.Random (randomRIO)

------------------------------------------------------------
-- Local Hidden Variables (LHV) are a class of trial models
-- where the outcomes are determined by a shared random variable λ,
-- which is sampled at the start of each trial and then determines
-- the outcomes for all settings. This is a common class of models
-- considered in discussions of Bell's theorem.

data Lambda = Lambda
  { a0 :: Outcome
  , a1 :: Outcome
  , b0 :: Outcome
  , b1 :: Outcome
  } deriving Eq

newtype LHV a = LHV { unLHV :: Reader Lambda a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Lambda)

instance TrialModel LHV where
  localA A0 = asks a0
  localA A1 = asks a1
  localB B0 = asks b0
  localB B1 = asks b1

-- Some instances of hidden variables

lambdaAllPlus :: Lambda
lambdaAllPlus = Lambda Plus Plus Plus Plus
-- Always equal => correlation +1 for every (a,b) => CHSH = +2

lambdaAntiAll :: Lambda
lambdaAntiAll = Lambda Plus Plus Minus Minus
-- Always opposite => correlation -1 for every (a,b) => CHSH = -2

allLambdas :: [Lambda]
allLambdas =
  [ Lambda x0 x1 y0 y1
  | x0 <- allOutcomes
  , x1 <- allOutcomes
  , y0 <- allOutcomes
  , y1 <- allOutcomes
  ]
  where allOutcomes = [Minus, Plus]

------------------------------------------------------------
-- Sampler menu
--
-- A sampler is an instruction of the experiment layer, like a
-- schedule: it may read the step, and it returns the strategy λ of
-- that step. (Before, type Sampler e = Int -> e Lambda: the same
-- thing, with the step as an explicit argument. ReaderT turns one
-- into the other, and every membrane applied its sampler at the
-- step it was running. Each sampler below is the old one, wrapped
-- in ReaderT.)

type Sampler e = Exp e Lambda

plus2, uniformAll :: Applicative e => Sampler e
plus2      = pure lambdaAllPlus
uniformAll = ReaderT (uniformCycle allLambdas)

-- the other extremal strategy, and the two in turn
minus2, altExtrema :: Applicative e => Sampler e
minus2     = pure lambdaAntiAll
altExtrema =
  ReaderT (\i -> if even i then pure lambdaAllPlus else pure lambdaAntiAll)

-- biasPlus2Loophole correlates with fixedSchedule because both use i.
-- With fixedSchedule using i `mod` 4, choosing λ based on i `mod` m
-- can leak information about (a,b). For m=8, the rare case i=7 is
-- always (A1,B1), which is precisely the minus term in CHSH, so this
-- bias can artificially inflate CHSH
--
-- biasPlus2Blocks chooses λ per block of 4 trials, so it is independent
-- of the within-block setting chosen by fixedSchedule (i `mod` 4).
biasPlus2Loophole, biasPlus2Blocks :: Applicative e =>
  Int -> Int -> Sampler e
biasPlus2Loophole = \k m -> ReaderT $
  \i -> if (i `mod` m) < k then pure lambdaAllPlus else pure lambdaAntiAll
biasPlus2Blocks = \k m -> ReaderT $
  \i -> let blk = i `div` 4
        in pure $ if (blk `mod` m) < k then lambdaAllPlus else lambdaAntiAll

-- uniformBlocks cycles through all 16 strategies, as uniformAll does,
-- but one per block of 4 trials, as biasPlus2Blocks does. Under
-- fixedSchedule every context then meets every strategy equally
-- often; with uniformAll it meets only the 4 that share its i `mod` 4.
uniformBlocks :: Applicative e => Sampler e
uniformBlocks = ReaderT (\i -> uniformCycle allLambdas (i `div` 4))

-- IO sampler: a genuine biased coin between the two extremals
-- Expected CHSH: 4p - 2 (no schedule correlation assumed)
biasedPlus2IO :: Double -> Sampler IO
biasedPlus2IO p = do
  u <- liftIO (randomRIO (0.0, 1.0))
  pure $ if u < p then lambdaAllPlus else lambdaAntiAll

-- A sampler that cannot read the step: λ comes from the lab's
-- effect alone. Every sampler above may read the step, and so may
-- the schedule; this one cannot follow a schedule through it.
fresh :: e Lambda -> Sampler e
fresh draw = ReaderT (const draw)

-- One of the 16 strategies, uniformly at random: uniformAll drawn
-- from coins instead of cycled through with the step.
uniformIO :: IO Lambda
uniformIO = (allLambdas !!) <$> randomRIO (0, length allLambdas - 1)

------------------------------------------------------------
-- Membrane: embed an LHV trial into the experiment monad Exp e,
-- by sampling a λ for each trial using a provided sampler.
-- (Before: ReaderT $ \i -> do lam <- sampleLam i; pure (runReader r lam).
-- With the sampler in Exp e, it reads the step itself.)

runTrialLHV :: Functor e => Sampler e -> RunTrial e LHV
runTrialLHV samp (LHV r) = runReader r <$> samp

------------------------------------------------------------
-- CHSH tests

lhvCHSHId :: Int -> Sampler Identity -> Scheduler Identity -> Double
lhvCHSHId n samp sched =
  runIdentity (chsh n sched (runTrialLHV samp))

lhvCHSHIO :: Int -> Sampler IO -> Scheduler IO -> IO Double
lhvCHSHIO n samp sched =
  chsh n sched (runTrialLHV samp)

testLHV_uniform_fixed :: Double
testLHV_uniform_fixed = lhvCHSHId 20000 uniformAll fixedSchedule
-- 0.0 exactly

testLHV_uniform_random :: IO Double
testLHV_uniform_random = lhvCHSHIO 200000 uniformAll randomScheduleIO
-- ~0.0

testLHV_plus2 :: Double
testLHV_plus2 = lhvCHSHId 20000 plus2 fixedSchedule
-- 2.0 exactly

testLHV_minus2 :: Double
testLHV_minus2 = lhvCHSHId 20000 minus2 fixedSchedule
-- -2.0 exactly


testLHV_bias_loophole_7of8 :: Double
testLHV_bias_loophole_7of8 =
  lhvCHSHId 20000 (biasPlus2Loophole 7 8) fixedSchedule
-- 3.0 under fixedSchedule (this is the point!)

testLHV_bias_blocks_7of8 :: Double
testLHV_bias_blocks_7of8 =
  lhvCHSHId 20000 (biasPlus2Blocks 7 8) fixedSchedule
-- 4*(7/8) - 2 = 1.5

testLHV_biasIO :: Double -> IO Double
testLHV_biasIO p = lhvCHSHIO 200000 (biasedPlus2IO p) fixedSchedule

------------------------------------------------------------
-- Reports: the CHSH value and the no-signaling verdict of the
-- same run. (Before, noSignalingLHV_uniform checked no-signaling
-- in separate experiments at fixed settings, and passed.)
-- Print one with printReport.

reportLHV_uniform_fixed :: Report
reportLHV_uniform_fixed =
  runIdentity (chshReport 80000 fixedSchedule (runTrialLHV uniformAll))
-- CHSH 0.0, yet Bob's marginal at B0 follows Alice's setting:
-- uniformAll cycles with the step index, and so does the fixed
-- schedule, so i `mod` 4 fixes Bob's entries of λ and the context
-- at once. The table is not the uniform model of Figure 4.

reportLHV_uniform_random :: IO Report
reportLHV_uniform_random =
  seeded 1 (chshReport 80000 randomScheduleIO (runTrialLHV uniformAll))
-- CHSH ~0.0, no-signaling: the uniform model of Figure 4

reportLHV_bias_loophole_7of8 :: Report
reportLHV_bias_loophole_7of8 =
  runIdentity (chshReport 80000 fixedSchedule (runTrialLHV (biasPlus2Loophole 7 8)))
-- CHSH 3.0, and Bob's marginal at B1 depends on Alice's setting

reportLHV_bias_blocks_7of8 :: Report
reportLHV_bias_blocks_7of8 =
  runIdentity (chshReport 80000 fixedSchedule (runTrialLHV (biasPlus2Blocks 7 8)))
-- CHSH 1.5, no-signaling
