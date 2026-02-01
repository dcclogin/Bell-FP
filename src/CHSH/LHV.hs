module CHSH.LHV where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Monad.Reader
import Control.Monad.Identity
import System.Random (randomRIO)

------------------------------------------------------------
-- Local Hidden Variables 

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

type Sampler e = Int -> e Lambda

plus2, minus2, uniformAll, altExtrema :: Applicative e => Sampler e
plus2      = constLam lambdaAllPlus
minus2     = constLam lambdaAntiAll
uniformAll = uniformCycle allLambdas
altExtrema =
  \i -> if even i then pure lambdaAllPlus else pure lambdaAntiAll

constLam :: Applicative e => Lambda -> Sampler e
constLam lam _ = pure lam

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
biasPlus2Loophole = \k m ->
  \i -> if (i `mod` m) < k then pure lambdaAllPlus else pure lambdaAntiAll
biasPlus2Blocks = \k m -> 
  \i -> let blk = i `div` 4
        in pure $ if (blk `mod` m) < k then lambdaAllPlus else lambdaAntiAll

-- IO sampler: a genuine biased coin between the two extremals
-- Expected CHSH: 4p - 2 (no schedule correlation assumed)
biasedPlus2IO :: Double -> Sampler IO
biasedPlus2IO p _ = do
  u <- randomRIO (0.0, 1.0)
  pure $ if u < p then lambdaAllPlus else lambdaAntiAll

------------------------------------------------------------
-- Example tests

runTrialLHV :: Monad e => Sampler e -> RunTrial e LHV
runTrialLHV sampleLam (LHV r) =
  ReaderT $ \i -> do
    lam <- sampleLam i
    pure (runReader r lam)

lhvCHSHId :: Int -> Sampler Identity -> Double
lhvCHSHId n samp =
  runIdentity (chsh n fixedSchedule (runTrialLHV samp))

lhvCHSHIO :: Int -> Sampler IO -> IO Double
lhvCHSHIO n samp =
  chsh n fixedSchedule (runTrialLHV samp)

testLHV_uniform0 :: Double
testLHV_uniform0 = lhvCHSHId 20000 uniformAll
-- ~0.0

testLHV_plus2 :: Double
testLHV_plus2 = lhvCHSHId 20000 plus2
-- 2.0 exactly

testLHV_minus2 :: Double
testLHV_minus2 = lhvCHSHId 20000 minus2
-- -2.0 exactly

testLHV_bias_loophole_7of8 :: Double
testLHV_bias_loophole_7of8 =
  lhvCHSHId 20000 (biasPlus2Loophole 7 8)
-- typically ~3.0 under fixedSchedule (this is the point!)

testLHV_bias_blocks_7of8 :: Double
testLHV_bias_blocks_7of8 =
  lhvCHSHId 20000 (biasPlus2Blocks 7 8)
-- should be close to 4*(7/8) - 2 = 1.5

testLHV_biasIO :: Double -> IO Double
testLHV_biasIO p = lhvCHSHIO 200000 (biasedPlus2IO p)

noSignalingLHV_uniform :: IO () 
noSignalingLHV_uniform = prettyReport $ runIdentity $ 
   noSignalingReport 20000 0.00 (runTrialLHV uniformAll)