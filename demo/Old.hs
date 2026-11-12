module Old where

-- The sampler code of revision 3, which is that of the submission,
-- copied verbatim but for the prefix "old" on each name, so that it
-- can run next to this package's. Sources:
--
--   type Sampler, the samplers, runTrialLHV   revised3/src/CHSH/LHV.hs
--   runTrialPRHiddenState                     revised3/src/CHSH/PRState.hs
--   type BadSampler, cheatLam, legalAllPlus,
--     cheatNS, trialSuperdet                  revised3/src/CHSH/Superdet.hs
--   trial                                     revised3/src/CHSH/Experiment.hs
--   runTrialSuperdet                          src/CHSH/Superdet.hs of the
--                                             submission (commit 0bba4ae),
--                                             lines 62-67
--
-- Everything else (Lambda, LHV, the schedules, Record, report) is
-- this package's, unchanged from revision 3.

import CHSH.Util
import CHSH.Experiment
import CHSH.LHV (Lambda(..), LHV(..), allLambdas, lambdaAllPlus, lambdaAntiAll)
import CHSH.PRState (PRHiddenState(..))

import Control.Monad.Reader
import Control.Monad.State
import System.Random (randomRIO)

------------------------------------------------------------
-- revision 3: the trial took a RunTrial

oldTrial :: (Monad e, TrialModel t) => Scheduler e -> RunTrial e t -> Exp e Record
oldTrial sched runTrial = do
  (a, b) <- sched
  (x, y) <- runTrial (jointAB a b)
  pure (Record a b x y)

------------------------------------------------------------
-- revision 3 and the submission: the sampler took the step

type OldSampler e = Int -> e Lambda

oldPlus2, oldMinus2, oldUniformAll, oldAltExtrema :: Applicative e => OldSampler e
oldPlus2      = oldConstLam lambdaAllPlus
oldMinus2     = oldConstLam lambdaAntiAll
oldUniformAll = uniformCycle allLambdas
oldAltExtrema =
  \i -> if even i then pure lambdaAllPlus else pure lambdaAntiAll

oldConstLam :: Applicative e => Lambda -> OldSampler e
oldConstLam lam _ = pure lam

oldBiasPlus2Loophole, oldBiasPlus2Blocks :: Applicative e =>
  Int -> Int -> OldSampler e
oldBiasPlus2Loophole = \k m ->
  \i -> if (i `mod` m) < k then pure lambdaAllPlus else pure lambdaAntiAll
oldBiasPlus2Blocks = \k m ->
  \i -> let blk = i `div` 4
        in pure $ if (blk `mod` m) < k then lambdaAllPlus else lambdaAntiAll

oldBiasedPlus2IO :: Double -> OldSampler IO
oldBiasedPlus2IO p _ = do
  u <- randomRIO (0.0, 1.0)
  pure $ if u < p then lambdaAllPlus else lambdaAntiAll

oldRunTrialLHV :: Monad e => OldSampler e -> RunTrial e LHV
oldRunTrialLHV sampleLam (LHV r) =
  ReaderT $ \i -> do
    lam <- sampleLam i
    pure (runReader r lam)

oldRunTrialPRHiddenState :: Monad e => OldSampler e -> RunTrial e PRHiddenState
oldRunTrialPRHiddenState sampleLam (PRHiddenState s) =
  ReaderT $ \i -> do
    lam <- sampleLam i
    pure $ evalState s lam

------------------------------------------------------------
-- revision 3 and the submission: the bad sampler took the step and
-- the settings

type OldBadSampler e = Int -> (ASetting, BSetting) -> e Lambda

oldCheatLam :: Applicative e => OldBadSampler e
oldCheatLam _ (a,b) =
  pure $ case (a,b) of
    (A1,B1) -> Lambda Plus Plus Plus Minus
    _       -> Lambda Plus Plus Plus Plus

oldLegalAllPlus :: Applicative e => OldBadSampler e
oldLegalAllPlus _ _ = pure (Lambda Plus Plus Plus Plus)

oldCheatNS :: OldBadSampler IO
oldCheatNS _ (a, b) = do
  r <- randomOutcome
  let y = if a == A1 && b == B1 then flipOutcome r else r
  pure (Lambda r r y y)

-- revision 3: superdeterminism had a trial of its own
oldTrialSuperdet :: Monad e => Scheduler e -> OldBadSampler e -> Exp e Record
oldTrialSuperdet sched badLam = do
  (a, b) <- sched
  lam    <- ReaderT (\i -> badLam i (a, b))
  let (x, y) = runReader (unLHV (jointAB a b)) lam
  pure (Record a b x y)

-- the submission: a membrane that re-ran the schedule
oldRunTrialSuperdet :: Monad e => Scheduler e -> OldBadSampler e -> RunTrial e LHV
oldRunTrialSuperdet sched badLam (LHV r) =
  ReaderT $ \i -> do
    (a,b) <- runReaderT sched i
    lam   <- badLam i (a,b)
    pure (runReader r lam)
