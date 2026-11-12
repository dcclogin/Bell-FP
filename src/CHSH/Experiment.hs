module CHSH.Experiment where

import CHSH.Util

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import System.Random (mkStdGen, randomRIO, setStdGen)

------------------------------------------------------------
-- Trial semantics: to be able to express the constraints
-- on the internals of the black boxes, we will use a monad
-- interface. Different choices of monads will express
-- different constraints on the black boxes.

class Monad t => TrialModel t where
  -- Alice's response to her local setting
  localA :: ASetting -> t Outcome
  -- Bob's response to his local setting
  localB :: BSetting -> t Outcome
  -- Compute joint response: default is the applicative pairing
  jointAB :: ASetting -> BSetting -> t (Outcome, Outcome)
  jointAB a b = liftA2 (,) (localA a) (localB b)

------------------------------------------------------------
-- Experiment semantics: to be able to express the constraints
-- across different trials, we use another monad.
-- To run each trial, we take the step index 'i', a
-- computation for the trial, and return a computation
-- in the experiment monad

type Exp e = ReaderT Int e
type RunTrial e t = forall a. t a -> Exp e a

-- The settings of one step, and the scheduler that chooses them.
type Context = (ASetting, BSetting)
type Scheduler e = Exp e Context

------------------------------------------------------------
-- The experiment's scheduler chooses, for each step
-- 'i', the settings (possibly using memory, random
-- number generation, etc): its schedule

fixedSchedule :: Monad e => Scheduler e
fixedSchedule = do
  i <- ask
  pure $ case i `mod` 4 of
    0 -> (A0,B0)
    1 -> (A0,B1)
    2 -> (A1,B0)
    _ -> (A1,B1)

randomScheduleIO :: Scheduler IO
randomScheduleIO = do
  ra <- liftIO $ randomRIO (0 :: Int, 1)
  rb <- liftIO $ randomRIO (0 :: Int, 1)
  pure (if ra == 0 then A0 else A1, if rb == 0 then B0 else B1)

-- Experiments in IO draw from the global generator; fixing it
-- first makes a run reproducible.
seeded :: Int -> IO a -> IO a
seeded s act = setStdGen (mkStdGen s) >> act

------------------------------------------------------------
-- A trial, as the experimenter records it: the settings the
-- scheduler chose and the outcomes the boxes produced. The
-- scheduler and the membrane both read the same step index, so a
-- trial runs one and then the other, and keeps both results.
-- (In the submission this was the body of chsh, which kept only
-- the score.)
--
-- The scheduler chooses the settings once, and the trial hands them
-- to the membrane. A membrane may depend on them: that is
-- superdeterminism, whose sampler is told the settings (see
-- CHSH.Superdet). The membrane of every other model is blind to
-- them. (In revision 3 the trial took a RunTrial, and
-- superdeterminism had a trial of its own, trialSuperdet, which
-- ran the boxes without a membrane.)

data Record = Record ASetting BSetting Outcome Outcome
  deriving Eq

trial :: (Monad e, TrialModel t)
      => Scheduler e -> (Context -> RunTrial e t) -> Exp e Record
trial sched membrane = do
  s@(a, b) <- sched
  (x, y)   <- membrane s (jointAB a b)
  pure (Record a b x y)

-- A membrane blind to the settings: the same at every one.
blind :: RunTrial e t -> Context -> RunTrial e t
blind runTrial _ = runTrial

-- An experiment runs a trial at the steps 0 .. n-1 and returns
-- the record of the run.
experiment :: Monad e => Int -> Exp e Record -> e [Record]
experiment n oneTrial = mapM (runReaderT oneTrial) [0 .. n - 1]

------------------------------------------------------------
-- The empirical model of a run: a table with a row for each
-- context and a column for each pair of outcomes, whose cells
-- count the trials. A cell is a page that a trial can write.
-- Divided by the number of trials in the context, the counts are
-- the frequencies P(x, y | a, b). Everything the experiment
-- reports is read off this one table.

-- every value of a type, in the order of its declaration
every :: (Bounded a, Enum a) => [a]
every = [minBound .. maxBound]

-- the rows, the columns and the cells of a table, in the order of
-- the paper's tables: the list applicative takes every combination
contexts :: [Context]
contexts = liftA2 (,) every every

outcomePairs :: [(Outcome, Outcome)]
outcomePairs = liftA2 (,) every every

pages :: [Record]
pages = Record <$> every <*> every <*> every <*> every

-- a table pairs each page with its count: the number of pages of
-- the record equal to it
newtype Empirical = Empirical [(Record, Int)]
  deriving Eq

empirical :: [Record] -> Empirical
empirical rs = Empirical [ (p, length (filter (== p) rs)) | p <- pages ]

type Event = (Outcome, Outcome) -> Bool

-- the number of trials in context (a, b) whose outcomes satisfy p
numTrials :: Empirical -> ASetting -> BSetting -> Event -> Int
numTrials (Empirical m) a b p =
  sum [ k | (Record a' b' x y, k) <- m, (a', b') == (a, b), p (x, y) ]

-- the frequency, among the trials of context (a, b), of the
-- outcomes that satisfy p
freq :: Empirical -> ASetting -> BSetting -> Event -> Double
freq m a b p =
  fromIntegral (numTrials m a b p) / fromIntegral (numTrials m a b (const True))

------------------------------------------------------------
-- The CHSH value, read off the table: chshOf of the correlators. A
-- correlator is the score of a trial in expectation, over the trials
-- of its own context. (Before, chsh averaged the signed scores of all
-- trials; under the fixed schedule with n a multiple of 4 the two
-- agree. Under the random schedule the contexts occur unequally
-- often, and the per-context value is the one an experimenter would
-- compute from the record.)
correlator :: Empirical -> ASetting -> BSetting -> Double
correlator m a b =
  sum [ fromIntegral (trialValue xy) * freq m a b (== xy) | xy <- outcomePairs ]

-- The CHSH value of a run: same arguments as before.
chsh :: (Monad e, TrialModel t)
     => Int -> Scheduler e -> RunTrial e t -> e Double
chsh n sched runTrial =
  chshOf . correlator . empirical <$> experiment n (trial sched (blind runTrial))
