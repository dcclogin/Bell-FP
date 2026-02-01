module CHSH.Experiment where

import CHSH.Util

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.State.Strict
import System.Random (randomRIO)

------------------------------------------------------------
-- Trial semantics: to be able to express the constraints
-- on the internals of the black boxes, we will use a monad
-- interface. Different choices of monads will express
-- different constraints on the black boxes.

class Monad t => TrialModel t where
  -- Alice’s response to her local setting
  localA :: ASetting -> t Outcome
  -- Bob’s response to his local setting
  localB :: BSetting -> t Outcome
  -- Compute joint response: default is the applicative pairing
  jointAB :: ASetting -> BSetting -> t (Outcome, Outcome)
  jointAB a b = liftA2 (,) (localA a) (localB b)

------------------------------------------------------------
-- Experiment semantics: to be able to express the constraints
-- across different trials, we will another monad.
-- To run each trial, we take the step index 'i', a
-- computation for the trial, and return a computation
-- in the experiment monad

type Exp e = ReaderT Int e
type RunTrial e t = forall a. t a -> Exp e a
type Schedule e = Exp e (ASetting, BSetting)

------------------------------------------------------------
-- The experiment chooses a schedule, i.e. for each
-- step 'i', it chooses a setting (possibly using 
-- memory, random number generation, etc)

fixedSchedule :: Monad e => Schedule e
fixedSchedule = do
  i <- ask
  pure $ case i `mod` 4 of
    0 -> (A0,B0)
    1 -> (A0,B1)
    2 -> (A1,B0)
    _ -> (A1,B1)

randomScheduleIO :: Schedule IO
randomScheduleIO = do
  ra <- liftIO $ randomRIO (0 :: Int, 1)
  rb <- liftIO $ randomRIO (0 :: Int, 1)
  pure (if ra == 0 then A0 else A1, if rb == 0 then B0 else B1)

-- CHSH correlation

chsh :: (Monad e, TrialModel t)
     => Int -> Schedule e -> RunTrial e t -> e Double
chsh n sched runTrial =
  (*4) <$> mcAverageIx n trial
  where
    trial i = do
      (a,b) <- runReaderT sched i
      xy    <- runReaderT (runTrial (jointAB a b)) i
      let v = trialValue xy
      pure $ case (a,b) of
        (A1,B1) -> -v
        _       ->  v

runTrialIdentity :: Monad e => RunTrial e Identity
runTrialIdentity ta = pure (runIdentity ta)

{--

instance TrialModel Identity where
  localA _ = pure Plus
  localB _ = pure Plus

chshId :: Int
chshId = runIdentity $ do
  v00 <- val A0 B0
  v01 <- val A0 B1
  v10 <- val A1 B0
  v11 <- val A1 B1
  pure $ v00 + v01 + v10 - v11
    where val a b = do
            xy <- jointAB a b
            pure (trialValue xy)

--

data Shared = Shared { lastASetting :: Maybe ASetting }

instance TrialModel (State Shared) where
  localA a = do
    modify (\s -> s { lastASetting = Just a })
    pure Minus
  localB b = do
    ma <- gets lastASetting
    pure $ case (ma, b) of
      (Just A1, B1) -> Plus
      _             -> Minus

chshST :: Int
chshST = evalState
  (do v00 <- val A0 B0
      v01 <- val A0 B1
      v10 <- val A1 B0
      v11 <- val A1 B1
      pure $ v00 + v01 + v10 - v11)
  (Shared Nothing)
    where val a b = do
            xy <- jointAB a b
            pure (trialValue xy)

--}
------------------------------------------------------------
