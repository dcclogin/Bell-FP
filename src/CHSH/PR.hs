module CHSH.PR where

-- PR box: jointAB may depend on both settings.
-- This preserves parameter independence at the level of marginals
-- (no-signaling) but violates outcome independence / factorization.

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import System.Random (randomRIO)

------------------------------------------------------------
-- PR trial model

newtype PR a = PR { unPR :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- Membrane: embed PR trials into Exp IO; ignore the step index.
runTrialPR :: RunTrial IO PR
runTrialPR (PR ioa) = ReaderT (\_ -> ioa)

instance TrialModel PR where
  -- Local marginals are uniform; defined for completeness.
  -- (They are not used once jointAB is overridden.)
  localA _ = PR randomOutcome
  localB _ = PR randomOutcome

  -- PR constraint (in bit form):
  --   x ⊕ y = a ∧ b
  -- with x uniform, hence uniform marginals for both parties.
  jointAB a b = PR $ do
    x <- randomRIO (0 :: Int, 1)          -- choose x uniformly
    let c = aBit a * bBit b               -- a ∧ b since bits are 0/1
        y = xor x c
    pure (bitOut x, bitOut y)

------------------------------------------------------------
-- Tests

-- PR should hit ~4 under any schedule (fixed or random).
testPR_fixed :: IO Double
testPR_fixed = chsh 20000 fixedSchedule runTrialPR

testPR_random :: IO Double
testPR_random = chsh 20000 randomScheduleIO runTrialPR

-- Reuse the generic no-signaling checker.
noSignalingPR :: IO (Bool, String)
noSignalingPR = noSignalingReport 20000 0.02 runTrialPR

testNoSignalPR :: IO ()
testNoSignalPR = prettyReport =<< noSignalingPR