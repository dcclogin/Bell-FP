module CHSH.PR where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import System.Random (randomRIO)

------------------------------------------------------------
-- PR box: a hypothetical no-signaling resource that achieves 
-- the algebraic maximum CHSH value of 4. It is defined 
-- by the constraint:
--   x ⊕ y = a ∧ b
-- where x,y are the outputs for Alice and Bob, and a,b are 
-- their respective inputs (settings). The PR box is not 
-- physically realizable, but serves as a useful theoretical 
-- tool for understanding the limits of nonlocal correlations.

newtype PR a = PR { unPR :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

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
-- Membrane: embed PR trials into Exp IO; ignore the step index.

runTrialPR :: RunTrial IO PR
runTrialPR (PR ioa) = ReaderT (\_ -> ioa)

------------------------------------------------------------
-- Tests

-- PR should hit ~4 under any schedule (fixed or random).
testPR_fixed :: IO Double
testPR_fixed = chsh 20000 fixedSchedule runTrialPR

testPR_random :: IO Double
testPR_random = chsh 20000 randomScheduleIO runTrialPR

------------------------------------------------------------
-- No-signaling checks

noSignalingPR :: IO (Bool, String)
noSignalingPR = noSignalingReport 20000 0.02 runTrialPR

noSignalingPR_print :: IO ()
noSignalingPR_print = prettyReport =<< noSignalingPR