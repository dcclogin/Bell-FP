module CHSH.PR where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))

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
  -- Either box on its own is a fair coin, at either setting: a box
  -- that reaches CHSH 4 without signaling can be nothing else.
  localA _ = PR randomOutcome
  localB _ = PR randomOutcome

  -- The two boxes in turn, as in the default: neither sees the other's
  -- setting or answer. All the nonlocality is in the constraint on the
  -- pair, which only the joint response can state.
  -- (Before: Alice's box answered, and the constraint computed Bob's
  -- answer from hers.)
  jointAB a b = liftA2 (,) (localA a) (localB b) `suchThat` prConstraint a b

-- The PR constraint, x xor y = a and b, in outcomes: the answers differ
-- exactly in context (A1,B1). In each context it keeps two pairs, and in
-- those two each party answers Plus once; so on fair coins, each answer
-- is still a fair coin.
prConstraint :: ASetting -> BSetting -> (Outcome, Outcome) -> Bool
prConstraint a b (x, y) = (x /= y) == (a == A1 && b == B1)

-- A computation, run again until its result satisfies p.
suchThat :: Monad m => m r -> (r -> Bool) -> m r
suchThat m p = do
  r <- m
  if p r then pure r else m `suchThat` p


------------------------------------------------------------
-- Membrane: embed PR trials into Exp IO; ignore the step index.

runTrialPR :: RunTrial IO PR
runTrialPR (PR ioa) = ReaderT (\_ -> ioa)

------------------------------------------------------------
-- Tests

-- PR should hit 4 under any schedule (fixed or random).
testPR_fixed :: IO Double
testPR_fixed = chsh 20000 fixedSchedule runTrialPR

testPR_random :: IO Double
testPR_random = chsh 20000 randomScheduleIO runTrialPR

------------------------------------------------------------
-- Reports: the CHSH value and the no-signaling verdict of the same
-- run. The table is the PR-box model of Figure 6, with uniform
-- marginals. (Before, noSignalingPR ran separate experiments; the
-- membrane ignores the step index, so the verdict is unchanged.)

reportPR_fixed :: IO Report
reportPR_fixed = seeded 7 (chshReport 80000 fixedSchedule runTrialPR)
-- CHSH 4.0, no-signaling

reportPR_random :: IO Report
reportPR_random = seeded 8 (chshReport 80000 randomScheduleIO runTrialPR)
-- CHSH 4.0, no-signaling
