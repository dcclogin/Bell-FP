module CHSH.BellTable where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.LHV (Lambda(..), Sampler, uniformAll, uniformBlocks)

import Control.Monad.State
import Data.Functor.Identity (runIdentity)

------------------------------------------------------------
-- The Bell table (Abramsky, "Contextuality: At the Borders of
-- Paradox", Figure 3) from a nonlocal hidden variable. Its outcomes
-- always agree in context (A0,B0), agree three times in four in
-- (A0,B1) and (A1,B0), and once in four in (A1,B1): CHSH 2.5, above
-- the local bound 2 and below Tsirelson's 2 sqrt 2. Quantum
-- mechanics realizes it: in the state |00> + |11> of CHSH.Quantum,
-- with Alice measuring Z or (Z + sqrt 3 X)/2 and Bob Z or
-- (Z - sqrt 3 X)/2, the outcomes agree with these probabilities.
--
-- As in CHSH.PRState, each box reads its entry of λ and then rewrites
-- the other party's entries, before the other box reads them. Here it
-- copies its answer into them, or flips it, by coins it finds in λ
-- itself. (Before: only Alice's box rewrote, and Bob's only read. With
-- Bob's box first, the two answers were then independent: CHSH 0.)

newtype BellHiddenState a = BellHiddenState { unBellHiddenState :: State Lambda a }
  deriving newtype (Functor, Applicative, Monad, MonadState Lambda)

-- Each box reads its own entry of λ, as in CHSH.LHV, and then
-- rewrites the other party's entries from its outcome. The coins are
-- entries of λ that it has not read: when λ is uniform they are fair,
-- and independent of its outcome. jointAB keeps its default,
-- liftA2 (,) (localA a) (localB b): Alice's box and then Bob's, whose
-- outcome then agrees with hers always in (A0,B0), unless the coin
-- comes up in (A0,B1) and (A1,B0), and only if it comes up in (A1,B1).
-- From a uniform λ either box could go first, with the same table.
-- (Before: the local methods only read, and jointAB paired two
-- computations, nonlocalA and nonlocalB, that read and wrote.)
instance TrialModel BellHiddenState where
  localA A0 = do
    x <- gets a0
    modify (\lam -> lam { b0 = x, b1 = flipWhen (rare (a1 lam) (b1 lam)) x })
    pure x
  localA A1 = do
    x <- gets a1
    modify (\lam -> lam { b0 = flipWhen (rare (a0 lam) (b0 lam)) x
                        , b1 = flipWhen (not (rare (a0 lam) (b1 lam))) x })
    pure x
  localB B0 = do
    y <- gets b0
    modify (\lam -> lam { a0 = y, a1 = flipWhen (rare (b1 lam) (a1 lam)) y })
    pure y
  localB B1 = do
    y <- gets b1
    modify (\lam -> lam { a0 = flipWhen (rare (b0 lam) (a0 lam)) y
                        , a1 = flipWhen (not (rare (b0 lam) (a1 lam))) y })
    pure y

-- Two fair coins both come up Minus a quarter of the time.
rare :: Outcome -> Outcome -> Bool
rare u v = u == Minus && v == Minus

flipWhen :: Bool -> Outcome -> Outcome
flipWhen c x = if c then flipOutcome x else x

------------------------------------------------------------
-- Membrane: as for the PR box, the sampler draws the λ that the
-- trial starts from, and evalState drops the λ it ends with.

runTrialBellHiddenState :: Functor e => Sampler e -> RunTrial e BellHiddenState
runTrialBellHiddenState samp (BellHiddenState s) = evalState s <$> samp

------------------------------------------------------------
-- Reports: the CHSH value and the no-signaling verdict of one run.

reportBellHiddenState_blocks_fixed :: Report
reportBellHiddenState_blocks_fixed =
  runIdentity (chshReport 80000 fixedSchedule (runTrialBellHiddenState uniformBlocks))
-- CHSH 2.5, no-signaling: the Bell table, count for count. Each
-- context meets all 16 strategies, 1250 times each.

reportBellHiddenState_uniform_random :: IO Report
reportBellHiddenState_uniform_random =
  seeded 15 (chshReport 80000 randomScheduleIO (runTrialBellHiddenState uniformAll))
-- CHSH ~2.5, no-signaling

reportBellHiddenState_uniform_fixed :: Report
reportBellHiddenState_uniform_fixed =
  runIdentity (chshReport 80000 fixedSchedule (runTrialBellHiddenState uniformAll))
-- CHSH 4.0, no-signaling: the PR box. Under the fixed schedule the
-- cycling sampler fixes Bob's entries of λ with the context (b1 is
-- Plus in (A0,B1) and (A1,B1), b0 in (A1,B0)), so no coin ever comes
-- up: Bob's answer copies Alice's in (A0,B1) and (A1,B0), and flips it
-- in (A1,B1).
