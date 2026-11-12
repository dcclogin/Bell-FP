module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (unless)
import Data.Functor.Identity (Identity, runIdentity)
import System.Exit (exitFailure)
import Test.HUnit

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.Clocked (runTrialClockLoose, runTrialClockTight)
import CHSH.Identity (liftIdentity)
import CHSH.BellTable (runTrialBellHiddenState)
import CHSH.LHV (Sampler, altExtrema, biasPlus2Blocks, biasPlus2Loophole, biasedPlus2IO,
                 fresh, minus2, plus2, runTrialLHV, uniformAll, uniformBlocks, uniformIO)
import CHSH.PR (PR(..), prConstraint, runTrialPR, suchThat)
import CHSH.PRState (runTrialPRHiddenState)
import CHSH.Quantum ((><), add, coin, collapse, correlation, entangled, maximallyEntangled, measA, measB,
                     nonlocalA, nonlocalB, normalize, pauliX, pauliZ, probability, projector,
                     runTrialQuantum, runTrialSource, scale, observableA, observableB)
import CHSH.Shared (runTrialState)
import CHSH.Superdet (cheatLam, cheatNS, legalAllPlus, peek, reportSuperdet)
import Old

------------------------------------------------------------
-- One run, one table: every CHSH value and every no-signaling
-- verdict below is read off the empirical model of a single run.

n :: Int
n = 20000

-- The original's estimator: the signed scores of all trials,
-- averaged, times 4.
pooled :: [Record] -> Double
pooled rs = 4 * fromIntegral (sum (map score rs)) / fromIntegral (length rs)
  where
    score (Record a b x y) = case (a, b) of
      (A1, B1) -> negate (trialValue (x, y))
      _        -> trialValue (x, y)

near :: String -> Double -> Double -> Double -> Assertion
near what tol expected actual =
  assertBool (what ++ ": " ++ show actual ++ " is not within " ++ show tol
              ++ " of " ++ show expected)
             (abs (actual - expected) <= tol)

-- a run that signals: its CHSH value, and exactly these conditions
signals :: String -> Double -> [String] -> Report -> Assertion
signals what v conds r = do
  assertEqual (what ++ ": CHSH") v (chshValue r)
  assertEqual (what ++ ": violated conditions") conds (signaling r)

-- a run that does not: its CHSH value, within tol, and no condition
noSignal :: String -> Double -> Double -> Report -> Assertion
noSignal what tol v r = do
  near (what ++ ": CHSH") tol v (chshValue r)
  assertEqual (what ++ ": violated conditions") [] (signaling r)

fixedRun :: TrialModel t => RunTrial Identity t -> Report
fixedRun runTrial = runIdentity (chshReport n fixedSchedule runTrial)

plusY :: Empirical -> ASetting -> BSetting -> Double
plusY m a b = freq m a b (\(_, y) -> y == Plus)

-- two runs with the same table, hence the same verdicts
sameRun :: String -> Report -> Report -> Assertion
sameRun what r r' = do
  assertBool (what ++ ": the tables differ") (table r == table r')
  assertEqual (what ++ ": CHSH") (chshValue r) (chshValue r')
  assertEqual (what ++ ": violated conditions") (signaling r) (signaling r')

-- the same model, run through revision 3's sampler code and this one's
oldLHV :: Monad e => Scheduler e -> OldSampler e -> e Report
oldLHV sched samp = report n (oldTrial sched (oldRunTrialLHV samp))

newLHV :: Monad e => Scheduler e -> Sampler e -> e Report
newLHV sched samp = chshReport n sched (runTrialLHV samp)

------------------------------------------------------------

tests :: Test
tests = TestList
  [ TestLabel "the fixed schedule gives every context n/4 trials" $ TestCase $ do
      let m = table (fixedRun (runTrialLHV plus2))
      assertEqual "trials per context" [n `div` 4 | _ <- contexts]
        [ numTrials m a b (const True) | (a, b) <- contexts ]

  , TestLabel "under the fixed schedule the table gives the original estimator's value" $
    TestCase $ do
      let check :: TrialModel t => String -> RunTrial Identity t -> Assertion
          check what runTrial = do
            let rs = runIdentity (experiment n (trial fixedSchedule (blind runTrial)))
            near what 1e-9 (pooled rs) (chshOf (correlator (empirical rs)))
      check "pure boxes" liftIdentity
      check "LHV, cycling sampler" (runTrialLHV uniformAll)
      check "LHV, biased sampler" (runTrialLHV (biasPlus2Loophole 7 8))
      check "clocked, clock visible" runTrialClockLoose
      check "shared state" runTrialState
      rs <- seeded 5 (experiment n (trial fixedSchedule (blind runTrialQuantum)))
      near "quantum" 1e-9 (pooled rs) (chshOf (correlator (empirical rs)))

  , TestLabel "no-signaling is decided from the counts" $ TestCase $ do
      assertBool "equal frequencies are never flagged"
        (not (violated (Comparison "x" (5000, 10000) (5000, 10000))))
      assertBool "a deterministic difference is flagged in a large run"
        (violated (Comparison "x" (10000, 10000) (0, 10000)))
      assertBool "1 vs 0 is not flagged with 16 trials in all"
        (not (violated (Comparison "x" (8, 8) (0, 8))))
      assertBool "1 vs 0 is flagged with 17 trials in all"
        (violated (Comparison "x" (9, 9) (0, 8)))
      assertBool "a context that never occurred is flagged"
        (violated (Comparison "x" (3, 10) (0, 0)))
      assertBool "a gap within four standard errors passes"
        (not (violated (Comparison "x" (5000, 10000) (5100, 10000))))
      assertBool "a gap beyond four standard errors is flagged"
        (violated (Comparison "x" (5000, 10000) (5400, 10000)))

  , TestLabel "local models: exact values, no-signaling" $ TestCase $ do
      noSignal "pure boxes" 0 2 (fixedRun liftIdentity)
      noSignal "LHV plus2" 0 2 (fixedRun (runTrialLHV plus2))
      noSignal "LHV minus2" 0 (-2) (fixedRun (runTrialLHV minus2))
      noSignal "LHV blocks 7/8" 1e-9 1.5 (fixedRun (runTrialLHV (biasPlus2Blocks 7 8)))
      noSignal "clocked, clock hidden" 0 2 (fixedRun runTrialClockTight)
      noSignal "superdeterminism, legal sampler" 0 2
        (runIdentity (reportSuperdet n fixedSchedule legalAllPlus))
      u <- seeded 1 (chshReport n randomScheduleIO (runTrialLHV uniformAll))
      noSignal "LHV uniformAll, random schedule" 0.1 0 u
      c <- seeded 2 (chshReport n randomScheduleIO runTrialClockLoose)
      noSignal "clocked, clock visible, random schedule" 0.1 1 c
      d <- seeded 3 (chshReport n randomScheduleIO runTrialClockTight)
      noSignal "clocked, clock hidden, random schedule" 0 2 d

  , TestLabel "the cycling sampler signals under the fixed schedule" $ TestCase $ do
      let r = fixedRun (runTrialLHV uniformAll)
      signals "LHV uniformAll, fixed schedule" 0 ["Bob at B0"] r
      assertEqual "P(y=+ | A0,B0)" 0 (plusY (table r) A0 B0)
      assertEqual "P(y=+ | A1,B0)" 1 (plusY (table r) A1 B0)

  , TestLabel "a biased sampler of the legal type signals too" $ TestCase $ do
      signals "LHV biasPlus2Loophole 7 8" 3 ["Bob at B1"]
        (fixedRun (runTrialLHV (biasPlus2Loophole 7 8)))

  , TestLabel "the scheduler attack signals in its own run" $ TestCase $ do
      let r = fixedRun runTrialClockLoose
      signals "clocked, clock visible, fixed schedule" 4 ["Bob at B1"] r
      assertEqual "P(y=+ | A0,B1)" 1 (plusY (table r) A0 B1)
      assertEqual "P(y=+ | A1,B1)" 0 (plusY (table r) A1 B1)

  , TestLabel "superdeterminism signals in its own run, under both schedules" $
    TestCase $ do
      let r = runIdentity (reportSuperdet n fixedSchedule cheatLam)
      signals "cheatLam, fixed schedule" 4 ["Bob at B1"] r
      assertEqual "P(y=+ | A0,B1)" 1 (plusY (table r) A0 B1)
      assertEqual "P(y=+ | A1,B1)" 0 (plusY (table r) A1 B1)
      s <- seeded 4 (reportSuperdet n randomScheduleIO cheatLam)
      signals "cheatLam, random schedule" 4 ["Bob at B1"] s

  , TestLabel "a sampler that sees the settings can still be no-signaling" $ TestCase $ do
      ns <- seeded 10 (reportSuperdet n randomScheduleIO cheatNS)
      noSignal "cheatNS, random schedule" 0 4 ns

  , TestLabel "the shared state signals" $ TestCase $
      signals "shared state, fixed schedule" 4 ["Bob at B1"] (fixedRun runTrialState)

  , TestLabel "quantum runs are no-signaling, near the Tsirelson value" $ TestCase $ do
      q <- seeded 5 (chshReport n fixedSchedule runTrialQuantum)
      noSignal "quantum, fixed schedule" 0.06 (2 * sqrt 2) q
      q' <- seeded 6 (chshReport n randomScheduleIO runTrialQuantum)
      noSignal "quantum, random schedule" 0.06 (2 * sqrt 2) q'
      assertBool "every marginal is near 1/2"
        (and [ abs (freq (table q) a b p - 0.5) < 0.03
             | (a, b) <- contexts
             , p <- [\(x, _) -> x == Plus, \(_, y) -> y == Plus] ])

  , TestLabel "the quantum boxes' joint answer has their local answers as marginals" $
    TestCase $ do
      let states  = [ maximallyEntangled, entangled (pi / 8), normalize [1, 0, 0, 0]
                    , normalize [0, 1, -1, 0], normalize [3, 1, -2, 5] ]
          close u v = abs (u - v) < 1e-12
          sign x = if x == Plus then 1 else -1
          -- Alice's outcome x, then Bob's outcome y in the state x leaves
          -- (an outcome of probability 0 never happens, and leaves no state)
          joint psi a b x y
            | px == 0   = 0
            | otherwise = px * probability (measB b y) (collapse (measA a x) psi)
            where px = probability (measA a x) psi
          -- the same pair, with Bob's box first
          jointBA psi a b x y
            | qy == 0   = 0
            | otherwise = qy * probability (measA a x) (collapse (measB b y) psi)
            where qy = probability (measB b y) psi
      assertBool "the joint answer has the probabilities of the Born rule on the pair"
        (and [ close (joint psi a b x y)
                     (probability (projector (observableA a) x >< projector (observableB b) y) psi)
             | psi <- states, (a, b) <- contexts, (x, y) <- outcomePairs ])
      assertBool "the joint answer does not depend on which box measures first"
        (and [ close (joint psi a b x y) (jointBA psi a b x y)
             | psi <- states, (a, b) <- contexts, (x, y) <- outcomePairs ])
      assertBool "averaged over Alice's outcome, Bob's answer is as on his own"
        (and [ close (sum [ joint psi a b x Plus | x <- every ])
                     (probability (measB b Plus) psi)
             | psi <- states, (a, b) <- contexts ])
      assertBool "the correlation <psi| A (x) B |psi> is the average of x y"
        (and [ close (sum [ sign x * sign y * joint psi a b x y | (x, y) <- outcomePairs ])
                     (correlation psi a b)
             | psi <- states, (a, b) <- contexts ])
      assertBool "|00> + |11> has the correlation 1/sqrt 2, but -1/sqrt 2 in (A1, B1)"
        (and [ close (correlation maximallyEntangled a b)
                     ((if (a, b) == (A1, B1) then negate else id) (1 / sqrt 2))
             | (a, b) <- contexts ])

  , TestLabel "the boxes are dual: with Bob's first, a run does not signal, and has the same CHSH value" $
    TestCase $ do
      let bobFirst psi = do
            (a, b) <- fixedSchedule
            (x, y) <- runTrialSource psi (do { y <- nonlocalB b; x <- nonlocalA a; pure (x, y) })
            pure (Record a b x y)
      r <- seeded 31 (report n (bobFirst maximallyEntangled))
      noSignal "quantum, Bob's box first" 0.06 (2 * sqrt 2) r
      let psi = entangled (pi / 8)
      r' <- seeded 33 (report n (bobFirst psi))
      noSignal "quantum at pi/8, Bob's box first" 0.06 (1 + sqrt 2) r'
      near "P(x=+ | A0, B0)" 0.02 (probability (measA A0 Plus) psi)
           (freq (table r') A0 B0 (\(x, _) -> x == Plus))
      near "P(y=+ | A1, B1)" 0.02 (probability (measB B1 Plus) psi) (plusY (table r') A1 B1)

  , TestLabel "cos t |00> + sin t |11> has CHSH sqrt 2 (1 + sin 2t), and 2 sqrt 2 at t = pi/4" $
    TestCase $ do
      mapM_ (\k -> let t = fromIntegral k * pi / 16 in
                   near ("CHSH at t = " ++ show k ++ " pi/16") 1e-12
                        (sqrt 2 * (1 + sin (2 * t))) (chshOf (correlation (entangled t))))
            [0 .. 8 :: Int]
      assertBool "entangled (pi/4) is |00> + |11>, normalized"
        (and (zipWith (\u v -> abs (u - v) < 1e-15) (entangled (pi / 4)) maximallyEntangled))

  , TestLabel "with the pair at t = pi/8, each marginal follows its own setting only" $
    TestCase $ do
      let psi = entangled (pi / 8)
          exact = chshOf (correlation psi)
      near "the exact CHSH value at pi/8" 1e-12 (1 + sqrt 2) exact
      q <- seeded 14 (chshReport n fixedSchedule (runTrialSource psi))
      noSignal "quantum, the pair at pi/8" 0.06 exact q
      let plusX a b = freq (table q) a b (\(x, _) -> x == Plus)
      near "P(x=+ | A0, B0)" 0.02 (probability (measA A0 Plus) psi) (plusX A0 B0)
      near "P(x=+ | A1, B1)" 0.02 (probability (measA A1 Plus) psi) (plusX A1 B1)
      near "P(y=+ | A1, B0)" 0.02 (probability (measB B0 Plus) psi) (plusY (table q) A1 B0)
      near "P(y=+ | A0, B1)" 0.02 (probability (measB B1 Plus) psi) (plusY (table q) A0 B1)

  , TestLabel "PR runs are no-signaling at the algebraic maximum" $ TestCase $ do
      p <- seeded 7 (chshReport n fixedSchedule runTrialPR)
      noSignal "PR box, fixed schedule" 0 4 p
      p' <- seeded 8 (chshReport n randomScheduleIO runTrialPR)
      noSignal "PR box, random schedule" 0 4 p'

  , TestLabel "the PR constraint keeps two pairs per context; on biased coins it still reaches 4, and signals" $
    TestCase $ do
      mapM_ (\(a, b) -> do
               let kept = filter (prConstraint a b) outcomePairs
               assertEqual "the pairs that meet it" 2 (length kept)
               assertEqual "Alice answers Plus once" 1 (length (filter ((== Plus) . fst) kept))
               assertEqual "Bob answers Plus once" 1 (length (filter ((== Plus) . snd) kept)))
            contexts
      let biased = do
            (a, b) <- fixedSchedule
            (x, y) <- runTrialPR (PR (liftA2 (,) (coin 0.75) (coin 0.75) `suchThat` prConstraint a b))
            pure (Record a b x y)
      r <- seeded 44 (report n biased)
      near "CHSH" 1e-9 4 (chshValue r)
      assertEqual "violated conditions" ["Alice at A1", "Bob at B1"] (signaling r)

  , TestLabel "every sampler runs as in revision 3, table for table" $ TestCase $ do
      let fixedLHV :: String -> OldSampler Identity -> Sampler Identity -> Assertion
          fixedLHV what o s = sameRun what (runIdentity (oldLHV fixedSchedule o))
                                           (runIdentity (newLHV fixedSchedule s))
      fixedLHV "plus2" oldPlus2 plus2
      fixedLHV "minus2" oldMinus2 minus2
      fixedLHV "uniformAll" oldUniformAll uniformAll
      fixedLHV "altExtrema" oldAltExtrema altExtrema
      fixedLHV "biasPlus2Loophole 7 8" (oldBiasPlus2Loophole 7 8) (biasPlus2Loophole 7 8)
      fixedLHV "biasPlus2Blocks 7 8" (oldBiasPlus2Blocks 7 8) (biasPlus2Blocks 7 8)
      u  <- seeded 1 (oldLHV randomScheduleIO oldUniformAll)
      u' <- seeded 1 (newLHV randomScheduleIO uniformAll)
      sameRun "uniformAll, random schedule" u u'
      b  <- seeded 12 (oldLHV fixedSchedule (oldBiasedPlus2IO 0.75))
      b' <- seeded 12 (newLHV fixedSchedule (biasedPlus2IO 0.75))
      sameRun "biasedPlus2IO 0.75" b b'
      let fixedPR what o s =
            sameRun what (runIdentity (report n (oldTrial fixedSchedule (oldRunTrialPRHiddenState o))))
                         (fixedRun (runTrialPRHiddenState s))
      fixedPR "PR hidden state, uniformAll" oldUniformAll uniformAll
      fixedPR "PR hidden state, plus2" oldPlus2 plus2
      h  <- seeded 11 (report n (oldTrial randomScheduleIO (oldRunTrialPRHiddenState oldUniformAll)))
      h' <- seeded 11 (chshReport n randomScheduleIO (runTrialPRHiddenState uniformAll))
      sameRun "PR hidden state, uniformAll, random schedule" h h'

  , TestLabel "superdeterminism runs through the common trial as it ran through its own" $
    TestCase $ do
      sameRun "cheatLam, fixed schedule"
        (runIdentity (report n (oldTrialSuperdet fixedSchedule oldCheatLam)))
        (runIdentity (reportSuperdet n fixedSchedule cheatLam))
      sameRun "legalAllPlus, fixed schedule"
        (runIdentity (report n (oldTrialSuperdet fixedSchedule oldLegalAllPlus)))
        (runIdentity (reportSuperdet n fixedSchedule legalAllPlus))
      c  <- seeded 4 (report n (oldTrialSuperdet randomScheduleIO oldCheatLam))
      c' <- seeded 4 (reportSuperdet n randomScheduleIO cheatLam)
      sameRun "cheatLam, random schedule" c c'
      d  <- seeded 10 (report n (oldTrialSuperdet randomScheduleIO oldCheatNS))
      d' <- seeded 10 (reportSuperdet n randomScheduleIO cheatNS)
      sameRun "cheatNS, random schedule" d d'

  , TestLabel "a legal sampler is a bad one that ignores the settings" $ TestCase $ do
      let check :: String -> Sampler Identity -> Assertion
          check what samp =
            sameRun what (runIdentity (reportSuperdet n fixedSchedule (const samp)))
                         (fixedRun (runTrialLHV samp))
      check "plus2" plus2
      check "uniformAll" uniformAll
      check "biasPlus2Loophole 7 8" (biasPlus2Loophole 7 8)
      r  <- seeded 1 (reportSuperdet n randomScheduleIO (const uniformAll))
      r' <- seeded 1 (chshReport n randomScheduleIO (runTrialLHV uniformAll))
      sameRun "uniformAll, random schedule" r r'

  , TestLabel "the submission's superdeterministic membrane is an LHV membrane that re-runs the schedule" $
    TestCase $ do
      sameRun "fixed schedule"
        (fixedRun (oldRunTrialSuperdet fixedSchedule oldCheatLam))
        (fixedRun (runTrialLHV (peek fixedSchedule cheatLam)))
      o  <- seeded 4 (chshReport n randomScheduleIO (oldRunTrialSuperdet randomScheduleIO oldCheatLam))
      o' <- seeded 4 (chshReport n randomScheduleIO (runTrialLHV (peek randomScheduleIO cheatLam)))
      sameRun "random schedule" o o'

  , TestLabel "through the shared step, a sampler of the legal type sees the settings" $
    TestCase $ do
      let r = fixedRun (runTrialLHV (peek fixedSchedule cheatLam))
      signals "peek fixedSchedule cheatLam, fixed schedule" 4 ["Bob at B1"] r
      -- a schedule that does not read the step: the peeked settings
      -- are not the trial's, and the cheat is gone ...
      p <- seeded 4 (chshReport n randomScheduleIO (runTrialLHV (peek randomScheduleIO cheatLam)))
      noSignal "peek randomScheduleIO cheatLam, random schedule" 0.1 2 p
      -- ... while the bad sampler, handed the trial's settings, keeps it
      q <- seeded 4 (reportSuperdet n randomScheduleIO cheatLam)
      signals "cheatLam, random schedule" 4 ["Bob at B1"] q

  , TestLabel "a sampler that cannot read the step does not follow the fixed schedule" $
    TestCase $ do
      f <- seeded 13 (chshReport n fixedSchedule (runTrialLHV (fresh uniformIO)))
      noSignal "fresh uniformIO, fixed schedule" 0.1 0 f
      signals "uniformAll, fixed schedule" 0 ["Bob at B0"] (fixedRun (runTrialLHV uniformAll))

  , TestLabel "the PR hidden state is no-signaling for the uniform sampler only" $
    TestCase $ do
      noSignal "PR hidden state, uniformAll, fixed schedule" 0 4
        (fixedRun (runTrialPRHiddenState uniformAll))
      h <- seeded 11 (chshReport n randomScheduleIO (runTrialPRHiddenState uniformAll))
      noSignal "PR hidden state, uniformAll, random schedule" 0 4 h
      signals "PR hidden state, plus2, fixed schedule" 4 ["Bob at B1"]
        (fixedRun (runTrialPRHiddenState plus2))

  , TestLabel "the Bell hidden state gives the Bell table when every context meets a uniform λ" $
    TestCase $ do
      -- 80000 trials: each context meets each of the 16 strategies
      -- 1250 times under uniformBlocks
      let r = runIdentity (chshReport 80000 fixedSchedule (runTrialBellHiddenState uniformBlocks))
          bell = [ [10000, 0, 0, 10000], [7500, 2500, 2500, 7500]
                 , [7500, 2500, 2500, 7500], [2500, 7500, 7500, 2500] ]
      noSignal "Bell hidden state, uniformBlocks, fixed schedule" 0 2.5 r
      assertEqual "the Bell table, count for count" bell
        [ [ numTrials (table r) a b (== xy) | xy <- outcomePairs ] | (a, b) <- contexts ]
      h <- seeded 15 (chshReport n randomScheduleIO (runTrialBellHiddenState uniformAll))
      noSignal "Bell hidden state, uniformAll, random schedule" 0.06 2.5 h
      -- the cycling sampler fixes Bob's entries, hence the coins, with the context
      noSignal "Bell hidden state, uniformAll, fixed schedule: the PR box" 0 4
        (fixedRun (runTrialBellHiddenState uniformAll))

  , TestLabel "the Bell boxes are dual: with Bob's first, a uniform λ gives the same table" $
    TestCase $ do
      let bobFirst = do
            (a, b) <- fixedSchedule
            (x, y) <- runTrialBellHiddenState uniformBlocks
                        (do { y <- localB b; x <- localA a; pure (x, y) })
            pure (Record a b x y)
          aliceRun = runIdentity (chshReport 80000 fixedSchedule (runTrialBellHiddenState uniformBlocks))
          bobRun   = runIdentity (report 80000 bobFirst)
      noSignal "Bell hidden state, Bob's box first" 0 2.5 bobRun
      assertBool "the two orders give the same table, count for count" (table aliceRun == table bobRun)

  , TestLabel "|00> + |11>, measured with Z or (Z + sqrt 3 X)/2 and Z or (Z - sqrt 3 X)/2, gives the Bell table" $
    TestCase $ do
      let turned s = scale (1 / 2) (add pauliZ (scale (s * sqrt 3) pauliX))
          agree oa ob = sum [ probability (projector oa x >< projector ob x) maximallyEntangled
                            | x <- [Plus, Minus] ]
      assertBool "they agree with probabilities 1, 3/4, 3/4 and 1/4"
        (and (zipWith (\u v -> abs (u - v) < 1e-12)
                [ agree oa ob | oa <- [pauliZ, turned 1], ob <- [pauliZ, turned (-1)] ]
                [1, 0.75, 0.75, 0.25]))
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  unless (errors result == 0 && failures result == 0) exitFailure
