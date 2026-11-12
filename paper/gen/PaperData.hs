module Main (main) where

-- The data of bell-fp.tex: every number, table and printed
-- report in the paper comes from a run of the package here.
--
--   cabal run -v0 bell-fp-paper-data -- DIR
--
-- writes into DIR
--   values.tex      \setval{KEY}{VALUE} for every number the text quotes
--   runs.tex        the rows of the table of every run, with both verdicts
--   table-RUN.tex   the empirical model of one run, one row per context
--   report-RUN.txt  the report of that run, as printReport prints it
--
-- The runs are those of app/Main.hs, with the same seeds. For every run
-- whose membrane ignores the settings, the program also runs the
-- separate check of the submission (Separate.hs, via Check.hs) on the
-- same membrane. It stops, writing nothing, if something the paper says
-- about the data does not hold.

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.BellTable (runTrialBellHiddenState)
import CHSH.Clocked (runTrialClockLoose, runTrialClockTight)
import CHSH.LHV
import CHSH.PR (prConstraint, runTrialPR, suchThat)
import CHSH.PRState (runTrialPRHiddenState)
import CHSH.Quantum ((><), Matrix, Source, add, collapse, correlation, entangled,
                     maximallyEntangled, measA, measB, observableA, observableB, pauliX, pauliZ,
                     probability, projector, runTrialQuantum, runTrialSource, scale)
import CHSH.Shared (runTrialState)
import CHSH.Superdet
import Check
import Examples
import Explored
import PureBoxes

import Control.Monad (forM, forM_, unless)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- trials per run, as in app/Main.hs
n :: Int
n = 80000

-- the exact CHSH value of a state, from its correlations
exactCHSH :: Source -> Double
exactCHSH psi = chshOf (correlation psi)

-- the partially entangled pair of the paper's runs, theta = pi/8
partial :: Source
partial = entangled (pi / 8)

-- states on which the claims about the quantum boxes are checked; every
-- outcome has a positive probability in each
quantumStates :: [Source]
quantumStates = [maximallyEntangled, partial, entangled 0.3, entangled 1.1]

-- the joint probability of (x, y) when Alice's box measures first, and
-- when Bob's does, as the quantum boxes compute it; and the formula that
-- the paper gives for both, <psi| P (x) Q |psi>
aliceFirst, bobFirst, onThePair :: Source -> ASetting -> BSetting -> Outcome -> Outcome -> Double
aliceFirst psi a b x y =
  probability (measA a x) psi * probability (measB b y) (collapse (measA a x) psi)
bobFirst psi a b x y =
  probability (measB b y) psi * probability (measA a x) (collapse (measB b y) psi)
onThePair psi a b x y =
  probability (projector (observableA a) x >< projector (observableB b) y) psi

-- Alice's frequency of Plus when the PR constraint keeps pairs of coins
-- biased to 3/4
-- A trial with Bob's box first (Section 9). For the boxes with a hidden
-- state, whose joint response is the default, it runs the two local
-- responses in the other order.
trialBobFirst :: (Monad e, TrialModel t) => Scheduler e -> RunTrial e t -> Exp e Record
trialBobFirst sched runTrial = do
  (a, b) <- sched
  (x, y) <- runTrial (do { y' <- localB b; x' <- localA a; pure (x', y') })
  pure (Record a b x y)

-- The PR box with Bob's coin drawn first.
prBobFirst :: Scheduler IO -> Exp IO Record
prBobFirst sched = do
  (a, b) <- sched
  (x, y) <- runTrialPR (do { y' <- localB b; x' <- localA a; pure (x', y') } `suchThat` prConstraint a b)
  pure (Record a b x y)

plusOnBiasedCoins :: ASetting -> BSetting -> Double
plusOnBiasedCoins a b = sum [ w | (x, w) <- kept, x == Plus ] / sum (map snd kept)
  where
    kept = [ (x, weight x * weight y) | (x, y) <- outcomePairs, prConstraint a b (x, y) ]
    weight o = if o == Plus then 0.75 else 0.25

-- the angle above which cos t |00> + sin t |11> exceeds the classical
-- bound with the paper's observables: sqrt 2 (1 + sin 2t) = 2
thetaClassical :: Double
thetaClassical = asin (sqrt 2 - 1) / 2

------------------------------------------------------------
-- The runs

data Run = Run
  { key      :: String          -- the name the paper uses for the run
  , model    :: String
  , sampler  :: String          -- the code, empty for none
  , schedule :: String
  , seed     :: Maybe Int       -- the seed of a run in IO; Nothing for a run in Identity
  , runIt    :: IO Report
  , sepIt    :: Maybe (IO Sep)  -- Nothing: the membrane sees the settings
  }

-- A run in Identity draws nothing at random, and is exact.
exact :: String -> String -> String -> String -> Report -> Maybe (IO Sep) -> Run
exact k m s sc rep = Run k m s sc Nothing (pure rep)

-- A run in IO starts from the generator of its own seed. The table of
-- every run prints the seed from here, so it is the seed that ran.
fromSeed :: String -> String -> String -> String -> Int -> IO Report -> Maybe (IO Sep) -> Run
fromSeed k m s sc sd act = Run k m s sc (Just sd) (seeded sd act)

fixedRun :: TrialModel t => RunTrial Identity t -> Report
fixedRun m = runIdentity (chshReport n fixedSchedule m)

-- unseeded: fromSeed seeds it
randomRun :: TrialModel t => RunTrial IO t -> IO Report
randomRun m = chshReport n randomScheduleIO m

-- the separate check takes no schedule: in Identity it is exact, and
-- in IO it is seeded once per membrane, whatever the run's schedule
sepId :: TrialModel t => RunTrial Identity t -> Maybe (IO Sep)
sepId m = Just (pure (runIdentity (separate m)))

sepIO :: TrialModel t => Int -> RunTrial IO t -> Maybe (IO Sep)
sepIO s m = Just (seeded s (separate m))

-- every run of app/Main.hs, in the order of the paper, with its seeds
runs :: [Run]
runs =
  [ exact "pure-fixed" "Pure boxes" "" "fixed"
        (pureFixed n) (Just pureSeparate)
  , fromSeed "pure-random" "Pure boxes" "" "random" 0
        (pureRandom n) (Just pureSeparate)
  , exact "shared" "Shared location" "" "fixed"
        (fixedRun runTrialState) (sepId runTrialState)
  , exact "clock-visible-fixed" "Clocked, clock visible" "" "fixed"
        (fixedRun runTrialClockLoose) (sepId runTrialClockLoose)
  , fromSeed "clock-visible-random" "Clocked, clock visible" "" "random" 2
        (randomRun runTrialClockLoose) (sepId runTrialClockLoose)
  , exact "clock-hidden-fixed" "Clocked, clock hidden" "" "fixed"
        (fixedRun runTrialClockTight) (sepId runTrialClockTight)
  , fromSeed "clock-hidden-random" "Clocked, clock hidden" "" "random" 3
        (randomRun runTrialClockTight) (sepId runTrialClockTight)
  , exact "lhv-plus2" "Local hidden variables" "plus2" "fixed"
        (fixedRun (runTrialLHV plus2)) (sepId (runTrialLHV plus2))
  , exact "lhv-minus2" "Local hidden variables" "minus2" "fixed"
        (fixedRun (runTrialLHV minus2)) (sepId (runTrialLHV minus2))
  , exact "lhv-uniform-fixed" "Local hidden variables" "uniformAll" "fixed"
        (fixedRun (runTrialLHV uniformAll)) (sepId (runTrialLHV uniformAll))
  , fromSeed "lhv-uniform-random" "Local hidden variables" "uniformAll" "random" 1
        (randomRun (runTrialLHV uniformAll)) (sepId (runTrialLHV uniformAll))
  , exact "lhv-loophole" "Local hidden variables" "biasPlus2Loophole 7 8" "fixed"
        (fixedRun (runTrialLHV (biasPlus2Loophole 7 8)))
        (sepId (runTrialLHV (biasPlus2Loophole 7 8)))
  , exact "lhv-blocks" "Local hidden variables" "biasPlus2Blocks 7 8" "fixed"
        (fixedRun (runTrialLHV (biasPlus2Blocks 7 8)))
        (sepId (runTrialLHV (biasPlus2Blocks 7 8)))
  , fromSeed "lhv-fresh" "Local hidden variables" "fresh uniformIO" "fixed" 13
        (chshReport n fixedSchedule (runTrialLHV (fresh uniformIO)))
        (sepIO 113 (runTrialLHV (fresh uniformIO)))
  , exact "superdet-cheat-fixed" "Superdeterminism" "cheatLam" "fixed"
        (runIdentity (reportSuperdet n fixedSchedule cheatLam)) Nothing
  , fromSeed "superdet-cheat-random" "Superdeterminism" "cheatLam" "random" 4
        (reportSuperdet n randomScheduleIO cheatLam) Nothing
  , exact "superdet-legal" "Superdeterminism" "legalAllPlus" "fixed"
        (runIdentity (reportSuperdet n fixedSchedule legalAllPlus)) Nothing
  , fromSeed "superdet-cheatns" "Superdeterminism" "cheatNS" "random" 10
        (reportSuperdet n randomScheduleIO cheatNS) Nothing
  , exact "peek-fixed" "LHV, sampler peeks" "peek fixedSchedule cheatLam" "fixed"
        (fixedRun (runTrialLHV (peek fixedSchedule cheatLam)))
        (sepId (runTrialLHV (peek fixedSchedule cheatLam)))
  , fromSeed "peek-random" "LHV, sampler peeks" "peek randomScheduleIO cheatLam" "random" 4
        (randomRun (runTrialLHV (peek randomScheduleIO cheatLam)))
        (sepIO 120 (runTrialLHV (peek randomScheduleIO cheatLam)))
  , fromSeed "quantum-fixed" "Quantum" "" "fixed" 5
        (chshReport n fixedSchedule runTrialQuantum) (sepIO 105 runTrialQuantum)
  , fromSeed "quantum-random" "Quantum" "" "random" 6
        (randomRun runTrialQuantum) (sepIO 105 runTrialQuantum)
  , fromSeed "quantum-partial" "Quantum, $\\theta = \\pi/8$" "" "fixed" 14
        (chshReport n fixedSchedule (runTrialSource partial))
        (sepIO 114 (runTrialSource partial))
  , fromSeed "pr-fixed" "PR box" "" "fixed" 7
        (chshReport n fixedSchedule runTrialPR) (sepIO 107 runTrialPR)
  , fromSeed "pr-random" "PR box" "" "random" 8
        (randomRun runTrialPR) (sepIO 107 runTrialPR)
  , exact "prstate-uniform-fixed" "PR hidden state" "uniformAll" "fixed"
        (fixedRun (runTrialPRHiddenState uniformAll))
        (sepId (runTrialPRHiddenState uniformAll))
  , fromSeed "prstate-uniform-random" "PR hidden state" "uniformAll" "random" 11
        (randomRun (runTrialPRHiddenState uniformAll))
        (sepId (runTrialPRHiddenState uniformAll))
  , exact "prstate-plus2" "PR hidden state" "plus2" "fixed"
        (fixedRun (runTrialPRHiddenState plus2))
        (sepId (runTrialPRHiddenState plus2))
  , exact "bell-blocks-fixed" "Bell table, hidden state" "uniformBlocks" "fixed"
        (fixedRun (runTrialBellHiddenState uniformBlocks))
        (sepId (runTrialBellHiddenState uniformBlocks))
  , fromSeed "bell-uniform-random" "Bell table, hidden state" "uniformAll" "random" 15
        (randomRun (runTrialBellHiddenState uniformAll))
        (sepId (runTrialBellHiddenState uniformAll))
  , exact "bell-uniform-fixed" "Bell table, hidden state" "uniformAll" "fixed"
        (fixedRun (runTrialBellHiddenState uniformAll))
        (sepId (runTrialBellHiddenState uniformAll))
  ]

------------------------------------------------------------
-- What the paper says about the runs. Each claim names the run, and
-- the program stops if one fails.

data Result = Result
  { rRun :: Run
  , rRep :: Report
  , rSep :: Maybe Sep
  }

-- the run signals at exactly these conditions (none: it does not)
signalsAt :: [String] -> Result -> Bool
signalsAt cs r = signaling (rRep r) == cs

chshIs :: Double -> Double -> Result -> Bool
chshIs tol v r = abs (chshValue (rRep r) - v) <= tol

-- the frequency of Plus, for Alice (fst) or Bob (snd), in a context
plusOf :: ((Outcome, Outcome) -> Outcome) -> Report -> ASetting -> BSetting -> Double
plusOf who rep a b = freq (table rep) a b (\xy -> who xy == Plus)

separatePasses, separateFails :: Result -> Bool
separatePasses r = maybe False sepOk (rSep r)
separateFails r = maybe False (not . sepOk) (rSep r)

claims :: [(String, String, Result -> Bool)]
claims =
  [ ("pure-fixed", "CHSH 2, no signaling", \r -> chshIs 0 2 r && signalsAt [] r)
  , ("shared", "CHSH 4, Bob at B1 signals, and both checks see it",
      \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r && separateFails r)
  , ("clock-visible-fixed", "CHSH 4, Bob at B1 signals, the separate check passes",
      \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r && separatePasses r)
  , ("clock-visible-random", "CHSH about 1, no signaling",
      \r -> chshIs 0.05 1 r && signalsAt [] r)
  , ("clock-hidden-fixed", "CHSH 2, no signaling", \r -> chshIs 0 2 r && signalsAt [] r)
  , ("clock-hidden-random", "CHSH 2, no signaling", \r -> chshIs 0 2 r && signalsAt [] r)
  , ("lhv-plus2", "CHSH 2, no signaling", \r -> chshIs 0 2 r && signalsAt [] r)
  , ("lhv-minus2", "CHSH -2, no signaling", \r -> chshIs 0 (-2) r && signalsAt [] r)
  , ("lhv-uniform-fixed", "CHSH 0, Bob at B0 signals, the separate check passes",
      \r -> chshIs 0 0 r && signalsAt ["Bob at B0"] r && separatePasses r)
  , ("lhv-uniform-random", "CHSH about 0, no signaling",
      \r -> chshIs 0.05 0 r && signalsAt [] r)
  , ("lhv-loophole", "CHSH 3, Bob at B1 signals, the separate check passes",
      \r -> chshIs 0 3 r && signalsAt ["Bob at B1"] r && separatePasses r)
  , ("lhv-blocks", "CHSH 1.5, no signaling", \r -> chshIs 0 1.5 r && signalsAt [] r)
  , ("lhv-fresh", "CHSH about 0, no signaling", \r -> chshIs 0.05 0 r && signalsAt [] r)
  , ("superdet-cheat-fixed", "CHSH 4, Bob at B1 signals",
      \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r)
  , ("superdet-cheat-random", "CHSH 4, Bob at B1 signals",
      \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r)
  , ("superdet-legal", "CHSH 2, no signaling", \r -> chshIs 0 2 r && signalsAt [] r)
  , ("superdet-cheatns", "CHSH 4, no signaling", \r -> chshIs 0 4 r && signalsAt [] r)
  , ("peek-fixed", "CHSH 4, Bob at B1 signals, the separate check passes",
      \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r && separatePasses r)
  , ("peek-random", "CHSH about 2, no signaling", \r -> chshIs 0.05 2 r && signalsAt [] r)
  , ("quantum-fixed", "CHSH about 2 sqrt 2, no signaling",
      \r -> chshIs 0.05 (2 * sqrt 2) r && signalsAt [] r && separatePasses r)
  , ("quantum-random", "CHSH about 2 sqrt 2, no signaling",
      \r -> chshIs 0.05 (2 * sqrt 2) r && signalsAt [] r)
  , ("quantum-partial", "CHSH about 1 + sqrt 2, no signaling, marginals as the Born rule predicts",
      \r -> chshIs 0.05 (exactCHSH partial) r && signalsAt [] r && separatePasses r
             && and [ abs (plusOf fst (rRep r) a b - probability (measA a Plus) partial) < 0.015
                      && abs (plusOf snd (rRep r) a b - probability (measB b Plus) partial) < 0.015
                    | (a, b) <- contexts ])
  , ("pr-fixed", "CHSH 4, no signaling", \r -> chshIs 0 4 r && signalsAt [] r && separatePasses r)
  , ("pr-random", "CHSH 4, no signaling", \r -> chshIs 0 4 r && signalsAt [] r)
  , ("prstate-uniform-fixed", "CHSH 4, no signaling", \r -> chshIs 0 4 r && signalsAt [] r)
  , ("prstate-uniform-random", "CHSH 4, no signaling", \r -> chshIs 0 4 r && signalsAt [] r)
  , ("prstate-plus2", "CHSH 4, Bob at B1 signals", \r -> chshIs 0 4 r && signalsAt ["Bob at B1"] r)
  , ("bell-blocks-fixed", "CHSH 2.5, no signaling, the Bell table count for count",
      \r -> chshIs 0 2.5 r && signalsAt [] r && separatePasses r && countsAre bellTable r)
  , ("bell-uniform-random", "CHSH about 2.5, no signaling",
      \r -> chshIs 0.05 2.5 r && signalsAt [] r)
  , ("bell-uniform-fixed", "CHSH 4, no signaling, the PR box count for count",
      \r -> chshIs 0 4 r && signalsAt [] r && countsAre prTable r)
  ]

-- the counts of a run's table, context by context, in the order of
-- outcomePairs
countsAre :: [[Int]] -> Result -> Bool
countsAre cs r = cs == [ [ numTrials (table (rRep r)) a b (== xy) | xy <- outcomePairs ] | (a, b) <- contexts ]

-- Figure 3 of Abramsky's "Contextuality: At the Borders of Paradox",
-- and the PR box, as counts of n/4 trials a context
bellTable, prTable :: [[Int]]
bellTable = map (map (* (n `div` 32)))
  [[4, 0, 0, 4], [3, 1, 1, 3], [3, 1, 1, 3], [1, 3, 3, 1]]
prTable = map (map (* (n `div` 8))) [[1, 0, 0, 1], [1, 0, 0, 1], [1, 0, 0, 1], [0, 1, 1, 0]]

-- the probability that the outcomes on |00> + |11> agree, when Alice
-- measures oa and Bob ob, by the Born rule on the pair
agreeOn :: Matrix -> Matrix -> Double
agreeOn oa ob = sum [ probability (projector oa x >< projector ob x) maximallyEntangled
                    | x <- [Plus, Minus] ]

-- (Z + sqrt 3 X)/2 and (Z - sqrt 3 X)/2: with Z, the observables that
-- give the Bell table
turned :: Double -> Matrix
turned s = scale (1 / 2) (add pauliZ (scale (s * sqrt 3) pauliX))

-- the paper's examples (Examples.hs) against the package
exampleClaims :: IO [(String, Bool)]
exampleClaims = do
  let steps = [0 .. n - 1]
      draws :: Exp Identity a -> [a]
      draws d = runIdentity (mapM (runReaderT d) steps)
      legal :: Sampler Identity -> Bool
      legal samp =
        runIdentity (experiment n (trial fixedSchedule (blind (runTrialLHV samp))))
          == runIdentity (experiment n (trialFrom (independent fixedSchedule samp)))
        && draws (independent fixedSchedule samp)
             == draws (dependent fixedSchedule (const samp))
      bad :: BadSampler Identity -> Bool
      bad b =
        runIdentity (experiment n (trial fixedSchedule (runTrialSuperdet b)))
          == runIdentity (experiment n (trialFrom (dependent fixedSchedule b)))
  ioLegal <- do
    xs <- seeded 1 (experiment n (trial randomScheduleIO (blind (runTrialLHV uniformAll))))
    ys <- seeded 1 (experiment n (trialFrom (independent randomScheduleIO uniformAll)))
    pure (xs == ys)
  ioBad <- do
    xs <- seeded 4 (experiment n (trial randomScheduleIO (runTrialSuperdet cheatLam)))
    ys <- seeded 4 (experiment n (trialFrom (dependent randomScheduleIO cheatLam)))
    pure (xs == ys)
  pure
    [ ("an LHV trial draws its settings and strategy with liftA2",
        all legal [plus2, uniformAll, biasPlus2Loophole 7 8, altExtrema] && ioLegal)
    , ("a superdeterministic trial draws them with a bind",
        all bad [cheatLam, legalAllPlus, const uniformAll] && ioBad)
    ]

------------------------------------------------------------
-- LaTeX

tt :: String -> String
tt s = "\\texttt{" ++ s ++ "}"

-- "Bob at B1" as the paper sets it
condTeX :: String -> String
condTeX c = case words c of
  [who, "at", s] -> who ++ " at " ++ setting s
  _              -> c
  where setting [p, d] = "\\texttt{" ++ [p, d] ++ "}"
        setting s      = s

verdictTeX :: Report -> String
verdictTeX r = case signaling r of
  [] -> "\\yes"
  cs -> "\\no\\ " ++ intercalate ", " (map condTeX cs)

sepTeX :: Maybe Sep -> String
sepTeX Nothing  = "--"
sepTeX (Just s) = if sepOk s then "\\yes" else "\\no"

f3 :: Double -> String
f3 = printf "%.3f"

-- a value of the text: \setval{KEY}{VALUE}
setval :: String -> String -> String
setval k v = "\\setval{" ++ k ++ "}{" ++ v ++ "}"

-- the key of a condition, e.g. BobB1
condKey :: String -> String
condKey = concat . filter (/= "at") . words

freqOf :: (Int, Int) -> Double
freqOf (k, m) = if m == 0 then 0 else fromIntegral k / fromIntegral m

valuesOf :: Result -> [String]
valuesOf (Result run rep sep) =
  [ setval (key run ++ "/chsh") (f3 (chshValue rep))
  , setval (key run ++ "/ns") (verdictTeX rep)
  , setval (key run ++ "/sep") (sepTeX sep) ]
  ++ concat
     [ [ setval (key run ++ "/run/" ++ condKey (condition c) ++ "/1") (f3 (freqOf (first c)))
       , setval (key run ++ "/run/" ++ condKey (condition c) ++ "/2") (f3 (freqOf (second c))) ]
     | c <- comparisons (table rep) ]
  ++ concat
     [ [ setval (key run ++ "/sep/" ++ condKey c ++ "/1") (f3 p1)
       , setval (key run ++ "/sep/" ++ condKey c ++ "/2") (f3 p2) ]
     | Just s <- [sep], (c, (p1, p2)) <- sepRows s ]

-- one row of the table of every run; a little space before the first
-- run of each group of sections of the paper, and a mark (\missedrow)
-- on a run whose own table signals while the separate check passes
rowOf :: Result -> String
rowOf r@(Result run rep sep) =
  (if key run `elem` ["lhv-plus2", "quantum-fixed", "prstate-uniform-fixed"]
     then "\\addlinespace\n" else "")
  ++ (if separatePasses r && not (noSignalingIn rep) then "\\missedrow " else "")
  ++ intercalate " & "
  [ model run
  , if null (sampler run) then "--" else tt (sampler run)
  , schedule run
  , maybe "--" show (seed run)
  , "$" ++ f3 (chshValue rep) ++ "$"
  , verdictTeX rep
  , sepTeX sep ] ++ " \\\\"

-- the empirical model of a run: context, the four frequencies, both
-- marginals and the number of trials. The marginals that a violated
-- condition compares are marked \sig{..}.
tableOf :: Report -> String
tableOf rep = unlines
  [ intercalate " & " ([tt (showA a), tt (showB b)]
      ++ [ f3 (freq m a b (== xy)) | xy <- outcomePairs ]
      ++ [ mark (aliceMoved a) (f3 (freq m a b (\(x, _) -> x == Plus)))
         , mark (bobMoved b) (f3 (freq m a b (\(_, y) -> y == Plus)))
         , show (numTrials m a b (const True)) ]) ++ " \\\\"
  | (a, b) <- contexts ]
  where
    m = table rep
    moved = signaling rep
    aliceMoved a = ("Alice at " ++ showA a) `elem` moved
    bobMoved b = ("Bob at " ++ showB b) `elem` moved
    mark True v  = "\\sig{" ++ v ++ "}"
    mark False v = v

------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  dir <- case args of
    [d] -> pure d
    _   -> hPutStrLn stderr "usage: bell-fp-paper-data DIR" >> exitFailure

  results <- forM runs $ \run -> do
    rep <- runIt run
    sep <- sequence (sepIt run)
    pure (Result run rep sep)

  failures <- newIORef []
  let failWith :: IORef [String] -> String -> IO ()
      failWith ref msg = modifyIORef ref (msg :)
  forM_ claims $ \(k, what, holds) ->
    case [ r | r <- results, key (rRun r) == k ] of
      [r] -> unless (holds r) $ failWith failures (k ++ ": " ++ what)
      _   -> failWith failures (k ++ ": no such run")
  examples <- exampleClaims
  forM_ examples $ \(what, ok) -> unless ok $ failWith failures ("example: " ++ what)
  -- the two values of Section 2 that the paper computes directly
  let direct = [ ("chshId evaluates to 2", chshIdValue == 2)
               , ("chshST evaluates to 4", chshSTValue == 4)
               , ("|00> + |11> with Z or (Z + sqrt 3 X)/2, and Z or (Z - sqrt 3 X)/2, agrees 1, 3/4, 3/4, 1/4 of the time",
                  and (zipWith (\u v -> abs (u - v) < 1e-12)
                         [ agreeOn oa ob | oa <- [pauliZ, turned 1], ob <- [pauliZ, turned (-1)] ]
                         [1, 0.75, 0.75, 0.25]))
               , ("|00> + |11> has correlations 1/sqrt 2, -1/sqrt 2 in (A1, B1), and CHSH 2 sqrt 2",
                  and [ abs (correlation maximallyEntangled a b
                             - (if (a, b) == (A1, B1) then -1 else 1) / sqrt 2) < 1e-12
                      | (a, b) <- contexts ]
                  && abs (exactCHSH maximallyEntangled - 2 * sqrt 2) < 1e-12)
               , ("cos t |00> + sin t |11> has CHSH sqrt 2 (1 + sin 2t), for t = 0, pi/16, ..., pi/2",
                  and [ abs (exactCHSH (entangled t) - sqrt 2 * (1 + sin (2 * t))) < 1e-12
                      | k <- [0 .. 8 :: Int], let t = fromIntegral k * pi / 16 ])
               , ("entangled (pi/4) is |00> + |11>, normalized",
                  and (zipWith (\u v -> abs (u - v) < 1e-15) (entangled (pi / 4)) maximallyEntangled))
               , ("whichever box measures first, the joint probability is <psi| P (x) Q |psi>",
                  and [ abs (aliceFirst psi a b x y - onThePair psi a b x y) < 1e-12
                        && abs (bobFirst psi a b x y - onThePair psi a b x y) < 1e-12
                      | psi <- quantumStates, (a, b) <- contexts, (x, y) <- outcomePairs ])
               , ("summing the joint probability over one party's outcome gives the other's local response",
                  and [ abs (sum [ onThePair psi a b x' y | x' <- every ] - probability (measB b y) psi) < 1e-12
                        && abs (sum [ onThePair psi a b x y' | y' <- every ] - probability (measA a x) psi) < 1e-12
                      | psi <- quantumStates, (a, b) <- contexts, (x, y) <- outcomePairs ])
               , ("the PR constraint keeps two pairs in each context, and in them each party answers Plus once",
                  and [ length kept == 2 && length (filter ((== Plus) . fst) kept) == 1
                        && length (filter ((== Plus) . snd) kept) == 1
                      | (a, b) <- contexts, let kept = filter (prConstraint a b) outcomePairs ])
               , ("on coins biased to 3/4, the PR constraint moves Alice's marginal at A1 with Bob's setting",
                  abs (plusOnBiasedCoins A1 B0 - 0.9) < 1e-12 && abs (plusOnBiasedCoins A1 B1 - 0.5) < 1e-12)
               , ("the CHSH value exceeds 2 exactly above thetaClassical",
                  exactCHSH (entangled (thetaClassical - 1e-6)) < 2
                  && exactCHSH (entangled (thetaClassical + 1e-6)) > 2) ]
  forM_ direct $ \(what, ok) -> unless ok $ failWith failures what

  -- The order of the boxes (Section 9): the runs that the paper describes
  -- with Bob's box first, from the seeds of the paper's runs.
  let prHiddenBobFixed = runIdentity (report n (trialBobFirst fixedSchedule (runTrialPRHiddenState uniformAll)))
      bellBobFixed     = runIdentity (report n (trialBobFirst fixedSchedule (runTrialBellHiddenState uniformAll)))
      bellBobBlocks    = runIdentity (report n (trialBobFirst fixedSchedule (runTrialBellHiddenState uniformBlocks)))
  prHiddenBobRandom <- seeded 11 (report n (trialBobFirst randomScheduleIO (runTrialPRHiddenState uniformAll)))
  bellBobRandom     <- seeded 15 (report n (trialBobFirst randomScheduleIO (runTrialBellHiddenState uniformAll)))
  prBobFixed        <- seeded 7 (report n (prBobFirst fixedSchedule))
  prBobRandom       <- seeded 8 (report n (prBobFirst randomScheduleIO))
  let tableOf' k = [ table (rRep r) | r <- results, key (rRun r) == k ]
      ordered =
        [ ("with Bob's box first, the PR box from a hidden state signals (uniformAll, fixed schedule)",
            not (noSignalingIn prHiddenBobFixed) && chshValue prHiddenBobFixed == 4)
        , ("with Bob's box first, the Bell boxes signal and give CHSH 2 (uniformAll, fixed schedule)",
            not (noSignalingIn bellBobFixed) && chshValue bellBobFixed == 2)
        , ("with Bob's box first, the Bell boxes give the same table (uniformBlocks, fixed schedule)",
            tableOf' "bell-blocks-fixed" == [table bellBobBlocks])
        , ("with Bob's box first, the boxes with a hidden state do not signal (uniformAll, random schedule)",
            noSignalingIn prHiddenBobRandom && noSignalingIn bellBobRandom)
        , ("with Bob's coin first, the PR box gives CHSH 4 without signaling",
            all (\r -> noSignalingIn r && chshValue r == 4) [prBobFixed, prBobRandom]) ]
  forM_ ordered $ \(what, ok) -> unless ok $ failWith failures what
  bad <- readIORef failures
  unless (null bad) $ do
    mapM_ (hPutStrLn stderr . ("claim does not hold: " ++)) (reverse bad)
    exitFailure

  -- the runs the separate check passes and whose own table signals
  let missed = [ key (rRun r) | r <- results
                              , separatePasses r, not (noSignalingIn (rRep r)) ]
  unless (missed == ["clock-visible-fixed", "lhv-uniform-fixed", "lhv-loophole", "peek-fixed"]) $ do
    hPutStrLn stderr ("the runs the separate check misses are not the paper's: " ++ show missed)
    exitFailure

  createDirectoryIfMissing True dir
  let header = "% generated by bell-fp-paper-data; do not edit"
  writeFile (dir </> "values.tex") $ unlines $
    header
    : setval "nTrials" (show n)
    : setval "nPerContext" (show (n `div` 4))
    : setval "nSep" (show nSep)
    : setval "eps" (printf "%.2f" eps)
    : setval "nRuns" (show (length results))
    : setval "nMissed" (show (length missed))
    : setval "tsirelson" (f3 (2 * sqrt 2))
    : setval "chshId" (show chshIdValue)
    : setval "partial/chsh" (f3 (exactCHSH partial))
    : setval "partial/AliceA0" (f3 (probability (measA A0 Plus) partial))
    : setval "partial/AliceA1" (f3 (probability (measA A1 Plus) partial))
    : setval "partial/Bob" (f3 (probability (measB B0 Plus) partial))
    : setval "pr/biasedB0" (f3 (plusOnBiasedCoins A1 B0))
    : setval "pr/biasedB1" (f3 (plusOnBiasedCoins A1 B1))
    : setval "theta/zero" (f3 (exactCHSH (entangled 0)))
    : setval "theta/classical" (f3 thetaClassical)
    : setval "theta/classicalDeg" (printf "%.1f" (thetaClassical * 180 / pi))
    : setval "chshST" (show chshSTValue)
    : setval "order/bell-uniform-fixed/chsh" (f3 (chshValue bellBobFixed))
    : concatMap valuesOf results
  writeFile (dir </> "runs.tex") $ unlines $
    header : map rowOf results
  forM_ results $ \r -> do
    writeFile (dir </> ("table-" ++ key (rRun r) ++ ".tex")) (header ++ "\n" ++ tableOf (rRep r))
    writeFile (dir </> ("report-" ++ key (rRun r) ++ ".txt")) (renderReport (rRep r))
  printf "wrote the data of %d runs into %s; all %d claims hold\n"
    (length results) dir (length claims + length examples + length direct + length ordered + 1)
