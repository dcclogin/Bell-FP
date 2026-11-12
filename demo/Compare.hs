module Main (main) where

-- This package's sampler and trial code (revision 4's, unchanged in
-- revision 5) against revision 3's, run for run, and the runs that show
-- what the shared step lets a sampler do.
--
--   cabal run bell-fp-compare                  print both tables
--   cabal run bell-fp-compare -- --latex DIR   also write them as
--                                                       LaTeX rows for the slides
--
-- Every model of the paper is run twice, through revision 3's sampler
-- code (demo/Old.hs) and through this package's, from the same seed.
-- The program stops if any two tables differ. What is compared is the
-- code that draws strategies and runs trials: both sides use this
-- package's trial models. (The quantum and PR models themselves have
-- changed since revision 3, see CHSH.Quantum and CHSH.PR, so their
-- tables are not revision 3's.)

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling
import CHSH.Clocked (runTrialClockLoose, runTrialClockTight)
import CHSH.Identity (liftIdentity)
import CHSH.LHV
import CHSH.PR (runTrialPR)
import CHSH.PRState (runTrialPRHiddenState)
import CHSH.Quantum (runTrialQuantum)
import CHSH.Shared (runTrialState)
import CHSH.Superdet
import Old

import Control.Monad (forM, forM_, unless)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Printf (printf)

n :: Int
n = 80000

data Row = Row
  { model    :: String
  , sampler  :: String       -- as LaTeX, empty for none
  , schedule :: String
  , oldRun   :: IO Report    -- revision 3's code
  , newRun   :: IO Report    -- this package's
  }

exact :: Report -> IO Report
exact = pure

fixedOld, fixedNew :: TrialModel t => RunTrial Identity t -> IO Report
fixedOld m = exact (runIdentity (report n (oldTrial fixedSchedule m)))
fixedNew m = exact (runIdentity (chshReport n fixedSchedule m))

randomOld, randomNew :: TrialModel t => Int -> RunTrial IO t -> IO Report
randomOld s m = seeded s (report n (oldTrial randomScheduleIO m))
randomNew s m = seeded s (chshReport n randomScheduleIO m)

tt :: String -> String
tt s = "\\texttt{" ++ s ++ "}"

-- every model of the paper, as app/Main.hs runs it
models :: [Row]
models =
  [ Row "Pure boxes" "" "fixed" (fixedOld liftIdentity) (fixedNew liftIdentity)
  , Row "Pure boxes" "" "random" (randomOld 0 liftIdentity) (randomNew 0 liftIdentity)
  , Row "Local hidden variables" (tt "plus2") "fixed"
      (fixedOld (oldRunTrialLHV oldPlus2)) (fixedNew (runTrialLHV plus2))
  , Row "Local hidden variables" (tt "minus2") "fixed"
      (fixedOld (oldRunTrialLHV oldMinus2)) (fixedNew (runTrialLHV minus2))
  , Row "Local hidden variables" (tt "uniformAll") "fixed"
      (fixedOld (oldRunTrialLHV oldUniformAll)) (fixedNew (runTrialLHV uniformAll))
  , Row "Local hidden variables" (tt "uniformAll") "random"
      (randomOld 1 (oldRunTrialLHV oldUniformAll)) (randomNew 1 (runTrialLHV uniformAll))
  , Row "Local hidden variables" (tt "biasPlus2Loophole 7 8") "fixed"
      (fixedOld (oldRunTrialLHV (oldBiasPlus2Loophole 7 8)))
      (fixedNew (runTrialLHV (biasPlus2Loophole 7 8)))
  , Row "Local hidden variables" (tt "biasPlus2Blocks 7 8") "fixed"
      (fixedOld (oldRunTrialLHV (oldBiasPlus2Blocks 7 8)))
      (fixedNew (runTrialLHV (biasPlus2Blocks 7 8)))
  , Row "Clocked boxes, clock visible" "" "fixed"
      (fixedOld runTrialClockLoose) (fixedNew runTrialClockLoose)
  , Row "Clocked boxes, clock visible" "" "random"
      (randomOld 2 runTrialClockLoose) (randomNew 2 runTrialClockLoose)
  , Row "Clocked boxes, clock hidden" "" "fixed"
      (fixedOld runTrialClockTight) (fixedNew runTrialClockTight)
  , Row "Clocked boxes, clock hidden" "" "random"
      (randomOld 3 runTrialClockTight) (randomNew 3 runTrialClockTight)
  , Row "Superdeterminism" (tt "cheatLam") "fixed"
      (exact (runIdentity (report n (oldTrialSuperdet fixedSchedule oldCheatLam))))
      (exact (runIdentity (reportSuperdet n fixedSchedule cheatLam)))
  , Row "Superdeterminism" (tt "cheatLam") "random"
      (seeded 4 (report n (oldTrialSuperdet randomScheduleIO oldCheatLam)))
      (seeded 4 (reportSuperdet n randomScheduleIO cheatLam))
  , Row "Superdeterminism" (tt "legalAllPlus") "fixed"
      (exact (runIdentity (report n (oldTrialSuperdet fixedSchedule oldLegalAllPlus))))
      (exact (runIdentity (reportSuperdet n fixedSchedule legalAllPlus)))
  , Row "Superdeterminism" (tt "cheatNS") "random"
      (seeded 10 (report n (oldTrialSuperdet randomScheduleIO oldCheatNS)))
      (seeded 10 (reportSuperdet n randomScheduleIO cheatNS))
  , Row "Shared state" "" "fixed" (fixedOld runTrialState) (fixedNew runTrialState)
  , Row "Quantum" "" "fixed"
      (seeded 5 (report n (oldTrial fixedSchedule runTrialQuantum)))
      (seeded 5 (chshReport n fixedSchedule runTrialQuantum))
  , Row "Quantum" "" "random" (randomOld 6 runTrialQuantum) (randomNew 6 runTrialQuantum)
  , Row "PR box" "" "fixed"
      (seeded 7 (report n (oldTrial fixedSchedule runTrialPR)))
      (seeded 7 (chshReport n fixedSchedule runTrialPR))
  , Row "PR box" "" "random" (randomOld 8 runTrialPR) (randomNew 8 runTrialPR)
  , Row "PR hidden state" (tt "uniformAll") "fixed"
      (fixedOld (oldRunTrialPRHiddenState oldUniformAll))
      (fixedNew (runTrialPRHiddenState uniformAll))
  , Row "PR hidden state" (tt "uniformAll") "random"
      (randomOld 11 (oldRunTrialPRHiddenState oldUniformAll))
      (randomNew 11 (runTrialPRHiddenState uniformAll))
  , Row "PR hidden state" (tt "plus2") "fixed"
      (fixedOld (oldRunTrialPRHiddenState oldPlus2))
      (fixedNew (runTrialPRHiddenState plus2))
  ]

-- The runs about the shared step. Each sampler has the legal type
-- Sampler e. For the two peek rows, the "old" run is the
-- submission's superdeterministic membrane, runTrialSuperdet sched
-- cheatLam, which must give the same table.
stepRows :: [Row]
stepRows =
  [ Row "" (tt "uniformAll") "fixed"
      (fixedOld (oldRunTrialLHV oldUniformAll)) (fixedNew (runTrialLHV uniformAll))
  , Row "" (tt "biasPlus2Loophole 7 8") "fixed"
      (fixedOld (oldRunTrialLHV (oldBiasPlus2Loophole 7 8)))
      (fixedNew (runTrialLHV (biasPlus2Loophole 7 8)))
  , Row "" (tt "peek fixedSchedule cheatLam") "fixed"
      (fixedNew (oldRunTrialSuperdet fixedSchedule oldCheatLam))
      (fixedNew (runTrialLHV (peek fixedSchedule cheatLam)))
  , Row "" (tt "peek randomScheduleIO cheatLam") "random"
      (randomNew 4 (oldRunTrialSuperdet randomScheduleIO oldCheatLam))
      (randomNew 4 (runTrialLHV (peek randomScheduleIO cheatLam)))
  , Row "" (tt "uniformAll") "random"
      (randomOld 1 (oldRunTrialLHV oldUniformAll)) (randomNew 1 (runTrialLHV uniformAll))
  , Row "" (tt "fresh uniformIO") "fixed"
      (seeded 13 (chshReport n fixedSchedule (runTrialLHV (fresh uniformIO))))
      (seeded 13 (chshReport n fixedSchedule (runTrialLHV (fresh uniformIO))))
  ]

-- The submission computed the CHSH value of superdeterminism with
-- chshSuperdet: the signed scores of all trials, averaged, times 4.
-- Its trials are those of revision 3's trialSuperdet, in the same
-- order of draws.
submittedChshSuperdet :: [Record] -> Double
submittedChshSuperdet rs = 4 * fromIntegral (sum (map score rs)) / fromIntegral (length rs)
  where
    score (Record a b x y) = case (a, b) of
      (A1, B1) -> negate (trialValue (x, y))
      _        -> trialValue (x, y)

------------------------------------------------------------

verdict :: Report -> String
verdict r
  | noSignalingIn r = "ok"
  | otherwise       = "violated: " ++ intercalate ", " (signaling r)

verdictTex :: Report -> String
verdictTex r
  | noSignalingIn r = "\\yes"
  | otherwise       = "\\no\\ " ++ intercalate ", " (signaling r)

sameTable :: Report -> Report -> Bool
sameTable r r' = table r == table r' && signaling r == signaling r'

plain :: String -> String
plain = filter (`notElem` "{}") . strip
  where strip ('\\':'t':'e':'x':'t':'t':'t':rest) = strip rest
        strip (c:rest) = c : strip rest
        strip [] = []

runRows :: String -> [Row] -> IO [(Row, Report)]
runRows what rows = forM rows $ \r -> do
  old <- oldRun r
  new <- newRun r
  unless (sameTable old new) $ do
    putStrLn (what ++ ": the two versions differ on "
              ++ intercalate ", " [model r, plain (sampler r), schedule r])
    exitFailure
  pure (r, new)

texRow :: Bool -> (Row, Report) -> String
texRow withModel (r, rep) =
  intercalate " & "
    ([ model r | withModel ] ++
     [ if null (sampler r) then "--" else sampler r
     , schedule r
     , printf "%.3f" (chshValue rep)
     , verdictTex rep ])
  ++ " \\\\"

main :: IO ()
main = do
  args <- getArgs
  ms <- runRows "every model" models
  ss <- runRows "the shared step" stepRows
  let subFixed = submittedChshSuperdet
                   (runIdentity (experiment n (oldTrialSuperdet fixedSchedule oldCheatLam)))
  subRandom <- submittedChshSuperdet
                 <$> seeded 4 (experiment n (oldTrialSuperdet randomScheduleIO oldCheatLam))

  printf "Every model of the paper, %d trials per run: this package's trial and sampler code gives the same table as revision 3's\n" n
  putStrLn (replicate 92 '-')
  forM_ ms $ \(r, rep) ->
    printf "%-29s %-30s %-7s %7.3f   %s\n"
      (model r) (plain (sampler r)) (schedule r) (chshValue rep) (verdict rep)
  putStrLn ""
  putStrLn "Samplers of the legal type, and the step they share with the schedule"
  putStrLn (replicate 92 '-')
  forM_ ss $ \(r, rep) ->
    printf "%-38s %-7s %7.3f   %s\n"
      (plain (sampler r)) (schedule r) (chshValue rep) (verdict rep)

  case args of
    ["--latex", dir] -> do
      -- the runs whose code changed: those with a sampler
      writeFile (dir </> "compare-models.tex") $ unlines
        [ texRow True row | row@(r, _) <- ms, not (null (sampler r)) ]
      writeFile (dir </> "compare-step.tex") $ unlines (map (texRow False) ss)
      let withSampler = length [ () | (r, _) <- ms, not (null (sampler r)) ]
          value rows m smp sch = head
            [ printf "%.3f" (chshValue rep) :: String
            | (r, rep) <- rows, model r == m, plain (sampler r) == smp, schedule r == sch ]
          macro name v = "\\newcommand{\\" ++ name ++ "}{" ++ v ++ "}"
      writeFile (dir </> "compare-values.tex") $ unlines
        [ macro "nTrials" (show n)
        , macro "nModels" (show (length ms))
        , macro "nSamplerRuns" (show withSampler)
        , macro "nOtherRuns" (show (length ms - withSampler))
        , macro "chshSuperdetFixed" (value ms "Superdeterminism" "cheatLam" "fixed")
        , macro "chshSuperdetRandom" (value ms "Superdeterminism" "cheatLam" "random")
        , macro "chshPeekFixed" (value ss "" "peek fixedSchedule cheatLam" "fixed")
        , macro "chshPeekRandom" (value ss "" "peek randomScheduleIO cheatLam" "random")
        , macro "chshFresh" (value ss "" "fresh uniformIO" "fixed")
        , macro "chshCyclingFixed" (value ss "" "uniformAll" "fixed")
        , macro "chshSubmittedFixed" (printf "%.3f" subFixed)
        , macro "chshSubmittedRandom" (printf "%.3f" subRandom) ]
      putStrLn ("\nwrote compare-models.tex, compare-step.tex, compare-values.tex to " ++ dir)
    [] -> pure ()
    _  -> do
      putStrLn "usage: bell-fp-compare [--latex DIR]"
      exitFailure
