module Main (main) where

import CHSH.Clocked (runTrialClockLoose, runTrialClockTight)
import CHSH.Experiment
import CHSH.Identity (liftIdentity)
import CHSH.BellTable (runTrialBellHiddenState)
import CHSH.LHV (biasPlus2Blocks, biasPlus2Loophole, fresh, minus2, plus2, runTrialLHV, uniformAll, uniformBlocks, uniformIO)
import CHSH.NoSignaling
import CHSH.PR (runTrialPR)
import CHSH.PRState (runTrialPRHiddenState)
import CHSH.Quantum (entangled, runTrialQuantum, runTrialSource)
import CHSH.Shared (runTrialState)
import CHSH.Superdet (cheatLam, cheatNS, legalAllPlus, peek, reportSuperdet)

import Control.Monad (forM_, unless)
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Text.Printf (printf)

------------------------------------------------------------
-- Every model of the paper, run once. The CHSH value and the
-- no-signaling verdict both come from that one run. For every run
-- that signals, its table follows, so that the reader can see which
-- marginal moved.

-- Trials per run: 20000 per context under the fixed schedule, as
-- many as the separate check of the original used.
n :: Int
n = 80000

data Run = Run
  { model    :: String
  , sampler  :: String
  , schedule :: String
  , runIt    :: IO Report
  }

-- runs in Identity are exact; runs in IO fix the generator first
exact :: Report -> IO Report
exact = pure

runs :: [Run]
runs =
  [ Run "Pure boxes" "" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule liftIdentity)))
  , Run "Pure boxes" "" "random"
        (seeded 0 (chshReport n randomScheduleIO liftIdentity))
  , Run "Local hidden variables" "plus2" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV plus2))))
  , Run "Local hidden variables" "minus2" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV minus2))))
  , Run "Local hidden variables" "uniformAll" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV uniformAll))))
  , Run "Local hidden variables" "uniformAll" "random"
        (seeded 1 (chshReport n randomScheduleIO (runTrialLHV uniformAll)))
  , Run "Local hidden variables" "biasPlus2Loophole 7 8" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV (biasPlus2Loophole 7 8)))))
  , Run "Local hidden variables" "biasPlus2Blocks 7 8" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV (biasPlus2Blocks 7 8)))))
  , Run "Clocked, clock visible" "" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule runTrialClockLoose)))
  , Run "Clocked, clock visible" "" "random"
        (seeded 2 (chshReport n randomScheduleIO runTrialClockLoose))
  , Run "Clocked, clock hidden" "" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule runTrialClockTight)))
  , Run "Clocked, clock hidden" "" "random"
        (seeded 3 (chshReport n randomScheduleIO runTrialClockTight))
  , Run "Superdeterminism" "cheatLam" "fixed"
        (exact (runIdentity (reportSuperdet n fixedSchedule cheatLam)))
  , Run "Superdeterminism" "cheatLam" "random"
        (seeded 4 (reportSuperdet n randomScheduleIO cheatLam))
  , Run "Superdeterminism" "legalAllPlus" "fixed"
        (exact (runIdentity (reportSuperdet n fixedSchedule legalAllPlus)))
  , Run "Superdeterminism" "cheatNS" "random"
        (seeded 10 (reportSuperdet n randomScheduleIO cheatNS))
  , Run "Shared state" "" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule runTrialState)))
  , Run "Quantum" "" "fixed"
        (seeded 5 (chshReport n fixedSchedule runTrialQuantum))
  , Run "Quantum" "" "random"
        (seeded 6 (chshReport n randomScheduleIO runTrialQuantum))
  , Run "Quantum, theta = pi/8" "" "fixed"
        (seeded 14 (chshReport n fixedSchedule (runTrialSource (entangled (pi / 8)))))
  , Run "PR box" "" "fixed"
        (seeded 7 (chshReport n fixedSchedule runTrialPR))
  , Run "PR box" "" "random"
        (seeded 8 (chshReport n randomScheduleIO runTrialPR))
  , Run "PR hidden state" "uniformAll" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialPRHiddenState uniformAll))))
  , Run "PR hidden state" "uniformAll" "random"
        (seeded 11 (chshReport n randomScheduleIO (runTrialPRHiddenState uniformAll)))
  , Run "PR hidden state" "plus2" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialPRHiddenState plus2))))
    -- the Bell table (CHSH 2.5) from a hidden state: exact when every
    -- context meets every strategy; the cycling sampler under the
    -- fixed schedule turns it into the PR box
  , Run "Bell table, hidden state" "uniformBlocks" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialBellHiddenState uniformBlocks))))
  , Run "Bell table, hidden state" "uniformAll" "random"
        (seeded 15 (chshReport n randomScheduleIO (runTrialBellHiddenState uniformAll)))
  , Run "Bell table, hidden state" "uniformAll" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialBellHiddenState uniformAll))))
    -- samplers of the legal type, and the step they share with the
    -- schedule: peek runs the schedule itself and hands the settings
    -- to cheatLam; fresh cannot read the step
  , Run "LHV, sampler peeks" "peek fixed cheatLam" "fixed"
        (exact (runIdentity (chshReport n fixedSchedule (runTrialLHV (peek fixedSchedule cheatLam)))))
  , Run "LHV, sampler peeks" "peek random cheatLam" "random"
        (seeded 4 (chshReport n randomScheduleIO (runTrialLHV (peek randomScheduleIO cheatLam))))
  , Run "Local hidden variables" "fresh uniformIO" "fixed"
        (seeded 13 (chshReport n fixedSchedule (runTrialLHV (fresh uniformIO))))
  ]

------------------------------------------------------------

verdict :: Report -> String
verdict r
  | noSignalingIn r = "✓"
  | otherwise       = "✗ " ++ intercalate ", " (signaling r)

title :: Run -> String
title r = intercalate ", " (filter (not . null) [model r, sampler r, schedule r ++ " schedule"])

main :: IO ()
main = do
  rows <- mapM (\r -> (,) r <$> runIt r) runs

  putStrLn ""
  putStrLn ("CHSH values and no-signaling, one run of " ++ show n ++ " trials each")
  putStrLn (replicate 86 '-')
  printf "%-24s %-22s %-9s %8s   %s\n" "Model" "Sampler" "Schedule" "CHSH" "No-signaling, this run"
  putStrLn (replicate 86 '-')
  forM_ rows $ \(r, rep) ->
    printf "%-24s %-22s %-9s %8.3f   %s\n"
      (model r) (sampler r) (schedule r) (chshValue rep) (verdict rep)

  putStrLn ""
  putStrLn "The tables of the runs that signal"
  putStrLn (replicate 86 '-')
  forM_ rows $ \(r, rep) ->
    unless (noSignalingIn rep) $ do
      putStrLn ""
      putStrLn (title r)
      putStr (renderTable (table rep))
      putStr (renderComparisons (table rep))
