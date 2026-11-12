module CHSH.NoSignaling where

import CHSH.Util
import CHSH.Experiment

import Data.List (intercalate)
import Text.Printf (printf)

------------------------------------------------------------
-- No-signaling, read off the table of the very run that produced
-- the CHSH value.
--
-- Each condition compares one party's probability of Plus in two
-- contexts that differ only in the other party's setting:
--
--   Alice at a:  P(x = Plus | a, B0)  =  P(x = Plus | a, B1)
--   Bob   at b:  P(y = Plus | A0, b)  =  P(y = Plus | A1, b)
--
-- (Before, noSignalingReport estimated these frequencies in
-- separate experiments whose settings were fixed in advance, with
-- the step index swept from 0 to n-1 at each fixed pair. Those
-- experiments are not the one whose CHSH value is reported: a
-- model whose trials depend on the step index, or whose sampler
-- depends on the settings, behaves differently in them.)

data Comparison = Comparison
  { condition :: String       -- e.g. "Bob at B1"
  , first     :: (Int, Int)   -- (trials with Plus, trials) in one context
  , second    :: (Int, Int)   -- the same in the other context
  }

comparisons :: Empirical -> [Comparison]
comparisons m = [ alice a | a <- [A0, A1] ] ++ [ bob b | b <- [B0, B1] ]
  where
    alice a   = Comparison ("Alice at " ++ showA a) (plusA a B0) (plusA a B1)
    bob b     = Comparison ("Bob at " ++ showB b) (plusB A0 b) (plusB A1 b)
    plusA a b = (numTrials m a b (\(x, _) -> x == Plus), numTrials m a b anything)
    plusB a b = (numTrials m a b (\(_, y) -> y == Plus), numTrials m a b anything)
    anything  = const True

-- The table comes from a finite run, so equality is decided
-- statistically. A condition is violated when one of its two
-- contexts never occurred, or when the two frequencies are more
-- than four standard errors apart (a two-proportion z-test).
-- Frequencies that agree exactly are never flagged. Frequencies of
-- 1 and 0 are flagged as soon as the two contexts together have more
-- than 16 trials: the standard error is then 1/sqrt (n1 + n2). At
-- four standard errors, a large run of a model that does not signal
-- is flagged in fewer than one run in three thousand.

violated :: Comparison -> Bool
violated (Comparison _ (k1, n1) (k2, n2)) =
  n1 == 0 || n2 == 0 || abs (p1 - p2) > 4 * se
  where
    p1 = fromIntegral k1 / fromIntegral n1 :: Double
    p2 = fromIntegral k2 / fromIntegral n2
    p  = fromIntegral (k1 + k2) / fromIntegral (n1 + n2)
    se = sqrt (p * (1 - p) * (1 / fromIntegral n1 + 1 / fromIntegral n2))

------------------------------------------------------------
-- One run, one table, both verdicts.

data Report = Report
  { table     :: Empirical
  , chshValue :: Double
  , signaling :: [String]    -- the conditions that the run violates
  }

report :: Monad e => Int -> Exp e Record -> e Report
report n oneTrial = do
  m <- empirical <$> experiment n oneTrial
  pure (Report m (chshOf (correlator m))
                 [ condition c | c <- comparisons m, violated c ])

-- Takes the same arguments as chsh.
chshReport :: (Monad e, TrialModel t)
           => Int -> Scheduler e -> RunTrial e t -> e Report
chshReport n sched runTrial = report n (trial sched (blind runTrial))

noSignalingIn :: Report -> Bool
noSignalingIn = null . signaling

------------------------------------------------------------
-- Printing

showA :: ASetting -> String
showA A0 = "A0"
showA A1 = "A1"

showB :: BSetting -> String
showB B0 = "B0"
showB B1 = "B1"

-- The empirical model as frequencies, one row per context, with
-- both marginals and the number of trials at the end.
renderTable :: Empirical -> String
renderTable m = unlines $
    "a   b   |  ++     +-     -+     --    | P(x=+) P(y=+)  trials"
  : [ showA a ++ "  " ++ showB b ++ "  | "
      ++ concat [ cell (freq m a b (== xy)) | xy <- outcomePairs ]
      ++ "| " ++ cell (freq m a b (\(x, _) -> x == Plus))
      ++ cell (freq m a b (\(_, y) -> y == Plus))
      ++ show (numTrials m a b (const True))
    | (a, b) <- contexts ]
  where
    cell :: Double -> String
    cell v = printf "%-7.3f" v

-- The four no-signaling comparisons of the table, with the two
-- frequencies each compares.
renderComparisons :: Empirical -> String
renderComparisons m = unlines
  [ printf "%-12s %s  vs  %s   %s" (condition c)
      (prob (first c)) (prob (second c)) (verdict c)
  | c <- comparisons m ]
  where
    prob :: (Int, Int) -> String
    prob (_, 0) = "  n/a"
    prob (k, k') = printf "%.3f" (fromIntegral k / fromIntegral k' :: Double)
    verdict c = if violated c then "violated" else "ok"

renderReport :: Report -> String
renderReport r = concat
  [ renderTable (table r)
  , "CHSH = " ++ show (chshValue r) ++ "\n"
  , renderComparisons (table r)
  , if noSignalingIn r
      then "✓ No-signaling holds in this run\n"
      else "✗ No-signaling VIOLATED in this run: "
           ++ intercalate ", " (signaling r) ++ "\n" ]

printReport :: Report -> IO ()
printReport = putStr . renderReport

------------------------------------------------------------
