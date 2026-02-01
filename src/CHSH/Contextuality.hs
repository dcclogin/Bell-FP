module CHSH.Contextuality where

import CHSH.Util
import CHSH.Experiment

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import System.Random (randomRIO)

------------------------------------------------------------
-- Contextuality via Specker's triangle (3 observables, only measured in pairs)
--
-- We encode the three observables as:
--   C = "Correctness"
--   L = "Clarity"
--   S = "Significance"
--
-- Contexts (pairs we are allowed to jointly measure):
--   (C,L), (L,S), (C,S)
--
-- We embed these three contexts into the existing CHSH setting grid:
--   (A0,B0) = (C,L)
--   (A0,B1) = (L,S)
--   (A1,B0) = (C,S)
--   (A1,B1) is unused (we keep it harmless).
--
-- Contextuality claim:
--   In each context, outputs are perfectly anti-correlated (always differ),
--   with uniform marginals.
--   But there is no global assignment v(C), v(L), v(S) ∈ {±1} that
--   makes all three anti-correlations hold at once.

------------------------------------------------------------
-- Trial monad

newtype Contextual a = Contextual { unContextual :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTrialContextual :: RunTrial IO Contextual
runTrialContextual (Contextual ioa) = ReaderT (\_ -> ioa)

------------------------------------------------------------
-- Anti-correlated sampler with uniform marginals

antiCorr :: IO (Outcome, Outcome)
antiCorr = do
  x <- randomOutcome
  pure (x, flipOutcome x)

------------------------------------------------------------
-- Contextual model: jointAB depends on BOTH settings,
-- but stays memoryless and keeps marginals uniform.

instance TrialModel Contextual where
  -- Unused once jointAB is overridden; kept for completeness.
  localA _ = Contextual randomOutcome
  localB _ = Contextual randomOutcome

  jointAB a b = Contextual $ case (a,b) of
    -- (C,L)
    (A0,B0) -> antiCorr
    -- (L,S)
    (A0,B1) -> antiCorr
    -- (C,S)
    (A1,B0) -> antiCorr
    -- unused cell: keep it benign (uniform independent)
    (A1,B1) -> do
      x <- randomOutcome
      y <- randomOutcome
      pure (x,y)

------------------------------------------------------------
-- Executable contextuality witnesses

-- Probability that Alice's outcome is Plus in a given context (a,b).
pAPlus :: TrialModel t => Int -> RunTrial IO t -> ASetting -> BSetting -> IO Double
pAPlus n runTrial a b = do
  let go 0 acc = pure (acc / fromIntegral n)
      go k acc = do
        (x,_) <- runReaderT (runTrial (jointAB a b)) 0
        go (k-1) (acc + if x == Plus then 1 else 0)
  go n 0

-- Probability that Bob's outcome is Plus in a given context (a,b).
pBPlus :: TrialModel t => Int -> RunTrial IO t -> ASetting -> BSetting -> IO Double
pBPlus n runTrial a b = do
  let go 0 acc = pure (acc / fromIntegral n)
      go k acc = do
        (_,y) <- runReaderT (runTrial (jointAB a b)) 0
        go (k-1) (acc + if y == Plus then 1 else 0)
  go n 0

-- Probability that outcomes are equal in a given context (a,b).
pEq :: TrialModel t => Int -> RunTrial IO t -> ASetting -> BSetting -> IO Double
pEq n runTrial a b = do
  let go 0 acc = pure (acc / fromIntegral n)
      go k acc = do
        (x,y) <- runReaderT (runTrial (jointAB a b)) 0
        go (k-1) (acc + if x == y then 1 else 0)
  go n 0

-- Overlap consistency check:
-- the observable L appears as Bob's outcome in (C,L) and as Alice's outcome in (L,S).
-- Both should be uniform ~ 0.5 if marginals are consistent.
pPlusOverlapL :: TrialModel t => Int -> RunTrial IO t -> IO (Double, Double)
pPlusOverlapL n runTrial = do
  -- L measured as Bob in (C,L) = (A0,B0)
  pL_asBob  <- pBPlus n runTrial A0 B0
  -- L measured as Alice in (L,S) = (A0,B1) but "Alice" here stands for L
  pL_asAlice <- pAPlus n runTrial A0 B1
  pure (pL_asBob, pL_asAlice)

-- Deterministic global assignment search for Specker triangle.
-- Returns True iff there exists vC,vL,vS ∈ {±1} such that:
--   vC ≠ vL, vL ≠ vS, vC ≠ vS
-- (This is impossible for booleans.)
globalAssignmentExists :: Bool
globalAssignmentExists =
  or [ c /= l && l /= s && c /= s
     | c <- [False, True]
     , l <- [False, True]
     , s <- [False, True]
     ]

contextualityReport :: IO (Bool, String)
contextualityReport = do
  let n   = 20000
      eps = 0.02

  peCL <- pEq n runTrialContextual A0 B0
  peLS <- pEq n runTrialContextual A0 B1
  peCS <- pEq n runTrialContextual A1 B0

  (pLbob, pLalice) <- pPlusOverlapL n runTrialContextual

  let antiCorrOK = peCL <= eps && peLS <= eps && peCS <= eps
      overlapOK  = abs (pLbob - 0.5) <= 0.03 && abs (pLalice - 0.5) <= 0.03
      -- contextuality witness: empirical constraints look sharp, but no global assignment exists
      contextual = antiCorrOK && overlapOK && not globalAssignmentExists

      msg = unlines
        [ "Contextuality report (Specker triangle encoded in CHSH settings)"
        , "-------------------------------------------------------------"
        , "Contexts:"
        , "  (A0,B0) = (C,L)    require anti-correlation"
        , "  (A0,B1) = (L,S)    require anti-correlation"
        , "  (A1,B0) = (C,S)    require anti-correlation"
        , ""
        , "Empirical checks (should be near 0):"
        , "  P(equal | C,L) = " ++ show peCL
        , "  P(equal | L,S) = " ++ show peLS
        , "  P(equal | C,S) = " ++ show peCS
        , "eps = " ++ show eps ++ "   n = " ++ show n
        , ""
        , "Overlap sanity (L appears in two contexts, should be ~0.5 both ways):"
        , "  P(L=+ | measured as Bob in (C,L)) = " ++ show pLbob
        , "  P(L=+ | measured as Alice in (L,S)) = " ++ show pLalice
        , ""
        , "Global assignment exists? " ++ show globalAssignmentExists
        , "If the three anti-correlations hold sharply, and marginals are uniform,"
        , "yet no global assignment exists, this is a contextuality witness."
        ]

  pure (contextual, msg)

testContextuality :: IO ()
testContextuality = prettyReport =<< contextualityReport