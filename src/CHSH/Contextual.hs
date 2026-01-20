-- Retrocausal / superdeterministic

module CHSH.Contextual where

import CHSH.DSL
import System.Random (StdGen, randomR)
import Control.Monad.State.Strict

------------------------------------------------------------
-- Store-based contextual model
--
-- The idea:
--   • lambdaA chooses Alice's outcome x (locally, symmetric).
--   • lambdaB *reads* x from a global store, and chooses y
--     with a probability that depends on (a,b) via the
--     quantum correlation E(a,b) = -cos(θ_A - θ_B).
--
-- No joint (x,y) sampling is ever done explicitly.
-- Coordination is entirely via the store.
------------------------------------------------------------

------------------------------------------------------------
-- Store: we only need to remember Alice's last outcome
-- and setting (for the current trial).
------------------------------------------------------------

data Store = Store
  { storedA :: Maybe (ASetting, Outcome)
  }

emptyStore :: Store
emptyStore = Store { storedA = Nothing }

------------------------------------------------------------
-- Contextual monad: global RNG + global store
------------------------------------------------------------

newtype Contextual a =
  Contextual { unContextual :: State (StdGen, Store) a }
  deriving (Functor, Applicative, Monad, MonadState (StdGen, Store))

evalContextual :: Contextual a -> StdGen -> (a, StdGen)
evalContextual (Contextual m) rng0 =
  let (x, (rng', _st)) = runState m (rng0, emptyStore)
  in  (x, rng')

------------------------------------------------------------
-- Local helpers
------------------------------------------------------------

-- Flip ±1
flipOutcome :: Outcome -> Outcome
flipOutcome Plus  = Minus
flipOutcome Minus = Plus

-- Draw a Double in [0,1)
randomDoubleC :: Contextual Double
randomDoubleC = Contextual $ state $ \(g, st) ->
  let (u, g') = randomR (0.0, 1.0 :: Double) g
  in  (u, (g', st))

-- Standard CHSH angles (same as Quantum.hs, but without Hilbert space)
angleA :: ASetting -> Double
angleA A0 = 0
angleA A1 = pi / 2

angleB :: BSetting -> Double
angleB B0 =  pi / 4
angleB B1 = -pi / 4

-- Quantum correlation for the singlet:
--   E(a,b) = -cos(θ_A - θ_B)
quantumCorr :: ASetting -> BSetting -> Double
quantumCorr a b =
  let θA = angleA a
      θB = angleB b
  in  - cos (θA - θB)

------------------------------------------------------------
-- LambdaModel instance: contextual hidden variables
--
-- Semantics of one trial (A then B):
--
--   lambdaA a:
--     • if A was already measured at this setting in this trial,
--       reuse that outcome;
--     • otherwise, choose x ∈ {±1} uniformly, store (a,x).
--
--   lambdaB b:
--     • read (a,x) from the store (must have been set by lambdaA);
--     • compute E = quantumCorr a b;
--     • let pSame = (1 + E) / 2;
--     • choose y = x with probability pSame, else y = -x;
--     • clear the store for the next trial.
--
-- This yields the *same* joint distribution as the singlet
-- for the CHSH settings, but implemented purely as:
--   local A + contextual B via global store.
------------------------------------------------------------

instance LambdaModel Contextual where

  lambdaA :: ASetting -> Contextual Outcome
  lambdaA a = do
    (g, st) <- get
    case storedA st of
      -- If we already have a value for this setting in this trial,
      -- reuse it (idempotence).
      Just (a', x) | a' == a ->
        pure x

      -- Otherwise, pick a fresh symmetric ±1 and store it.
      _ -> do
        let (u, g') = randomR (0.0, 1.0 :: Double) g
            x       = if u < 0.5 then Minus else Plus
            st'     = st { storedA = Just (a, x) }
        put (g', st')
        pure x

  lambdaB :: BSetting -> Contextual Outcome
  lambdaB b = do
    (g, st) <- get
    case storedA st of

      -- If somehow B is called with no prior A in this trial,
      -- fall back to an unbiased local outcome.
      Nothing -> do
        let (u, g') = randomR (0.0, 1.0 :: Double) g
            y       = if u < 0.5 then Minus else Plus
        put (g', st)
        pure y

      Just (a, x) -> do
        -- Contextual dependence: B conditions on A's outcome x
        -- via the quantum correlation for (a,b).
        let e      = quantumCorr a b
            pSame  = (1 + e) / 2   -- P(y = x | a,b)
            (u, g') = randomR (0.0, 1.0 :: Double) g
            y       = if u < pSame then x else flipOutcome x
            st'     = st { storedA = Nothing }  -- reset for next trial
        put (g', st')
        pure y

------------------------------------------------------------
-- CHSH runner using the generic 'chsh' from DSL
------------------------------------------------------------

runContextualCHSH :: Int -> StdGen -> Double
runContextualCHSH n rng0 =
  let (val, _) = evalContextual (chsh n) rng0
  in  val