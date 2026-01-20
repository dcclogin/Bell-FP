-- Future-dependent contextuality (Wharton / Silberstein)

module CHSH.Continuation where

import System.Random
import Control.Monad.State.Strict

import CHSH.DSL
import CHSH.Util  -- for randomDouble, flipOutcome, etc.

------------------------------------------------------------
-- CONTEXTUAL λ VIA CONTINUATIONS
--
-- A measurement of A yields:
--    x  :: Outcome
--    k  :: BSetting -> Continuation Outcome
--
-- Later, λB b = k b
------------------------------------------------------------
{--
newtype Continuation a =
  Continuation { unCont :: State StdGen a }
  deriving (Functor, Applicative, Monad, MonadState StdGen)

evalContinuation :: Continuation a -> StdGen -> (a, StdGen)
evalContinuation (Continuation m) = runState m


------------------------------------------------------------
-- CHSH angles and correlations (same as before)
------------------------------------------------------------

angleA :: ASetting -> Double
angleA A0 = 0
angleA A1 = pi/2

angleB :: BSetting -> Double
angleB B0 =  pi/4
angleB B1 = -pi/4

quantumCorr :: ASetting -> BSetting -> Double
quantumCorr a b =
  let θA = angleA a
      θB = angleB b
  in  - cos (θA - θB)

------------------------------------------------------------
-- λ MODEL
--
--  lambdaA a:
--     pick x uniformly from {±1}
--     return both x and a continuation k :: BSetting -> Continuation Outcome
--
--  lambdaB b:
--     instead of sampling directly, you *run the continuation*
--     that came from lambdaA.
------------------------------------------------------------

instance LambdaModel Continuation where

  lambdaA :: ASetting -> Continuation (Outcome, BSetting -> Continuation Outcome)
  lambdaA a = do
    u <- randomDouble
    let x = if u < 0.5 then Minus else Plus

    -- continuation that decides Bob’s behavior
    let k :: BSetting -> Continuation Outcome
        k b = do
          let e     = quantumCorr a b
              pSame = (1 + e) / 2
          u2 <- randomDouble
          pure (if u2 < pSame then x else flipOutcome x)

    pure (x, k)


  lambdaB :: BSetting
          -> (Outcome, BSetting -> Continuation Outcome)
          -> Continuation Outcome
  lambdaB b (x, k) =
    -- this is the *only* allowed coordination:
    -- use the continuation passed from A
    k b

------------------------------------------------------------

runContinuationCHSH :: Int -> StdGen -> Double
runContinuationCHSH n rng0 =
  let (val, _) = evalContinuation (chsh n) rng0
  in  val

--}