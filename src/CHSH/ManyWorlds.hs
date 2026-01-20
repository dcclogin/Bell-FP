module CHSH.ManyWorlds where

import Control.Monad.State.Strict
import System.Random (StdGen)

import CHSH.DSL
import CHSH.Util

------------------------------------------------------------
-- ManyWorld monad
--
-- We reuse the same operational shape as your other
-- interpreters: a State StdGen monad. The intended
-- story, however, is “many worlds”: each draw from
-- the generator corresponds to picking a branch.
--
-- So this is:
--   • computationally: randomness in State StdGen
--   • philosophically: concurrency / branching worlds
------------------------------------------------------------

newtype ManyWorld a = ManyWorld { unManyWorld :: State StdGen a }
  deriving ( Functor, Applicative, Monad, MonadState StdGen )

------------------------------------------------------------
-- Angles for the CHSH experiment
--
-- These match your Quantum interpreter and the usual
-- Tsirelson-optimal configuration for the singlet:
--
--   A0 = 0
--   A1 = π/2
--   B0 =  π/4
--   B1 = −π/4
------------------------------------------------------------

angleA :: ASetting -> Double
angleA A0 = 0
angleA A1 = pi / 2

angleB :: BSetting -> Double
angleB B0 =  pi / 4
angleB B1 = -pi / 4

-- Map Outcome to ±1 as Double
sVal :: Outcome -> Double
sVal Plus  =  1.0
sVal Minus = -1.0

------------------------------------------------------------
-- Singlet joint probabilities WITHOUT Hilbert space
--
-- For the spin-singlet state and measurement directions
-- with angle difference Δθ = θA − θB, the joint
-- distribution is:
--
--   P(x,y) = 1/4 * (1 − x*y * cos Δθ)
--
-- where x,y ∈ {+1,−1}. Here Plus ↦ +1, Minus ↦ −1.
--
-- This gives:
--   • uniform marginals P(x=±1) = P(y=±1) = 1/2
--   • correlation E[xy] = −cos(Δθ)
-- and yields the Tsirelson-optimal CHSH value for
-- the chosen angles.
------------------------------------------------------------

jointProbs :: ASetting -> BSetting -> (Double, Double, Double, Double)
jointProbs a b =
  let dθ = angleA a - angleB b
      c  = cos dθ

      p :: (Outcome, Outcome) -> Double
      p (x,y) = 0.25 * (1 - sVal x * sVal y * c)

      pPP = p (Plus,  Plus )
      pPM = p (Plus,  Minus)
      pMP = p (Minus, Plus )
      pMM = p (Minus, Minus)

      total = pPP + pPM + pMP + pMM  -- should be 1, but renormalise just in case
  in  ( pPP / total
      , pPM / total
      , pMP / total
      , pMM / total )

------------------------------------------------------------
-- Interpreter instance
--
-- One CHSH trial:
--   • look at the settings (a,b)
--   • compute the four joint probabilities
--   • sample u ∈ [0,1) from the shared StdGen
--   • choose (x,y) by walking the CDF
--
-- In the many-worlds story:
--   • the probabilities describe the relative “thickness”
--     of branches
--   • u chooses which branch *this* world inhabits
--   • there is no collapse, just selection.
------------------------------------------------------------
{--

instance Interpreter ManyWorld where
  interpretOneShot (MeasureAB a b) = do
    let (pPP, pPM, pMP, pMM) = jointProbs a b
        -- cumulative thresholds
        t1 = pPP
        t2 = t1 + pPM
        t3 = t2 + pMP
    u <- randomDouble
    pure $
      if      u < t1 then (Plus,  Plus )
      else if u < t2 then (Plus,  Minus)
      else if u < t3 then (Minus, Plus )
      else                (Minus, Minus)

------------------------------------------------------------
-- Runner
--
-- This is exactly parallel to Classical / Quantum:
-- we reuse the generic chshWith meta-protocol that
-- you already wrote, plugging ManyWorld semantics.
------------------------------------------------------------

evalManyWorld :: ManyWorld a -> StdGen -> (a, StdGen)
evalManyWorld (ManyWorld m) = runState m

runManyWorldProtocol :: Int -> StdGen -> Double
runManyWorldProtocol n rng =
  let (val, _) = evalManyWorld (chshWith interpretOneShot n) rng
  in  val

--}