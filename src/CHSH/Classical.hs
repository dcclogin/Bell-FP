-- Local realism

module CHSH.Classical where

import CHSH.DSL
import System.Random (StdGen, randomR)
import Control.Monad (ap)

------------------------------------------------------------
-- Classical hidden-variable monad
--
-- We use an explicit environment (for ρ(λ)) and
-- an explicit RNG threaded by hand.
--
--   Classical a  ~  ClassicalEnv -> StdGen -> (a, StdGen)
------------------------------------------------------------

newtype Classical a =
  Classical { runClassical :: ClassicalEnv -> StdGen -> (a, StdGen) }

instance Functor Classical where
  fmap f (Classical g) =
    Classical $ \env rng ->
      let (x, rng') = g env rng
      in  (f x, rng')

instance Applicative Classical where
  pure x = Classical $ \_env rng -> (x, rng)
  (<*>)  = ap

instance Monad Classical where
  Classical mx >>= k =
    Classical $ \env rng ->
      let (x, rng1) = mx env rng
          Classical my = k x
      in  my env rng1

------------------------------------------------------------
-- The Bell hidden-variable space:
--
--   λ = (A0, A1, B0, B1) ∈ {±1}^4
--
-- Each λ tells you what Alice (or Bob) would output if asked
-- for either setting. Only two entries are seen in a trial,
-- but λ stores all four.
------------------------------------------------------------

data Lambda = Lambda
  { lA0 :: Outcome
  , lA1 :: Outcome
  , lB0 :: Outcome
  , lB1 :: Outcome
  }
  deriving (Eq, Show)

-- Deterministic response functions for a fixed λ
fA :: Lambda -> ASetting -> Outcome
fA λ A0 = lA0 λ
fA λ A1 = lA1 λ

fB :: Lambda -> BSetting -> Outcome
fB λ B0 = lB0 λ
fB λ B1 = lB1 λ

------------------------------------------------------------
-- Enumerate the entire λ space: 16 possible hidden states
------------------------------------------------------------

allLambdas :: [Lambda]
allLambdas =
  [ Lambda a0 a1 b0 b1
  | a0 <- [Minus, Plus]
  , a1 <- [Minus, Plus]
  , b0 <- [Minus, Plus]
  , b1 <- [Minus, Plus]
  ]

------------------------------------------------------------
-- Probability distribution over λ
--
--   [(λ1, p1), (λ2, p2), ..., (λk, pk)]
--
-- with Σ pi = 1. We do NOT assume uniformity; locality
-- does not constrain ρ.
------------------------------------------------------------

type LambdaDist = [(Lambda, Double)]

data ClassicalEnv = ClassicalEnv
  { lambdaDist :: LambdaDist
  }

mkEnv :: LambdaDist -> ClassicalEnv
mkEnv dist = ClassicalEnv { lambdaDist = dist }

-- Some example distributions ------------------------------

uniformDist :: LambdaDist
uniformDist =
  let p = 1 / fromIntegral (length allLambdas)
  in  [ (λ, p) | λ <- allLambdas ]

-- A particular extremal point that saturates CHSH = 2
extremal :: Lambda
extremal = Lambda Plus Plus Plus Minus

diracDist :: Lambda -> LambdaDist
diracDist λ = [(λ, 1.0)]

distCHSH2 :: LambdaDist
distCHSH2 = diracDist extremal

------------------------------------------------------------
-- Random primitives for the Classical monad
------------------------------------------------------------

randomDouble :: Classical Double
randomDouble = Classical $ \_env rng ->
  let (u, rng') = randomR (0.0, 1.0 :: Double) rng
  in  (u, rng')

sampleLambda :: Classical Lambda
sampleLambda = Classical $ \env rng ->
  let dist = lambdaDist env
      (u, rng') = randomR (0.0, 1.0 :: Double) rng
      λ = pick u dist
  in  (λ, rng')
  where
    pick :: Double -> LambdaDist -> Lambda
    pick x ((λ,p):xs)
      | x <= p    = λ
      | otherwise = pick (x - p) xs
    pick _ [] = error "LambdaDist is empty or not normalized"

------------------------------------------------------------
-- LambdaModel instance: classical local realism
--
-- One *trial* samples a single λ and then uses fA,fB.
-- The crucial point:
--
--   - A's outcome depends only on (a, λ)
--   - B's outcome depends only on (b, λ)
--   - No dependence on the remote setting.
--
-- Bell then guarantees |CHSH| ≤ 2 for any ρ.
------------------------------------------------------------

instance LambdaModel Classical where

  -- We give a default local λ for each side,
  -- but the real CHSH semantics will use measureAB below.
  lambdaA :: ASetting -> Classical Outcome
  lambdaA a = do
    λ <- sampleLambda
    pure (fA λ a)

  lambdaB :: BSetting -> Classical Outcome
  lambdaB b = do
    λ <- sampleLambda
    pure (fB λ b)

  -- Override the default measureAB to ensure that Alice and Bob
  -- see the SAME λ within a trial, as in the usual Bell picture.
  measureAB :: ASetting -> BSetting -> Classical (Outcome, Outcome)
  measureAB a b = do
    λ <- sampleLambda      -- one λ per trial
    pure (fA λ a, fB λ b)

------------------------------------------------------------
-- Top-level evaluator and CHSH runner
------------------------------------------------------------

evalClassical :: ClassicalEnv -> StdGen -> Classical a -> (a, StdGen)
evalClassical env rng (Classical m) =
  m env rng

-- Run the generic CHSH protocol in the Classical model.
-- For ANY choice of dist, Bell ⇒ |result| ≤ 2 (up to MC noise).
runClassicalCHSH :: Int -> LambdaDist -> StdGen -> Double
runClassicalCHSH n dist rng0 =
  let env       = mkEnv dist
      (val, _)  = evalClassical env rng0 (chsh n)
  in  val

