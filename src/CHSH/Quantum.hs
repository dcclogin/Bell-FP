-- Hilbert space / Copenhagen

module CHSH.Quantum where

import Prelude hiding ((<>))
import Control.Monad.State.Strict
import System.Random (StdGen)

import Numeric.LinearAlgebra            ( (><), (#>), (<.>), kronecker, scale, realPart )
import Numeric.LinearAlgebra.Data       ( Vector, Matrix, Complex(..), fromList )

import CHSH.DSL
import CHSH.Util    -- for randomDouble :: MonadState StdGen m => m Double

------------------------------------------------------------
-- TYPES
------------------------------------------------------------

type Vec = Vector (Complex Double)
type Mat = Matrix (Complex Double)

-- Embed a real as a complex with zero imaginary part
re :: Double -> Complex Double
re x = x :+ 0

-- Build a vector from reals
vec :: [Double] -> Vec
vec xs = fromList (map re xs)

-- Scale a matrix by a real factor
rscale :: Double -> Mat -> Mat
rscale c m = scale (re c) m

------------------------------------------------------------
-- 2×2 IDENTITY AND PAULI MATRICES
------------------------------------------------------------

i2, sx, sz :: Mat
i2 = (2><2)
  [ 1, 0
  , 0, 1 ]

sx = (2><2)
  [ 0, 1
  , 1, 0 ]

sz = (2><2)
  [  1,  0
  ,  0, -1 ]

-- Tensor product: 2×2 ⊗ 2×2 → 4×4
tensor2 :: Mat -> Mat -> Mat
tensor2 = kronecker

------------------------------------------------------------
-- LOCAL OBSERVABLES AND PROJECTORS
--
-- Observable along angle θ in the x–z plane:
--   O(θ) = cos θ · σ_z + sin θ · σ_x
--
-- Projectors for outcomes ±1:
--   P⁺(θ) = (I + O(θ)) / 2
--   P⁻(θ) = (I - O(θ)) / 2
------------------------------------------------------------

observable :: Double -> Mat
observable θ = rscale (cos θ) sz + rscale (sin θ) sx

projPlus :: Double -> Mat
projPlus θ  = rscale 0.5 (i2 + observable θ)

projMinus :: Double -> Mat
projMinus θ = rscale 0.5 (i2 - observable θ)

------------------------------------------------------------
-- SINGLET STATE |ψ⁻⟩ = (|01⟩ − |10⟩)/√2
-- Basis order: |00>, |01>, |10>, |11>.
------------------------------------------------------------

singlet :: Vec
singlet = vec
  [ 0
  ,  1 / sqrt 2
  , -1 / sqrt 2
  , 0
  ]

------------------------------------------------------------
-- BORN WEIGHT  ⟨ψ | P | ψ⟩
------------------------------------------------------------

bornWeight :: Mat -> Vec -> Double
bornWeight p v =
  let w = p #> v
  in  realPart (v <.> w)

------------------------------------------------------------
-- MEASUREMENT DIRECTIONS FOR CHSH
--
-- Standard choice:
--   A0 = σ_z        (θ = 0)
--   A1 = σ_x        (θ = π/2)
--   B0 = σ at π/4
--   B1 = σ at -π/4
------------------------------------------------------------

angleA :: ASetting -> Double
angleA A0 = 0
angleA A1 = pi / 2

angleB :: BSetting -> Double
angleB B0 =  pi / 4
angleB B1 = -pi / 4

------------------------------------------------------------
-- JOINT PROJECTORS AND PROBABILITIES ON THE SINGLET
--
-- For a given pair of settings (a,b), define:
--
--   P++ = P_A^+(a) ⊗ P_B^+(b)
--   P+- = P_A^+(a) ⊗ P_B^-(b)
--   P-+ = P_A^-(a) ⊗ P_B^+(b)
--   P-- = P_A^-(a) ⊗ P_B^-(b)
--
-- and their Born weights:
--
--   w++ = ⟨ψ|P++|ψ⟩, etc.
--
-- We normalise to get a probability distribution:
--
--   p++ = w++ / (w++ + w+- + w-+ + w--)
--   ...
------------------------------------------------------------

jointProbs :: ASetting -> BSetting -> (Double, Double, Double, Double)
jointProbs a b =
  let θA = angleA a
      θB = angleB b

      paP = projPlus  θA
      paM = projMinus θA
      pbP = projPlus  θB
      pbM = projMinus θB

      pPP = tensor2 paP pbP
      pPM = tensor2 paP pbM
      pMP = tensor2 paM pbP
      pMM = tensor2 paM pbM

      wPP = bornWeight pPP singlet
      wPM = bornWeight pPM singlet
      wMP = bornWeight pMP singlet
      wMM = bornWeight pMM singlet

      total = wPP + wPM + wMP + wMM
  in  ( wPP / total
      , wPM / total
      , wMP / total
      , wMM / total )

------------------------------------------------------------
-- QuantumRun monad = "one quantum experiment run"
--
-- This is just State StdGen, but we give it a distinct
-- name to emphasise the semantic role.
------------------------------------------------------------

newtype QuantumRun a =
  QuantumRun { unQuantumRun :: State StdGen a }
  deriving (Functor, Applicative, Monad, MonadState StdGen)

evalQuantumRun :: QuantumRun a -> StdGen -> (a, StdGen)
evalQuantumRun (QuantumRun m) = runState m

------------------------------------------------------------
-- LambdaModel instance: genuine quantum semantics
--
-- Here the "monadic λ" is *intrinsically nonlocal*:
-- we override measureAB to do a joint Born sampling
-- of (A,B) from the singlet, for the chosen settings.
------------------------------------------------------------

instance LambdaModel QuantumRun where

  -- We don't actually use lambdaA / lambdaB for the
  -- quantum model; the physics is in the joint measurement.
  -- You could implement them as marginals if you wish.
  lambdaA :: ASetting -> QuantumRun Outcome
  lambdaA _ = error "lambdaA not used directly in QuantumRun"

  lambdaB :: BSetting -> QuantumRun Outcome
  lambdaB _ = error "lambdaB not used directly in QuantumRun"

  measureAB :: ASetting -> BSetting -> QuantumRun (Outcome, Outcome)
  measureAB a b = QuantumRun $ do
    let (pPP, pPM, pMP, pMM) = jointProbs a b

        -- cumulative thresholds
        t1 = pPP
        t2 = t1 + pPM
        t3 = t2 + pMP

    u <- randomDouble      -- from Util, in [0,1)
    pure $
      if      u < t1 then (Plus,  Plus )
      else if u < t2 then (Plus,  Minus)
      else if u < t3 then (Minus, Plus )
      else                (Minus, Minus)

------------------------------------------------------------
-- Quantum CHSH protocol runner
--
-- Uses the generic 'chsh' from DSL, but instantiates
-- it with the QuantumRun semantics above.
------------------------------------------------------------

runQuantumCHSH :: Int -> StdGen -> Double
runQuantumCHSH n rng0 =
  let (val, _) = evalQuantumRun (chsh n) rng0
  in  val
