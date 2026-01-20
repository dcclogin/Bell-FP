module CHSH.Examples where

import System.Random (mkStdGen)
import CHSH.DSL
import CHSH.Classical
import CHSH.Contextual
import CHSH.ManyWorlds
import CHSH.Quantum

-- Uniform classical hidden variables
e1 = runClassicalCHSH 10000 uniformDist (mkStdGen 1)
-- ≈ something close to 0

-- Extremal deterministic λ (a standard CHSH=2 assignment)
e2 = runClassicalCHSH 10000 distCHSH2 (mkStdGen 1)
-- ≈ 2.0 (up to sampling noise)
{--
-- Quantum, Hilbert-space model
e3 = runQuantumCHSH 10000 (mkStdGen 1)
-- ≈ 2.82...

-- Contextual, pure effectful λ
e4 = runContextualCHSH 10000 (mkStdGen 1)
-- ≈ 2.82...

-- Many worlds
e5 = do
  let rng = mkStdGen 2025
      v   = runManyWorldProtocol 5000 rng
  putStrLn ("Many-worlds CHSH ≈ " ++ show v)

--}

{--
module Examples where

import System.Random (newStdGen, mkStdGen)
import Classical
import Quantum

-- For demonstration only

exampleClassical :: LambdaDist -> IO ()
exampleClassical dist = do
  rng <- newStdGen
  let v = runClassicalProtocol 5000 dist rng
  putStrLn ("Classical CHSH ≈ " ++ show v)

eU, eB, e2 :: IO ()
eU = exampleClassical uniformDist
eB = exampleClassical biasedDist
e2 = exampleClassical dist2

eQ :: IO ()
eQ = do
  let rng = mkStdGen 42
      v   = runQuantumProtocol 5000 rng
  putStrLn ("Quantum CHSH ≈ " ++ show v)

--}

