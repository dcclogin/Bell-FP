module CHSH.Quantum where

import CHSH.Util
import CHSH.Experiment
import CHSH.NoSignaling

import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify)
import System.Random (randomRIO)

------------------------------------------------------------
-- Quantum trial model, as a quantum computing textbook writes it: the
-- state of two qubits is a vector of four amplitudes, one for each of
-- the basis states |00>, |01>, |10> and |11>; a measurement is a pair
-- of projectors; and the Born rule gives the probability of each
-- outcome and the state it leaves. Alice holds the first qubit, Bob the
-- second. Real amplitudes suffice for the states and measurements here.
-- (Before: pairs of polarization-entangled photons, with polarizers at
-- chosen angles, Malus's law and the collapse of the pair.)

type Vector = [Double]
type Matrix = [[Double]]

-- Dirac's brackets, as operators: u <| v is the inner product <u|v>,
-- m |> v the matrix m applied to the ket |v>, and m >< n the tensor
-- (Kronecker) product of two matrices, which the paper prints as a
-- circled times. Since |> binds tighter than <|, psi <| p |> psi is
-- <psi|P|psi>, and >< binds tighter still.
infixr 6 <|
infixr 7 |>
infixl 8 ><

(<|) :: Vector -> Vector -> Double
u <| v = sum (zipWith (*) u v)

(|>) :: Matrix -> Vector -> Vector
m |> v = map (<| v) m

(><) :: Matrix -> Matrix -> Matrix
m >< n = [ concat [ map (a *) row | a <- mrow ] | mrow <- m, row <- n ]

add, sub :: Matrix -> Matrix -> Matrix
add = zipWith (zipWith (+))
sub = zipWith (zipWith (-))

scale :: Double -> Matrix -> Matrix
scale c = map (map (c *))

normalize :: Vector -> Vector
normalize v = map (/ sqrt (v <| v)) v

------------------------------------------------------------
-- Measurements. A setting measures an observable whose eigenvalues
-- are +1 (Plus) and -1 (Minus). Alice measures Z or X, Bob
-- (Z + X)/sqrt 2 or (Z - X)/sqrt 2: the observables that maximize the
-- CHSH value of the state |00> + |11> below.

identity, pauliZ, pauliX :: Matrix
identity = [[1, 0], [0, 1]]
pauliZ   = [[1, 0], [0, -1]]
pauliX   = [[0, 1], [1, 0]]

observableA :: ASetting -> Matrix
observableA A0 = pauliZ
observableA A1 = pauliX

observableB :: BSetting -> Matrix
observableB B0 = scale (1 / sqrt 2) (add pauliZ pauliX)
observableB B1 = scale (1 / sqrt 2) (sub pauliZ pauliX)

-- The projector onto the eigenvalue of an outcome: (I + O)/2 for Plus,
-- (I - O)/2 for Minus.
projector :: Matrix -> Outcome -> Matrix
projector o Plus  = scale (1 / 2) (add identity o)
projector o Minus = scale (1 / 2) (sub identity o)

-- The same projectors on the pair: Alice's act on the first qubit,
-- P (x) I, and Bob's on the second, I (x) P.
measA :: ASetting -> Outcome -> Matrix
measA a x = projector (observableA a) x >< identity

measB :: BSetting -> Outcome -> Matrix
measB b y = identity >< projector (observableB b) y

-- The Born rule: the outcome whose projector is P has probability
-- <psi|P|psi>, and collapses the state to P|psi>, normalized.
probability :: Matrix -> Vector -> Double
probability p psi = psi <| p |> psi

collapse :: Matrix -> Vector -> Vector
collapse p psi = normalize (p |> psi)

-- Plus with probability p
coin :: Double -> IO Outcome
coin p = do
  u <- randomRIO (0, 1)
  pure (if u < p then Plus else Minus)

------------------------------------------------------------
-- A source of pairs prepares a state of the two qubits.

type Source = Vector

-- |00> + |11>, normalized: the maximally entangled state of two qubits
maximallyEntangled :: Source
maximallyEntangled = normalize [1, 0, 0, 1]

-- cos theta |00> + sin theta |11>: maximallyEntangled at theta = pi/4,
-- the product state |00> at theta = 0
entangled :: Double -> Source
entangled theta = [cos theta, 0, 0, sin theta]

-- The correlation of a context, <psi| A (x) B |psi>: the average of the
-- product of the two outcomes, which the CHSH value combines.
correlation :: Source -> ASetting -> BSetting -> Double
correlation psi a b = psi <| observableA a >< observableB b |> psi

------------------------------------------------------------
-- The trial monad holds the state of the pair, which a measurement
-- collapses, and flips coins in IO.

newtype Quantum a = Quantum { unQuantum :: StateT Vector IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState Vector)

instance TrialModel Quantum where
  -- each party measures its own qubit of the pair, and leaves the state
  -- as it is
  localA a = do
    psi <- get
    liftIO (coin (probability (measA a Plus) psi))
  localB b = do
    psi <- get
    liftIO (coin (probability (measB b Plus) psi))

  -- the two boxes in turn, each collapsing the state that the other reads
  jointAB a b = liftA2 (,) (nonlocalA a) (nonlocalB b)

-- Dual to each other: a box answers as on its own, and then collapses
-- the state to what its outcome leaves. (In jointAB nothing reads the
-- state after nonlocalB. The two measurements commute, so either box
-- could go first, with the same joint distribution.)
nonlocalA :: ASetting -> Quantum Outcome
nonlocalA a = do
  x <- localA a
  modify (collapse (measA a x))
  pure x

nonlocalB :: BSetting -> Quantum Outcome
nonlocalB b = do
  y <- localB b
  modify (collapse (measB b y))
  pure y

-- Membrane: embed a Quantum trial into the experiment monad Exp IO. A
-- trial starts from a pair in the given state, and the state it ends in
-- ends with it. The experiment has a step index, but this model does
-- not use it.
runTrialSource :: Source -> RunTrial IO Quantum
runTrialSource psi (Quantum m) = ReaderT (\_ -> evalStateT m psi)

runTrialQuantum :: RunTrial IO Quantum
runTrialQuantum = runTrialSource maximallyEntangled

------------------------------------------------------------
-- CHSH tests

-- Expect ~2.8284 (2*sqrt 2)
testQuantum_fixed :: IO Double
testQuantum_fixed = chsh 200000 fixedSchedule runTrialQuantum

-- Expect ~2.8284 as well (schedule randomness should not matter here)
testQuantum_random :: IO Double
testQuantum_random = chsh 200000 randomScheduleIO runTrialQuantum

------------------------------------------------------------
-- Reports: the CHSH value and the no-signaling verdict of the same
-- run; the table also shows every marginal.

reportQuantum_fixed :: IO Report
reportQuantum_fixed = seeded 5 (chshReport 80000 fixedSchedule runTrialQuantum)
-- CHSH ~2.83, no-signaling, every marginal ~1/2

reportQuantum_random :: IO Report
reportQuantum_random = seeded 6 (chshReport 80000 randomScheduleIO runTrialQuantum)
-- CHSH ~2.83, no-signaling

-- A partially entangled state, theta = pi/8. Each qubit on its own is
-- now more likely 0 than 1, so its marginal depends on the observable
-- measured; it still does not depend on the other party's.
reportQuantum_partial :: IO Report
reportQuantum_partial = seeded 14 (chshReport 80000 fixedSchedule (runTrialSource (entangled (pi / 8))))
-- CHSH ~1 + sqrt 2 ~ 2.41, no-signaling; P(x=+) ~0.854 at A0 and ~0.5
-- at A1, P(y=+) ~0.75 at both of Bob's settings
