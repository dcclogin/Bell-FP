module Spekken.Qubit (
    Qubit, M,
    qubit,
    xGateEp,
    yGateEp,
    zGateEp,
    hGateEp,
) where

import Spekken.Ontic
import Spekken.EpState
import Spekken.Utils

import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import System.Random (randomIO)


-- ontic state as hidden variable
-- epistemic as hidden variable too ???
-- epistemic states in Spekkens' model is implicit...

type M = StateT Ontic IO
type Qubit = EpQuery -> M Bool



--- | 50% chance swap_, 50% chance id
--- | general: finite state machine
--- | ontic disturbance upon measurement
--- | randomly pick an ontic state and generate swap permutation

updateOntic :: EpQuery -> M ()
updateOntic (EpState s) = do
    oPrev <- get
    oNext <- liftIO $ randomFromSet s
    let perm = swapPermO oPrev oNext
    modify (runPermO perm)


--- | check whether current ontic state is "covered" by epistemic state
cover :: EpState -> Ontic -> Bool
cover (EpState s) o = o `S.member` s


--- | explicit definition of a single qubit
qubit :: Qubit
qubit e = do
    o <- get
    if cover e o
        then do
            updateOntic e
            return True
        else do
            updateOntic (notEp e)
            return False


-- evolution of measurements ~ epistemic states
-- evolution of states ~ ontic states

-- pauli gates
xGateEp, yGateEp, zGateEp :: Qubit -> Qubit
xGateEp qu = qu . (runPermE xPermEp)
yGateEp qu = qu . (runPermE yPermEp)
zGateEp qu = qu . (runPermE zPermEp)

-- hadamard gate
hGateEp :: Qubit -> Qubit
hGateEp qu = qu . (runPermE hPermEp)



-- generally, any permutation of epistemic states induces a transformation
-- not all permutations of ontic states are unitary transformations

example_singleMeasurementEp :: M ()
example_singleMeasurementEp = do
    o1 <- get
    b <- qubit zT
    o2 <- get

    liftIO $ putStrLn "======== Single measurement (EpState) ========"
    liftIO $ putStrLn $ "Ontic state before: " ++ show o1
    liftIO $ putStrLn $ "Ontic state after: " ++ show o2
    liftIO $ putStrLn $ "Measurement (zT): " ++ show b
    liftIO $ putStrLn ""


-- reproducible measurement outcomes
example_idempotentEp :: M ()
example_idempotentEp = do
    o1 <- get
    b1 <- qubit zT
    b2 <- qubit zT
    b3 <- qubit zT
    o2 <- get

    liftIO $ putStrLn "======== Idempotent measurements (EpState) ========"
    liftIO $ putStrLn $ "Ontic state before: " ++ show o1
    liftIO $ putStrLn $ "Ontic state after: " ++ show o2
    liftIO $ putStrLn $ "1st measurement (zT): " ++ show b1
    liftIO $ putStrLn $ "2nd measurement (zT): " ++ show b2
    liftIO $ putStrLn $ "3rd measurement (zT): " ++ show b3
    liftIO $ putStrLn ""


-- noncommutativity example
-- |0⟩⟨0| and |+⟩⟨+|
example_noncommutativityEp :: M ()
example_noncommutativityEp = do
    o <- get
    b1 <- qubit zT  -- Z
    b2 <- qubit xT  -- X  

    put o  -- reset ontic state
    b3 <- qubit xT  -- X
    b4 <- qubit zT  -- Z

    liftIO $ putStrLn "======== Noncommutative measurements (EpState) ========"
    liftIO $ putStrLn $ "Ontic state before: " ++ show o
    liftIO $ putStrLn $ "X, Z: " ++ show b3 ++ ", " ++ show b4
    liftIO $ putStrLn $ "Z, X: " ++ show b1 ++ ", " ++ show b2
    liftIO $ putStrLn ""



-- (Ontic, Ontic)
-- Either Ontic Ontic
-- (Applicative f, Monad m) => f (Epistemic -> m Bool)


main :: IO ()
main = do
    o <- randomOntic
    runStateT example_singleMeasurementEp o
    runStateT example_idempotentEp o
    runStateT example_noncommutativityEp o
    return ()