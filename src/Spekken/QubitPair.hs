module Spekken.QubitPair where

import Spekken.Ontic
import Spekken.EpState
import Spekken.Utils

import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((***))
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import System.Random (randomIO)


type M2 = StateT (Ontic, Ontic) IO
type Qubit = EpQuery -> M2 Bool

-- allows one qubit to r/w the ontic state of the other qubit
-- to prepare (entangled) pair of qubits

{--
class (Monad m) => Bipartite m where
    qubit1 :: EpQuery -> m Bool
    qubit2 :: EpQuery -> m Bool
--}


updateOntic1, updateOntic2 :: EpQuery -> M2 ()
updateOntic1 (EpState s) = do
    (oPrev1, _) <- get
    oNext1 <- liftIO $ randomFromSet s
    let perm = swapPermO oPrev1 oNext1
    modify (runPermO perm *** id)
updateOntic2 (EpState s) = do
    (_, oPrev2) <- get
    oNext2 <- liftIO $ randomFromSet s
    let perm = swapPermO oPrev2 oNext2
    modify (id *** runPermO perm)


--- | check whether current ontic state is "covered" by epistemic state
cover :: EpState -> Ontic -> Bool
cover (EpState s) o = o `S.member` s


--- | explicit definition of two qubits
--- | no communication between two ontic states
qubit1, qubit2 :: Qubit
qubit1 e = do
    (o1, _) <- get
    if cover e o1
        then do
            updateOntic1 e
            return True
        else do
            updateOntic1 (notEp e)
            return False
qubit2 e = do
    (_, o2) <- get
    if cover e o2
        then do
            updateOntic2 e
            return True
        else do
            updateOntic2 (notEp e)
            return False



measurePair :: (Qubit, Qubit) -> (EpQuery, EpQuery) -> M2 (Bool, Bool)
measurePair (q1, q2) (e1, e2) = do
    b1 <- q1 e1
    b2 <- q2 e2
    return (b1, b2)


-- sequence :: [M2 Bool] -> M2 [Bool]
measureList :: [Qubit] -> [EpQuery] -> M2 [Bool]
measureList qs es = sequence [ q e | (q, e) <- zip qs es ]
-- sequence $ zipWith ($) qs es
-- mapM (\(q, e) -> q e) (zip qs es)
-- traverse (\(q, e) -> q e) (zip qs es)


example_uncorrelated :: M2 ()
example_uncorrelated = do
    (o1, o2) <- get
    [b1, b2] <- measureList [qubit1, qubit2] [zT, xT]

    liftIO $ putStrLn "======== QubitPair measurements ========"
    liftIO $ putStrLn $ "Ontic state before: " ++ show (o1, o2)
    liftIO $ putStrLn $ "Measurement results: " ++ show (b1, b2)
    liftIO $ putStrLn ""


-- |00⟩ + |11⟩
example_BP_explicit :: M2 ()
example_BP_explicit = do
    modify (\(o1, o2) -> (o1, o1))  -- explicitly enforce correlation
    b1 <- qubit1 zT
    b2 <- qubit2 zT

    liftIO $ putStrLn "======== BellPair measurements ========"
    liftIO $ putStrLn $ "Measurement results: " ++ show (b1, b2)
    liftIO $ putStrLn ""


-- commutativity example
-- context: [Z1, X2, Z1 ⊗ X2] (one column of the Mermin square)
example_commutativity :: M2 ()
example_commutativity = do
    (o1, o2) <- get
    b11 <- qubit1 zT  -- Z1
    b12 <- qubit2 xT  -- X2
    b13 <- measurePair (qubit1, qubit2) (zT, xT)  -- Z1 ⊗ X2

    put (o1, o2)  -- reset ontic state
    b21 <- measurePair (qubit1, qubit2) (zT, xT)  -- Z1 ⊗ X2
    b22 <- qubit2 xT  -- X2
    b23 <- qubit1 zT  -- Z1

    put (o1, o2)  -- reset ontic state
    b31 <- qubit2 xT  -- X2
    b32 <- qubit1 zT  -- Z1
    b33 <- measurePair (qubit1, qubit2) (zT, xT)  -- Z1 ⊗ X2

    liftIO $ putStrLn "======== Commutative measurements ========"
    liftIO $ putStrLn $ "Ontic state before: " ++ show (o1, o2)
    liftIO $ putStrLn $ "Z1, X2, Z1⊗X2: " ++ show (b11, b12, b13)
    liftIO $ putStrLn $ "Z1⊗X2, X2, Z1: " ++ show (b21, b22, b23)
    liftIO $ putStrLn $ "X2, Z1, Z1⊗X2: " ++ show (b31, b32, b33)
    liftIO $ putStrLn ""


-- implicit ontic theory: 
-- 1. modify definition of qubits (explicit preparation of entangled pair)
-- 2. modify update rules (see "Contextuality Extension of Spekkens' Model")
-- 3. modify definition of measurements


-- explicit epistemic theory
-- 1. add agents and agents memory (see "Toys Can't Play")
-- 2. forgetting information, reasoning about others' knowledge, etc.


main :: IO ()
main = do
    os <- randomOnticPair  -- no correlation
    runStateT example_uncorrelated os
    runStateT example_BP_explicit os
    runStateT example_commutativity os
    return ()