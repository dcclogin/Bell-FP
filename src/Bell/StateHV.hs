module Bell.StateHV where

import Config
import Bell.Config
import Bell.Hidden
import ProbabilityMonads
import Control.Monad.IO.Class
import Control.Monad.State.Lazy

--- | Attempt to realize Bell empirical model with effectful hidden-variables (state)


type SΛIO a = StateT Λ IO a
type SΛD a = StateT Λ Dist a
type SdΛD a = StateT (Dist Λ) Dist a

--- | separate distributions for each bit in λ
--- type M a = StateT (Dist Bool, Dist Bool, Dist Bool) Dist a
type MaybeΛ = (Maybe Bool, Maybe Bool, Maybe Bool)
type M a = StateT MaybeΛ Dist a


--- | re-implementation of LHV using SΛD ("read-only")
--- ==================================================

--- | p(oa | ia, λ)
lhvmParamA :: IA -> SΛD Bool
lhvmParamA ia = do
    λ <- get
    return $ (fst (f λ)) ia

--- | p(ob | ib, λ)
lhvmParamB :: IB -> SΛD Bool
lhvmParamB ib = do
    λ <- get
    return $ (snd (f λ)) ib

--- | p(oa, ob | ia, ib, λ) = p(oa | ia, λ) * p(ob | ib, λ)
lhvmParamAB :: (IA, IB) -> SΛD (Bool, Bool)
lhvmParamAB (ia, ib) = eval (lhvmParamA, lhvmParamB) (ia, ib)

--- | from distribution over Λ to the conditional empirical model
getEmpirical :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpirical dΛ (ia, ib) = do
    λ <- dΛ
    evalStateT (lhvmParamAB (ia, ib)) λ


--- tests
--- ===============================================

testUniformSame :: IO ()
testUniformSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ (ia, ib))

testUniformDiff :: IO ()
testUniformDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ (ia, ib))

testLowerBoundSame :: IO ()
testLowerBoundSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ_lb (ia, ib))

testLowerBoundDiff :: IO ()
testLowerBoundDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ_lb (ia, ib))

testUpperBoundSame :: IO ()
testUpperBoundSame = do
    (ia, ib) <- randomAB_same
    print (getEmpirical uniformΛ_ub (ia, ib))

testUpperBoundDiff :: IO ()
testUpperBoundDiff = do
    (ia, ib) <- randomAB_diff
    print (getEmpirical uniformΛ_ub (ia, ib))




--- | proto-qubit: hidden variable with stateful bits
--- ==================================================


--- | helpers
--- ==================================================

prodM :: (Monad m) => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
prodM f g (x, y) = (,) <$> f x <*> g y

coprod :: (a -> c) -> (b -> c) -> Either a b -> c
coprod f g (Left x)  = f x
coprod f g (Right y) = g y


--- | get one bit from λ
getBit :: Either IA IB -> M (Maybe Bool)
getBit (Left ia) = case ia of
    A1 -> gets (\(l1, _, _) -> l1)
    A2 -> gets (\(_, l2, _) -> l2)
    A3 -> gets (\(_, _, l3) -> l3)
getBit (Right ib) = case ib of
    B1 -> gets (\(l1, _, _) -> l1)
    B2 -> gets (\(_, l2, _) -> l2)
    B3 -> gets (\(_, _, l3) -> l3)


--- | forget one of three bits in λ = (l1, l2, l3)
forgetBit :: Either IA IB -> M ()
forgetBit (Left ia) = case ia of
    A1 -> modify (\(_, l2, l3) -> (Nothing, l2, l3))
    A2 -> modify (\(l1, _, l3) -> (l1, Nothing, l3))
    A3 -> modify (\(l1, l2, _) -> (l1, l2, Nothing))
forgetBit (Right ib) = case ib of
    B1 -> modify (\(_, l2, l3) -> (Nothing, l2, l3))
    B2 -> modify (\(l1, _, l3) -> (l1, Nothing, l3))
    B3 -> modify (\(l1, l2, _) -> (l1, l2, Nothing))

--- | conditional forget: forget the bit only if it satisfies the predicate
cforgetBit :: Either IA IB -> (Bool -> Bool) -> M ()
cforgetBit i pred = do
    mb <- getBit i
    case mb of
        Nothing -> return ()
        Just b  -> if pred b then forgetBit i else return ()

getBitF :: Either IA IB -> M (Maybe Bool)
getBitF i = do
    mb <- getBit i
    case mb of
        Nothing -> return Nothing
        Just b  ->
            case i of
                Left ia -> do
                    let others = filter (/= i) [Left A1, Left A2, Left A3]
                    mapM_ (\i -> cforgetBit i (== b)) others
                    return (Just b)
                Right ib -> do
                    let others = filter (/= i) [Right B1, Right B2, Right B3]
                    mapM_ (\i -> cforgetBit i (== b)) others
                    return (Just b)


smParamA :: IA -> M Bool
smParamA ia = do
    mb <- getBitF (Left ia)
    case mb of
        Nothing -> lift $ uniform [tt, ff]
        Just b  -> return b

smParamB :: IB -> M Bool
smParamB ib = do
    mb <- getBitF (Right ib)
    case mb of
        Nothing -> lift $ uniform [tt, ff]
        Just b  -> return b

smParamAB :: (IA, IB) -> M (Bool, Bool)
smParamAB (ia, ib) = eval (smParamA, smParamB) (ia, ib)

getEmpiricalSM :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpiricalSM dΛ (ia, ib) = do
    Λ (b1, b2, b3) <- dΛ
    evalStateT (smParamAB (ia, ib)) (Just b1, Just b2, Just b3)


--- | tests for stateful model
--- ===============================================

testUniformSameSM :: IO ()
testUniformSameSM = do
    (ia, ib) <- randomAB_same
    print (getEmpiricalSM uniformΛ (ia, ib))

testUniformDiffSM :: IO ()
testUniformDiffSM = do
    (ia, ib) <- randomAB_diff
    print (getEmpiricalSM uniformΛ (ia, ib))