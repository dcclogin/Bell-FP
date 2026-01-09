module EPR.StateHV where

import EPR.Config
import EPR.Hidden
import ProbabilityMonads
import Control.Monad.IO.Class
import Control.Monad.State.Lazy


type SΛIO a = StateT Λ IO a
type SΛD a = StateT Λ Dist a
type SdΛD a = StateT (Dist Λ) Dist a


lhvmParamA :: IA -> SΛD Bool
lhvmParamA A = do
    λ <- get
    return $ (fst (f λ)) A

lhvmParamB :: IB -> SΛD Bool
lhvmParamB B = do
    λ <- get
    return $ (snd (f λ)) B

lhvmParamAB :: (IA, IB) -> SΛD (Bool, Bool)
lhvmParamAB (A, B) = (,) <$> lhvmParamA A <*> lhvmParamB B


getEmpirical :: Dist Λ -> (IA, IB) -> Dist (Bool, Bool)
getEmpirical dΛ (A, B) = do
    λ <- dΛ
    evalStateT (lhvmParamAB (A, B)) λ


--- | tests
--- ================================================

testUniform :: IO ()
testUniform = do
    print $ jointFromCond qAB (getEmpirical uniformΛ)