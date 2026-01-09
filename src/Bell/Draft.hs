module Bell.Draft where

import Bell.Config
import ProbabilityMonads
import Control.Monad.IO.Class
import Control.Monad.State.Lazy


--- | Hidden Variable Model for Bell-Mermin scenario
--- | more general : 6 boolean variables instead of 3
data Λ = Λ (Bool, Bool, Bool) deriving (Show, Eq, Ord)


--- | determination functions, pure
δ1, δ2, δ3 :: Λ -> Bool
δ1 (Λ (l1, _, _)) = l1
δ2 (Λ (_, l2, _)) = l2
δ3 (Λ (_, _, l3)) = l3

--- | vector of determination functions
δ :: Λ -> (Bool, Bool, Bool)
δ λ = (δ1 λ, δ2 λ, δ3 λ)

--- | more general <instruction sets> 
--- | (see Abramsky "relational hidden variables" 12.4)
f :: Λ -> (IA -> Bool, IB -> Bool)
f λ = (fa, fb)
    where
        fa A1 = δ1 λ
        fa A2 = δ2 λ
        fa A3 = δ3 λ
        fb B1 = δ1 λ  -- not (δ1 λ) if anti-correlated
        fb B2 = δ2 λ  -- not (δ2 λ) if anti-correlated
        fb B3 = δ3 λ  -- not (δ3 λ) if anti-correlated

--- | coproduct version of f
f2 :: Λ -> (Either IA IB -> Bool)
f2 λ (Left ia) = let (fa, _) = f λ in fa ia
f2 λ (Right ib) = let (_, fb) = f λ in fb ib


hvmA :: (IA, Λ) -> Dist Bool
hvmA (ia, λ) = pure $ (fst (f λ)) ia

hvmB :: (IB, Λ) -> Dist Bool
hvmB (ib, λ) = pure $ (snd (f λ)) ib

--- | factorized HVM
hvm :: ((IA, IB), Λ) -> Dist (Bool, Bool)
hvm ((ia, ib), λ) = (hvmA (ia, λ)) `prodDist` (hvmB (ib, λ))

--- | λ-independence
uniformΛ :: Dist Λ
uniformΛ = uniform [ Λ (l1, l2, l3) | l1 <- [tt, ff], l2 <- [tt, ff], l3 <- [tt, ff] ]

--- | lower bound: no (true, true, true) or (false, false, false)
uniformΛ_lb :: Dist Λ
uniformΛ_lb = Dist [ (Λ (l1, l2, l3), 1/6) 
    | l1 <- [tt, ff]
    , l2 <- [tt, ff]
    , l3 <- [tt, ff]
    , (l1, l2, l3) /= (tt, tt, tt)
    , (l1, l2, l3) /= (ff, ff, ff) ]

--- | upper bound: only (true, true, true) or (false, false, false)
uniformΛ_ub :: Dist Λ
uniformΛ_ub = uniform [ Λ (tt, tt, tt), Λ (ff, ff, ff) ]



--- | less generic version
undistributeΛ_noΛ :: (Measurement a, Outcome b) 
    => Dist Λ -> ((a, Λ) -> Dist b) -> (a -> Dist b)
undistributeΛ_noΛ dΛ hvm a = do
    λ <- dΛ
    hvm (a, λ)

--- | explicitly return Λ as well
undistributeΛ :: (Measurement a, Outcome b) 
    => Dist Λ -> ((a, Λ) -> Dist b) -> (a -> Dist (Λ, b))
undistributeΛ dΛ hvm a = do
    λ <- dΛ
    b <- hvm (a, λ)
    return (λ, b)




--- | HVM Monad (nonlocal hidden variable model)
--- ================================================

type M a = StateT Λ Dist a
type MV a = StateT (Dist Λ) Dist a

--- | proto-qubit: might influence the other side
--- | reshape the distribution on-the-fly

hvmAM :: IA -> M Bool
hvmAM ia = do
    λ <- get
    lift $ hvmA (ia, λ)

hvmBM :: IB -> M Bool
hvmBM ib = do
    λ <- get
    lift $ hvmB (ib, λ)

--- | Run HVM to get joint distribution q(a,b|ia,ib)
hvmM :: (IA, IB) -> M (Bool, Bool)
hvmM (ia, ib) = do
    a <- hvmAM ia
    b <- hvmBM ib
    return (a, b)


--- | (IA, IB) ((IA -> M Bool), (IB -> M Bool))
--- | (M Bool, M Bool) -> M (Bool, Bool)
runHVM :: (IA, IB) -> Dist (Bool, Bool)
runHVM (ia, ib) = do
    λ <- uniformΛ
    evalStateT (hvmM (ia, ib)) λ