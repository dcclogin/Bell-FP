module Bell.Superdeterminism where

import Bell.Config
import Bell.Hidden
import ProbabilityMonads

-- | Superdeterministic empirical model (abandon λ-independence, save locality)


--- | indexing Λ by IA and IB choices


paramΛ :: (IA, IB) -> Dist Λ
paramΛ (ia, ib)
    | sameChoice ia ib 
        = uniformΛ
    | otherwise
        = uniformΛ