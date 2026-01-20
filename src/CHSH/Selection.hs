-- Operational contextual decision theory

module CHSH.Selection where

import CHSH.DSL

import Numeric.LinearAlgebra ( )
import Prelude hiding ((<>))

------------------------------------------------------------
-- SELECTION MONAD (Escardó–Oliva)
------------------------------------------------------------

{--

newtype Sel r a = Sel { runSel :: (a -> r) -> a }

instance Functor (Sel r) where
  fmap f (Sel s) = Sel $ \k -> f (s (k . f))

instance Applicative (Sel r) where
  pure x = Sel $ \_ -> x
  Sel sf <*> Sel sx =
    Sel $ \k ->
      let f = sf (\g -> k (g (sx k)))
          x = sx (\v -> k (f v))
      in  f x

instance Monad (Sel r) where
  Sel sx >>= f =
    Sel $ \k ->
      let x = sx (\v -> k (runSel (f v) k))
      in  runSel (f x) k

type OutcomeSel = Sel Double Outcome

instance LambdaModel OutcomeSel where

  lambdaA :: ASetting -> OutcomeSel (Outcome, BSetting -> OutcomeSel Outcome)
  lambdaA a = do
    -- pick x based only on future constraints (not random)
    x <- Sel $ \score ->
      if score Plus >= score Minus then Plus else Minus

    let k :: BSetting -> OutcomeSel Outcome
        k b = Sel $ \score ->
          let e     = quantumCorr a b
              pSame = (1 + e)/2
              pDiff = 1 - pSame
              scorePlus  = pSame  * score x
              scoreMinus = pDiff  * score (flipOutcome x)
          in if scorePlus >= scoreMinus
               then x
               else flipOutcome x

    pure (x, k)


  lambdaB :: BSetting
          -> (Outcome, BSetting -> OutcomeSel Outcome)
          -> OutcomeSel Outcome
  lambdaB b (x, k) = k b

instance Interpreter OutcomeSel where
  interpretOneShot (MeasureAB a b) =
    do (x,k) <- lambdaA a
       y     <- lambdaB b (x,k)
       pure (x,y)
--}
