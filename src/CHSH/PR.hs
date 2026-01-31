module CHSH.PR where

-- jointAB freely looks at both settings

-- preserves parameter indepedence
-- breaks outcome independence;
-- contrast with Quantum that has jointAB depend on a,b but
-- in a constrained way

import CHSH.Syntax
import CHSH.Util
import CHSH.Explore (fixedSchedule, randomScheduleIO)

import Control.Monad.IO.Class
import System.Random (randomRIO)

newtype PR a = PR { unPR :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTrialPR :: RunTrial IO PR
runTrialPR _ = unPR

instance TrialModel PR where
  -- local marginals are uniform; defined for completeness
  localA _ = PR $ do r <- randomRIO (0::Int,1); pure (if r==0 then Plus else Minus)
  localB _ = PR $ do r <- randomRIO (0::Int,1); pure (if r==0 then Plus else Minus)

  -- PR constraint: x ⊕ y = a ∧ b, with uniform marginals
  jointAB a b = PR $ do
    x <- randomRIO (0::Int,1)          -- choose x uniformly
    let c = (aBit a) * (bBit b)        -- a ∧ b (since bits are 0/1)
        y = x `xor` c
    pure (bitOut x, bitOut y)
    where xor u v = (u + v) `mod` 2

------------------------------------------------------------
-- Tests

-- PR should hit ~4 under any schedule
testPR_fixed :: IO Double
testPR_fixed = chsh 20000 fixedSchedule runTrialPR

testPR_random :: IO Double
testPR_random = chsh 20000 randomScheduleIO runTrialPR

-- Quick no-signaling sanity: Alice marginal at A0 should not depend on Bob setting.
-- We estimate P(Alice=Plus | A0,B0) and P(Alice=Plus | A0,B1) and compare.
pAlicePlus :: Int -> ASetting -> BSetting -> IO Double
pAlicePlus n a b = do
  xs <- sequence [ fst <$> runTrialPR 0 (jointAB a b) | _ <- [1..n] ]
  let k = length [ () | Plus <- xs ]
  pure (fromIntegral k / fromIntegral n)

testPR_noSignal_A0 :: IO (Double, Double, Double)
testPR_noSignal_A0 = do
  p00 <- pAlicePlus 20000 A0 B0
  p01 <- pAlicePlus 20000 A0 B1
  pure (p00, p01, abs (p00 - p01))
-- Expect p00 ~ 0.5, p01 ~ 0.5, diff small (say < 0.02 with 20k)