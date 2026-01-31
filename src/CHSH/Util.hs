module CHSH.Util where

import System.Random (randomRIO)

-- Outcomes and settings

-- We have an experiment with the following ingredients:
--   - two black boxes, one with Alice and one with Bob
--   - each black box has one input with two possible values
--   - each black box produces +/-1 as output
--   - the internals of the black boxes are unknown but are
--     subject to possible constraints we will explore

-- Each party has two possible settings (inputs to their black box)

data ASetting = A0 | A1 deriving Eq
data BSetting = B0 | B1 deriving Eq

-- A CHSH trial with settings (a,b) produces a pair (x,y).
-- The score is +/-1 indicating agreement or disagreement

data Outcome = Minus | Plus deriving Eq

-- Sample an unbiased sign (±1) as Outcome
randomOutcome :: IO Outcome
randomOutcome = do
  r <- randomRIO (0 :: Int, 1)
  pure $ if r == 0 then Minus else Plus

flipOutcome :: Outcome -> Outcome
flipOutcome Minus = Plus
flipOutcome Plus  = Minus

trialValue :: (Outcome, Outcome) -> Int
trialValue (x, y) | x == y    = 1
                  | otherwise = -1

outBit :: Outcome -> Int
outBit Plus  = 0
outBit Minus = 1

bitOut :: Int -> Outcome
bitOut b = if b `mod` 2 == 0 then Plus else Minus

aBit :: ASetting -> Int
aBit A0 = 0
aBit A1 = 1

bBit :: BSetting -> Int
bBit B0 = 0
bBit B1 = 1

-- Sampling

mcAverageIx :: Monad e => Int -> (Int -> e Int) -> e Double
mcAverageIx n sampleAt = do
  xs <- mapM sampleAt [0 .. n-1]
  pure (fromIntegral (sum xs) / fromIntegral n)

uniformCycle :: Applicative e => [a] -> Int -> e a
uniformCycle xs i = pure (xs !! (i `mod` length xs))

prettyReport :: (Bool, String) -> IO ()
prettyReport (ok, msg) = do
  putStrLn msg
  putStrLn $
    if ok then "\n✓ No-signaling holds"
          else "\n✗ No-signaling VIOLATED"

------------------------------------------------------------