module CHSH.DSL where

------------------------------------------------------------
-- Outcomes and settings
------------------------------------------------------------

data Outcome = Minus | Plus
  deriving (Eq, Show)

outVal :: Outcome -> Int
outVal Minus = -1
outVal Plus  =  1

-- Alice has two possible measurement settings
data ASetting = A0 | A1
  deriving (Eq, Show)

-- Bob has two possible measurement settings
data BSetting = B0 | B1
  deriving (Eq, Show)

data OneShot = MeasureAB ASetting BSetting
  deriving (Eq, Show)

------------------------------------------------------------
-- Monadic λ interface
--
-- A "model" is any monad m that can tell us what
-- Alice and Bob output for each *local* setting.
--
-- The *effect* of m captures what λ is allowed to do:
--   - Identity          => pure hidden variable
--   - State / Reader    => global contextual store
--   - QuantumRun        => Born sampling, etc.
------------------------------------------------------------

class Monad m => LambdaModel m where
  -- Alice’s response to her local setting
  lambdaA :: ASetting -> m Outcome

  -- Bob’s response to his local setting
  lambdaB :: BSetting -> m Outcome

  -- Default way to run a single CHSH trial:
  -- one Alice measurement and one Bob measurement.
  measureAB :: ASetting -> BSetting -> m (Outcome, Outcome)
  measureAB a b = do
    x <- lambdaA a
    y <- lambdaB b
    pure (x, y)

------------------------------------------------------------
-- Utility: value of a single trial
--
-- A CHSH trial with settings (a,b) produces a pair (x,y).
-- The contribution to the correlation E[AB | a,b] is x*y.
------------------------------------------------------------

trialValue :: (Outcome, Outcome) -> Int
trialValue (x, y) = outVal x * outVal y

------------------------------------------------------------
-- Estimate E[AB | a,b] by Monte Carlo in any LambdaModel m.
--
-- The monad m is responsible for providing whatever
-- randomness / state / effects it wants. We just call
-- measureAB n times and average.
------------------------------------------------------------

estimateCorrelation
  :: LambdaModel m
  => Int        -- ^ number of trials
  -> ASetting
  -> BSetting
  -> m Double
estimateCorrelation n a b = go 0 0
  where
    go :: LambdaModel m => Int -> Int -> m Double
    go k acc
      | k >= n = pure (fromIntegral acc / fromIntegral n)
      | otherwise = do
          xy <- measureAB a b
          let acc' = acc + trialValue xy
          go (k + 1) acc'

------------------------------------------------------------
-- CHSH expression:
--   S = E(A0,B0) + E(A0,B1) + E(A1,B0) - E(A1,B1)
--
-- Abstract over the model m: only depends on lambdaA/B
-- (via measureAB).
------------------------------------------------------------

chsh
  :: LambdaModel m
  => Int        -- ^ samples per correlation
  -> m Double
chsh n = do
  e00 <- estimateCorrelation n A0 B0
  e01 <- estimateCorrelation n A0 B1
  e10 <- estimateCorrelation n A1 B0
  e11 <- estimateCorrelation n A1 B1
  pure (e00 + e01 + e10 - e11)

