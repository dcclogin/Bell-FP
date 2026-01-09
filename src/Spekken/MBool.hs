module Spekken.MBool where

import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map




class (Eq a, Ord a) => Measurement a where
    measurements :: [a]

class (Eq a, Ord a) => Outcome a where
    outcomes :: [a]

{--
class (Measurement ps, Outcome os) => EmpiricalModel ps os where
    em :: Map ps os

class (Measurement ps, Outcome os, Monad m) => HiddenVarModel ps os m where
    hv :: ps -> m os
--}

data Choice1 = Only
    deriving (Show, Eq, Ord)

data Choice2 = This | That
    deriving (Show, Eq, Ord)

data Choice3 = C1 | C2 | C3
    deriving (Show, Eq, Ord)

instance Measurement Choice1 where
    measurements = [Only]

instance Measurement Choice2 where
    measurements = [This, That]

instance Measurement Choice3 where
    measurements = [C1, C2, C3]

instance Outcome Bool where
    outcomes = [True, False]

instance (Measurement a, Measurement b) => Measurement (a, b) where
    measurements = [(x, y) | x <- measurements, y <- measurements]

instance (Outcome a, Outcome b) => Outcome (a, b) where
    outcomes = [(x, y) | x <- outcomes, y <- outcomes]


-- class and instances for empirical model
-- class and instances for hidden-variable model
-- class and instances for point-wise measurement


-- Qubit type: Outcome must be Bool
newtype Qubit p m = Qubit { runQubit :: p -> m Bool }


-- EPR example
newtype EPR = EPR (Map (Choice1, Choice1) (Bool, Bool))
-- CHSH example: Bell, Hardy, PR-Box.
newtype CHSH = CHSH (Map (Choice2, Choice2) (Bool, Bool))
-- GHZ example
newtype GHZ = GHZ (Map (Choice2, Choice2, Choice2) (Bool, Bool, Bool))
-- Mermin's Device example
newtype Mermin = Mermin (Map (Choice3, Choice3) (Bool, Bool))


{--
instance EmpiricalModel (Choice1, Choice1) (Bool, Bool) where
    em = Map.fromList [
            ((Only, Only), (True, False)),
            ((Only, Only), (False, True))
        ]
--}

-- integrate spekken's toy theory
-- State monad storing ontic states


main :: IO ()
main = do
    putStrLn "MBool module loaded."

