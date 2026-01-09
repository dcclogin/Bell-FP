module Spekken.Utils where

import Spekken.Ontic
import System.Random
import Data.Set (Set)
import qualified Data.Set as S


--- | convert Bool to Int
bool2Int :: Bool -> Int
bool2Int True = 1
bool2Int False = -1


--- | randomly pick an element from a List (uniformly)
randomFromList :: [a] -> IO a
randomFromList [] = error "Empty list."
randomFromList xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)


--- | randomly pick an element from a Set
randomFromSet :: Set a -> IO a
randomFromSet s = randomFromList (S.toList s)


--- | generate a random Ontic state
randomOntic :: IO Ontic
randomOntic = randomFromList allOntics


--- | generate a random pair of Ontic states
randomOnticPair :: IO (Ontic, Ontic)
randomOnticPair = do
    o1 <- randomOntic
    o2 <- randomOntic
    return (o1, o2)
