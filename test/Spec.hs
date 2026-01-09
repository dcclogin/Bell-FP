module Main (main) where

import Test.HUnit
import TestQubit



main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ 
            testIdempotent 
        ]
    return ()
