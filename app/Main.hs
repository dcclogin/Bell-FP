module Main (main) where

import qualified Bell.Config as C
import qualified Bell.Empirical as E
import qualified Bell.LHV as L
import qualified Bell.StateHV as S
import qualified Bell.Hidden as H

import qualified EPR.Empirical as EPR_E
import qualified EPR.LHV as EPR_L
import qualified EPR.StateHV as EPR_S

import ProbabilityMonads


import Lib


main :: IO ()
main = do
    L.testUniformDiff
    L.testLowerBoundDiff
    L.testUpperBoundDiff
    S.testUniformDiff
    S.testLowerBoundDiff
    S.testUpperBoundDiff
    EPR_E.printMixed
    EPR_E.printEPR
    EPR_L.testUniform
    EPR_S.testUniform
    S.testUniformSameSM
    S.testUniformDiffSM