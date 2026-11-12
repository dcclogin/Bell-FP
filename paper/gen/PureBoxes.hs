-- The runs of the pure boxes of CHSH.Identity. They live in a module of
-- their own because CHSH.Explore defines a second TrialModel instance
-- for Identity (the all-plus boxes of Section 2, see Explored.hs), and
-- GHC cannot run boxes in Identity where both instances are in scope.
module PureBoxes (pureFixed, pureRandom, pureSeparate) where

import CHSH.Experiment
import CHSH.Identity (liftIdentity)
import CHSH.NoSignaling
import Check

import Data.Functor.Identity (runIdentity)

-- n trials under the fixed schedule, exact
pureFixed :: Int -> Report
pureFixed n = runIdentity (chshReport n fixedSchedule liftIdentity)

-- n trials under the random schedule, unseeded: the caller seeds it
-- (PaperData.fromSeed, which also prints the seed)
pureRandom :: Int -> IO Report
pureRandom n = chshReport n randomScheduleIO liftIdentity

pureSeparate :: IO Sep
pureSeparate = pure (runIdentity (separate liftIdentity))
