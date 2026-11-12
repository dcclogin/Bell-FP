-- The separate check of the submission (Separate.hs), run the way the
-- submission ran it, keeping the two frequencies that each of its four
-- conditions compares.
module Check (Sep (..), separate, nSep, eps) where

import CHSH.Experiment (RunTrial, TrialModel)
import CHSH.Util
import Separate (checkNoSignalA, checkNoSignalB)

-- trials per context, and the tolerance: the values with which the
-- submission called noSignalingReport
nSep :: Int
nSep = 20000

eps :: Double
eps = 0.02

-- the verdict, and for each condition the two frequencies it compares
data Sep = Sep
  { sepOk   :: Bool
  , sepRows :: [(String, (Double, Double))]
  }

separate :: (Monad e, TrialModel t) => RunTrial e t -> e Sep
separate m = do
  (okA0, pa0) <- checkNoSignalA nSep eps m A0
  (okA1, pa1) <- checkNoSignalA nSep eps m A1
  (okB0, pb0) <- checkNoSignalB nSep eps m B0
  (okB1, pb1) <- checkNoSignalB nSep eps m B1
  pure (Sep (and [okA0, okA1, okB0, okB1])
            [ ("Alice at A0", pa0), ("Alice at A1", pa1)
            , ("Bob at B0", pb0), ("Bob at B1", pb1) ])
