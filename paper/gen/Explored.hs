-- The CHSH values that Section 2 of the paper computes directly:
-- chshId (the all-plus boxes in Identity) and chshST (the shared
-- location). CHSH.Explore's instance TrialModel Identity must not meet
-- the one of CHSH.Identity, so only these two numbers leave the module
-- (see PureBoxes.hs).
module Explored (chshIdValue, chshSTValue) where

import CHSH.Explore (chshId, chshST)

chshIdValue, chshSTValue :: Int
chshIdValue = chshId
chshSTValue = chshST
