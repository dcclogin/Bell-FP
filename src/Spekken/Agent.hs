module Spekken.Agent where

import Spekken.Ontic
import Spekken.EpState
import Spekken.Qubit
import Data.Set (Set)
import qualified Data.Set as S

--- | an agent with an epistemic state and history
data Agent = Agent
  { currState :: EpState
  , history   :: [EpState]
  } deriving (Eq, Show)

--- | initialize an agent with a given epistemic state
initAgent :: EpState -> Agent
initAgent s = Agent
  { currState = s
  , history   = []
  }

--- | intialize an agent with maximum ignorance
initIgnorantAgent :: Agent
initIgnorantAgent = initAgent constT


--- | update the agent's epistemic state and record history
updateAgent :: EpState -> Agent -> Agent
updateAgent newState agent = agent
  { currState = newState
  , history   = currState agent : history agent
  }


--- | revert the agent to the previous epistemic state
revertAgent :: Agent -> Maybe Agent
revertAgent agent = case history agent of
  []       -> Nothing
  (s : ss) -> Just agent
    { currState = s
    , history   = ss
    }

--- | clear the agent's history
clearHistory :: Agent -> Agent
clearHistory agent = agent { history = [] }


--- | get the length of the agent's history
historyLength :: Agent -> Int
historyLength agent = length (history agent)

--- | get the agent's current epistemic state
getCurrentState :: Agent -> EpState
getCurrentState = currState

--- | get the agent's history of epistemic states
getHistory :: Agent -> [EpState]
getHistory = history

--- | check if the agent's current epistemic state covers a given ontic state
agentCovers :: Agent -> Ontic -> Bool
agentCovers agent o =  o `S.member` (let (EpState s) = currState agent in s)