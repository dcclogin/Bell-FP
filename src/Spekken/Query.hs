module Spekken.Query where

import Spekken.Ontic
import Spekken.Epistemic

-- query 
data Query = S Ontic
    | Query :+: Query
    | Query :*: Query
    deriving (Show, Eq, Ord)


-- map epistemic state to valid query (6/16)
e2q :: Epistemic -> Query
e2q E12 = S O1 :+: S O2
e2q E13 = S O1 :+: S O3
e2q E14 = S O1 :+: S O4
e2q E23 = S O2 :+: S O3
e2q E24 = S O2 :+: S O4
e2q E34 = S O3 :+: S O4
-- e2q I   = S O1 :+: S O2 :+: S O3 :+: S O4


-- map valid query to epistemic state
q2e :: Query -> Epistemic
q2e (S O1 :+: S O2) = E12
q2e (S O1 :+: S O3) = E13
q2e (S O1 :+: S O4) = E14
q2e (S O2 :+: S O3) = E23
q2e (S O2 :+: S O4) = E24
q2e (S O3 :+: S O4) = E34
q2e _ = error "invalid query." 


-- example query
example_query_ZX, example_query_Bell :: Query
example_query_ZX = (e2q E12) :*: (e2q E13)
example_query_Bell = 
        (S O1 :*: S O1) :+: (S O2 :*: S O2) 
    :+: (S O3 :*: S O3) :+: (S O4 :*: S O4)

-- query projections
-- repetitive with :+: but okay for now
proj1, proj2 :: Query -> Query
proj1 (q1 :*: q2) = q1
proj1 (q1 :+: q2) = proj1 q1 :+: proj1 q2
proj1 (S o) = S o
proj2 (q1 :*: q2) = q2
proj2 (q1 :+: q2) = proj2 q1 :+: proj2 q2
proj2 (S o) = S o


-- mapO2mapE :: (Ontic -> Ontic) -> (Epistemic -> Epistemic)