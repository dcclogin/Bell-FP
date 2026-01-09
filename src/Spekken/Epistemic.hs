module Spekken.Epistemic where


--- | epistemic states (single toy qubit)

--- | Z basis:
--- E12 ~ |0⟩ = ■■□□
--- E34 ~ |1⟩ = □□■■

--- |  X basis:
--- E13 ~ |+⟩ = ■□■□
--- E24 ~ |-⟩ = □■□■

--- | Y basis:
--- E14 ~ |↑⟩ = ■□□■
--- E23 ~ |↓⟩ = □■■□

--- ET ~ 1 ~ ■■■■ (constant truth, mixed, full ignorance)
--- EF ~ 0 ~ □□□□ (constant falsity)

data Epistemic = E12 | E13 | E14 | E23 | E24 | E34
    -- | ET | EF
    deriving (Show, Eq, Ord)


-- negate epistemic state
-- 1 - |φ⟩⟨φ|
opposite :: Epistemic -> Epistemic
opposite E12 = E34
opposite E13 = E24
opposite E14 = E23
opposite E23 = E14
opposite E24 = E13
opposite E34 = E12

-- X permutation
xPermE :: Epistemic -> Epistemic
xPermE e = 
    if e == E13 || e == E24
        then e
        else opposite e

-- Y permutation
yPermE :: Epistemic -> Epistemic
yPermE e = 
    if e == E14 || e == E23
        then e
        else opposite e

-- Z permutation
zPermE :: Epistemic -> Epistemic
zPermE e = 
    if e == E12 || e == E34
        then e
        else opposite e

-- Hadamard permutation
hPermE :: Epistemic -> Epistemic
hPermE e = 
    case e of
        E12 -> E13
        E13 -> E12
        E34 -> E24
        E24 -> E34
        els -> opposite els 
