module Spekken.EpState (
    EpState(..),
    -- mkEpState,
    fromOnticPair,
    constF,
    constT,
    zT, zF, 
    xT, xF,
    yT, yF,
    EpQuery,
    PermE(..),
    permuteEp,
    notEp,
    xPermEp,
    yPermEp,
    zPermEp,
    sPermEp,
    hPermEp
) where

import Spekken.Ontic

import Data.Set (Set)
import qualified Data.Set as S

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


--- | possible to make it "representation independent"!

newtype EpState = EpState { support :: Set Ontic }
  deriving (Eq, Ord, Show)


--- | Smart constructor: only size-2 subsets are accepted as “pure” toy-bit states.
mkEpState :: Set Ontic -> Maybe EpState
mkEpState s
  | S.size s == 2 = Just (EpState s)
  | otherwise     = Nothing


--- | Convenience: build an epistemic state from a pair of ontic states.
fromOnticPair :: Ontic -> Ontic -> EpState
fromOnticPair a b = EpState (S.fromList [a, b])  -- always size 2, so totally safe


constF, constT :: EpState
constF = EpState S.empty
constT = EpState (S.fromList allOntics)

zT, zF :: EpState
zT = fromOnticPair O1 O2
zF = fromOnticPair O3 O4

xT, xF :: EpState
xT = fromOnticPair O1 O3
xF = fromOnticPair O2 O4

yT, yF :: EpState
yT = fromOnticPair O1 O4
yF = fromOnticPair O2 O3


--- | from "statement/state" to "question/projector" (i.e. |φ⟩ to |φ⟩⟨φ|)
--- | temporary just type synonym, no distinction.
type EpQuery = EpState


--- | permutations of epistemic states
newtype PermE = PermE { runPermE :: EpState -> EpState }

--- | EpState permutation induced by an Ontic permutation
permuteEp :: PermO -> (EpState -> EpState)
permuteEp (PermO p) (EpState s) = EpState (S.map p s)

--- | negate EpState
notEp :: EpState -> EpState
notEp (EpState s) = EpState (S.difference (S.fromList allOntics) s)


--- | pauli X gate permutation
xPermEp :: PermE
xPermEp = PermE $ \e@(EpState s) ->
    if s == support xT || s == support xF
        then e
        else notEp e


--- | pauli Y gate permutation
yPermEp :: PermE
yPermEp = PermE $ \e@(EpState s) ->
    if s == support yT || s == support yF
        then e
        else notEp e


--- | pauli Z gate permutation
zPermEp :: PermE
zPermEp = PermE $ \e@(EpState s) ->
    if s == support zT || s == support zF
        then e
        else notEp e


--- | S gate permutation
sPermEp :: PermE
sPermEp = PermE $ \e@(EpState s) ->
    case s of
        _ | s == support zT -> yT
          | s == support zF -> yF
          | s == support yT -> notEp zF
          | s == support yF -> notEp zT
          | s == support xT -> xT
          | s == support xF -> xF
          | otherwise       -> e


--- | Hadamard gate permutation
hPermEp :: PermE
hPermEp = PermE $ \e@(EpState s) ->
    case s of
        _ | s == support zT -> xT
          | s == support zF -> xF
          | s == support xT -> zT
          | s == support xF -> zF
          | s == support yT -> notEp xF
          | s == support yF -> notEp xT
          | otherwise       -> e