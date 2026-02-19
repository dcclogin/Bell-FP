# Bell-FP

This repository accompanies the ICFP submission:

> **Functional Pearl: The Bell Experiment, Monadically**

It provides a small, executable framework for modeling the CHSH
experiment in Haskell.

## Build

Requirement: GHC and Cabal.

```bash
cabal build
```

To run the demo executable:
```bash
cabal run
```

Or open GHCi:
```bash
cabal repl
```
and run examples and tests one by one.

## Project Structure

```
bell-fp/
├── README.md
├── bell-fp.cabal
├── app/
│   └── Main.hs                -- Demo entry point (runs selected models)
└── src/
    └── CHSH/
        ├── Util.hs            -- Basic types, scoring, etc.
        ├── Experiment.hs      -- TrialModel class, scheduling, CHSH calculation
        ├── NoSignaling.hs     -- Empirical no-signaling checks
        │
        ├── Identity.hs        -- Deterministic local model
        ├── LHV.hs             -- Local hidden-variable models
        │
        ├── Shared.hs          -- Within-trial signaling model
        ├── Clocked.hs         -- Cross-trial scheduler leakage 
        ├── Superdet.hs        -- Superdeterministic model
        │
        ├── Quantum.hs         -- Quantum model (Tsirelson bound)
        ├── PR.hs              -- PR-box model (algebraic maximum 4)
        ├── PRState.hs         -- A nonlocal hidden-variable model of PR-box
        │
        └── Contextuality.hs   -- Specker triangle contextual model
```



