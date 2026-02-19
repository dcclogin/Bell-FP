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
        ├── Explore.hs         -- Three instructive examples in Section 2 
        │
        ├── Identity.hs        -- Deterministic local model
        ├── LHV.hs             -- Local hidden-variable models
        │
        ├── Shared.hs          -- Within-trial signaling model
        ├── Clocked.hs         -- Cross-trial scheduler leakage model
        ├── Superdet.hs        -- Superdeterministic model
        │
        ├── Quantum.hs         -- Quantum model (Tsirelson bound)
        ├── PR.hs              -- PR-box model (algebraic maximum 4)
        ├── PRState.hs         -- PR-box: a nonlocal hidden-variable model
        │
        └── Contextuality.hs   -- Specker triangle contextual model
```

You can either
- enter `Explore.hs` first and run three tiny examples one by one, and then navigate to other models listed above; or
- run all models (CHSH tests and no-signaling checks) at once using the command `cabal run`.



