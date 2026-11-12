# Bell-FP

This repository accompanies the paper

> **The Bell Experiment, Monadically** ([`paper/bell-fp.pdf`](paper/bell-fp.pdf))

It provides a small, executable framework for modeling the CHSH
experiment in Haskell. The boxes of the two parties compute in one monad,
the experiment that schedules and repeats their trials computes in
another, and a *membrane* runs the first inside the second. A run keeps
the record of its trials, and both of its verdicts, the CHSH value and
whether the boxes signal, are read off one table: the empirical model of
that run.

The code of the original ICFP 2026 submission is tagged
[`v1.0`](https://github.com/dcclogin/Bell-FP/tree/v1.0).

## Build

Requirement: GHC and Cabal (tested with GHC 9.4.8 and cabal 3.12).

```bash
cabal build
```

To run every model once and print its CHSH value and no-signaling verdict:
```bash
cabal run bell-fp
```

To run the test suite (27 cases):
```bash
cabal test
```

Or open GHCi:
```bash
cabal repl
```
and run the examples and reports one by one. Runs that use random numbers
fix the generator first (`seeded`), so every number is reproducible.

## Project Structure

```
Bell-FP/
├── README.md
├── bell-fp.cabal
├── app/
│   └── Main.hs                -- Runs every model once: CHSH values and no-signaling verdicts
├── src/
│   └── CHSH/
│       ├── Util.hs            -- Settings, outcomes, scores, the CHSH expression
│       ├── Experiment.hs      -- TrialModel, the experiment monad, schedulers, trials, the table
│       ├── NoSignaling.hs     -- No-signaling, read off the table of the run; reports
│       │
│       ├── Explore.hs         -- The two examples of Section 2: pure boxes and a shared location
│       ├── Identity.hs        -- Pure, deterministic boxes
│       ├── Shared.hs          -- A shared location: boxes that signal within a trial
│       ├── Clocked.hs         -- Boxes that read the step: scheduler-induced loopholes
│       │
│       ├── LHV.hs             -- Local hidden variables and their samplers
│       ├── Superdet.hs        -- Superdeterminism: samplers that see the settings
│       │
│       ├── Quantum.hs         -- Two qubits: state vectors, projectors, the Born rule
│       ├── PR.hs              -- PR boxes: the algebraic maximum without signaling
│       ├── PRState.hs         -- The PR box from a hidden state that the boxes rewrite
│       ├── BellTable.hs       -- The Bell table from a hidden state
│       │
│       └── Contextuality.hs   -- Specker triangle contextual model
├── demo/
│   ├── Old.hs                 -- The earlier sampler and trial code, verbatim
│   └── Compare.hs             -- Both versions, run for run (cabal run bell-fp-compare)
├── test/
│   └── Spec.hs                -- HUnit test suite
└── paper/
    ├── bell-fp.tex            -- The paper
    ├── bell-fp.pdf            -- The paper, built
    ├── Makefile               -- make: builds the PDF from the code
    ├── asciify.py             -- Copies the quoted sources, without comments, for the listings
    ├── gen/                   -- The program that runs every model and writes the paper's data
    └── generated/             -- Every number, table and printed report in the paper
```

You can either
- open `Explore.hs` and evaluate its two tiny examples, then move on to the other models listed above; or
- run all models at once with `cabal run bell-fp`.

## The Demo

After `cabal build` and `cabal run bell-fp`, you will see one line per run, then the tables of the runs that signal.

```
CHSH values and no-signaling, one run of 80000 trials each
--------------------------------------------------------------------------------------
Model                    Sampler                Schedule      CHSH   No-signaling, this run
--------------------------------------------------------------------------------------
Pure boxes                                      fixed        2.000   ✓
Local hidden variables   plus2                  fixed        2.000   ✓
Local hidden variables   uniformAll             fixed        0.000   ✗ Bob at B0
Local hidden variables   uniformAll             random       0.008   ✓
Clocked, clock visible                          fixed        4.000   ✗ Bob at B1
Clocked, clock visible                          random       0.990   ✓
Superdeterminism         cheatLam               random       4.000   ✗ Bob at B1
Superdeterminism         cheatNS                random       4.000   ✓
Shared state                                    fixed        4.000   ✗ Bob at B1
Quantum                                         fixed        2.820   ✓
Quantum, theta = pi/8                           fixed        2.421   ✓
PR box                                          random       4.000   ✓
PR hidden state          uniformAll             random       4.000   ✓
PR hidden state          plus2                  fixed        4.000   ✗ Bob at B1
Bell table, hidden state uniformBlocks          fixed        2.500   ✓
...

The tables of the runs that signal
--------------------------------------------------------------------------------------

Clocked, clock visible, fixed schedule
a   b   |  ++     +-     -+     --    | P(x=+) P(y=+)  trials
A0  B0  | 1.000  0.000  0.000  0.000  | 1.000  1.000  20000
A0  B1  | 1.000  0.000  0.000  0.000  | 1.000  1.000  20000
A1  B0  | 1.000  0.000  0.000  0.000  | 1.000  1.000  20000
A1  B1  | 0.000  1.000  0.000  0.000  | 1.000  0.000  20000
Alice at A0  1.000  vs  1.000   ok
Alice at A1  1.000  vs  1.000   ok
Bob at B0    1.000  vs  1.000   ok
Bob at B1    1.000  vs  0.000   violated
...
```

The full output has 31 runs.

## The Paper

```bash
cd paper && make        # writes paper/bell-fp.pdf
```

The build runs in three steps:
1. `asciify.py` copies the sources that the paper quotes, without their comments. Every listing is quoted from the code by line number.
2. The program in `gen/` runs every model and writes every number, table and printed report into `generated/`. It stops if a claim of the text about them does not hold (31 runs, 49 claims). It pins the random library (`random` 1.3.1, `splitmix` 0.1.2) that the numbers were produced with.
3. tectonic builds the PDF.

It needs Python 3, GHC with cabal, and [tectonic](https://tectonic-typesetting.github.io). The code is set in the font Ioskeley Mono, which must be installed.
