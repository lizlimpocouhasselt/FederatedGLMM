# Federated GLMM: Codes and Data

This repository contains the code and selected data assets for the Federated GLMM project.

## Repository Map

- `DEMO/`: real-data demo pipeline
  - `scripts_and_functions/`: demo scripts and reusable functions
  - `intermediate_results/`: generated demo artifacts (mostly `.RData`)
- `SIMULATION/`: simulation pipeline
  - `scripts/`: simulation scripts and reusable functions
  - `intermediate_results/`: generated simulation artifacts (large, reproducible)
  - `par_settings.csv`: simulation parameter settings
- `Figures/`: scripts and outputs for manuscript figures
  - `scripts_and_functions/`: figure generation code
  - `outputs/`: generated figure files

## Source of Truth

Treat these as source-of-truth assets that should be versioned and reviewed:

- R scripts in `DEMO/scripts_and_functions/`, `SIMULATION/scripts/`, and `Figures/scripts_and_functions/`
- Small configuration files such as `SIMULATION/par_settings.csv`
- Documentation and policies in the repository root

Treat these as generated artifacts that should usually not be versioned:

- `SIMULATION/intermediate_results/`
- Most `.RData` files and session artifacts (`.Rhistory`, `.DS_Store`)

See the concrete rules in `FOLDER_POLICY.md`.

## Minimal Workflow

1. Run scripts to generate intermediate outputs locally.
2. Keep edits focused on scripts/config/docs.
3. Verify only intended files are staged before commit.
4. Push small, reviewable commits to `main`.

Example checks:

```bash
git status -sb
git diff --cached --name-only
```

## Quick Start

Run all commands from the repository root.

### DEMO pipeline

Run in this order:

1. `DEMO/scripts_and_functions/data_provider.R`
2. `DEMO/scripts_and_functions/da_genps.R`
3. `DEMO/scripts_and_functions/da_est.R`

```bash
Rscript DEMO/scripts_and_functions/data_provider.R
Rscript DEMO/scripts_and_functions/da_genps.R
Rscript DEMO/scripts_and_functions/da_est.R
```

### SIMULATION pipeline

Run in this order:

1. `SIMULATION/scripts/simdata.R`
2. `SIMULATION/scripts/compute_summary.R`
3. `SIMULATION/scripts/pseudodata_2ndmom.R`, `SIMULATION/scripts/pseudodata_3rdmom.R`, `SIMULATION/scripts/pseudodata_4thmom.R`
4. `SIMULATION/scripts/estimates.R`
5. `SIMULATION/scripts/preds.R`

```bash
Rscript SIMULATION/scripts/simdata.R
Rscript SIMULATION/scripts/compute_summary.R
Rscript SIMULATION/scripts/pseudodata_2ndmom.R
Rscript SIMULATION/scripts/pseudodata_3rdmom.R
Rscript SIMULATION/scripts/pseudodata_4thmom.R
Rscript SIMULATION/scripts/estimates.R
Rscript SIMULATION/scripts/preds.R
```

## Sync Hygiene

Before pushing, confirm that staged files are mostly code/config/docs and not bulk generated outputs.

If you accidentally staged generated files:

```bash
git restore --staged SIMULATION/intermediate_results
```

Then re-check and commit only intentional changes.
