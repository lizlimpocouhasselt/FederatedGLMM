# Federated GLMM: Codes and Data

This repository contains the code and selected data assets for the Federated GLMM project.

## Repository Map

- `DEMO/`: real-data demo pipeline
  - `scripts_and_functions/`: demo scripts and reusable functions
  - `intermediate_results/`: generated demo artifacts (mostly `.RData`)
- `SIMULATION/`: simulation pipeline
  - `steps/`: simulation pipeline entry scripts (run order)
  - `scripts/`: reusable simulation functions and helper modules
  - `intermediate_results/`: generated simulation artifacts (large, reproducible)
  - `par_settings.csv`: simulation parameter settings
- `R_common/`: helper functions shared by both DEMO and SIMULATION
- `Figures/`: scripts and outputs for manuscript figures
  - `scripts_and_functions/`: figure generation code
  - `outputs/`: generated figure files

## Source of Truth

Treat these as source-of-truth assets that should be versioned and reviewed:

- R scripts in `DEMO/scripts_and_functions/`, `SIMULATION/steps/`, `SIMULATION/scripts/`, `R_common/`, and `Figures/scripts_and_functions/`
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

1. `SIMULATION/steps/simdata.R`
2. `SIMULATION/steps/compute_summary.R`
3. `SIMULATION/steps/pseudodata_2ndmom.R`, `SIMULATION/steps/pseudodata_3rdmom.R`, `SIMULATION/steps/pseudodata_4thmom.R`
4. `SIMULATION/steps/estimates.R`
5. `SIMULATION/steps/preds.R`

```bash
Rscript SIMULATION/steps/simdata.R
Rscript SIMULATION/steps/compute_summary.R
Rscript SIMULATION/steps/pseudodata_2ndmom.R
Rscript SIMULATION/steps/pseudodata_3rdmom.R
Rscript SIMULATION/steps/pseudodata_4thmom.R
Rscript SIMULATION/steps/estimates.R
Rscript SIMULATION/steps/preds.R
```

## Environment management

This project uses `renv` for a reproducible R library. The package list in `R_packages.txt` is the source manifest; use the lockfile for restores and updates.

```bash
cd Codes_and_Data
Rscript -e 'renv::restore()'
Rscript -e 'renv::snapshot(type = "all", prompt = FALSE)'
```

## Runner script

The full SIMULATION pipeline can be run from a single entry point. The baseline pipeline runs by default, and the x4_x5 variant can be added with a toggle.

```bash
cd Codes_and_Data
Rscript SIMULATION/steps/run_pipeline.R
Rscript SIMULATION/steps/run_pipeline.R --include-x4-x5
Rscript SIMULATION/steps/run_pipeline.R --dry-run --include-x4-x5
```

## Sync Hygiene

Before pushing, confirm that staged files are mostly code/config/docs and not bulk generated outputs.

If you accidentally staged generated files:

```bash
git restore --staged SIMULATION/intermediate_results
```

Then re-check and commit only intentional changes.
