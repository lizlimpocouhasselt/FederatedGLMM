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

### Remote intermediate storage

Intermediate data sets are now routed through Google Drive via `rclone`. Set the shared remote root with `FGLMM_RCLONE_ROOT` (or accept the default `gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data`). The `rclone` binary must be installed and authenticated before running DEMO or SIMULATION steps.

```bash
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
which rclone
rclone lsf "$FGLMM_RCLONE_ROOT/DEMO/intermediate_results"
```

The local `DEMO/intermediate_results` and `SIMULATION/intermediate_results` directories are no longer treated as the source of truth for generated artifacts; the remote Drive mirror is authoritative. Existing local copies can remain in place until you verify the remote contents and delete the local copies manually.

### Drive-backed run examples (rclone pipeline)

These examples show how to run scripts that read/write data on Google Drive through `rclone` and `R_common/remote_store.R`.

1. Set the remote root once per shell session.
2. Run scripts from `Codes_and_Data` so relative `source()` paths resolve.
3. Use helper checks to confirm remote inputs/outputs exist.

```bash
cd Codes_and_Data
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
```

Quick connectivity check:

```bash
rclone lsf --files-only "$FGLMM_RCLONE_ROOT/DEMO/intermediate_results/ps" | head
Rscript -e "source('R_common/remote_store.R'); print(length(remote_list('DEMO/intermediate_results/ps'))); print(remote_exists('DEMO/intermediate_results/preprocessed_data.csv'))"
```

Run DEMO with Drive-backed SPARCS streaming (default behavior when no local CSV is set):

```bash
cd Codes_and_Data
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
unset SPARCS_CSV_PATH
unset SPARCS_RCLONE_REMOTE
Rscript DEMO/scripts_and_functions/data_provider.R
Rscript DEMO/scripts_and_functions/da_genps.R
Rscript DEMO/scripts_and_functions/da_est.R
```

Force a full rebuild of `preprocessed_data.csv` from Drive (ignore existing remote cache):

```bash
cd Codes_and_Data
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
export DEMO_REBUILD=1
Rscript DEMO/scripts_and_functions/data_provider.R
unset DEMO_REBUILD
```

Override the Drive CSV location explicitly (for a non-default remote path):

```bash
cd Codes_and_Data
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
export SPARCS_RCLONE_REMOTE='gdrive:PhD/Working papers/Federated GLMM/alternate_path/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2022_20241021.csv'
Rscript DEMO/scripts_and_functions/data_provider.R
```

Run SIMULATION pipeline with remote `.RData` I/O:

```bash
cd Codes_and_Data
export FGLMM_RCLONE_ROOT='gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data'
Rscript SIMULATION/steps/run_pipeline.R
```

Check remote outputs after a run:

```bash
Rscript -e "source('R_common/remote_store.R'); cat('simdata files:', length(remote_list('SIMULATION/intermediate_results/poisson/simdata')), '\n'); cat('ps4 files:', length(remote_list('SIMULATION/intermediate_results/poisson/ps4')), '\n'); cat('pred files:', length(remote_list('SIMULATION/intermediate_results/poisson/preds')), '\n')"
```

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

`data_provider.R` streams the raw SPARCS CSV from Google Drive with `rclone cat` (no local copy) only when `DEMO/intermediate_results/preprocessed_data.csv` is missing or `DEMO_REBUILD=1`. One-time setup: `brew install rclone && rclone config` (Google Drive remote named `gdrive`). Override the source with `SPARCS_RCLONE_REMOTE` (rclone path) or `SPARCS_CSV_PATH` (local file).

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
