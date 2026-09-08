# Folder Policy

This policy keeps the repository organized, reproducible, and fast to sync.

## 1) Folder Roles

- `DEMO/`: demo data workflow and demo analysis code.
- `SIMULATION/`: simulation code, parameter settings, and generated simulation results.
- `Figures/`: scripts and outputs used to create manuscript figures.

## 2) What Must Be Tracked

Track files that define methods, reproducibility, and interpretation:

- `*.R` scripts under script folders
- Parameter/config files such as `SIMULATION/par_settings.csv`
- Final small outputs needed for papers (for example selected `.pdf` or `.eps`)
- Documentation (`README.md`, this policy)

## 3) What Should Not Be Tracked

Do not track large or regenerable artifacts unless explicitly required for release:

- `SIMULATION/intermediate_results/`
- Bulk `.RData` intermediates
- Local machine artifacts (`.DS_Store`, `.Rhistory`)

The `.gitignore` file enforces these defaults.

## 4) Placement Rules

- New reusable code goes in:
  - `DEMO/scripts_and_functions/`
  - `SIMULATION/scripts/`
  - `Figures/scripts_and_functions/`
- New generated files go in the relevant `intermediate_results/` or `outputs/` folder.
- Do not place generated files in script folders.

## 5) Naming Rules

- Use descriptive snake_case names for scripts and generated files.
- Include model/context tags when useful (for example `poisson`, `x4_x5`).
- Avoid spaces in filenames.

## 6) Pre-Commit Checklist

Run before each commit:

```bash
git status -sb
git diff --cached --name-only
```

If generated files are staged unintentionally:

```bash
git restore --staged SIMULATION/intermediate_results
```

## 7) Optional Release Exception

If a release needs archived intermediates, store them outside the main repository flow (for example GitHub release assets or an external data archive), and reference them from documentation.
