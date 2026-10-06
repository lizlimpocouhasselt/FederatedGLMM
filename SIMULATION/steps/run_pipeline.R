#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

dry_run <- FALSE
include_x4_x5 <- FALSE

for (arg in args) {
  if (arg %in% c("-h", "--help")) {
    writeLines(c(
      "Usage: Rscript SIMULATION/steps/run_pipeline.R [--include-x4-x5] [--dry-run]",
      "",
      "Runs the SIMULATION pipeline in order.",
      "--include-x4-x5 adds the x4_x5 variant after the default baseline steps.",
      "--dry-run prints the planned step order without executing anything."
    ))
    quit(status = 0)
  }

  if (arg %in% c("--include-x4-x5", "--all", "--variant=x4_x5")) {
    include_x4_x5 <- TRUE
  }

  if (arg %in% c("--dry-run", "--print-plan")) {
    dry_run <- TRUE
  }
}

script_dir <- normalizePath(dirname(sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))][1])))
project_root <- normalizePath(file.path(script_dir, "..", ".."))
setwd(project_root)

base_steps <- c(
  "simdata.R",
  "compute_summary.R",
  "pseudodata_2ndmom.R",
  "pseudodata_3rdmom.R",
  "pseudodata_4thmom.R",
  "estimates.R",
  "preds.R"
)

x4_x5_steps <- c(
  "simdata_x4_x5.R",
  "compute_summary_x4_x5.R",
  "pseudodata_2ndmom_x4_x5.R",
  "pseudodata_3rdmom_x4_x5.R",
  "pseudodata_4thmom_x4_x5.R"
)

steps <- base_steps
if (include_x4_x5) {
  steps <- c(steps, x4_x5_steps)
}

if (dry_run) {
  writeLines(c("Planned SIMULATION steps:", steps))
  quit(status = 0)
}

for (step in steps) {
  script_path <- file.path(project_root, "SIMULATION", "steps", step)
  message("\n=== Running: ", step, " ===")
  status <- system2("Rscript", script_path)
  if (status != 0L) {
    stop(sprintf("Pipeline failed while running '%s' (exit code %s).", step, status))
  }
}

message("\nSIMULATION pipeline completed successfully.")
