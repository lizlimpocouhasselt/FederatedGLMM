#---------------------------------------------------------------
# LOCAL I/O FOR FIGURE SCRIPTS
#   Inputs are pulled from SIMULATION/intermediate_results/<family>/ on Drive
#   into a temporary local directory; rendered figures are written to the
#   workspace under Figures/outputs/<family>/ instead of being uploaded.
#---------------------------------------------------------------

source(file.path(getwd(), "R_common", "remote_store.R"))

fig_log <- function(fmt, ...) {
  message(sprintf("[%s] [figures] %s",
                  format(Sys.time(), "%H:%M:%S"),
                  sprintf(fmt, ...)))
}

# dirs: named character vector, e.g. c(point_estimates = "point_estimate")
fig_pull_inputs <- function(dirs, family, settings, nsim) {
  local_root <- tempfile("fglmm_fig_")
  fig_log("pulling inputs for family=%s into %s", family, local_root)
  for (d in names(dirs)) {
    files <- unlist(lapply(seq_len(nrow(settings)), function(s) {
      sprintf("%s_%04d_%04d_%04d.RData", dirs[[d]], seq_len(nsim),
              settings$m[s], settings$uniform_cluster_size[s])
    }))
    fig_log("pulling %d file(s) from %s", length(files),
            remote_path(file.path("SIMULATION", "intermediate_results", family, d)))
    remote_pull(file.path("SIMULATION", "intermediate_results", family, d),
                files, file.path(local_root, d))
  }
  fig_log("finished pulling inputs for family=%s", family)
  local_root
}

fig_output_dir <- function(family) {
  out_dir <- file.path(getwd(), "Figures", "outputs", family)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_dir
}

fig_output_file <- function(name, family) {
  file.path(fig_output_dir(family), name)
}

fig_save <- function(local_file, family) {
  if (!file.exists(local_file)) {
    stop(sprintf("Figure file not found: %s", local_file))
  }
  target_dir <- fig_output_dir(family)
  target_file <- file.path(target_dir, basename(local_file))
  if (normalizePath(local_file) == normalizePath(target_file)) {
    fig_log("figure already in workspace: %s", target_file)
    return(invisible(target_file))
  }
  file.copy(from = local_file, to = target_file, overwrite = TRUE)
  fig_log("saved %s to %s", basename(local_file), target_file)
  invisible(target_file)
}

fig_upload <- function(local_file, family) {
  fig_save(local_file, family)
}
