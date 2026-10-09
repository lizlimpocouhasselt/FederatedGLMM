#---------------------------------------------------------------
# DRIVE I/O FOR FIGURE SCRIPTS
#   Inputs are pulled from SIMULATION/intermediate_results/<family>/ on Drive
#   into a temporary local directory; figures are written to a temporary file
#   and uploaded to Figures/outputs/<family>/ on Drive (see R_common/remote_store.R).
#---------------------------------------------------------------

source(file.path(getwd(), "R_common", "remote_store.R"))

# dirs: named character vector, e.g. c(point_estimates = "point_estimate")
fig_pull_inputs <- function(dirs, family, settings, nsim) {
  local_root <- tempfile("fglmm_fig_")
  for (d in names(dirs)) {
    files <- unlist(lapply(seq_len(nrow(settings)), function(s) {
      sprintf("%s_%04d_%04d_%04d.RData", dirs[[d]], seq_len(nsim),
              settings$m[s], settings$uniform_cluster_size[s])
    }))
    message(sprintf("[figures] pulling %d file(s) from %s", length(files),
                    remote_path(file.path("SIMULATION", "intermediate_results", family, d))))
    remote_pull(file.path("SIMULATION", "intermediate_results", family, d),
                files, file.path(local_root, d))
  }
  local_root
}

fig_output_file <- function(name) {
  file.path(tempdir(), name)
}

fig_upload <- function(local_file, family) {
  key <- file.path("Figures", "outputs", family, basename(local_file))
  upload_file(local_file, key)
  message("[figures] uploaded ", remote_path(key))
  invisible(key)
}
