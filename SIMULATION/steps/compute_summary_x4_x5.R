#----------------------------------------------------
# SUMMARIES: GENERALIZED LINEAR MIXED MODEL - LOG LINK (POISSON)
#----------------------------------------------------

rm(list = ls(all = TRUE))

library(matrixStats)
library(dplyr)

source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_compute_summary.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_run_rows.R'))

run_by_row(
  step = 'compute_summary_x4_x5',
  inputs = list(
    simdata = list(dir = 'simdata_x4_x5', prefix = 'simdata')
  ),
  outputs = list(
    mean_cov = list(dir = 'mean_cov_x4_x5', prefix = 'mean_cov'),
    var_cov_mat = list(dir = 'var_cov_mat_x4_x5', prefix = 'var_cov_mat'),
    mv_moment_3_4_bypair_df = list(dir = 'mv_moment_3_4_bypair_df_x4_x5', prefix = 'mv_moment_3_4_bypair_df'),
    mv_moment_3_4_by3_df = list(dir = 'mv_moment_3_4_by3_df_x4_x5', prefix = 'mv_moment_3_4_by3_df'),
    mv_moment_4_df = list(dir = 'mv_moment_4_df_x4_x5', prefix = 'mv_moment_4_df')
  ),
  worker = function(row, env) {
    summary_info <- fn_compute_summary(row$iter, row$m, row$uniform_cluster_size, row$seed, env$simdata, include_x4_x5 = TRUE)
    list(
      mean_cov = summary_info[[1]],
      var_cov_mat = summary_info[[2]],
      mv_moment_3_4_bypair_df = summary_info[[3]],
      mv_moment_3_4_by3_df = summary_info[[4]],
      mv_moment_4_df = summary_info[[5]]
    )
  }
)