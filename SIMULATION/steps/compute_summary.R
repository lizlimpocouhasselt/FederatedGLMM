#----------------------------------------------------
# SUMMARIES: GENERALIZED LINEAR MIXED MODEL - LOG LINK (POISSON)
#----------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE))

# Load packages
library(matrixStats)
library(dplyr)

# Call functions
source(file.path('SIMULATION', 'scripts', 'fn_compute_summary.R'))

# Load parameter settings
par_settings <- read.csv(file.path('SIMULATION', 'par_settings.csv'))

lapply(1:nrow(par_settings), function(row){
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]

    # Load data
    load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'simdata', sprintf('simdata_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
    summary_info <- fn_compute_summary(iter, m, uniform_cluster_size, seed, simdata) 
    mean_cov <- summary_info[[1]]
    var_cov_mat <- summary_info[[2]]
    mv_moment_3_4_bypair_df <- summary_info[[3]]
    mv_moment_3_4_by3_df <- summary_info[[4]]
    mv_moment_4_df <- summary_info[[5]]

    filename_save_mean_cov <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mean_cov', sprintf("mean_cov_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(mean_cov, file = sprintf("%s.RData", filename_save_mean_cov))

    filename_save_var_cov_mat <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'var_cov_mat', sprintf("var_cov_mat_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(var_cov_mat, file = sprintf("%s.RData", filename_save_var_cov_mat))

    filename_save_mv_moment_3_4_bypair_df <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mv_moment_3_4_bypair_df', sprintf("mv_moment_3_4_bypair_df_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(mv_moment_3_4_bypair_df, file = sprintf("%s.RData", filename_save_mv_moment_3_4_bypair_df))

    filename_save_mv_moment_3_4_by3_df <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mv_moment_3_4_by3_df', sprintf("mv_moment_3_4_by3_df_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(mv_moment_3_4_by3_df, file = sprintf("%s.RData", filename_save_mv_moment_3_4_by3_df))

    filename_save_mv_moment_4_df <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mv_moment_4_df', sprintf("mv_moment_4_df_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(mv_moment_4_df, file = sprintf("%s.RData", filename_save_mv_moment_4_df))
})