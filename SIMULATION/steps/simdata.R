#------------------------------------------------------------------
# SIMULATION: GENERALIZED LINEAR MIXED MODEL - LOG LINK (POISSON)
#------------------------------------------------------------------

rm(list = ls(all = TRUE))

source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_run_rows.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_simdata.R'))

x_pars <- list(
  x1_norm_mean = 3150.149,
  x1_norm_sd = 843.0881,
  x2_p = 0.5602384,
  x3_multinom_p = c(0.09535973, 0.1272882, 0.1111111, 0.3865475, 0.2796935)
)

run_by_row(
  step = 'simdata',
  inputs = list(),
  outputs = list(
    simdata = list(dir = 'simdata', prefix = 'simdata')
  ),
  worker = function(row, env) {
    simdata <- fn_simdata_glmm(row$iter, row$m, row$uniform_cluster_size, row$seed, x_pars)
    env$simdata <- simdata
    list(simdata = simdata)
  }
)