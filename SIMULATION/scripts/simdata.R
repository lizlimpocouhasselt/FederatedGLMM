#------------------------------------------------------------------
# SIMULATION: GENERALIZED LINEAR MIXED MODEL - LOG LINK (POISSON)
#------------------------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE)) 

# Call functions
source(file.path('SIMULATION', 'scripts', 'fn_simdata.R'))

# Set parameters based on real SPARCS dataset
x_pars <- list(x1_norm_mean = 3150.149,
               x1_norm_sd = 843.0881,
               x2_p = 0.5602384,
               x3_multinom_p = c(0.09535973, 0.1272882, 0.1111111, 0.3865475, 0.2796935)
               )

par_settings <- read.csv(file.path('SIMULATION', 'par_settings.csv'))

lapply(1:nrow(par_settings), function(row){
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]
    simdata <- fn_simdata_glmm(iter, m, uniform_cluster_size, seed, x_pars) 
    filename_save <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'simdata', sprintf("simdata_%04d_%04d_%04d", iter, m, uniform_cluster_size))
    save(simdata, file = sprintf("%s.RData", filename_save))
})