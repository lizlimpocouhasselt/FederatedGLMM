#-------------------------------------------------------------------
#PSEUDO-DATA (3rd MOM): GENERALIZED LINEAR MODEL - LOG LINK (POISSON)
#-------------------------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE))

# Load packages
library(pracma)
library(dplyr)
library(MASS)

# Call functions
source(file.path(getwd(),'SIMULATION', 'scripts', 'lsqnonlin_2.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'gen_pseudo.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'obj.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'mvrnorm2.R'))

par_settings <- read.csv(file.path(getwd(), 'SIMULATION', 'par_settings.csv'))

lapply(1:nrow(par_settings), function(row){
  iter <- par_settings$iter[row]
  m <- par_settings$m[row]
  uniform_cluster_size <- par_settings$uniform_cluster_size[row]
  seed <- par_settings$seed[row]

  # Load summary data
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mean_cov_x4_x5', sprintf("mean_cov_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'var_cov_mat_x4_x5', sprintf("var_cov_mat_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mv_moment_3_4_bypair_df_x4_x5', sprintf("mv_moment_3_4_bypair_df_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'mv_moment_3_4_by3_df_x4_x5', sprintf("mv_moment_3_4_by3_df_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  set.seed(seed)
  pseudodata_3rdmom <- lapply(1:m, function(group_num){
    var_names <- mean_cov[[group_num]][, 'variable']
    pseudodata_3rdmom_ <- gen_pseudo(moment = 3, var_names = var_names, uni_sum = mean_cov[[group_num]], covmat = var_cov_mat[[group_num]], bv_34 = mv_moment_3_4_bypair_df[[group_num]], tv_34 = mv_moment_3_4_by3_df[[group_num]]) 
    pseudodata_3rdmom_$g <- group_num
    cat("grp ", group_num, " is finished")
    pseudodata_3rdmom_
  })
  filename_save <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'ps3_x4_x5', sprintf("pseudodata_3rdmom_%04d_%04d_%04d", iter, m, uniform_cluster_size))
  save(pseudodata_3rdmom, file = sprintf("%s.RData", filename_save))
})