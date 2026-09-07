#-------------------------------------------------------------------
# MODEL PREDICTION
#-------------------------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE))

# Load packages
library(lme4)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)

# # Call functions
source(file.path(getwd(), 'SIMULATION', 'scripts','soft_poisson.R'))

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "intermediate_results", "poisson", "par_settings.csv"))


# Redefine glm families with no scale to accommodate non-binary or non-integer responses
lme4.env <- getNamespace("lme4")
newhasNoScale <- function (family) {
  any(substr(family$family, 1L, 16L) == c("poisson", "binomial", "negative.bin", "Negative Bin", "soft_poisson"))
}
unlockBinding("hasNoScale", lme4.env)
assign("hasNoScale", newhasNoScale, envir = lme4.env)
lockBinding("hasNoScale", lme4.env)

#-----------------
# POISSON MODELS
#-----------------

mod.selection.df <- sapply(1:nrow(par_settings), function(row){
  iter <- par_settings$iter[row]
  m <- par_settings$m[row]
  uniform_cluster_size <- par_settings$uniform_cluster_size[row]

  # Load data
  load(file.path("SIMULATION", "intermediate_results", "poisson", "aic_x4_x5", sprintf("aic_values_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  # load(file.path("SIMULATION", "intermediate_results", "poisson", "simdata_x4_x5", sprintf("simdata_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  # load(file.path("SIMULATION", "intermediate_results", "poisson", "ps2_x4_x5", sprintf("pseudodata_2ndmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  # ps2 <- bind_rows(pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  # load(file.path("SIMULATION", "intermediate_results", "poisson", "ps3_x4_x5", sprintf("pseudodata_3rdmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  # ps3 <- bind_rows(pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  # load(file.path("SIMULATION", "intermediate_results", "poisson", "ps4_x4_x5", sprintf("pseudodata_4thmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  # ps4 <- bind_rows(pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

  # # Estimate a Poisson mixed model using x1 only
  # poi.glmm.sim_1 <- glmer(y ~ scale(x1) + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_1 <- glmer(y ~ scale(x1) + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_1 <- glmer(y ~ scale(x1) + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_1 <- glmer(y ~ scale(x1) + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x2 only
  # poi.glmm.sim_2 <- glmer(y ~ x2 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_2 <- glmer(y ~ x2 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_2 <- glmer(y ~ x2 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_2 <- glmer(y ~ x2 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x3 only
  # poi.glmm.sim_3 <- glmer(y ~ x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4 only
  # poi.glmm.sim_4 <- glmer(y ~ x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_4 <- glmer(y ~ x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_4 <- glmer(y ~ x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_4 <- glmer(y ~ x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x5 only
  # poi.glmm.sim_5 <- glmer(y ~ x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_5 <- glmer(y ~ x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_5 <- glmer(y ~ x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_5 <- glmer(y ~ x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1 and x2 only
  # poi.glmm.sim_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1 and x3 only
  # poi.glmm.sim_13 <- glmer(y ~ scale(x1) + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1 and x4 only
  # poi.glmm.sim_14 <- glmer(y ~ scale(x1) + x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_14 <- glmer(y ~ scale(x1) + x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_14 <- glmer(y ~ scale(x1) + x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_14 <- glmer(y ~ scale(x1) + x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1 and x5 only
  # poi.glmm.sim_15 <- glmer(y ~ scale(x1) + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_15 <- glmer(y ~ scale(x1) + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_15 <- glmer(y ~ scale(x1) + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_15 <- glmer(y ~ scale(x1) + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x2 and x3 only
  # poi.glmm.sim_23 <- glmer(y ~ x2 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x2 and x4 only
  # poi.glmm.sim_24 <- glmer(y ~ x2 + x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_24 <- glmer(y ~ x2 + x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_24 <- glmer(y ~ x2 + x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_24 <- glmer(y ~ x2 + x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x2 and x5 only
  # poi.glmm.sim_25 <- glmer(y ~ x2 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_25 <- glmer(y ~ x2 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_25 <- glmer(y ~ x2 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_25 <- glmer(y ~ x2 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4 and x3 only
  # poi.glmm.sim_34 <- glmer(y ~ x4 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_34 <- glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_34 <- glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_34 <- glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x5 and x3 only
  # poi.glmm.sim_35 <- glmer(y ~ x5 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_35 <- glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_35 <- glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_35 <- glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4 and x5 only
  # poi.glmm.sim_45 <- glmer(y ~ x4 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_45 <- glmer(y ~ x4 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_45 <- glmer(y ~ x4 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_45 <- glmer(y ~ x4 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x2, and x3 variables 
  # poi.glmm.sim_123 <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_123 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_123 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_123 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using all x1, x2, and x4 variables 
  # poi.glmm.sim_124 <- glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_124 <- glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_124 <- glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_124 <- glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using all x1, x2, and x5 variables 
  # poi.glmm.sim_125 <- glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_125 <- glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_125 <- glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_125 <- glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x4, and x3 variables 
  # poi.glmm.sim_134 <- glmer(y ~ scale(x1) + x4 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_134 <- glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_134 <- glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_134 <- glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x5, and x3 variables 
  # poi.glmm.sim_135 <- glmer(y ~ scale(x1) + x5 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_135 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_135 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_135 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using all x1, x4, and x5 variables 
  # poi.glmm.sim_145 <- glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_145 <- glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_145 <- glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_145 <- glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4, x2, and x3 variables 
  # poi.glmm.sim_234 <- glmer(y ~ x4 + x2 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_234 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_234 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_234 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x5, x2, and x3 variables 
  # poi.glmm.sim_235 <- glmer(y ~ x5 + x2 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_235 <- glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_235 <- glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_235 <- glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x2, x4, and x5 variables 
  # poi.glmm.sim_245 <- glmer(y ~ x2 + x4 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_245 <- glmer(y ~ x2 + x4 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_245 <- glmer(y ~ x2 + x4 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_245 <- glmer(y ~ x2 + x4 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4, x5, and x3 variables 
  # poi.glmm.sim_345 <- glmer(y ~ x4 + x5 + x3 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_345 <- glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_345 <- glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_345 <- glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x2, x3, x4 variables 
  # poi.glmm.sim_1234 <- glmer(y ~ scale(x1) + x2 + x3 + x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_1234 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_1234 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_1234 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x2, x3, x5 variables 
  # poi.glmm.sim_1235 <- glmer(y ~ scale(x1) + x2 + x3 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_1235 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_1235 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_1235 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x2, x4, x5 variables 
  # poi.glmm.sim_1245 <- glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_1245 <- glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_1245 <- glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_1245 <- glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x1, x5, x3, x4 variables 
  # poi.glmm.sim_1345 <- glmer(y ~ scale(x1) + x5 + x3 + x4 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_1345 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_1345 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_1345 <- glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using x4, x2, x3, x5 variables 
  # poi.glmm.sim_2345 <- glmer(y ~ x4 + x2 + x3 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_2345 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_2345 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_2345 <- glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Estimate a Poisson mixed model using all variables 
  # poi.glmm.sim_all <- glmer(y ~ scale(x1) + x2 + x3 + x4 + x5 + (1|g), data = simdata, family = poisson)
  # poi.glmm.ps2_all <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps2, family = soft_poisson)
  # poi.glmm.ps3_all <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps3, family = soft_poisson)
  # poi.glmm.ps4_all <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps4, family = soft_poisson)

  # # Compute AIC for each model
  # aic_values <- data.frame(modname = c("x1 only", "x2 only", "x3 only", "x4 only", "x5 only", "x1 and x2", "x1 and x3", "x1 and x4", "x1 and x5", "x2 and x3", "x2 and x4", "x2 and x5", "x3 and x4", "x3 and x5", "x4 and x5", "x1, x2, x3", "x1, x2, x4", "x1, x2, x5", "x1, x3, x4", "x1, x3, x5", "x1, x4, x5", "x2, x3, x4", "x2, x3, x5", "x2, x4, x5", "x3, x4, x5", "x1, x2, x3, x4", "x1, x2, x3, x5", "x1, x2, x4, x5", "x1, x3, x4, x5", "x2, x3, x4, x5", "all"),
  #                          sim = c(AIC(poi.glmm.sim_1), AIC(poi.glmm.sim_2), AIC(poi.glmm.sim_3), AIC(poi.glmm.sim_4), AIC(poi.glmm.sim_5), AIC(poi.glmm.sim_12), AIC(poi.glmm.sim_13), AIC(poi.glmm.sim_14), AIC(poi.glmm.sim_15), AIC(poi.glmm.sim_23), AIC(poi.glmm.sim_24), AIC(poi.glmm.sim_25), AIC(poi.glmm.sim_34), AIC(poi.glmm.sim_35), AIC(poi.glmm.sim_45), AIC(poi.glmm.sim_123), AIC(poi.glmm.sim_124), AIC(poi.glmm.sim_125), AIC(poi.glmm.sim_134), AIC(poi.glmm.sim_135), AIC(poi.glmm.sim_145), AIC(poi.glmm.sim_234), AIC(poi.glmm.sim_235), AIC(poi.glmm.sim_245), AIC(poi.glmm.sim_345), AIC(poi.glmm.sim_1234), AIC(poi.glmm.sim_1235), AIC(poi.glmm.sim_1245), AIC(poi.glmm.sim_1345), AIC(poi.glmm.sim_2345), AIC(poi.glmm.sim_all)),
  #                          ps2 = c(AIC(poi.glmm.ps2_1), AIC(poi.glmm.ps2_2), AIC(poi.glmm.ps2_3), AIC(poi.glmm.ps2_4), AIC(poi.glmm.ps2_5), AIC(poi.glmm.ps2_12), AIC(poi.glmm.ps2_13), AIC(poi.glmm.ps2_14), AIC(poi.glmm.ps2_15), AIC(poi.glmm.ps2_23), AIC(poi.glmm.ps2_24), AIC(poi.glmm.ps2_25), AIC(poi.glmm.ps2_34), AIC(poi.glmm.ps2_35), AIC(poi.glmm.ps2_45), AIC(poi.glmm.ps2_123), AIC(poi.glmm.ps2_124), AIC(poi.glmm.ps2_125), AIC(poi.glmm.ps2_134), AIC(poi.glmm.ps2_135), AIC(poi.glmm.ps2_145), AIC(poi.glmm.ps2_234), AIC(poi.glmm.ps2_235), AIC(poi.glmm.ps2_245), AIC(poi.glmm.ps2_345), AIC(poi.glmm.ps2_1234), AIC(poi.glmm.ps2_1235), AIC(poi.glmm.ps2_1245), AIC(poi.glmm.ps2_1345), AIC(poi.glmm.ps2_2345), AIC(poi.glmm.ps2_all)),
  #                          ps3 = c(AIC(poi.glmm.ps3_1), AIC(poi.glmm.ps3_2), AIC(poi.glmm.ps3_3), AIC(poi.glmm.ps3_4), AIC(poi.glmm.ps3_5), AIC(poi.glmm.ps3_12), AIC(poi.glmm.ps3_13), AIC(poi.glmm.ps3_14), AIC(poi.glmm.ps3_15), AIC(poi.glmm.ps3_23), AIC(poi.glmm.ps3_24), AIC(poi.glmm.ps3_25), AIC(poi.glmm.ps3_34), AIC(poi.glmm.ps3_35), AIC(poi.glmm.ps3_45), AIC(poi.glmm.ps3_123), AIC(poi.glmm.ps3_124), AIC(poi.glmm.ps3_125), AIC(poi.glmm.ps3_134), AIC(poi.glmm.ps3_135), AIC(poi.glmm.ps3_145), AIC(poi.glmm.ps3_234), AIC(poi.glmm.ps3_235), AIC(poi.glmm.ps3_245), AIC(poi.glmm.ps3_345), AIC(poi.glmm.ps3_1234), AIC(poi.glmm.ps3_1235), AIC(poi.glmm.ps3_1245), AIC(poi.glmm.ps3_1345), AIC(poi.glmm.ps3_2345), AIC(poi.glmm.ps3_all)),
  #                          ps4 = c(AIC(poi.glmm.ps4_1), AIC(poi.glmm.ps4_2), AIC(poi.glmm.ps4_3), AIC(poi.glmm.ps4_4), AIC(poi.glmm.ps4_5), AIC(poi.glmm.ps4_12), AIC(poi.glmm.ps4_13), AIC(poi.glmm.ps4_14), AIC(poi.glmm.ps4_15), AIC(poi.glmm.ps4_23), AIC(poi.glmm.ps4_24), AIC(poi.glmm.ps4_25), AIC(poi.glmm.ps4_34), AIC(poi.glmm.ps4_35), AIC(poi.glmm.ps4_45), AIC(poi.glmm.ps4_123), AIC(poi.glmm.ps4_124), AIC(poi.glmm.ps4_125), AIC(poi.glmm.ps4_134), AIC(poi.glmm.ps4_135), AIC(poi.glmm.ps4_145), AIC(poi.glmm.ps4_234), AIC(poi.glmm.ps4_235), AIC(poi.glmm.ps4_245), AIC(poi.glmm.ps4_345), AIC(poi.glmm.ps4_1234), AIC(poi.glmm.ps4_1235), AIC(poi.glmm.ps4_1245), AIC(poi.glmm.ps4_1345), AIC(poi.glmm.ps4_2345), AIC(poi.glmm.ps4_all))
  #                          )
  cat('Finished iter:', iter, 'm:', m, 'uniform_cluster_size:', uniform_cluster_size, '\n')
  cat('Best model for sim:', aic_values$modname[which.min(aic_values$sim)], '\n')
  cat('Best model for ps2:', aic_values$modname[which.min(aic_values$ps2)], '\n')
  cat('Best model for ps3:', aic_values$modname[which.min(aic_values$ps3)], '\n')
  cat('Best model for ps4:', aic_values$modname[which.min(aic_values$ps4)], '\n')
  c(iter, m, uniform_cluster_size,
    aic_values$modname[which.min(aic_values$sim)],
    aic_values$modname[which.min(aic_values$ps2)],
    aic_values$modname[which.min(aic_values$ps3)],
    aic_values$modname[which.min(aic_values$ps4)])
})

mod.selection.df <- as.data.frame(t(mod.selection.df))
names(mod.selection.df) <- c('iter', 'm', 'uniform_cluster_size', 'sim', 'ps2', 'ps3', 'ps4')
# save(mod.selection.df, file = file.path('SIMULATION', 'intermediate_results', 'poisson', 'model_selected.RData'))

# Proportion of times the correct model (x1, x2, x3) is selected for each method and sample size
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 30, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 30, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 30, 'ps4'] == 'x1, x2, x3')

mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 50, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 50, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 50, 'ps4'] == 'x1, x2, x3')

mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 100, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 100, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 100, 'ps4'] == 'x1, x2, x3')


mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 30, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 30, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 30, 'ps4'] == 'x1, x2, x3')

mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 50, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 50, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 50, 'ps4'] == 'x1, x2, x3')

mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 100, 'ps2'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 100, 'ps3'] == 'x1, x2, x3')
mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == 'x1, x2, x3' & mod.selection.df[mod.selection.df$m == 100, 'ps4'] == 'x1, x2, x3')

# Proportion of times the same model is selected by sim and each ps method for each sample size
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == mod.selection.df[mod.selection.df$m == 30, 'ps2'])
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == mod.selection.df[mod.selection.df$m == 30, 'ps3'])
mean(mod.selection.df[mod.selection.df$m == 30, 'sim'] == mod.selection.df[mod.selection.df$m == 30, 'ps4'])

mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == mod.selection.df[mod.selection.df$m == 50, 'ps2'])
mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == mod.selection.df[mod.selection.df$m == 50, 'ps3'])
mean(mod.selection.df[mod.selection.df$m == 50, 'sim'] == mod.selection.df[mod.selection.df$m == 50, 'ps4'])

mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == mod.selection.df[mod.selection.df$m == 100, 'ps2'])
mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == mod.selection.df[mod.selection.df$m == 100, 'ps3'])
mean(mod.selection.df[mod.selection.df$m == 100, 'sim'] == mod.selection.df[mod.selection.df$m == 100, 'ps4'])

# 1. Transform the data to long format
plot_data <- mod.selection.df %>%
  pivot_longer(
    cols = c(sim, ps2, ps3, ps4),
    names_to = "method",
    values_to = "selected_model"
  ) %>%
  # Ensure method is an ordered factor to control horizontal order
  mutate(method = factor(method, levels = c("ps2", "ps3", "ps4", "sim")))
  # mutate(method = factor(method, levels = c("sim", "ps2", "ps3", "ps4")))

# 2. Generate the faceted tile plot
ggplot(plot_data[plot_data$m == 30, ], aes(x = method, y = iter, fill = selected_model)) +
  geom_tile() +
  # Facet by m (rows) and cluster size (columns)
  facet_grid(m ~ uniform_cluster_size, labeller = label_both) +
  # Use a qualitative color palette for different models
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() +
  labs(
    title = "Comparison of Model Selection across 200 Iterations",
    x = "Method",
    y = "Iteration",
    fill = "Selected Model"
  ) +
  theme(
    # Since there are 200 iterations, we might want to hide or shrink Y labels
    axis.text.y = element_text(size = 6), 
    panel.grid = element_blank(),
    legend.position = "bottom"
  )
