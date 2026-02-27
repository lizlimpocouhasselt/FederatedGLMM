#-------------------------------------------------------------------
# MODEL PREDICTION
#-------------------------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE))

# Load packages
library(lme4)

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
  load(file.path("SIMULATION", "intermediate_results", "poisson", "simdata", sprintf("simdata_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  load(file.path("SIMULATION", "intermediate_results", "poisson", "ps2", sprintf("pseudodata_2ndmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  ps2 <- bind_rows(pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  load(file.path("SIMULATION", "intermediate_results", "poisson", "ps3", sprintf("pseudodata_3rdmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  ps3 <- bind_rows(pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  load(file.path("SIMULATION", "intermediate_results", "poisson", "ps4", sprintf("pseudodata_4thmom_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
  ps4 <- bind_rows(pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

  # Estimate a Poisson mixed model using x1 only
  poi.glmm.sim_1 <- glmer(y ~ scale(x1) + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_1 <- glmer(y ~ scale(x1) + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_1 <- glmer(y ~ scale(x1) + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_1 <- glmer(y ~ scale(x1) + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using x2 only
  poi.glmm.sim_2 <- glmer(y ~ x2 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_2 <- glmer(y ~ x2 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_2 <- glmer(y ~ x2 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_2 <- glmer(y ~ x2 + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using x3 only
  poi.glmm.sim_3 <- glmer(y ~ x3 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_3 <- glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using x1 and x2 only
  poi.glmm.sim_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_12 <- glmer(y ~ scale(x1) + x2 + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using x1 and x3 only
  poi.glmm.sim_13 <- glmer(y ~ scale(x1) + x3 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_13 <- glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using x2 and x3 only
  poi.glmm.sim_23 <- glmer(y ~ x2 + x3 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4_23 <- glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # Estimate a Poisson mixed model using all 3 variables 
  poi.glmm.sim <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # Compute AIC for each model
  aic_values <- data.frame(modname = c("x1 only", "x2 only", "x3 only", "x1 and x2", "x1 and x3", "x2 and x3", "all"),
                           sim = c(AIC(poi.glmm.sim_1), AIC(poi.glmm.sim_2), AIC(poi.glmm.sim_3), AIC(poi.glmm.sim_12), AIC(poi.glmm.sim_13), AIC(poi.glmm.sim_23), AIC(poi.glmm.sim)),
                           ps2 = c(AIC(poi.glmm.ps2_1), AIC(poi.glmm.ps2_2), AIC(poi.glmm.ps2_3), AIC(poi.glmm.ps2_12), AIC(poi.glmm.ps2_13), AIC(poi.glmm.ps2_23), AIC(poi.glmm.ps2)),
                           ps3 = c(AIC(poi.glmm.ps3_1), AIC(poi.glmm.ps3_2), AIC(poi.glmm.ps3_3), AIC(poi.glmm.ps3_12), AIC(poi.glmm.ps3_13), AIC(poi.glmm.ps3_23), AIC(poi.glmm.ps3)),
                           ps4 = c(AIC(poi.glmm.ps4_1), AIC(poi.glmm.ps4_2), AIC(poi.glmm.ps4_3), AIC(poi.glmm.ps4_12), AIC(poi.glmm.ps4_13), AIC(poi.glmm.ps4_23), AIC(poi.glmm.ps4)))
  
  cat('Iter ', iter, ' m = ', m, ' uniform_cluster_size = ', uniform_cluster_size, '\n')
  sapply(c('sim', 'ps2', 'ps3', 'ps4'), function(dat) aic_values$modname[which.min(aic_values[, dat])])
})

mod.selection.df <- t(mod.selection.df)
save(mod.selection.df, file = file.path('SIMULATION', 'intermediate_results', 'poisson', 'model_selected.RData'))
sum(mod.selection.df[, 'sim'] == 'all')
sum(mod.selection.df[, 'ps2'] == 'all')
sum(mod.selection.df[, 'ps3'] == 'all')
sum(mod.selection.df[, 'ps4'] == 'all')
