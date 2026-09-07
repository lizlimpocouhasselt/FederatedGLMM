# Empty the environment
rm(list=ls(all=TRUE))

# Load libraries / functions
library(lme4)
library(dplyr)
library(tidyr)
source(file.path(getwd(), "SIMULATION", "scripts", "soft_poisson.R"))

par_settings <- read.csv(file.path(getwd(), 'SIMULATION', 'par_settings.csv'))

lapply(1:nrow(par_settings), function(row){
  iter <- par_settings$iter[row]
  m <- par_settings$m[row]
  uniform_cluster_size <- par_settings$uniform_cluster_size[row]
  seed <- par_settings$seed[row]

  # Load data
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'simdata', sprintf('simdata_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'ps2', sprintf('pseudodata_2ndmom_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'ps3', sprintf('pseudodata_3rdmom_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
  load(file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'ps4', sprintf('pseudodata_4thmom_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
  pseudodata_2ndmom <- bind_rows(pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  pseudodata_3rdmom <- bind_rows(pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  pseudodata_4thmom <- bind_rows(pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

  # Redefine glm families with no scale
  lme4.env <- getNamespace("lme4")
  newhasNoScale <- function (family) {
       any(substr(family$family, 1L, 16L) == c("poisson", "binomial", 
                                             "negative.bin", "Negative Bin", "soft_poisson"))
  }
  unlockBinding("hasNoScale", lme4.env)
  assign("hasNoScale", newhasNoScale, envir = lme4.env)
  lockBinding("hasNoScale", lme4.env)

  set.seed(seed)
  mod.result.sim <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), simdata, family = poisson)
  mod.result.ps2 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_2ndmom, family = soft_poisson)
  mod.result.ps3 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_3rdmom, family = soft_poisson)
  mod.result.ps4 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_4thmom, family = soft_poisson)
  confint.result.sim <- confint(mod.result.sim)
  confint.result.ps2 <- confint(mod.result.ps2)
  confint.result.ps3 <- confint(mod.result.ps3)
  confint.result.ps4 <- confint(mod.result.ps4)

  point_estimate <- data.frame(true = c(0.4811, 2.285513, -0.302612, 0.087566, -0.959036, -0.812642, -0.809044, -0.794925),
                             sim = if(!is.null(mod.result.sim)){c(as.data.frame(VarCorr(mod.result.sim))$sdcor,
                                     summary(mod.result.sim)$coefficients['(Intercept)','Estimate'],
                                     summary(mod.result.sim)$coefficients['scale(x1)','Estimate'],
                                     summary(mod.result.sim)$coefficients['x2','Estimate'],
                                     summary(mod.result.sim)$coefficients['x32','Estimate'],
                                     summary(mod.result.sim)$coefficients['x33','Estimate'],
                                     summary(mod.result.sim)$coefficients['x34','Estimate'],
                                     summary(mod.result.sim)$coefficients['x35','Estimate'])} else{rep(NA, 8)},
                             ps2 = if(!is.null(mod.result.ps2)){c(as.data.frame(VarCorr(mod.result.ps2))$sdcor,
                                     summary(mod.result.ps2)$coefficients['(Intercept)','Estimate'],
                                     summary(mod.result.ps2)$coefficients['scale(x1)','Estimate'],
                                     summary(mod.result.ps2)$coefficients['x2','Estimate'],
                                     summary(mod.result.ps2)$coefficients['x32','Estimate'],
                                     summary(mod.result.ps2)$coefficients['x33','Estimate'],
                                     summary(mod.result.ps2)$coefficients['x34','Estimate'],
                                     summary(mod.result.ps2)$coefficients['x35','Estimate'])} else{rep(NA, 8)},
                             ps3 = if(!is.null(mod.result.ps3)){c(as.data.frame(VarCorr(mod.result.ps3))$sdcor,
                                     summary(mod.result.ps3)$coefficients['(Intercept)','Estimate'],
                                     summary(mod.result.ps3)$coefficients['scale(x1)','Estimate'],
                                     summary(mod.result.ps3)$coefficients['x2','Estimate'],
                                     summary(mod.result.ps3)$coefficients['x32','Estimate'],
                                     summary(mod.result.ps3)$coefficients['x33','Estimate'],
                                     summary(mod.result.ps3)$coefficients['x34','Estimate'],
                                     summary(mod.result.ps3)$coefficients['x35','Estimate'])} else{rep(NA, 8)},
                             ps4 = if(!is.null(mod.result.ps4)){c(as.data.frame(VarCorr(mod.result.ps4))$sdcor,
                                     summary(mod.result.ps4)$coefficients['(Intercept)','Estimate'],
                                     summary(mod.result.ps4)$coefficients['scale(x1)','Estimate'],
                                     summary(mod.result.ps4)$coefficients['x2','Estimate'],
                                     summary(mod.result.ps4)$coefficients['x32','Estimate'],
                                     summary(mod.result.ps4)$coefficients['x33','Estimate'],
                                     summary(mod.result.ps4)$coefficients['x34','Estimate'],
                                     summary(mod.result.ps4)$coefficients['x35','Estimate'])} else{rep(NA, 8)}
                                     )
  filename_save <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'point_estimates', sprintf("point_estimate_%04d_%04d_%04d", iter, m, uniform_cluster_size))
  save(point_estimate, file = sprintf("%s.RData", filename_save))

  interval_estimate <- list(sim = if(!is.null(confint.result.sim)){confint.result.sim} else{matrix(NA, nrow = 8, ncol = 2)},
                           ps2 = if(!is.null(confint.result.ps2)){confint.result.ps2} else{matrix(NA, nrow = 8, ncol = 2)},
                           ps3 = if(!is.null(confint.result.ps3)){confint.result.ps3} else{matrix(NA, nrow = 8, ncol = 2)},
                           ps4 = if(!is.null(confint.result.ps4)){confint.result.ps4} else{matrix(NA, nrow = 8, ncol = 2)}
                           )
  filename_save <- file.path(getwd(), 'SIMULATION', 'intermediate_results', 'poisson', 'interval_estimates', sprintf("interval_estimate_%04d_%04d_%04d", iter, m, uniform_cluster_size))
  save(interval_estimate, file = sprintf("%s.RData", filename_save))
})
