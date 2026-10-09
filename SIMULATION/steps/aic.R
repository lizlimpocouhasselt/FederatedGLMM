#-------------------------------------------------------------------
# MODEL SELECTION (AIC)
#-------------------------------------------------------------------

rm(list = ls(all = TRUE))

library(lme4)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'R_common', 'soft_poisson.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_run_rows.R'))

par_settings <- read.csv(file.path('SIMULATION', 'par_settings.csv'), stringsAsFactors = FALSE)

lme4.env <- getNamespace("lme4")
newhasNoScale <- function(family) {
  any(substr(family$family, 1L, 16L) == c("poisson", "binomial", "negative.bin", "Negative Bin", "soft_poisson"))
}
unlockBinding("hasNoScale", lme4.env)
assign("hasNoScale", newhasNoScale, envir = lme4.env)
lockBinding("hasNoScale", lme4.env)

run_by_row(
  step = 'aic',
  inputs = list(
    simdata = list(dir = 'simdata_x4_x5', prefix = 'simdata'),
    pseudodata_2ndmom = list(dir = 'ps2_x4_x5', prefix = 'pseudodata_2ndmom'),
    pseudodata_3rdmom = list(dir = 'ps3_x4_x5', prefix = 'pseudodata_3rdmom'),
    pseudodata_4thmom = list(dir = 'ps4_x4_x5', prefix = 'pseudodata_4thmom')
  ),
  outputs = list(
    aic_values = list(dir = 'aic_x4_x5', prefix = 'aic_values')
  ),
  max_iter = 200,
  worker = function(row, env) {
    simdata <- env$simdata
    ps2 <- bind_rows(env$pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    ps3 <- bind_rows(env$pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    ps4 <- bind_rows(env$pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

    poi.glmm.sim_1 <- suppressWarnings(glmer(y ~ scale(x1) + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_1 <- suppressWarnings(glmer(y ~ scale(x1) + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_1 <- suppressWarnings(glmer(y ~ scale(x1) + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_1 <- suppressWarnings(glmer(y ~ scale(x1) + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_2 <- suppressWarnings(glmer(y ~ x2 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_2 <- suppressWarnings(glmer(y ~ x2 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_2 <- suppressWarnings(glmer(y ~ x2 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_2 <- suppressWarnings(glmer(y ~ x2 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_3 <- suppressWarnings(glmer(y ~ x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_3 <- suppressWarnings(glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_3 <- suppressWarnings(glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_3 <- suppressWarnings(glmer(y ~ x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_4 <- suppressWarnings(glmer(y ~ x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_4 <- suppressWarnings(glmer(y ~ x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_4 <- suppressWarnings(glmer(y ~ x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_4 <- suppressWarnings(glmer(y ~ x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_5 <- suppressWarnings(glmer(y ~ x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_5 <- suppressWarnings(glmer(y ~ x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_5 <- suppressWarnings(glmer(y ~ x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_5 <- suppressWarnings(glmer(y ~ x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_12 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_12 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_12 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_12 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_13 <- suppressWarnings(glmer(y ~ scale(x1) + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_13 <- suppressWarnings(glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_13 <- suppressWarnings(glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_13 <- suppressWarnings(glmer(y ~ scale(x1) + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_14 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_14 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_14 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_14 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_15 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_15 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_15 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_15 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_23 <- suppressWarnings(glmer(y ~ x2 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_23 <- suppressWarnings(glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_23 <- suppressWarnings(glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_23 <- suppressWarnings(glmer(y ~ x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_24 <- suppressWarnings(glmer(y ~ x2 + x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_24 <- suppressWarnings(glmer(y ~ x2 + x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_24 <- suppressWarnings(glmer(y ~ x2 + x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_24 <- suppressWarnings(glmer(y ~ x2 + x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_25 <- suppressWarnings(glmer(y ~ x2 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_25 <- suppressWarnings(glmer(y ~ x2 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_25 <- suppressWarnings(glmer(y ~ x2 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_25 <- suppressWarnings(glmer(y ~ x2 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_34 <- suppressWarnings(glmer(y ~ x4 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_34 <- suppressWarnings(glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_34 <- suppressWarnings(glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_34 <- suppressWarnings(glmer(y ~ x4 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_35 <- suppressWarnings(glmer(y ~ x5 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_35 <- suppressWarnings(glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_35 <- suppressWarnings(glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_35 <- suppressWarnings(glmer(y ~ x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_45 <- suppressWarnings(glmer(y ~ x4 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_45 <- suppressWarnings(glmer(y ~ x4 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_45 <- suppressWarnings(glmer(y ~ x4 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_45 <- suppressWarnings(glmer(y ~ x4 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_123 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_123 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_123 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_123 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_124 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_124 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_124 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_124 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_125 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_125 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_125 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_125 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_134 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_134 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_134 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_134 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_135 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_135 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_135 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_135 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_145 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_145 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_145 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_145 <- suppressWarnings(glmer(y ~ scale(x1) + x4 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_234 <- suppressWarnings(glmer(y ~ x4 + x2 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_234 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_234 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_234 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_235 <- suppressWarnings(glmer(y ~ x5 + x2 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_235 <- suppressWarnings(glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_235 <- suppressWarnings(glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_235 <- suppressWarnings(glmer(y ~ x5 + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_245 <- suppressWarnings(glmer(y ~ x2 + x4 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_245 <- suppressWarnings(glmer(y ~ x2 + x4 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_245 <- suppressWarnings(glmer(y ~ x2 + x4 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_245 <- suppressWarnings(glmer(y ~ x2 + x4 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_345 <- suppressWarnings(glmer(y ~ x4 + x5 + x3 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_345 <- suppressWarnings(glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_345 <- suppressWarnings(glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_345 <- suppressWarnings(glmer(y ~ x4 + x5 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_1234 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x3 + x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_1234 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_1234 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_1234 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_1235 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x3 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_1235 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_1235 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_1235 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_1245 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_1245 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_1245 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_1245 <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x4 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_1345 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x3 + x4 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_1345 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_1345 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_1345 <- suppressWarnings(glmer(y ~ scale(x1) + x5 + x32 + x33 + x34 + x35 + x4 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_2345 <- suppressWarnings(glmer(y ~ x4 + x2 + x3 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_2345 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_2345 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_2345 <- suppressWarnings(glmer(y ~ x4 + x2 + x32 + x33 + x34 + x35 + x5 + (1|g), data = ps4, family = soft_poisson))

    poi.glmm.sim_all <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x3 + x4 + x5 + (1|g), data = simdata, family = poisson))
    poi.glmm.ps2_all <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps2, family = soft_poisson))
    poi.glmm.ps3_all <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps3, family = soft_poisson))
    poi.glmm.ps4_all <- suppressWarnings(glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + x4 + x5 + (1|g), data = ps4, family = soft_poisson))

    aic_values <- data.frame(
      modname = c('x1 only', 'x2 only', 'x3 only', 'x4 only', 'x5 only', 'x1 and x2', 'x1 and x3', 'x1 and x4', 'x1 and x5', 'x2 and x3', 'x2 and x4', 'x2 and x5', 'x3 and x4', 'x3 and x5', 'x4 and x5', 'x1, x2, x3', 'x1, x2, x4', 'x1, x2, x5', 'x1, x3, x4', 'x1, x3, x5', 'x1, x4, x5', 'x2, x3, x4', 'x2, x3, x5', 'x2, x4, x5', 'x3, x4, x5', 'x1, x2, x3, x4', 'x1, x2, x3, x5', 'x1, x2, x4, x5', 'x1, x3, x4, x5', 'x2, x3, x4, x5', 'all'),
      sim = c(AIC(poi.glmm.sim_1), AIC(poi.glmm.sim_2), AIC(poi.glmm.sim_3), AIC(poi.glmm.sim_4), AIC(poi.glmm.sim_5), AIC(poi.glmm.sim_12), AIC(poi.glmm.sim_13), AIC(poi.glmm.sim_14), AIC(poi.glmm.sim_15), AIC(poi.glmm.sim_23), AIC(poi.glmm.sim_24), AIC(poi.glmm.sim_25), AIC(poi.glmm.sim_34), AIC(poi.glmm.sim_35), AIC(poi.glmm.sim_45), AIC(poi.glmm.sim_123), AIC(poi.glmm.sim_124), AIC(poi.glmm.sim_125), AIC(poi.glmm.sim_134), AIC(poi.glmm.sim_135), AIC(poi.glmm.sim_145), AIC(poi.glmm.sim_234), AIC(poi.glmm.sim_235), AIC(poi.glmm.sim_245), AIC(poi.glmm.sim_345), AIC(poi.glmm.sim_1234), AIC(poi.glmm.sim_1235), AIC(poi.glmm.sim_1245), AIC(poi.glmm.sim_1345), AIC(poi.glmm.sim_2345), AIC(poi.glmm.sim_all)),
      ps2 = c(AIC(poi.glmm.ps2_1), AIC(poi.glmm.ps2_2), AIC(poi.glmm.ps2_3), AIC(poi.glmm.ps2_4), AIC(poi.glmm.ps2_5), AIC(poi.glmm.ps2_12), AIC(poi.glmm.ps2_13), AIC(poi.glmm.ps2_14), AIC(poi.glmm.ps2_15), AIC(poi.glmm.ps2_23), AIC(poi.glmm.ps2_24), AIC(poi.glmm.ps2_25), AIC(poi.glmm.ps2_34), AIC(poi.glmm.ps2_35), AIC(poi.glmm.ps2_45), AIC(poi.glmm.ps2_123), AIC(poi.glmm.ps2_124), AIC(poi.glmm.ps2_125), AIC(poi.glmm.ps2_134), AIC(poi.glmm.ps2_135), AIC(poi.glmm.ps2_145), AIC(poi.glmm.ps2_234), AIC(poi.glmm.ps2_235), AIC(poi.glmm.ps2_245), AIC(poi.glmm.ps2_345), AIC(poi.glmm.ps2_1234), AIC(poi.glmm.ps2_1235), AIC(poi.glmm.ps2_1245), AIC(poi.glmm.ps2_1345), AIC(poi.glmm.ps2_2345), AIC(poi.glmm.ps2_all)),
      ps3 = c(AIC(poi.glmm.ps3_1), AIC(poi.glmm.ps3_2), AIC(poi.glmm.ps3_3), AIC(poi.glmm.ps3_4), AIC(poi.glmm.ps3_5), AIC(poi.glmm.ps3_12), AIC(poi.glmm.ps3_13), AIC(poi.glmm.ps3_14), AIC(poi.glmm.ps3_15), AIC(poi.glmm.ps3_23), AIC(poi.glmm.ps3_24), AIC(poi.glmm.ps3_25), AIC(poi.glmm.ps3_34), AIC(poi.glmm.ps3_35), AIC(poi.glmm.ps3_45), AIC(poi.glmm.ps3_123), AIC(poi.glmm.ps3_124), AIC(poi.glmm.ps3_125), AIC(poi.glmm.ps3_134), AIC(poi.glmm.ps3_135), AIC(poi.glmm.ps3_145), AIC(poi.glmm.ps3_234), AIC(poi.glmm.ps3_235), AIC(poi.glmm.ps3_245), AIC(poi.glmm.ps3_345), AIC(poi.glmm.ps3_1234), AIC(poi.glmm.ps3_1235), AIC(poi.glmm.ps3_1245), AIC(poi.glmm.ps3_1345), AIC(poi.glmm.ps3_2345), AIC(poi.glmm.ps3_all)),
      ps4 = c(AIC(poi.glmm.ps4_1), AIC(poi.glmm.ps4_2), AIC(poi.glmm.ps4_3), AIC(poi.glmm.ps4_4), AIC(poi.glmm.ps4_5), AIC(poi.glmm.ps4_12), AIC(poi.glmm.ps4_13), AIC(poi.glmm.ps4_14), AIC(poi.glmm.ps4_15), AIC(poi.glmm.ps4_23), AIC(poi.glmm.ps4_24), AIC(poi.glmm.ps4_25), AIC(poi.glmm.ps4_34), AIC(poi.glmm.ps4_35), AIC(poi.glmm.ps4_45), AIC(poi.glmm.ps4_123), AIC(poi.glmm.ps4_124), AIC(poi.glmm.ps4_125), AIC(poi.glmm.ps4_134), AIC(poi.glmm.ps4_135), AIC(poi.glmm.ps4_145), AIC(poi.glmm.ps4_234), AIC(poi.glmm.ps4_235), AIC(poi.glmm.ps4_245), AIC(poi.glmm.ps4_345), AIC(poi.glmm.ps4_1234), AIC(poi.glmm.ps4_1235), AIC(poi.glmm.ps4_1245), AIC(poi.glmm.ps4_1345), AIC(poi.glmm.ps4_2345), AIC(poi.glmm.ps4_all)),
      stringsAsFactors = FALSE
    )

    list(aic_values = aic_values)
  }
)

remote_dir <- file.path("SIMULATION", "intermediate_results", "poisson", "aic_x4_x5")
remote_files <- remote_ls(remote_dir)
expected_files <- with(par_settings[par_settings$iter <= 200, , drop = FALSE], sprintf("aic_values_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size))
missing_files <- setdiff(expected_files, remote_files)
if (length(missing_files) > 0L) {
  stop("Missing AIC files in remote directory: ", paste(missing_files, collapse = ", "))
}

local_dir <- tempfile("aic_local_")
dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
remote_pull(remote_dir, expected_files, local_dir)

selected_rows <- par_settings[par_settings$iter <= 200, , drop = FALSE]
mod.selection.df <- do.call(rbind, lapply(seq_len(nrow(selected_rows)), function(row_num) {
  row <- selected_rows[row_num, , drop = FALSE]
  local_file <- file.path(local_dir, sprintf("aic_values_%04d_%04d_%04d.RData", row$iter, row$m, row$uniform_cluster_size))
  if (!file.exists(local_file)) {
    stop("Missing local AIC file: ", local_file)
  }
  load(local_file)
  if (!exists("aic_values", inherits = FALSE)) {
    stop("Loaded AIC file does not contain the expected aic_values object: ", local_file)
  }
  data.frame(
    iter = row$iter,
    m = row$m,
    uniform_cluster_size = row$uniform_cluster_size,
    sim = aic_values$modname[which.min(aic_values$sim)],
    ps2 = aic_values$modname[which.min(aic_values$ps2)],
    ps3 = aic_values$modname[which.min(aic_values$ps3)],
    ps4 = aic_values$modname[which.min(aic_values$ps4)],
    stringsAsFactors = FALSE
  )
}))

remote_save(mod.selection.df, key = file.path("SIMULATION", "intermediate_results", "poisson", "model_selected.RData"))

unlink(local_dir, recursive = TRUE, force = TRUE)

summary_table <- mod.selection.df %>%
  group_by(m) %>%
  summarise(
    iterations = n(),
    sim_x123_pct = round(mean(sim == "x1, x2, x3") * 100, 1),
    ps2_x123_pct = round(mean(ps2 == "x1, x2, x3") * 100, 1),
    ps3_x123_pct = round(mean(ps3 == "x1, x2, x3") * 100, 1),
    ps4_x123_pct = round(mean(ps4 == "x1, x2, x3") * 100, 1),
    ps2_same_as_sim_pct = round(mean(ps2 == sim) * 100, 1),
    ps3_same_as_sim_pct = round(mean(ps3 == sim) * 100, 1),
    ps4_same_as_sim_pct = round(mean(ps4 == sim) * 100, 1),
    .groups = "drop"
  )

print(summary_table)
