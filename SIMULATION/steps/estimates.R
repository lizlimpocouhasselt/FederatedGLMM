# Empty the environment
rm(list = ls(all = TRUE))

library(lme4)
library(dplyr)
library(tidyr)
source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'R_common', 'soft_poisson.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_run_rows.R'))

lme4.env <- getNamespace("lme4")
newhasNoScale <- function(family) {
  any(substr(family$family, 1L, 16L) == c("poisson", "binomial", "negative.bin", "Negative Bin", "soft_poisson"))
}
unlockBinding("hasNoScale", lme4.env)
assign("hasNoScale", newhasNoScale, envir = lme4.env)
lockBinding("hasNoScale", lme4.env)

run_by_row(
  step = 'estimates',
  inputs = list(
    simdata = list(dir = 'simdata', prefix = 'simdata'),
    pseudodata_2ndmom = list(dir = 'ps2', prefix = 'pseudodata_2ndmom'),
    pseudodata_3rdmom = list(dir = 'ps3', prefix = 'pseudodata_3rdmom'),
    pseudodata_4thmom = list(dir = 'ps4', prefix = 'pseudodata_4thmom')
  ),
  outputs = list(
    point_estimate = list(dir = 'point_estimates', prefix = 'point_estimate'),
    interval_estimate = list(dir = 'interval_estimates', prefix = 'interval_estimate')
  ),
  worker = function(row, env) {
    pseudodata_2ndmom <- bind_rows(env$pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    pseudodata_3rdmom <- bind_rows(env$pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    pseudodata_4thmom <- bind_rows(env$pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

    set.seed(row$seed)
    mod.result.sim <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), env$simdata, family = poisson)
    mod.result.ps2 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_2ndmom, family = soft_poisson)
    mod.result.ps3 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_3rdmom, family = soft_poisson)
    mod.result.ps4 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), pseudodata_4thmom, family = soft_poisson)

    confint.result.sim <- confint(mod.result.sim)
    confint.result.ps2 <- confint(mod.result.ps2)
    confint.result.ps3 <- confint(mod.result.ps3)
    confint.result.ps4 <- confint(mod.result.ps4)

    point_estimate <- data.frame(
      true = c(0.4811, 2.285513, -0.302612, 0.087566, -0.959036, -0.812642, -0.809044, -0.794925),
      sim = if (!is.null(mod.result.sim)) c(as.data.frame(VarCorr(mod.result.sim))$sdcor, summary(mod.result.sim)$coefficients['(Intercept)', 'Estimate'], summary(mod.result.sim)$coefficients['scale(x1)', 'Estimate'], summary(mod.result.sim)$coefficients['x2', 'Estimate'], summary(mod.result.sim)$coefficients['x32', 'Estimate'], summary(mod.result.sim)$coefficients['x33', 'Estimate'], summary(mod.result.sim)$coefficients['x34', 'Estimate'], summary(mod.result.sim)$coefficients['x35', 'Estimate']) else rep(NA, 8),
      ps2 = if (!is.null(mod.result.ps2)) c(as.data.frame(VarCorr(mod.result.ps2))$sdcor, summary(mod.result.ps2)$coefficients['(Intercept)', 'Estimate'], summary(mod.result.ps2)$coefficients['scale(x1)', 'Estimate'], summary(mod.result.ps2)$coefficients['x2', 'Estimate'], summary(mod.result.ps2)$coefficients['x32', 'Estimate'], summary(mod.result.ps2)$coefficients['x33', 'Estimate'], summary(mod.result.ps2)$coefficients['x34', 'Estimate'], summary(mod.result.ps2)$coefficients['x35', 'Estimate']) else rep(NA, 8),
      ps3 = if (!is.null(mod.result.ps3)) c(as.data.frame(VarCorr(mod.result.ps3))$sdcor, summary(mod.result.ps3)$coefficients['(Intercept)', 'Estimate'], summary(mod.result.ps3)$coefficients['scale(x1)', 'Estimate'], summary(mod.result.ps3)$coefficients['x2', 'Estimate'], summary(mod.result.ps3)$coefficients['x32', 'Estimate'], summary(mod.result.ps3)$coefficients['x33', 'Estimate'], summary(mod.result.ps3)$coefficients['x34', 'Estimate'], summary(mod.result.ps3)$coefficients['x35', 'Estimate']) else rep(NA, 8),
      ps4 = if (!is.null(mod.result.ps4)) c(as.data.frame(VarCorr(mod.result.ps4))$sdcor, summary(mod.result.ps4)$coefficients['(Intercept)', 'Estimate'], summary(mod.result.ps4)$coefficients['scale(x1)', 'Estimate'], summary(mod.result.ps4)$coefficients['x2', 'Estimate'], summary(mod.result.ps4)$coefficients['x32', 'Estimate'], summary(mod.result.ps4)$coefficients['x33', 'Estimate'], summary(mod.result.ps4)$coefficients['x34', 'Estimate'], summary(mod.result.ps4)$coefficients['x35', 'Estimate']) else rep(NA, 8)
    )

    interval_estimate <- list(
      sim = if (!is.null(confint.result.sim)) confint.result.sim else matrix(NA_real_, nrow = 8, ncol = 2),
      ps2 = if (!is.null(confint.result.ps2)) confint.result.ps2 else matrix(NA_real_, nrow = 8, ncol = 2),
      ps3 = if (!is.null(confint.result.ps3)) confint.result.ps3 else matrix(NA_real_, nrow = 8, ncol = 2),
      ps4 = if (!is.null(confint.result.ps4)) confint.result.ps4 else matrix(NA_real_, nrow = 8, ncol = 2)
    )

    list(point_estimate = point_estimate, interval_estimate = interval_estimate)
  }
)
