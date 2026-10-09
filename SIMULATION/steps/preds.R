#-------------------------------------------------------------------
# MODEL PREDICTION
#-------------------------------------------------------------------

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
  step = 'preds',
  inputs = list(
    simdata = list(dir = 'simdata', prefix = 'simdata'),
    pseudodata_2ndmom = list(dir = 'ps2', prefix = 'pseudodata_2ndmom'),
    pseudodata_3rdmom = list(dir = 'ps3', prefix = 'pseudodata_3rdmom'),
    pseudodata_4thmom = list(dir = 'ps4', prefix = 'pseudodata_4thmom')
  ),
  outputs = list(
    poi.predictions = list(dir = 'preds', prefix = 'preds')
  ),
  worker = function(row, env) {
    ps2 <- bind_rows(env$pseudodata_2ndmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    ps3 <- bind_rows(env$pseudodata_3rdmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
    ps4 <- bind_rows(env$pseudodata_4thmom) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

    poi.glmm.sim <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), data = env$simdata, family = poisson)
    poi.glmm.ps2 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
    poi.glmm.ps3 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
    poi.glmm.ps4 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

    newdata <- cbind(x1 = env$simdata$x1, model.matrix(poi.glmm.sim)[, -(1:2)], env$simdata[, c('y', 'g')])
    pred.sim <- predict(poi.glmm.sim, newdata = env$simdata, type = 'response', re.form = NA)
    pred.ps2 <- predict(poi.glmm.ps2, newdata = newdata, type = 'response', re.form = NA)
    pred.ps3 <- predict(poi.glmm.ps3, newdata = newdata, type = 'response', re.form = NA)
    pred.ps4 <- predict(poi.glmm.ps4, newdata = newdata, type = 'response', re.form = NA)

    poi.predictions <- data.frame(
      true = env$simdata$y,
      sim = pred.sim,
      ps2 = pred.ps2,
      ps3 = pred.ps3,
      ps4 = pred.ps4
    )

    list(poi.predictions = poi.predictions)
  }
)
