#-------------------------------------------------------------------
# MODEL PREDICTION
#-------------------------------------------------------------------

# Empty the environment
rm(list=ls(all=TRUE))

# Load packages
library(lme4)
# library(dplyr)
# library(tidyr)

# # Call functions
source(file.path(getwd(), 'SIMULATION', 'scripts','soft_poisson.R'))

# family = 'poisson'

# # Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "intermediate_results", "poisson", "par_settings.csv"))

# seed <- par_settings$seed[row]


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

# set.seed(seed)
lapply(1:nrow(par_settings), function(row){
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

  # Estimate a Poisson mixed model 
  poi.glmm.sim <- glmer(y ~ scale(x1) + x2 + x3 + (1|g), data = simdata, family = poisson)
  poi.glmm.ps2 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps2, family = soft_poisson)
  poi.glmm.ps3 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps3, family = soft_poisson)
  poi.glmm.ps4 <- glmer(y ~ scale(x1) + x2 + x32 + x33 + x34 + x35 + (1|g), data = ps4, family = soft_poisson)

  # Make predictions on the original simdata using each model
  newdata <- cbind(x1 = simdata$x1, model.matrix(poi.glmm.sim)[, -(1:2)], simdata[, c('y', 'g')])
  pred.sim <- predict(poi.glmm.sim, newdata = simdata, type = "response", re.form = NA)
  pred.ps2 <- predict(poi.glmm.ps2, newdata = newdata, type = "response", re.form = NA)
  pred.ps3 <- predict(poi.glmm.ps3, newdata = newdata, type = "response", re.form = NA)
  pred.ps4 <- predict(poi.glmm.ps4, newdata = newdata, type = "response", re.form = NA)

  # Combine predictions into a data frame
  poi.predictions <- data.frame(
    true = simdata$y,
    sim = pred.sim,
    ps2 = pred.ps2,
    ps3 = pred.ps3,
    ps4 = pred.ps4
  )

  # Save
  filename <- file.path("SIMULATION", "intermediate_results", "poisson", "preds", sprintf("preds_%04d_%04d_%04d", iter, m, uniform_cluster_size))
  save(poi.predictions, file = sprintf("%s.RData", filename))
})
