#-----------------------------------------
# DATA ANALYST TASK
# ESTIMATE MODEL
#-----------------------------------------

library(lme4)
library(dplyr)
library(tidyr)
source(file.path('DEMO', 'scripts_and_functions', 'soft_binomial.R'))
source(file.path('DEMO', 'scripts_and_functions', 'soft_binomInitialize.R'))
source(file.path('DEMO', 'scripts_and_functions', 'soft_poisson.R'))


# Count number of groups
all_files <- list.files(path = file.path("DEMO", "intermediate_results", "ps"))
m <- length(all_files)

# Load pseudo-data
grp_nums <- unlist(lapply(all_files, function(file.name){
  load(file.path(getwd(), "DEMO", "intermediate_results", "ps", file.name))
  cat('file ', file.name, ' is loaded\n')
  ps$g[1]
}))

ps.ls <- lapply(grp_nums, function(grp_num){
  load(file.path(getwd(), "DEMO", "intermediate_results", "ps", sprintf("ps_%04d.RData", grp_num)))
  cat('grp ', grp_num, ' is loaded\n')
  ps
})
ps <- bind_rows(ps.ls) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))


# Redefine glm families with no scale to accommodate non-binary or non-integer responses
lme4.env <- getNamespace("lme4")
newhasNoScale <- function (family) {
  any(substr(family$family, 1L, 16L) == c("poisson", "binomial", "negative.bin", "Negative Bin",
                                          "soft_binomial",
                                          "soft_poisson"))
}
unlockBinding("hasNoScale", lme4.env)
assign("hasNoScale", newhasNoScale, envir = lme4.env)
lockBinding("hasNoScale", lme4.env)

# Estimate a linear mixed model from pseudo-data
start.time <- Sys.time()
gau.glmm.ps <- lmer(scale(Total.Charges) ~ scale(Length.of.Stay) + COVID19_positive + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps)
end.time <- Sys.time()
end.time - start.time
#Time difference of 8.131239 secs


summary(gau.glmm.ps)
ci.gau.glmm.ps <- confint(gau.glmm.ps, method = 'Wald')

# Estimate a logistic mixed model from pseudo-data
start.time <- Sys.time()
logit.glmm.ps <- glmer(COVID19_positive ~ scale(Length.of.Stay) + scale(Total.Charges) + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, family = soft_binomial)
end.time <- Sys.time()
end.time - start.time
#Time difference of 3.75578 mins


summary(logit.glmm.ps)
ci.logit.glmm.ps <- confint(logit.glmm.ps, method = "Wald")


# Estimate a Poisson mixed model from pseudo-data
start.time <- Sys.time()
poi.glmm.ps <- glmer(Length.of.Stay ~ COVID19_positive + scale(Total.Charges) + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, family = soft_poisson)
end.time <- Sys.time()
end.time - start.time
# Time difference of 2.315422 mins

# Model 2
start.time <- Sys.time()
poi.glmm.ps <- glmer(Length.of.Stay ~ COVID19_positive + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, family = soft_poisson)
end.time <- Sys.time()
end.time - start.time

summary(poi.glmm.ps)
ci.poi.glmm.ps <- confint(poi.glmm.ps, method = "Wald")

#------------------------------------
# Compare results with actual data
#------------------------------------


# Load actual data
data <- read.csv(file.path(getwd(), "DEMO", "intermediate_results", "preprocessed_data.csv"), header = T)
grpd_data <- data %>% split(f = as.factor(data$Facility.Name))
grpd_data <- grpd_data[grp_nums]
data <- do.call(rbind, grpd_data)

# Estimate a linear mixed model from actual data
start.time <- Sys.time()
gau.glmm.actual <- lmer(scale(Total.Charges) ~ scale(Length.of.Stay) + COVID19 + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data)
end.time <- Sys.time()
end.time - start.time
#Time difference of 6.364935 secs

summary(gau.glmm.actual)
ci.gau.glmm.actual <- confint(gau.glmm.actual, method = 'Wald')

# Estimate a logistic mixed model from actual data
start.time <- Sys.time()
logit.glmm.actual <- glmer(ifelse(COVID19 == "positive", 1, 0) ~ scale(Length.of.Stay) + scale(Total.Charges) + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = binomial)
end.time <- Sys.time()
end.time - start.time
#Time difference of 2.727469 mins

summary(logit.glmm.actual)
ci.logit.glmm.actual <- confint(logit.glmm.actual, method = 'Wald')

# Estimate a Poisson mixed model from actual data
start.time <- Sys.time()
poi.glmm.actual <- glmer(Length.of.Stay ~ COVID19 + scale(Total.Charges) + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = poisson)
# poi.glmm.actual <- glmer(Length.of.Stay ~ COVID19 + scale(Total.Charges) + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = soft_poisson)
end.time <- Sys.time()
end.time - start.time
#Time difference of 2.662622 mins
# poi.glmm.actual <- glmer(Length.of.Stay ~ COVID19 + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = poisson)

summary(poi.glmm.actual)
ci.poi.glmm.actual <- confint(poi.glmm.actual, method = "Wald")


