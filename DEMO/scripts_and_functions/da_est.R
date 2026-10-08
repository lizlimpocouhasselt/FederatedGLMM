#-----------------------------------------
# DATA ANALYST TASK
# ESTIMATE MODEL
#-----------------------------------------

library(lme4)
library(data.table)
library(parallel)
source(file.path('R_common', 'remote_store.R'))
source(file.path('R_common', 'soft_binom_initialize.R'))
source(file.path('R_common', 'soft_binomial.R'))
source(file.path('R_common', 'soft_poisson.R'))

script.start <- Sys.time()
cat("[da_est] starting run\n")

# Fork-based parallelism is unavailable on Windows
n_cores <- as.integer(Sys.getenv("FGLMM_CORES", unset = max(1L, detectCores() - 1L)))
if (.Platform$OS.type == "windows") n_cores <- 1L
cat(sprintf("[da_est] using %d cores\n", n_cores))


# Load pseudo-data (each file is downloaded once, in parallel)
ps_dir <- file.path("DEMO", "intermediate_results", "ps")
all_files <- remote_list(ps_dir)
cat(sprintf("[da_est] found %d pseudo-data files\n", length(all_files)))

ps.ls <- mclapply(all_files, function(file.name) {
  e <- new.env(parent = emptyenv())
  remote_load(file.path(ps_dir, file.name), envir = e)
  e$ps
}, mc.cores = min(n_cores, 8L, length(all_files)))

failed <- vapply(ps.ls, inherits, logical(1), what = "try-error")
if (any(failed)) stop("Failed to load: ", paste(all_files[failed], collapse = ", "))

ps <- rbindlist(ps.ls, fill = TRUE)
rm(ps.ls)
grp_nums <- unique(ps$g)
num_cols <- names(ps)[vapply(ps, is.numeric, logical(1))]
setnafill(ps, type = "const", fill = 0, cols = num_cols)
setDF(ps)
cat(sprintf("[da_est] pseudo-data assembled: %d rows, %d groups\n", nrow(ps), length(grp_nums)))


# Load actual data (only the needed columns and groups)
cat("[da_est] loading actual data\n")
data <- remote_fread(
  file.path("DEMO", "intermediate_results", "preprocessed_data.csv"),
  select = c("Facility.Name", "Gender", "Length.of.Stay", "COVID19",
             "Emergency.Department.Indicator", "Total.Charges"),
  colClasses = list(character = c("Facility.Name", "Gender", "COVID19",
                                  "Emergency.Department.Indicator")),
  data.table = FALSE
)
# Group number g is the position of the facility among the sorted facility names
data <- data[as.integer(as.factor(data$Facility.Name)) %in% grp_nums, , drop = FALSE]
cat(sprintf("[da_est] actual data filtered: %d rows\n", nrow(data)))


# Redefine glm families with no scale to accommodate non-binary or non-integer responses
cat("[da_est] defining custom mixed-model families\n")
lme4.env <- getNamespace("lme4")
newhasNoScale <- function (family) {
  any(substr(family$family, 1L, 16L) == c("poisson", "binomial", "negative.bin", "Negative Bin",
                                          "soft_binomial",
                                          "soft_poisson"))
}
unlockBinding("hasNoScale", lme4.env)
assign("hasNoScale", newhasNoScale, envir = lme4.env)
lockBinding("hasNoScale", lme4.env)

# Fit all six models in parallel, longest fits first.
# calc.derivs = FALSE skips the post-fit gradient/Hessian convergence checks.
cat("[da_est] fitting mixed models\n")
lctl <- lmerControl(calc.derivs = FALSE)
gctl <- glmerControl(calc.derivs = FALSE)

fits <- list(
  logit.glmm.ps = function() glmer(COVID19_positive ~ scale(Length.of.Stay) + scale(Total.Charges) + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, family = soft_binomial, control = gctl),
  logit.glmm.actual = function() glmer(ifelse(COVID19 == "positive", 1, 0) ~ scale(Length.of.Stay) + scale(Total.Charges) + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = binomial, control = gctl),
  poi.glmm.actual = function() glmer(Length.of.Stay ~ COVID19 + scale(Total.Charges) + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, family = poisson, control = gctl),
  poi.glmm.ps = function() glmer(Length.of.Stay ~ COVID19_positive + scale(Total.Charges) + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, family = soft_poisson, control = gctl),
  gau.glmm.actual = function() lmer(scale(Total.Charges) ~ scale(Length.of.Stay) + COVID19 + Gender + Emergency.Department.Indicator + (1|Facility.Name), data = data, control = lctl),
  gau.glmm.ps = function() lmer(scale(Total.Charges) ~ scale(Length.of.Stay) + COVID19_positive + Gender_M + Emergency.Department.Indicator_Y + (1|g), data = ps, control = lctl)
)

# One fork per model so a long fit does not block a queue of short ones
res <- mclapply(fits, function(f) {
  fit <- f()
  list(fit = fit, ci = confint(fit, method = "Wald"))
}, mc.cores = min(n_cores, length(fits)), mc.preschedule = FALSE)

failed <- vapply(res, inherits, logical(1), what = "try-error")
if (any(failed)) stop("Model fit failed: ", paste(names(res)[failed], collapse = ", "))
cat("[da_est] all model fits completed\n")

gau.glmm.ps       <- res$gau.glmm.ps$fit;       ci.gau.glmm.ps       <- res$gau.glmm.ps$ci
logit.glmm.ps     <- res$logit.glmm.ps$fit;     ci.logit.glmm.ps     <- res$logit.glmm.ps$ci
poi.glmm.ps       <- res$poi.glmm.ps$fit;       ci.poi.glmm.ps       <- res$poi.glmm.ps$ci
gau.glmm.actual   <- res$gau.glmm.actual$fit;   ci.gau.glmm.actual   <- res$gau.glmm.actual$ci
logit.glmm.actual <- res$logit.glmm.actual$fit; ci.logit.glmm.actual <- res$logit.glmm.actual$ci
poi.glmm.actual   <- res$poi.glmm.actual$fit;   ci.poi.glmm.actual   <- res$poi.glmm.actual$ci
rm(res)

cat("[da_est] summarizing fitted models\n")
output_dir <- file.path("DEMO", "model_summaries")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_model_summary <- function(model_name, fit, ci) {
  out <- list(summary = summary(fit), confint = ci)
  saveRDS(out, file = file.path(output_dir, sprintf("%s.rds", model_name)))
  cat(sprintf("[da_est] saved %s to %s\n", model_name, file.path(output_dir, sprintf("%s.rds", model_name))))
  invisible(out)
}

save_model_summary("gau_glmm_ps", gau.glmm.ps, ci.gau.glmm.ps)
save_model_summary("logit_glmm_ps", logit.glmm.ps, ci.logit.glmm.ps)
save_model_summary("poi_glmm_ps", poi.glmm.ps, ci.poi.glmm.ps)
save_model_summary("gau_glmm_actual", gau.glmm.actual, ci.gau.glmm.actual)
save_model_summary("logit_glmm_actual", logit.glmm.actual, ci.logit.glmm.actual)
save_model_summary("poi_glmm_actual", poi.glmm.actual, ci.poi.glmm.actual)

cat("[da_est] summarizing fitted models\n")
summary(gau.glmm.ps)
summary(logit.glmm.ps)
summary(poi.glmm.ps)
summary(gau.glmm.actual)
summary(logit.glmm.actual)
summary(poi.glmm.actual)

cat(sprintf("[da_est] run finished in %.2f seconds\n", as.numeric(difftime(Sys.time(), script.start, units = "secs"))))
