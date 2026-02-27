#-----------------------------------------
# DATA PROVIDER TASK
# COMPUTE SUMMARY STATISTICS (PER CLUSTER)
# HANKEL MATRICES
#-----------------------------------------

# Load libraries / functions
library(dplyr)
library(stringr)
library(fastDummies)
source(file.path('DEMO', 'scripts_and_functions', 'fn_compute_summary.R'))
source(file.path('DEMO', 'scripts_and_functions', 'construct_hankel.R')) # Hankel moment matrix (summations)


#---------------- DATA PROVIDER TASK ----------------

# I. Load and preprocess data
#Include only hospitals with:
# a. complete cases
# b. valid values
# c. more than 1 patient record
hospital_inpatient_discharges <- read.csv(file.path("DEMO", "Hospital_Inpatient_Discharges__SPARCS_De-Identified___2022_20241021.csv"))
data <- hospital_inpatient_discharges %>% 
  filter(complete.cases(.), Length.of.Stay != "120 +", Gender %in% c("F", "M")) %>%
  mutate(Total.Charges = str_remove_all(Total.Charges, ","),
          COVID19 = ifelse(CCSR.Diagnosis.Description == "COVID-19", "positive", "negative")) %>%
  dplyr::select(Facility.Name, Gender, Length.of.Stay, COVID19, Emergency.Department.Indicator, Total.Charges) %>%
  mutate_at(c('Length.of.Stay', 'Total.Charges'), as.numeric) %>%
  group_by(Facility.Name) %>% filter(n() > 1) %>% as_tibble()
# Save preprocessed data
write.csv(data, file = file.path("DEMO", "intermediate_results", "preprocessed_data.csv"), row.names = F)



# II. Compute summary statistics (per cluster)
num.varnames <- c("Length.of.Stay", "Total.Charges")
# Break entire dataset into clusters, if applicable
grpd_data <- data %>% split(f = as.factor(data$Facility.Name))
grp_info <- data.frame(num = 1:length(grpd_data),
                      name = names(grpd_data))
lapply(grpd_data, function(grp){
  if(nrow(grp) > 500){
    N <- nrow(grp); n_i <- 250; n_full_grps <- N %/% n_i
    remainder <- N %% n_i
    n_last <- n_i + remainder #add remainder to last full group
    summary_info <- lapply(1:n_full_grps, function(grp_i){
      if(grp_i < n_full_grps) {row.idx <- ((grp_i - 1) * n_i + 1) : (grp_i * n_i)} else {row.idx <- ((grp_i - 1) * n_i + 1) : nrow(grp)}
      fn_compute_summary(grp[row.idx, -1], num.varnames)
    })
  } else{
    summary_info <- list(fn_compute_summary(grp[, -1], num.varnames))
  }
  grp_name <- grp$Facility.Name[1]
  grp_num <- grp_info$num[grp_info$name == grp_name]
  save(summary_info, file = file.path("DEMO", "intermediate_results", "summary_info", sprintf("summary_info_%04d.RData", grp_num)))
})