#-----------------------------------------
# DATA ANALYST TASK
# GENERATE PSEUDO-DATA (GENERIC)
#-----------------------------------------

library(pracma)
library(tictoc)
library(dplyr)
library(tidyr)
library(randtoolbox)
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'gen_pseudo.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'extract_unique_moments.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'obj.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'lsqnonlin_2.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'construct_hankel.R'))


# Count number of groups
all_files <- list.files(path = file.path("DEMO", "intermediate_results", "summary_info"))
m <- length(all_files)

# In the interest of time, we generated pseudo-data using Vlaams Supercomputer
lapply(1:m, function(grp_num){
  # I. Load summary data
  load(file.path("DEMO", "intermediate_results", "summary_info", sprintf("summary_info_%04d.RData", grp_num)))
  
  # II. Generate pseudo-data
  set.seed(121314)
  ps.ls <- lapply(summary_info, function(summary_info){
    unsc.H <- summary_info[[1]]
    sc.H <- summary_info[[2]]
    var.names <- rownames(unsc.H)[!rowSums(sapply(c('^', ' '), grepl, rownames(unsc.H), fixed = T)) > 0]
    var.names <- var.names[var.names %in% c('Length.of.Stay', 'Total.Charges', 'Gender_M', 'COVID19_positive', 'Emergency.Department.Indicator_Y')]
    p <- length(var.names) #no. of variables in actual data
    tic()
    ps <- gen_pseudo(moment = 4, var.names, unsc.H, sc.H)
    toc()
    ps
  })
  ps <- bind_rows(ps.ls) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
  ps$g <- grp_num
  cat("grp ", grp_num, " is finished")
  save(ps, file = file.path(getwd(), "DEMO", "intermediate_results", "ps", sprintf("ps_%04d.RData", grp_num))) 
})