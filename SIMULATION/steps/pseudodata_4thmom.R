#-------------------------------------------------------------------
#PSEUDO-DATA (4th MOM): GENERALIZED LINEAR MODEL - LOG LINK (POISSON)
#-------------------------------------------------------------------

rm(list=ls(all=TRUE))

library(pracma)
library(dplyr)
library(MASS)

source(file.path(getwd(),'SIMULATION', 'scripts', 'lsqnonlin_2.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'gen_pseudo.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'obj.R'))
source(file.path(getwd(),'R_common', 'mvrnorm2.R'))
source(file.path(getwd(),'SIMULATION', 'scripts', 'fn_pseudodata.R'))

run_pseudodata(moment = 4, input_suffix = '', output_suffix = '')