#-------------------------------------------------------------------
#PSEUDO-DATA (3rd MOM): GENERALIZED LINEAR MODEL - LOG LINK (POISSON)
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

run_pseudodata(moment = 3, input_suffix = '_x4_x5', output_suffix = '_x4_x5')