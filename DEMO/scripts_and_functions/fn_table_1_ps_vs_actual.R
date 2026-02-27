#------------------------------------
# HELPER FUNCTIONS TO FORMAT OUTPUT
# 'fmt'
#   Inputs:
#     est : point estimate
#     se : standard error of estimate
#     stars : significance stars
#   Output:
#     formatted version of inputs
# 'get_stars'
#   Input:
#     p-value
#   Output:
#     significance stars
# 'fn_table_1b'
#   Input:
#     object from glmm output
#   Output:
#     formatted values in dataframe
#------------------------------------

fmt <- function(est, se, stars) {
  sprintf("%.3f (%.3f)%s", est, se, stars)
}


get_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

fn_table_1b <- function(object){
  paste0(round(object, 3))
}