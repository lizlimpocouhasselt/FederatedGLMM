#---------------------------------------------------------------
# FUNCTION TO COMPUTE UNIQUE SAMPLE MOMENTS PER CLUSTER
# INPUTS:
#   data : dataframe
#   num.varnames : vector of numeric variable names
#
# OUTPUT: 
#   list containing:
#   1. Hankel moment matrix for unstandardized variables
#   2. Hankel moment matrix for standardized variables
#---------------------------------------------------------------

fn_compute_summary <- function(data, num.varnames){

  # Convert categorical variable to dummy variables
  if(sum(names(data) %in% num.varnames) == ncol(data)){
    X.df <- data
  } else{
    X.df <- dummy_cols(.data = data,
                       select_columns = names(data)[!(names(data) %in% num.varnames)],
                       remove_first_dummy = F,
                       remove_selected_columns = T)
  }
  X <- as.matrix(X.df)
  X.names <- colnames(X)

  # Set weights = 1
  w <- rep(1, nrow(X))

  # Construct Hankel moment matrix for unstandardized variables
  unsc.H <- construct_hankel(X, w, max_order = 4)

  # Scale variables with positive variance by dividing by their standard deviation
  idx <- rownames(unsc.H) %in% X.names
  mu <- unsc.H[idx, 1] / nrow(X)
  xbar_ybar <- outer(mu, mu, FUN = '*')
  covmat <- (unsc.H[idx, idx] - nrow(X) * xbar_ybar) / (nrow(X) - 1)
  # X[, diag(covmat) > 0] <- t(t(X[, diag(covmat) > 0]) / sqrt(diag(covmat)[diag(covmat) > 0]))
  X[, diag(covmat) > 0] <- scale(X[, diag(covmat) > 0])
   
  # Construct Hankel moment matrix for scaled variables
  sc.H <- construct_hankel(X, w, max_order = 4)
  return(list(unsc.H = unsc.H, sc.H = sc.H))
}