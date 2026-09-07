#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA DEPENDING ON K
#
# INPUTS:
#   moment : Taylor polynomial degree (K = 2, 3, or 4)
#   var_names : variable names including response
#   uni_sum : mean and covariance dataframe
#   covmat : variance-covariance matrix
#   bv_34 : bivariate 3rd and 4th sample moments
#   tv_34 : trivariate 3rd and 4th sample moments
#   qv_4 : quadvariate 4th sample moments
#
# OUTPUT: 
#   dataframe of pseudo-data
#---------------------------------------------------------------

gen_pseudo <- function(moment=2, var_names,
 uni_sum, covmat, bv_34 = NULL, tv_34 = NULL, qv_4 = NULL, start = NULL){

  n <- uni_sum[1, 'n']
  p <- length(var_names)
  idx <- match(var_names, uni_sum$variable)
  Sigma <- covmat[var_names, var_names]

  if(moment == 2){
    syn <- mvrnorm2(n, mu = rep(0,p), 
                       Sigma = Sigma, 
                       empirical = T)
  } else{
    # Initialize
    if(is.null(start)){
    ## Takes shorter but may not be reproducible across different platforms (e.g. MacOS vs Linux vs Windows)
    start <- mvrnorm2(n, mu = rep(0,p), 
                       Sigma = Sigma, 
                       empirical = T)
    ## Takes longer but reproducible across different platforms (e.g. MacOS vs Linux vs Windows)
    # start <- c(sapply(1:p, function(var) mvrnorm(n, 0, covmat[var_names, var_names])))
    }
    start[, uni_sum$variance[idx] > 0] <- scale(start[, uni_sum$variance[idx] > 0])
    start <- c(as.matrix(start))
    # Optimize objective function
    vals <- lsqnonlin_2(obj_fn, start,
                      options = list(tolx = 1e-10, tolg = 1e-10, maxeval = 100),
                      moment, var_names, n, p,
                      uni_sum, covmat, 
                      bv_34, tv_34, qv_4)
    syn <- matrix(vals$x, nrow = n, byrow = F)
  }
  

  # Unstandardize numeric pseudo-data
  x.mean <- uni_sum$mean[idx]
  x.var <- uni_sum$variance[idx]
  unstd_syn <- t(syn) * sqrt(x.var) + x.mean
  unstd_syn_df <- as.data.frame(t(unstd_syn))
  names(unstd_syn_df) <- var_names
  unstd_syn_df
}
