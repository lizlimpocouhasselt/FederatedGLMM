#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA DEPENDING ON K
#
# INPUTS:
#   moment : Taylor polynomial degree (K = 2, 3, or 4)
#   var.names : variable names including response
#   unsc.H : unstandardized Hankel moment matrix
#   sc.H : standardized Hankel moment matrix
#   m.per.dim
#
# OUTPUT: 
#   dataframe of pseudo-data
#---------------------------------------------------------------

gen_pseudo <- function(moment = 2, var.names, unsc.H, sc.H){

  n <- unsc.H[1, 1]
  p <- length(var.names)

  # Extract unstandardized mean vector and covariance matrix from Hankel moment matrix
  idx <- rownames(unsc.H) %in% var.names
  unsc.mean <- unsc.H[idx, 1] / n
  xbar.ybar <- outer(unsc.mean, unsc.mean, FUN = '*')
  unsc.sum.xy <- unsc.H[idx, idx]
  unsc.covmat <- (unsc.sum.xy - n * xbar.ybar) / (n - 1)
  unsc.var <- diag(unsc.covmat)

  if(moment == 2){
    unsc.syn.df <- mvrnorm2(n, mu = unsc.mean, Sigma = unsc.covmat, empirical = T)
  } else{

    # Use only elements of Hankel moment matrix found in var.names
    exc.idx <- setdiff(rownames(unsc.H)[!rowSums(sapply(c('^', ' '), grepl, rownames(unsc.H), fixed = T)) > 0],
     var.names)[-1]
    H.idx <- !rowSums(sapply(exc.idx, grepl, rownames(sc.H), fixed = T)) > 0
    H <- sc.H[H.idx, H.idx]
    
    # Construct target moments
    target.df <- extract_unique_moments(H)

    # Initialize
    U <- sobol(n = sc.H[1,1], dim = p)
    lower <- rep(-15, p)
    upper <- rep(15, p)
    support <- t(t(U) * (upper - lower) + lower)
    colnames(support) <- var.names

    # Minimize objective function wrt support points while fixing weights to be equal
    int.w0 <- rep(1, sc.H[1,1])
    vals <- lsqnonlin_2(obj_fn, as.vector(support), n.sp = sc.H[1,1], p = p, var.names = var.names, target.df = target.df, int.w = int.w0)
    
    opt.support <- matrix(vals$x[1:(sc.H[1,1] * p)], ncol = p, nrow = sc.H[1,1], byrow = F)
    colnames(opt.support) <- var.names

    # Create matrix of [scaled] pseudo-data from optimal support points with fixed weights
    syn <- opt.support

    # Unscale numeric pseudo-data
    unsc.syn <- t(syn) * sqrt(unsc.var) + unsc.mean
    unsc.syn.df <- as.data.frame(t(unsc.syn))
    names(unsc.syn.df) <- var.names
  }
  unsc.syn.df
}
