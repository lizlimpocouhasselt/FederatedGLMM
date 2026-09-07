#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA WITH EXACT MEAN AND COVARIANCE
# (adapted version of mvrnorm from R package MASS;
# the difference is the possibility of n_observations < n_variables)
# INPUTS:
#   n : the number of samples required
#   mu : a vector giving the means of the variables.
#   Sigma : a positive-definite symmetric matrix specifying the covariance matrix of the variables.
#   tol : tolerance (relative to largest variance) for numerical lack of positive-definiteness in Sigma.
#   empirical : logical. If true, mu and Sigma specify the empirical not population mean and covariance matrix.
#   EISPACK : logical: values other than FALSE are an error.
#
# OUTPUT: 
#   n = 1:
#     vector of same length as mu
#   otherwise:
#     n by length(mu) matrix with one sample in each row
#---------------------------------------------------------------

mvrnorm2 <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE) 
{
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  if (EISPACK) 
    stop("'EISPACK' is no longer supported by R", domain = NA)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(runif(p * n), n) #changed from rnorm to runif
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- apply(X, 2, function(column) ifelse(is.na(column), 0, column))
    X <- X %*% svd(X, nu = 0, nv = p)$v
    X <- scale(X, FALSE, TRUE)
    X <- apply(X, 2, function(column) ifelse(is.na(column), 0, column))
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) drop(X) else t(X)
}
