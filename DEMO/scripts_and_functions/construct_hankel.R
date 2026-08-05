construct_hankel <- function(X, w, max_order = 4) {
  if (!is.matrix(X)) X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  if (length(w) != n) stop("Length of weights must equal number of rows of X")
  
  varnames <- colnames(X)
  if (is.null(varnames)) varnames <- paste0("x", seq_len(p))

  bv.names <- combn(varnames, 2)
  XY <- sapply(1:ncol(bv.names), function(col) X[, bv.names[1, col]] * X[, bv.names[2, col]])
  colnames(XY) <- paste(bv.names[1, ], bv.names[2, ])
  V <- cbind(`1` = 1, X, X^2, XY)
  colnames(V)[(ncol(X) + 2) : (2 * ncol(X) + 1)] <- paste(varnames, '^2', sep = '')

  
  # --- 4. Apply weights ---
  V_weighted <- V * sqrt(w)
  
  # --- 6. Moment matrix ---
  crossprod(V_weighted)  # faster than t(V_weighted) %*% V_weighted
}
