#---------------------------------------------------------------
# FUNCTION TO PERFORM OPTIMIZATION
# (adapted version of lsqnonlin from R package pracma;
# only difference is the addition of iteration message updates)
# INPUTS:
#   fun : user-defined, vector-valued function
#   x0 : starting point
#   options : list of options
#   ... : additional parameters passed to the function
#
# OUTPUT: 
#   list containing:
#   1. x : point with least sum of squares value
#   2. ssq : sum of squares
#   3. ng : norm of last gradient
#   4. nh : norm of last step used
#   5. mu : damping parameter of Levenberg-Marquardt
#   6. neval : number of function evaluations
#   7. errno : error number, corresponds to error message
#   8. errmess : error message, i.e. reason for stopping
#---------------------------------------------------------------

lsqnonlin_2 <- function(fun, x0, options = list(), ...) {
  stopifnot(is.numeric(x0))
  
  #-- Option list handling -----------
  opts <- list(tau     = 1e-3,
               tolx    = 1e-8,
               tolg    = 1e-8,
               maxeval = 700)
  namedOpts <- match.arg(names(options), choices = names(opts),
                         several.ok = TRUE)
  if (!is.null(names(options)))
    opts[namedOpts] <- options
  # x0 is   good...not so good start value
  tau     <- opts$tau             # tau = 1e-6...1e-3...1
  tolx    <- opts$tolx            #
  tolg    <- opts$tolg            #
  maxeval <- opts$maxeval         # max. number of iterations
  
  #-- Matching function
  fct <- match.fun(fun)
  fun <- function(x) fct(x, ...)
  
  n <- length(x0)                     # fun: R^n --> R^m; n <- nrow(A)
  m <- length(fun(x0))
  
  # Initialization: Compute f, r, and J
  x <- x0;              r <- fun(x)
  f <- 0.5 * sum(r^2);  J <- jacobian(fun, x)
  g <- t(J) %*% r;     ng <- pracma::Norm(g, Inf)
  A <- t(J) %*% J                     # g is a column vector
  
  mu <- tau * max(diag(A))            # damping parameter
  nu <- 2; nh <- 0
  
  #-- Main loop
  errno <- 0
  k <- 1
  ssq <- 1
  counter <- 0
  while (k < maxeval & ssq > 1e-16) {
    prev_ssq <- ssq
    k <- k + 1
    # R <- chol(A + mu*eye(n))        # use the Cholesky decomposition
    ## attempt Cholesky decomposition
    R <- tryCatch(
      chol(A + mu*eye(n)),
      error = function(e) NULL
    )
    ## if decomposition failed -> exit loop gracefully
    if (is.null(R)) {
      message(paste0("Stopped: Cholesky failed at iteration ", k))
      errno <- 3   # custom code for "chol failed"
      xnew <- x    # keep the last valid solution
      break
    }
    h <- c(-t(g) %*% chol2inv(R))   # h <- solve(R, solve(t(R), -g))
    
    nh <- Norm(h); nx <- tolx + Norm(x)
    if (nh <= tolx * nx) {
      errno <- 1
      xnew <- x
      print(paste0('iteration: ', k-1, ', ','ssq: ', sum(fun(xnew)^2)))
      break} else{xnew <- x + h
    h <- xnew - x
    dL <- sum(h * (mu*h - g))/2
    rn <- fun(xnew)
    fn <- 0.5 * sum(rn^2); Jn <- jacobian(fun, xnew)
    if (length(rn) != length(r))  {df <- f - fn
    } else                        {df <- sum((r - rn) * (r + rn))/2}
    
    if (dL > 0 && df > 0) {
      x <- xnew;         f <- fn;  J <- Jn;       r <- rn; 
      A <- t(J) %*% J;   g <- t(J) %*% r;         ng <- Norm(g,Inf)
      mu <- mu * max(1/3, 1 - (2*df/dL - 1)^3);   nu <- 2
    } else {
      mu <- mu*nu;  nu <- 2*nu
    }
    if (ng <= tolg) { errno <- 2; break }}
    
    
    print(paste0('iteration: ', k-1, ', ','ssq: ', sum(fun(xnew)^2)))
    if((sum(fun(xnew)^2)/prev_ssq) >= 0.99){
      counter = counter + 1
      print(paste0('counter: ', counter))
      if(counter > 19) break
    } else{counter <- 0}
    ssq <- sum(fun(xnew)^2)
    
  }
  
  if (k >= maxeval) errno <- 3
  errmess <- switch(errno,
                    "Stopped by small x-step.",
                    "Stopped by small gradient.",
                    "No. of function evaluations exceeded.")
  
  return(list(x = c(xnew), ssq = sum(fun(xnew)^2), ng = ng, nh = nh,
              mu = mu, neval = k, errno = errno, errmess = errmess))
}