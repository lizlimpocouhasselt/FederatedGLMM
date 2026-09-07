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
  g <- t(J) %*% r;     ng <- Norm(g, Inf)
  A <- t(J) %*% J                     # g is a column vector
  
  mu <- tau * max(diag(A))            # damping parameter
  nu <- 2; nh <- 0
  
  #-- Main loop
  errno <- 0
  k <- 1
  ssq <- 1
  counter <- 0
  while (k < maxeval & ssq > 1e-17) {
    prev_ssq <- ssq
    k <- k + 1
    #tic()
    # h <- solve(A + mu*eye(n), -g) # compute step through linear system
    R <- chol(A + mu*eye(n))        # use the Cholesky decomposition
    h <- c(-t(g) %*% chol2inv(R))   # h <- solve(R, solve(t(R), -g))
    
    nh <- Norm(h); nx <- tolx + Norm(x)
    if (nh <= tolx * nx) {errno <- 1; xnew <- x; print(paste0('iteration: ', k-1, ', ','ssq: ', sum(fun(xnew)^2))); break} else{xnew <- x + h
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
    
    
    #toc()
    print(paste0('iteration: ', k-1, ', ','ssq: ', sum(fun(xnew)^2)))
    if((sum(fun(xnew)^2)/prev_ssq) >= 0.9){counter = counter + 1; print(paste0('counter: ', counter))}
    ssq <- sum(fun(xnew)^2)
    if(counter > 19) break
  }
  
  if (k >= maxeval) errno <- 3
  errmess <- switch(errno,
                    "Stopped by small x-step.",
                    "Stopped by small gradient.",
                    "No. of function evaluations exceeded.")
  
  return(list(x = c(xnew), ssq = sum(fun(xnew)^2), ng = ng, nh = nh,
              mu = mu, neval = k, errno = errno, errmess = errmess))
}