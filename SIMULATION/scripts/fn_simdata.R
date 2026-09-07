fn_simdata_glmm <- function(iter, m, uniform_cluster_size, seed, x_pars){
  
  n_h <- rep(uniform_cluster_size, m)
  n = sum(n_h) #total sample size
  
  # Set simulation parameters
  b <- c(2.285513, -0.302612, 0.087566, -0.959036, -0.812642, -0.809044, -0.794925)
  sigma_h <- 0.4811
  var_h <- 0.2315
  
  # Generate predictor data
  set.seed(seed)
  x1 <- rnorm(n, x_pars$x1_norm_mean, x_pars$x1_norm_sd)
  x2 <- rbinom(n, 1, x_pars$x2_p)
  x3 <- rmultinom(n, 1, x_pars$x3_multinom_p)
  X <- cbind(scale(x1), x2, t(x3)[,-1]) #exclude ref cat
  u0 <- rnorm(m, 0, sigma_h) 
  z <- model.matrix(~-1 + ., data.frame(grp_num = as.factor(rep(1:m, n_h))))
  
  
  # Generate response data
  xb <- b[1] + X %*% b[-1] + z %*% u0
  lambda <- exp(xb)
  y <- sapply(lambda, function(lam) rpois(1, lam))
  
  # Create dataframe
  simdata <- data.frame(x1=x1, 
             x2=x2,
             x3=as.factor(ifelse(x3[1,]==1, 1,
                                 ifelse(x3[2,]==1, 2,
                                        ifelse(x3[3,]==1, 3,
                                               ifelse(x3[4,]==1, 4, 5))))),
             y=y, g=as.factor(rep(1:m, n_h)))
  # while(sum(unlist(lapply(1:m, function(j){
  #   length(unique(simdata$x3[simdata$g==j])) != 5
  # }))) != 0){
  #   x3 <- rmultinom(n, 1, x_pars$x3_multinom_p)
  #   X <- cbind(scale(x1), x2, t(x3)[,-1]) #exclude ref cat
  #   u0 <- rnorm(m, 0, sigma_h) 
  #   z <- model.matrix(~-1 + ., data.frame(grp_num = as.factor(rep(1:m, n_h))))
    
  #   # Generate response data
  #   xb <- b[1] + X %*% b[-1] + z %*% u0
  #   lambda <- exp(xb)
  #   y <- sapply(lambda, function(lam) rpois(1, lam))
  
  #   # Create dataframe
  #   simdata <- data.frame(x1=x1, 
  #                         x2=x2,
  #                         x3=as.factor(ifelse(x3[1,]==1, 1,
  #                                ifelse(x3[2,]==1, 2,
  #                                       ifelse(x3[3,]==1, 3,
  #                                              ifelse(x3[4,]==1, 4, 5))))),
  #                         y=y, g=as.factor(rep(1:m, n_h)))
  # }
  simdata
}