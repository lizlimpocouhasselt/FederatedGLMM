# Define objective function (only for family %in% c("binomial", "poisson"))
  obj_fn <- function(x, n.sp, p, var.names, target.df, int.w = NULL, support = NULL){
    
    if(is.null(int.w)){
      if(is.null(support)){
        support <- matrix(x[1 : (n.sp * p)], ncol = p, nrow = n.sp, byrow = F)
        int.w <- x[(n.sp * p + 1) : (n.sp * p + n.sp)]
      } else{int.w <- x}
      w <- int.w^2 / target.df$value[1]  # positive + normalized
    } else{
      support <- matrix(x, ncol = p, nrow = n.sp, byrow = F)
      w <- int.w / target.df$value[1]
      }  # positive + normalized
    colnames(support) <- var.names
    H <- construct_hankel(support, w)
    syn.mom.df <- extract_unique_moments(H)
    c((syn.mom.df$value - target.df$value/target.df$value[1]))
  }