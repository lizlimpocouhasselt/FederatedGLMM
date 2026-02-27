#---------------------------------------------------------------
# FUNCTION TO TABULATE BIAS
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#
# OUTPUT: 
#   long dataframe ready for creating boxplots
#---------------------------------------------------------------

fn_relbias <- function(nsim, m, uniform_cluster_size, family){
  bias.ls <- lapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
    mat <- t(sapply(1:nsim, function(iter){
      file <- sprintf(file.path("SIMULATION", "intermediate_results", 
                                family, "point_estimates",
                                "point_estimate_%04d_%04d_%04d.RData"),
                      iter, m, uniform_cluster_size)
      if(file.exists(file)){
        load(file) 
        return((point_estimate[, data.type] - point_estimate[, 'true'])*100/point_estimate[, 'true'])
      } else return(rep(NA, ifelse(family == "poisson", 8, 6)))
    }))
    df <- as.data.frame(mat)
    df$dat <- rep(data.type, nsim)
    df$iter <- 1:nsim
    df
  })
  bias.wide <- do.call(rbind, bias.ls)
  betas <- c('b0', 'b1', 'b2', 'b32', 'b33')
  betas <- if(family == "logit") betas else c(betas, 'b34', 'b35')
  colnames(bias.wide)[1:(1 + length(betas))] <- c('sig.u', betas)
  
  bias.df <- melt(bias.wide, id.vars = c("dat", "iter"),
                  variable.name = "pars",
                  value.name = "bias")
  return(bias.df)
}