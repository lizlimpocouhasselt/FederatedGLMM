#---------------------------------------------------------------
# FUNCTION TO TABULATE ESTIMATES (POINT & INTERVAL)
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   parnum : nth parameter
#   local_root : local directory of inputs pulled from Drive (fig_pull_inputs)
#
# OUTPUT: 
#   long dataframe consisting of point and interval estimates
#---------------------------------------------------------------

fn_confint <- function(nsim, m, uniform_cluster_size, parnum, family, local_root){
    .log <- if (exists("fig_log", mode = "function")) {
      get("fig_log", mode = "function")
    } else {
      function(fmt, ...) message(sprintf(paste0("[figures] ", fmt), ...))
    }

    ci.ls <- lapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
              .log("fn_confint start: par=%d m=%d n=%d type=%s", parnum, m, uniform_cluster_size, data.type)
              mat <- t(sapply(1:nsim, function(iter){
                      if (iter %% 100 == 0 || iter == nsim) {
                        .log("fn_confint progress: par=%d m=%d n=%d type=%s iter=%d/%d",
                             parnum, m, uniform_cluster_size, data.type, iter, nsim)
                      }
                      file.pt = file.path(local_root, "point_estimates",
                                          sprintf("point_estimate_%04d_%04d_%04d.RData",
                                                  iter, m, uniform_cluster_size))
                      file.int = file.path(local_root, "interval_estimates",
                                           sprintf("interval_estimate_%04d_%04d_%04d.RData",
                                                   iter, m, uniform_cluster_size))
                      if(file.exists(file.pt) & file.exists(file.int)){
                        load(file.pt)
                        load(file.int)
                        if(exists("interval_estimate") & exists("point_estimate")){
                          c(interval_estimate[[data.type]][parnum, 1],
                            point_estimate[parnum, data.type], 
                            interval_estimate[[data.type]][parnum, 2])
                        } else rep(NA, 3)
                      } else rep(NA, 3)
                    }))
              df <- as.data.frame(mat)
              df$dat <- rep(data.type, nsim)
              .log("fn_confint done: par=%d m=%d n=%d type=%s", parnum, m, uniform_cluster_size, data.type)
              df
            })
    ci.ls <- lapply(ci.ls, function(ci) ci <- ci[order(unlist(ci.ls[[1]][, 2])), ])
    ci.df <- do.call(rbind, ci.ls)
    colnames(ci.df)[1:3] <- c('LL', 'point', 'UL')
    return(ci.df)
}