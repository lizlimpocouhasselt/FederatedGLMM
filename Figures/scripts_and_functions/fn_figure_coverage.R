#---------------------------------------------------------------
# FUNCTION TO COMPUTE COVERAGE
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   parnum : parameter number
#   lookup_df : dataframe consisting of the true parameter values
#   local_root : local directory of inputs pulled from Drive (fig_pull_inputs)
#
# OUTPUT: 
#   vector of 95% confidence interval coverage for
#      estimates from pseudo- and actual data
#---------------------------------------------------------------

fn_coverage <- function(nsim, m, uniform_cluster_size, parnum, lookup_df, family, local_root){
  .log <- if (exists("fig_log", mode = "function")) {
    get("fig_log", mode = "function")
  } else {
    function(fmt, ...) message(sprintf(paste0("[figures] ", fmt), ...))
  }

  sapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
    .log("fn_coverage start: par=%d m=%d n=%d type=%s", parnum, m, uniform_cluster_size, data.type)
    mean(sapply(1:nsim, function(iter){
      if (iter %% 100 == 0 || iter == nsim) {
        .log("fn_coverage progress: par=%d m=%d n=%d type=%s iter=%d/%d",
             parnum, m, uniform_cluster_size, data.type, iter, nsim)
      }
      file <- file.path(local_root, "interval_estimates",
                        sprintf("interval_estimate_%04d_%04d_%04d.RData",
                                iter, m, uniform_cluster_size))
      if(file.exists(file)){
        load(file)
        if(exists("interval_estimate")){
          partrue <- lookup_df$true[match(parnum, lookup_df$num)]
          partrue >= interval_estimate[[data.type]][parnum, 1] &
            partrue <= interval_estimate[[data.type]][parnum, 2]
        } else NA
      } else NA
    }), na.rm = TRUE)
  })
}