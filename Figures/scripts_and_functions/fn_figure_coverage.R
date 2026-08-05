#---------------------------------------------------------------
# FUNCTION TO COMPUTE COVERAGE
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   parnum : parameter number
#   lookup_df : dataframe consisting of the true parameter values
#
# OUTPUT: 
#   vector of 95% confidence interval coverage for
#      estimates from pseudo- and actual data
#---------------------------------------------------------------

fn_coverage <- function(nsim, m, uniform_cluster_size, parnum, lookup_df, family){
  sapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
    mean(sapply(1:nsim, function(iter){
      file <- sprintf(file.path(getwd(), "SIMULATION", "intermediate_results", 
                                family, "interval_estimates",
                                "interval_estimate_%04d_%04d_%04d.RData"),
                      iter, m, uniform_cluster_size)
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