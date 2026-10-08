source(file.path(getwd(), 'R_common', 'remote_store.R'))

run_pseudodata <- function(moment, input_suffix = "", output_suffix = "") {
  if (!(moment %in% c(2, 3, 4))) {
    stop("moment must be one of 2, 3, or 4")
  }

  par_settings <- read.csv(file.path(getwd(), 'SIMULATION', 'par_settings.csv'))

  mean_cov_dir <- paste0('mean_cov', input_suffix)
  var_cov_dir <- paste0('var_cov_mat', input_suffix)
  pair_dir <- paste0('mv_moment_3_4_bypair_df', input_suffix)
  by3_dir <- paste0('mv_moment_3_4_by3_df', input_suffix)
  m4_dir <- paste0('mv_moment_4_df', input_suffix)

  pseudodata_name <- switch(as.character(moment),
    '2' = 'pseudodata_2ndmom',
    '3' = 'pseudodata_3rdmom',
    '4' = 'pseudodata_4thmom'
  )
  output_dir <- switch(as.character(moment),
    '2' = paste0('ps2', output_suffix),
    '3' = paste0('ps3', output_suffix),
    '4' = paste0('ps4', output_suffix)
  )

  lapply(seq_len(nrow(par_settings)), function(row) {
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]

    remote_load(file.path('SIMULATION', 'intermediate_results', 'poisson', mean_cov_dir,
                          sprintf('mean_cov_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
    remote_load(file.path('SIMULATION', 'intermediate_results', 'poisson', var_cov_dir,
                          sprintf('var_cov_mat_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))

    if (moment >= 3) {
      remote_load(file.path('SIMULATION', 'intermediate_results', 'poisson', pair_dir,
                            sprintf('mv_moment_3_4_bypair_df_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
      remote_load(file.path('SIMULATION', 'intermediate_results', 'poisson', by3_dir,
                            sprintf('mv_moment_3_4_by3_df_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
    }
    if (moment == 4) {
      remote_load(file.path('SIMULATION', 'intermediate_results', 'poisson', m4_dir,
                            sprintf('mv_moment_4_df_%04d_%04d_%04d.RData', iter, m, uniform_cluster_size)))
    }

    set.seed(seed)
    pseudodata <- lapply(1:m, function(group_num) {
      var_names <- mean_cov[[group_num]][, 'variable']
      pseudo <- if (moment == 2) {
        gen_pseudo(moment = 2, var_names = var_names,
                   uni_sum = mean_cov[[group_num]], covmat = var_cov_mat[[group_num]])
      } else if (moment == 3) {
        gen_pseudo(moment = 3, var_names = var_names,
                   uni_sum = mean_cov[[group_num]], covmat = var_cov_mat[[group_num]],
                   bv_34 = mv_moment_3_4_bypair_df[[group_num]],
                   tv_34 = mv_moment_3_4_by3_df[[group_num]])
      } else {
        gen_pseudo(moment = 4, var_names = var_names,
                   uni_sum = mean_cov[[group_num]], covmat = var_cov_mat[[group_num]],
                   bv_34 = mv_moment_3_4_bypair_df[[group_num]],
                   tv_34 = mv_moment_3_4_by3_df[[group_num]],
                   qv_4 = mv_moment_4_df[[group_num]])
      }
      pseudo$g <- group_num
      cat('grp ', group_num, ' is finished')
      pseudo
    })

    assign(pseudodata_name, pseudodata, envir = .GlobalEnv)
    filename_save <- file.path('SIMULATION', 'intermediate_results', 'poisson', output_dir,
                               sprintf('%s_%04d_%04d_%04d', pseudodata_name, iter, m, uniform_cluster_size))
    remote_save(list = pseudodata_name, key = filename_save, envir = .GlobalEnv)
  })
}
