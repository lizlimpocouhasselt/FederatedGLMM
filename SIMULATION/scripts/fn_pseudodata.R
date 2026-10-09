source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'SIMULATION', 'scripts', 'fn_run_rows.R'))
run_by_row <- get('run_by_row', mode = 'function')
sim_log <- get('sim_log', mode = 'function')
if (exists('gen_pseudo', mode = 'function')) {
  gen_pseudo <- get('gen_pseudo', mode = 'function')
}

pseudodata_log_interval <- function(total_groups) {
  max(1L, ceiling(total_groups / 5L))
}

run_pseudodata <- function(moment, input_suffix = "", output_suffix = "", max_iter = NULL) {
  if (!(moment %in% c(2, 3, 4))) {
    stop("moment must be one of 2, 3, or 4")
  }

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

  inputs <- list(
    mean_cov = list(dir = mean_cov_dir, prefix = 'mean_cov'),
    var_cov_mat = list(dir = var_cov_dir, prefix = 'var_cov_mat')
  )
  if (moment >= 3) {
    inputs$mv_moment_3_4_bypair_df <- list(dir = pair_dir, prefix = 'mv_moment_3_4_bypair_df')
    inputs$mv_moment_3_4_by3_df <- list(dir = by3_dir, prefix = 'mv_moment_3_4_by3_df')
  }
  if (moment == 4) {
    inputs$mv_moment_4_df <- list(dir = m4_dir, prefix = 'mv_moment_4_df')
  }

  outputs <- list()
  outputs[[pseudodata_name]] <- list(dir = output_dir, prefix = pseudodata_name)

  run_by_row(
    step = pseudodata_name,
    inputs = inputs,
    outputs = outputs,
    max_iter = max_iter,
    worker = function(row, env) {
      out <- list()
      out[[pseudodata_name]] <- gen_pseudodata_row(moment, row, env)
      out
    }
  )
}

gen_pseudodata_row <- function(moment, row, env) {
  mean_cov <- env$mean_cov
  var_cov_mat <- env$var_cov_mat
  mv_moment_3_4_bypair_df <- env$mv_moment_3_4_bypair_df
  mv_moment_3_4_by3_df <- env$mv_moment_3_4_by3_df
  mv_moment_4_df <- env$mv_moment_4_df

  set.seed(row$seed)
  total_groups <- row$m
  log_every <- pseudodata_log_interval(total_groups)
  pseudodata <- lapply(seq_len(row$m), function(group_num) {
    if (group_num == 1L || group_num == total_groups || (group_num %% log_every) == 0L) {
      sim_log(env$step, sprintf("moment %s: processing group %d/%d", moment, group_num, total_groups), row = row$idx)
    }
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
    pseudo
  })

  names(pseudodata) <- paste0('g', seq_along(pseudodata))
  pseudodata
}
