fn_compute_summary <- function(iter, m, uniform_cluster_size, seed, simdata){
  
  # Prepare functions to compute moments
  mv_moment_3_4_bypair <- function(a,b,n){
    a2b <- mean(a^2 * b)
    ab2 <- mean(a * b^2)
    ab3 <- mean(a * b^3)
    a2b2 <- mean(a^2 * b^2)
    a3b <- mean(a^3 * b)
    return(list(a2b=a2b, ab2=ab2, ab3=ab3, a2b2=a2b2, a3b=a3b))
  }
  mv_moment_3_4_by3 <- function(a,b,c,n){
    abc <- mean(a * b * c)
    a2bc <- mean(a^2 * b * c)
    ab2c <- mean(a * b^2 * c)
    abc2 <- mean(a * b * c^2)
    return(list(abc=abc, a2bc=a2bc, ab2c=ab2c, abc2=abc2))
  }
  mv_moment_4 <- function(a,b,c,d,n){
    abcd <- mean(a * b * c * d)
    abcd
  }
  

  
  # Prepare summary statistics (data provider task) ------------------
  # Identify numeric variables
  numeric_var_names <- c('x1', 'x4', 'y', 'log_yfac')
  # numeric_var_names <- c('x1', 'y')
  
  
  # Break up data per group
  group_data_design_df <- lapply(1:m, function(group_num){
    df <- simdata %>% filter(g == group_num)
    df$log_yfac <- log(factorial(df$y))
    df$x3 <- factor(df$x3)
    as.data.frame(cbind(model.matrix(~-1+., df[,!(names(df) == 'g')]), g=df$g))
  })
  
  mean_cov <- lapply(1:m, function(group_num){
    x3.vals <- setdiff(names(group_data_design_df[[group_num]]), c('x1', 'x2', 'x4', 'x5', 'log_yfac', 'y', 'g'))
    type <- c('num', 'bin', rep('bin', length(x3.vals)), 'num', 'bin', 'num', 'num')
    df <- group_data_design_df[[group_num]]
    df <- df[, names(df) != 'g']
    sumdf1 <- data.frame(
      variable = names(df),
      type = type,
      n = uniform_cluster_size,
      mean = apply(df, 2, mean, na.rm = TRUE),
      variance = apply(df, 2, var),
      target_3_moment = apply(df, 2, function(x) mean(x^3)),
      target_4_moment = apply(df, 2, function(x) mean(x^4)),
      row.names = NULL)
    sumdf2 <- as.data.frame(
      cbind(sumdf1,
            std_mean = rep(0, ncol(df)),
            std_variance = rep(1, ncol(df)),
            std_target_3_moment = apply(df, 2, function(x) mean(scale(x)^3)),
            std_target_4_moment = apply(df, 2, function(x) mean(scale(x)^4))),
      row.names = NULL)
    sumdf2
  })
  
  numeric_var_names <- mean_cov[[1]][mean_cov[[1]][,'type'] == 'num', 'variable']
  
  
  var_cov_mat <- lapply(1:m, function(group_num){
    df <- group_data_design_df[[group_num]]
    df <- df[, names(df) != 'g']
    var_cov_mat_ <- cov(sapply(names(df), function(name){scale(df[,names(df)==name])}))
    colnames(var_cov_mat_) <- rownames(var_cov_mat_) <- names(df)
    var_cov_mat_
  })
  
  
  mv_moment_3_4_bypair_df <- lapply(1:m, function(group_num){
    df <- group_data_design_df[[group_num]]
    df <- df[, names(df) != 'g']
    xy <- combn(names(df)[!(names(df) %in% c('y','log_yfac','g'))], 2) 
    data.frame(
      vars = apply(xy, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2])),
      a2b = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(scale(df[, xy[1,x]]), scale(df[, xy[2,x]]), n)$a2b),
      ab2 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(scale(df[, xy[1,x]]), scale(df[, xy[2,x]]), n)$ab2),
      a3b = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(scale(df[, xy[1,x]]), scale(df[, xy[2,x]]), n)$a3b),
      a2b2 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(scale(df[, xy[1,x]]), scale(df[, xy[2,x]]), n)$a2b2),
      ab3 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(scale(df[, xy[1,x]]), scale(df[, xy[2,x]]), n)$ab3)
    )
  })
  
  mv_moment_3_4_by3_df <- lapply(1:m, function(group_num){
    df <- group_data_design_df[[group_num]]
    df <- df[, names(df) != 'g']
    xyz <- combn(names(df)[!(names(df) %in% c('y','log_yfac','g'))], 3) 
    data.frame(
      vars = apply(xyz, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2],"_", t(x)[, 3])),
      abc = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(scale(df[, xyz[1,x]]), scale(df[, xyz[2,x]]), scale(df[, xyz[3,x]]), n)$abc),
      a2bc = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(scale(df[, xyz[1,x]]), scale(df[, xyz[2,x]]), scale(df[, xyz[3,x]]), n)$a2bc),
      ab2c = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(scale(df[, xyz[1,x]]), scale(df[, xyz[2,x]]), scale(df[, xyz[3,x]]), n)$ab2c),
      abc2 = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(scale(df[, xyz[1,x]]), scale(df[, xyz[2,x]]), scale(df[, xyz[3,x]]), n)$abc2))
  })
  
  mv_moment_4_df <- lapply(1:m, function(group_num){
    df <- group_data_design_df[[group_num]]
    df <- df[, names(df) != 'g']
    wxyz <- combn(names(df)[!(names(df) %in% c('y','log_yfac','g'))], 4) 
    data.frame(
      vars = apply(wxyz, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2],"_", t(x)[, 3], "_", t(x)[, 4])),
      abcd = sapply(1:dim(wxyz)[2], function(x) mv_moment_4(scale(df[, wxyz[1,x]]), scale(df[, wxyz[2,x]]), scale(df[, wxyz[3,x]]), scale(df[, wxyz[4,x]]), n))
    )
  })
  
  
  return(list(mean_cov,
              var_cov_mat,
              mv_moment_3_4_bypair_df,
              mv_moment_3_4_by3_df,
              mv_moment_4_df))
}