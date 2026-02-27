# Define objective function (only for family %in% c("binomial", "poisson"))
  obj_fn <- function(x, moment, var_names, n, p,
  uni_sum, covmat, bv_34 = NULL, tv_34 = NULL, qv_4 = NULL){

    unlist(c(
      # Define univariate constraints
      c(sapply(var_names, function(var_name){
        var_ind <- which(var_names == var_name)
        var_vec <- x[((var_ind-1) * n + 1):(n * var_ind)]
        x.row.ind <- uni_sum$variable == var_name
        x.std.mean <- uni_sum$std_mean[x.row.ind]
        x.std.var <- uni_sum$std_var[x.row.ind]
        c(
        mean(var_vec) - x.std.mean,
        mean(var_vec^2) - ((n - 1) * x.std.var + n * x.std.mean^2)/ n,
        if(moment >= 3) mean(var_vec^3, na.rm = TRUE) - 
                          uni_sum$std_mu_3[x.row.ind],
        if(moment == 4) mean(var_vec^4, na.rm = TRUE) -
                          uni_sum$std_mu_4[x.row.ind])
        })),
      # Define bivariate constraints
      if(p > 1){
        bv_names <- combn(var_names, 2)
        c(sapply(1:ncol(bv_names), function(col){
          var1_name <- bv_names[1, col]
          var2_name <- bv_names[2, col]
          var_ind1 <- which(var_names == var1_name)
          var_ind2 <- which(var_names == var2_name)
          var_vec1 <- x[((var_ind1-1) * n + 1):(n * var_ind1)]
          var_vec2 <- x[((var_ind2-1) * n + 1):(n * var_ind2)]
          target_cov <- covmat[var1_name, var2_name]
          v1.std.mean <- uni_sum$std_mean[uni_sum$variable == var1_name]
          v2.std.mean <- uni_sum$std_mean[uni_sum$variable == var2_name]
          bv.con <- mean(var_vec1 * var_vec2) -
           (target_cov * (n-1) + n * v1.std.mean * v2.std.mean)/n
          if(moment >= 3){
            bivars <- bv_34$vars
            row.idx <- grepl(var1_name, bivars, fixed = TRUE) &
                        grepl(var2_name, bivars, fixed = TRUE)
            target_a2b <- bv_34$a2b[row.idx]
            target_ab2 <- bv_34$ab2[row.idx]
            bv.con <- c(bv.con,
              mean(var_vec1^2 * var_vec2, na.rm = TRUE) - target_a2b,
              mean(var_vec1 * var_vec2^2, na.rm = TRUE) - target_ab2
            )
            if(moment == 4){
              target_a3b <- bv_34$a3b[row.idx]
              target_ab3 <- bv_34$ab3[row.idx]
              target_a2b2 <- bv_34$a2b2[row.idx]
              bv.con <- c(bv.con,
                mean(var_vec1^3 * var_vec2, na.rm = TRUE) - target_a3b,
                mean(var_vec1 * var_vec2^3, na.rm = TRUE) - target_ab3,
                mean(var_vec1^2 * var_vec2^2, na.rm = TRUE) - target_a2b2
              )
            }
          }
          bv.con
          }))
      },
      # Define trivariate constraints
      if(p > 2){
        tv_names <- combn(var_names, 3)
        c(sapply(1:ncol(tv_names), function(col){
          var1_name <- tv_names[1, col]
          var2_name <- tv_names[2, col]
          var3_name <- tv_names[3, col]
          var_ind1 <- which(var_names == var1_name)
          var_ind2 <- which(var_names == var2_name)
          var_ind3 <- which(var_names == var3_name)
          var_vec1 <- x[((var_ind1-1)*n + 1):(n*var_ind1)]
          var_vec2 <- x[((var_ind2-1)*n + 1):(n*var_ind2)]
          var_vec3 <- x[((var_ind3-1)*n + 1):(n*var_ind3)]
          trivars <- tv_34$vars
          row.idx <- grepl(var1_name, trivars, fixed = TRUE) &
                      grepl(var2_name, trivars, fixed = TRUE) &
                      grepl(var3_name, trivars, fixed = TRUE)
          if(moment >= 3){
            target_abc <- tv_34$abc[row.idx]
            tv.con <- mean(var_vec1 * var_vec2 * var_vec3) - target_abc
            if(moment == 4){
              target_a2bc <- tv_34$a2bc[row.idx]
              target_ab2c <- tv_34$ab2c[row.idx]
              target_abc2 <- tv_34$abc2[row.idx]
              tv.con <- c(tv.con,
                  mean(var_vec1^2 * var_vec2 * var_vec3) - target_a2bc,
                  mean(var_vec1 * var_vec2^2 * var_vec3) - target_ab2c,
                  mean(var_vec1 * var_vec2 * var_vec3^2) - target_abc2
                  )
            }
            tv.con
          }
        }))
      },
      # Define quadvariate constraints
      if(p > 3 & moment == 4){
        qv_names <- combn(var_names, 4)
        c(sapply(1:ncol(qv_names), function(col){
          var1_name <- qv_names[1, col]
          var2_name <- qv_names[2, col]
          var3_name <- qv_names[3, col]
          var4_name <- qv_names[4, col]
          var_ind1 <- which(var_names == var1_name)
          var_ind2 <- which(var_names == var2_name)
          var_ind3 <- which(var_names == var3_name)
          var_ind4 <- which(var_names == var4_name)
          var_vec1 <- x[((var_ind1-1)*n + 1):(n*var_ind1)]
          var_vec2 <- x[((var_ind2-1)*n + 1):(n*var_ind2)]
          var_vec3 <- x[((var_ind3-1)*n + 1):(n*var_ind3)]
          var_vec4 <- x[((var_ind4-1)*n + 1):(n*var_ind4)]
          quadvars <- qv_4$vars
          row.idx <- grepl(var1_name, quadvars, fixed = TRUE) &
                      grepl(var2_name, quadvars, fixed = TRUE) &
                      grepl(var3_name, quadvars, fixed = TRUE) &
                      grepl(var4_name, quadvars, fixed = TRUE)
          target_abcd <- qv_4$abcd[row.idx]
          mean(var_vec1 * var_vec2 * var_vec3 * var_vec4) - target_abcd
        }))
      }
    ))
  }