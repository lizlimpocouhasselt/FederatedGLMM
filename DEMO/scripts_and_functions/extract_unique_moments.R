extract_unique_moments <- function(H) {
  # moments up to 2nd order
  values <- H[, 1]
  mom.names <- rownames(H)

  # indexing variables
  idx <- sapply(c(' ', '^2'), grepl, rownames(H), fixed = T)

  # univariate (a3) 3rd moments
  a_idx <- rowSums(idx) == 0
  a_idx[1] <- FALSE #exclude '1'
  a2_idx <- as.vector(idx[, 2])
  values <- c(values, diag(H[a_idx, a2_idx]))
  mom.names <- c(mom.names, 
                  paste(rownames(H)[a_idx], rownames(H)[a2_idx]))

  # bivariate and trivariate 3rd moments
  ab_idx <- as.vector(idx[, 1])
  df <- data.frame(values = as.vector(H[a_idx, ab_idx]),
                    mom.names = as.vector(outer(rownames(H)[a_idx], rownames(H)[ab_idx], FUN = paste)))
  var.comp <- sapply(rownames(H)[a_idx], grepl, df$mom.names, fixed = T)
  if(length(rownames(H)[a_idx]) > 2){
    idx <- (!duplicated(apply(var.comp, 1, paste, collapse = ' '))) +
     (rowSums(var.comp) < 3)
  } else{idx <- rowSums(var.comp) < 3}
  df <- df[idx > 0, ]
  values <- c(values, df$values)
  mom.names <- c(mom.names, df$mom.names)
  
  # univariate 4th moments
  values <- c(values, diag(H[a2_idx, a2_idx]))
  mom.names <- c(mom.names, paste(rownames(H)[a2_idx], rownames(H)[a2_idx]))

  # bivariate (a3b) 4th moments
  a3b_idx <- sapply(rownames(H)[a_idx], grepl, rownames(H)[ab_idx])
  values <- c(values, H[ab_idx, a2_idx][a3b_idx])
  names.mat <- outer(rownames(H)[ab_idx], rownames(H)[a2_idx], FUN = paste)
  mom.names <- c(mom.names, names.mat[a3b_idx])
  
  # bivariate (a2b2), trivariate (abc2), and quadvariate 4th moments
  lt.idx <- lower.tri(H[ab_idx, ab_idx], diag = T) 
  df <- data.frame(values = H[ab_idx, ab_idx][lt.idx],
                    mom.names = (outer(rownames(H)[ab_idx], rownames(H)[ab_idx], FUN = paste))[lt.idx])
  var.comp <- sapply(rownames(H)[a_idx], grepl, df$mom.names, fixed = T)
  if(length(rownames(H)[a_idx]) > 3){
    idx <- (!duplicated(apply(var.comp, 1, paste, collapse = ' '))) +
     (rowSums(var.comp) < 4)
  } else{idx <- rowSums(var.comp) < 4}
  df <- df[idx > 0, ]
  values <- c(values, df$values)
  mom.names <- c(mom.names, df$mom.names)
  
  
  
  data.frame(
    mom.name = mom.names,
    value = values,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
