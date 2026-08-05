# make_A <- function(X, mom.names){
#   if (!is.matrix(X)) X <- as.matrix(X)
#   var.names <- colnames(X)
#   if (is.null(var.names)) var.names <- paste0("x", seq_len(p))

#   idx <- sapply(c(' ', '^2'), grepl, mom.names, fixed = T)

#   split_idx <- (idx[, 1]) & (rowSums(idx) == 1) #no powers '^2'
#   split_names <- strsplit(mom.names[split_idx], ' ')
#   mv_cols_ls <- lapply(split_names, function(colnames) rowProds(X[, colnames]))
#   mv_cols <- do.call(cbind, mv_cols_ls)

#   split2_idx <- rowSums(idx) == 2 #multivariate with powers '^2'
#   split2_names <- strsplit(mom.names[split2_idx], ' ')
#   mv2_cols_ls <- lapply(split2_names, function(names) {
#     sq_idx <- which(grepl('^2', names, fixed = T))
#     names[sq_idx] <- unlist(strsplit(names[sq_idx], '^2', fixed = T))
#     rowProds(X[, c(names, names[sq_idx])])
#     })
#   mv2_cols <- do.call(cbind, mv2_cols_ls)

#   # Construct A
#   A <- matrix(0, ncol = length(mom.names), nrow = nrow(X))
#   A[, 1:(2 * ncol(X) + 1)] <- cbind(`1` = 1, X, X^2)
#   A[, split_idx] <- mv_cols
#   A[, split2_idx] <- mv2_cols
  
#   colnames(A) <- mom.names
#   A
# }
make_A <- function(X, mom.names){
  if (!is.matrix(X)) X <- as.matrix(X)
  var.names <- colnames(X)
  if (is.null(var.names)) var.names <- paste0("x", seq_len(p))

  idx <- sapply(c(' ', '^2'), grepl, mom.names, fixed = T)

  split_idx <- (idx[, 1]) & (rowSums(idx) == 1) #no powers '^2'
  split_names <- strsplit(mom.names[split_idx], ' ')
  mv_cols_ls <- lapply(split_names, function(colnames) if(nrow(X) > 1) rowProds(X[, colnames]) else prod(X[, colnames]))
  mv_cols <- do.call(cbind, mv_cols_ls)

  split2_idx <- rowSums(idx) == 2 #multivariate with powers '^2'
  split2_names <- strsplit(mom.names[split2_idx], ' ')
  mv2_cols_ls <- lapply(split2_names, function(names) {
    sq_idx <- which(grepl('^2', names, fixed = T))
    names[sq_idx] <- unlist(strsplit(names[sq_idx], '^2', fixed = T))
    if(nrow(X) > 1) rowProds(X[, c(names, names[sq_idx])]) else prod(X[, c(names, names[sq_idx])])
    })
  mv2_cols <- do.call(cbind, mv2_cols_ls)

  # Construct A
  A <- matrix(0, ncol = length(mom.names), nrow = nrow(X))
  A[, 1:(2 * ncol(X) + 1)] <- cbind(`1` = 1, X, X^2)
  A[, split_idx] <- mv_cols
  A[, split2_idx] <- mv2_cols
  
  colnames(A) <- mom.names
  A
}