#-----------------------------------------
# DATA ANALYST TASK
# GENERATE PSEUDO-DATA (GENERIC)
#-----------------------------------------
# Performs the same floating-point operations as gen_pseudo(moment = 4) with obj_fn,
# lsqnonlin_2 and pracma::jacobian, so output is bitwise identical to that code on the
# same machine/BLAS. Speed-ups: moment positions are precomputed instead of re-derived
# from names, each finite-difference column only rewrites the perturbed row, the Jacobian
# is only formed for accepted steps, and chunks of all groups run in parallel.
#
# Env vars: FGLMM_CORES (default: all cores), FGLMM_PS_DIR (Drive output dir; groups
# already present there are skipped).

suppressPackageStartupMessages({
  library(pracma)
  library(dplyr)
  library(tidyr)
  library(randtoolbox)
  library(parallel)
})
source(file.path(getwd(), 'R_common', 'remote_store.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'extract_unique_moments.R'))
source(file.path(getwd(), 'DEMO', 'scripts_and_functions', 'construct_hankel.R'))

keep_vars <- c('Length.of.Stay', 'Total.Charges', 'Gender_M', 'COVID19_positive', 'Emergency.Department.Indicator_Y')

base_names <- function(nm) nm[!rowSums(sapply(c('^', ' '), grepl, nm, fixed = TRUE)) > 0]

# Linear indices of the entries of H returned by extract_unique_moments(H), in its order
moment_pos <- function(H) {
  as.integer(extract_unique_moments(matrix(seq_along(H), nrow(H), dimnames = dimnames(H)))$value)
}

gen_pseudo_chunk <- function(unsc.H, sc.H, maxeval = 700, tau = 1e-3, tolx = 1e-8, tolg = 1e-8) {
  all.vars <- base_names(rownames(unsc.H))
  var.names <- all.vars[all.vars %in% keep_vars]
  n <- unsc.H[1, 1]
  p <- length(var.names)
  n.sp <- sc.H[1, 1]

  # Unstandardized mean and variance
  idx <- rownames(unsc.H) %in% var.names
  unsc.mean <- unsc.H[idx, 1] / n
  unsc.var <- diag((unsc.H[idx, idx] - n * outer(unsc.mean, unsc.mean, FUN = '*')) / (n - 1))

  # Target moments: standardized Hankel matrix restricted to var.names
  exc.idx <- setdiff(all.vars, var.names)[-1]
  H.idx <- !rowSums(sapply(exc.idx, grepl, rownames(sc.H), fixed = TRUE)) > 0
  H <- sc.H[H.idx, H.idx]
  tv <- H[moment_pos(H)]
  tgt <- tv / tv[1]

  # Sobol starting support on [-15, 15]^p, equal fixed weights
  lower <- rep(-15, p)
  upper <- rep(15, p)
  U <- sobol(n = n.sp, dim = p)
  x <- as.vector(t(t(U) * (upper - lower) + lower))
  sw <- sqrt(rep(1, n.sp) / tv[1])

  # Columns of construct_hankel's design: 1, x, x^2, pairwise products (combn order)
  pr <- combn(p, 2)
  i1 <- pr[1, ]
  i2 <- pr[2, ]
  pos <- moment_pos(construct_hankel(matrix(x, n.sp, p, dimnames = list(NULL, var.names)), rep(1, n.sp)))
  build_V <- function(X) cbind(1, X, X^2, X[, i1, drop = FALSE] * X[, i2, drop = FALSE]) * sw
  fun <- function(x) crossprod(build_V(matrix(x, n.sp, p)))[pos] - tgt

  # Central differences as in pracma::jacobian; only row j of V depends on x[(a - 1) * n.sp + j]
  heps <- .Machine$double.eps^(1/3)
  dep <- lapply(seq_len(p), function(a) {
    k <- which(i1 == a | i2 == a)
    list(cols = c(1 + a, 1 + p + a, 1 + 2 * p + k), other = ifelse(i1[k] == a, i2[k], i1[k]))
  })
  jac <- function(x) {
    X <- matrix(x, n.sp, p)
    V <- build_V(X)
    J <- matrix(NA_real_, length(tgt), length(x))
    for (a in seq_len(p)) {
      cols <- dep[[a]]$cols
      other <- dep[[a]]$other
      for (j in seq_len(n.sp)) {
        k <- (a - 1L) * n.sp + j
        v0 <- V[j, cols]
        xo <- X[j, other]
        xa <- x[k] + heps
        V[j, cols] <- c(xa, xa^2, xa * xo) * sw[j]
        fp <- crossprod(V)[pos] - tgt
        xa <- x[k] - heps
        V[j, cols] <- c(xa, xa^2, xa * xo) * sw[j]
        J[, k] <- (fp - (crossprod(V)[pos] - tgt)) / (2 * heps)
        V[j, cols] <- v0
      }
    }
    J
  }

  # Levenberg-Marquardt exactly as lsqnonlin_2 (incl. returning the last proposed step)
  r <- fun(x)
  J <- jac(x)
  tJ <- t(J)
  g <- tJ %*% r
  ng <- Norm(g, Inf)
  A <- tJ %*% J
  rm(J, tJ)
  mu <- tau * max(diag(A))
  nu <- 2
  k <- 1
  ssq <- 1
  counter <- 0
  while (k < maxeval & ssq > 1e-16) {
    prev_ssq <- ssq
    k <- k + 1
    Amu <- A
    diag(Amu) <- diag(A) + mu  # == A + mu * eye(n)
    R <- tryCatch(chol(Amu), error = function(e) NULL)
    rm(Amu)
    if (is.null(R)) {
      message("Stopped: Cholesky failed at iteration ", k)
      xnew <- x
      break
    }
    h <- c(-t(g) %*% chol2inv(R))
    rm(R)
    if (Norm(h) <= tolx * (tolx + Norm(x))) {
      xnew <- x
      break
    }
    xnew <- x + h
    h <- xnew - x
    dL <- sum(h * (mu * h - g)) / 2
    rn <- fun(xnew)
    df <- sum((r - rn) * (r + rn)) / 2
    if (dL > 0 && df > 0) {
      x <- xnew
      r <- rn
      J <- jac(xnew)
      tJ <- t(J)
      A <- tJ %*% J
      g <- tJ %*% r
      rm(J, tJ)
      ng <- Norm(g, Inf)
      mu <- mu * max(1/3, 1 - (2 * df / dL - 1)^3)
      nu <- 2
    } else {
      mu <- mu * nu
      nu <- 2 * nu
    }
    if (ng <= tolg) break
    ssq_new <- sum(rn^2)
    if (ssq_new / prev_ssq >= 0.99) {
      counter <- counter + 1
      if (counter > 19) break
    } else {
      counter <- 0
    }
    ssq <- ssq_new
  }
  rm(A)

  # Unscale pseudo-data
  syn <- matrix(xnew[1:(n.sp * p)], ncol = p, nrow = n.sp)
  colnames(syn) <- var.names
  unsc.syn.df <- as.data.frame(t(t(syn) * sqrt(unsc.var) + unsc.mean))
  names(unsc.syn.df) <- var.names
  unsc.syn.df
}

# ---- Run ----
summary_dir <- file.path("DEMO", "intermediate_results", "summary_info")
ps_dir <- Sys.getenv("FGLMM_PS_DIR", file.path("DEMO", "intermediate_results", "ps"))
n_cores <- as.integer(Sys.getenv("FGLMM_CORES", detectCores()))

info_files <- run_rclone(c("lsf", "--files-only", "--include", "summary_info_*.RData", remote_path(summary_dir)))
if (length(info_files) == 0L) stop("No summary_info files found on Drive: ", summary_dir)
grps <- sort(as.integer(sub("^summary_info_(\\d+)\\.RData$", "\\1", info_files)))
done <- tryCatch(run_rclone(c("lsf", "--files-only", remote_path(ps_dir))), error = function(e) character(0))
todo <- grps[!sprintf("ps_%04d.RData", grps) %in% done]
cat(sprintf("%d groups, %d already in %s, %d to generate on %d cores\n",
            length(grps), length(grps) - length(todo), ps_dir, length(todo), n_cores))

if (length(todo) > 0L) {
  # One bulk download of the needed summary_info files to a session temp dir
  local_dir <- file.path(tempdir(), "summary_info")
  dir.create(local_dir, showWarnings = FALSE)
  list_file <- tempfile(fileext = ".txt")
  writeLines(sprintf("summary_info_%04d.RData", todo), list_file)
  run_rclone(c("copy", "--files-from", list_file, remote_path(summary_dir), local_dir))
  load_info <- function(g) {
    e <- new.env()
    load(file.path(local_dir, sprintf("summary_info_%04d.RData", g)), envir = e)
    e$summary_info
  }

  # Batch groups so each batch keeps all cores busy; groups are uploaded after each batch
  n_chunks <- vapply(todo, function(g) length(load_info(g)), integer(1))
  batch <- integer(length(todo))
  b <- 1L
  acc <- 0L
  for (i in seq_along(todo)) {
    batch[i] <- b
    acc <- acc + n_chunks[i]
    if (acc >= 4L * n_cores) {
      b <- b + 1L
      acc <- 0L
    }
  }

  for (b in unique(batch)) {
    t0 <- Sys.time()
    bg <- todo[batch == b]
    info <- setNames(lapply(bg, load_info), bg)
    tasks <- do.call(rbind, lapply(bg, function(g) {
      data.frame(g = g, chunk = seq_along(info[[as.character(g)]]),
                 n = vapply(info[[as.character(g)]], function(s) s[[2]][1, 1], numeric(1)))
    }))
    tasks <- tasks[order(-tasks$n), ]  # largest chunks first
    res <- mclapply(seq_len(nrow(tasks)), function(i) {
      s <- info[[as.character(tasks$g[i])]][[tasks$chunk[i]]]
      gen_pseudo_chunk(s[[1]], s[[2]])
    }, mc.cores = n_cores, mc.preschedule = FALSE)
    failed <- vapply(res, inherits, logical(1), what = "try-error")
    if (any(failed)) stop(sprintf("grp %d chunk %d failed: %s", tasks$g[which(failed)[1]],
                                  tasks$chunk[which(failed)[1]], res[[which(failed)[1]]]))
    rm(info)

    for (g in bg) {
      sel <- which(tasks$g == g)
      ps <- bind_rows(res[sel[order(tasks$chunk[sel])]]) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))
      ps$g <- as.numeric(g)
      remote_save(ps, key = file.path(ps_dir, sprintf("ps_%04d.RData", g)))
      cat(sprintf("grp %d finished (%d chunks, %d rows)\n", g, length(sel), nrow(ps)))
    }
    rm(res, ps)
    cat(sprintf("batch %d/%d done in %.1f min\n", b, max(batch), as.numeric(Sys.time() - t0, units = "mins")))
  }
}