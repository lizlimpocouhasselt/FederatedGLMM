if (!exists("remote_exists", mode = "function")) {
  source(file.path(getwd(), "R_common", "remote_store.R"))
}
remote_exists <- get("remote_exists", mode = "function")
remote_pull <- get("remote_pull", mode = "function")
remote_push <- get("remote_push", mode = "function")

sim_log <- function(step, ..., row = NULL) {
  parts <- c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sprintf("[%s]", step))
  if (!is.null(row)) {
    parts <- c(parts, sprintf("[row %s]", row))
  }
  message(paste(parts, collapse = " "), " ", paste0(..., collapse = ""))
}

parse_sim_rows <- function(value) {
  value <- trimws(value)
  if (!nzchar(value)) {
    return(integer(0))
  }

  parts <- strsplit(value, ",", fixed = TRUE)[[1]]
  values <- integer(0)
  for (part in parts) {
    part <- trimws(part)
    if (!nzchar(part)) {
      next
    }
    if (grepl(":", part, fixed = TRUE)) {
      range_vals <- strsplit(part, ":", fixed = TRUE)[[1]]
      start <- suppressWarnings(as.integer(range_vals[1]))
      end <- suppressWarnings(as.integer(range_vals[2]))
      if (is.na(start) || is.na(end) || start > end) {
        stop("Invalid SIM_ROWS range: ", part)
      }
      values <- c(values, seq.int(start, end))
    } else {
      v <- suppressWarnings(as.integer(part))
      if (is.na(v)) {
        stop("Invalid SIM_ROWS value: ", part)
      }
      values <- c(values, v)
    }
  }
  sort(unique(values))
}

row_file_key <- function(spec, row) {
  if (is.character(spec)) {
    spec <- list(dir = "", prefix = spec)
  }
  if (is.null(spec$dir) || is.na(spec$dir)) {
    spec$dir <- ""
  }
  if (is.null(spec$prefix) || is.na(spec$prefix) || !nzchar(spec$prefix)) {
    stop("Each row spec must include a non-empty prefix.")
  }

  iter <- row$iter
  m <- row$m
  uniform_cluster_size <- row$uniform_cluster_size
  file_name <- sprintf("%s_%04d_%04d_%04d.RData", spec$prefix, iter, m, uniform_cluster_size)
  if (nzchar(spec$dir)) {
    file.path("SIMULATION", "intermediate_results", "poisson", spec$dir, file_name)
  } else {
    file.path("SIMULATION", "intermediate_results", "poisson", file_name)
  }
}

row_local_file <- function(spec, row, local_root) {
  if (is.null(spec$dir) || is.na(spec$dir) || !nzchar(spec$dir)) {
    dir_part <- ""
  } else {
    dir_part <- spec$dir
  }
  file_name <- sprintf("%s_%04d_%04d_%04d.RData", spec$prefix, row$iter, row$m, row$uniform_cluster_size)
  file.path(local_root, dir_part, file_name)
}

load_inputs_for_row <- function(row, inputs, env, local_root) {
  for (spec_name in names(inputs)) {
    spec <- inputs[[spec_name]]
    local_file <- row_local_file(spec, row, local_root)
    if (!file.exists(local_file)) {
      stop("Missing local input for row ", row$iter, ": ", local_file)
    }
    load(local_file, envir = env)
  }
}

save_outputs_for_row <- function(row, outputs, env, local_root) {
  saved <- character(0)
  for (nm in names(outputs)) {
    spec <- outputs[[nm]]
    local_file <- row_local_file(spec, row, local_root)
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    if (!exists(nm, mode = "any", envir = env)) {
      stop("Output object not found in worker: ", nm)
    }
    save(list = nm, file = local_file, envir = env)
    saved <- c(saved, local_file)
  }
  invisible(saved)
}

run_by_row <- function(step, inputs = list(), outputs = list(), worker, max_iter = NULL) {
  if (missing(worker) || !is.function(worker)) {
    stop("A worker function is required for run_by_row().")
  }

  if (!file.exists(file.path(getwd(), "SIMULATION", "par_settings.csv"))) {
    stop("SIMULATION/par_settings.csv is required for run_by_row().")
  }

  par_settings <- read.csv(file.path(getwd(), "SIMULATION", "par_settings.csv"), stringsAsFactors = FALSE)
  available_rows <- seq_len(nrow(par_settings))
  if (!is.null(max_iter)) {
    max_iter <- suppressWarnings(as.integer(max_iter))
    if (is.na(max_iter) || max_iter < 1L) {
      stop("max_iter must be a positive integer when provided.")
    }
    available_rows <- which(par_settings$iter <= max_iter)
    if (!length(available_rows)) {
      stop("No simulation rows matched max_iter=", max_iter)
    }
  }

  selected_rows <- parse_sim_rows(Sys.getenv("SIM_ROWS", unset = ""))
  if (length(selected_rows) == 0L) {
    selected_rows <- available_rows
  } else {
    selected_rows <- intersect(selected_rows, available_rows)
    if (!length(selected_rows)) {
      stop("No simulation rows matched SIM_ROWS=", Sys.getenv("SIM_ROWS", unset = ""))
    }
  }
  sim_log(step, sprintf("selected %d row(s): %s", length(selected_rows), paste(selected_rows, collapse = ", ")))

  cores <- as.integer(Sys.getenv("FGLMM_CORES", unset = ""))
  if (is.na(cores) || cores < 1L) {
    cores <- if (.Platform$OS.type == "windows") 1L else max(1L, min(parallel::detectCores(), 4L))
  }
  if (.Platform$OS.type == "windows") {
    cores <- 1L
  }
  batch_size <- max(1L, 4L * cores)
  sim_log(step, sprintf("using %d core(s); batch size %d", cores, batch_size))

  if (Sys.getenv("SIM_REBUILD", unset = "") == "1") {
    sim_log(step, "SIM_REBUILD=1 forcing all rows to be rebuilt.")
  }

  filtered_rows <- integer(0)
  for (idx in selected_rows) {
    row <- par_settings[idx, , drop = FALSE]
    all_outputs_exist <- TRUE
    for (nm in names(outputs)) {
      spec <- outputs[[nm]]
      remote_key <- row_file_key(spec, row)
      if (!remote_exists(remote_key)) {
        all_outputs_exist <- FALSE
        break
      }
    }
    if (Sys.getenv("SIM_REBUILD", unset = "") == "1" || !all_outputs_exist) {
      filtered_rows <- c(filtered_rows, idx)
    }
  }

  if (!length(filtered_rows)) {
    sim_log(step, "all selected rows already exist on Drive; nothing to do.")
    return(invisible(integer(0)))
  }
  sim_log(step, sprintf("queued %d row(s) for execution: %s", length(filtered_rows), paste(filtered_rows, collapse = ", ")))

  batch_number <- 0L
  batch_total <- ceiling(length(filtered_rows) / batch_size)
  for (start_idx in seq(1, length(filtered_rows), by = batch_size)) {
    batch_number <- batch_number + 1L
    batch <- filtered_rows[start_idx:min(start_idx + batch_size - 1L, length(filtered_rows))]
    local_root <- tempfile("fglmm_batch_")
    dir.create(local_root, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(local_root, recursive = TRUE, force = TRUE), add = TRUE)
    sim_log(step, sprintf("starting batch %d/%d for row(s): %s", batch_number, batch_total, paste(batch, collapse = ", ")))

    for (spec_name in names(inputs)) {
      spec <- inputs[[spec_name]]
      file_names <- vapply(batch, function(row_idx) {
        row <- par_settings[row_idx, , drop = FALSE]
        remote_key <- row_file_key(spec, row)
        if (!remote_exists(remote_key)) {
          stop(sprintf("[%s] missing required input for row %s: %s", step, row_idx, remote_key))
        }
        basename(remote_key)
      }, character(1))
      if (length(file_names) == 0L) {
        next
      }
      local_target <- file.path(local_root, spec$dir)
      dir.create(local_target, recursive = TRUE, showWarnings = FALSE)
      sim_log(step, sprintf("pulling %d file(s) for input '%s' into %s", length(unique(file_names)), spec_name, spec$dir))
      remote_pull(file.path("SIMULATION", "intermediate_results", "poisson", spec$dir), unique(file_names), local_target)
    }

    results <- parallel::mclapply(batch, function(row_idx) {
      row <- par_settings[row_idx, , drop = FALSE]
      row_env <- new.env(parent = globalenv())
      row_env$local_root <- local_root
      row_env$step <- step
      started_at <- Sys.time()
      sim_log(step, sprintf("starting iter=%s, m=%s, uniform_cluster_size=%s", row$iter, row$m, row$uniform_cluster_size), row = row_idx)
      if (length(inputs) > 0L) {
        load_inputs_for_row(row, inputs, row_env, local_root)
      }
      tryCatch({
        out <- worker(row, row_env)
        if (is.null(out)) {
          return(list(error = "worker returned NULL"))
        }
        if (!is.list(out)) {
          out <- list(value = out)
        }
        names_out <- names(out)
        if (length(names_out) == 0L) {
          stop("worker must return a named list of output objects.")
        }
        for (nm in names_out) {
          assign(nm, out[[nm]], envir = row_env)
        }
        save_outputs_for_row(row, outputs, row_env, local_root)
        elapsed <- round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 1)
        sim_log(step, sprintf("finished in %ss", elapsed), row = row_idx)
        list(ok = TRUE)
      }, error = function(e) {
        elapsed <- round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 1)
        sim_log(step, sprintf("failed after %ss: %s", elapsed, conditionMessage(e)), row = row_idx)
        list(error = conditionMessage(e))
      })
    }, mc.cores = cores, mc.preschedule = FALSE)

    failures <- vapply(results, function(x) is.list(x) && !is.null(x$error), logical(1))
    if (any(failures)) {
      if (length(unique(file.path("SIMULATION", "intermediate_results", "poisson"))) > 0L) {
        sim_log(step, "pushing partial batch outputs before raising failure.")
        remote_push(local_root, file.path("SIMULATION", "intermediate_results", "poisson"))
      }
      stop(sprintf("[%s] failed rows: %s\n%s", step, paste(batch[failures], collapse = ", "),
                   paste(vapply(results[failures], function(x) x$error, character(1)), collapse = "\n")))
    }

    sim_log(step, sprintf("pushing completed batch %d/%d to remote storage", batch_number, batch_total))
    remote_push(local_root, file.path("SIMULATION", "intermediate_results", "poisson"))
    unlink(local_root, recursive = TRUE, force = TRUE)
    sim_log(step, sprintf("completed batch %d/%d for row(s): %s", batch_number, batch_total, paste(batch, collapse = ", ")))
  }

  sim_log(step, sprintf("finished %d row(s)", length(filtered_rows)))
  invisible(filtered_rows)
}
