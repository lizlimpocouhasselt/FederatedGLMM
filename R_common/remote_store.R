remote_root <- function() {
  if (!nzchar(Sys.which("rclone"))) {
    stop("rclone not found. Install it (`brew install rclone`), add a Google Drive remote named 'gdrive' (`rclone config`), or set FGLMM_RCLONE_ROOT.")
  }

  root <- Sys.getenv("FGLMM_RCLONE_ROOT", unset = "gdrive:PhD/Working papers/Federated GLMM/Codes_and_Data")
  sub("/+$", "", root)
}

remote_path <- function(key) {
  key <- gsub("\\\\", "/", key)
  key <- sub("^/+", "", key)
  paste0(remote_root(), "/", key)
}

normalize_remote_key <- function(key) {
  if (!length(key) || !is.character(key) || is.na(key) || !nzchar(key)) {
    stop("A remote key is required.")
  }
  key <- gsub("\\\\", "/", key)
  key <- sub("^/+", "", key)
  if (!grepl("\\.[A-Za-z0-9]+$", key)) {
    key <- paste0(key, ".RData")
  }
  key
}

run_rclone <- function(args) {
  if (!nzchar(Sys.which("rclone"))) {
    stop("rclone not found. Install it (`brew install rclone`), add a Google Drive remote named 'gdrive' (`rclone config`), or set FGLMM_RCLONE_ROOT.")
  }

  err_file <- tempfile()
  on.exit(unlink(err_file, force = TRUE), add = TRUE)
  cmd <- paste(c("rclone", args), collapse = " ")
  out <- suppressWarnings(system2("rclone", args = shQuote(args), stdout = TRUE, stderr = err_file))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    msg <- paste(c(out, readLines(err_file, warn = FALSE)), collapse = "\n")
    stop(sprintf("rclone command failed (status %s): %s\n%s", status, cmd, msg))
  }
  out
}

remote_ls <- function(dir_key = "", include = NULL, must_exist = TRUE) {
  target <- if (nzchar(dir_key)) remote_path(dir_key) else remote_root()
  args <- c("lsf", "--files-only", target)
  if (!is.null(include) && nzchar(include)) {
    args <- c(args, "--filter", paste0("include=", include))
  }

  out <- tryCatch(run_rclone(args), error = function(e) {
    if (isTRUE(must_exist)) {
      stop(e)
    }
    character(0)
  })
  if (length(out) == 0L) {
    return(character(0))
  }
  sort(unique(out[!grepl("^\\s*$", out)]))
}

remote_list <- function(dir_key = "") {
  remote_ls(dir_key = dir_key, must_exist = FALSE)
}

remote_load <- function(key, envir = parent.frame()) {
  key <- normalize_remote_key(key)
  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  run_rclone(c("copyto", remote_path(key), tmp))
  if (!file.exists(tmp)) {
    stop("Remote file missing after copy: ", key)
  }

  before <- ls(envir = envir, all.names = TRUE)
  load(tmp, envir = envir)
  after <- ls(envir = envir, all.names = TRUE)
  invisible(setdiff(after, before))
}

remote_save <- function(..., list = character(), key, envir = parent.frame()) {
  if (missing(key)) {
    stop("A remote key is required: remote_save(..., key = '...')")
  }
  key <- normalize_remote_key(key)

  names_from_dots <- as.character(substitute(list(...)))[-1L]
  objects <- unique(c(as.character(list), names_from_dots))

  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  save(list = objects, file = tmp, envir = envir)
  run_rclone(c("copyto", tmp, remote_path(key)))
  invisible(objects)
}

remote_pull <- function(dir_key, files, local_dir) {
  if (missing(files) || length(files) == 0L) {
    return(character(0))
  }
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  }

  files <- unique(as.character(files))
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(files, con = tmp)

  target <- remote_path(dir_key)
  run_rclone(c("copy", target, local_dir, "--files-from", tmp, "--transfers", "8"))
  file.path(local_dir, files)
}

remote_push <- function(local_dir, dir_key) {
  if (!dir.exists(local_dir)) {
    stop("Local directory does not exist: ", local_dir)
  }
  run_rclone(c("copy", local_dir, remote_path(dir_key), "--checksum", "--transfers", "8"))
  invisible(dir_key)
}

remote_exists <- function(key) {
  if (!nzchar(key)) {
    return(FALSE)
  }

  target_dir <- dirname(key)
  remote_dir <- if (identical(target_dir, ".")) "" else target_dir
  items <- remote_list(remote_dir)
  file_name <- basename(key)
  candidates <- unique(c(file_name, paste0(file_name, ".RData"), sub("\\.[A-Za-z0-9]+$", "", file_name)))
  any(candidates %in% items)
}

remote_read_csv <- function(key, ...) {
  cmd <- paste("rclone cat", shQuote(remote_path(key)))
  read.csv(pipe(cmd, open = "r"), ...)
}

remote_fread <- function(key, ...) {
  data.table::fread(cmd = paste("rclone cat", shQuote(remote_path(key))), ...)
}

remote_write_csv <- function(df, key, ...) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  write.csv(df, file = tmp, row.names = FALSE, ...)
  run_rclone(c("copyto", tmp, remote_path(key)))
  invisible(key)
}

upload_file <- function(local_path, key) {
  if (!file.exists(local_path)) {
    stop("Local file not found for upload: ", local_path)
  }
  run_rclone(c("copyto", local_path, remote_path(key)))
  unlink(local_path, force = TRUE)
  invisible(key)
}
