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

run_rclone <- function(args) {
  if (!nzchar(Sys.which("rclone"))) {
    stop("rclone not found. Install it (`brew install rclone`), add a Google Drive remote named 'gdrive' (`rclone config`), or set FGLMM_RCLONE_ROOT.")
  }

  # system2 does not quote args; the default remote path contains spaces
  err_file <- tempfile()
  on.exit(unlink(err_file, force = TRUE), add = TRUE)
  out <- suppressWarnings(system2("rclone", args = shQuote(args), stdout = TRUE, stderr = err_file))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    msg <- paste(c(out, readLines(err_file, warn = FALSE)), collapse = "\n")
    stop(sprintf("rclone command failed (status %s): rclone %s\n%s", status, paste(args, collapse = " "), msg))
  }
  out
}

remote_load <- function(key, envir = parent.frame()) {
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

  names_from_dots <- as.character(substitute(list(...)))[-1L]
  objects <- unique(c(as.character(list), names_from_dots))

  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  save(list = objects, file = tmp, envir = envir)
  run_rclone(c("copyto", tmp, remote_path(key)))
  invisible(objects)
}

remote_list <- function(dir_key = "") {
  target <- if (nzchar(dir_key)) remote_path(dir_key) else remote_root()
  out <- tryCatch(run_rclone(c("lsf", "--files-only", target)), error = function(e) character(0))
  if (length(out) == 0L) {
    return(character(0))
  }
  sort(unique(out[!grepl("^\\s*$", out)]))
}

remote_exists <- function(key) {
  if (!nzchar(key)) {
    return(FALSE)
  }
  target_dir <- dirname(key)
  if (identical(target_dir, ".")) {
    items <- remote_list("")
  } else {
    items <- remote_list(target_dir)
  }
  basename(key) %in% items
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
