#-----------------------------------------
# DATA PROVIDER TASK
# COMPUTE SUMMARY STATISTICS (PER CLUSTER)
# HANKEL MATRICES
#-----------------------------------------

# Load libraries / functions
library(fastDummies)
source(file.path('R_common', 'remote_store.R'))
source(file.path('DEMO', 'scripts_and_functions', 'fn_compute_summary.R'))
source(file.path('DEMO', 'scripts_and_functions', 'construct_hankel.R')) # Hankel moment matrix (summations)


#---------------- DATA PROVIDER TASK ----------------

# I. Load and preprocess data
# Include only hospitals with:
# a. complete cases in required variables
# b. valid values
# c. more than 1 patient record

sparcs_csv_name <- "Hospital_Inpatient_Discharges__SPARCS_De-Identified___2022_20241021.csv"
local_csv_path <- file.path("DEMO", sparcs_csv_name)
default_rclone_remote <- remote_path(file.path("DEMO", sparcs_csv_name))
preprocessed_key <- file.path("DEMO", "intermediate_results", "preprocessed_data.csv")

# Source priority: SPARCS_CSV_PATH > local DEMO/<csv> > rclone stream from SPARCS_RCLONE_REMOTE.
open_sparcs_connection <- function() {
  csv_path <- Sys.getenv("SPARCS_CSV_PATH", unset = "")
  if (!nzchar(csv_path) && file.exists(local_csv_path)) csv_path <- local_csv_path

  if (nzchar(csv_path)) {
    if (!file.exists(csv_path)) {
      stop("SPARCS CSV not found at: ", csv_path)
    }
    # Online-only cloud placeholders report full size but ~0 KB on disk; reading them stalls.
    on_disk_kb <- suppressWarnings(as.numeric(
      sub("\\s.*$", "", system2("du", c("-k", shQuote(csv_path)), stdout = TRUE)[1])))
    if (!is.na(on_disk_kb) && on_disk_kb * 1024 < 0.5 * file.size(csv_path)) {
      stop(sprintf(paste0(
        "SPARCS CSV is an online-only cloud placeholder (%.0f MB logical, %.0f KB on disk): %s\n",
        "Unset SPARCS_CSV_PATH to stream it with rclone instead."),
        file.size(csv_path) / 1024^2, on_disk_kb, csv_path))
    }
    cat(sprintf("DEBUG: reading local SPARCS CSV at %s\n", csv_path))
    return(file(csv_path, open = "r"))
  }

  remote <- Sys.getenv("SPARCS_RCLONE_REMOTE", unset = default_rclone_remote)
  if (!nzchar(Sys.which("rclone"))) {
    stop("rclone not found. Install it (`brew install rclone`), add a Google Drive remote ",
         "named 'gdrive' (`rclone config`), or set SPARCS_RCLONE_REMOTE / SPARCS_CSV_PATH.")
  }
  cat(sprintf("DEBUG: streaming SPARCS CSV via rclone from %s\n", remote))
  pipe(paste("rclone cat", shQuote(remote)), open = "r")
}

cache_dir <- Sys.getenv("DEMO_CACHE_DIR", unset = file.path(tempdir(), "fglmm_demo_cache"))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

preprocessed_csv <- preprocessed_key

normalize_header_name <- function(x) {
  tolower(gsub("[^A-Za-z0-9]+", "", x))
}

find_required_column <- function(raw_names, candidate_names) {
  ids <- which(normalize_header_name(raw_names) %in% normalize_header_name(candidate_names))
  if (length(ids) == 0L) {
    stop("Could not locate required SPARCS column. Looking for one of: ",
         paste(candidate_names, collapse = ", "))
  }
  as.integer(ids[1L])
}

num.varnames <- c("Length.of.Stay", "Total.Charges")

compute_and_save_summary <- function(grp, grp_num) {
  if (nrow(grp) > 500) {
    N <- nrow(grp)
    n_i <- 250
    n_full_grps <- N %/% n_i
    summary_info <- lapply(seq_len(n_full_grps), function(grp_i) {
      if (grp_i < n_full_grps) {
        row.idx <- ((grp_i - 1) * n_i + 1):(grp_i * n_i)
      } else {
        row.idx <- ((grp_i - 1) * n_i + 1):N
      }
      fn_compute_summary(grp[row.idx, -1, drop = FALSE], num.varnames)
    })
  } else {
    summary_info <- list(fn_compute_summary(grp[, -1, drop = FALSE], num.varnames))
  }

  remote_save(summary_info,
              key = file.path("DEMO", "intermediate_results", "summary_info",
                              sprintf("summary_info_%04d.RData", grp_num)))
}

build_preprocessed_csv <- function(chunk_lines = 100000L) {
  con <- open_sparcs_connection()
  con_open <- TRUE
  on.exit(if (con_open) close(con), add = TRUE)

  header_line <- readLines(con, n = 1L, warn = FALSE)
  if (length(header_line) == 0L) {
    stop("SPARCS source returned no data (check the rclone remote path / authentication).")
  }
  raw_names <- names(data.table::fread(text = header_line, header = TRUE, nrows = 0L))

  required_col_idx <- c(
    Facility.Name = find_required_column(raw_names, c("Facility.Name", "Facility Name")),
    Gender = find_required_column(raw_names, c("Gender")),
    Length.of.Stay = find_required_column(raw_names, c("Length.of.Stay", "Length of Stay")),
    CCSR.Diagnosis.Description = find_required_column(raw_names, c("CCSR.Diagnosis.Description", "CCSR Diagnosis Description")),
    Emergency.Department.Indicator = find_required_column(raw_names, c("Emergency.Department.Indicator", "Emergency Department Indicator")),
    Total.Charges = find_required_column(raw_names, c("Total.Charges", "Total Charges"))
  )

  filtered_csv <- file.path(cache_dir, "filtered_sparcs_chunks.csv")
  if (file.exists(filtered_csv)) {
    file.remove(filtered_csv)
  }
  writeLines("Facility.Name,Gender,Length.of.Stay,COVID19,Emergency.Department.Indicator,Total.Charges", filtered_csv)

  count_quotes <- function(x) {
    nchar(x, type = "bytes") - nchar(gsub('"', "", x, fixed = TRUE, useBytes = TRUE), type = "bytes")
  }

  n_read <- 0
  n_kept <- 0
  repeat {
    lines <- readLines(con, n = chunk_lines, warn = FALSE)
    if (length(lines) == 0L) break

    # A quoted field may contain a newline; extend the chunk so no record is split.
    n_quotes <- sum(count_quotes(lines))
    while (n_quotes %% 2 == 1) {
      more <- readLines(con, n = 1L, warn = FALSE)
      if (length(more) == 0L) break
      lines <- c(lines, more)
      n_quotes <- n_quotes + count_quotes(more)
    }
    n_read <- n_read + length(lines)

    chunk <- data.table::fread(text = lines, header = FALSE, sep = ",",
                               select = as.integer(required_col_idx),
                               colClasses = "character", na.strings = c("", "NA"),
                               showProgress = FALSE)
    data.table::setnames(chunk, names(required_col_idx))
    data.table::setDF(chunk)

    chunk <- chunk[stats::complete.cases(chunk), , drop = FALSE]
    chunk <- chunk[chunk$Gender %in% c("F", "M") & chunk$Length.of.Stay != "120 +", , drop = FALSE]
    chunk$Total.Charges <- suppressWarnings(as.numeric(gsub(",", "", chunk$Total.Charges, fixed = TRUE)))
    chunk$Length.of.Stay <- suppressWarnings(as.numeric(chunk$Length.of.Stay))
    chunk$COVID19 <- ifelse(chunk$CCSR.Diagnosis.Description == "COVID-19", "positive", "negative")
    chunk <- chunk[!is.na(chunk$Length.of.Stay) & !is.na(chunk$Total.Charges), , drop = FALSE]
    chunk <- chunk[, c("Facility.Name", "Gender", "Length.of.Stay", "COVID19",
                      "Emergency.Department.Indicator", "Total.Charges")]

    if (nrow(chunk) > 0L) {
      data.table::fwrite(chunk, filtered_csv, append = TRUE)
      n_kept <- n_kept + nrow(chunk)
    }
    cat(sprintf("DEBUG: streamed %s rows, kept %s\n",
                format(n_read, big.mark = ","), format(n_kept, big.mark = ",")))
  }

  close_status <- close(con)
  con_open <- FALSE
  if (!is.null(close_status) && close_status != 0L) {
    stop("SPARCS source exited with status ", close_status, "; stream may be incomplete.")
  }

  filtered_dt <- data.table::fread(filtered_csv, header = TRUE, showProgress = FALSE,
                                   stringsAsFactors = FALSE)
  cat(sprintf("DEBUG: read filtered_csv -> rows=%d, cols=%d, file_size=%s\n",
              nrow(filtered_dt), ncol(filtered_dt),
              if (file.exists(filtered_csv)) format(file.info(filtered_csv)$size, big.mark = ",") else "missing"))
  keep_facilities <- names(which(table(filtered_dt$Facility.Name) > 1L))
  filtered_dt <- filtered_dt[filtered_dt$Facility.Name %in% keep_facilities, , drop = FALSE]
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(filtered_dt, file = tmp_csv, row.names = FALSE)
  upload_file(tmp_csv, preprocessed_key)
  if (file.exists(filtered_csv)) {
    file.remove(filtered_csv)
  }
  cat(sprintf("DEBUG: uploaded preprocessed_data.csv -> rows=%d, cols=%d\n",
              nrow(filtered_dt), ncol(filtered_dt)))

  invisible(preprocessed_key)
}

if (Sys.getenv("DEMO_REBUILD", unset = "") != "1" && remote_exists(preprocessed_key)) {
  cat(sprintf("DEBUG: loading existing preprocessed csv from remote key %s\n", preprocessed_key))
  dt <- remote_fread(preprocessed_key, header = TRUE, showProgress = FALSE,
                     stringsAsFactors = FALSE)
} else {
  cat("DEBUG: building preprocessed csv from raw SPARCS file\n")
  build_preprocessed_csv()
  cat(sprintf("DEBUG: finished build_preprocessed_csv(); reading remote key %s\n", preprocessed_key))
  dt <- remote_fread(preprocessed_key, header = TRUE, showProgress = FALSE,
                     stringsAsFactors = FALSE)
}

facility_names <- sort(unique(dt$Facility.Name))
cat(sprintf("DEBUG: number of facilities=%d\n", length(facility_names)))
for (grp_num in seq_along(facility_names)) {
  grp_name <- facility_names[grp_num]
  grp <- as.data.frame(dt[dt$Facility.Name == grp_name, c("Facility.Name", "Gender", "Length.of.Stay",
                                                          "COVID19", "Emergency.Department.Indicator", "Total.Charges")])
  cat(sprintf("DEBUG: facility %d/%d -> %s, rows=%d, cols=%d\n",
              grp_num, length(facility_names), grp_name, nrow(grp), ncol(grp)))
  compute_and_save_summary(grp, grp_num)
  cat(sprintf("DEBUG: completed summary for facility %d/%d -> %s\n",
              grp_num, length(facility_names), grp_name))
  rm(grp)
  gc(verbose = FALSE)
}