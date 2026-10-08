#-----------------------------------------
# DATA PROVIDER TASK
# COMPUTE SUMMARY STATISTICS (PER CLUSTER)
# HANKEL MATRICES
#-----------------------------------------

# Load libraries / functions
library(data.table)
library(parallel)
library(fastDummies)
source(file.path('R_common', 'remote_store.R'))
source(file.path('DEMO', 'scripts_and_functions', 'fn_compute_summary.R'))
source(file.path('DEMO', 'scripts_and_functions', 'construct_hankel.R')) # Hankel moment matrix (summations)


#---------------- DATA PROVIDER TASK ----------------

# I. Load and preprocess data
# Include only hospitals with:
# a. complete cases
# b. valid values
# c. more than 1 patient record

sparcs_csv_name <- "Hospital_Inpatient_Discharges__SPARCS_De-Identified___2022_20241021.csv"
local_csv_path <- file.path("DEMO", sparcs_csv_name)
preprocessed_key <- file.path("DEMO", "intermediate_results", "preprocessed_data.csv")
summary_key <- file.path("DEMO", "intermediate_results", "summary_info")
num.varnames <- c("Length.of.Stay", "Total.Charges")
model_cols <- c("Gender", "Length.of.Stay", "COVID19", "Emergency.Department.Indicator", "Total.Charges")
n_cores <- as.integer(Sys.getenv("DEMO_CORES", unset = detectCores()))

# Source priority: SPARCS_CSV_PATH > local DEMO/<csv> > download from SPARCS_RCLONE_REMOTE.
locate_sparcs_csv <- function(download_to) {
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
        "Unset SPARCS_CSV_PATH to download it with rclone instead."),
        file.size(csv_path) / 1024^2, on_disk_kb, csv_path))
    }
    cat(sprintf("DEBUG: reading local SPARCS CSV at %s\n", csv_path))
    return(csv_path)
  }

  remote <- Sys.getenv("SPARCS_RCLONE_REMOTE", unset = remote_path(file.path("DEMO", sparcs_csv_name)))
  cat(sprintf("DEBUG: downloading SPARCS CSV via rclone from %s\n", remote))
  run_rclone(c("copyto", remote, download_to))
  download_to
}

normalize_header_name <- function(x) {
  tolower(gsub("[^A-Za-z0-9]+", "", x))
}

find_required_column <- function(raw_names, candidate_name) {
  ids <- which(normalize_header_name(raw_names) == normalize_header_name(candidate_name))
  if (length(ids) == 0L) {
    stop("Could not locate required SPARCS column: ", candidate_name)
  }
  as.integer(ids[1L])
}

build_preprocessed_data <- function() {
  tmp_src <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_src, force = TRUE), add = TRUE)
  src <- locate_sparcs_csv(download_to = tmp_src)

  raw_names <- names(fread(src, nrows = 0L))
  required_cols <- c("Facility.Name", "Gender", "Length.of.Stay", "CCSR.Diagnosis.Description",
                     "Emergency.Department.Indicator", "Total.Charges")
  required_idx <- vapply(required_cols, find_required_column, integer(1), raw_names = raw_names)

  # Mirror read.csv(): blanks are NA only in numeric columns, and complete cases span ALL columns.
  raw <- fread(src, na.strings = "NA", colClasses = list(character = unname(required_idx)),
               showProgress = FALSE)
  dt <- raw[complete.cases(raw), required_idx, with = FALSE]
  rm(raw)
  setnames(dt, required_cols)

  dt <- dt[Length.of.Stay != "120 +" & Gender %chin% c("F", "M")]
  dt[, `:=`(COVID19 = fifelse(CCSR.Diagnosis.Description == "COVID-19", "positive", "negative"),
            Length.of.Stay = as.numeric(Length.of.Stay),
            Total.Charges = as.numeric(gsub(",", "", Total.Charges, fixed = TRUE)),
            CCSR.Diagnosis.Description = NULL)]
  setcolorder(dt, c("Facility.Name", model_cols))
  dt <- dt[Facility.Name %chin% dt[, .N, by = Facility.Name][N > 1L, Facility.Name]]

  # quote = TRUE / na = "NA" reproduce write.csv() output byte-for-byte.
  out_csv <- tempfile(fileext = ".csv")
  fwrite(dt, out_csv, quote = TRUE, na = "NA")
  upload_file(out_csv, preprocessed_key)
  cat(sprintf("DEBUG: uploaded preprocessed_data.csv -> rows=%d, cols=%d\n", nrow(dt), ncol(dt)))
  dt
}

if (Sys.getenv("DEMO_REBUILD", unset = "") != "1" && remote_exists(preprocessed_key)) {
  cat(sprintf("DEBUG: loading existing preprocessed csv from remote key %s\n", preprocessed_key))
  dt <- remote_fread(preprocessed_key, showProgress = FALSE)
} else {
  cat("DEBUG: building preprocessed csv from raw SPARCS file\n")
  dt <- build_preprocessed_data()
}


# II. Compute summary statistics (per cluster)
# Locale-collated sort, as in the original as.factor() split, fixes the summary_info_#### numbering.
facility_names <- sort(unique(dt$Facility.Name))
rows_by_facility <- split(seq_len(nrow(dt)), factor(dt$Facility.Name, levels = facility_names))
cat(sprintf("DEBUG: number of facilities=%d, cores=%d\n", length(facility_names), n_cores))

summarise_facility <- function(rows) {
  grp <- setDF(dt[rows, ..model_cols])
  N <- nrow(grp)
  # Hospitals with >500 records are split into 250-row clusters; the last absorbs the remainder.
  starts <- if (N > 500) seq(1L, by = 250L, length.out = N %/% 250L) else 1L
  ends <- c(starts[-1L] - 1L, N)
  lapply(seq_along(starts), function(k) {
    fn_compute_summary(grp[starts[k]:ends[k], , drop = FALSE], num.varnames)
  })
}

out_dir <- tempfile("summary_info_")
dir.create(out_dir)
done <- mclapply(seq_along(facility_names), function(grp_num) {
  summary_info <- summarise_facility(rows_by_facility[[grp_num]])
  save(summary_info, file = file.path(out_dir, sprintf("summary_info_%04d.RData", grp_num)))
  TRUE
}, mc.cores = n_cores)

failed <- which(!vapply(done, isTRUE, logical(1)))
if (length(failed) > 0L) {
  stop("Summary computation failed for facilities: ", paste(failed, collapse = ", "), "\n",
       paste(unique(vapply(done[failed], function(x) paste(format(x), collapse = " "), "")),
             collapse = "\n"))
}

# Single batched upload; --checksum skips files already identical on the remote.
run_rclone(c("copy", out_dir, remote_path(summary_key), "--checksum", "--transfers", "8"))
unlink(out_dir, recursive = TRUE)
cat(sprintf("DEBUG: uploaded %d summary_info files to %s\n", length(facility_names), summary_key))