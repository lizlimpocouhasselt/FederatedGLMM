#-----------------------------------------
# DATA PROVIDER TASK
# COMPUTE SUMMARY STATISTICS (PER CLUSTER)
# HANKEL MATRICES
#-----------------------------------------

# Load libraries / functions
library(fastDummies)
source(file.path('DEMO', 'scripts_and_functions', 'fn_compute_summary.R'))
source(file.path('DEMO', 'scripts_and_functions', 'construct_hankel.R')) # Hankel moment matrix (summations)


#---------------- DATA PROVIDER TASK ----------------

# I. Load and preprocess data
# Include only hospitals with:
# a. complete cases in required variables
# b. valid values
# c. more than 1 patient record

default_csv_path <- "/Users/lizlimpoco/Library/CloudStorage/GoogleDrive-liz.limpoco@uhasselt.be/My Drive/PhD/Working papers/Federated GLMM/Codes_and_Data/DEMO/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2022_20241021.csv"
sparcs_csv_path <- Sys.getenv("SPARCS_CSV_PATH", unset = default_csv_path)

if (!file.exists(sparcs_csv_path)) {
  stop(paste0("SPARCS CSV not found at: ", sparcs_csv_path,
              "\nSet SPARCS_CSV_PATH to the CSV location."))
}

cache_dir <- Sys.getenv("DEMO_CACHE_DIR", unset = file.path("DEMO", "intermediate_results", "cache"))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

summary_dir <- file.path("DEMO", "intermediate_results", "summary_info")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

preprocessed_csv <- file.path("DEMO", "intermediate_results", "preprocessed_data.csv")

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

  save(summary_info,
       file = file.path(summary_dir, sprintf("summary_info_%04d.RData", grp_num)))
}

build_preprocessed_csv <- function() {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("readr is required to stream a Google Drive-backed CSV without stalling.")
  }

  raw_header <- readr::read_csv(sparcs_csv_path, n_max = 0L, show_col_types = FALSE)
  raw_names <- names(raw_header)

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

  readr::read_csv_chunked(
    file = sparcs_csv_path,
    callback = readr::DataFrameCallback$new(function(chunk, pos) {
      keep_cols <- as.integer(required_col_idx)
      chunk <- chunk[, keep_cols, drop = FALSE]
      names(chunk) <- c("Facility.Name", "Gender", "Length.of.Stay", "CCSR.Diagnosis.Description",
                        "Emergency.Department.Indicator", "Total.Charges")

      chunk <- chunk[
        !is.na(chunk$Facility.Name) &
          !is.na(chunk$Gender) &
          !is.na(chunk$Length.of.Stay) &
          !is.na(chunk$CCSR.Diagnosis.Description) &
          !is.na(chunk$Emergency.Department.Indicator) &
          !is.na(chunk$Total.Charges),
        , drop = FALSE
      ]

      chunk <- chunk[chunk$Gender %in% c("F", "M") & chunk$Length.of.Stay != "120 +", , drop = FALSE]
      chunk$Total.Charges <- suppressWarnings(as.numeric(gsub(",", "", chunk$Total.Charges, fixed = TRUE)))
      chunk$Length.of.Stay <- suppressWarnings(as.numeric(chunk$Length.of.Stay))
      chunk$COVID19 <- ifelse(chunk$CCSR.Diagnosis.Description == "COVID-19", "positive", "negative")
      chunk <- chunk[!is.na(chunk$Length.of.Stay) & !is.na(chunk$Total.Charges), , drop = FALSE]
      chunk <- chunk[, c("Facility.Name", "Gender", "Length.of.Stay", "COVID19",
                        "Emergency.Department.Indicator", "Total.Charges")]

      if (nrow(chunk) > 0L) {
        write.table(chunk, file = filtered_csv, sep = ",", append = TRUE,
                    row.names = FALSE, col.names = FALSE, quote = FALSE)
      }

      NULL
    }),
    col_types = readr::cols(.default = readr::col_character()),
    chunk_size = 250000L,
    progress = FALSE
  )

  filtered_dt <- data.table::fread(filtered_csv, header = TRUE, showProgress = FALSE,
                                   stringsAsFactors = FALSE)
  keep_facilities <- names(which(table(filtered_dt$Facility.Name) > 1L))
  filtered_dt <- filtered_dt[filtered_dt$Facility.Name %in% keep_facilities, , drop = FALSE]
  write.csv(filtered_dt, file = preprocessed_csv, row.names = FALSE)

  invisible(preprocessed_csv)
}

if (file.exists(preprocessed_csv) && Sys.getenv("DEMO_REBUILD", unset = "") != "1") {
  dt <- data.table::fread(preprocessed_csv, header = TRUE, showProgress = FALSE,
                          stringsAsFactors = FALSE)
} else {
  build_preprocessed_csv()
  dt <- data.table::fread(preprocessed_csv, header = TRUE, showProgress = FALSE,
                          stringsAsFactors = FALSE)
}

facility_names <- sort(unique(dt$Facility.Name))
for (grp_num in seq_along(facility_names)) {
  grp_name <- facility_names[grp_num]
  grp <- as.data.frame(dt[dt$Facility.Name == grp_name, c("Facility.Name", "Gender", "Length.of.Stay",
                                                          "COVID19", "Emergency.Department.Indicator", "Total.Charges")])
  compute_and_save_summary(grp, grp_num)
  rm(grp)
  gc(verbose = FALSE)
}