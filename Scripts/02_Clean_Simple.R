## =========================================================
## 02_Cleaning
## Purpose: Basic cleaning of Garmin data and timestamp creation
## Output:  cleaned full file + separate files by data type
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)
library(stringr)

## ---- 2. Read raw data ----
pro_dir <- Sys.getenv("PRODIR")
if (pro_dir == "") stop("PRODIR is not set.")

input_file <- file.path(pro_dir, "garmin_all_raw.rds")
if (!file.exists(input_file)) stop("Input file does not exist: ", input_file)

garmin_all <- readRDS(input_file)

if (nrow(garmin_all) == 0) stop("garmin_all is empty.")

message("garmin_all read.")

## ---- 3. Clean column names ----
names(garmin_all) <- make.names(names(garmin_all))

## ---- 4. Check that timestamp column exists ----
if (!"unixTimestampInMs" %in% names(garmin_all)) {
  stop("unixTimestampInMs is missing.")
}

## ---- 5. Convert unix timestamp to numeric ----
garmin_all <- garmin_all %>%
  mutate(
    unixTimestampInMs = suppressWarnings(as.numeric(unixTimestampInMs))
  )

if (all(is.na(garmin_all$unixTimestampInMs))) {
  stop("unixTimestampInMs could not be converted to numeric.")
}

## ---- 6. Create UTC timestamp/date ----
garmin_all <- garmin_all %>%
  mutate(
    ts_utc = as.POSIXct(unixTimestampInMs / 1000, origin = "1970-01-01", tz = "UTC"),
    date_utc = as.Date(ts_utc),
    hour_utc = as.integer(format(ts_utc, "%H"))
  )

## ---- 7. Clean timezone column if present ----
if ("timezone" %in% names(garmin_all)) {
  garmin_all <- garmin_all %>%
    mutate(
      timezone = as.character(timezone),
      timezone = str_trim(timezone),
      timezone = na_if(timezone, "")
    )
  message("timezone column found and cleaned.")
} else {
  garmin_all$timezone <- NA_character_
  message("No timezone column found. Local time will default to UTC.")
}

## ---- 8. Create local timestamp/date ----
valid_tz <- unique(stats::na.omit(garmin_all$timezone))
valid_olson <- valid_tz[valid_tz %in% OlsonNames()]

garmin_all <- garmin_all %>%
  mutate(
    ts_local = ts_utc,
    date_local = date_utc,
    hour_local = hour_utc,
    timezone_used = "UTC_fallback"
  )

if (length(valid_olson) > 0) {
  for (tz_i in valid_olson) {
    idx <- which(!is.na(garmin_all$timezone) & garmin_all$timezone == tz_i)

    if (length(idx) > 0) {
      local_chr <- format(garmin_all$ts_utc[idx], tz = tz_i, usetz = TRUE)
      local_posix <- as.POSIXct(local_chr, tz = tz_i)

      garmin_all$ts_local[idx] <- local_posix
      garmin_all$date_local[idx] <- as.Date(local_posix)
      garmin_all$hour_local[idx] <- as.integer(format(local_posix, "%H"))
      garmin_all$timezone_used[idx] <- tz_i
    }
  }

  message("Local timestamp variables created where valid timezone names were available.")
} else {
  message("No valid Olson timezone names found. Using UTC as local time.")
}

## ---- 9. Quick check of rows and columns ----
print(dim(garmin_all))
print(names(garmin_all))

## ---- 10. Coverage summary by participant and data type ----
qc_coverage <- garmin_all %>%
  group_by(participant_id, data_type) %>%
  summarise(
    n_rows = n(),
    first_ts = min(ts_utc, na.rm = TRUE),
    last_ts = max(ts_utc, na.rm = TRUE),
    n_days_utc = n_distinct(date_utc),
    n_days_local = n_distinct(date_local),
    n_unique_ts = n_distinct(ts_utc),
    n_dup_ts = n_rows - n_unique_ts,
    .groups = "drop"
  ) %>%
  arrange(participant_id, data_type)

print(qc_coverage)

## ---- 11. Create output folders ----
type_dir <- Sys.getenv("TYPEDIR")
if (type_dir == "") stop("TYPEDIR is not set.")

fs::dir_create(pro_dir)
fs::dir_create(type_dir)

message("Output folders set.")

## ---- 12. Save cleaned full dataset ----
saveRDS(garmin_all, file.path(pro_dir, "garmin_all_clean.rds"))

## ---- 13. Save coverage summary ----
saveRDS(qc_coverage, file.path(pro_dir, "garmin_qc_coverage.rds"))

message("QC coverage saved.")

## ---- 14. Split by data type ----
tables <- split(garmin_all, garmin_all$data_type)

## ---- 15. Save one RDS per data type ----
for (nm in names(tables)) {

  safe_name <- gsub("[^A-Za-z0-9]+", "_", nm)

  saveRDS(
    tables[[nm]],
    file.path(type_dir, paste0("garmin_clean_", safe_name, ".rds"))
  )
}

message("02_Cleaning.R complete")
