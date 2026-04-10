## =========================================================
## 02_Cleaning
## Purpose: Basic cleaning of Garmin data
## Output:  cleaned full file + separate files by data type
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Read raw data ----
pro_folder <- Sys.getenv("PRODIR")
garmin_all <- readRDS(file.path(pro_folder, "garmin_all_raw.rds"))

if (nrow(garmin_all) == 0) stop("garmin_all is empty.")

message("garmin_all read.")

## ---- 3. Clean column names ----
names(garmin_all) <- make.names(names(garmin_all))

## ---- 4. Check that timestamp column exists ----
stopifnot("unixTimestampInMs" %in% names(garmin_all))

## ---- 5. Convert unix timestamp to numeric ----
garmin_all$unixTimestampInMs <- suppressWarnings(as.numeric(garmin_all$unixTimestampInMs))

## ---- 6. Create UTC timestamp ----
garmin_all$ts_utc <- as.POSIXct(
  garmin_all$unixTimestampInMs / 1000,
  origin = "2000-01-01",
  tz = "UTC"
)

## ---- 7. Create UTC date ----
garmin_all$date_utc <- as.Date(garmin_all$ts_utc)

## ---- 8. Quick check of rows and columns ----
print(dim(garmin_all))
print(names(garmin_all))

## ---- 9. Coverage summary by participant and data type ----
qc_coverage <- garmin_all %>%
  group_by(participant_id, data_type) %>%
  summarise(
    n_rows = n(),
    first_ts = min(ts_utc, na.rm = TRUE),
    last_ts = max(ts_utc, na.rm = TRUE),
    n_days = n_distinct(date_utc),
    n_unique_ts = n_distinct(ts_utc),
    n_dup_ts = n_rows - n_unique_ts,
    .groups = "drop"
  ) %>%
  arrange(participant_id, data_type)

print(qc_coverage)

## ---- 10. Create output folders ----
type_dir <- Sys.getenv("TYPEDIR")

if (type_dir == "") stop("TYPEDIR is not set.")
fs::dir_create(type_dir)

message("TYPEDIR set.")

## ---- 11. Save cleaned full dataset ----
saveRDS(garmin_all, file.path(pro_folder, "garmin_all_clean.rds"))

## ---- 12. Save coverage summary ----
if (exists("qc_coverage")) {
  saveRDS(qc_coverage, file.path(pro_folder, "garmin_qc_coverage.rds"))
}

message("QC coverage saved.")

## ---- 13. Split by data type ----
tables <- split(garmin_all, garmin_all$data_type)

## ---- 14. Save one RDS per data type ----
for (nm in names(tables)) {
  
  safe_name <- gsub("[^A-Za-z0-9]+", "_", nm)
  
  saveRDS(
    tables[[nm]],
    file.path(type_dir, paste0("garmin_clean_", safe_name, ".rds"))
  )
}

message("02_clean_simple.R complete")