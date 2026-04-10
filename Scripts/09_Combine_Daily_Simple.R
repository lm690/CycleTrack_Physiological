## =========================================================
## 09_combine_daily_simple.R
## Purpose: Combine daily Garmin outputs into one file
## Output:  data/processed/daily/garmin_daily_all.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Set folders ----
day_folder <- Sys.getenv("DAILYDIR")

## ---- 3. Create empty list for daily files ----
daily_list <- list()

## ---- 4. Read adherence file if it exists ----
adherence_file <- file.path(day_folder, "garmin_daily_adherence.rds")

if (file_exists(adherence_file)) {
  daily_adherence <- readRDS(adherence_file)
  print("Loaded adherence")
  print(names(daily_adherence))
  daily_list[[length(daily_list) + 1]] <- daily_adherence
}

## ---- 5. Read HRV file if it exists ----
hrv_file <- file.path(day_folder, "garmin_daily_hrv.rds")

if (file_exists(hrv_file)) {
  daily_hrv <- readRDS(hrv_file)
  print("Loaded HRV")
  print(names(daily_hrv))
  daily_list[[length(daily_list) + 1]] <- daily_hrv
}

## ---- 6. Read stress file if it exists ----
stress_file <- file.path(day_folder, "garmin_daily_stress.rds")

if (file_exists(stress_file)) {
  daily_stress <- readRDS(stress_file)
  print("Loaded stress")
  print(names(daily_stress))
  daily_list[[length(daily_list) + 1]] <- daily_stress
}

## ---- 7. Read steps file if it exists ----
steps_file <- file.path(day_folder, "garmin_daily_steps.rds")

if (file_exists(steps_file)) {
  daily_steps <- readRDS(steps_file)
  print("Loaded steps")
  print(names(daily_steps))
  daily_list[[length(daily_list) + 1]] <- daily_steps
}

## ---- 8. Read heart-rate file if it exists ----
hr_file <- file.path(day_folder, "garmin_daily_hr.rds")

if (file_exists(hr_file)) {
  daily_hr <- readRDS(hr_file)
  print("Loaded heart rate")
  print(names(daily_hr))
  daily_list[[length(daily_list) + 1]] <- daily_hr
}

## ---- 9. Check that at least one file was loaded ----
if (length(daily_list) == 0) {
  stop("No daily files were found to combine.")
}

## ---- 10. Start with the first file ----
daily_all <- daily_list[[1]]

## ---- 11. Join remaining files one by one ----
if (length(daily_list) > 1) {
  for (i in 2:length(daily_list)) {
    daily_all <- full_join(
      daily_all,
      daily_list[[i]],
      by = c("participant_id", "date")
    )
  }
}

## ---- 12. Sort rows ----
daily_all <- daily_all %>%
  arrange(participant_id, date)

## ---- 13. Check result ----
print(dim(daily_all))
print(names(daily_all))
print(daily_all)

## ---- 14. Check for duplicate participant-date rows ----
dup_check <- daily_all %>%
  count(participant_id, date, name = "n_rows") %>%
  filter(n_rows > 1)

print(dup_check)

## ---- 15. Save combined file ----
saveRDS(daily_all, file.path(day_folder, "garmin_daily_all.rds"))

## ---- 16. Save duplicate check ----
saveRDS(dup_check, file.path(day_folder, "garmin_daily_duplicate_check.rds"))

message("09_combine_daily_simple.R complete")