## =========================================================
## 03_Daily Derivation
## Purpose: Create simple daily summaries from cleaned Garmin
##          files, one row per participant per date
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Create DAILYDIR ----
day_folder <- Sys.getenv("DAILYDIR")

if (day_folder == "") stop("DAILYDIR is not set.")
fs::dir_create(day_folder)


## =========================================================
## 			STEPS
## =========================================================

## ---- 3. Read step data ----
type_folder <- Sys.getenv("TYPEDIR")
step_file <- file.path(type_folder, "garmin_clean_garmin_device_step.rds")

if (step_file == "") stop("No steps file.")

if (file_exists(step_file)) {
  
  step_data <- readRDS(step_file)
  
  ## ---- 4. Check columns ----
  print(names(step_data))
  
  ## ---- 5. Make sure numeric columns are numeric ----
  step_data$steps <- suppressWarnings(as.numeric(step_data$steps))
  step_data$totalSteps <- suppressWarnings(as.numeric(step_data$totalSteps))
  
  ## ---- 6. Make sure date exists ----
  step_data$date <- as.Date(step_data$ts_utc)
  
  ## ---- 7. Create simple daily step summary ----
  daily_steps <- step_data %>%
    group_by(participant_id, date) %>%
    summarise(
      steps_sum_24hr = sum(steps, na.rm = TRUE),
      steps_max_total_24hr = max(totalSteps, na.rm = TRUE),
      n_rows_step = n(),
      .groups = "drop"
    ) %>%
    arrange(participant_id, date)
  
  ## ---- 8. Replace impossible max values if all missing ----
  daily_steps$steps_max_total_24hr[is.infinite(daily_steps$steps_max_total_24hr)] <- NA
  
  ## ---- 9. View result ----
  print(daily_steps)
  
  ## ---- 10. Save result ----
  saveRDS(daily_steps, file.path(day_folder, "garmin_daily_steps.rds"))
}

## =========================================================
## 			HEART RATE
## =========================================================

## ---- 11. Read heart-rate data ----
type_folder <- Sys.getenv("TYPEDIR")
hr_file <- file.path(type_folder, "garmin_clean_garmin_device_heart_rate.rds")

if (step_file == "") stop("No heart rate file.")

if (file_exists(hr_file)) {
  
  hr_data <- readRDS(hr_file)
  
  ## ---- 12. Check columns ----
  print(names(hr_data))
  
  ## ---- 13. Make heart-rate numeric ----
  hr_data$beatsPerMinute <- suppressWarnings(as.numeric(hr_data$beatsPerMinute))
  
  ## ---- 14. Create date ----
  hr_data$date <- as.Date(hr_data$ts_utc)
  
  ## ---- 15. Keep rows with heart-rate values ----
  hr_data <- hr_data %>%
    filter(!is.na(beatsPerMinute))
  
  ## ---- 16. Create daily heart-rate summary ----
  daily_hr <- hr_data %>%
    group_by(participant_id, date) %>%
    summarise(
      hr_mean_24hr = mean(beatsPerMinute, na.rm = TRUE),
      hr_min_24hr = min(beatsPerMinute, na.rm = TRUE),
      hr_max_24hr = max(beatsPerMinute, na.rm = TRUE),
      n_rows_hr = n(),
      .groups = "drop"
    ) %>%
    arrange(participant_id, date)
  
  ## ---- 17. View result ----
  print(daily_hr)
  
  ## ---- 18. Save result ----
  saveRDS(daily_hr, file.path(day_folder, "garmin_daily_hr.rds"))
}

## =========================================================
## 			STRESS
## =========================================================

## ---- 19. Read stress data ----
type_folder <- Sys.getenv("TYPEDIR")
stress_file <- file.path(type_folder, "garmin_clean_garmin_device_stress.rds")

if (stress_file == "") stop("No stress file.")

if (file_exists(stress_file)) {
  
  stress_data <- readRDS(stress_file)
  
  ## ---- 20. Check columns ----
  print(names(stress_data))
  
  ## ---- 21. Make stress numeric ----
  stress_data$stressLevel <- suppressWarnings(as.numeric(stress_data$stressLevel))
  
  ## ---- 22. Create date ----
  stress_data$date <- as.Date(stress_data$ts_utc)
  
  ## ---- 23. Keep rows with stress values ----
  stress_data <- stress_data %>%
    filter(!is.na(stressLevel))
  
  ## ---- 24. Create daily stress summary ----
  daily_stress <- stress_data %>%
    group_by(participant_id, date) %>%
    summarise(
      stress_mean_24hr = mean(stressLevel, na.rm = TRUE),
      stress_max_24hr = max(stressLevel, na.rm = TRUE),
      n_rows_stress = n(),
      .groups = "drop"
    ) %>%
    arrange(participant_id, date)
  
  ## ---- 25. View result ----
  print(daily_stress)
  
  ## ---- 26. Save result ----
  saveRDS(daily_stress, file.path(day_folder, "garmin_daily_stress.rds"))
}

## =========================================================
## 			BBI CHECK FILE
## =========================================================

## ---- 27. Read enhanced BBI data if it exists ----
type_folder <- Sys.getenv("TYPEDIR")
bbi_file <- file.path(type_folder, "garmin_clean_garmin_device_bbi.rds")

if (bbi_file == "") stop("No bbi file.")

if (file_exists(bbi_file)) {
  
  bbi_data <- readRDS(bbi_file)
  
  ## ---- 28. Check columns ----
  print(names(bbi_data))
  
  ## ---- 29. Make BBI numeric ----
  bbi_data$bbi <- suppressWarnings(as.numeric(bbi_data$bbi))
  
  ## ---- 30. Create date ----
  bbi_data$date <- as.Date(bbi_data$ts_utc)
  
  ## ---- 31. Create very simple daily BBI summary
  ## For now this is just a check file, not full HRV yet
  daily_bbi_check <- bbi_data %>%
    group_by(participant_id, date) %>%
    summarise(
      n_rows_bbi = n(),
      n_nonmissing_bbi = sum(!is.na(bbi)),
      mean_bbi_24hr = mean(bbi, na.rm = TRUE),
      sd_bbi_24hr = sd(bbi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(participant_id, date)
  
  ## ---- 32. View result ----
  print(daily_bbi_check)
  
  ## ---- 33. Save result ----
  saveRDS(daily_bbi_check, file.path(day_folder, "garmin_daily_bbi_check.rds"))
}

## =========================================================
## 		JOIN DAILY FILES TOGETHER
## =========================================================

## ---- 34. Start with an empty list ----
daily_list <- list()

## ---- 35. Add daily files if they exist ----
if (exists("daily_steps")) daily_list[[length(daily_list) + 1]] <- daily_steps
if (exists("daily_hr")) daily_list[[length(daily_list) + 1]] <- daily_hr
if (exists("daily_stress")) daily_list[[length(daily_list) + 1]] <- daily_stress
if (exists("daily_bbi_check")) daily_list[[length(daily_list) + 1]] <- daily_bbi_check

## ---- 36. Join all daily files together ----
if (length(daily_list) > 0) {
  
  daily_all <- daily_list[[1]]
  
  if (length(daily_list) > 1) {
    for (i in 2:length(daily_list)) {
      daily_all <- full_join(daily_all, daily_list[[i]], by = c("participant_id", "date"))
    }
  }
  
  ## ---- 37. Sort rows ----
  daily_all <- daily_all %>%
    arrange(participant_id, date)
  
  ## ---- 38. View combined daily file ----
  print(daily_all)
  
  ## ---- 39. Save combined daily file ----
  saveRDS(daily_all, file.path(day_folder, "garmin_daily_all_simple.rds"))
}

message("03_derive_daily_simple.R complete")