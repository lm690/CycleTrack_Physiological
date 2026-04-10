## =========================================================
## 06_stress_simple.R
## Purpose: Derive simple daily 24hr stress metrics
## Output:  data/processed/daily/garmin_daily_stress.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Set folders ----
day_folder <- Sys.getenv("DAILYDIR")
type_folder <- Sys.getenv("TYPEDIR")

## ---- 3. Read stress data ----
stress_data <- readRDS(file.path(type_folder, "garmin_clean_garmin_device_stress.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(stress_data))
print(names(stress_data))

## ---- 5. Make sure stress is numeric ----
stress_data$stressLevel <- suppressWarnings(as.numeric(stress_data$stressLevel))

## ---- 6. Make sure unix timestamp is numeric ----
stress_data$unixTimestampInMs <- suppressWarnings(as.numeric(stress_data$unixTimestampInMs))

## ---- 7. Make sure ts_utc exists ----
if (!"ts_utc" %in% names(stress_data)) {
  stress_data$ts_utc <- as.POSIXct(
    stress_data$unixTimestampInMs / 1000,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

## ---- 8. Create date variable ----
stress_data$date <- as.Date(stress_data$ts_utc)

## ---- 9. Keep only non-missing stress values ----
stress_data <- stress_data %>%
  filter(!is.na(stressLevel))

## ---- 10. Keep only valid Garmin stress values for summaries ----
## Remove special codes -1 and -2, and anything outside 1 to 100
stress_data_valid <- stress_data %>%
  filter(stressLevel >= 1, stressLevel <= 100)

## ---- 11. Create stress category flags ----
stress_data_valid <- stress_data_valid %>%
  mutate(
    stress_rest_flag   = ifelse(stressLevel >= 1  & stressLevel <= 25, 1, 0),
    stress_low_flag    = ifelse(stressLevel >= 26 & stressLevel <= 50, 1, 0),
    stress_medium_flag = ifelse(stressLevel >= 51 & stressLevel <= 75, 1, 0),
    stress_high_flag   = ifelse(stressLevel >= 76 & stressLevel <= 100, 1, 0)
  )

## ---- 12. Summarise to daily stress metrics ----
daily_stress <- stress_data_valid %>%
  group_by(participant_id, date) %>%
  summarise(
    n_stress = n(),
    
    stress_mean_24hr = mean(stressLevel, na.rm = TRUE),
    stress_min_24hr  = min(stressLevel, na.rm = TRUE),
    stress_max_24hr  = max(stressLevel, na.rm = TRUE),
    stress_sd_24hr   = sd(stressLevel, na.rm = TRUE),
    
    stress_rest_24hr   = mean(stress_rest_flag, na.rm = TRUE) * 100,
    stress_low_24hr    = mean(stress_low_flag, na.rm = TRUE) * 100,
    stress_medium_24hr = mean(stress_medium_flag, na.rm = TRUE) * 100,
    stress_high_24hr   = mean(stress_high_flag, na.rm = TRUE) * 100,
    
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 13. View output ----
print(daily_stress)
summary(daily_stress)

## ---- 14. Save output ----
saveRDS(daily_stress, file.path(day_folder, "garmin_daily_stress.rds"))

message("06_stress_simple.R complete")