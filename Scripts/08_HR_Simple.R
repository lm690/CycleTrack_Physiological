## =========================================================
## 08_hr_simple.R
## Purpose: Derive simple daily 24hr heart-rate metrics
## Output:  data/processed/daily/garmin_daily_hr.rds
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

## ---- 3. Read heart-rate data ----
hr_data <- readRDS(file.path(type_folder, "garmin_clean_garmin_device_heart_rate.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(hr_data))
print(names(hr_data))

## ---- 5. Make sure heart rate is numeric ----
hr_data$beatsPerMinute <- suppressWarnings(as.numeric(hr_data$beatsPerMinute))

## ---- 6. Make sure unix timestamp is numeric ----
hr_data$unixTimestampInMs <- suppressWarnings(as.numeric(hr_data$unixTimestampInMs))

## ---- 7. Make sure ts_utc exists ----
if (!"ts_utc" %in% names(hr_data)) {
  hr_data$ts_utc <- as.POSIXct(
    hr_data$unixTimestampInMs / 1000,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

## ---- 8. Create date variable ----
hr_data$date <- as.Date(hr_data$ts_utc)

## ---- 9. Keep only rows with non-missing heart rate ----
hr_data <- hr_data %>%
  filter(!is.na(beatsPerMinute))

## ---- 10. Keep only plausible heart-rate values ----
## Simple first-pass cleaning rule
hr_data <- hr_data %>%
  filter(beatsPerMinute >= 20, beatsPerMinute <= 250)

## ---- 11. Sort rows ----
hr_data <- hr_data %>%
  arrange(participant_id, date, ts_utc)

## ---- 12. Summarise to daily heart-rate metrics ----
daily_hr <- hr_data %>%
  group_by(participant_id, date) %>%
  summarise(
    n_hr = n(),
    
    hr_mean_24hr = mean(beatsPerMinute, na.rm = TRUE),
    hr_min_24hr  = min(beatsPerMinute, na.rm = TRUE),
    hr_max_24hr  = max(beatsPerMinute, na.rm = TRUE),
    hr_sd_24hr   = sd(beatsPerMinute, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 13. View output ----
print(daily_hr)
summary(daily_hr)

## ---- 14. Save output ----
saveRDS(daily_hr, file.path(day_folder, "garmin_daily_hr.rds"))

message("08_hr_simple.R complete")