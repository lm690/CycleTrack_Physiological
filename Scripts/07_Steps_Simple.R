## =========================================================
## 07_steps_simple.R
## Purpose: Derive simple daily 24hr step metrics
## Output:  data/processed/daily/garmin_daily_steps.rds
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

## ---- 3. Read step data ----
step_data <- readRDS(file.path(type_folder, "garmin_clean_garmin_device_step.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(step_data))
print(names(step_data))

## ---- 5. Make sure key variables are numeric ----
step_data$steps <- suppressWarnings(as.numeric(step_data$steps))
step_data$totalSteps <- suppressWarnings(as.numeric(step_data$totalSteps))
step_data$unixTimestampInMs <- suppressWarnings(as.numeric(step_data$unixTimestampInMs))

## ---- 6. Make sure ts_utc exists ----
if (!"ts_utc" %in% names(step_data)) {
  step_data$ts_utc <- as.POSIXct(
    step_data$unixTimestampInMs / 1000,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

## ---- 7. Create date variable ----
step_data$date <- as.Date(step_data$ts_utc)

## ---- 8. Create hour variable ----
step_data$hour_utc <- floor_date(step_data$ts_utc, unit = "hour")

## ---- 9. Sort rows ----
step_data <- step_data %>%
  arrange(participant_id, ts_utc)

## ---- 10. Create hourly step totals
## Use steps field for hourly summed step counts
hourly_steps <- step_data %>%
  group_by(participant_id, date, hour_utc) %>%
  summarise(
    hourly_steps = sum(steps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(participant_id, date, hour_utc)

## ---- 11. Create flag for hourly threshold
## Simple provisional threshold: >100 steps in the hour
hourly_steps <- hourly_steps %>%
  mutate(
    over_100_flag = ifelse(hourly_steps > 100, 1, 0)
  )

## ---- 12. Summarise hourly steps to daily metrics
daily_steps_hourly <- hourly_steps %>%
  group_by(participant_id, date) %>%
  summarise(
    steps_minhourlysteps_24hr = min(hourly_steps, na.rm = TRUE),
    steps_maxhourlysteps_24hr = max(hourly_steps, na.rm = TRUE),
    steps_meanhourlysteps_24hr = mean(hourly_steps, na.rm = TRUE),
    steps_sdhourlysteps_24hr = sd(hourly_steps, na.rm = TRUE),
    steps_pcstepsover100_24hr = mean(over_100_flag, na.rm = TRUE) * 100,
    n_hours_with_data = n(),
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 13. Create daily total steps
## Use maximum totalSteps value within each day
daily_steps_total <- step_data %>%
  group_by(participant_id, date) %>%
  summarise(
    steps_totalsteps_24hr = max(totalSteps, na.rm = TRUE),
    steps_sumsteps_24hr = sum(steps, na.rm = TRUE),
    n_rows_steps = n(),
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 14. Replace infinite totals caused by all-missing values
daily_steps_total$steps_totalsteps_24hr[
  is.infinite(daily_steps_total$steps_totalsteps_24hr)
] <- NA

## ---- 15. Join daily totals and hourly summaries
daily_steps <- full_join(
  daily_steps_total,
  daily_steps_hourly,
  by = c("participant_id", "date")
) %>%
  arrange(participant_id, date)

## ---- 16. View output
print(daily_steps)
summary(daily_steps)

## ---- 17. Save output
saveRDS(daily_steps, file.path(day_folder, "garmin_daily_steps.rds"))

message("07_steps_simple.R complete")