## =========================================================
## 05_HRV_simple.R
## Purpose: Derive simple daily 24hr HRV metrics from Garmin BBI
## Output:  data/processed/daily/garmin_daily_hrv.rds
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

## ---- 3. Read BBI data ----
bbi_data <- readRDS(file.path(type_folder, "garmin_clean_garmin_device_bbi.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(bbi_data))
print(names(bbi_data))

## ---- 5. Make sure BBI is numeric ----
bbi_data$bbi <- suppressWarnings(as.numeric(bbi_data$bbi))

## ---- 6. Make sure unix timestamp is numeric ----
bbi_data$unixTimestampInMs <- suppressWarnings(as.numeric(bbi_data$unixTimestampInMs))

## ---- 7. Make sure ts_utc exists ----
if (!"ts_utc" %in% names(bbi_data)) {
  bbi_data$ts_utc <- as.POSIXct(
    bbi_data$unixTimestampInMs / 1000,
    origin = "2000-01-01",
    tz = "UTC"
  )
}

## ---- 8. Create date variable ----
bbi_data$date <- as.Date(bbi_data$ts_utc)

## ---- 9. Keep only rows with non-missing BBI ----
bbi_data <- bbi_data %>%
  filter(!is.na(bbi))

## ---- 10. Keep only plausible BBI values ----
## Simple first-pass cleaning rule: 300 to 2000 ms
bbi_data <- bbi_data %>%
  filter(bbi >= 300, bbi <= 2000)

## ---- 11. Sort data by participant and time ----
bbi_data <- bbi_data %>%
  arrange(participant_id, date, ts_utc)

## ---- 12. Calculate BBI difference within each participant-day ----
bbi_data <- bbi_data %>%
  group_by(participant_id, date) %>%
  mutate(
    bbi_diff = bbi - lag(bbi)
  ) %>%
  ungroup()

## ---- 13. Calculate squared BBI difference ----
bbi_data <- bbi_data %>%
  mutate(
    bbi_diff_sq = bbi_diff^2
  )

## ---- 14. Create indicator for RR50 ----
## Count absolute differences greater than 50 ms
bbi_data <- bbi_data %>%
  mutate(
    rr50_flag = ifelse(!is.na(bbi_diff) & abs(bbi_diff) > 50, 1, 0)
  )

## ---- 15. Summarise to daily HRV metrics ----
daily_hrv <- bbi_data %>%
  group_by(participant_id, date) %>%
  summarise(
    n_bbi = n(),
    n_bbi_diff = sum(!is.na(bbi_diff)),
    
    hrv_sdrr_24hr = sd(bbi, na.rm = TRUE),
    
    hrv_rmssd_24hr = sqrt(mean(bbi_diff_sq, na.rm = TRUE)),
    
    hrv_rr50_24hr = sum(rr50_flag, na.rm = TRUE),
    
    hrv_prr50_24hr = hrv_rr50_24hr / n_bbi_diff,
    
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 16. Avoid divide-by-zero problems ----
daily_hrv$hrv_prr50_24hr[daily_hrv$n_bbi_diff == 0] <- NA

## ---- 17. View output ----
print(daily_hrv)
summary(daily_hrv)

## ---- 18. Save output ----
saveRDS(daily_hrv, file.path(day_folder, "garmin_daily_hrv.rds"))

message("05_HRV_simple.R complete")