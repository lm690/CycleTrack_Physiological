library(dplyr)
library(lubridate)
library(slider)
library(tidyr)

## =========================================================
## Helper function:
## fill short FALSE gaps inside TRUE runs
##
## Example:
## TRUE TRUE FALSE FALSE TRUE
## becomes
## TRUE TRUE TRUE TRUE TRUE
## if max_gap >= 2
## =========================================================
fill_short_false_gaps <- function(x, max_gap = 10) {
  r <- rle(x)
  
  for (i in seq_along(r$values)) {
    if (r$values[i] == FALSE &&
        r$lengths[i] <= max_gap &&
        i > 1 &&
        i < length(r$values) &&
        r$values[i - 1] == TRUE &&
        r$values[i + 1] == TRUE) {
      r$values[i] <- TRUE
    }
  }
  
  inverse.rle(r)
}

## =========================================================
## 1. Start from garmin_all and create time variables
## =========================================================
x <- garmin_all %>%
  mutate(
    ts_local = as.POSIXct(ts_local),
    minute_local = floor_date(ts_local, "minute"),
    date_local = as.Date(ts_local)
  )

## =========================================================
## 2. Create minute-level wear
##
## wear_raw:
##   TRUE if there is any usable BBI or HR in that minute
##
## wear:
##   smoothed version using an 11-minute rolling window
## =========================================================
wear_df <- x %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    wear_raw = any(
      (!is.na(bbi) & bbi > 0) |
        (!is.na(beatsPerMinute) & beatsPerMinute > 30)
    ),
    .groups = "drop"
  ) %>%
  group_by(participant_id) %>%
  complete(
    minute_local = seq(min(minute_local), max(minute_local), by = "1 min"),
    fill = list(wear_raw = FALSE)
  ) %>%
  arrange(minute_local, .by_group = TRUE) %>%
  mutate(
    wear = slide_dbl(
      as.numeric(wear_raw),
      ~ mean(.x, na.rm = TRUE),
      .before = 5,
      .after = 5
    ) >= 0.5
  ) %>%
  ungroup()

## =========================================================
## 3. Minute-level movement
##
## Uses zeroCrossingCount averaged within the minute
## =========================================================
act_df <- x %>%
  filter(!is.na(zeroCrossingCount)) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    zc = mean(zeroCrossingCount, na.rm = TRUE),
    .groups = "drop"
  )

## =========================================================
## 4. Minute-level heart rate
## =========================================================
hr_df <- x %>%
  filter(!is.na(beatsPerMinute), beatsPerMinute > 30) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    hr = mean(beatsPerMinute, na.rm = TRUE),
    .groups = "drop"
  )

## =========================================================
## 5. Minute-level steps
## =========================================================
steps_df <- x %>%
  filter(!is.na(steps)) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    steps = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )

## =========================================================
## 6. Combine minute-level signals
##
## monitoring_date:
##   if time is 20:00 or later, assign to next day's sleep
## =========================================================
minute_state <- wear_df %>%
  left_join(act_df, by = c("participant_id", "minute_local")) %>%
  left_join(hr_df, by = c("participant_id", "minute_local")) %>%
  left_join(steps_df, by = c("participant_id", "minute_local")) %>%
  mutate(
    hour_local = hour(minute_local),
    date_local = as.Date(minute_local),
    monitoring_date = if_else(
      hour_local >= 20,
      as.Date(minute_local) + 1,
      as.Date(minute_local)
    ),
    in_night = hour_local >= 20 | hour_local < 12
  )

## =========================================================
## 7. Define raw sleep-like minutes
##
## Rule:
## - watch worn
## - in night window
## - low movement
## - low HR if HR present
## =========================================================
minute_state <- minute_state %>%
  mutate(
    sleep_raw = wear &
      in_night &
      !is.na(zc) & zc <= 50 &
      (is.na(hr) | hr <= 70)
  )

## =========================================================
## 8. Smooth sleep and bridge short overnight interruptions
##
## sleep_smooth:
##   5-minute rolling majority
##
## sleep_bridge:
##   fills short FALSE gaps (<= 10 minutes) inside sleep
##
## sleep:
##   only TRUE if part of a run lasting at least 10 minutes
## =========================================================
minute_state <- minute_state %>%
  arrange(participant_id, minute_local) %>%
  group_by(participant_id, monitoring_date) %>%
  mutate(
    sleep_smooth = slide_dbl(
      as.numeric(sleep_raw),
      ~ mean(.x, na.rm = TRUE),
      .before = 2,
      .after = 2
    ) >= 0.6
  ) %>%
  mutate(
    sleep_bridge = fill_short_false_gaps(sleep_smooth, max_gap = 10)
  ) %>%
  mutate(
    sleep_run = cumsum(coalesce(sleep_bridge != lag(sleep_bridge), TRUE))
  ) %>%
  group_by(participant_id, monitoring_date, sleep_run) %>%
  mutate(
    sleep_run_length = ifelse(first(sleep_bridge), n(), 0)
  ) %>%
  ungroup() %>%
  mutate(
    sleep = sleep_bridge & sleep_run_length >= 20
  )

## =========================================================
## 9. Final minute-level state
##
## IMPORTANT:
## state must be created BEFORE period_id is created
## =========================================================
minute_state <- minute_state %>%
  mutate(
    state = case_when(
      !wear ~ "nonwear",
      sleep ~ "sleep",
      TRUE ~ "wake"
    )
  )

## =========================================================
## 10. Create contiguous state periods
##
## period_id changes whenever state changes
## =========================================================
minute_state <- minute_state %>%
  arrange(participant_id, minute_local) %>%
  group_by(participant_id) %>%
  mutate(
    state_change = coalesce(state != lag(state), TRUE),
    period_id = cumsum(state_change)
  ) %>%
  ungroup()

## =========================================================
## 11. Summarise each contiguous period
##
## One row per continuous block of sleep / wake / nonwear
## =========================================================
period_summary <- minute_state %>%
  group_by(participant_id, period_id, state) %>%
  summarise(
    period_start = min(minute_local),
    period_end = max(minute_local) + minutes(1),
    duration_min = as.numeric(difftime(period_end, period_start, units = "mins")),
    monitoring_date = first(monitoring_date),
    n_minutes = n(),
    wear_minutes = sum(wear, na.rm = TRUE),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = sd(hr, na.rm = TRUE),
    zc_mean = mean(zc, na.rm = TRUE),
    zc_sd = sd(zc, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )

## =========================================================
## 12. Keep just sleep and wake periods if wanted
## =========================================================
sleep_wake_period_summary <- period_summary %>%
  filter(state %in% c("sleep", "wake"))

## =========================================================
## 13. Add labels back to the original long garmin_all
##
## Each original row gets the minute-level state
## =========================================================
garmin_all_labeled <- x %>%
  left_join(
    minute_state %>%
      select(
        participant_id,
        minute_local,
        monitoring_date,
        wear_raw,
        wear,
        sleep_raw,
        sleep_smooth,
        sleep_bridge,
        sleep_run_length,
        sleep,
        state,
        period_id
      ),
    by = c("participant_id", "minute_local")
  )

## =========================================================
## 14. Summarise original long data within each period
##
## This gives one row per participant x period_id x state
## using the original long rows
## =========================================================
period_summary_long <- garmin_all_labeled %>%
  filter(state %in% c("sleep", "wake")) %>%
  group_by(participant_id, monitoring_date, period_id, state) %>%
  summarise(
    period_start = min(ts_local, na.rm = TRUE),
    period_end = max(ts_local, na.rm = TRUE),
    n_rows = n(),
    bbi_mean = mean(bbi, na.rm = TRUE),
    bbi_sd = sd(bbi, na.rm = TRUE),
    hr_mean = mean(beatsPerMinute, na.rm = TRUE),
    hr_sd = sd(beatsPerMinute, na.rm = TRUE),
    stress_mean = mean(stressLevel, na.rm = TRUE),
    stress_sd = sd(stressLevel, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    zeroCrossing_mean = mean(zeroCrossingCount, na.rm = TRUE),
    .groups = "drop"
  )

## =========================================================
## 15. Daily summary by state
##
## One row per participant x monitoring_date x state
## =========================================================
daily_period_summary <- minute_state %>%
  filter(state %in% c("sleep", "wake")) %>%
  group_by(participant_id, monitoring_date, state) %>%
  summarise(
    total_minutes = n(),
    wear_minutes = sum(wear, na.rm = TRUE),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = sd(hr, na.rm = TRUE),
    zc_mean = mean(zc, na.rm = TRUE),
    zc_sd = sd(zc, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )
