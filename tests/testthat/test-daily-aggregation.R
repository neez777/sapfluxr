# ============================================================================
# test-daily-aggregation.R
# ============================================================================
# Tests for daily aggregation functions (Step 9)
#
# Tests cover:
# - aggregate_daily() - temporal aggregation to daily totals
# - normalise_daily() - daily normalisation
# - Internal helper functions (detect_interval, parse_interval)
# ============================================================================

library(testthat)
library(sapfluxr)

# ============================================================================
# Tests for aggregate_daily()
# ============================================================================

test_that("aggregate_daily works with hourly data (n=24)", {
  # Create 3 days of hourly data
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-03 23:00:00", tz = "UTC"),
    by = "hour"
  )

  # Simulate diurnal pattern
  hour_of_day <- as.numeric(format(datetime, "%H"))
  Qps <- 2 + 8 * sin((hour_of_day - 6) * pi / 12)  # Peak at noon
  Qps[Qps < 0] <- 0

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = Qps,
    Q_cm3_hr = Qps * 200  # Assuming As = 200 cm²
  )

  daily_data <- aggregate_daily(flux_data, interval = "hourly")

  # Check structure
  expect_equal(nrow(daily_data), 3)
  expect_true("Jvm_daily_cm3_cm2_day" %in% names(daily_data))
  expect_true("Qp_daily_L_day" %in% names(daily_data))

  # Check that daily sum is calculated correctly
  # For hourly: Jvm_daily = sum(Qps)
  day1_data <- flux_data[as.Date(flux_data$datetime) == as.Date("2024-01-01"), ]
  expected_Jvm <- sum(day1_data$Qps_cm_hr)

  expect_equal(daily_data$Jvm_daily_cm3_cm2_day[1], expected_Jvm, tolerance = 0.01)
})

test_that("aggregate_daily works with half-hourly data (n=48)", {
  # Create 2 days of half-hourly data
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-02 23:30:00", tz = "UTC"),
    by = "30 min"
  )

  Qps <- rep(5, length(datetime))  # Constant for simplicity
  Q <- Qps * 200

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = Qps,
    Q_cm3_hr = Q
  )

  daily_data <- aggregate_daily(flux_data, interval = "half-hourly")

  # Check structure
  expect_equal(nrow(daily_data), 2)

  # For half-hourly: Jvm_daily = sum(Qps) / 2
  # With Qps = 5 cm/hr for 48 measurements:
  # sum = 240, Jvm_daily = 240 / 2 = 120 cm/day
  expect_equal(daily_data$Jvm_daily_cm3_cm2_day[1], 120, tolerance = 0.01)
})

test_that("aggregate_daily auto-detects hourly interval", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, 24),
    Q_cm3_hr = rep(1000, 24)
  )

  daily_data <- aggregate_daily(flux_data, interval = "auto")

  # Should detect 1-hour interval
  # Jvm_daily = sum(5) × 1 = 120 cm/day
  expect_equal(daily_data$Jvm_daily_cm3_cm2_day[1], 120, tolerance = 0.01)
})

test_that("aggregate_daily auto-detects half-hourly interval", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:30:00", tz = "UTC"),
    by = "30 min"
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, 48)
  )

  daily_data <- aggregate_daily(flux_data, interval = "auto")

  # Should detect 0.5-hour interval
  # Jvm_daily = sum(5) × 0.5 = 120 cm/day
  expect_equal(daily_data$Jvm_daily_cm3_cm2_day[1], 120, tolerance = 0.01)
})

test_that("aggregate_daily calculates total flux correctly", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    datetime = datetime,
    Q_cm3_hr = rep(1000, 24)  # 1000 cm³/hr
  )

  daily_data <- aggregate_daily(flux_data, interval = "hourly",
                                 flux_density_col = NULL)

  # Qp_daily = sum(1000) × 1 = 24000 cm³/day
  expect_equal(daily_data$Qp_daily_cm3_day[1], 24000, tolerance = 0.01)

  # Convert to L/day: 24000 / 1000 = 24 L/day
  expect_equal(daily_data$Qp_daily_L_day[1], 24.0, tolerance = 0.01)
})

test_that("aggregate_daily handles multiple days", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-05 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, length(datetime))
  )

  daily_data <- aggregate_daily(flux_data)

  expect_equal(nrow(daily_data), 5)
  expect_equal(daily_data$date[1], as.Date("2024-01-01"))
  expect_equal(daily_data$date[5], as.Date("2024-01-05"))
})

test_that("aggregate_daily handles incomplete days", {
  # Day 1: complete (24 hours)
  # Day 2: incomplete (12 hours)
  datetime <- c(
    seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-01 23:00:00", tz = "UTC"), by = "hour"),
    seq(as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-02 11:00:00", tz = "UTC"), by = "hour")
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, length(datetime))
  )

  # Without requiring complete days
  daily_data <- aggregate_daily(flux_data, require_complete_days = FALSE)

  expect_equal(nrow(daily_data), 2)
  expect_equal(daily_data$n_measurements[1], 24)
  expect_equal(daily_data$n_measurements[2], 12)
  expect_equal(daily_data$data_completeness[1], 1.0)
  expect_equal(daily_data$data_completeness[2], 0.5)
})

test_that("aggregate_daily excludes incomplete days when required", {
  # Day 1: complete, Day 2: incomplete
  datetime <- c(
    seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-01 23:00:00", tz = "UTC"), by = "hour"),
    seq(as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-02 11:00:00", tz = "UTC"), by = "hour")
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, length(datetime))
  )

  daily_data <- aggregate_daily(flux_data, require_complete_days = TRUE)

  # Should only include day 1
  expect_equal(nrow(daily_data), 1)
  expect_equal(daily_data$date[1], as.Date("2024-01-01"))
})

test_that("aggregate_daily respects min_measurements_per_day", {
  datetime <- c(
    seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-01 23:00:00", tz = "UTC"), by = "hour"),  # 24 obs
    seq(as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-02 11:00:00", tz = "UTC"), by = "hour")   # 12 obs
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, length(datetime))
  )

  daily_data <- aggregate_daily(flux_data, min_measurements_per_day = 20)

  # Should only include day 1 (24 >= 20)
  expect_equal(nrow(daily_data), 1)
})

test_that("aggregate_daily converts mm/day correctly", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = rep(5, 24)
  )

  daily_data <- aggregate_daily(flux_data)

  # Jvm_daily_cm = 120 cm/day
  # Jvm_daily_mm = 120 × 10 = 1200 mm/day
  expect_equal(daily_data$Jvm_daily_mm_day[1],
               daily_data$Jvm_daily_cm3_cm2_day[1] * 10)
})

test_that("aggregate_daily handles NA values correctly", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  Qps <- rep(5, 24)
  Qps[c(5, 10, 15)] <- NA

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = Qps
  )

  daily_data <- aggregate_daily(flux_data)

  # sum(Qps, na.rm=TRUE) = 5 × 21 = 105
  expect_equal(daily_data$Jvm_daily_cm3_cm2_day[1], 105, tolerance = 0.01)
})

test_that("aggregate_daily handles custom column names", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    timestamp = datetime,
    flux_density = rep(5, 24),
    total_flux = rep(1000, 24)
  )

  daily_data <- aggregate_daily(
    flux_data,
    datetime_col = "timestamp",
    flux_density_col = "flux_density",
    total_flux_col = "total_flux"
  )

  expect_true("Jvm_daily_cm3_cm2_day" %in% names(daily_data))
  expect_true("Qp_daily_L_day" %in% names(daily_data))
})

test_that("aggregate_daily rejects missing datetime column", {
  flux_data <- data.frame(
    timestamp = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    Qps_cm_hr = 5
  )

  expect_error(
    aggregate_daily(flux_data),
    "Column 'datetime' not found"
  )
})

test_that("aggregate_daily rejects non-POSIXct datetime", {
  flux_data <- data.frame(
    datetime = "2024-01-01 00:00:00",
    Qps_cm_hr = 5
  )

  expect_error(
    aggregate_daily(flux_data),
    "must be POSIXct class"
  )
})

test_that("aggregate_daily requires at least one flux column", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  flux_data <- data.frame(
    datetime = datetime,
    other_column = rep(5, 24)
  )

  expect_error(
    aggregate_daily(flux_data, flux_density_col = NULL, total_flux_col = NULL),
    "At least one of flux_density_col or total_flux_col must be provided"
  )
})


# ============================================================================
# Tests for normalise_daily()
# ============================================================================

test_that("normalise_daily performs global normalisation correctly", {
  daily_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    Jvm_daily_cm3_cm2_day = c(50, 100, 150)
  )

  result <- normalise_daily(daily_data, period = "global")

  expect_true("Jvm_daily_cm3_cm2_day_normalised" %in% names(result))
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised, c(50/150, 100/150, 1.0))
})

test_that("normalise_daily performs monthly normalisation correctly", {
  daily_data <- data.frame(
    date = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-01-30",  # Jan
      "2024-02-01", "2024-02-15", "2024-02-28"   # Feb
    )),
    Jvm_daily_cm3_cm2_day = c(50, 100, 80,  # Jan max = 100
                               60, 120, 90)  # Feb max = 120
  )

  result <- normalise_daily(daily_data, period = "monthly")

  # January normalised by 100
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[1], 50/100)
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[2], 1.0)
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[3], 80/100)

  # February normalised by 120
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[4], 60/120)
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[5], 1.0)
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised[6], 90/120)
})

test_that("normalise_daily performs custom normalisation correctly", {
  daily_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    Jvm_daily_cm3_cm2_day = c(50, 100)
  )

  result <- normalise_daily(daily_data, period = "custom", max_value = 200)

  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised, c(50/200, 100/200))
})

test_that("normalise_daily warns when custom max not provided", {
  daily_data <- data.frame(
    date = as.Date("2024-01-01"),
    Jvm_daily_cm3_cm2_day = 100
  )

  expect_warning(
    result <- normalise_daily(daily_data, period = "custom"),
    "period = 'custom' but max_value not provided"
  )

  # Should use global max
  expect_equal(result$Jvm_daily_cm3_cm2_day_normalised, 1.0)
})

test_that("normalise_daily handles custom output column name", {
  daily_data <- data.frame(
    date = as.Date("2024-01-01"),
    Jvm_daily_cm3_cm2_day = 100
  )

  result <- normalise_daily(daily_data, output_col = "normalised_flux")

  expect_true("normalised_flux" %in% names(result))
  expect_false("Jvm_daily_cm3_cm2_day_normalised" %in% names(result))
})

test_that("normalise_daily can normalise total flux", {
  daily_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    Qp_daily_L_day = c(10, 20)
  )

  result <- normalise_daily(daily_data, normalise_col = "Qp_daily_L_day")

  expect_equal(result$Qp_daily_L_day_normalised, c(0.5, 1.0))
})

test_that("normalise_daily rejects missing column", {
  daily_data <- data.frame(
    date = as.Date("2024-01-01"),
    other_col = 100
  )

  expect_error(
    normalise_daily(daily_data),
    "Column 'Jvm_daily_cm3_cm2_day' not found"
  )
})

test_that("normalise_daily rejects invalid period", {
  daily_data <- data.frame(
    date = as.Date("2024-01-01"),
    Jvm_daily_cm3_cm2_day = 100
  )

  expect_error(
    normalise_daily(daily_data, period = "yearly"),
    "period must be one of"
  )
})


# ============================================================================
# Tests for internal helper functions
# ============================================================================

test_that("detect_interval identifies hourly data", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  interval <- sapfluxr:::detect_interval(datetime)

  expect_equal(interval, 1.0)
})

test_that("detect_interval identifies half-hourly data", {
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 23:30:00", tz = "UTC"),
    by = "30 min"
  )

  interval <- sapfluxr:::detect_interval(datetime)

  expect_equal(interval, 0.5)
})

test_that("detect_interval handles irregular data", {
  # Mostly hourly with one gap
  datetime <- c(
    seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2024-01-01 10:00:00", tz = "UTC"), by = "hour"),
    as.POSIXct("2024-01-01 15:00:00", tz = "UTC"),  # 5-hour gap
    seq(as.POSIXct("2024-01-01 16:00:00", tz = "UTC"),
        as.POSIXct("2024-01-01 23:00:00", tz = "UTC"), by = "hour")
  )

  interval <- sapfluxr:::detect_interval(datetime)

  # Should use median (1 hour), not affected by the 5-hour gap
  expect_equal(interval, 1.0)
})

test_that("parse_interval parses hour strings", {
  expect_equal(sapfluxr:::parse_interval("1 hour"), 1.0)
  expect_equal(sapfluxr:::parse_interval("2 hours"), 2.0)
  expect_equal(sapfluxr:::parse_interval("0.5 hour"), 0.5)
})

test_that("parse_interval parses minute strings", {
  expect_equal(sapfluxr:::parse_interval("30 min"), 0.5)
  expect_equal(sapfluxr:::parse_interval("15 min"), 0.25)
  expect_equal(sapfluxr:::parse_interval("60 minutes"), 1.0)
})

test_that("parse_interval rejects invalid strings", {
  expect_error(
    sapfluxr:::parse_interval("invalid"),
    "Could not parse interval string"
  )
})


# ============================================================================
# Integration test with realistic data
# ============================================================================

test_that("Integration: full daily aggregation workflow", {
  # Create 7 days of hourly data with realistic diurnal pattern
  datetime <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-07 23:00:00", tz = "UTC"),
    by = "hour"
  )

  # Diurnal pattern with day-to-day variation
  hour_of_day <- as.numeric(format(datetime, "%H"))
  day_of_week <- as.numeric(format(datetime, "%d"))

  # Base pattern + daily variation
  Qps <- (2 + 8 * sin((hour_of_day - 6) * pi / 12)) * (0.8 + 0.4 * (day_of_week / 7))
  Qps[Qps < 0] <- 0

  flux_data <- data.frame(
    datetime = datetime,
    Qps_cm_hr = Qps,
    Q_cm3_hr = Qps * 300
  )

  # Aggregate to daily
  daily_data <- aggregate_daily(flux_data)

  # Normalise
  daily_data <- normalise_daily(daily_data)

  # Verify structure
  expect_equal(nrow(daily_data), 7)
  expect_true(all(c("Jvm_daily_cm3_cm2_day", "Qp_daily_L_day",
                     "Jvm_daily_cm3_cm2_day_normalised") %in% names(daily_data)))

  # Verify completeness
  expect_true(all(daily_data$data_completeness == 1.0))

  # Verify normalisation
  expect_equal(max(daily_data$Jvm_daily_cm3_cm2_day_normalised), 1.0)
  expect_true(min(daily_data$Jvm_daily_cm3_cm2_day_normalised) > 0)
  expect_true(min(daily_data$Jvm_daily_cm3_cm2_day_normalised) < 1)

  # Verify increasing trend (due to day_of_week factor)
  expect_true(daily_data$Jvm_daily_cm3_cm2_day[7] >
              daily_data$Jvm_daily_cm3_cm2_day[1])
})
