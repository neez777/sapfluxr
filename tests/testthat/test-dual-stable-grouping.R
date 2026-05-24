# Regression tests for dual-stable spacing filter
#
# Guards against the triple-application bug where the spacing filter was applied
# independently to VPD-stable days and Vh-stable days before their intersection,
# causing second-half deployment dates to vanish at min_segment_days >= 14.

# ── Helpers ───────────────────────────────────────────────────────────────────

make_dual_stable_data <- function(n_days = 400, seed = 42) {
  set.seed(seed)
  start <- as.POSIXct("2024-08-01 00:00:00", tz = "UTC")
  datetimes <- seq(start, by = "30 min", length.out = n_days * 48)
  dates <- as.Date(datetimes, tz = "UTC")
  hours <- as.integer(format(datetimes, "%H"))

  # VPD: low (0.1–0.4 kPa) in predawn, higher during day, scattered across full year
  vpd <- ifelse(
    hours %in% 2:5,
    runif(length(datetimes), 0.05, 0.35),
    runif(length(datetimes), 0.6, 2.0)
  )

  weather_data <- data.frame(
    datetime = datetimes,
    vpd_kpa  = vpd,
    stringsAsFactors = FALSE
  )

  # Vh: predawn values near zero (–0.3 to 0.3 cm/hr) throughout full year
  vh <- ifelse(
    hours %in% 2:5,
    runif(length(datetimes), -0.30, 0.30),
    runif(length(datetimes),  0.5,  2.5)
  )

  vh_data <- data.frame(
    datetime = datetimes,
    vh_col   = vh,
    stringsAsFactors = FALSE
  )

  list(weather_data = weather_data, vh_data = vh_data)
}

# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("dual-stable dates span the full deployment with min_segment_days = 30", {
  d <- make_dual_stable_data(n_days = 400)

  result <- find_dual_stable_periods(
    vh_data        = d$vh_data,
    weather_data   = d$weather_data,
    vh_col         = "vh_col",
    predawn_window = c(2, 6),
    mode           = "static",
    vpd_threshold  = 0.5,
    vh_threshold   = 2.0,
    min_segment_days = 30
  )

  dates <- result$dual_stable_dates
  skip_if(length(dates) == 0, "No dual-stable dates found in synthetic data — check thresholds")

  deployment_days <- as.numeric(diff(range(as.Date(d$weather_data$datetime, tz = "UTC"))))
  span_days <- as.numeric(diff(range(dates)))

  # Selected dates must cover at least 80% of the deployment span
  expect_gte(span_days / deployment_days, 0.80)
})


test_that("increasing min_segment_days shrinks count but preserves deployment span", {
  d <- make_dual_stable_data(n_days = 400)

  runs <- lapply(c(1, 7, 14, 30), function(msd) {
    find_dual_stable_periods(
      vh_data        = d$vh_data,
      weather_data   = d$weather_data,
      vh_col         = "vh_col",
      predawn_window = c(2, 6),
      mode           = "static",
      vpd_threshold  = 0.5,
      vh_threshold   = 2.0,
      min_segment_days = msd
    )
  })

  counts <- sapply(runs, function(r) length(r$dual_stable_dates))
  spans  <- sapply(runs, function(r) {
    if (length(r$dual_stable_dates) < 2) return(0)
    as.numeric(diff(range(r$dual_stable_dates)))
  })

  deployment_days <- as.numeric(diff(range(as.Date(d$weather_data$datetime, tz = "UTC"))))

  # Count must decrease (or stay equal) as min_segment_days increases
  expect_true(all(diff(counts) <= 0),
    info = paste("Counts should be non-increasing:", paste(counts, collapse = " -> ")))

  # Span must remain >= 80% of deployment for all settings (no second-half collapse)
  for (i in seq_along(spans)) {
    if (counts[i] >= 2) {
      expect_gte(spans[i] / deployment_days, 0.80,
        label = sprintf("span at min_segment_days=%s", c(1, 7, 14, 30)[i]))
    }
  }
})


test_that("find_dual_stable_periods result matches manual single-filter application", {
  d <- make_dual_stable_data(n_days = 200, seed = 7)
  msd <- 14

  # Automatic (fixed) path
  result_auto <- find_dual_stable_periods(
    vh_data        = d$vh_data,
    weather_data   = d$weather_data,
    vh_col         = "vh_col",
    predawn_window = c(2, 6),
    mode           = "static",
    vpd_threshold  = 0.5,
    vh_threshold   = 2.0,
    min_segment_days = msd
  )

  # Manual path: bypass filter in sub-calls, intersect, apply once
  vpd_raw <- detect_stable_vpd_periods(
    weather_data     = d$weather_data,
    predawn_window   = c(2, 3, 4, 5),
    vpd_threshold    = 0.5,
    stability_threshold = 0.1,
    min_n_points     = 3,
    min_segment_days = msd,
    .apply_spacing_filter = FALSE
  )
  vh_raw <- find_stable_vh_dates(
    vh_data          = d$vh_data,
    vh_col           = "vh_col",
    predawn_window   = c(2, 6),
    vh_threshold     = 2.0,
    stability_threshold = 0.5,
    min_n_points     = 4,
    min_segment_days = msd,
    .apply_spacing_filter = FALSE
  )

  dual_raw <- intersect(vpd_raw$valid_dates, vh_raw$valid_dates)

  if (length(dual_raw) > 1 && msd > 0) {
    vh_vals <- vh_raw$daily_stats$mean_predawn_vh[match(dual_raw, vh_raw$daily_stats$date)]
    idx <- sapfluxr:::filter_stable_by_spacing(dual_raw, vh_vals, msd)
    manual_dates <- sort(dual_raw[idx])
  } else {
    manual_dates <- sort(dual_raw)
  }

  expect_equal(result_auto$dual_stable_dates, manual_dates)
})
