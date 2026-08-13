# Regression tests for the Date class surviving the dual-stable intersection.
#
# Before R 4.5.0 the set operations applied as.vector() to their arguments,
# stripping the Date class from intersect(x, y) and returning bare numbers. The
# subsequent match() against daily_stats$date then compared "20028" with
# "2024-11-01", matched nothing, and every mean-vh lookup came back NA. That
# all-NA vector made filter_stable_by_spacing() loop forever; once that loop was
# made to terminate, it instead produced zero changepoints with no error at all.
#
# These tests run green on R >= 4.5.0 whether or not the bug is present, so the
# failing branch is exercised explicitly rather than relied upon.

make_overlapping_data <- function(n_days = 30, seed = 7) {
  set.seed(seed)
  start <- as.POSIXct("2024-11-01 00:00:00", tz = "UTC")
  datetimes <- seq(start, by = "30 min", length.out = n_days * 48)
  hours <- as.integer(format(datetimes, "%H"))

  weather_data <- data.frame(
    datetime = datetimes,
    vpd_kpa  = ifelse(hours %in% 2:5,
                      runif(length(datetimes), 0.05, 0.35),
                      runif(length(datetimes), 0.6, 2.0)),
    stringsAsFactors = FALSE
  )

  vh_data <- data.frame(
    datetime = datetimes,
    vh_col   = ifelse(hours %in% 2:5,
                      runif(length(datetimes), -0.30, 0.30),
                      runif(length(datetimes),  0.5,  2.5)),
    stringsAsFactors = FALSE
  )

  list(weather_data = weather_data, vh_data = vh_data)
}

run_detection <- function(d, min_segment_days = 7) {
  find_dual_stable_periods(
    vh_data          = d$vh_data,
    weather_data     = d$weather_data,
    vh_col           = "vh_col",
    predawn_window   = c(2, 6),
    mode             = "static",
    vpd_threshold    = 0.5,
    vh_threshold     = 2.0,
    min_segment_days = min_segment_days
  )
}

# The pre-R-4.5.0 definition, reproduced so the failing input can be built on
# any R version.
intersect_pre_450 <- function(x, y) {
  u <- as.vector(x)
  v <- as.vector(y)
  unique(v[match(u, v, 0L)])
}


test_that("dual_stable_dates keeps the Date class", {
  result <- run_detection(make_overlapping_data())

  skip_if(length(result$dual_stable_dates) == 0,
          "No dual-stable dates in synthetic data - check thresholds")

  expect_s3_class(result$dual_stable_dates, "Date")
})


test_that("every dual-stable date matches a row in the daily statistics", {
  result <- run_detection(make_overlapping_data())

  skip_if(length(result$dual_stable_dates) == 0,
          "No dual-stable dates in synthetic data - check thresholds")

  # This is the property that was silently false on R < 4.5.0: the dates existed
  # but could no longer be looked up, so the mean-vh vector was entirely NA.
  idx <- match(result$dual_stable_dates, result$vh_results$daily_stats$date)
  expect_false(anyNA(idx))
  expect_false(all(is.na(result$vh_results$daily_stats$mean_predawn_vh[idx])))
})


test_that("subsetting preserves the Date class where intersect() historically did not", {
  vpd_dates <- as.Date("2024-11-01") + c(0, 1, 3, 4, 6, 7, 8, 9)
  vh_dates  <- as.Date("2024-11-01") + 0:8

  # What the package does now.
  kept <- vpd_dates[vpd_dates %in% vh_dates]
  expect_s3_class(kept, "Date")
  expect_identical(kept, as.Date(c("2024-11-01", "2024-11-02", "2024-11-04",
                                   "2024-11-05", "2024-11-07", "2024-11-08",
                                   "2024-11-09")))

  # What R < 4.5.0 produced from the same inputs.
  stripped <- intersect_pre_450(vpd_dates, vh_dates)
  expect_false(inherits(stripped, "Date"))
  expect_type(stripped, "double")

  # And why that was fatal: the lookup silently finds nothing.
  daily_stats <- data.frame(date = vh_dates, mean_predawn_vh = seq_along(vh_dates) / 10)
  expect_true(all(is.na(match(stripped, daily_stats$date))))
  expect_false(anyNA(match(kept, daily_stats$date)))
})


test_that("an all-NA lookup makes the spacing filter select nothing", {
  # Documents why a class mismatch surfaced as "no stable periods found" rather
  # than as an error: the filter is handed NAs and correctly selects none.
  dates <- as.Date("2024-11-01") + 0:9
  expect_length(filter_stable_by_spacing(dates, rep(NA_real_, 10), 7), 0)

  # With real values the same call selects normally.
  expect_gt(length(filter_stable_by_spacing(dates, seq(0.1, 1.0, by = 0.1), 3)), 0)
})


test_that("unmatched dates raise an error instead of returning nothing", {
  d <- make_overlapping_data()

  # Corrupt the class exactly as the pre-4.5.0 intersect() did, by making the
  # daily statistics unmatchable against the dates derived from them. Bind the
  # real function first -- referring to it inside the replacement would resolve
  # back to the mock and recurse.
  real_find_stable_vh_dates <- find_stable_vh_dates
  local_mocked_bindings(
    find_stable_vh_dates = function(...) {
      out <- real_find_stable_vh_dates(...)
      out$daily_stats$date <- as.numeric(out$daily_stats$date)
      out
    }
  )

  expect_error(run_detection(d), "Could not match dual-stable dates")
})
