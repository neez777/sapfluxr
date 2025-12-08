# Tests for VPD-based changepoint detection functions

test_that("calculate_daily_vpd_minima calculates correctly", {
  # Create test weather data - exactly 3 complete days
  day1_vpd <- c(0.2, 0.3, 0.5, 0.8, 1.2, 1.5, 1.8, 2.0,
                2.0, 1.8, 1.2, 0.8, 0.5, 0.3, 0.25, 0.2,
                0.18, 0.15, 0.12, 0.1, 0.12, 0.15, 0.18, 0.2)

  day2_vpd <- c(0.15, 0.2, 0.4, 0.7, 1.0, 1.3, 1.6, 1.8,
                1.8, 1.6, 1.0, 0.7, 0.4, 0.25, 0.2, 0.15,
                0.13, 0.11, 0.1, 0.09, 0.1, 0.12, 0.14, 0.15)

  day3_vpd <- c(0.25, 0.35, 0.55, 0.9, 1.3, 1.6, 1.9, 2.1,
                2.1, 1.9, 1.3, 0.9, 0.55, 0.35, 0.3, 0.25,
                0.23, 0.2, 0.18, 0.16, 0.18, 0.2, 0.22, 0.25)

  test_weather <- data.frame(
    datetime = seq(
      as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
      by = "hour",
      length.out = 72
    ),
    vpd_kpa = c(day1_vpd, day2_vpd, day3_vpd)
  )

  daily_vpd <- calculate_daily_vpd_minima(test_weather)

  expect_s3_class(daily_vpd, "data.frame")
  expect_equal(nrow(daily_vpd), 3)
  expect_true(all(c("date", "min_vpd", "mean_vpd", "max_vpd", "sd_vpd", "n_obs") %in% names(daily_vpd)))

  # Check min values
  expect_equal(daily_vpd$min_vpd[1], 0.1, tolerance = 1e-10)
  expect_equal(daily_vpd$min_vpd[2], 0.09, tolerance = 1e-10)
  expect_equal(daily_vpd$min_vpd[3], 0.16, tolerance = 1e-10)

  # Check n_obs
  expect_equal(daily_vpd$n_obs, c(24, 24, 24))
})


test_that("calculate_daily_vpd_minima handles missing values", {
  # Create 2 complete days with some NAs
  day1_with_na <- c(rep(0.5, 10), NA, NA, rep(0.3, 12))
  day2_with_na <- c(rep(0.4, 10), NA, NA, rep(0.2, 12))

  test_weather <- data.frame(
    datetime = seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = "hour", length.out = 48),
    vpd_kpa = c(day1_with_na, day2_with_na)
  )

  daily_vpd <- calculate_daily_vpd_minima(test_weather)

  expect_equal(nrow(daily_vpd), 2)
  expect_equal(daily_vpd$n_obs, c(22, 22))  # Excludes NAs
})


test_that("calculate_daily_vpd_minima errors on invalid input", {
  expect_error(
    calculate_daily_vpd_minima("not a data frame"),
    "must be a data frame"
  )

  expect_error(
    calculate_daily_vpd_minima(data.frame(x = 1:10)),
    "Missing required columns"
  )
})


test_that("detect_vpd_changepoints detects with default threshold", {
  # Create daily VPD data with some days below 0.5 kPa
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
    min_vpd = c(
      0.8, 0.7, 0.6, 0.4, 0.3,  # Days 4-5 below 0.5
      0.9, 1.0, 0.8, 0.7, 0.6,
      0.45, 0.35, 0.40,          # Days 11-13 below 0.5
      0.8, 0.9, 1.1, 1.2, 1.0,
      0.25, 0.30,                 # Days 19-20 below 0.5
      0.7, 0.8, 0.9, 0.6, 0.5,
      0.42, 0.38, 0.45,          # Days 26-28 below 0.5
      0.7, 0.8
    )
  )

  result <- detect_vpd_changepoints(daily_vpd)

  expect_s3_class(result, "vpd_changepoints")
  expect_true("changepoints" %in% names(result))
  expect_true("changepoint_indices" %in% names(result))
  expect_true("vpd_values" %in% names(result))
  expect_true("segments" %in% names(result))
  expect_true("parameters" %in% names(result))

  expect_true(length(result$changepoints) > 0)
  expect_equal(length(result$changepoints), length(result$changepoint_indices))
  expect_equal(length(result$changepoints), length(result$vpd_values))

  # All selected VPD values should be <= threshold
  expect_true(all(result$vpd_values <= 0.5))
})


test_that("detect_vpd_changepoints respects vpd_threshold", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    min_vpd = c(0.2, 0.5, 0.8, 0.3, 0.6, 0.4, 0.7, 0.25, 0.9, 0.35)
  )

  # Low threshold (0.3)
  result_low <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.3)
  expect_true(all(result_low$vpd_values <= 0.3))

  # High threshold (0.8)
  result_high <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.8)
  expect_true(all(result_high$vpd_values <= 0.8))
  expect_true(length(result_high$changepoints) >= length(result_low$changepoints))
})


test_that("detect_vpd_changepoints applies min_segment_days", {
  # Create data with many consecutive low VPD days
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 20),
    min_vpd = c(0.3, 0.25, 0.28, 0.32, 0.27,  # Days 1-5 all below 0.5
                0.8, 0.9, 1.0,                  # Days 6-8 above
                0.35, 0.30, 0.33,               # Days 9-11 below
                0.7, 0.8,                       # Days 12-13 above
                0.29, 0.26,                     # Days 14-15 below
                0.9, 1.0, 0.8, 0.7, 0.6)
  )

  # With min_segment_days = 5, should space out selections
  result <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.5, min_segment_days = 5)

  if (length(result$changepoints) > 1) {
    # Check spacing between consecutive changepoints
    date_diffs <- diff(as.numeric(result$changepoints))
    expect_true(all(date_diffs >= 5))
  }
})


test_that("detect_vpd_changepoints handles require_consecutive", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 15),
    min_vpd = c(
      0.3, 0.28, 0.32,  # 3 consecutive below 0.5
      0.8, 0.9,          # Gap
      0.25,              # 1 day below (should be excluded)
      0.7,               # Gap
      0.35, 0.30, 0.33, 0.29,  # 4 consecutive below 0.5
      0.8, 0.9, 1.0, 0.7
    )
  )

  # Without consecutive requirement
  result_no_consec <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    require_consecutive = FALSE,
    min_segment_days = 1
  )

  # With consecutive requirement (need 3 days)
  result_consec <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    require_consecutive = TRUE,
    min_consecutive_days = 3,
    min_segment_days = 1
  )

  # Consecutive requirement should select fewer or equal changepoints
  expect_true(length(result_consec$changepoints) <= length(result_no_consec$changepoints))
})


test_that("detect_vpd_changepoints limits max_changepoints", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 50),
    min_vpd = runif(50, 0.1, 0.4)  # All below 0.5
  )

  result <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    max_changepoints = 5,
    min_segment_days = 1
  )

  expect_equal(length(result$changepoints), 5)

  # Should select the 5 days with lowest VPD
  expected_lowest <- sort(daily_vpd$min_vpd)[1:5]
  actual_vpd <- sort(result$vpd_values)
  expect_equal(actual_vpd, expected_lowest, tolerance = 1e-10)
})


test_that("detect_vpd_changepoints warns when no days below threshold", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    min_vpd = rep(1.5, 10)  # All above any reasonable threshold
  )

  expect_warning(
    result <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.5),
    "No days found with VPD"
  )

  expect_equal(length(result$changepoints), 0)
  expect_equal(result$n_changepoints_selected, 0)
})


test_that("detect_vpd_changepoints creates correct segments", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
    min_vpd = c(rep(0.8, 5), 0.3, rep(0.8, 10), 0.25, rep(0.9, 13))
  )

  result <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    min_segment_days = 5
  )

  # Should have 3 segments (before 1st cpt, between cpts, after 2nd cpt)
  expect_equal(nrow(result$segments), length(result$changepoints) + 1)

  # Segments should cover entire date range
  expect_equal(result$segments$start_date[1], daily_vpd$date[1])
  expect_equal(result$segments$end_date[nrow(result$segments)], daily_vpd$date[nrow(daily_vpd)])

  # Segments should be contiguous
  if (nrow(result$segments) > 1) {
    for (i in 1:(nrow(result$segments) - 1)) {
      expect_equal(
        result$segments$end_date[i] + 1,
        result$segments$start_date[i + 1]
      )
    }
  }
})


test_that("detect_vpd_changepoints assigns segment IDs correctly", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 20),
    min_vpd = c(rep(0.8, 5), 0.3, rep(0.8, 8), 0.25, rep(0.9, 5))
  )

  result <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.5, min_segment_days = 3)

  # Check daily_vpd_with_segments
  expect_true("segment_id" %in% names(result$daily_vpd_with_segments))
  expect_equal(nrow(result$daily_vpd_with_segments), nrow(daily_vpd))

  # All segment IDs should be valid
  expect_true(all(result$daily_vpd_with_segments$segment_id >= 1))
  expect_true(all(result$daily_vpd_with_segments$segment_id <= nrow(result$segments)))
})


test_that("find_consecutive_sequences works correctly", {
  # Test with consecutive indices
  result1 <- sapfluxr:::find_consecutive_sequences(c(1, 2, 3, 4, 5), min_length = 3)
  expect_equal(length(result1), 1)
  expect_equal(result1[[1]], c(1, 2, 3, 4, 5))

  # Test with breaks
  result2 <- sapfluxr:::find_consecutive_sequences(c(1, 2, 3, 7, 8, 9, 10), min_length = 3)
  expect_equal(length(result2), 2)
  expect_equal(result2[[1]], c(1, 2, 3))
  expect_equal(result2[[2]], c(7, 8, 9, 10))

  # Test with min_length filter
  result3 <- sapfluxr:::find_consecutive_sequences(c(1, 2, 5, 6, 7, 10), min_length = 3)
  expect_equal(length(result3), 1)
  expect_equal(result3[[1]], c(5, 6, 7))

  # Test with empty input
  result4 <- sapfluxr:::find_consecutive_sequences(integer(0), min_length = 1)
  expect_equal(length(result4), 0)
})


test_that("filter_by_minimum_spacing works correctly", {
  dates <- as.Date("2024-01-01") + 0:20
  vpd_values <- c(0.5, 0.3, 0.4, 0.2, 0.45, 0.35, 0.25, 0.5, 0.4, 0.3,
                  0.2, 0.45, 0.35, 0.25, 0.5, 0.4, 0.3, 0.2, 0.45, 0.35, 0.25)
  indices <- 1:21

  # Filter with min spacing of 5 days
  result <- sapfluxr:::filter_by_minimum_spacing(
    indices,
    dates,
    vpd_values,
    min_spacing_days = 5
  )

  # Check spacing
  if (length(result) > 1) {
    date_diffs <- diff(as.numeric(dates[result]))
    expect_true(all(date_diffs >= 5))
  }

  # Check that selected indices have lowest VPD in their windows
  expect_true(4 %in% result)  # Index 4 has VPD = 0.2 (lowest)
  expect_true(11 %in% result || 18 %in% result)  # Other 0.2 values
})


test_that("print.vpd_changepoints displays correctly", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 20),
    min_vpd = c(rep(0.8, 5), 0.3, rep(0.8, 8), 0.25, rep(0.9, 5))
  )

  result <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.5)

  # Capture print output
  output <- capture.output(print(result))

  expect_true(any(grepl("VPD-Based Changepoints", output)))
  expect_true(any(grepl("VPD threshold", output)))
  expect_true(any(grepl("Changepoints selected", output)))
  expect_true(any(grepl("Segments", output)))
})


test_that("detect_vpd_changepoints handles edge case: single day below threshold", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    min_vpd = c(0.8, 0.9, 1.0, 0.3, 0.8, 0.9, 1.0, 0.8, 0.9, 0.8)
  )

  result <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    min_segment_days = 1
  )

  expect_equal(length(result$changepoints), 1)
  expect_equal(result$changepoints[1], as.Date("2024-01-04"))
})


test_that("detect_vpd_changepoints handles all days below threshold", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    min_vpd = rep(0.3, 10)
  )

  result <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    min_segment_days = 3
  )

  # Should select multiple changepoints spaced appropriately
  expect_true(length(result$changepoints) >= 2)
})


test_that("detect_vpd_changepoints errors on invalid parameters", {
  daily_vpd <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
    min_vpd = runif(10, 0.2, 0.8)
  )

  expect_error(
    detect_vpd_changepoints("not a data frame"),
    "must be a data frame"
  )

  expect_error(
    detect_vpd_changepoints(data.frame(x = 1:10)),
    "Missing required columns"
  )

  expect_error(
    detect_vpd_changepoints(daily_vpd, vpd_threshold = -0.5),
    "must be positive"
  )

  expect_error(
    detect_vpd_changepoints(daily_vpd, min_segment_days = 0),
    "must be at least 1"
  )

  expect_error(
    detect_vpd_changepoints(daily_vpd, require_consecutive = TRUE, min_consecutive_days = 1),
    "must be at least 2"
  )
})


test_that("VPD changepoints integrate with existing workflow", {
  # Create synthetic sap flow and weather data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 30)

  # Weather data
  weather_hourly <- data.frame(
    datetime = rep(dates, each = 24) + rep(0:23, times = 30) * 3600,
    air_temp_c = 20 + rnorm(30 * 24, 0, 5),
    relative_humidity = pmin(100, pmax(30, 60 + rnorm(30 * 24, 0, 15)))
  )

  weather_vpd <- calc_vpd(weather_hourly)
  daily_vpd <- calculate_daily_vpd_minima(weather_vpd)

  # Detect VPD changepoints
  vpd_cpts <- detect_vpd_changepoints(
    daily_vpd,
    vpd_threshold = 0.5,
    min_segment_days = 7
  )

  # Should have structure compatible with spacing correction workflow
  expect_true("changepoints" %in% names(vpd_cpts))
  expect_true("segments" %in% names(vpd_cpts))
  expect_s3_class(vpd_cpts$changepoints, "Date")

  # Segments should have required structure
  expect_true(all(c("segment_id", "start_date", "end_date", "n_days") %in% names(vpd_cpts$segments)))
})
