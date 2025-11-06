# Tests for Data Filtering and Interpolation Functions

test_that("filter_and_interpolate_vh requires valid input", {
  # Missing required columns
  bad_data <- data.frame(x = 1:10)
  expect_error(
    filter_and_interpolate_vh(bad_data),
    "Missing required columns"
  )

  # Not a data frame
  expect_error(
    filter_and_interpolate_vh(list(a = 1)),
    "must be a data frame"
  )
})

test_that("filter_and_interpolate_vh handles simple linear interpolation", {
  # Create test data with a gap
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 10),
    pulse_id = 1:10,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, NA, 9, 10, 11, 12, 13, 14),
    quality_flag = c("OK", "OK", "DATA_MISSING", "DATA_MISSING", "OK", "OK", "OK", "OK", "OK", "OK")
  )

  result <- filter_and_interpolate_vh(
    test_data,
    interpolation_method = "linear",
    max_gap_hours = 2,
    verbose = FALSE
  )

  # Check that gap was filled
  expect_true(all(!is.na(result$Vh_cm_hr)))

  # Check that interpolated values are marked
  expect_true(any(result$is_interpolated))
  expect_equal(sum(result$is_interpolated), 2)

  # Check that interpolated flags are set
  expect_true(any(result$quality_flag == "INTERPOLATED"))

  # Linear interpolation: should be 7 and 8
  expect_equal(result$Vh_cm_hr[3], 7)
  expect_equal(result$Vh_cm_hr[4], 8)
})

test_that("filter_and_interpolate_vh preserves original values", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 5),
    pulse_id = 1:5,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, NA, 7, 8, 9),
    quality_flag = c("OK", "DATA_MISSING", "OK", "OK", "OK")
  )

  result <- filter_and_interpolate_vh(
    test_data,
    keep_original_values = TRUE,
    verbose = FALSE
  )

  # Check that original values are preserved
  expect_true("Vh_original" %in% names(result))
  expect_equal(result$Vh_original, test_data$Vh_cm_hr)

  # Check that original flags are preserved
  expect_true("quality_flag_original" %in% names(result))
  expect_equal(result$quality_flag_original, test_data$quality_flag)
})

test_that("filter_and_interpolate_vh respects max_gap_hours threshold", {
  # Create data with large gap (3 hours)
  test_data <- data.frame(
    datetime = c(
      as.POSIXct("2023-01-01 10:00:00"),
      as.POSIXct("2023-01-01 10:30:00"),
      as.POSIXct("2023-01-01 11:00:00"),
      as.POSIXct("2023-01-01 11:30:00"),
      as.POSIXct("2023-01-01 12:00:00"),
      as.POSIXct("2023-01-01 15:00:00")  # 3-hour gap
    ),
    pulse_id = 1:6,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, NA, NA, 10),
    quality_flag = c("OK", "OK", "DATA_MISSING", "DATA_MISSING", "DATA_MISSING", "OK")
  )

  # With max_gap = 1 hour, should not fill
  result <- filter_and_interpolate_vh(
    test_data,
    max_gap_hours = 1,
    verbose = FALSE
  )

  # Gap should NOT be filled
  expect_true(any(is.na(result$Vh_cm_hr)))
  expect_true(any(result$quality_flag == "LARGE_GAP"))

  # With max_gap = 5 hours, should fill
  result2 <- suppressWarnings(filter_and_interpolate_vh(
    test_data,
    max_gap_hours = 5,
    verbose = FALSE
  ))

  # Gap SHOULD be filled
  expect_true(all(!is.na(result2$Vh_cm_hr)))
})

test_that("filter_and_interpolate_vh handles multiple interpolation methods", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 7),
    pulse_id = 1:7,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, 8, 9, 10, 11),
    quality_flag = c("OK", "OK", "DATA_MISSING", "OK", "OK", "OK", "OK")
  )

  # Test linear
  result_linear <- filter_and_interpolate_vh(
    test_data,
    interpolation_method = "linear",
    verbose = FALSE
  )
  expect_true(all(!is.na(result_linear$Vh_cm_hr)))

  # Test spline
  result_spline <- filter_and_interpolate_vh(
    test_data,
    interpolation_method = "spline",
    verbose = FALSE
  )
  expect_true(all(!is.na(result_spline$Vh_cm_hr)))

  # Test approx
  result_approx <- filter_and_interpolate_vh(
    test_data,
    interpolation_method = "approx",
    verbose = FALSE
  )
  expect_true(all(!is.na(result_approx$Vh_cm_hr)))
})

test_that("filter_and_interpolate_vh handles multiple flags", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 10),
    pulse_id = 1:10,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, 600, 9, -5, 11, 12, NA, 14),
    quality_flag = c("OK", "OK", "DATA_MISSING", "DATA_ILLOGICAL", "OK",
                     "DATA_SUSPECT", "OK", "OK", "DATA_OUTLIER", "OK")
  )

  result <- filter_and_interpolate_vh(
    test_data,
    flags_to_interpolate = c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL"),
    flags_to_preserve = c("DATA_SUSPECT"),
    verbose = FALSE
  )

  # DATA_MISSING and DATA_OUTLIER and DATA_ILLOGICAL should be interpolated
  expect_equal(result$quality_flag[3], "INTERPOLATED")
  expect_equal(result$quality_flag[4], "INTERPOLATED")
  expect_equal(result$quality_flag[9], "INTERPOLATED")

  # DATA_SUSPECT should be preserved
  expect_equal(result$quality_flag[6], "DATA_SUSPECT")
  expect_equal(result$Vh_cm_hr[6], -5)  # Original value preserved
})

test_that("filter_and_interpolate_vh groups by method and sensor", {
  test_data <- data.frame(
    datetime = rep(seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 5), 4),
    pulse_id = rep(1:5, 4),
    method = rep(c("HRM", "HRM", "MHR", "MHR"), each = 5),
    sensor_position = rep(c("outer", "inner", "outer", "inner"), each = 5),
    Vh_cm_hr = c(
      c(5, NA, 7, 8, 9),      # HRM outer
      c(10, NA, 12, 13, 14),  # HRM inner
      c(15, NA, 17, 18, 19),  # MHR outer
      c(20, NA, 22, 23, 24)   # MHR inner
    ),
    quality_flag = rep(c("OK", "DATA_MISSING", "OK", "OK", "OK"), 4)
  )

  result <- filter_and_interpolate_vh(
    test_data,
    group_by_method = TRUE,
    group_by_sensor = TRUE,
    verbose = FALSE
  )

  # Each group should be interpolated independently
  # HRM outer: should interpolate to (5+7)/2 = 6
  expect_equal(result$Vh_cm_hr[result$method == "HRM" & result$sensor_position == "outer"][2], 6)

  # HRM inner: should interpolate to (10+12)/2 = 11
  expect_equal(result$Vh_cm_hr[result$method == "HRM" & result$sensor_position == "inner"][2], 11)

  # MHR outer: should interpolate to (15+17)/2 = 16
  expect_equal(result$Vh_cm_hr[result$method == "MHR" & result$sensor_position == "outer"][2], 16)

  # MHR inner: should interpolate to (20+22)/2 = 21
  expect_equal(result$Vh_cm_hr[result$method == "MHR" & result$sensor_position == "inner"][2], 21)
})

test_that("preview_interpolation_changes returns correct structure", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 10),
    pulse_id = 1:10,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, NA, 9, 10, 11, 12, 13, 14),
    quality_flag = c("OK", "OK", "DATA_MISSING", "DATA_MISSING", "OK", "OK", "OK", "OK", "OK", "OK")
  )

  preview <- preview_interpolation_changes(
    test_data,
    max_gap_hours = 2
  )

  # Check structure
  expect_s3_class(preview, "interpolation_preview")
  expect_true("summary_table" %in% names(preview))
  expect_true("gap_report" %in% names(preview))
  expect_true("affected_indices" %in% names(preview))
  expect_true("n_total" %in% names(preview))
  expect_true("n_interpolated" %in% names(preview))

  # Check counts
  expect_equal(preview$n_total, 10)
  expect_equal(preview$n_interpolated, 2)

  # Check gap report
  expect_true(is.data.frame(preview$gap_report))
  expect_true(nrow(preview$gap_report) > 0)
})

test_that("preview_interpolation_changes identifies large gaps", {
  test_data <- data.frame(
    datetime = c(
      seq(as.POSIXct("2023-01-01 10:00:00"), by = "30 min", length.out = 3),
      as.POSIXct("2023-01-01 15:00:00")  # Large gap
    ),
    pulse_id = 1:4,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = c(5, 6, NA, 10),
    quality_flag = c("OK", "OK", "DATA_MISSING", "OK")
  )

  preview <- preview_interpolation_changes(
    test_data,
    max_gap_hours = 1
  )

  # Should detect large gap
  expect_true(preview$n_large_gaps > 0)
  expect_true(any(preview$gap_report$action == "TOO_LARGE"))
})
