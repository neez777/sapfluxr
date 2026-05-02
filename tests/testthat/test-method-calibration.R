# tests/testthat/test-method-calibration.R
# Test method calibration and handover functionality

library(testthat)

# Helper to create test calibration data
create_test_calibration_data <- function(n = 100) {
  pulse_ids <- 1:n
  # HRM: linear up to 10, then plateaus
  hrm_vals <- pmin(seq(0, 15, length.out = n), 10)
  # MHR: linear relationship with HRM in the lower range
  mhr_vals <- hrm_vals * 1.2 + 1.0

  set.seed(123)
  hrm_vals <- hrm_vals + rnorm(n, 0, 0.05)
  mhr_vals <- mhr_vals + rnorm(n, 0, 0.05)

  data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals),
    stringsAsFactors = FALSE
  )
}

test_that("find_optimal_calibration_threshold() finds a valid threshold", {
  vh_data <- create_test_calibration_data()

  result <- find_optimal_calibration_threshold(
    vh_data,
    secondary_method = "MHR",
    threshold_start = 5,
    threshold_max = 12,
    threshold_step = 1,
    min_points = 5,
    create_plots = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "threshold_optimization_result")
  expect_true(!is.null(result$optimal_threshold))
  expect_true(result$optimal_threshold >= 5 && result$optimal_threshold <= 12)
  expect_true(result$optimal_r_squared > 0.9)
})

test_that("calibrate_method_to_primary() calculates coefficients correctly", {
  # Create data with known linear relationship: MHR = 1.2 * HRM + 1.0
  n <- 50
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 15, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1.0

  set.seed(789)
  hrm_vals <- hrm_vals + rnorm(n, 0, 0.01)
  mhr_vals <- mhr_vals + rnorm(n, 0, 0.01)

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Calibrate with threshold of 10
  calib_result <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_velocity = 10,
    min_points = 5,
    verbose = FALSE
  )

  # Check structure
  expect_s3_class(calib_result, "method_calibration")
  expect_true(all(c("coefficients", "fit_type", "r_squared") %in% names(calib_result)))

  # Check coefficients (should be close to 1.2 and 1.0)
  # Form: HRM = a * MHR + b  => HRM = (MHR - 1.0) / 1.2 = 0.833 * MHR - 0.833
  expect_equal(as.numeric(calib_result$coefficients["Vh_cm_hr_secondary"]), 0.833, tolerance = 0.05)
})

test_that("calibrate_method_to_primary() respects manual threshold", {
  vh_data <- create_test_calibration_data()

  result <- find_optimal_calibration_threshold(
    vh_data,
    secondary_method = "MHR",
    manual_threshold = 12,
    min_points = 5,
    create_plots = FALSE,
    verbose = FALSE
  )

  expect_equal(result$optimal_threshold, 12)

  # Calibration data should only include points below threshold
  calib_data <- result$optimal_calibration$calibration_data
  expect_true(all(calib_data$Vh_cm_hr_primary <= 12))
})

test_that("calibration fails appropriately when insufficient data", {
  # Create sparse test data
  n <- 10
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 5, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Should throw error due to insufficient points (default 50)
  expect_error(
    calibrate_method_to_primary(
      vh_corrected = vh_test,
      primary_method = "HRM",
      secondary_method = "MHR",
      sensor_position = "outer",
      threshold_velocity = 3,
      min_points = 50,
      verbose = FALSE
    ),
    regexp = "Insufficient data"
  )
})

test_that("calibration works with both sensor positions", {
  # Create test data for both sensor positions
  n <- 100
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 20, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  set.seed(202)
  hrm_outer <- hrm_vals + rnorm(n, 0, 0.05)
  mhr_outer <- mhr_vals + rnorm(n, 0, 0.05)
  hrm_inner <- hrm_vals + rnorm(n, 0, 0.05)
  mhr_inner <- mhr_vals + rnorm(n, 0, 0.05)

  vh_test <- data.frame(
    pulse_id = rep(rep(pulse_ids, 2), 2),
    method = rep(rep(c("HRM", "MHR"), each = n), 2),
    sensor_position = rep(c("outer", "inner"), each = n * 2),
    Vh_cm_hr = c(hrm_outer, mhr_outer, hrm_inner, mhr_inner)
  )

  # Calibrate outer
  calib_outer <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_velocity = 10,
    min_points = 5,
    verbose = FALSE
  )

  # Calibrate inner
  calib_inner <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "inner",
    threshold_velocity = 10,
    min_points = 5,
    verbose = FALSE
  )

  expect_true(calib_outer$r_squared > 0.9)
  expect_true(calib_inner$r_squared > 0.9)
})

test_that("edge case: all data below threshold", {
  n <- 100
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 8, length.out = n) # All below 10
  mhr_vals <- hrm_vals * 1.2 + 1

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  calib_result <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_velocity = 10,
    min_points = 5,
    verbose = FALSE
  )

  # Check if we got results (exact number depends on join/merge logic)
  expect_true(nrow(calib_result$calibration_data) > 0)
  expect_true(calib_result$r_squared > 0.95)
})
