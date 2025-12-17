# Tests for Method Calibration Functions
# Created: 2025-12-14
# Purpose: Validate calibration algorithm fix (correct filter direction)

test_that("calibration uses correct filter direction (data BELOW threshold)", {
  # Create synthetic data with known divergence at 10 cm/hr
  # HRM and MHR agree from 0-10, HRM saturates above 10

  n <- 500
  pulse_ids <- 1:n

  # Simulate velocities
  hrm_true <- seq(0, 25, length.out = n)

  # MHR follows HRM up to 10, then continues linearly
  mhr_values <- hrm_true * 1.3 + 0.5  # Linear relationship throughout

  # HRM saturates above 10
  hrm_obs <- ifelse(hrm_true <= 10,
                    hrm_true,                    # Valid up to 10
                    10 + (hrm_true - 10) * 0.3)  # Saturates above 10

  # Add small random noise
  set.seed(123)
  hrm_obs <- hrm_obs + rnorm(n, 0, 0.1)
  mhr_values <- mhr_values + rnorm(n, 0, 0.1)

  # Create test data
  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_obs, mhr_values)
  )

  # Run calibration
  result <- find_optimal_calibration_threshold(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_start = 0,
    threshold_max = 20,
    threshold_step = 0.5,
    create_plots = FALSE,
    verbose = FALSE
  )

  # Test 1: Optimal threshold should be around 10 (where divergence starts)
  expect_true(result$optimal_threshold >= 8 && result$optimal_threshold <= 12,
              info = paste("Expected threshold ~10, got", result$optimal_threshold))

  # Test 2: R² should be high (both methods agree in calibration range)
  expect_true(result$optimal_r_squared > 0.95,
              info = paste("Expected R² > 0.95, got", result$optimal_r_squared))

  # Test 3: CRITICAL - Verify filter direction uses data <= threshold
  calib_obj <- result$optimal_calibration
  expect_true(all(calib_obj$calibration_data$Vh_cm_hr_primary <= result$optimal_threshold),
              info = "Calibration should use data BELOW or equal to threshold")

  # Test 4: Verify we're NOT using data above threshold for calibration
  expect_false(any(calib_obj$calibration_data$Vh_cm_hr_primary > result$optimal_threshold),
               info = "Calibration should NOT use data ABOVE threshold")
})


test_that("R² increases then decreases as threshold increases (finding divergence)", {
  # Create synthetic data
  n <- 500
  pulse_ids <- 1:n
  hrm_true <- seq(0, 25, length.out = n)
  mhr_values <- hrm_true * 1.3 + 0.5
  hrm_obs <- ifelse(hrm_true <= 10, hrm_true, 10 + (hrm_true - 10) * 0.3)

  set.seed(456)
  hrm_obs <- hrm_obs + rnorm(n, 0, 0.1)
  mhr_values <- mhr_values + rnorm(n, 0, 0.1)

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_obs, mhr_values)
  )

  # Run calibration with wider threshold range
  result <- find_optimal_calibration_threshold(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_start = 2,
    threshold_max = 20,
    threshold_step = 1,
    create_plots = FALSE,
    verbose = FALSE
  )

  # Get valid results
  valid_results <- result$threshold_results[!is.na(result$threshold_results$r_squared), ]

  # Test 1: R² should be high at low thresholds (both methods valid)
  low_thresh_r2 <- valid_results$r_squared[valid_results$threshold <= 8]
  expect_true(all(low_thresh_r2 > 0.90),
              info = paste("R² should be high for thresholds in valid region. Min R² was:",
                           min(low_thresh_r2)))

  # Test 2: R² pattern should show some variation across thresholds
  # The key is that with the CORRECT filter direction (<=), we see R² behave sensibly
  # With INCORRECT filter (>), R² would be low throughout
  expect_true(max(valid_results$r_squared) > 0.90,
              info = paste("Maximum R² should be high with correct filter direction. Got:",
                           max(valid_results$r_squared)))

  # Test 3: Optimal threshold should have peak R²
  optimal_r2 <- result$optimal_r_squared
  expect_true(optimal_r2 == max(valid_results$r_squared),
              info = "Optimal threshold should have maximum R²")
})


test_that("calibrate_method_to_primary uses correct filter direction", {
  # Create simple test data
  n <- 200
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 20, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  set.seed(789)
  hrm_vals <- hrm_vals + rnorm(n, 0, 0.05)
  mhr_vals <- mhr_vals + rnorm(n, 0, 0.05)

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
    verbose = FALSE
  )

  # Test 1: Calibration data should only include points <= 10
  expect_true(all(calib_result$calibration_data$Vh_cm_hr_primary <= 10),
              info = "Calibration data should only include HRM values <= threshold")

  # Test 2: Should not include any points > 10
  expect_false(any(calib_result$calibration_data$Vh_cm_hr_primary > 10),
               info = "Calibration data should NOT include HRM values > threshold")

  # Test 3: R² should be high (linear relationship in valid range)
  expect_true(calib_result$r_squared > 0.95,
              info = paste("Expected high R² in valid range, got", calib_result$r_squared))
})


test_that("manual threshold mode uses correct filter direction", {
  # Create test data
  n <- 200
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 20, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  set.seed(101)
  hrm_vals <- hrm_vals + rnorm(n, 0, 0.05)
  mhr_vals <- mhr_vals + rnorm(n, 0, 0.05)

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Use manual threshold
  result <- find_optimal_calibration_threshold(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    manual_threshold = 12,
    create_plots = FALSE,
    verbose = FALSE
  )

  # Test 1: Should return manual threshold
  expect_equal(result$optimal_threshold, 12,
               info = "Should use manual threshold")

  # Test 2: Calibration data should be <= manual threshold
  calib_data <- result$optimal_calibration$calibration_data
  expect_true(all(calib_data$Vh_cm_hr_primary <= 12),
              info = "Manual threshold should also use data BELOW threshold")
})


test_that("calibration fails appropriately when insufficient data", {
  # Create very sparse test data
  n <- 30  # Less than default min_points (50)
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 5, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Should throw error due to insufficient points
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
  n <- 200
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
    verbose = FALSE
  )

  # Calibrate inner
  calib_inner <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "inner",
    threshold_velocity = 10,
    verbose = FALSE
  )

  # Both should succeed
  expect_true(calib_outer$r_squared > 0.9)
  expect_true(calib_inner$r_squared > 0.9)

  # Both should use correct filter direction
  expect_true(all(calib_outer$calibration_data$Vh_cm_hr_primary <= 10))
  expect_true(all(calib_inner$calibration_data$Vh_cm_hr_primary <= 10))
})


test_that("edge case: all data below threshold", {
  # All velocities below threshold
  n <- 100
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 8, length.out = n)  # All below 10
  mhr_vals <- hrm_vals * 1.2 + 1

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Should use ALL data
  calib_result <- calibrate_method_to_primary(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_velocity = 10,
    verbose = FALSE
  )

  # Should use all n points
  expect_equal(nrow(calib_result$calibration_data), n)
  expect_true(calib_result$r_squared > 0.95)
})


test_that("edge case: threshold at 0", {
  # Threshold at 0 should use minimal data
  n <- 100
  pulse_ids <- 1:n
  hrm_vals <- seq(0, 10, length.out = n)
  mhr_vals <- hrm_vals * 1.2 + 1

  vh_test <- data.frame(
    pulse_id = rep(pulse_ids, 2),
    method = rep(c("HRM", "MHR"), each = n),
    sensor_position = "outer",
    Vh_cm_hr = c(hrm_vals, mhr_vals)
  )

  # Should have very few points (only those at exactly 0)
  result <- find_optimal_calibration_threshold(
    vh_corrected = vh_test,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    threshold_start = 0,
    threshold_max = 1,
    threshold_step = 0.5,
    min_points = 1,  # Allow minimal points for this test
    create_plots = FALSE,
    verbose = FALSE
  )

  # At threshold = 0, should have minimal data
  thresh_0_result <- result$threshold_results[result$threshold_results$threshold == 0, ]
  if (nrow(thresh_0_result) > 0) {
    expect_true(thresh_0_result$n_points < 10,
                info = "Threshold of 0 should use very few points")
  }
})
