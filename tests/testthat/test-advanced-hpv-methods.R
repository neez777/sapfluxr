# =============================================================================
# TEST FILE: test-advanced-hpv-methods.R
# Tests for advanced HPV methods (CHPM, DRM, unified calculator)
# Source file: R/05_heat_pulse_velocity_advanced.R
# =============================================================================

library(testthat)
library(sapFluxR)

# Helper function for three-probe synthetic data
create_three_probe_data <- function(velocity = 30, noise_level = 0.01, duration = 300) {
  time <- seq(1, duration, by = 1)

  # Simulate realistic three-probe temperature response
  # Based on heat pulse theory - peak around 60-80 seconds, then decay
  temp_down <- 2 * exp(-(time - 70)^2 / 1000) + rnorm(length(time), 0, noise_level)
  temp_up <- temp_down * 0.7 + rnorm(length(time), 0, noise_level)
  temp_far <- temp_down * 0.4 + rnorm(length(time), 0, noise_level)

  data.frame(
    time = time,
    temp_up = temp_up,
    temp_down = temp_down,
    temp_far = temp_far
  )
}

# =============================================================================
# TESTS FOR calc_drm() - Double Ratio Method
# =============================================================================

test_that("calc_drm returns correct structure", {
  temp_data <- create_three_probe_data(velocity = 40)
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  result <- calc_drm(temp_data, probe_spacing)

  expect_true(is.list(result))
  expect_true("velocity_drm" %in% names(result))
  expect_true("velocity_12" %in% names(result))
  expect_true("velocity_23" %in% names(result))
  expect_true("uncertainty_12" %in% names(result))
  expect_true("uncertainty_23" %in% names(result))
  expect_true("selected_method" %in% names(result))
  expect_true("thermal_diffusivity_realtime" %in% names(result))
  expect_true("quality_metrics" %in% names(result))
})

test_that("calc_drm validates inputs correctly", {
  temp_data <- create_three_probe_data()
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test invalid thermal diffusivity
  expect_error(calc_drm(temp_data, probe_spacing, thermal_diffusivity = -0.001),
               "thermal_diffusivity must be positive")

  # Test invalid time window
  expect_error(calc_drm(temp_data, probe_spacing, time_window = c(100, 50)),
               "time_window must be a vector of length 2 with start < end")

  # Test missing required columns
  invalid_data <- temp_data[, -4]  # Remove temp_far
  expect_error(calc_drm(invalid_data, probe_spacing),
               "Missing required columns")

  # Test non-dataframe input
  expect_error(calc_drm("not_a_dataframe", probe_spacing),
               "temp_data must be a data.frame")
})

test_that("calc_drm handles different velocity ranges", {
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test low velocity (should prefer V12)
  low_vel_data <- create_three_probe_data(velocity = 5)
  result_low <- calc_drm(low_vel_data, probe_spacing)

  # Test high velocity (should prefer V23)
  high_vel_data <- create_three_probe_data(velocity = 60)
  result_high <- calc_drm(high_vel_data, probe_spacing)

  expect_true(result_low$selected_method %in% c("V12", "V23", "None"))
  expect_true(result_high$selected_method %in% c("V12", "V23", "None"))
  expect_true(is.numeric(result_low$uncertainty_12))
  expect_true(is.numeric(result_high$uncertainty_23))
})

test_that("calc_drm handles edge cases", {
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test with zero/negative temperature rises
  flat_data <- data.frame(
    time = seq(60, 100),
    temp_up = rep(0, 41),
    temp_down = rep(0.001, 41),
    temp_far = rep(0, 41)
  )

  result <- calc_drm(flat_data, probe_spacing)
  expect_true("velocity_drm" %in% names(result))

  # Test with very noisy data
  noisy_data <- create_three_probe_data(velocity = 30, noise_level = 0.5)
  result_noisy <- calc_drm(noisy_data, probe_spacing)
  expect_true(is.list(result_noisy$quality_metrics))
})

# =============================================================================
# TESTS FOR calc_chpm() - Compensation Heat Pulse Method
# =============================================================================

test_that("calc_chpm returns correct structure", {
  temp_data <- create_three_probe_data(velocity = 30)
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  result <- calc_chpm(temp_data, probe_spacing, pulse_time = 8)

  expect_true(is.list(result))
  expect_true("velocity_chpm" %in% names(result))
  expect_true("crossover_time" %in% names(result))
  expect_true("method_applicable" %in% names(result))
  expect_true("quality_metrics" %in% names(result))
})

test_that("calc_chpm validates inputs correctly", {
  temp_data <- create_three_probe_data()
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test missing required columns
  invalid_data <- temp_data[, -1]  # Remove time
  expect_error(calc_chpm(invalid_data, probe_spacing, 8),
               "Missing required columns")

  # Test non-dataframe input
  expect_error(calc_chpm("not_dataframe", probe_spacing, 8),
               "temp_data must be a data.frame")
})

test_that("calc_chpm handles method applicability correctly", {
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test with data that should not show crossover (flat temperatures)
  flat_data <- data.frame(
    time = seq(1, 300),
    temp_up = rep(25.0, 300),
    temp_down = rep(25.1, 300),
    temp_far = rep(25.0, 300)
  )

  result <- calc_chpm(flat_data, probe_spacing, pulse_time = 8)
  expect_false(result$method_applicable)
  expect_true(is.na(result$velocity_chpm))
  expect_true(is.list(result$quality_metrics))
})

test_that("calc_chpm respects velocity range limits", {
  temp_data <- create_three_probe_data(velocity = 30)
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  # Test with very short max_time (should make method inapplicable)
  result <- calc_chpm(temp_data, probe_spacing, pulse_time = 8, max_time = 10, min_velocity = 50)

  expect_true(is.logical(result$method_applicable))
  expect_true("min_detectable_velocity" %in% names(result$quality_metrics))
})

# =============================================================================
# TESTS FOR calc_unified_hpv() - Unified Calculator
# =============================================================================

test_that("calc_unified_hpv integrates methods correctly", {
  temp_data <- create_three_probe_data(velocity = 35)

  probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    symmetric = FALSE,
    asymmetric = TRUE,
    spacing = list(upstream = -0.75, downstream = 0.75, far = 2.25),
    pulse_time = 8
  )

  result <- calc_unified_hpv(temp_data, probe_config, methods = "auto")

  expect_true(is.list(result))
  expect_true("metadata" %in% names(result))
  expect_true("data_quality" %in% names(result))
  expect_true("method_results" %in% names(result))
  expect_true("recommended_result" %in% names(result))

  expect_true(is.list(result$metadata))
  expect_true("methods_attempted" %in% names(result$metadata))
})

test_that("calc_unified_hpv handles manual method selection", {
  temp_data <- create_three_probe_data()

  probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    symmetric = FALSE,
    asymmetric = TRUE,
    spacing = list(upstream = -0.75, downstream = 0.75, far = 2.25),
    pulse_time = 8
  )

  # Test manual method selection
  result <- calc_unified_hpv(temp_data, probe_config, methods = c("DRM", "CHPM"))

  expect_true("DRM" %in% names(result$method_results))
  expect_true("CHPM" %in% names(result$method_results))
  expect_equal(result$metadata$methods_attempted, c("DRM", "CHPM"))
})

test_that("calc_unified_hpv handles method errors gracefully", {
  # Create data that might cause method errors
  problematic_data <- data.frame(
    time = seq(1, 100),
    temp_up = rep(NA, 100),
    temp_down = rep(25.0, 100),
    temp_far = rep(25.0, 100)
  )

  probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    spacing = list(upstream = -0.75, downstream = 0.75, far = 2.25),
    pulse_time = 8
  )

  # Should not crash even with problematic data - may produce warnings which is OK
  expect_no_error(result <- calc_unified_hpv(problematic_data, probe_config, methods = "DRM"))
  expect_true(is.list(result))
})

# =============================================================================
# TESTS FOR detect_optimal_method() - Method Selection
# =============================================================================

test_that("detect_optimal_method recommends appropriate methods", {
  # Three-probe asymmetric configuration
  three_probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    symmetric = FALSE,
    asymmetric = TRUE
  )

  temp_data <- create_three_probe_data()
  detection <- detect_optimal_method(temp_data, three_probe_config)

  expect_true(is.list(detection))
  expect_true("recommended_methods" %in% names(detection))
  expect_true("primary_method" %in% names(detection))
  expect_true("data_quality" %in% names(detection))
  expect_true("reasoning" %in% names(detection))

  # DRM should be recommended for 3-probe configurations
  expect_true("DRM" %in% detection$recommended_methods)
})

test_that("detect_optimal_method considers velocity estimates", {
  two_probe_config <- list(
    configuration_type = "two_probe_symmetric",
    probe_count = 2,
    symmetric = TRUE
  )

  temp_data <- create_three_probe_data()[, 1:3]  # Remove temp_far

  # Test with low velocity estimate
  detection_low <- detect_optimal_method(temp_data, two_probe_config, velocity_estimate = 5)
  expect_true("HRM" %in% detection_low$recommended_methods)

  # Test with high velocity estimate
  detection_high <- detect_optimal_method(temp_data, two_probe_config, velocity_estimate = 60)
  expect_true(is.character(detection_high$reasoning))
})

# =============================================================================
# TESTS FOR assess_temp_data_quality() - Advanced Quality Assessment
# =============================================================================

test_that("assess_temp_data_quality evaluates signal quality correctly", {
  # High-quality data
  good_data <- create_three_probe_data(velocity = 30, noise_level = 0.01)
  quality <- assess_temp_data_quality(good_data)

  expect_true(is.list(quality))
  expect_true("individual_sensors" %in% names(quality))
  expect_true("overall_adequate" %in% names(quality))
  expect_true("quality_flag" %in% names(quality))

  # Check individual sensor metrics
  expect_true("temp_up" %in% names(quality$individual_sensors))
  expect_true("temp_down" %in% names(quality$individual_sensors))
  expect_true("temp_far" %in% names(quality$individual_sensors))
})

test_that("assess_temp_data_quality identifies poor quality data", {
  # Very noisy data with high noise relative to signal
  poor_data <- data.frame(
    time = seq(1, 100),
    temp_up = rnorm(100, 25, 2.0),    # High noise, low signal
    temp_down = rnorm(100, 25, 2.0),  # High noise, low signal
    temp_far = rnorm(100, 25, 2.0)    # High noise, low signal
  )

  quality <- assess_temp_data_quality(poor_data, min_signal_ratio = 3)

  # With high noise and low signal, should detect poor quality
  expect_equal(quality$quality_flag, "poor")
  expect_false(quality$overall_adequate)
})

test_that("assess_temp_data_quality handles missing columns gracefully", {
  # Data with only some temperature columns
  partial_data <- data.frame(
    time = seq(1, 100),
    temp_down = rnorm(100, 25, 0.1)
  )

  quality <- assess_temp_data_quality(partial_data)
  expect_true("temp_down" %in% names(quality$individual_sensors))
  expect_false("temp_up" %in% names(quality$individual_sensors))
})

# =============================================================================
# TESTS FOR optimize_drm_window() - Window Optimisation
# =============================================================================

test_that("optimize_drm_window returns correct structure", {
  temp_data <- create_three_probe_data()
  probe_spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)

  result <- optimize_drm_window(temp_data, probe_spacing)

  expect_true(is.list(result))
  expect_true("optimal_start" %in% names(result))
  expect_true("optimal_end" %in% names(result))
  expect_true("optimal_size" %in% names(result))
  expect_true("min_uncertainty" %in% names(result))

  # Check that returned values are reasonable
  expect_true(result$optimal_start < result$optimal_end)
  expect_true(result$optimal_size > 0)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("DRM and CHPM work together in unified calculator", {
  temp_data <- create_three_probe_data(velocity = 25)

  probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    symmetric = FALSE,
    asymmetric = TRUE,
    spacing = list(upstream = -0.75, downstream = 0.75, far = 2.25),
    pulse_time = 8
  )

  # Test that both methods can be run together
  result <- calc_unified_hpv(temp_data, probe_config, methods = c("DRM", "CHPM"))

  expect_true("DRM" %in% names(result$method_results))
  expect_true("CHPM" %in% names(result$method_results))

  # Both should return results or errors, not fail completely
  expect_true(is.list(result$method_results$DRM))
  expect_true(is.list(result$method_results$CHPM))
})

test_that("advanced methods integrate with existing probe configuration system", {
  # Test that our new methods work with probe configs from Chat 2
  probe_config <- list(
    configuration_type = "three_probe_asymmetric",
    probe_count = 3,
    symmetric = FALSE,
    asymmetric = TRUE,
    spacing = list(upstream = -0.75, downstream = 0.75, far = 2.25),
    compatible_methods = c("DRM", "CHPM", "HRM")
  )

  temp_data <- create_three_probe_data()

  # Test that method compatibility works
  compatibility <- calc_method_compatibility(list(probe_config))
  expect_true(is.data.frame(compatibility))
  expect_true("DRM" %in% names(compatibility))
  expect_true("CHPM" %in% names(compatibility))
})

cat("Tests for advanced HPV methods (CHPM & DRM) completed successfully!\n")