# Test file: tests/testthat/test-core-hpv-methods.R
# Tests for core HPV calculation methods with proper data types
# Source file: R/04_heat_pulse_velocity_core.R

library(testthat)

# Helper function to create mock sap_data with realistic temperature patterns
create_mock_sap_data_for_vh <- function() {

  # Create realistic temperature data with heat pulse pattern
  n_samples <- 120  # 2 minutes of data at 1 second intervals
  time_seq <- seq(from = as.POSIXct("2024-11-15 10:00:00"), by = 1, length.out = n_samples)

  # Simulate heat pulse effect (pulse starts at 30 seconds)
  baseline_temp <- 18.8
  heat_pulse_start <- 31  # After 30 second pre-pulse period

  # Create realistic temperature profiles with heat pulse
  do_temps <- rep(baseline_temp, n_samples)
  di_temps <- rep(baseline_temp - 0.2, n_samples)
  uo_temps <- rep(baseline_temp + 0.1, n_samples)
  ui_temps <- rep(baseline_temp - 0.1, n_samples)

  # Add heat pulse effect (stronger downstream, delayed upstream)
  for (i in heat_pulse_start:n_samples) {
    time_after_pulse <- i - heat_pulse_start
    # Downstream sensors get stronger, earlier signal
    do_temps[i] <- baseline_temp + 1.2 * exp(-time_after_pulse / 20) * (time_after_pulse > 0)
    di_temps[i] <- baseline_temp - 0.2 + 1.0 * exp(-time_after_pulse / 18) * (time_after_pulse > 0)
    # Upstream sensors get weaker, later signal
    uo_temps[i] <- baseline_temp + 0.1 + 0.8 * exp(-(time_after_pulse-5) / 25) * (time_after_pulse > 5)
    ui_temps[i] <- baseline_temp - 0.1 + 0.6 * exp(-(time_after_pulse-5) / 22) * (time_after_pulse > 5)
  }

  measurements <- data.frame(
    pulse_id = 1,
    datetime = time_seq,
    do = do_temps,
    di = di_temps,
    uo = uo_temps,
    ui = ui_temps,
    stringsAsFactors = FALSE
  )

  diagnostics <- data.frame(
    pulse_id = 1,
    datetime = time_seq[1],
    batt_volt = 4.1,
    batt_current = 200,
    batt_temp = 30,
    external_volt = 22,
    external_current = 85,
    stringsAsFactors = FALSE
  )

  sap_data <- list(
    diagnostics = diagnostics,
    measurements = measurements,
    metadata = list(
      file_path = "test_data.txt",
      format = "ict_current",
      import_time = Sys.time(),
      n_pulses = 1
    )
  )

  class(sap_data) <- c("sap_data", "list")
  return(sap_data)
}

# Helper function to create test vectors
create_test_vectors <- function() {
  # Simulate delta temperature vectors with realistic heat pulse response
  n_points <- 120
  time <- 1:n_points
  pulse_start <- 31

  # Create temperature responses (downstream > upstream due to flow)
  heat_effect <- pmax(0, exp(-(time - pulse_start)/30) * (time >= pulse_start))

  delatT_do <- 0.8 * heat_effect + rnorm(n_points, 0, 0.02)
  delatT_di <- 0.6 * heat_effect + rnorm(n_points, 0, 0.02)
  delatT_uo <- 0.5 * heat_effect + rnorm(n_points, 0, 0.02)
  delatT_ui <- 0.4 * heat_effect + rnorm(n_points, 0, 0.02)

  # Set pre-pulse period to NA
  pre_pulse_indices <- 1:30
  delatT_do[pre_pulse_indices] <- NA
  delatT_di[pre_pulse_indices] <- NA
  delatT_uo[pre_pulse_indices] <- NA
  delatT_ui[pre_pulse_indices] <- NA

  # Calculate temperature ratios for HRM
  dTratio_douo <- delatT_do / delatT_uo
  dTratio_diui <- delatT_di / delatT_ui

  # Create HRM period (typically 60-100 seconds after pulse)
  HRM_period <- rep(FALSE, n_points)
  HRM_period[61:100] <- TRUE

  return(list(
    delatT_do = delatT_do,
    delatT_di = delatT_di,
    delatT_uo = delatT_uo,
    delatT_ui = delatT_ui,
    dTratio_douo = dTratio_douo,
    dTratio_diui = dTratio_diui,
    HRM_period = HRM_period,
    pre_pulse_indices = pre_pulse_indices
  ))
}

# Test HRM calculation
test_that("calc_hrm produces reasonable results", {
  test_data <- create_test_vectors()

  result <- sapFluxR:::calc_hrm(
    test_data$dTratio_douo,
    test_data$dTratio_diui,
    test_data$HRM_period,
    0.0025,
    0.5
  )

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
  expect_true(all(is.finite(c(result$outer, result$inner))))
})

test_that("calc_hrm handles edge cases", {
  # Test with constant temperature ratios (should produce zero velocity)
  dTratio_douo <- rep(1.0, 120)
  dTratio_diui <- rep(1.0, 120)
  HRM_period <- rep(FALSE, 120)
  HRM_period[61:100] <- TRUE

  result <- sapFluxR:::calc_hrm(dTratio_douo, dTratio_diui, HRM_period, 0.0025, 0.5)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))

  # When ratio = 1, log(1) = 0, so velocity should be 0
  expect_equal(result$outer, 0)
  expect_equal(result$inner, 0)
})

test_that("calc_hrm warns when HRM period has no valid data", {
  test_data <- create_test_vectors()

  # Create HRM period with no valid ratios
  HRM_period_empty <- rep(FALSE, 120)
  HRM_period_empty[61:65] <- TRUE
  test_data$dTratio_douo[61:65] <- NA
  test_data$dTratio_diui[61:65] <- NA

  expect_warning(
    result <- sapFluxR:::calc_hrm(test_data$dTratio_douo, test_data$dTratio_diui, HRM_period_empty, 0.0025, 0.5),
    "temperature ratios are NA"
  )
})

# Test MHR calculation
test_that("calc_mhr produces reasonable results", {
  test_data <- create_test_vectors()

  result <- sapFluxR:::calc_mhr(
    test_data$delatT_do,
    test_data$delatT_di,
    test_data$delatT_uo,
    test_data$delatT_ui,
    0.0025,
    0.5
  )

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
})

test_that("calc_mhr handles edge cases", {
  # Test with flat temperature profile (no maxima)
  delatT_flat <- rep(0.1, 120)
  delatT_flat[1:30] <- NA

  result <- sapFluxR:::calc_mhr(delatT_flat, delatT_flat, delatT_flat, delatT_flat, 0.0025, 0.5)

  expect_type(result, "list")
  # When upstream and downstream maxima are equal, log(1) = 0, velocity = 0
  expect_equal(result$outer, 0)
  expect_equal(result$inner, 0)
})

# Test T-max Cohen calculation
test_that("calc_tmax_coh produces reasonable results", {
  test_data <- create_test_vectors()

  result <- sapFluxR:::calc_tmax_coh(
    test_data$delatT_do,
    test_data$delatT_di,
    0.0025,
    0.5,
    30  # pre_pulse parameter
  )

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
})

test_that("calc_tmax_coh handles edge cases", {
  # Test with very small probe spacing (could cause negative discriminant)
  test_data <- create_test_vectors()

  expect_warning(
    result <- sapFluxR:::calc_tmax_coh(test_data$delatT_do, test_data$delatT_di, 0.0025, 0.1, 30),
    "Negative discriminant"
  )
})

# Test T-max Kluitenberg calculation
test_that("calc_tmax_klu produces reasonable results", {
  test_data <- create_test_vectors()

  # Suppress expected warnings about pulse duration constraints
  result <- suppressWarnings(sapFluxR:::calc_tmax_klu(
    test_data$delatT_do,
    test_data$delatT_di,
    0.0025,
    0.5,
    2,   # tp_1
    30   # pre_pulse
  ))

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
})

test_that("calc_tmax_klu handles edge cases", {
  test_data <- create_test_vectors()

  # Test with time to max <= pulse duration (should warn)
  # Create data where max occurs very early
  early_max_do <- test_data$delatT_do
  early_max_do[32] <- max(early_max_do, na.rm = TRUE) * 2  # Force early max

  expect_warning(
    result <- sapFluxR:::calc_tmax_klu(early_max_do, test_data$delatT_di, 0.0025, 0.5, 2, 30),
    "Time to maximum temperature must be greater than pulse duration"
  )
})

# Test DMA calculation
test_that("calc_dma combines HRM and Tmax results correctly", {
  hrm_results <- list(outer = 5, inner = 4)  # Below critical velocity
  tmax_results <- list(outer = 25, inner = 30)  # Above critical velocity

  result <- sapFluxR:::calc_dma(hrm_results, tmax_results, 0.0025, 0.5)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))

  # Should use HRM values since they're below critical
  Vh_HRM_crit <- 0.0025 / 0.5 * 3600  # = 18 cm/hr
  expect_equal(result$outer, hrm_results$outer)  # 5 < 18, so use HRM
  expect_equal(result$inner, hrm_results$inner)  # 4 < 18, so use HRM
})

test_that("calc_dma handles NA results properly", {
  hrm_results <- list(outer = NA, inner = 15)  # NA outer, valid inner
  tmax_results <- list(outer = 25, inner = 30)  # Valid results

  result <- sapFluxR:::calc_dma(hrm_results, tmax_results, 0.0025, 0.5)

  expect_type(result, "list")
  # Should use tmax for outer (HRM is NA), HRM for inner (15 < 18)
  expect_equal(result$outer, tmax_results$outer)
  expect_equal(result$inner, hrm_results$inner)
})

# Integration test using main function
test_that("calc_heat_pulse_velocity integrates all methods", {
  # Create realistic test data
  sap_data <- create_mock_sap_data_for_vh()

  result <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR", "DMA"))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("datetime", "pulse_id", "method", "sensor_position", "Vh_cm_hr", "quality_flag") %in% names(result)))

  # Should have results for multiple methods
  expect_gt(length(unique(result$method)), 1)
})

test_that("calc_heat_pulse_velocity handles edge cases", {
  # Test with problematic data
  sap_data <- create_mock_sap_data_for_vh()

  # Make all temperatures the same (should cause issues but not crash)
  sap_data$measurements[, c("do", "di", "uo", "ui")] <- 18.8

  result <- calc_heat_pulse_velocity(sap_data, methods = "HRM")

  expect_s3_class(result, "data.frame")
  expect_true("quality_flag" %in% names(result))

  # Results may have quality flags indicating problems
  if (nrow(result) > 0) {
    expect_true(any(result$quality_flag %in% c("INFINITE", "MISSING", "OK")))
  }
})

# Test quality flag assignment
test_that("add_quality_flags correctly identifies issues", {
  results <- data.frame(
    datetime = rep(Sys.time(), 5),
    pulse_id = rep(1, 5),
    method = rep("HRM", 5),
    sensor_position = rep("outer", 5),
    Vh_cm_hr = c(10, 250, -60, Inf, NA),  # Normal, high, negative, infinite, missing
    stringsAsFactors = FALSE
  )

  flagged_results <- sapFluxR:::add_quality_flags(results)

  expect_equal(flagged_results$quality_flag[1], "OK")
  expect_equal(flagged_results$quality_flag[2], "HIGH_VELOCITY")
  expect_equal(flagged_results$quality_flag[3], "NEGATIVE_FLOW")
  expect_equal(flagged_results$quality_flag[4], "INFINITE")
  expect_equal(flagged_results$quality_flag[5], "MISSING")
})
