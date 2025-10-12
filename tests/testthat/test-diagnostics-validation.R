# Test file: tests/testthat/test-diagnostics-validation.R
# Tests for diagnostics and validation functions
# Source files: R/07_method_comparison.R, R/08_quality_control.R, R/09_diagnostic_reporting.R

library(testthat)

# Helper function to create test data with known issues
create_test_sap_data_with_issues <- function() {

  # Create base test data
  n_measurements <- 200
  n_pulses <- 4
  measurements_per_pulse <- n_measurements / n_pulses

  datetime <- seq(from = as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
                  by = 1, length.out = n_measurements)

  # Create pulse structure
  pulse_id <- rep(1:n_pulses, each = measurements_per_pulse)

  # Base temperatures with known issues
  base_temp <- 20

  # Issue 1: Sensor drift in 'do'
  do_temps <- base_temp + seq(0, 2, length.out = n_measurements) + rnorm(n_measurements, 0, 0.05)

  # Issue 2: High noise in 'di'
  di_temps <- base_temp + rnorm(n_measurements, 0, 0.5)

  # Issue 3: Baseline offset in 'uo' (calibration issue)
  uo_temps <- base_temp + 1.5 + rnorm(n_measurements, 0, 0.05)

  # Issue 4: Normal 'ui' sensor
  ui_temps <- base_temp + rnorm(n_measurements, 0, 0.05)

  # Create measurements data frame
  measurements <- data.frame(
    pulse_id = pulse_id,
    datetime = datetime,
    do = do_temps,
    di = di_temps,
    uo = uo_temps,
    ui = ui_temps,
    stringsAsFactors = FALSE
  )

  # Create diagnostics data frame
  diagnostics <- data.frame(
    pulse_id = rep(1:n_pulses, each = 1),
    datetime = datetime[seq(1, n_measurements, by = measurements_per_pulse)],
    batt_volt = rep(12.5, n_pulses) + rnorm(n_pulses, 0, 0.1),
    batt_current = rep(200, n_pulses) + rnorm(n_pulses, 0, 10),
    batt_temp = rep(25, n_pulses) + rnorm(n_pulses, 0, 2),
    external_volt = rep(12.0, n_pulses) + rnorm(n_pulses, 0, 0.1),
    external_current = rep(0, n_pulses),
    stringsAsFactors = FALSE
  )

  # Create metadata
  metadata <- list(
    file_path = "test_data_with_issues.txt",
    format = "test",
    import_time = Sys.time(),
    file_size = 12345,
    n_pulses = n_pulses
  )

  # Create validation placeholder
  validation <- list(
    valid = TRUE,
    issues = character(0),
    warnings = character(0),
    summary = list()
  )

  # Return sap_data object
  structure(
    list(
      diagnostics = diagnostics,
      measurements = measurements,
      metadata = metadata,
      validation = validation
    ),
    class = "sap_data"
  )
}

# Test sensor diagnostics
test_that("diagnose_sensor_performance detects sensor issues", {

  sap_data <- create_test_sap_data_with_issues()

  # Test basic functionality
  sensor_diag <- diagnose_sensor_performance(sap_data)

  expect_true(is.list(sensor_diag))
  expect_true("sensor_status" %in% names(sensor_diag))
  expect_true("drift_analysis" %in% names(sensor_diag))
  expect_true("noise_analysis" %in% names(sensor_diag))
  expect_true("calibration_assessment" %in% names(sensor_diag))

  # Check that drift is detected in 'do' sensor
  expect_true("do" %in% names(sensor_diag$drift_analysis))
  do_drift <- sensor_diag$drift_analysis$do
  expect_true(is.numeric(do_drift$drift_rate))
  expect_true(abs(do_drift$drift_rate) > 0.01)  # Should detect the programmed drift

  # Check that high noise is detected in 'di' sensor
  expect_true("di" %in% names(sensor_diag$noise_analysis))
  di_noise <- sensor_diag$noise_analysis$di
  expect_true(is.numeric(di_noise$signal_to_noise))
  expect_true(di_noise$signal_to_noise < 100)  # Should have lower SNR due to noise

  # Check calibration assessment detects baseline differences
  expect_true("calibration_quality" %in% names(sensor_diag$calibration_assessment))
  expect_true(sensor_diag$calibration_assessment$max_difference > 1.0)  # uo offset
})

test_that("diagnose_sensor_performance handles edge cases", {

  # Test with insufficient data
  minimal_data <- create_test_sap_data_with_issues()
  minimal_data$measurements <- minimal_data$measurements[1:10, ]  # Very few measurements

  expect_warning(
    sensor_diag <- diagnose_sensor_performance(minimal_data),
    "Insufficient pulses"
  )

  # Test with missing sensors
  no_sensors <- create_test_sap_data_with_issues()
  no_sensors$measurements <- no_sensors$measurements[, c("pulse_id", "datetime")]

  expect_error(
    diagnose_sensor_performance(no_sensors),
    "Need at least 2 temperature sensors"
  )
})

# Test outlier detection
test_that("detect_sensor_outliers identifies outliers correctly", {

  sap_data <- create_test_sap_data_with_issues()

  # Add some obvious outliers
  sap_data$measurements$do[50] <- 100  # Extreme temperature
  sap_data$measurements$di[100] <- -50  # Extreme low temperature

  outlier_results <- detect_sensor_outliers(sap_data)

  expect_true(is.list(outlier_results))
  expect_true("outlier_summary" %in% names(outlier_results))
  expect_true("outlier_indices" %in% names(outlier_results))

  # Check that outliers were detected in 'do' and 'di'
  expect_true("do" %in% names(outlier_results$outlier_indices))
  expect_true("di" %in% names(outlier_results$outlier_indices))

  # Check that the specific outliers were found
  do_outliers <- outlier_results$outlier_indices$do
  di_outliers <- outlier_results$outlier_indices$di

  expect_true(50 %in% do_outliers)  # Should detect the extreme high value
  expect_true(100 %in% di_outliers)  # Should detect the extreme low value
})

test_that("detect_sensor_outliers works with different methods", {

  sap_data <- create_test_sap_data_with_issues()
  sap_data$measurements$do[50] <- 100  # Add outlier

  # Test different detection methods
  zscore_results <- detect_sensor_outliers(sap_data, methods = "zscore")
  iqr_results <- detect_sensor_outliers(sap_data, methods = "iqr")
  temporal_results <- detect_sensor_outliers(sap_data, methods = "temporal")

  expect_true(length(zscore_results$outlier_indices$do) > 0)
  expect_true(length(iqr_results$outlier_indices$do) > 0)

  # Test method comparison
  multi_method <- detect_sensor_outliers(sap_data, methods = c("zscore", "iqr"))
  expect_true("method_comparison" %in% names(multi_method))
})

# Test probe alignment validation
test_that("validate_probe_alignment_advanced detects alignment issues", {

  sap_data <- create_test_sap_data_with_issues()

  alignment_results <- validate_probe_alignment_advanced(sap_data)

  expect_true(is.list(alignment_results))
  expect_true("alignment_status" %in% names(alignment_results))
  expect_true("baseline_analysis" %in% names(alignment_results))

  # Should detect poor alignment due to uo offset
  expect_true(alignment_results$alignment_status %in% c("FAIR", "POOR"))

  # Check baseline analysis
  baseline <- alignment_results$baseline_analysis
  expect_true("temperature_range" %in% names(baseline))
  expect_true(baseline$temperature_range > 1.0)  # Should detect the 1.5°C offset in uo

  # Check that recommendations are provided
  expect_true(length(alignment_results$recommendations) > 0)
})

test_that("validate_probe_alignment_advanced handles edge cases", {

  # Test with insufficient sensors
  minimal_sensors <- create_test_sap_data_with_issues()
  minimal_sensors$measurements <- minimal_sensors$measurements[, c("pulse_id", "datetime", "do")]

  expect_error(
    validate_probe_alignment_advanced(minimal_sensors),
    "Need at least 2 temperature sensors"
  )

  # Test with insufficient pulses
  minimal_pulses <- create_test_sap_data_with_issues()
  minimal_pulses$measurements <- minimal_pulses$measurements[1:30, ]  # Only one pulse worth

  alignment_result <- validate_probe_alignment_advanced(minimal_pulses)
  expect_equal(alignment_result$alignment_status, "INSUFFICIENT_DATA")
})

# Test cross-method comparison
test_that("compare_hpv_methods compares methods correctly", {

  # Create mock vh_results with multiple methods
  datetime <- seq(from = as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
                  by = 3600, length.out = 50)

  # Create realistic velocity data with known differences
  hrm_velocities <- rnorm(50, 15, 5)  # Mean 15 cm/hr
  mhr_velocities <- hrm_velocities * 1.1 + rnorm(50, 0, 1)  # Slight systematic bias

  vh_results <- data.frame(
    datetime = rep(datetime, 2),
    pulse_id = rep(1:50, 2),
    method = rep(c("HRM", "MHR"), each = 50),
    sensor_position = rep("outer", 100),
    Vh_cm_hr = c(hrm_velocities, mhr_velocities),
    quality_flag = rep("OK", 100),
    stringsAsFactors = FALSE
  )

  class(vh_results) <- c("vh_results", "data.frame")

  comparison <- compare_hpv_methods(vh_results)

  expect_true(is.list(comparison))
  expect_true("method_summary" %in% names(comparison))
  expect_true("agreement_analysis" %in% names(comparison))
  expect_true("pairwise_comparison" %in% names(comparison))

  # Check method summaries
  expect_true("HRM" %in% names(comparison$method_summary))
  expect_true("MHR" %in% names(comparison$method_summary))

  hrm_summary <- comparison$method_summary$HRM
  expect_true(is.numeric(hrm_summary$mean))
  expect_true(hrm_summary$n_observations == 50)

  # Check agreement analysis
  agreement_names <- names(comparison$agreement_analysis)
  expect_true(any(grepl("MHR vs HRM", agreement_names)))
})

test_that("compare_hpv_methods handles edge cases", {

  # Test with single method
  single_method <- data.frame(
    datetime = as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
    pulse_id = 1,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = 15,
    quality_flag = "OK"
  )
  class(single_method) <- c("vh_results", "data.frame")

  expect_error(
    compare_hpv_methods(single_method),
    "Need at least 2 methods"
  )

  # Test with all outlier data
  outlier_data <- data.frame(
    datetime = rep(as.POSIXct("2024-01-01 10:00:00", tz = "UTC"), 4),
    pulse_id = rep(1:2, 2),
    method = rep(c("HRM", "MHR"), each = 2),
    sensor_position = rep("outer", 4),
    Vh_cm_hr = c(15, 20, 25, 30),
    quality_flag = rep("HIGH_VELOCITY", 4)
  )
  class(outlier_data) <- c("vh_results", "data.frame")

  expect_error(
    compare_hpv_methods(outlier_data, exclude_outliers = TRUE),
    "No valid data remaining"
  )
})

# Test comprehensive diagnostic report
test_that("generate_diagnostic_report creates comprehensive report", {

  sap_data <- create_test_sap_data_with_issues()

  # Test basic report generation
  report <- generate_diagnostic_report(sap_data, detailed = FALSE)

  expect_true(is.list(report))
  expect_true("report_summary" %in% names(report))
  expect_true("data_quality" %in% names(report))
  expect_true("sensor_diagnostics" %in% names(report))
  expect_true("probe_alignment" %in% names(report))
  expect_true("recommendations" %in% names(report))
  expect_true("report_text" %in% names(report))

  # Check report summary
  summary <- report$report_summary
  expect_true(is.numeric(summary$overall_quality_score))
  expect_true(summary$overall_quality_score >= 0 && summary$overall_quality_score <= 100)
  expect_true(is.numeric(summary$total_recommendations))

  # Check that issues were detected
  expect_true(length(report$recommendations) > 0)

  # Check report text is generated
  expect_true(is.character(report$report_text))
  expect_true(length(report$report_text) > 10)
})

test_that("generate_diagnostic_report works with velocity results", {

  sap_data <- create_test_sap_data_with_issues()

  # Create mock velocity results
  vh_results <- data.frame(
    datetime = as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
    pulse_id = 1,
    method = "HRM",
    sensor_position = "outer",
    Vh_cm_hr = 15,
    quality_flag = "OK"
  )
  class(vh_results) <- c("vh_results", "data.frame")

  report <- generate_diagnostic_report(sap_data, vh_results)

  # Should include method comparison (even though with one method it may error)
  expect_true("method_comparison" %in% names(report))
})

test_that("create_diagnostic_summary works correctly", {

  sap_data <- create_test_sap_data_with_issues()
  report <- generate_diagnostic_report(sap_data)

  summary_text <- create_diagnostic_summary(report)

  expect_true(is.character(summary_text))
  expect_true(length(summary_text) > 5)
  expect_true(any(grepl("DIAGNOSTIC SUMMARY", summary_text)))
  expect_true(any(grepl("Overall Quality", summary_text)))
})

# Test error handling
test_that("diagnostic functions handle invalid inputs", {

  # Test with non-sap_data object
  invalid_data <- list(a = 1, b = 2)

  expect_error(diagnose_sensor_performance(invalid_data), "sap_data must be a sap_data object")
  expect_error(detect_sensor_outliers(invalid_data), "sap_data must be a sap_data object")
  expect_error(validate_probe_alignment_advanced(invalid_data), "sap_data must be a sap_data object")
  expect_error(generate_diagnostic_report(invalid_data), "sap_data must be a sap_data object")

  # Test compare_hpv_methods with invalid vh_results
  invalid_vh <- data.frame(a = 1, b = 2)
  expect_error(compare_hpv_methods(invalid_vh), "vh_results must be a vh_results object")
})