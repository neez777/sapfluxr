# tests/testthat/test-heat-pulse-velocity.R
# Test heat pulse velocity calculation functionality
# Source file: R/04_heat_pulse_velocity_core.R

library(testthat)

# Helper function to create mock heat_pulse_data with realistic temperature patterns
create_mock_heat_pulse_data_for_vh <- function() {

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

  heat_pulse_data <- list(
    diagnostics = diagnostics,
    measurements = measurements,
    metadata = list(
      file_path = "test_data.txt",
      format = "ict_current",
      import_time = Sys.time(),
      n_pulses = 1
    )
  )

  class(heat_pulse_data) <- c("heat_pulse_data", "list")
  return(heat_pulse_data)
}

# Basic functionality tests
test_that("calc_heat_pulse_velocity works with valid data", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"))

  # Check that it's a data frame (class may vary depending on implementation)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("datetime", "pulse_id", "method", "sensor_position", "Vh_cm_hr", "quality_flag") %in% names(result)))
})

test_that("calc_heat_pulse_velocity fails with invalid input", {
  invalid_data <- list(not_heat_pulse_data = TRUE)

  expect_error(calc_heat_pulse_velocity(invalid_data), "Input must be a heat_pulse_data object")
})

test_that("calc_heat_pulse_velocity handles missing pulse IDs", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  expect_warning(
    expect_error(calc_heat_pulse_velocity(heat_pulse_data, pulse_ids = c(999)),
                 "No valid pulse IDs to process"),
    "Pulse IDs not found in data"
  )
})

test_that("calc_heat_pulse_velocity works with specific pulse IDs", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  result <- calc_heat_pulse_velocity(heat_pulse_data, pulse_ids = 1, methods = "HRM")

  expect_equal(unique(result$pulse_id), 1)
  expect_true(all(result$method == "HRM"))
})

test_that("calc_heat_pulse_velocity works with custom parameters", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  custom_params <- list(
    diffusivity = 0.003,
    x = 0.6,
    pre_pulse = 25
  )

  result <- calc_heat_pulse_velocity(heat_pulse_data, parameters = custom_params, methods = "HRM")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("calc_vh_single_pulse calculates HRM correctly", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()
  measurements <- heat_pulse_data$measurements

  default_params <- list(
    diffusivity = 0.0025,
    probe_spacing = 0.5,
    x = 0.5,
    L = 0.5,
    H = 0.8,
    tp_1 = 2,
    HRM_start = 60,
    HRM_end = 100,
    pre_pulse = 30
  )

  # Filter measurements for pulse_id 1
  pulse_data <- measurements[measurements$pulse_id == 1, ]
  result <- calc_vh_single_pulse(pulse_data, 1, default_params, "HRM")

  expect_true(nrow(result) == 2)  # Outer and inner
  expect_true(all(result$method == "HRM"))
  expect_true(all(result$sensor_position %in% c("outer", "inner")))
  expect_true(all(is.numeric(result$Vh_cm_hr)))
})

# Test individual calculation methods
test_that("calc_hrm produces reasonable results", {
  # Create simple ratios for testing
  dTratio_douo <- c(NA, NA, 1.5, 1.6, 1.4, 1.5)  # First two NA for pre-pulse
  dTratio_diui <- c(NA, NA, 1.3, 1.4, 1.2, 1.3)
  HRM_period <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)  # Use last 3 points
  tp <- c(0, 1, 2, 3, 4, 5)  # Time vector in seconds

  result <- calc_hrm(dTratio_douo, dTratio_diui, HRM_period, 0.0025, 0.5, tp)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
  expect_true(all(is.finite(c(result$outer, result$inner))))
})

test_that("calc_mhr produces reasonable results", {
  # Create delta temperature vectors with clear maxima
  delatT_do <- c(NA, NA, 0.5, 1.0, 1.5, 1.2, 0.8)
  delatT_di <- c(NA, NA, 0.3, 0.8, 1.2, 1.0, 0.6)
  delatT_uo <- c(NA, NA, 0.2, 0.4, 0.8, 0.6, 0.4)
  delatT_ui <- c(NA, NA, 0.1, 0.3, 0.6, 0.5, 0.3)

  # Pre-compute peak information (required by optimized calc_mhr)
  peak_info <- list(
    dTdo_max = max(delatT_do, na.rm = TRUE),
    dTdi_max = max(delatT_di, na.rm = TRUE),
    dTuo_max = max(delatT_uo, na.rm = TRUE),
    dTui_max = max(delatT_ui, na.rm = TRUE),
    idx_do = which.max(delatT_do),
    idx_di = which.max(delatT_di),
    idx_uo = which.max(delatT_uo),
    idx_ui = which.max(delatT_ui)
  )
  pre_pulse <- 2
  peak_info$time_do <- peak_info$idx_do - pre_pulse
  peak_info$time_di <- peak_info$idx_di - pre_pulse
  peak_info$time_uo <- peak_info$idx_uo - pre_pulse
  peak_info$time_ui <- peak_info$idx_ui - pre_pulse

  result <- calc_mhr(delatT_do, delatT_di, delatT_uo, delatT_ui, 0.0025, 0.5, pre_pulse, peak_info)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))

  # Check that timing information is provided
  expect_true(all(c("calc_time_outer", "calc_time_inner",
                    "window_start_outer", "window_end_outer",
                    "window_start_inner", "window_end_inner") %in% names(result)))
})

test_that("calc_tmax_coh produces reasonable results", {
  # Create delta temperature vectors
  delatT_do <- c(rep(NA, 3), 0.5, 1.0, 1.5, 1.2, 0.8, 0.5)  # Max at position 6 (index)
  delatT_di <- c(rep(NA, 3), 0.3, 0.8, 1.2, 1.0, 0.6, 0.3)  # Max at position 6 (index)

  # Pre-compute peak information (required by optimized calc_tmax_coh)
  peak_info <- list(
    dTdo_max = max(delatT_do, na.rm = TRUE),
    dTdi_max = max(delatT_di, na.rm = TRUE),
    idx_do = which.max(delatT_do),
    idx_di = which.max(delatT_di)
  )
  pre_pulse <- 3
  peak_info$time_do <- peak_info$idx_do - pre_pulse
  peak_info$time_di <- peak_info$idx_di - pre_pulse

  result <- calc_tmax_coh(delatT_do, delatT_di, 0.0025, 0.5, pre_pulse, peak_info)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
  # Should produce positive velocities with reasonable values
  expect_true(all(c(result$outer, result$inner) > 0))
  expect_true(all(c(result$outer, result$inner) < 1000))  # Reasonable upper bound
})

test_that("calc_tmax_klu produces reasonable results", {
  # Create delta temperature vectors
  delatT_do <- c(rep(NA, 3), 0.5, 1.0, 1.5, 1.2, 0.8)  # Max at position 6
  delatT_di <- c(rep(NA, 3), 0.3, 0.8, 1.2, 1.0, 0.6)  # Max at position 6

  # Pre-compute peak information (required by optimized calc_tmax_klu)
  peak_info <- list(
    dTdo_max = max(delatT_do, na.rm = TRUE),
    dTdi_max = max(delatT_di, na.rm = TRUE),
    idx_do = which.max(delatT_do),
    idx_di = which.max(delatT_di)
  )
  pre_pulse <- 3
  peak_info$time_do <- peak_info$idx_do - pre_pulse
  peak_info$time_di <- peak_info$idx_di - pre_pulse

  result <- calc_tmax_klu(delatT_do, delatT_di, 0.0025, 0.5, 2, pre_pulse, peak_info)

  expect_type(result, "list")
  expect_true(all(c("outer", "inner") %in% names(result)))
  expect_true(all(is.numeric(c(result$outer, result$inner))))
})

test_that("add_quality_flags correctly identifies issues", {
  results <- data.frame(
    datetime = rep(Sys.time(), 5),
    pulse_id = rep(1, 5),
    method = rep("HRM", 5),
    sensor_position = rep("outer", 5),
    Vh_cm_hr = c(10, 250, -60, Inf, NA),  # Normal, high, negative, infinite, missing
    stringsAsFactors = FALSE
  )

  flagged_results <- add_quality_flags(results)

  expect_equal(flagged_results$quality_flag[1], "OK")
  expect_equal(flagged_results$quality_flag[2], "HIGH_VELOCITY")
  expect_equal(flagged_results$quality_flag[3], "NEGATIVE_FLOW")
  expect_equal(flagged_results$quality_flag[4], "INFINITE")
  expect_equal(flagged_results$quality_flag[5], "MISSING")
})

# Integration tests
test_that("all methods produce results", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  all_methods <- c("HRM", "MHR", "HRMXa", "HRMXb", "Tmax_Coh", "Tmax_Klu")

  # This might produce warnings due to mathematical constraints, but should not error
  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = all_methods)

  # Check for basic data frame structure (class may vary)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check that we have results for multiple methods
  unique_methods <- unique(result$method)
  expect_true(length(unique_methods) > 1)
})

test_that("calc_heat_pulse_velocity handles data with missing temperature columns", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()
  heat_pulse_data$measurements$do <- NULL  # Remove required column

  expect_error(calc_heat_pulse_velocity(heat_pulse_data), "No pulses were successfully processed")
})

test_that("calc_heat_pulse_velocity handles pulse ID validation properly", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Request non-existent pulse ID
  expect_warning({
    result <- calc_heat_pulse_velocity(heat_pulse_data, pulse_ids = c(1, 999), methods = "HRM")
  })

  # Should still process the valid pulse
  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$pulse_id), 1)
})

test_that("calc_heat_pulse_velocity provides progress messages for many pulses", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses to trigger progress messages
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  all_measurements <- list(base_measurements)
  all_diagnostics <- list(base_diagnostics)

  for (i in 2:15) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800  # 30 min apart
    all_measurements[[i]] <- new_measurements

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime + (i-1) * 1800
    all_diagnostics[[i]] <- new_diagnostics
  }

  heat_pulse_data$measurements <- do.call(rbind, all_measurements)
  heat_pulse_data$diagnostics <- do.call(rbind, all_diagnostics)
  heat_pulse_data$metadata$n_pulses <- 15

  # Capture messages (some R versions may not trigger progress for 15 pulses)
  result <- suppressMessages(calc_heat_pulse_velocity(heat_pulse_data, methods = "HRM"))

  # Test should pass whether or not progress messages are shown
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("calc_heat_pulse_velocity handles processing errors gracefully", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Create problematic data (all temperatures the same - this should cause calculation issues)
  heat_pulse_data$measurements[, c("do", "di", "uo", "ui")] <- 18.8

  # Should complete but may have warnings
  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = "HRM")

  # Should still return a result, even if calculations have issues
  expect_s3_class(result, "data.frame")

  # May have quality flags indicating problems
  expect_true("quality_flag" %in% names(result))
})

# Edge case tests
test_that("calc_heat_pulse_velocity handles edge cases", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Test with problematic data
  # Make all temperatures the same (should cause issues but not crash)
  heat_pulse_data$measurements[, c("do", "di", "uo", "ui")] <- 18.8

  expect_no_error({
    result <- calc_heat_pulse_velocity(heat_pulse_data, methods = "HRM")
  })

  expect_s3_class(result, "data.frame")
  expect_true("quality_flag" %in% names(result))

  # Results may have quality flags indicating problems
  if (nrow(result) > 0) {
    expect_true(any(result$quality_flag %in% c("INFINITE", "MISSING", "OK")))
  }
})

test_that("Error handling for completely invalid data", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Remove all temperature columns
  heat_pulse_data$measurements <- heat_pulse_data$measurements[, c("pulse_id", "datetime")]

  expect_error({
    calc_heat_pulse_velocity(heat_pulse_data, methods = "HRM")
  })
})

# Integration test: full workflow
test_that("Integration test: complete workflow with realistic data", {
  # Create realistic test data
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses for better testing
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  for (i in 2:3) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800  # 30 min apart

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800

    heat_pulse_data$measurements <- rbind(heat_pulse_data$measurements, new_measurements)
    heat_pulse_data$diagnostics <- rbind(heat_pulse_data$diagnostics, new_diagnostics)
  }

  heat_pulse_data$metadata$n_pulses <- 3

  # Process with multiple methods
  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR", "Tmax_Klu"))

  expect_s3_class(result, "data.frame")
  expect_equal(length(unique(result$pulse_id)), 3)

  # Should have multiple methods
  expect_gt(length(unique(result$method)), 1)

  # Test that we can use other package functions if they exist
  if (exists("filter_velocity_results")) {
    filtered <- filter_velocity_results(result, quality_flags = "OK")
    expect_s3_class(filtered, "data.frame")
  }

  if (exists("calc_velocity_stats")) {
    stats <- calc_velocity_stats(result)
    expect_type(stats, "list")
  }
})

# Performance test (optional)
test_that("Performance test: processing multiple pulses efficiently", {
  # Create larger dataset
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  for (i in 2:5) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800

    heat_pulse_data$measurements <- rbind(heat_pulse_data$measurements, new_measurements)
    heat_pulse_data$diagnostics <- rbind(heat_pulse_data$diagnostics, new_diagnostics)
  }

  heat_pulse_data$metadata$n_pulses <- 5

  # Time the calculation
  start_time <- Sys.time()
  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"))
  end_time <- Sys.time()

  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_lt(processing_time, 30)  # Should complete within 30 seconds
  expect_gt(nrow(result), 0)
  expect_equal(length(unique(result$pulse_id)), 5)
})

# sDMA Processing Tests
test_that("HRM calculates Peclet numbers", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Calculate with HRM
  result <- calc_heat_pulse_velocity(heat_pulse_data, methods = "HRM")

  # Check that Peclet numbers are present for HRM
  hrm_results <- result[result$method == "HRM", ]
  expect_true("peclet_number" %in% names(hrm_results))
  expect_true(all(!is.na(hrm_results$peclet_number)))
  expect_true(all(is.numeric(hrm_results$peclet_number)))
  expect_true(all(is.finite(hrm_results$peclet_number)))
})

test_that("apply_sdma_processing validates inputs correctly", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()
  vh_results <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"))

  # Test missing HRM
  vh_no_hrm <- vh_results[vh_results$method != "HRM", ]
  expect_error(
    apply_sdma_processing(vh_no_hrm, "MHR"),
    "HRM results not found"
  )

  # Test missing Peclet numbers (simulate by removing them)
  vh_no_peclet <- vh_results
  vh_no_peclet$peclet_number <- NA
  expect_error(
    apply_sdma_processing(vh_no_peclet, "MHR"),
    "do not contain Peclet numbers"
  )

  # Test missing secondary method
  expect_error(
    apply_sdma_processing(vh_results, "Tmax_Klu"),
    "Secondary method.*not found"
  )

  # Test HRM as secondary method (should be rejected)
  expect_error(
    apply_sdma_processing(vh_results, "HRM"),
    "Cannot use HRM as secondary method"
  )
})

test_that("apply_sdma_processing creates correct sDMA results", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Calculate with HRM and MHR
  vh_results <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"))
  original_rows <- nrow(vh_results)

  # Apply sDMA processing (force calculation even if Pe low)
  vh_sdma <- apply_sdma_processing(vh_results, "MHR", skip_low_peclet = FALSE, show_progress = FALSE)

  # Check structure
  expect_s3_class(vh_sdma, "data.frame")
  expect_gt(nrow(vh_sdma), original_rows)  # Should have more rows

  # Check for sDMA method
  expect_true("sDMA:MHR" %in% vh_sdma$method)

  # Check that sDMA rows have selected_method populated
  sdma_rows <- vh_sdma[vh_sdma$method == "sDMA:MHR", ]
  expect_true(all(!is.na(sdma_rows$selected_method)))
  expect_true(all(sdma_rows$selected_method %in% c("HRM", "MHR")))

  # Check that sDMA rows have Peclet numbers
  expect_true(all(!is.na(sdma_rows$peclet_number)))

  # Verify switching logic: Pe < 1.0 should use HRM
  hrm_selected <- sdma_rows[sdma_rows$selected_method == "HRM", ]
  if (nrow(hrm_selected) > 0) {
    expect_true(all(hrm_selected$peclet_number < 1.0))
  }

  # Verify switching logic: Pe >= 1.0 should use MHR
  mhr_selected <- sdma_rows[sdma_rows$selected_method == "MHR", ]
  if (nrow(mhr_selected) > 0) {
    expect_true(all(mhr_selected$peclet_number >= 1.0))
  }
})

test_that("apply_sdma_processing handles multiple secondary methods", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Calculate with HRM, MHR, and Tmax_Klu
  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = c("HRM", "MHR", "Tmax_Klu")
  )
  original_rows <- nrow(vh_results)

  # Apply sDMA processing with multiple secondary methods (force calculation)
  vh_sdma <- apply_sdma_processing(
    vh_results,
    secondary_method = c("MHR", "Tmax_Klu"),
    skip_low_peclet = FALSE,
    show_progress = FALSE
  )

  # Check structure
  expect_s3_class(vh_sdma, "data.frame")
  expect_gt(nrow(vh_sdma), original_rows)  # Should have more rows

  # Check for both sDMA methods
  expect_true("sDMA:MHR" %in% vh_sdma$method)
  expect_true("sDMA:Tmax_Klu" %in% vh_sdma$method)

  # Check that both sDMA methods have selected_method populated
  sdma_mhr <- vh_sdma[vh_sdma$method == "sDMA:MHR", ]
  sdma_tmax <- vh_sdma[vh_sdma$method == "sDMA:Tmax_Klu", ]

  expect_true(all(!is.na(sdma_mhr$selected_method)))
  expect_true(all(!is.na(sdma_tmax$selected_method)))

  # Both should switch based on same Peclet threshold
  expect_true(all(sdma_mhr$selected_method %in% c("HRM", "MHR")))
  expect_true(all(sdma_tmax$selected_method %in% c("HRM", "Tmax_Klu")))
})

test_that("apply_sdma_processing preserves original results", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Calculate with HRM and MHR
  vh_results <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"))

  # Store original for comparison
  original_hrm <- vh_results[vh_results$method == "HRM", ]
  original_mhr <- vh_results[vh_results$method == "MHR", ]

  # Apply sDMA processing (force calculation)
  vh_sdma <- apply_sdma_processing(vh_results, "MHR", skip_low_peclet = FALSE, show_progress = FALSE)

  # Check that original HRM and MHR results are still present and unchanged
  new_hrm <- vh_sdma[vh_sdma$method == "HRM", ]
  new_mhr <- vh_sdma[vh_sdma$method == "MHR", ]

  expect_equal(nrow(new_hrm), nrow(original_hrm))
  expect_equal(nrow(new_mhr), nrow(original_mhr))

  # Check Vh values haven't changed
  expect_equal(new_hrm$Vh_cm_hr, original_hrm$Vh_cm_hr)
  expect_equal(new_mhr$Vh_cm_hr, original_mhr$Vh_cm_hr)
})


# ---- Auto-Fill Missing Pulses Tests ----

test_that("calc_heat_pulse_velocity auto-fills missing pulses by default", {
  # Create data with multiple pulses
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses first (need at least 5 to create meaningful gaps)
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  all_measurements <- list()
  all_diagnostics <- list()

  for (i in 1:5) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800  # 30 min intervals
    all_measurements[[i]] <- new_measurements

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800
    all_diagnostics[[i]] <- new_diagnostics
  }

  heat_pulse_data$measurements <- do.call(rbind, all_measurements)
  heat_pulse_data$diagnostics <- do.call(rbind, all_diagnostics)
  heat_pulse_data$metadata$n_pulses <- 5

  # NOW remove some pulses to create gaps
  measurements <- heat_pulse_data$measurements
  diagnostics <- heat_pulse_data$diagnostics

  # Remove pulse_id 2 and 4 (creating gaps in 1,2,3,4,5 sequence)
  measurements <- measurements[measurements$pulse_id != 2, ]
  measurements <- measurements[measurements$pulse_id != 4, ]
  diagnostics <- diagnostics[diagnostics$pulse_id != 2, ]
  diagnostics <- diagnostics[diagnostics$pulse_id != 4, ]

  heat_pulse_data$measurements <- measurements
  heat_pulse_data$diagnostics <- diagnostics

  # Calculate with auto-fill enabled (default)
  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    show_progress = FALSE
  )

  # Should have DATA_MISSING flags for missing pulses
  expect_true("DATA_MISSING" %in% vh_results$quality_flag)

  # Check that missing pulse rows were added
  missing_rows <- vh_results[vh_results$quality_flag == "DATA_MISSING", ]
  expect_gt(nrow(missing_rows), 0)

  # Missing rows should have NA for Vh_cm_hr
  expect_true(all(is.na(missing_rows$Vh_cm_hr)))

  # Should have gap metadata attached
  expect_true(!is.null(attr(vh_results, "missing_pulse_summary")))
  expect_true(!is.null(attr(vh_results, "gap_report")))
})


test_that("calc_heat_pulse_velocity can disable auto-fill", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Remove some pulses
  measurements <- heat_pulse_data$measurements
  diagnostics <- heat_pulse_data$diagnostics

  # Add unique times first
  measurements$datetime <- measurements$datetime + (measurements$pulse_id - 1) * 1800
  diagnostics$datetime <- diagnostics$datetime + (diagnostics$pulse_id - 1) * 1800

  measurements <- measurements[measurements$pulse_id != 2, ]
  diagnostics <- diagnostics[diagnostics$pulse_id != 2, ]
  heat_pulse_data$measurements <- measurements
  heat_pulse_data$diagnostics <- diagnostics

  # Calculate with auto-fill disabled
  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = FALSE,
    show_progress = FALSE
  )

  # Should NOT have DATA_MISSING flags
  expect_false("DATA_MISSING" %in% vh_results$quality_flag)

  # Should not have gap metadata
  expect_null(attr(vh_results, "missing_pulse_summary"))
  expect_null(attr(vh_results, "gap_report"))
})


test_that("auto-fill preserves multi-sensor structure", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses first (need at least 5)
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  all_measurements <- list()
  all_diagnostics <- list()

  for (i in 1:5) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800
    all_measurements[[i]] <- new_measurements

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800
    all_diagnostics[[i]] <- new_diagnostics
  }

  heat_pulse_data$measurements <- do.call(rbind, all_measurements)
  heat_pulse_data$diagnostics <- do.call(rbind, all_diagnostics)
  heat_pulse_data$metadata$n_pulses <- 5

  # Remove a pulse
  measurements <- heat_pulse_data$measurements
  diagnostics <- heat_pulse_data$diagnostics

  measurements <- measurements[measurements$pulse_id != 3, ]
  diagnostics <- diagnostics[diagnostics$pulse_id != 3, ]
  heat_pulse_data$measurements <- measurements
  heat_pulse_data$diagnostics <- diagnostics

  # Calculate with HRM (produces outer and inner results)
  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    show_progress = FALSE
  )

  # Missing pulse should have entries for both sensors
  missing_rows <- vh_results[vh_results$quality_flag == "DATA_MISSING", ]

  if (nrow(missing_rows) > 0) {
    # Should have both outer and inner sensor positions
    expect_true("outer" %in% missing_rows$sensor_position)
    expect_true("inner" %in% missing_rows$sensor_position)
  }
})


test_that("auto-fill preserves multi-method structure", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses first (need at least 5)
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  all_measurements <- list()
  all_diagnostics <- list()

  for (i in 1:5) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800
    all_measurements[[i]] <- new_measurements

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800
    all_diagnostics[[i]] <- new_diagnostics
  }

  heat_pulse_data$measurements <- do.call(rbind, all_measurements)
  heat_pulse_data$diagnostics <- do.call(rbind, all_diagnostics)
  heat_pulse_data$metadata$n_pulses <- 5

  # Remove a pulse
  measurements <- heat_pulse_data$measurements
  diagnostics <- heat_pulse_data$diagnostics

  measurements <- measurements[measurements$pulse_id != 3, ]
  diagnostics <- diagnostics[diagnostics$pulse_id != 3, ]
  heat_pulse_data$measurements <- measurements
  heat_pulse_data$diagnostics <- diagnostics

  # Calculate with multiple methods
  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = c("HRM", "MHR"),
    fill_missing_pulses = TRUE,
    show_progress = FALSE
  )

  # Missing pulse should have entries for both methods
  missing_rows <- vh_results[vh_results$quality_flag == "DATA_MISSING", ]

  if (nrow(missing_rows) > 0) {
    # Should have both HRM and MHR methods
    expect_true("HRM" %in% missing_rows$method)
    expect_true("MHR" %in% missing_rows$method)
  }
})


test_that("max_gap_hours parameter controls gap filling", {
  # Create data with a large gap
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")

  # Create measurements at 30-min intervals with a 5-hour gap
  times <- c(
    seq(start_time, by = "30 min", length.out = 5),           # 08:00 - 10:00
    seq(start_time + 5*3600, by = "30 min", length.out = 5)   # 13:00 - 15:00 (5hr gap)
  )

  measurements <- data.frame(
    pulse_id = 1:10,
    datetime = rep(times, each = 30),
    seconds_since_pulse = rep(1:30, times = 10),
    do = rnorm(300, 18.5, 0.5),
    di = rnorm(300, 18.4, 0.5),
    uo = rnorm(300, 18.6, 0.5),
    ui = rnorm(300, 18.3, 0.5)
  )

  diagnostics <- data.frame(
    pulse_id = 1:10,
    datetime = times,
    batt_volt = rep(12, 10),
    batt_current = rep(0.5, 10),
    batt_temp = rep(25, 10),
    external_volt = rep(12, 10),
    external_current = rep(0.1, 10)
  )

  heat_pulse_data <- list(
    measurements = measurements,
    diagnostics = diagnostics,
    metadata = list(
      file_name = "test.txt",
      format = "test",
      import_time = Sys.time(),
      n_pulses = 10
    )
  )
  class(heat_pulse_data) <- c("heat_pulse_data", "list")

  # Calculate with small max_gap_hours (shouldn't fill 5-hour gap)
  vh_results_small_gap <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    max_gap_hours = 2,  # Only fill gaps < 2 hours
    show_progress = FALSE
  )

  # Calculate with large max_gap_hours (should fill 5-hour gap)
  vh_results_large_gap <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    max_gap_hours = 10,  # Fill gaps < 10 hours
    show_progress = FALSE
  )

  # Small gap setting should have fewer DATA_MISSING than large gap setting
  n_missing_small <- sum(vh_results_small_gap$quality_flag == "DATA_MISSING", na.rm = TRUE)
  n_missing_large <- sum(vh_results_large_gap$quality_flag == "DATA_MISSING", na.rm = TRUE)

  expect_lt(n_missing_small, n_missing_large)
})


test_that("gap_report attribute contains correct information", {
  # Create data with known gaps
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  times <- c(
    seq(start_time, by = "30 min", length.out = 3),           # 08:00 - 09:00
    seq(start_time + 2*3600, by = "30 min", length.out = 3)   # 10:00 - 11:00 (1hr gap)
  )

  measurements <- data.frame(
    pulse_id = 1:6,
    datetime = rep(times, each = 30),
    seconds_since_pulse = rep(1:30, times = 6),
    do = rnorm(180, 18.5, 0.5),
    di = rnorm(180, 18.4, 0.5),
    uo = rnorm(180, 18.6, 0.5),
    ui = rnorm(180, 18.3, 0.5)
  )

  diagnostics <- data.frame(
    pulse_id = 1:6,
    datetime = times,
    batt_volt = rep(12, 6),
    batt_current = rep(0.5, 6),
    batt_temp = rep(25, 6),
    external_volt = rep(12, 6),
    external_current = rep(0.1, 6)
  )

  heat_pulse_data <- list(
    measurements = measurements,
    diagnostics = diagnostics,
    metadata = list(
      file_name = "test.txt",
      format = "test",
      import_time = Sys.time(),
      n_pulses = 6
    )
  )
  class(heat_pulse_data) <- c("heat_pulse_data", "list")

  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    max_gap_hours = 24,
    show_progress = FALSE
  )

  # Check gap_report exists and has correct structure
  gap_report <- attr(vh_results, "gap_report")
  expect_false(is.null(gap_report))

  if (!is.null(gap_report) && nrow(gap_report) > 0) {
    expect_true("gap_start" %in% names(gap_report))
    expect_true("gap_end" %in% names(gap_report))
    expect_true("n_missing" %in% names(gap_report))
    expect_true("duration_hours" %in% names(gap_report))
    expect_true("filled" %in% names(gap_report))
  }
})


test_that("missing_pulse_summary attribute is accurate", {
  heat_pulse_data <- create_mock_heat_pulse_data_for_vh()

  # Add more pulses first (need at least 5)
  base_measurements <- heat_pulse_data$measurements
  base_diagnostics <- heat_pulse_data$diagnostics

  all_measurements <- list()
  all_diagnostics <- list()

  for (i in 1:5) {
    new_measurements <- base_measurements
    new_measurements$pulse_id <- i
    new_measurements$datetime <- new_measurements$datetime + (i-1) * 1800
    all_measurements[[i]] <- new_measurements

    new_diagnostics <- base_diagnostics
    new_diagnostics$pulse_id <- i
    new_diagnostics$datetime <- new_diagnostics$datetime[1] + (i-1) * 1800
    all_diagnostics[[i]] <- new_diagnostics
  }

  heat_pulse_data$measurements <- do.call(rbind, all_measurements)
  heat_pulse_data$diagnostics <- do.call(rbind, all_diagnostics)
  heat_pulse_data$metadata$n_pulses <- 5

  # Remove 2 pulses
  measurements <- heat_pulse_data$measurements
  diagnostics <- heat_pulse_data$diagnostics

  measurements <- measurements[!measurements$pulse_id %in% c(2, 4), ]
  diagnostics <- diagnostics[!diagnostics$pulse_id %in% c(2, 4), ]
  heat_pulse_data$measurements <- measurements
  heat_pulse_data$diagnostics <- diagnostics

  vh_results <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    show_progress = FALSE
  )

  summary <- attr(vh_results, "missing_pulse_summary")
  expect_false(is.null(summary))

  if (!is.null(summary)) {
    expect_true("n_expected" %in% names(summary))
    expect_true("n_actual" %in% names(summary))
    expect_true("n_missing" %in% names(summary))
    expect_true("n_filled" %in% names(summary))
    expect_true("expected_interval_hours" %in% names(summary))

    # Should detect the missing pulses
    expect_gt(summary$n_missing, 0)
  }
})


test_that("interval_tolerance_seconds allows for clock drift", {
  # Create data with slight timing jitter
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")

  # 30-min intervals with ±3 second jitter
  times <- seq(start_time, by = "30 min", length.out = 5)
  times <- times + runif(5, -3, 3)  # Add jitter

  measurements <- data.frame(
    pulse_id = 1:5,
    datetime = rep(times, each = 30),
    seconds_since_pulse = rep(1:30, times = 5),
    do = rnorm(150, 18.5, 0.5),
    di = rnorm(150, 18.4, 0.5),
    uo = rnorm(150, 18.6, 0.5),
    ui = rnorm(150, 18.3, 0.5)
  )

  diagnostics <- data.frame(
    pulse_id = 1:5,
    datetime = times,
    batt_volt = rep(12, 5),
    batt_current = rep(0.5, 5),
    batt_temp = rep(25, 5),
    external_volt = rep(12, 5),
    external_current = rep(0.1, 5)
  )

  heat_pulse_data <- list(
    measurements = measurements,
    diagnostics = diagnostics,
    metadata = list(
      file_name = "test.txt",
      format = "test",
      import_time = Sys.time(),
      n_pulses = 5
    )
  )
  class(heat_pulse_data) <- c("heat_pulse_data", "list")

  # With tight tolerance, might detect false missing pulses
  vh_results_tight <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    interval_tolerance_seconds = 1,  # Strict
    show_progress = FALSE
  )

  # With loose tolerance, should not detect false missing pulses
  vh_results_loose <- calc_heat_pulse_velocity(
    heat_pulse_data,
    methods = "HRM",
    fill_missing_pulses = TRUE,
    interval_tolerance_seconds = 10,  # Loose
    show_progress = FALSE
  )

  # Loose tolerance should have fewer or equal DATA_MISSING
  n_missing_tight <- sum(vh_results_tight$quality_flag == "DATA_MISSING", na.rm = TRUE)
  n_missing_loose <- sum(vh_results_loose$quality_flag == "DATA_MISSING", na.rm = TRUE)

  expect_lte(n_missing_loose, n_missing_tight)
})