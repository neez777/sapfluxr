# tests/testthat/test-spacing-correction.R
# Tests for Spacing Correction Functions

# Test data generation helpers ------------------------------------------------

create_test_vh_data <- function(n = 100, add_zero_periods = TRUE) {
  # Create test velocity data with known zero periods

  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00"),
    by = "30 min",
    length.out = n
  )

  # Create baseline velocity pattern (simulated diurnal)
  hour_of_day <- as.numeric(format(dates, "%H")) +
                 as.numeric(format(dates, "%M")) / 60
  base_vh <- 5 + 10 * sin((hour_of_day - 6) * pi / 12)  # Peaks at noon
  base_vh[base_vh < 0] <- 0

  # Add small random noise
  set.seed(123)
  noise <- rnorm(n, mean = 0, sd = 0.5)

  # Create zero-flow period with known offset
  if (add_zero_periods) {
    # Days 3-5: zero flow with offset = 0.8 cm/hr
    zero_period_idx <- which(dates >= as.POSIXct("2024-01-03 00:00:00") &
                            dates <= as.POSIXct("2024-01-05 23:59:59"))
    base_vh[zero_period_idx] <- 0
    noise[zero_period_idx] <- rnorm(length(zero_period_idx), mean = 0.8, sd = 0.1)
  }

  vh <- base_vh + noise

  # Create data frame for both sensors
  data.frame(
    datetime = rep(dates, 2),
    pulse_id = rep(1:n, 2),
    method = "HRM",
    sensor_position = rep(c("outer", "inner"), each = n),
    Vh_cm_hr = c(vh, vh * 0.9),  # Inner slightly lower
    quality_flag = "OK",
    stringsAsFactors = FALSE
  )
}


# Test calculate_burgess_coefficients() ---------------------------------------

test_that("calculate_burgess_coefficients() generates correct structure", {

  lookup <- calculate_burgess_coefficients()

  # Check class
  expect_s3_class(lookup, "burgess_lookup")
  expect_s3_class(lookup, "data.frame")

  # Check columns
  expect_true(all(c("zero_vh", "coef_a", "coef_b", "range_type") %in% names(lookup)))

  # Check default range
  expect_equal(min(lookup$zero_vh), -10)
  expect_equal(max(lookup$zero_vh), 10)
  expect_equal(nrow(lookup), 201)  # -10 to 10 by 0.1

  # Check attributes
  expect_equal(attr(lookup, "k_assumed"), 0.0025)
  expect_equal(attr(lookup, "x_nominal"), 0.5)
  expect_equal(attr(lookup, "t_measurement"), 80)
})

test_that("calculate_burgess_coefficients() handles custom parameters", {

  lookup <- calculate_burgess_coefficients(
    zero_vh_range = seq(-5, 5, by = 0.5),
    k = 0.003,
    x = 0.6,
    t = 90
  )

  expect_equal(nrow(lookup), 21)  # -5 to 5 by 0.5
  expect_equal(attr(lookup, "k_assumed"), 0.003)
  expect_equal(attr(lookup, "x_nominal"), 0.6)
  expect_equal(attr(lookup, "t_measurement"), 90)
})

test_that("calculate_burgess_coefficients() sets range_type correctly", {

  lookup <- calculate_burgess_coefficients()

  # Within ±5 should be "modeled"
  modeled_rows <- lookup[abs(lookup$zero_vh) <= 5, ]
  expect_true(all(modeled_rows$range_type == "modeled"))

  # Beyond ±5 should be "extrapolated"
  extrap_rows <- lookup[abs(lookup$zero_vh) > 5, ]
  expect_true(all(extrap_rows$range_type == "extrapolated"))
})

test_that("calculate_burgess_coefficients() extrapolation uses 1:1 offset", {

  lookup <- calculate_burgess_coefficients()

  # For extrapolated range, coef_a should be 1 and coef_b should be -zero_vh
  extrap_rows <- lookup[abs(lookup$zero_vh) > 5, ]

  expect_equal(extrap_rows$coef_a, rep(1, nrow(extrap_rows)))
  expect_equal(extrap_rows$coef_b, -extrap_rows$zero_vh)
})


# Test calculate_zero_offset() ------------------------------------------------

test_that("calculate_zero_offset() calculates correct offset", {

  vh_data <- create_test_vh_data(n = 200, add_zero_periods = TRUE)

  zero_periods <- list(
    list(start = "2024-01-03 00:00:00", end = "2024-01-05 23:59:59")
  )

  result <- calculate_zero_offset(
    vh_data = vh_data,
    zero_periods = zero_periods,
    sensor_position = "outer",
    method = "HRM"
  )

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("zero_vh", "mean_vh_raw", "n_observations",
                    "overall_sd", "overall_cv") %in% names(result)))

  # Check zero offset is approximately 0.8 (what we injected)
  expect_equal(result$zero_vh, 0.8, tolerance = 0.2)

  # Check CV is reasonable (should be low since we used controlled noise)
  expect_lt(result$overall_cv, 0.3)

  # Check number of observations
  expect_gt(result$n_observations, 0)

  # Check period summary
  expect_s3_class(result$period_summary, "data.frame")
  expect_equal(nrow(result$period_summary), 1)
})

test_that("calculate_zero_offset() handles multiple periods", {

  # Create dataset with sufficient timespan
  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00"),
    by = "30 min",
    length.out = 1000  # Longer dataset
  )

  vh_data <- data.frame(
    datetime = rep(dates, 2),
    pulse_id = rep(1:1000, 2),
    method = "HRM",
    sensor_position = rep(c("outer", "inner"), each = 1000),
    Vh_cm_hr = rnorm(2000, mean = 5, sd = 2),
    quality_flag = "OK",
    stringsAsFactors = FALSE
  )

  # Manually set two zero periods with different offsets (both within data range)
  period1_idx <- which(vh_data$datetime >= as.POSIXct("2024-01-05 00:00:00") &
                      vh_data$datetime <= as.POSIXct("2024-01-06 23:59:59") &
                      vh_data$sensor_position == "outer")
  period2_idx <- which(vh_data$datetime >= as.POSIXct("2024-01-10 00:00:00") &
                      vh_data$datetime <= as.POSIXct("2024-01-11 23:59:59") &
                      vh_data$sensor_position == "outer")

  set.seed(456)
  vh_data$Vh_cm_hr[period1_idx] <- rnorm(length(period1_idx), mean = 0.7, sd = 0.1)
  vh_data$Vh_cm_hr[period2_idx] <- rnorm(length(period2_idx), mean = 0.9, sd = 0.1)

  zero_periods <- list(
    list(start = "2024-01-05 00:00:00", end = "2024-01-06 23:59:59"),
    list(start = "2024-01-10 00:00:00", end = "2024-01-11 23:59:59")
  )

  result <- calculate_zero_offset(
    vh_data = vh_data,
    zero_periods = zero_periods,
    sensor_position = "outer",
    method = "HRM"
  )

  # Check period summary has 2 rows
  expect_equal(nrow(result$period_summary), 2)

  # Check overall mean is between 0.7 and 0.9
  expect_true(result$mean_vh_raw > 0.7 && result$mean_vh_raw < 0.9)
})

test_that("calculate_zero_offset() validates inputs", {

  vh_data <- create_test_vh_data()
  zero_periods <- list(list(start = "2024-01-03 00:00:00", end = "2024-01-05 23:59:59"))

  # Missing sensor_position
  expect_error(
    calculate_zero_offset(vh_data, zero_periods, sensor_position = "invalid"),
    "sensor_position must be 'outer' or 'inner'"
  )

  # Missing columns
  vh_bad <- vh_data[, !names(vh_data) %in% "datetime"]
  expect_error(
    calculate_zero_offset(vh_bad, zero_periods, sensor_position = "outer"),
    "Missing required columns"
  )

  # Empty zero_periods
  expect_error(
    calculate_zero_offset(vh_data, list(), sensor_position = "outer"),
    "zero_periods must be a non-empty list"
  )
})


# Test get_correction_coefficients() ------------------------------------------

test_that("get_correction_coefficients() retrieves correct coefficients", {

  lookup <- calculate_burgess_coefficients()

  result <- get_correction_coefficients(zero_vh = 0.5, lookup_table = lookup)

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("coef_a", "coef_b", "zero_vh", "range_type",
                    "severity") %in% names(result)))

  # For small offset, should be "minor" or "none"
  expect_true(result$severity %in% c("none", "minor"))
  expect_equal(result$range_type, "modeled")
})

test_that("get_correction_coefficients() assesses severity correctly", {

  lookup <- calculate_burgess_coefficients()

  # Minor: 1-3 cm/hr
  result_minor <- get_correction_coefficients(2.0, lookup)
  expect_equal(result_minor$severity, "minor")

  # Moderate: 3-5 cm/hr
  expect_warning(
    result_mod <- get_correction_coefficients(4.0, lookup),
    "significant probe misalignment"
  )
  expect_equal(result_mod$severity, "moderate")

  # Severe: 5-10 cm/hr
  expect_warning(
    result_sev <- get_correction_coefficients(7.0, lookup),
    "exceeds modeled range"
  )
  expect_equal(result_sev$severity, "severe")
})

test_that("get_correction_coefficients() rejects extreme offsets", {

  lookup <- calculate_burgess_coefficients()

  expect_error(
    get_correction_coefficients(15.0, lookup),
    "exceeds maximum correctable range"
  )

  expect_error(
    get_correction_coefficients(-12.0, lookup),
    "exceeds maximum correctable range"
  )
})


# Test apply_spacing_correction() ---------------------------------------------

test_that("apply_spacing_correction() applies correction correctly", {

  vh_data <- create_test_vh_data()

  # Define simple correction: Vh_corr = 1.05 * Vh - 0.5
  correction_params <- list(
    outer = list(
      sensor_position = "outer",
      coef_a = 1.05,
      coef_b = -0.5
    ),
    inner = list(
      sensor_position = "inner",
      coef_a = 1.02,
      coef_b = -0.3
    )
  )

  result <- apply_spacing_correction(
    vh_data = vh_data,
    correction_params = correction_params,
    method = "HRM",
    create_new_col = TRUE
  )

  # Check new column created
  expect_true("Vh_cm_hr_sc" %in% names(result))
  expect_true("spacing_correction_applied" %in% names(result))

  # Check correction applied
  expect_true(all(result$spacing_correction_applied))

  # Verify correction formula for outer sensor
  outer_rows <- result$sensor_position == "outer"
  expected_outer <- 1.05 * result$Vh_cm_hr[outer_rows] - 0.5
  expect_equal(result$Vh_cm_hr_sc[outer_rows], expected_outer, tolerance = 1e-10)

  # Verify correction formula for inner sensor
  inner_rows <- result$sensor_position == "inner"
  expected_inner <- 1.02 * result$Vh_cm_hr[inner_rows] - 0.3
  expect_equal(result$Vh_cm_hr_sc[inner_rows], expected_inner, tolerance = 1e-10)
})

test_that("apply_spacing_correction() can replace original column", {

  vh_data <- create_test_vh_data()
  original_vh <- vh_data$Vh_cm_hr

  correction_params <- list(
    outer = list(sensor_position = "outer", coef_a = 1.0, coef_b = -0.5),
    inner = list(sensor_position = "inner", coef_a = 1.0, coef_b = -0.3)
  )

  result <- apply_spacing_correction(
    vh_data = vh_data,
    correction_params = correction_params,
    create_new_col = FALSE
  )

  # Check original column was modified
  expect_false("Vh_cm_hr_sc" %in% names(result))
  expect_false(all(result$Vh_cm_hr == original_vh))
})


# Test apply_spacing_correction_workflow() ------------------------------------

test_that("apply_spacing_correction_workflow() runs complete workflow", {

  vh_data <- create_test_vh_data(n = 200, add_zero_periods = TRUE)

  zero_periods <- list(
    list(start = "2024-01-03 00:00:00", end = "2024-01-05 23:59:59")
  )

  result <- suppressMessages(
    apply_spacing_correction_workflow(
      vh_data = vh_data,
      zero_periods = zero_periods,
      sensors = c("outer", "inner"),
      method = "HRM",
      k_assumed = 0.0025,
      verbose = FALSE
    )
  )

  # Check result structure
  expect_s3_class(result, "spacing_correction_result")
  expect_type(result, "list")
  expect_true(all(c("vh_corrected", "zero_offset_results",
                    "correction_coefficients", "metadata") %in% names(result)))

  # Check corrected data
  expect_true("Vh_cm_hr_sc" %in% names(result$vh_corrected))
  expect_true("spacing_correction_applied" %in% names(result$vh_corrected))

  # Check zero offset results for both sensors
  expect_equal(length(result$zero_offset_results), 2)
  expect_true("outer" %in% names(result$zero_offset_results))
  expect_true("inner" %in% names(result$zero_offset_results))

  # Check correction coefficients
  expect_equal(length(result$correction_coefficients), 2)
  expect_true(all(c("coef_a", "coef_b") %in% names(result$correction_coefficients$outer)))

  # Check metadata
  expect_equal(result$metadata$method, "HRM")
  expect_equal(result$metadata$k_assumed, 0.0025)
  expect_equal(result$metadata$phase, "Phase 1: Assumed k")
})

test_that("apply_spacing_correction_workflow() handles errors gracefully", {

  vh_data <- create_test_vh_data(n = 50)  # No zero periods in data

  # This period has no data
  zero_periods <- list(
    list(start = "2025-01-01 00:00:00", end = "2025-01-02 00:00:00")
  )

  # Should error because no valid zero-flow data
  expect_error(
    suppressWarnings(
      apply_spacing_correction_workflow(
        vh_data = vh_data,
        zero_periods = zero_periods,
        sensors = c("outer"),
        verbose = FALSE
      )
    ),
    "No valid zero-flow data|No sensors were successfully processed"
  )
})


# Test print_spacing_correction_summary() --------------------------------------

test_that("print_spacing_correction_summary() displays correctly", {

  vh_data <- create_test_vh_data(n = 100, add_zero_periods = TRUE)
  zero_periods <- list(
    list(start = "2024-01-03 00:00:00", end = "2024-01-05 23:59:59")
  )

  result <- suppressMessages(
    apply_spacing_correction_workflow(
      vh_data = vh_data,
      zero_periods = zero_periods,
      sensors = c("outer"),
      verbose = FALSE
    )
  )

  # Should not error when printing
  expect_output(
    print_spacing_correction_summary(result),
    "SPACING CORRECTION SUMMARY REPORT"
  )

  # Test print method
  expect_output(
    print.spacing_correction_result(result),
    "SPACING CORRECTION SUMMARY REPORT"
  )
})


# Test check_heartwood_reference_available() -----------------------------------

test_that("check_heartwood_reference_available() correctly identifies available case", {
  # Standard ICT probe: 35mm length, inner sensor 7.5mm from tip

# Inner sensor depth = (35 - 7.5) / 10 = 2.75 cm from handle
  # With 2cm sapwood and no bark: margin = 2.75 - 2.0 = 0.75 cm
  # Required margin = 1.0 / 2 = 0.5 cm
  # 0.75 >= 0.5 → AVAILABLE

  result <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 2.0,
    bark_thickness = 0,
    field_of_influence = 1.0
  )

  expect_s3_class(result, "heartwood_reference_check")
  expect_true(result$available)
  expect_equal(result$inner_depth_cm, 2.75)
  expect_equal(result$sapwood_depth_cm, 2.0)
  expect_equal(result$margin_cm, 0.75)
  expect_equal(result$required_margin_cm, 0.5)
})

test_that("check_heartwood_reference_available() correctly identifies unavailable case", {
  # With 3cm sapwood: margin = 2.75 - 3.0 = -0.25 cm (negative = in sapwood)

  result <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 3.0,
    bark_thickness = 0,
    field_of_influence = 1.0
  )

  expect_false(result$available)
  expect_equal(result$margin_cm, -0.25)
  expect_true(grepl("WITHIN sapwood", result$recommendation))
})

test_that("check_heartwood_reference_available() accounts for bark thickness", {
  # With 0.5cm bark: inner_depth = 2.75 - 0.5 = 2.25 cm
  # With 2cm sapwood: margin = 2.25 - 2.0 = 0.25 cm
  # Required = 0.5 cm → NOT AVAILABLE (0.25 < 0.5)

  result <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 2.0,
    bark_thickness = 0.5,
    field_of_influence = 1.0
  )

  expect_false(result$available)
  expect_equal(result$inner_depth_cm, 2.25)
  expect_equal(result$margin_cm, 0.25)
})

test_that("check_heartwood_reference_available() respects field_of_influence", {
  # With default: margin = 0.75, required = 0.5 → AVAILABLE
  # With field_of_influence = 2.0: required = 1.0 → NOT AVAILABLE

  result_default <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 2.0,
    field_of_influence = 1.0
  )

  result_strict <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 2.0,
    field_of_influence = 2.0
  )

  expect_true(result_default$available)
  expect_false(result_strict$available)
  expect_equal(result_strict$required_margin_cm, 1.0)
})
test_that("check_heartwood_reference_available() validates inputs", {
  # Invalid sapwood_depth
  expect_error(
    check_heartwood_reference_available(
      probe_config = list(length = 35, inner_sensor = 7.5),
      sapwood_depth = -1
    ),
    "sapwood_depth must be a positive number"
  )

  # Invalid bark_thickness
  expect_error(
    check_heartwood_reference_available(
      probe_config = list(length = 35, inner_sensor = 7.5),
      sapwood_depth = 2.0,
      bark_thickness = -0.5
    ),
    "bark_thickness must be a non-negative number"
  )

  # Invalid probe_config
  expect_error(
    check_heartwood_reference_available(
      probe_config = "invalid",
      sapwood_depth = 2.0
    ),
    "probe_config must be a ProbeConfig object or a named list"
  )
})

test_that("check_heartwood_reference_available() print method works", {
  result <- check_heartwood_reference_available(
    probe_config = list(length = 35, inner_sensor = 7.5),
    sapwood_depth = 2.0
  )

  expect_output(print(result), "HEARTWOOD REFERENCE AVAILABILITY CHECK")
  expect_output(print(result), "Status: AVAILABLE")
})


# Test apply_heartwood_reference_correction() ----------------------------------

# Helper to create test data with known inner/outer relationship
create_heartwood_test_data <- function(n = 50, inner_offset = 0.8) {
  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00"),
    by = "30 min",
    length.out = n
  )

  set.seed(456)

  # Outer sensor: diurnal pattern
  hour_of_day <- as.numeric(format(dates, "%H")) + as.numeric(format(dates, "%M")) / 60
  outer_vh <- 5 + 8 * sin((hour_of_day - 6) * pi / 12)
  outer_vh[outer_vh < 0] <- 0
  outer_vh <- outer_vh + rnorm(n, 0, 0.3)

  # Inner sensor: constant offset (simulating heartwood with misalignment)
  inner_vh <- rep(inner_offset, n) + rnorm(n, 0, 0.1)

  data.frame(
    datetime = rep(dates, 2),
    pulse_id = rep(1:n, 2),
    method = "HRM",
    sensor_position = rep(c("outer", "inner"), each = n),
    Vh_cm_hr = c(outer_vh, inner_vh),
    quality_flag = "OK",
    stringsAsFactors = FALSE
  )
}

test_that("apply_heartwood_reference_correction() returns correct structure", {
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = 0.8)

  result <- apply_heartwood_reference_correction(
    vh_data = vh_data,
    method = "HRM",
    verbose = FALSE
  )

  expect_s3_class(result, "heartwood_reference_correction_result")
  expect_true("vh_corrected" %in% names(result))
  expect_true("offset_summary" %in% names(result))
  expect_true("metadata" %in% names(result))

  # Check corrected data has new column
  expect_true("Vh_cm_hr_hrc" %in% names(result$vh_corrected))
  expect_true("heartwood_ref_applied" %in% names(result$vh_corrected))
})

test_that("apply_heartwood_reference_correction() calculates offset summary", {
  inner_offset <- 1.2
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = inner_offset)

  result <- apply_heartwood_reference_correction(
    vh_data = vh_data,
    method = "HRM",
    verbose = FALSE
  )

  # Mean offset should be close to the specified inner_offset
  expect_true(abs(result$offset_summary$mean_offset - inner_offset) < 0.3)
  expect_true(result$offset_summary$sd_offset < 0.5)  # Low variability
  expect_true(result$offset_summary$n_observations == 50)
})

test_that("apply_heartwood_reference_correction() applies Burgess correction", {
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = 0.8)

  result <- apply_heartwood_reference_correction(
    vh_data = vh_data,
    method = "HRM",
    verbose = FALSE
  )

  # Get outer sensor data
  outer_original <- vh_data$Vh_cm_hr[vh_data$sensor_position == "outer"]
  outer_corrected <- result$vh_corrected$Vh_cm_hr_hrc[
    result$vh_corrected$sensor_position == "outer"
  ]

  # Corrected values should differ from original (correction applied)
  expect_false(all(outer_original == outer_corrected))

  # With positive offset, corrected should generally be lower
  expect_true(mean(outer_corrected, na.rm = TRUE) < mean(outer_original, na.rm = TRUE))
})

test_that("apply_heartwood_reference_correction() marks inner sensor as not corrected", {
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = 0.8)

  result <- apply_heartwood_reference_correction(
    vh_data = vh_data,
    method = "HRM",
    verbose = FALSE
  )

  inner_applied <- result$vh_corrected$heartwood_ref_applied[
    result$vh_corrected$sensor_position == "inner"
  ]

  # Inner sensor should not have correction applied
  expect_true(all(inner_applied == FALSE))
})

test_that("apply_heartwood_reference_correction() validates inputs", {
  vh_data <- create_heartwood_test_data(n = 50)

  # Missing required columns
  bad_data <- vh_data[, c("datetime", "Vh_cm_hr")]
  expect_error(
    apply_heartwood_reference_correction(bad_data, verbose = FALSE),
    "Missing required columns"
  )

  # Missing inner sensor
  outer_only <- vh_data[vh_data$sensor_position == "outer", ]
  expect_error(
    apply_heartwood_reference_correction(outer_only, verbose = FALSE),
    "must contain both 'inner' and 'outer'"
  )
})

test_that("apply_heartwood_reference_correction() handles missing matches", {
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = 0.8)

  # Remove some inner sensor readings
  inner_rows <- which(vh_data$sensor_position == "inner")
  vh_data <- vh_data[-inner_rows[1:10], ]  # Remove first 10 inner readings

  result <- apply_heartwood_reference_correction(
    vh_data = vh_data,
    method = "HRM",
    verbose = FALSE
  )

  # Should have some uncorrected observations
  expect_true(result$metadata$n_outer_uncorrected > 0)
})

test_that("apply_heartwood_reference_correction() quality assessment works", {
  # Good alignment (low offset)
  vh_good <- create_heartwood_test_data(n = 50, inner_offset = 0.5)
  result_good <- apply_heartwood_reference_correction(vh_good, verbose = FALSE)
  expect_equal(result_good$offset_summary$quality, "GOOD")

  # Moderate misalignment
  vh_moderate <- create_heartwood_test_data(n = 50, inner_offset = 2.0)
  result_moderate <- apply_heartwood_reference_correction(vh_moderate, verbose = FALSE)
  expect_equal(result_moderate$offset_summary$quality, "ACCEPTABLE")

  # Significant misalignment
  vh_warning <- create_heartwood_test_data(n = 50, inner_offset = 4.0)
  result_warning <- apply_heartwood_reference_correction(vh_warning, verbose = FALSE)
  expect_equal(result_warning$offset_summary$quality, "WARNING")
})

test_that("apply_heartwood_reference_correction() print method works", {
  vh_data <- create_heartwood_test_data(n = 50, inner_offset = 0.8)
  result <- apply_heartwood_reference_correction(vh_data, verbose = FALSE)

  expect_output(print(result), "HEARTWOOD REFERENCE CORRECTION RESULT")
  expect_output(print(result), "Offset Summary")
})
