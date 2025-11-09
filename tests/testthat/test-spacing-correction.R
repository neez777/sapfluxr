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
