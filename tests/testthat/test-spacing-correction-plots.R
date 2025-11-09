# tests/testthat/test-spacing-correction-plots.R
# Tests for Spacing Correction Plotting Functions

# Skip all tests if ggplot2 is not available
skip_if_not_installed_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    skip("ggplot2 not available")
  }
}

# Helper function to create test velocity data
create_test_vh_data_for_plots <- function(n = 200) {
  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00"),
    by = "30 min",
    length.out = n
  )

  # Create diurnal pattern
  hour_of_day <- as.numeric(format(dates, "%H")) +
                 as.numeric(format(dates, "%M")) / 60
  base_vh <- 5 + 10 * sin((hour_of_day - 6) * pi / 12)
  base_vh[base_vh < 0] <- 0

  set.seed(123)
  noise <- rnorm(n, mean = 0, sd = 0.5)

  # Add zero period offset
  zero_period_idx <- which(dates >= as.POSIXct("2024-01-02 22:00:00") &
                          dates <= as.POSIXct("2024-01-03 06:00:00"))
  base_vh[zero_period_idx] <- 0
  noise[zero_period_idx] <- rnorm(length(zero_period_idx), mean = 0.8, sd = 0.1)

  vh <- base_vh + noise

  data.frame(
    datetime = rep(dates, 2),
    pulse_id = rep(1:n, 2),
    method = "HRM",
    sensor_position = rep(c("outer", "inner"), each = n),
    Vh_cm_hr = c(vh, vh * 0.95),
    quality_flag = "OK",
    stringsAsFactors = FALSE
  )
}


# Test plot_zero_flow_periods() -----------------------------------------------

test_that("plot_zero_flow_periods() creates plot without error", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  p <- plot_zero_flow_periods(
    vh_data,
    zero_periods,
    sensor_position = "outer"
  )

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_zero_flow_periods() handles both sensors", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  p_outer <- plot_zero_flow_periods(vh_data, zero_periods, sensor_position = "outer")
  p_inner <- plot_zero_flow_periods(vh_data, zero_periods, sensor_position = "inner")

  expect_s3_class(p_outer, "ggplot")
  expect_s3_class(p_inner, "ggplot")
})

test_that("plot_zero_flow_periods() validates inputs", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00"))

  # Missing required columns
  vh_bad <- vh_data[, !names(vh_data) %in% "datetime"]
  expect_error(
    plot_zero_flow_periods(vh_bad, zero_periods, sensor_position = "outer"),
    "must contain columns"
  )

  # Invalid sensor position
  expect_error(
    plot_zero_flow_periods(vh_data, zero_periods, sensor_position = "invalid"),
    "'arg' should be one of"
  )
})


# Test plot_spacing_correction_comparison() -----------------------------------

test_that("plot_spacing_correction_comparison() creates plot", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  spacing_result <- suppressMessages(
    apply_spacing_correction_workflow(
      vh_data,
      zero_periods,
      sensors = c("outer"),
      verbose = FALSE
    )
  )

  p <- plot_spacing_correction_comparison(
    spacing_result,
    sensor_position = "outer"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_spacing_correction_comparison() handles date range", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  spacing_result <- suppressMessages(
    apply_spacing_correction_workflow(
      vh_data,
      zero_periods,
      sensors = c("outer"),
      verbose = FALSE
    )
  )

  date_range <- as.POSIXct(c("2024-01-02 00:00:00", "2024-01-03 12:00:00"))

  p <- plot_spacing_correction_comparison(
    spacing_result,
    sensor_position = "outer",
    date_range = date_range
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_spacing_correction_comparison() validates input", {
  skip_if_not_installed_ggplot2()

  expect_error(
    plot_spacing_correction_comparison(list(), sensor_position = "outer"),
    "must be a spacing_correction_result object"
  )
})


# Test plot_burgess_coefficients() --------------------------------------------

test_that("plot_burgess_coefficients() creates plot", {
  skip_if_not_installed_ggplot2()

  lookup <- calculate_burgess_coefficients()

  p <- plot_burgess_coefficients(lookup)

  expect_s3_class(p, "ggplot")
})

test_that("plot_burgess_coefficients() handles observed points", {
  skip_if_not_installed_ggplot2()

  lookup <- calculate_burgess_coefficients()

  p <- plot_burgess_coefficients(
    lookup,
    zero_vh_observed = c(0.5, 1.2, 2.0),
    labels = c("Outer", "Inner", "Test")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_burgess_coefficients() validates input", {
  skip_if_not_installed_ggplot2()

  expect_error(
    plot_burgess_coefficients(data.frame(x = 1)),
    "must be a burgess_lookup object"
  )
})


# Test plot_temperature_traces() ----------------------------------------------

test_that("plot_temperature_traces() creates plot", {
  skip_if_not_installed_ggplot2()

  # Create mock heat pulse data
  n_pulses <- 5
  n_measurements_per_pulse <- 121
  total_measurements <- n_pulses * n_measurements_per_pulse

  base_time <- as.POSIXct("2024-01-02 22:00:00")
  pulse_times <- base_time + (0:(n_pulses - 1)) * 1800

  datetimes <- do.call(c, lapply(pulse_times, function(pt) {
    pt + 0:(n_measurements_per_pulse - 1)
  }))

  pulse_ids <- rep(1:n_pulses, each = n_measurements_per_pulse)
  time_seq <- rep(0:(n_measurements_per_pulse - 1), n_pulses)

  # Simple temperature profile
  temps <- 20 + 3 * exp(-((time_seq - 55)^2) / 200)

  measurements <- data.frame(
    datetime = datetimes,
    pulse_id = pulse_ids,
    do = temps,
    di = temps * 0.98,
    uo = temps,
    ui = temps * 0.98
  )

  heat_pulse_data <- list(
    measurements = measurements,
    diagnostics = data.frame(
      datetime = pulse_times,
      pulse_id = 1:n_pulses,
      battery_voltage = 12.0
    ),
    metadata = list(n_pulses = n_pulses),
    validation = list(is_valid = TRUE)
  )
  class(heat_pulse_data) <- "heat_pulse_data"

  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  p <- plot_temperature_traces(
    heat_pulse_data,
    zero_periods,
    sensor_position = "outer",
    n_pulses = 3
  )

  expect_s3_class(p, "ggplot")
})


# Test plot_k_estimation_summary() ---------------------------------------------

test_that("plot_k_estimation_summary() creates plot", {
  skip_if_not_installed_ggplot2()

  # Create minimal k_result object
  k_result <- list(
    k_mean = 0.00245,
    k_sd = 0.00005,
    k_cv = 2.0,
    k_by_sensor = list(outer = 0.00245, inner = 0.00243),
    k_nominal = 0.0025,
    k_difference = -0.00005,
    k_difference_percent = -2.0,
    recommendation = "no_action",
    symmetry_results = list(
      outer = list(
        t_max_downstream = 25,
        t_max_upstream = 26,
        difference_seconds = 1,
        difference_percent = 4,
        status = "PASS"
      )
    )
  )
  class(k_result) <- c("k_estimation_result", "list")

  p <- plot_k_estimation_summary(k_result)

  expect_s3_class(p, "ggplot")
})

test_that("plot_k_estimation_summary() validates input", {
  skip_if_not_installed_ggplot2()

  expect_error(
    plot_k_estimation_summary(list()),
    "must be a k_estimation_result object"
  )
})


# Test plot_symmetry_check() --------------------------------------------------

test_that("plot_symmetry_check() creates plot", {
  skip_if_not_installed_ggplot2()

  k_result <- list(
    k_by_sensor = list(outer = 0.00245, inner = 0.00243),
    symmetry_results = list(
      outer = list(
        t_max_downstream = 25.0,
        t_max_upstream = 25.5,
        difference_seconds = 0.5,
        difference_percent = 2.0,
        status = "PASS"
      ),
      inner = list(
        t_max_downstream = 26.0,
        t_max_upstream = 26.2,
        difference_seconds = 0.2,
        difference_percent = 0.8,
        status = "PASS"
      )
    )
  )
  class(k_result) <- c("k_estimation_result", "list")

  p <- plot_symmetry_check(k_result)

  expect_s3_class(p, "ggplot")
})

test_that("plot_symmetry_check() validates input", {
  skip_if_not_installed_ggplot2()

  expect_error(
    plot_symmetry_check(list()),
    "must be a k_estimation_result object"
  )
})


# Test plot_spacing_correction_report() ---------------------------------------

test_that("plot_spacing_correction_report() creates report", {
  skip_if_not_installed_ggplot2()

  vh_data <- create_test_vh_data_for_plots()
  zero_periods <- list(
    list(start = "2024-01-02 22:00:00", end = "2024-01-03 06:00:00")
  )

  spacing_result <- suppressMessages(
    apply_spacing_correction_workflow(
      vh_data,
      zero_periods,
      sensors = c("outer"),
      verbose = FALSE
    )
  )

  report <- plot_spacing_correction_report(
    spacing_result,
    vh_data,
    zero_periods,
    sensor_position = "outer"
  )

  # Should return either patchwork object or list of plots
  expect_true(inherits(report, "patchwork") || is.list(report))
})
