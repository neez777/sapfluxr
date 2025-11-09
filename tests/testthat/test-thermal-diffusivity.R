# tests/testthat/test-thermal-diffusivity.R
# Tests for Thermal Diffusivity Estimation Functions

# Helper to create mock heat pulse data ----------------------------------------

create_mock_heat_pulse_data <- function(n_pulses = 50,
                                        k_actual = 0.0025,
                                        probe_spacing = 0.5) {
  # Calculate expected t_max from k
  t_max_expected <- (probe_spacing^2) / (4 * k_actual)

  # Create measurement times (1 second intervals, 0-120 seconds per pulse)
  n_measurements_per_pulse <- 121
  total_measurements <- n_pulses * n_measurements_per_pulse

  # Generate pulse data
  pulse_ids <- rep(1:n_pulses, each = n_measurements_per_pulse)

  # Base datetime
  base_time <- as.POSIXct("2024-01-01 00:00:00")
  # Each pulse is 30 minutes apart
  pulse_times <- base_time + (0:(n_pulses - 1)) * 1800

  # Measurement times within each pulse
  datetimes <- do.call(c, lapply(pulse_times, function(pt) {
    pt + 0:(n_measurements_per_pulse - 1)
  }))

  # Create temperature profiles
  # Simulate heat pulse with peak at t_max_expected
  temps_downstream <- numeric(total_measurements)
  temps_upstream <- numeric(total_measurements)

  set.seed(123)
  for (i in 1:n_pulses) {
    idx_start <- (i - 1) * n_measurements_per_pulse + 1
    idx_end <- i * n_measurements_per_pulse
    time_seq <- 0:(n_measurements_per_pulse - 1)

    # Pre-pulse baseline (first 30 seconds)
    baseline_temp <- 20 + rnorm(1, 0, 0.1)

    # Heat pulse response (simplified Gaussian-like peak)
    # Peak at t_max with some variability
    t_max_actual <- t_max_expected + rnorm(1, 0, 2)  # Add noise

    # Downstream temperature rise
    heat_response_down <- ifelse(
      time_seq < 30,
      0,  # Pre-pulse
      3 * exp(-((time_seq - 30 - t_max_actual)^2) / (2 * 200))  # Gaussian
    )

    # Upstream temperature rise (symmetric at zero flow)
    heat_response_up <- ifelse(
      time_seq < 30,
      0,  # Pre-pulse
      3 * exp(-((time_seq - 30 - t_max_actual)^2) / (2 * 200))  # Same as downstream
    )

    temps_downstream[idx_start:idx_end] <- baseline_temp + heat_response_down +
                                            rnorm(n_measurements_per_pulse, 0, 0.02)
    temps_upstream[idx_start:idx_end] <- baseline_temp + heat_response_up +
                                          rnorm(n_measurements_per_pulse, 0, 0.02)
  }

  # Create measurements data frame
  measurements <- data.frame(
    datetime = datetimes,
    pulse_id = pulse_ids,
    do = temps_downstream,
    di = temps_downstream * 0.98,  # Inner slightly different
    uo = temps_upstream,
    ui = temps_upstream * 0.98,
    stringsAsFactors = FALSE
  )

  # Create mock heat_pulse_data object
  heat_pulse_data <- list(
    measurements = measurements,
    diagnostics = data.frame(
      datetime = pulse_times,
      pulse_id = 1:n_pulses,
      battery_voltage = 12.0,
      battery_current = 0.5
    ),
    metadata = list(
      file_path = "mock_data.txt",
      import_time = Sys.time(),
      n_pulses = n_pulses
    ),
    validation = list(
      is_valid = TRUE,
      issues = character(0)
    )
  )

  class(heat_pulse_data) <- "heat_pulse_data"

  return(heat_pulse_data)
}


# Test calculate_time_to_max_temperatures() -----------------------------------

test_that("calculate_time_to_max_temperatures() finds correct peak times", {

  # Create simple test data with known peak
  test_temps <- data.frame(
    pulse_id = rep(1, 100),
    datetime = as.POSIXct("2024-01-01 00:00:00") + 0:99,
    sensor_position = "outer",
    do = c(rep(20, 30), 20 + 3 * exp(-((0:69 - 25)^2) / 200)),  # Peak at ~55 sec
    uo = c(rep(20, 30), 20 + 3 * exp(-((0:69 - 25)^2) / 200)),  # Same peak
    stringsAsFactors = FALSE
  )

  result <- calculate_time_to_max_temperatures(test_temps, pre_pulse = 30)

  expect_type(result, "list")
  expect_true(all(c("t_max_downstream", "t_max_upstream") %in% names(result)))

  # Peak should be around time 25 (after pre-pulse adjustment)
  expect_true(result$t_max_downstream[1] > 20 && result$t_max_downstream[1] < 30)
  expect_true(result$t_max_upstream[1] > 20 && result$t_max_upstream[1] < 30)

  # Should be symmetric (same peak time)
  expect_equal(result$t_max_downstream[1], result$t_max_upstream[1], tolerance = 1)
})

test_that("calculate_time_to_max_temperatures() handles inner sensor", {

  test_temps <- data.frame(
    pulse_id = rep(1, 100),
    datetime = as.POSIXct("2024-01-01 00:00:00") + 0:99,
    sensor_position = "inner",
    di = c(rep(20, 30), 20 + 3 * exp(-((0:69 - 25)^2) / 200)),
    ui = c(rep(20, 30), 20 + 3 * exp(-((0:69 - 25)^2) / 200)),
    stringsAsFactors = FALSE
  )

  result <- calculate_time_to_max_temperatures(test_temps, pre_pulse = 30)

  expect_type(result, "list")
  expect_length(result$t_max_downstream, 1)
  expect_length(result$t_max_upstream, 1)
})


# Test extract_zero_flow_temperatures() ---------------------------------------

test_that("extract_zero_flow_temperatures() filters to zero periods", {

  heat_pulse_data <- create_mock_heat_pulse_data(n_pulses = 100)

  zero_periods <- list(
    list(start = "2024-01-01 01:00:00", end = "2024-01-01 02:00:00")
  )

  result <- extract_zero_flow_temperatures(
    heat_pulse_data = heat_pulse_data,
    zero_periods = zero_periods,
    sensor_position = "outer"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("sensor_position" %in% names(result))
  expect_true(all(result$sensor_position == "outer"))

  # Check all datetimes are within zero period
  start_time <- as.POSIXct("2024-01-01 01:00:00")
  end_time <- as.POSIXct("2024-01-01 02:00:00")
  expect_true(all(result$datetime >= start_time | result$datetime <= end_time))
})

test_that("extract_zero_flow_temperatures() returns NULL for empty periods", {

  heat_pulse_data <- create_mock_heat_pulse_data(n_pulses = 10)

  # Period outside data range
  zero_periods <- list(
    list(start = "2025-01-01 00:00:00", end = "2025-01-02 00:00:00")
  )

  result <- extract_zero_flow_temperatures(
    heat_pulse_data = heat_pulse_data,
    zero_periods = zero_periods,
    sensor_position = "outer"
  )

  expect_null(result)
})


# Test estimate_k_from_tmax() -------------------------------------------------

test_that("estimate_k_from_tmax() calculates k correctly", {

  # Create data with known k
  k_known <- 0.0025
  heat_pulse_data <- create_mock_heat_pulse_data(
    n_pulses = 100,
    k_actual = k_known,
    probe_spacing = 0.5
  )

  # Use early time period as "zero flow"
  zero_periods <- list(
    list(start = "2024-01-01 00:00:00", end = "2024-01-01 02:00:00")
  )

  result <- suppressMessages(
    estimate_k_from_tmax(
      heat_pulse_data = heat_pulse_data,
      zero_periods = zero_periods,
      sensors = c("outer"),
      probe_spacing = 0.5,
      k_nominal = k_known
    )
  )

  # Check structure
  expect_s3_class(result, "k_estimation_result")
  expect_type(result, "list")
  expect_true(all(c("k_mean", "k_sd", "k_cv", "k_by_sensor",
                    "recommendation") %in% names(result)))

  # Estimated k should be close to actual k (within 25% due to noise in mock data)
  relative_error <- abs(result$k_mean - k_known) / k_known
  expect_lt(relative_error, 0.25)

  # Should recommend no action since k is close
  expect_true(result$recommendation %in% c("no_action", "reprocess_optional"))
})

test_that("estimate_k_from_tmax() detects differences from nominal", {

  # Create data with different k
  k_actual <- 0.0030  # 20% higher than nominal
  k_nominal <- 0.0025

  heat_pulse_data <- create_mock_heat_pulse_data(
    n_pulses = 50,
    k_actual = k_actual,
    probe_spacing = 0.5
  )

  zero_periods <- list(
    list(start = "2024-01-01 00:00:00", end = "2024-01-01 01:00:00")
  )

  result <- suppressMessages(
    estimate_k_from_tmax(
      heat_pulse_data = heat_pulse_data,
      zero_periods = zero_periods,
      sensors = c("outer"),
      k_nominal = k_nominal
    )
  )

  # Should detect significant difference
  expect_gt(abs(result$k_difference_percent), 5)

  # Should recommend reprocessing
  expect_true(result$recommendation %in% c("reprocess_recommended", "reprocess_critical"))
})

test_that("estimate_k_from_tmax() handles multiple sensors", {

  heat_pulse_data <- create_mock_heat_pulse_data(n_pulses = 50)

  zero_periods <- list(
    list(start = "2024-01-01 00:00:00", end = "2024-01-01 01:00:00")
  )

  result <- suppressMessages(
    estimate_k_from_tmax(
      heat_pulse_data = heat_pulse_data,
      zero_periods = zero_periods,
      sensors = c("outer", "inner"),
      k_nominal = 0.0025
    )
  )

  # Should have estimates for both sensors
  expect_equal(length(result$k_by_sensor), 2)
  expect_true("outer" %in% names(result$k_by_sensor))
  expect_true("inner" %in% names(result$k_by_sensor))

  # Should have symmetry results
  expect_equal(length(result$symmetry_results), 2)
})

test_that("estimate_k_from_tmax() checks symmetry", {

  heat_pulse_data <- create_mock_heat_pulse_data(n_pulses = 50)

  zero_periods <- list(
    list(start = "2024-01-01 00:00:00", end = "2024-01-01 01:00:00")
  )

  result <- suppressMessages(
    estimate_k_from_tmax(
      heat_pulse_data = heat_pulse_data,
      zero_periods = zero_periods,
      sensors = c("outer"),
      k_nominal = 0.0025
    )
  )

  # Check symmetry results exist
  expect_true("symmetry_results" %in% names(result))
  expect_true("outer" %in% names(result$symmetry_results))

  sym <- result$symmetry_results$outer
  expect_true(all(c("t_max_downstream", "t_max_upstream", "difference_seconds",
                    "status") %in% names(sym)))

  # At zero flow, should be symmetric (status = PASS)
  expect_equal(sym$status, "PASS")
})


# Test print method ------------------------------------------------------------

test_that("print.k_estimation_result() displays correctly", {

  heat_pulse_data <- create_mock_heat_pulse_data(n_pulses = 50)

  zero_periods <- list(
    list(start = "2024-01-01 00:00:00", end = "2024-01-01 01:00:00")
  )

  result <- suppressMessages(
    estimate_k_from_tmax(
      heat_pulse_data = heat_pulse_data,
      zero_periods = zero_periods,
      sensors = c("outer"),
      k_nominal = 0.0025
    )
  )

  # Should print without error
  expect_output(
    print.k_estimation_result(result),
    "THERMAL DIFFUSIVITY ESTIMATION SUMMARY"
  )

  expect_output(
    print.k_estimation_result(result),
    "RECOMMENDATION"
  )

  expect_output(
    print.k_estimation_result(result),
    "SYMMETRY CHECK"
  )
})
