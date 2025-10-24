# Test Data Creation Helper Functions
# Used across multiple test files

#' Create Test Sap Data Object
#'
#' Creates a properly formatted sap_data object for testing purposes.
#'
#' @param n_points Number of temperature measurements per pulse
#' @param n_pulses Number of pulses to simulate
#' @param add_noise Whether to add random noise to temperature readings
#'
#' @return A sap_data object compatible with package functions
#'
#' @keywords internal
create_test_sap_data <- function(n_points = 120, n_pulses = 1, add_noise = TRUE) {

  # Create test data for multiple pulses
  all_measurements <- list()
  all_diagnostics <- list()

  for (pulse_num in 1:n_pulses) {
    # Simulate a heat pulse response
    time <- 1:n_points

    # Pre-pulse temperatures (stable)
    baseline_do <- 18.8
    baseline_di <- 18.6
    baseline_uo <- 18.9
    baseline_ui <- 18.7

    # Simulate heat pulse response (starts at point 31)
    pulse_start <- 31

    # Temperature responses
    do <- rep(baseline_do, n_points)
    di <- rep(baseline_di, n_points)
    uo <- rep(baseline_uo, n_points)
    ui <- rep(baseline_ui, n_points)

    # Add heat pulse response
    for (i in pulse_start:n_points) {
      t_after_pulse <- i - pulse_start
      if (t_after_pulse > 0) {
        # Downstream sensors (higher response)
        do[i] <- baseline_do + 1.5 * exp(-0.02 * t_after_pulse) * (1 - exp(-0.1 * t_after_pulse))
        di[i] <- baseline_di + 1.2 * exp(-0.02 * t_after_pulse) * (1 - exp(-0.1 * t_after_pulse))

        # Upstream sensors (delayed, lower response)
        if (t_after_pulse > 5) {
          uo[i] <- baseline_uo + 0.8 * exp(-0.015 * (t_after_pulse - 5)) * (1 - exp(-0.08 * (t_after_pulse - 5)))
          ui[i] <- baseline_ui + 0.6 * exp(-0.015 * (t_after_pulse - 5)) * (1 - exp(-0.08 * (t_after_pulse - 5)))
        }
      }
    }

    # Add noise if requested
    if (add_noise) {
      noise_level <- 0.01
      do <- do + rnorm(n_points, 0, noise_level)
      di <- di + rnorm(n_points, 0, noise_level)
      uo <- uo + rnorm(n_points, 0, noise_level)
      ui <- ui + rnorm(n_points, 0, noise_level)
    }

    # Create timestamp
    start_time <- as.POSIXct("2024-01-01 10:00:00") + (pulse_num - 1) * 1800  # 30 min apart
    datetime <- start_time + time

    # Create measurements data frame
    measurements <- data.frame(
      pulse_id = pulse_num,
      datetime = datetime,
      do = do,
      di = di,
      uo = uo,
      ui = ui
    )

    # Create diagnostics data frame
    diagnostics <- data.frame(
      pulse_id = pulse_num,
      batt_volt = 4.1,
      batt_current = 15.0,
      batt_temp = 25.0,
      external_volt = 23.0,
      external_current = 50.0
    )

    all_measurements[[pulse_num]] <- measurements
    all_diagnostics[[pulse_num]] <- diagnostics
  }

  # Combine all data
  combined_measurements <- do.call(rbind, all_measurements)
  combined_diagnostics <- do.call(rbind, all_diagnostics)

  # Create heat_pulse_data object structure
  heat_pulse_data <- list(
    measurements = combined_measurements,
    diagnostics = combined_diagnostics,
    metadata = list(
      format = "test_format",
      file_path = "test_data.txt",
      import_time = Sys.time(),
      n_pulses = n_pulses
    ),
    validation = list(
      valid = TRUE,
      issues = character(0)
    )
  )

  class(heat_pulse_data) <- "heat_pulse_data"
  return(heat_pulse_data)
}
