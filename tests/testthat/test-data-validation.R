# Test data validation functionality
# Source file: R/02_data_validation.R

# Helper function to create mock heat_pulse_data object
create_mock_heat_pulse_data <- function(valid = TRUE, missing_components = NULL,
                                 empty_data = FALSE, extreme_values = FALSE,
                                 missing_timestamps = FALSE) {

  # Create basic structure
  heat_pulse_data <- list(
    diagnostics = data.frame(
      pulse_id = 1:3,
      datetime = as.POSIXct(c("2024-11-15 10:00:00", "2024-11-15 10:30:00", "2024-11-15 11:00:00")),
      batt_volt = c(4.1, 4.0, 3.9),
      batt_current = c(200, 15, 15),
      batt_temp = c(30, 28, 25),
      external_volt = c(22, 21, 20),
      external_current = c(85, 18, 18),
      stringsAsFactors = FALSE
    ),
    measurements = data.frame(
      pulse_id = rep(1:3, each = 3),
      datetime = as.POSIXct(rep(c("2024-11-15 10:00:00", "2024-11-15 10:30:00", "2024-11-15 11:00:00"), each = 3)),
      do = c(18.8, 18.8, 18.8, 18.9, 18.9, 18.9, 19.0, 19.0, 19.0),
      di = c(18.6, 18.6, 18.6, 18.7, 18.7, 18.7, 18.8, 18.8, 18.8),
      uo = c(18.9, 18.9, 18.9, 19.0, 19.0, 19.0, 19.1, 19.1, 19.1),
      ui = c(18.7, 18.7, 18.7, 18.8, 18.8, 18.8, 18.9, 18.9, 18.9),
      stringsAsFactors = FALSE
    ),
    metadata = list(
      file_path = "test_file.txt",
      format = "ict_current",
      import_time = Sys.time(),
      n_pulses = 3
    )
  )

  # Apply modifications for testing different conditions
  if (!is.null(missing_components)) {
    for (comp in missing_components) {
      heat_pulse_data[[comp]] <- NULL
    }
  }

  if (empty_data) {
    heat_pulse_data$diagnostics <- heat_pulse_data$diagnostics[0, ]
    heat_pulse_data$measurements <- heat_pulse_data$measurements[0, ]
  }

  if (extreme_values) {
    heat_pulse_data$measurements$do[1] <- 100  # Extreme temperature
    heat_pulse_data$diagnostics$batt_volt[1] <- 50  # Extreme voltage
  }

  if (missing_timestamps) {
    heat_pulse_data$diagnostics$datetime[1] <- NA
    heat_pulse_data$measurements$datetime[1:2] <- NA
  }

  class(heat_pulse_data) <- c("heat_pulse_data", "list")
  return(heat_pulse_data)
}

test_that("validate_heat_pulse_data works with valid data", {
  heat_pulse_data <- create_mock_heat_pulse_data(valid = TRUE)

  result <- validate_heat_pulse_data(heat_pulse_data)

  expect_type(result, "list")
  expect_true(all(c("valid", "issues", "warnings", "summary") %in% names(result)))
  expect_true(result$valid)
  expect_equal(length(result$issues), 0)
})

test_that("validate_heat_pulse_data fails with wrong input type", {
  expect_error(validate_heat_pulse_data(data.frame(x = 1)), "Input must be a heat_pulse_data object")
})

test_that("validate_heat_pulse_data detects missing components", {
  heat_pulse_data <- create_mock_heat_pulse_data(missing_components = c("measurements"))

  result <- validate_heat_pulse_data(heat_pulse_data)

  expect_false(result$valid)
  expect_true(any(grepl("Missing required components", result$issues)))
})

test_that("validate_heat_pulse_data detects empty data", {
  heat_pulse_data <- create_mock_heat_pulse_data(empty_data = TRUE)

  result <- validate_heat_pulse_data(heat_pulse_data)

  expect_false(result$valid)
  expect_true(any(grepl("empty", result$issues)))
})

test_that("validate_heat_pulse_data warns about extreme values", {
  heat_pulse_data <- create_mock_heat_pulse_data(extreme_values = TRUE)

  result <- validate_heat_pulse_data(heat_pulse_data)

  # Should still be valid but have warnings
  expect_true(result$valid)
  expect_true(length(result$warnings) > 0)
  expect_true(any(grepl("outside reasonable", result$warnings)))
})

test_that("validate_heat_pulse_data detects missing timestamps", {
  heat_pulse_data <- create_mock_heat_pulse_data(missing_timestamps = TRUE)

  result <- validate_heat_pulse_data(heat_pulse_data)

  expect_false(result$valid)
  expect_true(any(grepl("Missing timestamps", result$issues)))
})

test_that("validate_structure detects missing columns", {
  heat_pulse_data <- create_mock_heat_pulse_data()
  # Remove required column
  heat_pulse_data$diagnostics$pulse_id <- NULL

  structure_result <- validate_structure(heat_pulse_data)

  expect_true(length(structure_result$issues) > 0)
  expect_true(any(grepl("Missing diagnostic columns", structure_result$issues)))
})

test_that("validate_ranges detects out-of-range values", {
  heat_pulse_data <- create_mock_heat_pulse_data()
  heat_pulse_data$measurements$do[1] <- -20  # Below reasonable range
  heat_pulse_data$diagnostics$batt_volt[1] <- 100  # Above reasonable range

  range_result <- validate_ranges(heat_pulse_data, temperature_range = c(-10, 60),
                                  voltage_range = c(0, 30))

  expect_true(length(range_result$warnings) >= 2)
  expect_true(any(grepl("outside reasonable temperature range", range_result$warnings)))
  expect_true(any(grepl("outside reasonable voltage range", range_result$warnings)))
})

test_that("validate_temporal_consistency detects timestamp issues", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Create duplicate timestamps
  heat_pulse_data$diagnostics$datetime[2] <- heat_pulse_data$diagnostics$datetime[1]

  # Create out-of-order timestamps
  heat_pulse_data$measurements$datetime[3] <- heat_pulse_data$measurements$datetime[1] - 3600

  temporal_result <- validate_temporal_consistency(heat_pulse_data)

  expect_true(length(temporal_result$warnings) > 0)
  expect_true(any(grepl("Duplicate timestamps", temporal_result$warnings)))
})

test_that("validate_sensor_consistency detects identical sensor readings", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Make sensors read identical values
  heat_pulse_data$measurements$do <- heat_pulse_data$measurements$di

  sensor_result <- validate_sensor_consistency(heat_pulse_data)

  expect_true(length(sensor_result$issues) > 0)
  expect_true(any(grepl("suspiciously similar readings", sensor_result$issues)))
})

test_that("validate_sensor_consistency detects excessive missing values", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Make most values missing
  heat_pulse_data$measurements$do[1:8] <- NA

  sensor_result <- validate_sensor_consistency(heat_pulse_data)

  expect_true(length(sensor_result$issues) > 0)
  expect_true(any(grepl("excessive missing values", sensor_result$issues)))
})

test_that("calculate_validation_summary provides correct statistics", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  summary_result <- calculate_validation_summary(heat_pulse_data, character(0), character(0))

  expect_type(summary_result, "list")
  expect_true(all(c("n_issues", "n_warnings", "n_diagnostics", "n_measurements") %in% names(summary_result)))
  expect_equal(summary_result$n_diagnostics, 3)
  expect_equal(summary_result$n_measurements, 9)
  expect_equal(summary_result$n_issues, 0)
  expect_equal(summary_result$n_warnings, 0)
})

test_that("validation works with custom parameters", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Set very strict temperature range
  result <- validate_heat_pulse_data(heat_pulse_data, temperature_range = c(0, 10))

  expect_true(length(result$warnings) > 0)
  expect_true(any(grepl("outside reasonable temperature range", result$warnings)))
})

test_that("strict validation converts warnings to issues", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Make sensors read identical values (normally a warning)
  heat_pulse_data$measurements$do <- heat_pulse_data$measurements$di

  # With strict validation, this should be an issue
  result_strict <- validate_heat_pulse_data(heat_pulse_data, strict_validation = TRUE)
  expect_false(result_strict$valid)

  # Without strict validation, this should still be valid
  result_normal <- validate_heat_pulse_data(heat_pulse_data, strict_validation = FALSE)
  expect_true(result_normal$valid)
})

test_that("data completeness is calculated correctly", {
  heat_pulse_data <- create_mock_heat_pulse_data()

  # Add some missing values
  heat_pulse_data$measurements$do[1:2] <- NA
  heat_pulse_data$measurements$di[1] <- NA

  result <- validate_heat_pulse_data(heat_pulse_data)

  expect_true("data_completeness" %in% names(result$summary))
  expect_true("overall_completeness" %in% names(result$summary))

  # do should be ~78% complete (7/9)
  expect_lt(result$summary$data_completeness["do"], 0.8)
  # di should be ~89% complete (8/9)
  expect_gt(result$summary$data_completeness["di"], 0.85)
})