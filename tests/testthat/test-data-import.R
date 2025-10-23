# tests/testthat/test-data-import.R
# Tests for data import functionality
# Source file: R/01_data_import.R

library(testthat)
library(sapfluxr)

# Helper function to create sample ICT current format data
create_sample_ict_current <- function(file_path, n_pulses = 3) {
  # Create malformed single-line JSON like the real files
  records <- character(n_pulses)

  for (i in seq_len(n_pulses)) {
    datetime <- format(Sys.time() + (i-1) * 1800, "%Y-%m-%dT%H:%M:%OSZ")

    # Create diagnostic values
    bv <- round(4 + runif(1, -0.2, 0.2), 2)
    bc <- round(15 + runif(1, -5, 5), 2)
    bt <- round(25 + runif(1, -5, 5), 2)
    ev <- round(21 + runif(1, -1, 1), 2)
    ec <- round(18 + runif(1, -2, 2), 2)

    # Create temperature measurements (30 seconds worth)
    temps <- character(30)
    for (j in 1:30) {
      do_val <- round(18.5 + runif(1, -0.5, 0.5), 3)
      di_val <- round(18.4 + runif(1, -0.5, 0.5), 3)
      uo_val <- round(18.6 + runif(1, -0.5, 0.5), 3)
      ui_val <- round(18.3 + runif(1, -0.5, 0.5), 3)

      temps[j] <- sprintf('{"do":%0.3f,"di":%0.3f,"uo":%0.3f,"ui":%0.3f}',
                          do_val, di_val, uo_val, ui_val)
    }

    records[i] <- sprintf('{"date":"%s","bv":%0.2f,"bc":%0.2f,"bt":%0.2f,"ep":1,"ev":%0.2f,"ec":%0.2f,%s',
                          datetime, bv, bc, bt, ev, ec, paste(temps, collapse = ","))
  }

  # Write as single line (malformed JSON)
  writeLines(paste0("[", paste(records, collapse = "},{"), "}]"), file_path, sep = "")
}

# Helper function to create sample CSV data
create_sample_csv <- function(file_path, n_rows = 100, delimiter = "\t") {
  data <- data.frame(
    Date = rep(Sys.Date(), n_rows),
    Time = format(Sys.time() + seq(0, n_rows-1), "%H:%M:%S"),
    Pulse_ID = rep(1:2, each = n_rows/2),
    DO = round(rnorm(n_rows, 18.5, 0.5), 3),
    DI = round(rnorm(n_rows, 18.4, 0.5), 3),
    UO = round(rnorm(n_rows, 18.6, 0.5), 3),
    UI = round(rnorm(n_rows, 18.3, 0.5), 3),
    Batt_V = round(runif(n_rows, 3.8, 4.2), 2),
    Ext_V = round(runif(n_rows, 20, 25), 2)
  )

  write.table(data, file_path, sep = delimiter, row.names = FALSE, quote = FALSE)
}

# Helper function to create sample legacy format data
create_sample_ict_legacy <- function(file_path) {
  # Create a simple legacy format with Rdg structure
  content <- '{"Rdg":[{"k":"date","v":"2024-03-15T12:30:01Z"},{"k":"bv","v":4.03}],
    "T":[{"k":"do","v":18.5},{"k":"di","v":18.4},{"k":"uo","v":18.6},{"k":"ui","v":18.3}]}'

  writeLines(content, file_path)
}

# Test format detection
test_that("detect_format correctly identifies formats", {

  # Test ICT current format
  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file), add = TRUE)

  create_sample_ict_current(temp_file)
  expect_equal(detect_format(temp_file), "ict_current")

  # Test CSV format
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)

  create_sample_csv(temp_csv)
  expect_equal(detect_format(temp_csv), "csv")

  # Test legacy format
  temp_legacy <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_legacy), add = TRUE)

  create_sample_ict_legacy(temp_legacy)
  expect_equal(detect_format(temp_legacy), "ict_legacy")

  # Test empty file
  temp_empty <- tempfile()
  on.exit(unlink(temp_empty), add = TRUE)
  writeLines("", temp_empty)

  expect_error(detect_format(temp_empty), "File appears to be empty")
})

# Test main read_heat_pulse_data function
test_that("read_heat_pulse_data works with ICT current format", {

  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))

  create_sample_ict_current(temp_file, n_pulses = 5)

  result <- read_heat_pulse_data(temp_file, show_progress = FALSE, validate_data = FALSE)

  expect_s3_class(result, "heat_pulse_data")
  expect_true(all(c("diagnostics", "measurements", "metadata") %in% names(result)))
  expect_equal(nrow(result$diagnostics), 5)
  expect_equal(nrow(result$measurements), 150) # 5 pulses * 30 measurements each
  expect_equal(result$metadata$format, "ict_current")
})

test_that("read_heat_pulse_data works with CSV format", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_csv(temp_file, n_rows = 100)

  result <- read_heat_pulse_data(temp_file, show_progress = FALSE, validate_data = FALSE)

  expect_s3_class(result, "heat_pulse_data")
  expect_equal(result$metadata$format, "csv")
  expect_equal(nrow(result$measurements), 100)
})

test_that("read_heat_pulse_data handles non-existent files", {
  expect_error(read_heat_pulse_data("nonexistent_file.txt"), "File not found")
})

# Skipping tests for internal parse_pulse_record function
# These test internal implementation details, not the public API

# Test large file handling
test_that("read_heat_pulse_data handles large single-line JSON files", {

  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))

  # Create a larger file with many pulses on one line
  create_sample_ict_current(temp_file, n_pulses = 100)

  result <- read_heat_pulse_data(temp_file, show_progress = FALSE, validate_data = FALSE)

  expect_s3_class(result, "heat_pulse_data")
  expect_equal(nrow(result$diagnostics), 100)
  expect_equal(nrow(result$measurements), 3000) # 100 pulses * 30 measurements
})

# Test progress and validation options
test_that("read_heat_pulse_data respects show_progress and validate_data options", {

  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))

  create_sample_ict_current(temp_file, n_pulses = 2)

  # Test with validation
  expect_silent({
    result_validated <- read_heat_pulse_data(temp_file,
                                      show_progress = FALSE,
                                      validate_data = TRUE)
  })
  expect_true("validation" %in% names(result_validated))

  # Test without validation
  result_no_validation <- read_heat_pulse_data(temp_file,
                                        show_progress = FALSE,
                                        validate_data = FALSE)
  expect_false("validation" %in% names(result_no_validation))
})

# Test metadata
test_that("read_heat_pulse_data generates correct metadata", {

  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))

  create_sample_ict_current(temp_file, n_pulses = 3)

  result <- read_heat_pulse_data(temp_file, show_progress = FALSE, validate_data = FALSE)

  expect_true(all(c("file_path", "file_name", "format", "import_time",
                    "file_size", "file_size_mb", "n_pulses", "n_measurements",
                    "chunk_size", "r_version", "package_version") %in%
                    names(result$metadata)))

  expect_equal(result$metadata$n_pulses, 3)
  expect_equal(result$metadata$n_measurements, 90)
  expect_equal(basename(result$metadata$file_path), basename(temp_file))
})

# Test empty dataframe creation
test_that("empty dataframe functions work correctly", {

  empty_diag <- create_empty_diagnostics()
  expect_s3_class(empty_diag, "data.frame")
  expect_equal(nrow(empty_diag), 0)
  expect_true(all(c("pulse_id", "datetime", "batt_volt", "batt_current",
                    "batt_temp", "external_volt", "external_current") %in%
                    names(empty_diag)))

  empty_meas <- create_empty_measurements()
  expect_s3_class(empty_meas, "data.frame")
  expect_equal(nrow(empty_meas), 0)
  expect_true(all(c("pulse_id", "datetime", "do", "di", "uo", "ui") %in%
                    names(empty_meas)))
})

# Test CSV delimiter detection
test_that("read_csv_format handles different delimiters", {

  # Test tab-delimited
  temp_tab <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_tab), add = TRUE)
  create_sample_csv(temp_tab, delimiter = "\t")

  result_tab <- read_csv_format(temp_tab, show_progress = FALSE)
  expect_equal(nrow(result_tab$measurements), 100)

  # Test comma-delimited
  temp_comma <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_comma), add = TRUE)
  create_sample_csv(temp_comma, delimiter = ",")

  result_comma <- read_csv_format(temp_comma, show_progress = FALSE)
  expect_equal(nrow(result_comma$measurements), 100)
})

# Skipping datetime parsing tests - internal function

# Test chunk size settings
test_that("chunk size is set appropriately based on file size", {

  temp_small <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_small), add = TRUE)
  create_sample_ict_current(temp_small, n_pulses = 1)

  result_small <- read_heat_pulse_data(temp_small, show_progress = FALSE, validate_data = FALSE)
  expect_true(result_small$metadata$chunk_size == 100000) # Small file default

  # Test custom chunk size
  result_custom <- read_heat_pulse_data(temp_small, chunk_size = 50000,
                                 show_progress = FALSE, validate_data = FALSE)
  expect_equal(result_custom$metadata$chunk_size, 50000)
})


# ---- Clock Drift Correction Tests ----

test_that("fix_clock_drift() corrects timestamps linearly", {
  # Create test data with known drift
  # Simulate 10 minutes of drift over 24 hours
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  n_points <- 48  # 30-min intervals over 24 hours

  # Create device times with linear drift
  true_times <- seq(start_time, by = "30 min", length.out = n_points)

  # Add linear drift: 0 at start, 10 minutes at end
  drift_minutes <- seq(0, 10, length.out = n_points)
  device_times <- true_times + drift_minutes * 60  # Convert to seconds

  test_data <- data.frame(
    datetime = device_times,
    temperature = rnorm(n_points, 25, 2)
  )

  # Calibration point: at end of dataset
  observed_device <- device_times[n_points]  # Device time at last point
  observed_actual <- true_times[n_points]    # Actual time at last point

  # Apply correction
  corrected <- fix_clock_drift(
    data = test_data,
    device_time_col = "datetime",
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  # Check that correction brings times close to true times
  # Allow small numerical tolerance
  max_error <- max(abs(as.numeric(corrected$datetime - true_times)))
  expect_lt(max_error, 1)  # Less than 1 second error

  # Check device_datetime column exists
  expect_true("device_datetime" %in% names(corrected))

  # Check original times preserved
  expect_equal(corrected$device_datetime, device_times)

  # Check attributes
  expect_true(attr(corrected$datetime, "drift_corrected"))
  expect_equal(attr(corrected$datetime, "total_drift_seconds"), -600)  # -10 minutes
  expect_equal(attr(corrected$datetime, "calibration_date"), observed_actual)
})


test_that("fix_clock_drift() handles positive drift (device slow)", {
  # Device is slow - shows earlier time than actual
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  device_times <- seq(start_time, by = "1 hour", length.out = 10)

  test_data <- data.frame(
    datetime = device_times,
    value = 1:10
  )

  # Device shows 17:00, actual is 18:00 (device is 1 hour slow)
  observed_device <- as.POSIXct("2025-01-15 17:00:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 18:00:00", tz = "UTC")

  corrected <- fix_clock_drift(
    test_data,
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  # Corrected times should be later than or equal to device times
  expect_true(all(corrected$datetime >= test_data$datetime))

  # First point should have minimal correction (at dataset start, correction is 0)
  expect_lt(
    abs(as.numeric(difftime(corrected$datetime[1], test_data$datetime[1], units = "secs"))),
    1  # Less than 1 second correction at start (essentially 0)
  )

  # Last point should have significant positive correction (closer to calibration)
  last_idx <- nrow(test_data)
  expect_gt(
    as.numeric(difftime(corrected$datetime[last_idx], test_data$datetime[last_idx], units = "secs")),
    3000  # More than 50 minutes correction near end
  )
})


test_that("fix_clock_drift() handles negative drift (device fast)", {
  # Device is fast - shows later time than actual
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  device_times <- seq(start_time, by = "1 hour", length.out = 10)

  test_data <- data.frame(
    datetime = device_times,
    value = 1:10
  )

  # Device shows 18:00, actual is 17:00 (device is 1 hour fast)
  observed_device <- as.POSIXct("2025-01-15 18:00:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 17:00:00", tz = "UTC")

  corrected <- fix_clock_drift(
    test_data,
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  # Corrected times should be earlier than or equal to device times
  expect_true(all(corrected$datetime <= test_data$datetime))

  # First point should have minimal correction (at dataset start, correction is 0)
  expect_lt(
    abs(as.numeric(difftime(corrected$datetime[1], test_data$datetime[1], units = "secs"))),
    1  # Less than 1 second correction at start (essentially 0)
  )

  # Last point should have significant negative correction (closer to calibration)
  last_idx <- nrow(test_data)
  expect_lt(
    as.numeric(difftime(corrected$datetime[last_idx], test_data$datetime[last_idx], units = "secs")),
    -3000  # More than 50 minutes negative correction near end
  )
})


test_that("fix_clock_drift() detects already-corrected data", {
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  test_data <- data.frame(
    datetime = seq(start_time, by = "30 min", length.out = 10),
    value = 1:10
  )

  observed_device <- as.POSIXct("2025-01-15 12:30:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 12:20:00", tz = "UTC")

  # Apply correction first time
  corrected_once <- fix_clock_drift(
    test_data,
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  # Try to apply again - should warn and return unchanged
  expect_warning(
    corrected_twice <- fix_clock_drift(
      corrected_once,
      observed_device_time = observed_device,
      observed_actual_time = observed_actual
    ),
    "Clock drift correction appears to have already been applied"
  )

  # Data should be unchanged
  expect_equal(corrected_twice, corrected_once)
})


test_that("fix_clock_drift() warns about extrapolation", {
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  test_data <- data.frame(
    datetime = seq(start_time, by = "30 min", length.out = 20),
    value = 1:20
  )

  # Calibration point is in the middle of the dataset
  observed_device <- as.POSIXct("2025-01-15 12:00:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 11:50:00", tz = "UTC")

  # Should warn about extrapolation beyond calibration point
  expect_warning(
    corrected <- fix_clock_drift(
      test_data,
      observed_device_time = observed_device,
      observed_actual_time = observed_actual
    ),
    "extrapolated beyond the calibration point"
  )

  # But should still return corrected data
  expect_true("device_datetime" %in% names(corrected))
})


test_that("fix_clock_drift() validates inputs", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2025-01-15 08:00:00", tz = "UTC"),
                   by = "30 min", length.out = 10),
    value = 1:10
  )

  observed_device <- as.POSIXct("2025-01-15 12:30:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 12:20:00", tz = "UTC")

  # Test non-data.frame input
  expect_error(
    fix_clock_drift(
      data = as.list(test_data),
      observed_device_time = observed_device,
      observed_actual_time = observed_actual
    ),
    "'data' must be a data frame"
  )

  # Test invalid column name
  expect_error(
    fix_clock_drift(
      data = test_data,
      device_time_col = "nonexistent",
      observed_device_time = observed_device,
      observed_actual_time = observed_actual
    ),
    "Column 'nonexistent' not found"
  )

  # Test non-POSIXct observed times
  expect_error(
    fix_clock_drift(
      data = test_data,
      observed_device_time = "2025-01-15 12:30:00",
      observed_actual_time = observed_actual
    ),
    "'observed_device_time' must be a POSIXct object"
  )

  expect_error(
    fix_clock_drift(
      data = test_data,
      observed_device_time = observed_device,
      observed_actual_time = "2025-01-15 12:20:00"
    ),
    "'observed_actual_time' must be a POSIXct object"
  )
})


test_that("fix_clock_drift() handles calibration before dataset start", {
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  test_data <- data.frame(
    datetime = seq(start_time, by = "30 min", length.out = 10),
    value = 1:10
  )

  # Calibration point before dataset start
  observed_device <- as.POSIXct("2025-01-15 07:00:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 06:50:00", tz = "UTC")

  expect_error(
    fix_clock_drift(
      test_data,
      observed_device_time = observed_device,
      observed_actual_time = observed_actual
    ),
    "Calibration point must be after the dataset start time"
  )
})


test_that("fix_clock_drift() works with custom column names", {
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  test_data <- data.frame(
    timestamp = seq(start_time, by = "30 min", length.out = 10),
    value = 1:10
  )

  observed_device <- as.POSIXct("2025-01-15 12:30:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 12:20:00", tz = "UTC")

  corrected <- fix_clock_drift(
    test_data,
    device_time_col = "timestamp",
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  expect_true("device_datetime" %in% names(corrected))
  expect_true(attr(corrected$timestamp, "drift_corrected"))
})


test_that("fix_clock_drift() converts character times to POSIXct", {
  start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
  test_data <- data.frame(
    datetime = format(seq(start_time, by = "30 min", length.out = 10)),
    value = 1:10
  )

  observed_device <- as.POSIXct("2025-01-15 12:30:00", tz = "UTC")
  observed_actual <- as.POSIXct("2025-01-15 12:20:00", tz = "UTC")

  corrected <- fix_clock_drift(
    test_data,
    observed_device_time = observed_device,
    observed_actual_time = observed_actual
  )

  expect_s3_class(corrected$datetime, "POSIXct")
  expect_true("device_datetime" %in% names(corrected))
})
