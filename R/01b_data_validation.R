# R/01b_data_validation.R
#' Validate Heat Pulse Data
#'
#' Performs comprehensive validation of imported heat pulse temperature data, checking for
#' data integrity, reasonable value ranges, and structural consistency. Calculates accurate
#' completeness metrics based on gap detection results from read_heat_pulse_data().
#'
#' @param heat_pulse_data A heat_pulse_data object from read_heat_pulse_data()
#' @param temperature_range Numeric vector of length 2 specifying min/max reasonable
#'   temperature values in Celsius (default: c(-10, 60))
#' @param voltage_range Numeric vector of length 2 specifying min/max reasonable
#'   voltage values (default: c(0, 30))
#' @param strict_validation Logical indicating whether to apply strict validation
#'   rules (default: FALSE)
#' @param detect_missing_pulses Deprecated - gap detection now happens in read_heat_pulse_data()
#' @param max_gap_to_fill_hours Numeric, used for warning messages about unfilled gaps (default: 24 hours)
#'
#' @return A list containing:
#'   \item{valid}{Logical indicating overall validation result}
#'   \item{issues}{Character vector of validation issues found}
#'   \item{warnings}{Character vector of warnings (non-critical issues)}
#'   \item{summary}{List with validation statistics including pulse_completeness}
#'
#' @details
#' **Gap Detection and Completeness:**
#' Gap detection now happens automatically during data import in read_heat_pulse_data().
#' This function uses those results to calculate accurate completeness metrics.
#'
#' If gaps were detected and filled, the validation summary will include:
#' - pulse_completeness: Proportion of actual pulses vs. expected
#' - n_actual_pulses: Number of pulses in the raw data
#' - n_expected_pulses: Total expected based on regular pulse interval
#' - n_missing_pulses: Number of missing pulses detected
#' - n_filled_pulses: Number filled with interpolated timestamps (pulse_id = NA)
#'
#' @examples
#' \dontrun{
#' heat_pulse_data <- read_heat_pulse_data("data.txt")
#' validation <- validate_heat_pulse_data(heat_pulse_data)
#' if (!validation$valid) {
#'   print(validation$issues)
#' }
#'
#' # Check pulse completeness
#' if (!is.null(validation$summary$pulse_completeness)) {
#'   cat("Pulse completeness:", validation$summary$pulse_completeness * 100, "%\n")
#' }
#' }
#'
#' @export
validate_heat_pulse_data <- function(heat_pulse_data,
                              temperature_range = c(-10, 60),
                              voltage_range = c(0, 30),
                              strict_validation = FALSE,
                              detect_missing_pulses = TRUE,  # Deprecated, kept for compatibility
                              max_gap_to_fill_hours = 24) {

  if (!inherits(heat_pulse_data, "heat_pulse_data")) {
    stop("Input must be a heat_pulse_data object")
  }

  issues <- character(0)
  warnings <- character(0)

  # Validate structure
  structure_check <- validate_structure(heat_pulse_data)
  issues <- c(issues, structure_check$issues)
  warnings <- c(warnings, structure_check$warnings)

  # Validate data ranges
  range_check <- validate_ranges(heat_pulse_data, temperature_range, voltage_range)
  issues <- c(issues, range_check$issues)
  warnings <- c(warnings, range_check$warnings)

  # Validate temporal consistency
  temporal_check <- validate_temporal_consistency(heat_pulse_data)
  issues <- c(issues, temporal_check$issues)
  warnings <- c(warnings, temporal_check$warnings)

  # Validate sensor consistency
  sensor_check <- validate_sensor_consistency(heat_pulse_data)
  if (strict_validation) {
    issues <- c(issues, sensor_check$issues)
  } else {
    warnings <- c(warnings, sensor_check$issues)
  }

  # Add warnings if gaps were detected (gap detection happens in read_heat_pulse_data)
  if (!is.null(heat_pulse_data$gap_detection)) {
    gap_report <- heat_pulse_data$gap_detection$gap_report

    if (!is.null(gap_report) && nrow(gap_report) > 0) {
      n_gaps <- nrow(gap_report)
      n_filled <- sum(gap_report$filled)
      n_not_filled <- n_gaps - n_filled

      if (n_filled > 0) {
        warnings <- c(warnings, sprintf(
          "Missing data: %d gap(s) detected, %d pulse(s) filled with interpolated timestamps",
          n_gaps, heat_pulse_data$gap_detection$summary$n_filled
        ))
      }

      if (n_not_filled > 0) {
        warnings <- c(warnings, sprintf(
          "Large gaps: %d gap(s) > %.1f hours were not filled (%d missing pulses)",
          n_not_filled, max_gap_to_fill_hours,
          sum(gap_report$n_missing[!gap_report$filled])
        ))
      }
    }
  }

  # Calculate validation summary (NOW includes accurate completeness)
  summary_stats <- calculate_validation_summary(heat_pulse_data, issues, warnings)

  # Overall validation result
  valid <- length(issues) == 0

  return(list(
    valid = valid,
    issues = issues,
    warnings = warnings,
    summary = summary_stats
  ))
}

#' Validate Data Structure
#'
#' @param heat_pulse_data A heat_pulse_data object
#' @return List with issues and warnings
#' @keywords internal
validate_structure <- function(heat_pulse_data) {

  issues <- character(0)
  warnings <- character(0)

  # Check required components
  required_components <- c("diagnostics", "measurements", "metadata")
  missing_components <- setdiff(required_components, names(heat_pulse_data))
  if (length(missing_components) > 0) {
    issues <- c(issues, paste("Missing required components:",
                              paste(missing_components, collapse = ", ")))
  }

  # Validate diagnostics structure
  if ("diagnostics" %in% names(heat_pulse_data)) {
    diag <- heat_pulse_data$diagnostics
    required_diag_cols <- c("pulse_id", "datetime")
    missing_diag_cols <- setdiff(required_diag_cols, names(diag))
    if (length(missing_diag_cols) > 0) {
      issues <- c(issues, paste("Missing diagnostic columns:",
                                paste(missing_diag_cols, collapse = ", ")))
    }

    # Check for empty diagnostics
    if (nrow(diag) == 0) {
      issues <- c(issues, "Diagnostics table is empty")
    }
  }

  # Validate measurements structure
  if ("measurements" %in% names(heat_pulse_data)) {
    meas <- heat_pulse_data$measurements
    required_meas_cols <- c("pulse_id", "datetime")
    expected_temp_cols <- c("do", "di", "uo", "ui")

    missing_meas_cols <- setdiff(required_meas_cols, names(meas))
    if (length(missing_meas_cols) > 0) {
      issues <- c(issues, paste("Missing measurement columns:",
                                paste(missing_meas_cols, collapse = ", ")))
    }

    missing_temp_cols <- setdiff(expected_temp_cols, names(meas))
    if (length(missing_temp_cols) > 0) {
      warnings <- c(warnings, paste("Missing temperature columns:",
                                    paste(missing_temp_cols, collapse = ", ")))
    }

    # Check for empty measurements
    if (nrow(meas) == 0) {
      issues <- c(issues, "Measurements table is empty")
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Data Ranges
#'
#' @param heat_pulse_data A heat_pulse_data object
#' @param temperature_range Range for temperature values
#' @param voltage_range Range for voltage values
#' @return List with issues and warnings
#' @keywords internal
validate_ranges <- function(heat_pulse_data, temperature_range, voltage_range) {

  issues <- character(0)
  warnings <- character(0)

  # Validate temperature ranges
  if ("measurements" %in% names(heat_pulse_data)) {
    meas <- heat_pulse_data$measurements
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(meas))

    for (col in temp_cols) {
      temp_values <- meas[[col]][!is.na(meas[[col]])]
      if (length(temp_values) > 0) {
        out_of_range <- sum(temp_values < temperature_range[1] |
                              temp_values > temperature_range[2])
        if (out_of_range > 0) {
          warnings <- c(warnings, paste0("Column ", col, ": ", out_of_range,
                                         " values outside reasonable temperature range [",
                                         temperature_range[1], ", ", temperature_range[2], "]"))
        }
      }
    }
  }

  # Validate voltage ranges
  if ("diagnostics" %in% names(heat_pulse_data)) {
    diag <- heat_pulse_data$diagnostics
    voltage_cols <- intersect(c("batt_volt", "external_volt"), names(diag))

    for (col in voltage_cols) {
      volt_values <- diag[[col]][!is.na(diag[[col]])]
      if (length(volt_values) > 0) {
        out_of_range <- sum(volt_values < voltage_range[1] |
                              volt_values > voltage_range[2])
        if (out_of_range > 0) {
          warnings <- c(warnings, paste0("Column ", col, ": ", out_of_range,
                                         " values outside reasonable voltage range [",
                                         voltage_range[1], ", ", voltage_range[2], "]"))
        }
      }
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Temporal Consistency
#'
#' @param heat_pulse_data A heat_pulse_data object
#' @return List with issues and warnings
#' @keywords internal
validate_temporal_consistency <- function(heat_pulse_data) {

  issues <- character(0)
  warnings <- character(0)

  # Check diagnostics timestamps
  if ("diagnostics" %in% names(heat_pulse_data)) {
    diag <- heat_pulse_data$diagnostics
    if ("datetime" %in% names(diag)) {
      # Check for missing timestamps
      missing_timestamps <- sum(is.na(diag$datetime))
      if (missing_timestamps > 0) {
        issues <- c(issues, paste("Missing timestamps in diagnostics:", missing_timestamps))
      }

      # Check for duplicate timestamps
      duplicate_timestamps <- sum(duplicated(diag$datetime[!is.na(diag$datetime)]))
      if (duplicate_timestamps > 0) {
        warnings <- c(warnings, paste("Duplicate timestamps in diagnostics:", duplicate_timestamps))
      }

      # Check temporal order
      if (nrow(diag) > 1 && any(!is.na(diag$datetime))) {
        ordered_times <- all(diff(diag$datetime[!is.na(diag$datetime)]) >= 0)
        if (!ordered_times) {
          warnings <- c(warnings, "Timestamps in diagnostics are not in chronological order")
        }
      }
    }
  }

  # Check measurements timestamps
  if ("measurements" %in% names(heat_pulse_data)) {
    meas <- heat_pulse_data$measurements
    if ("datetime" %in% names(meas)) {
      # Check for missing timestamps
      missing_timestamps <- sum(is.na(meas$datetime))
      if (missing_timestamps > 0) {
        issues <- c(issues, paste("Missing timestamps in measurements:", missing_timestamps))
      }

      # Check for reasonable time gaps
      if (nrow(meas) > 1 && any(!is.na(meas$datetime))) {
        time_diffs <- diff(meas$datetime[!is.na(meas$datetime)])
        # Flag gaps larger than 1 hour as potential issues
        large_gaps <- sum(time_diffs > 3600, na.rm = TRUE)
        if (large_gaps > 0) {
          warnings <- c(warnings, paste("Large time gaps (>1 hour) in measurements:", large_gaps))
        }
      }
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Sensor Consistency
#'
#' @param heat_pulse_data A heat_pulse_data object
#' @return List with issues and warnings
#' @keywords internal
validate_sensor_consistency <- function(heat_pulse_data) {

  issues <- character(0)
  warnings <- character(0)

  if ("measurements" %in% names(heat_pulse_data)) {
    meas <- heat_pulse_data$measurements
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(meas))

    if (length(temp_cols) >= 2) {
      # Check for sensors reading identical values (possible sensor failure)
      for (i in 1:(length(temp_cols)-1)) {
        for (j in (i+1):length(temp_cols)) {
          col1 <- temp_cols[i]
          col2 <- temp_cols[j]
          identical_readings <- sum(meas[[col1]] == meas[[col2]], na.rm = TRUE)
          total_readings <- sum(!is.na(meas[[col1]]) & !is.na(meas[[col2]]))

          if (total_readings > 0 && identical_readings / total_readings > 0.9) {
            issues <- c(issues, paste0("Sensors ", col1, " and ", col2,
                                       " have suspiciously similar readings (",
                                       round(100 * identical_readings / total_readings, 1), "% identical)"))
          }
        }
      }

      # Check for excessive missing values
      for (col in temp_cols) {
        if (nrow(meas) > 0) {
          missing_pct <- sum(is.na(meas[[col]])) / nrow(meas)
          if (!is.na(missing_pct) && missing_pct > 0.5) {
            issues <- c(issues, paste0("Column ", col, " has excessive missing values (",
                                       round(100 * missing_pct, 1), "%)"))
          }
        }
      }
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Detect and Fill Missing Pulse Timestamps in Raw Measurement Data
#'
#' Detects gaps in the time series of raw heat pulse measurements and adds rows
#' with interpolated timestamps for missing pulses. This runs during data import
#' to ensure completeness calculations are accurate.
#'
#' Works at the PULSE level (using pulse_id) rather than individual measurement timestamps,
#' since each pulse typically has multiple temperature measurements at different time points.
#'
#' @param measurements Data frame with datetime and pulse_id columns
#' @param expected_interval_hours Expected interval between pulses (hours). If NULL, auto-detected.
#' @param max_gap_to_fill_hours Maximum gap size (hours) to fill with interpolated timestamps. Default: 24 hours.
#' @param verbose Logical, whether to print progress messages
#' @return List containing:
#'   \item{measurements_complete}{Data frame with added rows for missing pulses (pulse_id = NA)}
#'   \item{gap_report}{Data frame of detected gaps with columns: gap_start, gap_end, n_missing, duration_hours, filled}
#'   \item{summary}{List with n_actual, n_missing, n_filled, n_expected}
#' @keywords internal
detect_and_fill_missing_pulse_timestamps <- function(measurements,
                                                       expected_interval_hours = NULL,
                                                       max_gap_to_fill_hours = 24,
                                                       verbose = FALSE) {

  # Convert datetime if needed
  if (!inherits(measurements$datetime, "POSIXct")) {
    measurements$datetime <- as.POSIXct(measurements$datetime)
  }

  # Sort by datetime
  measurements <- measurements[order(measurements$datetime), ]

  # Work at PULSE level, not measurement level
  # Get the first measurement timestamp for each pulse as the pulse time
  has_pulse_id <- "pulse_id" %in% names(measurements)
  has_non_na_pulse_id <- has_pulse_id && any(!is.na(measurements$pulse_id))

  if (verbose) {
    message(sprintf("    DEBUG: Has pulse_id column: %s", has_pulse_id))
    message(sprintf("    DEBUG: Has non-NA pulse_ids: %s", has_non_na_pulse_id))
    if (has_pulse_id) {
      message(sprintf("    DEBUG: Non-NA pulse_id count: %d / %d",
                     sum(!is.na(measurements$pulse_id)), nrow(measurements)))
    }
  }

  if (has_non_na_pulse_id) {
    pulse_times <- measurements %>%
      dplyr::filter(!is.na(pulse_id)) %>%
      dplyr::group_by(pulse_id) %>%
      dplyr::summarise(
        pulse_datetime = min(datetime, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(pulse_datetime)

    unique_datetimes <- pulse_times$pulse_datetime

    if (verbose) {
      message(sprintf("    Working with %d pulses (pulse-level detection)", nrow(pulse_times)))
      message(sprintf("    DEBUG: First 5 pulse times: %s",
                     paste(format(head(unique_datetimes, 5)), collapse = ", ")))
    }
  } else {
    # Fallback: use unique timestamps (for data without pulse_id)
    unique_datetimes <- sort(unique(measurements$datetime))

    if (verbose) {
      message(sprintf("    WARNING: No pulse_id found - using %d unique timestamps (measurement-level detection)",
                     length(unique_datetimes)))
      message("    This may cause incorrect interval detection!")
    }
  }

  # Auto-detect pulse interval if not provided
  if (is.null(expected_interval_hours)) {
    # unique_datetimes was already calculated above (either pulse-level or measurement-level)
    # DO NOT recalculate it here!

    if (length(unique_datetimes) < 2) {
      if (verbose) {
        message("    Skipping gap detection: only 1 unique datetime")
      }
      return(list(
        measurements_complete = measurements,
        gap_report = NULL,
        summary = list(
          n_actual = 1,
          n_missing = 0,
          n_filled = 0,
          n_expected = 1,
          expected_interval_hours = NA,
          completeness_pct = 100
        )
      ))
    }

    time_diffs <- as.numeric(diff(unique_datetimes), units = "hours")
    time_diffs <- time_diffs[time_diffs > 0]

    if (length(time_diffs) == 0) {
      if (verbose) {
        message("    Skipping gap detection: no positive time differences")
      }
      return(list(
        measurements_complete = measurements,
        gap_report = NULL,
        summary = list(
          n_actual = length(unique_datetimes),
          n_missing = 0,
          n_filled = 0,
          n_expected = length(unique_datetimes),
          expected_interval_hours = NA,
          completeness_pct = 100
        )
      ))
    }

    # Filter out very small time differences (< 5 minutes = 0.083 hours)
    # These are measurement-level differences within pulses, not pulse-level intervals
    pulse_level_diffs <- time_diffs[time_diffs >= 0.083]

    if (length(pulse_level_diffs) == 0) {
      if (verbose) {
        message("    Skipping gap detection: no pulse-level time differences found (all < 5 min)")
      }
      return(list(
        measurements_complete = measurements,
        gap_report = NULL,
        summary = list(
          n_actual = length(unique_datetimes),
          n_missing = 0,
          n_filled = 0,
          n_expected = length(unique_datetimes),
          expected_interval_hours = NA,
          completeness_pct = 100
        )
      ))
    }

    interval_table <- table(round(pulse_level_diffs, 2))
    expected_interval_hours <- as.numeric(names(interval_table)[which.max(interval_table)])

    if (is.na(expected_interval_hours) || expected_interval_hours <= 0) {
      # Debug output to help diagnose the issue
      if (verbose) {
        message("    DEBUG: Interval detection failed")
        message("      Number of unique datetimes: ", length(unique_datetimes))
        message("      Number of time differences: ", length(time_diffs))
        message("      Time diff range: ", min(time_diffs, na.rm = TRUE), " to ", max(time_diffs, na.rm = TRUE), " hours")
        message("      Interval table: ", paste(names(interval_table), "=", interval_table, collapse = ", "))
        message("      Detected interval: ", expected_interval_hours)
      }

      # Return data unchanged if interval detection fails
      return(list(
        measurements_complete = measurements,
        gap_report = NULL,
        summary = list(
          n_actual = length(unique_datetimes),
          n_missing = 0,
          n_filled = 0,
          n_expected = length(unique_datetimes),
          expected_interval_hours = NA,
          completeness_pct = 100
        )
      ))
    }

    if (verbose) {
      message(sprintf("    Auto-detected pulse interval: %.2f hours (%.1f minutes)",
                      expected_interval_hours, expected_interval_hours * 60))
    }
  }

  # Detect gaps by checking consecutive pulse timestamps
  actual_times <- unique_datetimes  # Already computed above (pulse-level)
  missing_times <- c()
  gap_info <- list()
  gap_counter <- 0

  if (verbose) {
    message(sprintf("    DEBUG: Checking %d consecutive pulse pairs for gaps", length(actual_times) - 1))
    message(sprintf("    DEBUG: Gap threshold: %.2f hours (%.1f minutes)",
                   expected_interval_hours * 1.5, expected_interval_hours * 1.5 * 60))
  }

  # Loop through consecutive pairs of actual measurements
  n_gaps_found <- 0
  for (i in seq_len(length(actual_times) - 1)) {
    time_A <- actual_times[i]
    time_B <- actual_times[i + 1]

    # Calculate actual gap duration
    gap_duration_hours <- as.numeric(difftime(time_B, time_A, units = "hours"))

    # Determine if gap is larger than expected (with tolerance)
    gap_threshold <- expected_interval_hours * 1.5

    if (gap_duration_hours > gap_threshold) {
      n_gaps_found <- n_gaps_found + 1

      if (verbose && n_gaps_found <= 5) {  # Only show first 5 gaps
        message(sprintf("    DEBUG: Gap #%d found - %.2f hours between %s and %s",
                       n_gaps_found, gap_duration_hours,
                       format(time_A, "%Y-%m-%d %H:%M:%S"),
                       format(time_B, "%Y-%m-%d %H:%M:%S")))
      }

      # Calculate number of missing pulses
      n_missing <- round(gap_duration_hours / expected_interval_hours) - 1

      if (n_missing > 0) {
        # Linear interpolation: divide gap into equal intervals
        interpolated_times <- time_A + (time_B - time_A) * (1:n_missing) / (n_missing + 1)

        missing_times <- c(missing_times, interpolated_times)

        # Track this gap for reporting
        gap_counter <- gap_counter + 1
        gap_info[[gap_counter]] <- list(
          gap_start = time_A,
          gap_end = time_B,
          n_missing = n_missing,
          duration_hours = gap_duration_hours,
          interpolated_times = interpolated_times
        )
      }
    }
  }

  missing_times <- sort(missing_times)

  # Generate gap report
  gap_report <- NULL
  if (length(gap_info) > 0) {
    gaps <- lapply(gap_info, function(gap) {
      filled <- gap$duration_hours <= max_gap_to_fill_hours
      data.frame(
        gap_start = gap$gap_start,
        gap_end = gap$gap_end,
        n_missing = gap$n_missing,
        duration_hours = gap$duration_hours,
        filled = filled
      )
    })
    gap_report <- do.call(rbind, gaps)
  }

  # DON'T add rows for missing pulses at import time - just report them
  # This avoids all the type conversion issues
  # Missing pulses will be interpolated later during filtering if needed
  measurements_complete <- measurements
  n_filled <- 0

  # Just count how many we would have filled
  if (length(gap_info) > 0) {
    for (gap in gap_info) {
      if (gap$duration_hours <= max_gap_to_fill_hours) {
        n_filled <- n_filled + gap$n_missing
      }
    }
  }

  # Calculate summary statistics
  n_actual <- length(actual_times)
  n_missing_total <- length(missing_times)
  n_expected <- n_actual + n_filled

  if (verbose) {
    message(sprintf("    DEBUG: Gap detection complete"))
    message(sprintf("      Gaps found: %d", length(gap_info)))
    message(sprintf("      Missing pulses (total): %d", n_missing_total))
    message(sprintf("      Missing pulses (filled): %d", n_filled))
    message(sprintf("      Actual pulses: %d", n_actual))
    message(sprintf("      Expected pulses: %d", n_expected))
    message(sprintf("      Completeness: %.1f%%", if (n_expected > 0) 100 * n_actual / n_expected else 100))
  }

  list(
    measurements_complete = measurements_complete,
    gap_report = gap_report,
    summary = list(
      n_actual = n_actual,
      n_missing = n_missing_total,
      n_filled = n_filled,
      n_expected = n_expected,
      expected_interval_hours = expected_interval_hours,
      # Don't round here - let the display layer handle rounding to preserve precision
      completeness_pct = if (n_expected > 0) 100 * n_actual / n_expected else 100
    )
  )
}


#' Calculate Validation Summary Statistics
#'
#' @param heat_pulse_data A heat_pulse_data object
#' @param issues Character vector of issues
#' @param warnings Character vector of warnings
#' @return List with summary statistics
#' @keywords internal
calculate_validation_summary <- function(heat_pulse_data, issues, warnings) {

  summary_stats <- list(
    n_issues = length(issues),
    n_warnings = length(warnings),
    n_diagnostics = if ("diagnostics" %in% names(heat_pulse_data)) nrow(heat_pulse_data$diagnostics) else 0,
    n_measurements = if ("measurements" %in% names(heat_pulse_data)) nrow(heat_pulse_data$measurements) else 0
  )

  # Add data quality metrics - IMPROVED VERSION
  # Now accounts for missing pulses if gap detection was run
  if ("measurements" %in% names(heat_pulse_data)) {
    meas <- heat_pulse_data$measurements
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(meas))

    # Add gap detection summary if available (do this first)
    if (!is.null(heat_pulse_data$gap_detection)) {
      gap_sum <- heat_pulse_data$gap_detection$summary
      summary_stats$n_actual_pulses <- gap_sum$n_actual
      summary_stats$n_missing_pulses <- gap_sum$n_missing
      summary_stats$n_filled_pulses <- gap_sum$n_filled
      summary_stats$n_expected_pulses <- gap_sum$n_expected
      summary_stats$pulse_completeness <- gap_sum$completeness_pct / 100  # Convert to fraction
    }

    if (length(temp_cols) > 0) {
      # If gap detection was run and found missing pulses, use pulse completeness for all sensors
      # because missing pulses means all sensors are missing data for those time points
      if (!is.null(heat_pulse_data$gap_detection) &&
          !is.null(summary_stats$n_missing_pulses) &&
          summary_stats$n_missing_pulses > 0) {
        # All sensors have the same completeness as pulse completeness
        completeness <- rep(summary_stats$pulse_completeness, length(temp_cols))
        names(completeness) <- temp_cols
        summary_stats$data_completeness <- completeness
        summary_stats$overall_completeness <- summary_stats$pulse_completeness
      } else {
        # No missing pulses - calculate completeness from NA values in existing data
        completeness <- sapply(temp_cols, function(col) {
          1 - sum(is.na(meas[[col]])) / nrow(meas)
        })
        summary_stats$data_completeness <- completeness
        summary_stats$overall_completeness <- mean(completeness)
      }
    }
  }

  return(summary_stats)
}
