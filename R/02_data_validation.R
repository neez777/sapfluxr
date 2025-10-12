#' Validate Sap Flow Data
#'
#' Performs comprehensive validation of imported sap flow data, checking for
#' data integrity, reasonable value ranges, and structural consistency.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param temperature_range Numeric vector of length 2 specifying min/max reasonable
#'   temperature values in Celsius (default: c(-10, 60))
#' @param voltage_range Numeric vector of length 2 specifying min/max reasonable
#'   voltage values (default: c(0, 30))
#' @param strict_validation Logical indicating whether to apply strict validation
#'   rules (default: FALSE)
#'
#' @return A list containing:
#'   \item{valid}{Logical indicating overall validation result}
#'   \item{issues}{Character vector of validation issues found}
#'   \item{warnings}{Character vector of warnings (non-critical issues)}
#'   \item{summary}{List with validation statistics}
#'
#' @examples
#' \dontrun{
#' sap_data <- read_sap_data("data.txt")
#' validation <- validate_sap_data(sap_data)
#' if (!validation$valid) {
#'   print(validation$issues)
#' }
#' }
#'
#' @export
validate_sap_data <- function(sap_data,
                              temperature_range = c(-10, 60),
                              voltage_range = c(0, 30),
                              strict_validation = FALSE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("Input must be a sap_data object")
  }

  issues <- character(0)
  warnings <- character(0)

  # Validate structure
  structure_check <- validate_structure(sap_data)
  issues <- c(issues, structure_check$issues)
  warnings <- c(warnings, structure_check$warnings)

  # Validate data ranges
  range_check <- validate_ranges(sap_data, temperature_range, voltage_range)
  issues <- c(issues, range_check$issues)
  warnings <- c(warnings, range_check$warnings)

  # Validate temporal consistency
  temporal_check <- validate_temporal_consistency(sap_data)
  issues <- c(issues, temporal_check$issues)
  warnings <- c(warnings, temporal_check$warnings)

  # Validate sensor consistency
  sensor_check <- validate_sensor_consistency(sap_data)
  if (strict_validation) {
    issues <- c(issues, sensor_check$issues)
  } else {
    warnings <- c(warnings, sensor_check$issues)
  }

  # Calculate validation summary
  summary_stats <- calculate_validation_summary(sap_data, issues, warnings)

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
#' @param sap_data A sap_data object
#' @return List with issues and warnings
#' @keywords internal
validate_structure <- function(sap_data) {

  issues <- character(0)
  warnings <- character(0)

  # Check required components
  required_components <- c("diagnostics", "measurements", "metadata")
  missing_components <- setdiff(required_components, names(sap_data))
  if (length(missing_components) > 0) {
    issues <- c(issues, paste("Missing required components:",
                              paste(missing_components, collapse = ", ")))
  }

  # Validate diagnostics structure
  if ("diagnostics" %in% names(sap_data)) {
    diag <- sap_data$diagnostics
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
  if ("measurements" %in% names(sap_data)) {
    meas <- sap_data$measurements
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
#' @param sap_data A sap_data object
#' @param temperature_range Range for temperature values
#' @param voltage_range Range for voltage values
#' @return List with issues and warnings
#' @keywords internal
validate_ranges <- function(sap_data, temperature_range, voltage_range) {

  issues <- character(0)
  warnings <- character(0)

  # Validate temperature ranges
  if ("measurements" %in% names(sap_data)) {
    meas <- sap_data$measurements
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
  if ("diagnostics" %in% names(sap_data)) {
    diag <- sap_data$diagnostics
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
#' @param sap_data A sap_data object
#' @return List with issues and warnings
#' @keywords internal
validate_temporal_consistency <- function(sap_data) {

  issues <- character(0)
  warnings <- character(0)

  # Check diagnostics timestamps
  if ("diagnostics" %in% names(sap_data)) {
    diag <- sap_data$diagnostics
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
  if ("measurements" %in% names(sap_data)) {
    meas <- sap_data$measurements
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
#' @param sap_data A sap_data object
#' @return List with issues and warnings
#' @keywords internal
validate_sensor_consistency <- function(sap_data) {

  issues <- character(0)
  warnings <- character(0)

  if ("measurements" %in% names(sap_data)) {
    meas <- sap_data$measurements
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

#' Calculate Validation Summary Statistics
#'
#' @param sap_data A sap_data object
#' @param issues Character vector of issues
#' @param warnings Character vector of warnings
#' @return List with summary statistics
#' @keywords internal
calculate_validation_summary <- function(sap_data, issues, warnings) {

  summary_stats <- list(
    n_issues = length(issues),
    n_warnings = length(warnings),
    n_diagnostics = if ("diagnostics" %in% names(sap_data)) nrow(sap_data$diagnostics) else 0,
    n_measurements = if ("measurements" %in% names(sap_data)) nrow(sap_data$measurements) else 0
  )

  # Add data quality metrics
  if ("measurements" %in% names(sap_data)) {
    meas <- sap_data$measurements
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(meas))

    if (length(temp_cols) > 0) {
      completeness <- sapply(temp_cols, function(col) {
        1 - sum(is.na(meas[[col]])) / nrow(meas)
      })
      summary_stats$data_completeness <- completeness
      summary_stats$overall_completeness <- mean(completeness)
    }
  }

  return(summary_stats)
}