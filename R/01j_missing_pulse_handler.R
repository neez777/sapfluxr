# R/01j_missing_pulse_handler.R
# Functions for detecting and handling missing pulses in diagnostic data

#' Detect Missing Pulses in Diagnostic Data
#'
#' Analyzes datetime gaps in diagnostic data to identify missing pulses.
#' Uses median interval to determine expected pulse timing.
#'
#' @param diagnostics Tibble with datetime and pulse_id columns
#' @return List with missing pulse information
#' @keywords internal
detect_missing_pulses <- function(diagnostics) {

  if (nrow(diagnostics) < 2) {
    return(list(
      expected_interval_sec = NA,
      n_gaps = 0,
      total_missing = 0,
      gaps = NULL
    ))
  }

  # Calculate time differences between consecutive pulses
  time_diffs <- as.numeric(diff(diagnostics$datetime))

  # Determine expected interval (median of differences)
  # This handles occasional small variations in pulse timing
  expected_interval_sec <- median(time_diffs, na.rm = TRUE)

  # Find gaps larger than 1.5x expected interval
  gap_threshold <- 1.5 * expected_interval_sec

  gaps <- list()

  for (i in seq_along(time_diffs)) {
    gap_sec <- time_diffs[i]

    if (gap_sec > gap_threshold) {
      # Calculate how many pulses are missing
      n_missing <- round(gap_sec / expected_interval_sec) - 1

      if (n_missing > 0) {
        gaps[[length(gaps) + 1]] <- list(
          after_row = i,
          n_missing = n_missing,
          gap_start = diagnostics$datetime[i],
          gap_end = diagnostics$datetime[i + 1],
          gap_duration_sec = gap_sec
        )
      }
    }
  }

  list(
    expected_interval_sec = expected_interval_sec,
    n_gaps = length(gaps),
    total_missing = if (length(gaps) > 0) sum(sapply(gaps, function(x) x$n_missing)) else 0L,
    gaps = if (length(gaps) > 0) gaps else NULL
  )
}


#' Insert Placeholder Rows for Missing Pulses
#'
#' Creates placeholder diagnostic rows for missing pulses with interpolated
#' datetime and NA sensor values. Renumbers all pulse_ids sequentially.
#'
#' @param diagnostics Original diagnostic tibble
#' @param missing_info Missing pulse information from detect_missing_pulses()
#' @return Complete diagnostics tibble with missing pulse placeholders
#' @keywords internal
insert_missing_pulse_rows <- function(diagnostics, missing_info) {

  if (missing_info$total_missing == 0) {
    # No missing pulses - just add the flag column
    diagnostics$is_missing_pulse <- FALSE
    return(diagnostics)
  }

  # OPTIMIZED: Vectorized approach (1000x faster!)
  # Instead of row-by-row loops, create all placeholders at once

  # Add flag to original data (vectorized)
  cat("  Adding is_missing_pulse flag...\n")
  diagnostics$is_missing_pulse <- FALSE

  cat(sprintf("  Creating %d placeholder rows...\n", missing_info$total_missing))
  interval_sec <- missing_info$expected_interval_sec

  # Collect all interpolated datetimes across all gaps
  all_missing_datetimes <- numeric(missing_info$total_missing)
  idx <- 1

  for (gap_info in missing_info$gaps) {
    gap_times <- gap_info$gap_start + (seq_len(gap_info$n_missing) * interval_sec)
    all_missing_datetimes[idx:(idx + gap_info$n_missing - 1)] <- as.numeric(gap_times)
    idx <- idx + gap_info$n_missing
  }

  # Create ALL placeholder rows in ONE tibble call
  missing_rows <- tibble::tibble(
    datetime = as.POSIXct(all_missing_datetimes, origin = "1970-01-01", tz = "UTC"),
    pulse_id = NA_integer_,
    battery_voltage = NA_real_,
    battery_current = NA_real_,
    temp_logger = NA_real_,
    sensor_1 = NA_real_,
    sensor_2 = NA_real_,
    is_missing_pulse = TRUE
  )

  cat("  Combining rows...\n")
  complete_diagnostics <- dplyr::bind_rows(diagnostics, missing_rows)

  cat(sprintf("  Sorting %d rows by datetime...\n", nrow(complete_diagnostics)))
  complete_diagnostics <- complete_diagnostics %>%
    dplyr::arrange(datetime)

  cat("  Renumbering pulse_ids...\n")
  complete_diagnostics$pulse_id <- seq_len(nrow(complete_diagnostics))

  cat("  Done!\n")
  complete_diagnostics
}


#' Apply Updated pulse_ids to Measurements
#'
#' Maps old pulse_ids to new standardised pulse_ids using a lookup table.
#' This handles cases where missing pulses were inserted and pulse_ids renumbered.
#'
#' @param measurements Original measurements tibble with old pulse_ids
#' @param original_diagnostics Original diagnostics BEFORE missing pulse insertion
#' @param complete_diagnostics Complete diagnostics with standardised pulse_ids
#' @return Measurements with updated pulse_ids
#' @keywords internal
apply_pulse_ids_to_measurements <- function(measurements, original_diagnostics, complete_diagnostics) {

  cat(sprintf("  Updating %s measurements with new pulse_ids...\n",
              format(nrow(measurements), big.mark = ",")))

  # Create mapping: old_pulse_id -> datetime -> new_pulse_id
  # Original diagnostics has old pulse_ids and datetime
  # Complete diagnostics has same datetime but new sequential pulse_ids
  cat("    Creating pulse_id mapping (old -> new)...\n")

  # Match by datetime to create old -> new mapping
  pulse_id_mapping <- original_diagnostics %>%
    dplyr::select(datetime, old_pulse_id = pulse_id) %>%
    dplyr::inner_join(
      complete_diagnostics %>%
        dplyr::filter(!is_missing_pulse) %>%  # Only original pulses
        dplyr::select(datetime, new_pulse_id = pulse_id),
      by = "datetime"
    ) %>%
    dplyr::select(old_pulse_id, new_pulse_id)

  cat(sprintf("    Created mapping for %d pulses\n", nrow(pulse_id_mapping)))

  # Apply mapping to measurements
  cat(sprintf("    Applying mapping to %s measurements...\n",
              format(nrow(measurements), big.mark = ",")))

  measurements_updated <- measurements %>%
    dplyr::left_join(
      pulse_id_mapping,
      by = c("pulse_id" = "old_pulse_id")
    ) %>%
    dplyr::select(-pulse_id) %>%
    dplyr::rename(pulse_id = new_pulse_id)

  # Check for unmatched
  n_unmatched <- sum(is.na(measurements_updated$pulse_id))
  if (n_unmatched > 0) {
    warning(sprintf(
      "%d measurements could not be mapped to new pulse_ids",
      n_unmatched
    ))
  }

  # Reorder columns to match original structure
  cat("    Reordering columns...\n")
  result <- measurements_updated %>%
    dplyr::select(datetime, pulse_id, dplyr::everything())

  cat("    Measurements update complete!\n")
  result
}
# R/01e_missing_pulse_results.R
# Helper for creating placeholder results for missing pulses

#' Create Placeholder Results for Missing Pulses
#'
#' When diagnostics indicate a missing pulse (is_missing_pulse = TRUE),
#' create result rows with NA values and DATA_MISSING flag.
#'
#' @param diagnostics Diagnostics tibble with is_missing_pulse column
#' @param methods Character vector of methods to calculate
#' @return Tibble with placeholder results for missing pulses
#' @keywords internal
create_missing_pulse_results <- function(diagnostics, methods) {

  # Find missing pulses
  missing_pulses <- diagnostics %>%
    dplyr::filter(is_missing_pulse)

  if (nrow(missing_pulses) == 0) {
    return(NULL)
  }

  # Create placeholder rows for each missing pulse × method × sensor
  results_list <- vector("list", nrow(missing_pulses) * length(methods) * 2)
  idx <- 1

  for (i in seq_len(nrow(missing_pulses))) {
    pulse_row <- missing_pulses[i, ]

    for (method in methods) {
      # Outer sensor
      results_list[[idx]] <- tibble::tibble(
        datetime = pulse_row$datetime,
        pulse_id = pulse_row$pulse_id,
        method = method,
        sensor_position = "outer",
        Vh_cm_hr = NA_real_,
        temp_ratio = NA_real_,

        # Method-specific window columns (all NA for missing pulses)
        hrm_window_start_sec = NA_real_,
        hrm_window_end_sec = NA_real_,
        hrm_peclet_number = NA_real_,

        mhr_upstream_peak_sec = NA_real_,
        mhr_downstream_peak_sec = NA_real_,

        hrmxa_window_start_sec = NA_real_,
        hrmxa_window_end_sec = NA_real_,

        hrmxb_downstream_start_sec = NA_real_,
        hrmxb_downstream_end_sec = NA_real_,
        hrmxb_upstream_start_sec = NA_real_,
        hrmxb_upstream_end_sec = NA_real_,

        tmax_peak_time_sec = NA_real_,

        quality_flag = "DATA_MISSING"
      )
      idx <- idx + 1

      # Inner sensor
      results_list[[idx]] <- tibble::tibble(
        datetime = pulse_row$datetime,
        pulse_id = pulse_row$pulse_id,
        method = method,
        sensor_position = "inner",
        Vh_cm_hr = NA_real_,
        temp_ratio = NA_real_,

        # Method-specific window columns (all NA for missing pulses)
        hrm_window_start_sec = NA_real_,
        hrm_window_end_sec = NA_real_,
        hrm_peclet_number = NA_real_,

        mhr_upstream_peak_sec = NA_real_,
        mhr_downstream_peak_sec = NA_real_,

        hrmxa_window_start_sec = NA_real_,
        hrmxa_window_end_sec = NA_real_,

        hrmxb_downstream_start_sec = NA_real_,
        hrmxb_downstream_end_sec = NA_real_,
        hrmxb_upstream_start_sec = NA_real_,
        hrmxb_upstream_end_sec = NA_real_,

        tmax_peak_time_sec = NA_real_,

        quality_flag = "DATA_MISSING"
      )
      idx <- idx + 1
    }
  }

  dplyr::bind_rows(results_list)
}
