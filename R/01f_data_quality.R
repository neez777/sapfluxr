# R/01f_data_quality.R
# Data Quality Control and Flagging for Vh Results
# Simple rolling median and rate of change detection

#' Data Quality Control Functions
#'
#' Quality control functions for heat pulse velocity (Vh) results.
#' Detects missing data, illogical values, and outliers using simple rolling statistics.
#'
#' @name data_quality
NULL



#' Flag Vh Quality Issues
#'
#' @description
#' Comprehensive quality control for heat pulse velocity (Vh) results.
#' Detects missing pulses, illogical values, and statistical outliers using
#' simple rolling median and rate of change detection.
#'
#' @param vh_results Data frame from calc_heat_pulse_velocity() with columns:
#'   datetime, pulse_id, method, sensor_position, Vh_cm_hr
#' @param wood_properties Optional WoodProperties object or path to YAML file
#'   for species-specific thresholds
#' @param detect_missing_pulses Logical, whether to detect gaps in time series (default: TRUE)
#' @param expected_interval_hours Numeric, expected hours between pulses.
#'   If NULL, auto-detected from data (default: NULL)
#' @param interval_tolerance_seconds Numeric, tolerance in seconds for matching pulse times.
#'   Allows for clock drift and jitter (default: 5 seconds)
#' @param check_illogical Logical, whether to check for physically impossible values (default: TRUE)
#' @param hard_max_vh Numeric, absolute maximum Vh in cm/hr (default: 500)
#' @param flag_negative Logical, whether to flag negative flows as SUSPECT (default: TRUE)
#' @param detect_outliers Logical, whether to detect statistical outliers (default: TRUE)
#' @param rolling_window Integer, half-width of rolling window for median calculation
#'   (default: 5, meaning 11-point window)
#' @param rolling_threshold Numeric, SD multiplier for rolling mean outlier detection.
#'   Points deviating more than threshold × SD from rolling mean are flagged (default: 3)
#' @param detect_rate_of_change Logical, whether to detect excessive rate of change (default: TRUE)
#' @param max_change_cm_hr Numeric, maximum allowed change in Vh between consecutive pulses (cm/hr).
#'   Default: 4 cm/hr - larger jumps are unlikely to be real sap flow changes
#' @param check_cross_sensor Logical, detect cross-sensor inconsistencies (default: TRUE)
#' @param cross_sensor_threshold Numeric, SD multiplier for cross-sensor comparison (default: 3)
#' @param add_rows_for_missing Logical, add rows for missing pulses (default: TRUE)
#' @param max_gap_to_fill_hours Numeric, maximum gap duration (hours) to fill with MISSING rows.
#'   Gaps larger than this are reported but not filled to avoid creating excessive missing rows.
#'   Default: 24 hours
#' @param return_gap_report Logical, include detailed gap report (default: TRUE)
#' @param return_full_report Logical, return full report list or just flagged data frame (default: FALSE).
#'   If FALSE, returns only the vh_flagged data frame. If TRUE, returns full list with gap_report,
#'   outlier_summary, flag_counts, and metadata. Use FALSE for simple workflows and Shiny apps.
#' @param verbose Logical, print progress messages (default: TRUE)
#'
#' @return If return_full_report = FALSE (default), returns data frame with quality_flag column added.
#'   If return_full_report = TRUE, returns list containing:
#'   \describe{
#'     \item{vh_flagged}{Data frame with added quality_flag column}
#'     \item{gap_report}{Data frame of missing data periods (if return_gap_report = TRUE)}
#'     \item{outlier_summary}{Summary statistics per sensor and method}
#'     \item{flag_counts}{Count of each flag type}
#'     \item{metadata}{Detection parameters and methods used}
#'   }
#'
#' @details
#' **Two-Tier Quality Flag System:**
#'
#' **Tier 1 - Calculation Quality (CALC_ prefix):**
#' Set during \code{calc_heat_pulse_velocity()}, indicates issues with the calculation itself:
#' - **CALC_FAILED** - Calculation returned NA (e.g., Tmax couldn't find peak)
#' - **CALC_INFINITE** - Calculation returned Inf (division by zero, etc.)
#' - **CALC_EXTREME** - Result outside physically realistic range (|Vh| > 200 or Vh < -50)
#'
#' **Tier 2 - Data Quality (DATA_ prefix):**
#' Set during \code{flag_vh_quality()}, indicates issues with time series patterns:
#' - **DATA_MISSING** - No pulse recorded at expected timestamp (hardware/logging gap)
#' - **DATA_ILLOGICAL** - Exceeds hard_max_vh or species max_velocity_cm_hr
#' - **DATA_OUTLIER** - Statistical outlier (rolling median or rate of change)
#' - **DATA_SUSPECT** - Negative flow or cross-sensor anomaly
#'
#' **OK** - No issues detected in either tier
#'
#' CALC_ flags are preserved when flag_vh_quality() is run. DATA_ flags are only
#' applied to rows currently flagged as OK.
#'
#' **Outlier Detection Methods:**
#'
#' 1. **Rolling Mean**: Compares each point to the mean of a sliding window.
#'    Points that deviate by more than \code{rolling_threshold × SD} from the local
#'    mean are flagged. This detects isolated spikes while respecting local
#'    trends and daily variation. Default threshold of 3 corresponds to 99.7%
#'    confidence interval.
#'
#' 2. **Rate of Change**: Flags consecutive measurements that change by more than
#'    \code{max_change_cm_hr}. Sap flow typically changes gradually, so large jumps
#'    (e.g., > 4 cm/hr between 30-min intervals) are likely sensor errors.
#'
#' 3. **Cross-Sensor**: Compares sensors at the same timestamp. Sensors that deviate
#'    significantly from the median across all sensors are flagged as SUSPECT.
#'
#' **Why simple methods work:**
#' Sap flow data typically has:
#' - Regular measurement intervals (e.g., 30 min)
#' - Gradual changes in velocity (smooth daily cycles)
#' - Isolated outliers (sensor spikes, logging errors)
#'
#' Rolling mean and rate of change detection are sufficient to catch these issues
#' without needing complex seasonal decomposition.
#'
#' **Handling Large Gaps:**
#' Gaps larger than \code{max_gap_to_fill_hours} are reported in the gap_report but
#' NOT filled with MISSING rows to avoid creating thousands of missing entries.
#' Only small gaps (< max_gap_to_fill_hours) are filled.
#'
#' @examples
#' \dontrun{
#' # Basic usage - returns data frame with quality_flag column
#' vh_results <- calc_heat_pulse_velocity(heat_pulse_data)
#' vh_flagged <- flag_vh_quality(vh_results)
#' table(vh_flagged$quality_flag)
#'
#' # Get full report for detailed analysis
#' qc_results <- flag_vh_quality(vh_results, return_full_report = TRUE)
#' print(qc_results)  # Shows summary
#' table(qc_results$vh_flagged$quality_flag)
#' print(qc_results$gap_report)
#'
#' # With species-specific thresholds
#' vh_flagged <- flag_vh_quality(
#'   vh_results,
#'   wood_properties = "eucalyptus"
#' )
#'
#' # Custom outlier detection with full report
#' qc_results <- flag_vh_quality(
#'   vh_results,
#'   rolling_window = 7,           # Wider window
#'   rolling_threshold = 2.5,      # More sensitive
#'   max_change_cm_hr = 3,         # Stricter rate of change
#'   max_gap_to_fill_hours = 12,   # Don't fill gaps > 12 hours
#'   return_full_report = TRUE
#' )
#' }
#'
#' @seealso \code{\link{interpolate_missing_vh}}
#' @family data quality functions
#' @export
flag_vh_quality <- function(vh_results,
                            wood_properties = NULL,
                            detect_missing_pulses = TRUE,
                            expected_interval_hours = NULL,
                            interval_tolerance_seconds = 5,
                            check_illogical = TRUE,
                            hard_max_vh = 500,
                            flag_negative = TRUE,
                            detect_outliers = TRUE,
                            rolling_window = 5,
                            rolling_threshold = 3,
                            detect_rate_of_change = TRUE,
                            max_change_cm_hr = 4,
                            check_cross_sensor = FALSE,
                            cross_sensor_threshold = 3,
                            add_rows_for_missing = TRUE,
                            max_gap_to_fill_hours = 24,
                            return_gap_report = TRUE,
                            return_full_report = FALSE,
                            verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame")
  }

  required_cols <- c("datetime", "Vh_cm_hr")
  missing_cols <- setdiff(required_cols, names(vh_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert datetime if needed
  if (!inherits(vh_results$datetime, "POSIXct")) {
    vh_results$datetime <- as.POSIXct(vh_results$datetime)
  }

  # Sort by datetime
  vh_results <- vh_results[order(vh_results$datetime), ]

  # Load wood properties if provided
  species_max_vh <- NULL
  if (!is.null(wood_properties)) {
    if (inherits(wood_properties, "WoodProperties")) {
      wood_props <- wood_properties
    } else if (is.character(wood_properties)) {
      wood_props <- load_wood_properties(wood_properties)
    } else {
      stop("wood_properties must be a WoodProperties object or path to YAML file")
    }
    species_max_vh <- wood_props$quality_thresholds$max_velocity_cm_hr
  }

  # Preserve existing quality flags from calc_heat_pulse_velocity()
  if (!"quality_flag" %in% names(vh_results)) {
    vh_results$quality_flag <- "OK"
  }

  # Track detection results
  detection_summary <- list()
  gap_report <- NULL

  if (verbose) message("Starting quality control analysis...")

  # 1. Detect missing pulses
  if (detect_missing_pulses) {
    if (verbose) message("  Detecting missing pulses...")

    missing_results <- detect_and_fill_missing_pulses(
      vh_results,
      expected_interval_hours = expected_interval_hours,
      tolerance_seconds = interval_tolerance_seconds,
      add_rows = add_rows_for_missing,
      max_gap_to_fill_hours = max_gap_to_fill_hours,
      verbose = verbose
    )

    vh_results <- missing_results$vh_complete
    gap_report <- missing_results$gap_report
    detection_summary$missing_pulses <- missing_results$summary

    if (verbose && !is.null(gap_report) && nrow(gap_report) > 0) {
      n_filled <- sum(gap_report$n_missing[gap_report$filled], na.rm = TRUE)
      n_not_filled <- sum(gap_report$n_missing[!gap_report$filled], na.rm = TRUE)
      if (n_filled > 0) {
        message(sprintf("    Found %d gap(s): %d pulses added", nrow(gap_report), n_filled))
      }
      if (n_not_filled > 0) {
        message(sprintf("    WARNING: %d large gap(s) > %.1f hours not filled (%d missing pulses)",
                        sum(!gap_report$filled), max_gap_to_fill_hours, n_not_filled))
      }
    }
  }

  # 2. Detect illogical values
  if (check_illogical) {
    if (verbose) message("  Checking for illogical values...")

    illogical_indices <- detect_illogical_values(
      vh_results$Vh_cm_hr,
      hard_max = hard_max_vh,
      species_max = species_max_vh
    )

    if (length(illogical_indices) > 0) {
      vh_results$quality_flag[illogical_indices] <- "DATA_ILLOGICAL"
    }

    detection_summary$illogical <- list(
      n_illogical = length(illogical_indices),
      indices = illogical_indices
    )

    if (verbose && length(illogical_indices) > 0) {
      message(sprintf("    Flagged %d illogical values", length(illogical_indices)))
    }
  }

  # 3. Detect outliers using rolling median
  if (detect_outliers) {
    if (verbose) message("  Detecting outliers using rolling median...")

    outlier_indices <- integer(0)

    # Create grouping combinations (method × sensor_position)
    grouping_cols <- c()
    if ("method" %in% names(vh_results)) {
      grouping_cols <- c(grouping_cols, "method")
    }
    if ("sensor_position" %in% names(vh_results)) {
      grouping_cols <- c(grouping_cols, "sensor_position")
    }

    if (length(grouping_cols) > 0) {
      # Get unique combinations of method and sensor_position
      unique_groups <- unique(vh_results[vh_results$quality_flag == "OK", grouping_cols, drop = FALSE])
      unique_groups <- unique_groups[complete.cases(unique_groups), , drop = FALSE]

      for (i in seq_len(nrow(unique_groups))) {
        # Build filter for this group
        group_filter <- rep(TRUE, nrow(vh_results))
        group_label <- c()

        for (col in grouping_cols) {
          group_filter <- group_filter & vh_results[[col]] == unique_groups[i, col]
          group_label <- c(group_label, paste0(col, "=", unique_groups[i, col]))
        }

        group_indices <- which(group_filter & vh_results$quality_flag == "OK")

        if (length(group_indices) < 2 * rolling_window + 1) {
          if (verbose) {
            message(sprintf("    Skipping %s (insufficient data: %d points)",
                            paste(group_label, collapse=", "), length(group_indices)))
          }
          next
        }

        if (verbose) message(sprintf("    Checking %s", paste(group_label, collapse=", ")))

        rolling_outliers <- detect_outliers_rolling_mean(
          vh_results$Vh_cm_hr[group_indices],
          window = rolling_window,
          threshold = rolling_threshold
        )

        outlier_indices <- c(outlier_indices, group_indices[rolling_outliers])

        if (verbose && length(rolling_outliers) > 0) {
          message(sprintf("      Found %d outliers", length(rolling_outliers)))
        }
      }
    } else {
      # No grouping columns - process all together
      ok_indices <- which(vh_results$quality_flag == "OK")

      if (length(ok_indices) >= 2 * rolling_window + 1) {
        rolling_outliers <- detect_outliers_rolling_mean(
          vh_results$Vh_cm_hr[ok_indices],
          window = rolling_window,
          threshold = rolling_threshold
        )

        outlier_indices <- ok_indices[rolling_outliers]

        if (verbose && length(rolling_outliers) > 0) {
          message(sprintf("    Found %d outliers", length(rolling_outliers)))
        }
      }
    }

    outlier_indices <- unique(outlier_indices)
    if (length(outlier_indices) > 0) {
      to_flag <- intersect(outlier_indices, which(vh_results$quality_flag == "OK"))
      vh_results$quality_flag[to_flag] <- "DATA_OUTLIER"
    }

    detection_summary$outliers_rolling <- list(
      n_outliers = length(outlier_indices),
      indices = outlier_indices
    )
  }

  # 4. Detect excessive rate of change
  if (detect_rate_of_change) {
    if (verbose) message("  Detecting excessive rate of change...")

    rate_change_indices <- integer(0)

    # Use same grouping as rolling median (method × sensor_position)
    grouping_cols <- c()
    if ("method" %in% names(vh_results)) {
      grouping_cols <- c(grouping_cols, "method")
    }
    if ("sensor_position" %in% names(vh_results)) {
      grouping_cols <- c(grouping_cols, "sensor_position")
    }

    if (length(grouping_cols) > 0) {
      # Get unique combinations of method and sensor_position
      unique_groups <- unique(vh_results[vh_results$quality_flag == "OK", grouping_cols, drop = FALSE])
      unique_groups <- unique_groups[complete.cases(unique_groups), , drop = FALSE]

      for (i in seq_len(nrow(unique_groups))) {
        # Build filter for this group
        group_filter <- rep(TRUE, nrow(vh_results))
        group_label <- c()

        for (col in grouping_cols) {
          group_filter <- group_filter & vh_results[[col]] == unique_groups[i, col]
          group_label <- c(group_label, paste0(col, "=", unique_groups[i, col]))
        }

        group_indices <- which(group_filter & vh_results$quality_flag == "OK")

        if (length(group_indices) < 2) next

        # Sort by datetime
        sorted_order <- order(vh_results$datetime[group_indices])
        group_indices_sorted <- group_indices[sorted_order]

        rate_outliers <- detect_rate_of_change_outliers(
          vh_results$Vh_cm_hr[group_indices_sorted],
          max_change = max_change_cm_hr
        )

        rate_change_indices <- c(rate_change_indices, group_indices_sorted[rate_outliers])

        if (verbose && length(rate_outliers) > 0) {
          message(sprintf("    %s: %d excessive rate changes",
                          paste(group_label, collapse=", "), length(rate_outliers)))
        }
      }
    } else {
      # No grouping columns - process all together
      ok_indices <- which(vh_results$quality_flag == "OK")

      if (length(ok_indices) >= 2) {
        sorted_order <- order(vh_results$datetime[ok_indices])
        ok_indices_sorted <- ok_indices[sorted_order]

        rate_outliers <- detect_rate_of_change_outliers(
          vh_results$Vh_cm_hr[ok_indices_sorted],
          max_change = max_change_cm_hr
        )

        rate_change_indices <- ok_indices_sorted[rate_outliers]

        if (verbose && length(rate_outliers) > 0) {
          message(sprintf("    Found %d excessive rate changes", length(rate_outliers)))
        }
      }
    }

    rate_change_indices <- unique(rate_change_indices)
    if (length(rate_change_indices) > 0) {
      to_flag <- intersect(rate_change_indices, which(vh_results$quality_flag == "OK"))
      vh_results$quality_flag[to_flag] <- "DATA_OUTLIER"
    }

    detection_summary$outliers_rate_change <- list(
      n_outliers = length(rate_change_indices),
      indices = rate_change_indices
    )
  }

  # 5. Check for negative flows
  if (flag_negative) {
    negative_indices <- which(vh_results$Vh_cm_hr < 0 & vh_results$quality_flag == "OK")
    if (length(negative_indices) > 0) {
      vh_results$quality_flag[negative_indices] <- "DATA_SUSPECT"

      if (verbose) {
        message(sprintf("  Flagged %d negative flows as SUSPECT", length(negative_indices)))
      }
    }

    detection_summary$negative_flows <- list(
      n_negative = length(negative_indices),
      indices = negative_indices
    )
  }

  # 6. Cross-sensor detection
  if (check_cross_sensor && "sensor_position" %in% names(vh_results)) {
    if (verbose) message("  Detecting cross-sensor anomalies...")

    cross_sensor_indices <- detect_cross_sensor_outliers(
      vh_results,
      threshold = cross_sensor_threshold,
      flag_levels = c("OK")
    )

    if (length(cross_sensor_indices) > 0) {
      vh_results$quality_flag[cross_sensor_indices] <- "DATA_SUSPECT"

      if (verbose) {
        message(sprintf("    Flagged %d cross-sensor anomalies", length(cross_sensor_indices)))
      }
    }

    detection_summary$cross_sensor <- list(
      n_anomalies = length(cross_sensor_indices),
      indices = cross_sensor_indices
    )
  }

  # Generate summary
  flag_counts <- table(vh_results$quality_flag)

  if (verbose) {
    message("\nQuality control complete:")
    message("  ", paste(names(flag_counts), "=", flag_counts, collapse = ", "))
  }

  # Return results
  if (return_full_report) {
    # Full report with all diagnostics
    result <- list(
      vh_flagged = vh_results,
      gap_report = if (return_gap_report) gap_report else NULL,
      outlier_summary = detection_summary,
      flag_counts = as.data.frame(flag_counts),
      metadata = list(
        rolling_window = rolling_window,
        rolling_threshold = rolling_threshold,
        max_change_cm_hr = max_change_cm_hr,
        hard_max_vh = hard_max_vh,
        species_max_vh = species_max_vh,
        max_gap_to_fill_hours = max_gap_to_fill_hours,
        timestamp = Sys.time()
      )
    )
    class(result) <- c("vh_quality_results", "list")
    return(result)
  } else {
    # Simple return - just the flagged data frame
    return(vh_results)
  }
}


#' Detect and Fill Missing Pulses
#'
#' @param vh_results Data frame with datetime column
#' @param expected_interval_hours Expected interval between pulses (hours)
#' @param tolerance_seconds Tolerance in seconds for matching pulse times
#' @param add_rows Logical, whether to add rows for missing pulses
#' @param max_gap_to_fill_hours Maximum gap size (hours) to fill
#' @param verbose Logical, print messages
#' @return List with vh_complete, gap_report, summary
#' @keywords internal
detect_and_fill_missing_pulses <- function(vh_results,
                                           expected_interval_hours = NULL,
                                           tolerance_seconds = 5,
                                           add_rows = TRUE,
                                           max_gap_to_fill_hours = 24,
                                           verbose = FALSE) {

  # Auto-detect pulse interval
  if (is.null(expected_interval_hours)) {
    if (nrow(vh_results) < 2) {
      stop("Cannot auto-detect pulse interval: vh_results has fewer than 2 rows")
    }

    unique_datetimes <- sort(unique(vh_results$datetime))
    if (length(unique_datetimes) < 2) {
      stop("Cannot auto-detect pulse interval: only 1 unique datetime found")
    }

    time_diffs <- as.numeric(diff(unique_datetimes), units = "hours")
    time_diffs <- time_diffs[time_diffs > 0]

    if (length(time_diffs) == 0) {
      stop("Cannot auto-detect pulse interval: no positive time differences found")
    }

    interval_table <- table(round(time_diffs, 2))
    expected_interval_hours <- as.numeric(names(interval_table)[which.max(interval_table)])

    if (is.na(expected_interval_hours) || expected_interval_hours <= 0) {
      stop("Auto-detected pulse interval is invalid: ", expected_interval_hours)
    }

    if (verbose) {
      message(sprintf("    Auto-detected pulse interval: %.2f hours", expected_interval_hours))
    }
  }

  # Detect gaps by checking consecutive timestamps and interpolate linearly
  # This approach handles clock drift naturally by using actual timing between measurements
  actual_times <- sort(unique(vh_results$datetime))
  missing_times <- c()
  gap_info <- list()
  gap_counter <- 0

  # Loop through consecutive pairs of actual measurements
  for (i in seq_len(length(actual_times) - 1)) {
    time_A <- actual_times[i]
    time_B <- actual_times[i + 1]

    # Calculate actual gap duration
    gap_duration_hours <- as.numeric(difftime(time_B, time_A, units = "hours"))

    # Determine if gap is larger than expected (with tolerance)
    gap_threshold <- expected_interval_hours * 1.5

    if (gap_duration_hours > gap_threshold) {
      # Calculate number of missing pulses
      n_missing <- round(gap_duration_hours / expected_interval_hours) - 1

      if (n_missing > 0) {
        # Linear interpolation: divide gap into equal intervals
        # For 1 missing: midpoint
        # For 2 missing: 1/3 and 2/3 points, etc.
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

  # Generate gap report from gap_info
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

  # Add rows for small gaps only
  vh_complete <- vh_results

  if (add_rows && length(gap_info) > 0) {
    # Collect times to fill from gaps that meet the max_gap_hours threshold
    times_to_fill <- c()
    for (gap in gap_info) {
      if (gap$duration_hours <= max_gap_to_fill_hours) {
        times_to_fill <- c(times_to_fill, gap$interpolated_times)
      }
    }

    if (length(times_to_fill) > 0) {
      grouping_cols <- c()
      if ("sensor_position" %in% names(vh_results)) {
        grouping_cols <- c(grouping_cols, "sensor_position")
      }
      if ("method" %in% names(vh_results)) {
        grouping_cols <- c(grouping_cols, "method")
      }

      if (length(grouping_cols) > 0) {
        unique_combinations <- unique(vh_results[, grouping_cols, drop = FALSE])
        template_row <- vh_results[1, ]
        template_row[1, ] <- NA

        missing_rows_list <- lapply(seq_len(nrow(unique_combinations)), function(i) {
          combo_missing <- template_row[rep(1, length(times_to_fill)), ]
          combo_missing$datetime <- times_to_fill
          combo_missing$quality_flag <- "DATA_MISSING"

          for (col in grouping_cols) {
            combo_missing[[col]] <- unique_combinations[i, col]
          }
          combo_missing
        })

        missing_rows <- do.call(rbind, missing_rows_list)
      } else {
        template_row <- vh_results[1, ]
        template_row[1, ] <- NA
        missing_rows <- template_row[rep(1, length(times_to_fill)), ]
        missing_rows$datetime <- times_to_fill
        missing_rows$quality_flag <- "DATA_MISSING"
      }

      vh_complete <- rbind(vh_results, missing_rows)
      vh_complete <- vh_complete[order(vh_complete$datetime), ]
    }
  }

  list(
    vh_complete = vh_complete,
    gap_report = gap_report,
    summary = list(
      n_actual = length(actual_times),
      n_missing = length(missing_times),
      n_filled = if (!is.null(gap_report)) sum(gap_report$n_missing[gap_report$filled]) else 0,
      n_gaps = length(gap_info),
      expected_interval_hours = expected_interval_hours
    )
  )
}


#' Detect Illogical Vh Values
#'
#' @param vh_values Numeric vector of Vh values
#' @param hard_max Absolute maximum velocity
#' @param species_max Species-specific maximum
#' @return Integer vector of indices flagged as illogical
#' @keywords internal
detect_illogical_values <- function(vh_values, hard_max = 500, species_max = NULL) {
  illogical <- integer(0)

  hard_max_idx <- which(abs(vh_values) > hard_max)
  illogical <- c(illogical, hard_max_idx)

  if (!is.null(species_max) && !is.na(species_max)) {
    species_max_idx <- which(abs(vh_values) > species_max)
    illogical <- c(illogical, species_max_idx)
  }

  unique(illogical)
}


#' Detect Outliers Using Rolling Mean
#'
#' @param vh_values Numeric vector of Vh values
#' @param window Integer, half-width of rolling window
#' @param threshold Numeric, standard deviation multiplier (default 3 = 99.7% confidence)
#' @return Integer vector of outlier indices
#' @keywords internal
detect_outliers_rolling_mean <- function(vh_values, window = 5, threshold = 3) {
  # Use C++ implementation for speed (10-50x faster than R version)
  # See src/01f_data_quality.cpp for implementation
  detect_outliers_rolling_mean_cpp(vh_values, window, threshold)
}
#detect_outliers_rolling_mean <- function(vh_values, window = 5, threshold = 3) {
#  n <- length(vh_values)
#  outlier_indices <- integer(0)
#
#  if (n < 2 * window + 1) {
#    return(outlier_indices)
#  }
#
#  for (i in (window + 1):(n - window)) {
#    if (is.na(vh_values[i])) next
#
#    window_data <- vh_values[(i - window):(i + window)]
#    window_mean <- mean(window_data, na.rm = TRUE)
#    window_sd <- sd(window_data, na.rm = TRUE)
#
#    # Check if we have valid mean and SD
#    if (!is.na(window_mean) && !is.na(window_sd) && window_sd > 0) {
#      deviation <- abs(vh_values[i] - window_mean) / window_sd
#
#      if (deviation > threshold) {
#        outlier_indices <- c(outlier_indices, i)
#      }
#    }
#  }
#
#  outlier_indices
#}


#' Detect Rate of Change Outliers
#'
#' @param vh_values Numeric vector of Vh values (must be sorted by time)
#' @param max_change Numeric, maximum allowed change between consecutive points (cm/hr)
#' @return Integer vector of outlier indices (second point in each pair)
#' @keywords internal
detect_rate_of_change_outliers <- function(vh_values, max_change = 4) {
  # Use C++ implementation for speed (20-100x faster than R version)
  # See src/01f_data_quality.cpp for implementation
  detect_rate_of_change_outliers_cpp(vh_values, max_change)
}
#detect_rate_of_change_outliers <- function(vh_values, max_change = 4) {
#  n <- length(vh_values)
#  outlier_indices <- integer(0)
#
#  if (n < 2) {
#    return(outlier_indices)
#  }
#
#  for (i in 2:n) {
#    if (is.na(vh_values[i]) || is.na(vh_values[i - 1])) next
#
#    change <- abs(vh_values[i] - vh_values[i - 1])
#
#    if (change > max_change) {
#      outlier_indices <- c(outlier_indices, i)
#    }
#  }
#
#  outlier_indices
#}


#' Detect Cross-Sensor Outliers
#'
#' @param vh_results Data frame with datetime, sensor_position, Vh_cm_hr, quality_flag
#' @param threshold Numeric, SD multiplier for cross-sensor comparison
#' @param flag_levels Character vector of flag levels to check
#' @return Integer vector of row indices flagged as cross-sensor outliers
#' @keywords internal
detect_cross_sensor_outliers <- function(vh_results,
                                        threshold = 3,
                                        flag_levels = "OK") {

  if (!"sensor_position" %in% names(vh_results)) {
    return(integer(0))
  }

  check_indices <- which(vh_results$quality_flag %in% flag_levels)
  if (length(check_indices) == 0) {
    return(integer(0))
  }

  outlier_indices <- integer(0)
  unique_times <- unique(vh_results$datetime)

  for (dt in unique_times) {
    time_indices <- which(vh_results$datetime == dt &
                            1:nrow(vh_results) %in% check_indices)

    if (length(time_indices) < 3) next

    vh_at_time <- vh_results$Vh_cm_hr[time_indices]
    vh_at_time <- vh_at_time[!is.na(vh_at_time)]

    if (length(vh_at_time) < 3) next

    sensor_mean <- mean(vh_at_time, na.rm = TRUE)
    sensor_sd <- sd(vh_at_time, na.rm = TRUE)

    if (sensor_sd > 0) {
      deviations <- abs(vh_results$Vh_cm_hr[time_indices] - sensor_mean) / sensor_sd
      outlier_mask <- deviations > threshold & !is.na(deviations)
      outlier_indices <- c(outlier_indices, time_indices[outlier_mask])
    }
  }

  unique(outlier_indices)
}


#' Print Method for Vh Quality Results
#'
#' @param x A vh_quality_results object
#' @param ... Additional arguments (ignored)
#' @export
print.vh_quality_results <- function(x, ...) {
  cat("Vh Quality Control Results\n")
  cat("==========================\n\n")

  cat("Flag Summary:\n")
  print(x$flag_counts)
  cat("\n")

  if (!is.null(x$gap_report) && nrow(x$gap_report) > 0) {
    cat(sprintf("Missing Data: %d gap(s) detected\n", nrow(x$gap_report)))
    total_missing <- sum(x$gap_report$n_missing)
    total_filled <- sum(x$gap_report$n_missing[x$gap_report$filled])
    cat(sprintf("  Total missing pulses: %d (%d filled, %d not filled)\n",
                total_missing, total_filled, total_missing - total_filled))
    cat(sprintf("  Longest gap: %.1f hours\n", max(x$gap_report$duration_hours)))
    cat("\n")
  }

  if (!is.null(x$outlier_summary$outliers_rolling)) {
    cat(sprintf("Rolling median outliers: %d\n", x$outlier_summary$outliers_rolling$n_outliers))
  }

  if (!is.null(x$outlier_summary$outliers_rate_change)) {
    cat(sprintf("Rate of change outliers: %d\n", x$outlier_summary$outliers_rate_change$n_outliers))
  }

  cat("\nDetection Parameters:\n")
  cat(sprintf("  Rolling window: %d\n", x$metadata$rolling_window))
  cat(sprintf("  Rolling threshold: %.1f SD\n", x$metadata$rolling_threshold))
  cat(sprintf("  Max change: %.1f cm/hr\n", x$metadata$max_change_cm_hr))
  cat(sprintf("  Hard max Vh: %.0f cm/hr\n", x$metadata$hard_max_vh))
  if (!is.null(x$metadata$species_max_vh)) {
    cat(sprintf("  Species max Vh: %.0f cm/hr\n", x$metadata$species_max_vh))
  }
  cat(sprintf("  Max gap to fill: %.1f hours\n", x$metadata$max_gap_to_fill_hours))

  invisible(x)
}
