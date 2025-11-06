# R/02a_data_filtering.R
# Data Filtering and Interpolation for Heat Pulse Velocity Results
# Removes outliers and fills gaps in quality-flagged velocity data

#' Data Filtering and Interpolation Functions
#'
#' Functions for filtering outliers and interpolating missing data in heat pulse
#' velocity (Vh) results. Processes quality-flagged data to produce clean datasets
#' ready for flux calculations.
#'
#' @name data_filtering
NULL


#' Filter and Interpolate Heat Pulse Velocity Data
#'
#' @description
#' Applies filtering and interpolation to quality-flagged velocity data.
#' Removes or interpolates flagged values (outliers, illogical values, missing data)
#' while preserving suspect values (e.g., negative flows, high Peclet numbers).
#'
#' @param vh_flagged Data frame from flag_vh_quality() with quality_flag column
#' @param flags_to_interpolate Character vector of quality flags to interpolate.
#'   Default: c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL")
#' @param flags_to_preserve Character vector of quality flags to leave unchanged.
#'   Default: c("DATA_SUSPECT")
#' @param interpolation_method Character string specifying interpolation method.
#'   Options: "linear" (default), "spline", "approx". See Details.
#' @param max_gap_hours Numeric, maximum gap duration (hours) to interpolate.
#'   Gaps larger than this are left as NA. Default: 1 hour (typically 2 data points).
#'   Warning issued if > 3 hours.
#' @param keep_original_values Logical, whether to preserve original values in
#'   new column (default: TRUE). Adds Vh_original column.
#' @param keep_interpolated_flag Logical, whether to add is_interpolated column
#'   (default: TRUE). TRUE for interpolated values, FALSE otherwise.
#' @param group_by_method Logical, whether to interpolate within each HPV method
#'   separately (default: TRUE). Recommended to avoid mixing methods.
#' @param group_by_sensor Logical, whether to interpolate within each sensor position
#'   separately (default: TRUE). Required - inner/outer should never be mixed.
#' @param handle_multi_method Character string specifying how to handle sDMA methods.
#'   Options: "regenerate" (default) or "direct". See Details.
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @details
#' **Interpolation Methods:**
#' \itemize{
#'   \item \strong{linear}: Straight line between points. For single missing point,
#'     this is the average of previous and following values. Fast and simple.
#'   \item \strong{spline}: Cubic spline using multiple surrounding points. Creates
#'     smooth curves through data. Good for longer gaps with gradual changes.
#'   \item \strong{approx}: Linear approximation with better edge handling. Similar
#'     to linear but handles gaps at start/end of time series more robustly.
#' }
#'
#' **Quality Flag Handling:**
#' \itemize{
#'   \item \strong{DATA_MISSING}: Hardware/logging gaps - interpolated by default
#'   \item \strong{DATA_OUTLIER}: Statistical outliers, rate-of-change spikes - interpolated
#'   \item \strong{DATA_ILLOGICAL}: Exceeds physical limits - interpolated
#'   \item \strong{DATA_SUSPECT}: Negative flows, cross-sensor anomalies - preserved (may be real)
#'   \item \strong{CALC_FAILED/INFINITE/EXTREME}: Calculation failures - left as NA
#' }
#'
#' **Gap Thresholds:**
#' \itemize{
#'   \item Default: 1 hour (typically 2 consecutive 30-min measurements)
#'   \item Warning if > 3 hours (interpolating large gaps is not recommended)
#'   \item Gaps exceeding max_gap_hours are flagged as "LARGE_GAP" and left as NA
#' }
#'
#' **sDMA Handling (handle_multi_method parameter):**
#' \itemize{
#'   \item \strong{"regenerate"}: Interpolate HRM and secondary methods separately,
#'     then re-apply Peclet-based switching to regenerate sDMA values. Recommended
#'     to avoid artifacts at method switching boundaries.
#'   \item \strong{"direct"}: Interpolate sDMA values directly as a single time series.
#'     Simpler but may create discontinuities at Pe = 1.0 threshold.
#' }
#'
#' **Grouping:**
#' Data is always grouped by sensor_position (inner/outer must be separate).
#' Optionally also group by method to interpolate each HPV method independently.
#'
#' @return A tibble with filtered and interpolated velocity data containing:
#'   \item{datetime}{Timestamp of measurement}
#'   \item{pulse_id}{Pulse identification number}
#'   \item{method}{Calculation method}
#'   \item{sensor_position}{Inner or outer sensor position}
#'   \item{Vh_cm_hr}{Cleaned/interpolated velocity (cm/hr)}
#'   \item{Vh_original}{Original velocity before interpolation (if keep_original_values = TRUE)}
#'   \item{quality_flag}{Updated quality flag ("INTERPOLATED" for filled values, "LARGE_GAP" for unfilled)}
#'   \item{quality_flag_original}{Original quality flag before filtering}
#'   \item{is_interpolated}{Logical indicating if value was interpolated (if keep_interpolated_flag = TRUE)}
#'   \item{gap_duration_hours}{Duration of gap that was filled (hours), NA for non-interpolated values}
#'
#' @examples
#' \dontrun{
#' # Basic workflow
#' vh_results <- calc_heat_pulse_velocity(heat_pulse_data)
#' vh_flagged <- flag_vh_quality(vh_results)
#' vh_cleaned <- filter_and_interpolate_vh(vh_flagged)
#'
#' # Custom interpolation settings
#' vh_cleaned <- filter_and_interpolate_vh(
#'   vh_flagged,
#'   flags_to_interpolate = c("DATA_MISSING", "DATA_OUTLIER"),
#'   interpolation_method = "spline",
#'   max_gap_hours = 2
#' )
#'
#' # With sDMA regeneration
#' vh_sdma <- apply_sdma_processing(vh_flagged, secondary_method = "MHR")
#' vh_cleaned <- filter_and_interpolate_vh(
#'   vh_sdma,
#'   handle_multi_method = "regenerate"  # Re-applies Peclet switching after interpolation
#' )
#' }
#'
#' @seealso \code{\link{flag_vh_quality}}, \code{\link{preview_interpolation_changes}},
#'   \code{\link{apply_sdma_processing}}
#' @family data filtering functions
#' @export
filter_and_interpolate_vh <- function(vh_flagged,
                                       flags_to_interpolate = c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL"),
                                       flags_to_preserve = c("DATA_SUSPECT"),
                                       interpolation_method = c("linear", "spline", "approx"),
                                       max_gap_hours = 1,
                                       keep_original_values = TRUE,
                                       keep_interpolated_flag = TRUE,
                                       group_by_method = TRUE,
                                       group_by_sensor = TRUE,
                                       handle_multi_method = c("regenerate", "direct"),
                                       verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_flagged)) {
    stop("vh_flagged must be a data frame")
  }

  required_cols <- c("datetime", "Vh_cm_hr", "quality_flag")
  missing_cols <- setdiff(required_cols, names(vh_flagged))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Match arguments
  interpolation_method <- match.arg(interpolation_method)
  handle_multi_method <- match.arg(handle_multi_method)

  # Warning for large gap threshold
  if (max_gap_hours > 3) {
    warning("max_gap_hours > 3 is not recommended. Interpolating large gaps may introduce artifacts.",
            call. = FALSE)
  }

  # Convert datetime if needed
  if (!inherits(vh_flagged$datetime, "POSIXct")) {
    vh_flagged$datetime <- as.POSIXct(vh_flagged$datetime)
  }

  # Sort by datetime
  vh_flagged <- vh_flagged[order(vh_flagged$datetime), ]

  if (verbose) {
    message("Starting data filtering and interpolation...")
    message(sprintf("  Interpolation method: %s", interpolation_method))
    message(sprintf("  Max gap to fill: %.1f hours", max_gap_hours))
  }

  # Preserve original data
  if (keep_original_values) {
    vh_flagged$Vh_original <- vh_flagged$Vh_cm_hr
  }
  vh_flagged$quality_flag_original <- vh_flagged$quality_flag

  # Check for sDMA methods
  has_sdma <- any(grepl("^sDMA:", vh_flagged$method))

  if (has_sdma && handle_multi_method == "regenerate") {
    if (verbose) {
      message("  sDMA methods detected - will regenerate after interpolating base methods")
    }

    # Separate sDMA from base methods
    sdma_rows <- grepl("^sDMA:", vh_flagged$method)
    base_data <- vh_flagged[!sdma_rows, ]
    sdma_data <- vh_flagged[sdma_rows, ]

    # Get unique sDMA methods and their secondary methods
    sdma_methods <- unique(vh_flagged$method[sdma_rows])
    secondary_methods <- gsub("^sDMA:", "", sdma_methods)

    # Interpolate base methods (HRM and secondaries)
    base_cleaned <- interpolate_by_groups(
      base_data,
      flags_to_interpolate = flags_to_interpolate,
      flags_to_preserve = flags_to_preserve,
      interpolation_method = interpolation_method,
      max_gap_hours = max_gap_hours,
      keep_interpolated_flag = keep_interpolated_flag,
      group_by_method = group_by_method,
      group_by_sensor = group_by_sensor,
      verbose = verbose
    )

    # Regenerate sDMA from cleaned base methods
    if (verbose) {
      message("\n  Regenerating sDMA methods from interpolated data...")
    }

    # Apply sDMA processing to cleaned base data
    result <- apply_sdma_processing(
      base_cleaned,
      secondary_method = secondary_methods,
      show_progress = verbose
    )

  } else {
    # Direct interpolation (all methods including sDMA)
    result <- interpolate_by_groups(
      vh_flagged,
      flags_to_interpolate = flags_to_interpolate,
      flags_to_preserve = flags_to_preserve,
      interpolation_method = interpolation_method,
      max_gap_hours = max_gap_hours,
      keep_interpolated_flag = keep_interpolated_flag,
      group_by_method = group_by_method,
      group_by_sensor = group_by_sensor,
      verbose = verbose
    )
  }

  # Summary
  if (verbose) {
    n_interpolated <- sum(result$is_interpolated, na.rm = TRUE)
    n_large_gaps <- sum(result$quality_flag == "LARGE_GAP", na.rm = TRUE)

    message("\nFiltering and interpolation complete:")
    message(sprintf("  Interpolated values: %d", n_interpolated))
    if (n_large_gaps > 0) {
      message(sprintf("  Large gaps (> %.1f hr, not filled): %d", max_gap_hours, n_large_gaps))
    }
  }

  # Preserve class
  class(result) <- class(vh_flagged)

  return(result)
}


#' Interpolate Velocity Data by Groups
#'
#' Internal function that performs interpolation within method/sensor groups.
#'
#' @param vh_data Data frame with velocity data
#' @param flags_to_interpolate Quality flags to interpolate
#' @param flags_to_preserve Quality flags to leave unchanged
#' @param interpolation_method Interpolation method ("linear", "spline", "approx")
#' @param max_gap_hours Maximum gap to interpolate (hours)
#' @param keep_interpolated_flag Add is_interpolated column
#' @param group_by_method Group by method
#' @param group_by_sensor Group by sensor position
#' @param verbose Print messages
#' @return Data frame with interpolated values
#' @keywords internal
interpolate_by_groups <- function(vh_data,
                                   flags_to_interpolate,
                                   flags_to_preserve,
                                   interpolation_method,
                                   max_gap_hours,
                                   keep_interpolated_flag,
                                   group_by_method,
                                   group_by_sensor,
                                   verbose) {

  # Build grouping columns
  grouping_cols <- c()
  if (group_by_sensor && "sensor_position" %in% names(vh_data)) {
    grouping_cols <- c(grouping_cols, "sensor_position")
  }
  if (group_by_method && "method" %in% names(vh_data)) {
    grouping_cols <- c(grouping_cols, "method")
  }

  if (length(grouping_cols) == 0) {
    stop("Must group by at least one column (sensor_position or method)")
  }

  # Get unique group combinations
  unique_groups <- unique(vh_data[, grouping_cols, drop = FALSE])
  unique_groups <- unique_groups[complete.cases(unique_groups), , drop = FALSE]

  # Process each group
  results_list <- list()

  for (i in seq_len(nrow(unique_groups))) {
    # Build filter for this group
    group_filter <- rep(TRUE, nrow(vh_data))
    group_label <- c()

    for (col in grouping_cols) {
      group_filter <- group_filter & vh_data[[col]] == unique_groups[i, col]
      group_label <- c(group_label, paste0(col, "=", unique_groups[i, col]))
    }

    group_data <- vh_data[group_filter, ]

    if (nrow(group_data) < 2) {
      if (verbose) {
        message(sprintf("  Skipping %s (insufficient data: %d points)",
                       paste(group_label, collapse = ", "), nrow(group_data)))
      }
      results_list[[i]] <- group_data
      next
    }

    if (verbose) {
      message(sprintf("  Processing %s (%d points)",
                     paste(group_label, collapse = ", "), nrow(group_data)))
    }

    # Interpolate this group
    group_cleaned <- interpolate_single_group(
      group_data,
      flags_to_interpolate = flags_to_interpolate,
      flags_to_preserve = flags_to_preserve,
      interpolation_method = interpolation_method,
      max_gap_hours = max_gap_hours,
      keep_interpolated_flag = keep_interpolated_flag
    )

    results_list[[i]] <- group_cleaned
  }

  # Combine all groups
  result <- dplyr::bind_rows(results_list)

  # Sort by datetime
  result <- result[order(result$datetime), ]

  return(result)
}


#' Interpolate Single Group of Velocity Data
#'
#' Internal function that performs interpolation on a single method/sensor group.
#'
#' @param group_data Data frame for a single group (one method × sensor combination)
#' @param flags_to_interpolate Quality flags to interpolate
#' @param flags_to_preserve Quality flags to leave unchanged
#' @param interpolation_method Interpolation method
#' @param max_gap_hours Maximum gap to interpolate (hours)
#' @param keep_interpolated_flag Add is_interpolated column
#' @return Data frame with interpolated values
#' @keywords internal
interpolate_single_group <- function(group_data,
                                      flags_to_interpolate,
                                      flags_to_preserve,
                                      interpolation_method,
                                      max_gap_hours,
                                      keep_interpolated_flag) {

  # Identify values to interpolate
  should_interpolate <- group_data$quality_flag %in% flags_to_interpolate

  # Preserve certain flags
  should_preserve <- group_data$quality_flag %in% flags_to_preserve

  # Don't interpolate preserved values
  should_interpolate <- should_interpolate & !should_preserve

  # Initialize tracking columns
  if (keep_interpolated_flag) {
    group_data$is_interpolated <- FALSE
    group_data$gap_duration_hours <- NA_real_
  }

  # If nothing to interpolate, return as-is
  if (sum(should_interpolate) == 0) {
    return(group_data)
  }

  # Create working velocity vector
  vh_values <- group_data$Vh_cm_hr

  # Set values to interpolate as NA
  vh_values[should_interpolate] <- NA

  # Find gaps and their durations
  gap_info <- identify_gaps(group_data$datetime, vh_values)

  # Interpolate gaps that meet threshold
  if (nrow(gap_info) > 0) {
    for (j in seq_len(nrow(gap_info))) {
      gap_duration <- gap_info$duration_hours[j]
      gap_indices <- gap_info$start_idx[j]:gap_info$end_idx[j]

      if (gap_duration <= max_gap_hours) {
        # Interpolate this gap
        vh_values <- interpolate_gap(
          vh_values = vh_values,
          datetimes = group_data$datetime,
          gap_indices = gap_indices,
          method = interpolation_method
        )

        # Mark as interpolated
        if (keep_interpolated_flag) {
          group_data$is_interpolated[gap_indices] <- TRUE
          group_data$gap_duration_hours[gap_indices] <- gap_duration
        }

        # Update quality flag
        group_data$quality_flag[gap_indices] <- "INTERPOLATED"

      } else {
        # Gap too large - flag but don't fill
        group_data$quality_flag[gap_indices] <- "LARGE_GAP"
      }
    }
  }

  # Update cleaned values
  group_data$Vh_cm_hr <- vh_values

  return(group_data)
}


#' Identify Gaps in Time Series
#'
#' Finds consecutive NA values in velocity data and calculates gap durations.
#'
#' @param datetimes POSIXct vector of timestamps
#' @param vh_values Numeric vector of velocities (may contain NA)
#' @return Data frame with columns: start_idx, end_idx, n_missing, duration_hours
#' @keywords internal
identify_gaps <- function(datetimes, vh_values) {

  # Find NA runs
  is_na <- is.na(vh_values)
  na_runs <- rle(is_na)

  if (!any(na_runs$values)) {
    # No gaps
    return(data.frame(
      start_idx = integer(0),
      end_idx = integer(0),
      n_missing = integer(0),
      duration_hours = numeric(0)
    ))
  }

  # Extract gap indices
  run_ends <- cumsum(na_runs$lengths)
  run_starts <- c(1, run_ends[-length(run_ends)] + 1)

  gap_starts <- run_starts[na_runs$values]
  gap_ends <- run_ends[na_runs$values]
  gap_lengths <- na_runs$lengths[na_runs$values]

  # Calculate gap durations in hours
  gap_durations <- numeric(length(gap_starts))
  for (i in seq_along(gap_starts)) {
    # Find surrounding valid timestamps
    before_idx <- if (gap_starts[i] > 1) gap_starts[i] - 1 else NULL
    after_idx <- if (gap_ends[i] < length(datetimes)) gap_ends[i] + 1 else NULL

    if (!is.null(before_idx) && !is.null(after_idx)) {
      gap_durations[i] <- as.numeric(difftime(datetimes[after_idx], datetimes[before_idx], units = "hours"))
    } else if (!is.null(before_idx)) {
      # Gap at end - estimate from pulse interval
      gap_durations[i] <- gap_lengths[i] * estimate_pulse_interval(datetimes)
    } else if (!is.null(after_idx)) {
      # Gap at start - estimate from pulse interval
      gap_durations[i] <- gap_lengths[i] * estimate_pulse_interval(datetimes)
    } else {
      # All NA
      gap_durations[i] <- NA_real_
    }
  }

  data.frame(
    start_idx = gap_starts,
    end_idx = gap_ends,
    n_missing = gap_lengths,
    duration_hours = gap_durations
  )
}


#' Estimate Pulse Interval
#'
#' Estimates typical time between pulses from datetime vector.
#'
#' @param datetimes POSIXct vector of timestamps
#' @return Numeric, estimated interval in hours
#' @keywords internal
estimate_pulse_interval <- function(datetimes) {
  if (length(datetimes) < 2) {
    return(0.5)  # Default 30 minutes
  }

  diffs <- as.numeric(diff(datetimes), units = "hours")
  diffs <- diffs[diffs > 0]

  if (length(diffs) == 0) {
    return(0.5)
  }

  # Use median to avoid outliers from large gaps
  median(diffs, na.rm = TRUE)
}


#' Interpolate a Single Gap
#'
#' Fills a gap in velocity data using specified interpolation method.
#'
#' @param vh_values Numeric vector of velocities
#' @param datetimes POSIXct vector of timestamps
#' @param gap_indices Integer vector of indices to fill
#' @param method Interpolation method ("linear", "spline", "approx")
#' @return Numeric vector with gap filled
#' @keywords internal
interpolate_gap <- function(vh_values, datetimes, gap_indices, method) {

  # Get indices of non-NA values
  valid_indices <- which(!is.na(vh_values))

  if (length(valid_indices) < 2) {
    # Can't interpolate with fewer than 2 points
    return(vh_values)
  }

  # Convert datetimes to numeric for interpolation
  times_numeric <- as.numeric(datetimes)

  # Perform interpolation
  if (method == "linear") {
    # Linear interpolation
    interpolated <- approx(
      x = times_numeric[valid_indices],
      y = vh_values[valid_indices],
      xout = times_numeric[gap_indices],
      method = "linear",
      rule = 1  # NA outside range
    )$y

  } else if (method == "spline") {
    # Cubic spline interpolation
    if (length(valid_indices) < 4) {
      # Fall back to linear for < 4 points
      interpolated <- approx(
        x = times_numeric[valid_indices],
        y = vh_values[valid_indices],
        xout = times_numeric[gap_indices],
        method = "linear",
        rule = 1
      )$y
    } else {
      interpolated <- spline(
        x = times_numeric[valid_indices],
        y = vh_values[valid_indices],
        xout = times_numeric[gap_indices],
        method = "natural"
      )$y
    }

  } else if (method == "approx") {
    # Linear approximation with better edge handling
    interpolated <- approx(
      x = times_numeric[valid_indices],
      y = vh_values[valid_indices],
      xout = times_numeric[gap_indices],
      method = "linear",
      rule = 2  # Extend edges
    )$y

  } else {
    stop("Unknown interpolation method: ", method)
  }

  # Fill gap
  vh_values[gap_indices] <- interpolated

  return(vh_values)
}


#' Preview Interpolation Changes
#'
#' @description
#' Previews the effects of interpolation without modifying data.
#' Returns summary statistics and detailed reports of what would be changed.
#' Useful for exploring different interpolation settings before applying.
#'
#' @param vh_flagged Data frame from flag_vh_quality() with quality_flag column
#' @param flags_to_interpolate Character vector of quality flags to interpolate.
#'   Default: c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL")
#' @param max_gap_hours Numeric, maximum gap duration (hours) to interpolate.
#'   Default: 1 hour.
#' @param group_by_method Logical, whether to group by method (default: TRUE)
#' @param group_by_sensor Logical, whether to group by sensor (default: TRUE)
#'
#' @return A list containing:
#'   \item{summary_table}{Data frame summarising changes by flag type}
#'   \item{gap_report}{Data frame of gaps with action (INTERPOLATE or TOO_LARGE)}
#'   \item{affected_indices}{Integer vector of row indices that would be modified}
#'   \item{n_total}{Total number of rows}
#'   \item{n_interpolated}{Number of values that would be interpolated}
#'   \item{n_large_gaps}{Number of values in gaps too large to fill}
#'
#' @examples
#' \dontrun{
#' # Preview before applying
#' preview <- preview_interpolation_changes(vh_flagged, max_gap_hours = 2)
#' print(preview$summary_table)
#' print(preview$gap_report)
#'
#' # Try different settings
#' preview1 <- preview_interpolation_changes(vh_flagged, max_gap_hours = 1)
#' preview2 <- preview_interpolation_changes(vh_flagged, max_gap_hours = 3)
#' }
#'
#' @seealso \code{\link{filter_and_interpolate_vh}}
#' @family data filtering functions
#' @export
preview_interpolation_changes <- function(vh_flagged,
                                           flags_to_interpolate = c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL"),
                                           max_gap_hours = 1,
                                           group_by_method = TRUE,
                                           group_by_sensor = TRUE) {

  # Input validation
  if (!is.data.frame(vh_flagged)) {
    stop("vh_flagged must be a data frame")
  }

  required_cols <- c("datetime", "Vh_cm_hr", "quality_flag")
  missing_cols <- setdiff(required_cols, names(vh_flagged))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Count by flag type
  flag_counts <- table(vh_flagged$quality_flag)

  # Identify values to interpolate
  should_interpolate <- vh_flagged$quality_flag %in% flags_to_interpolate
  n_to_interpolate <- sum(should_interpolate)

  # Build grouping structure
  grouping_cols <- c()
  if (group_by_sensor && "sensor_position" %in% names(vh_flagged)) {
    grouping_cols <- c(grouping_cols, "sensor_position")
  }
  if (group_by_method && "method" %in% names(vh_flagged)) {
    grouping_cols <- c(grouping_cols, "method")
  }

  # Identify gaps across all groups
  all_gaps <- list()
  gap_counter <- 0

  if (length(grouping_cols) > 0) {
    unique_groups <- unique(vh_flagged[, grouping_cols, drop = FALSE])
    unique_groups <- unique_groups[complete.cases(unique_groups), , drop = FALSE]

    for (i in seq_len(nrow(unique_groups))) {
      # Filter this group
      group_filter <- rep(TRUE, nrow(vh_flagged))
      for (col in grouping_cols) {
        group_filter <- group_filter & vh_flagged[[col]] == unique_groups[i, col]
      }

      group_data <- vh_flagged[group_filter, ]
      group_indices <- which(group_filter)

      # Create working vector with flagged values as NA
      vh_values <- group_data$Vh_cm_hr
      vh_values[group_data$quality_flag %in% flags_to_interpolate] <- NA

      # Identify gaps
      gap_info <- identify_gaps(group_data$datetime, vh_values)

      if (nrow(gap_info) > 0) {
        for (j in seq_len(nrow(gap_info))) {
          gap_counter <- gap_counter + 1

          # Map back to original indices
          original_indices <- group_indices[gap_info$start_idx[j]:gap_info$end_idx[j]]

          all_gaps[[gap_counter]] <- data.frame(
            gap_id = gap_counter,
            group = paste(unique_groups[i, ], collapse = " × "),
            gap_start = group_data$datetime[gap_info$start_idx[j]],
            gap_end = group_data$datetime[gap_info$end_idx[j]],
            duration_hours = gap_info$duration_hours[j],
            n_points = gap_info$n_missing[j],
            action = ifelse(gap_info$duration_hours[j] <= max_gap_hours, "INTERPOLATE", "TOO_LARGE"),
            affected_rows = paste(range(original_indices), collapse = "-")
          )
        }
      }
    }
  }

  # Combine gap reports
  gap_report <- if (length(all_gaps) > 0) {
    dplyr::bind_rows(all_gaps)
  } else {
    data.frame(
      gap_id = integer(0),
      group = character(0),
      gap_start = as.POSIXct(character(0)),
      gap_end = as.POSIXct(character(0)),
      duration_hours = numeric(0),
      n_points = integer(0),
      action = character(0),
      affected_rows = character(0)
    )
  }

  # Summary by flag type
  summary_table <- data.frame(
    flag_type = names(flag_counts),
    original_count = as.vector(flag_counts),
    will_interpolate = sapply(names(flag_counts), function(flag) {
      if (flag %in% flags_to_interpolate) {
        # Count how many would actually be filled (vs. large gaps)
        if (nrow(gap_report) > 0) {
          sum(gap_report$action == "INTERPOLATE" & vh_flagged$quality_flag[as.integer(gap_report$affected_rows)] == flag, na.rm = TRUE)
        } else {
          0
        }
      } else {
        0
      }
    }),
    large_gaps = sapply(names(flag_counts), function(flag) {
      if (flag %in% flags_to_interpolate && nrow(gap_report) > 0) {
        sum(gap_report$action == "TOO_LARGE")
      } else {
        0
      }
    })
  )

  # Extract affected indices
  if (nrow(gap_report) > 0) {
    affected_indices <- sort(unique(unlist(lapply(gap_report$affected_rows, function(range_str) {
      parts <- as.integer(strsplit(range_str, "-")[[1]])
      parts[1]:parts[2]
    }))))
  } else {
    affected_indices <- integer(0)
  }

  # Calculate counts
  n_interpolated <- if (nrow(gap_report) > 0) {
    sum(gap_report$n_points[gap_report$action == "INTERPOLATE"])
  } else {
    0
  }

  n_large_gaps <- if (nrow(gap_report) > 0) {
    sum(gap_report$n_points[gap_report$action == "TOO_LARGE"])
  } else {
    0
  }

  result <- list(
    summary_table = summary_table,
    gap_report = gap_report,
    affected_indices = affected_indices,
    n_total = nrow(vh_flagged),
    n_interpolated = n_interpolated,
    n_large_gaps = n_large_gaps
  )

  class(result) <- c("interpolation_preview", "list")

  return(result)
}


#' Print Method for Interpolation Preview
#'
#' @param x An interpolation_preview object
#' @param ... Additional arguments (ignored)
#' @export
print.interpolation_preview <- function(x, ...) {
  cat("Interpolation Preview\n")
  cat(strrep("=", 50), "\n\n")

  cat("Summary:\n")
  cat(sprintf("  Total rows: %d\n", x$n_total))
  cat(sprintf("  Will interpolate: %d values\n", x$n_interpolated))
  if (x$n_large_gaps > 0) {
    cat(sprintf("  Large gaps (not filled): %d values\n", x$n_large_gaps))
  }
  cat("\n")

  if (nrow(x$summary_table) > 0) {
    cat("Changes by Flag Type:\n")
    print(x$summary_table, row.names = FALSE)
    cat("\n")
  }

  if (nrow(x$gap_report) > 0) {
    cat(sprintf("Gap Report (%d gaps detected):\n", nrow(x$gap_report)))
    print(head(x$gap_report, 10), row.names = FALSE)
    if (nrow(x$gap_report) > 10) {
      cat(sprintf("  ... and %d more gaps\n", nrow(x$gap_report) - 10))
    }
  } else {
    cat("No gaps detected.\n")
  }

  invisible(x)
}
