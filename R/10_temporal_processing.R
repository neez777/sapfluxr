# ==============================================================================
# FILE: R/temporal_processing.R
# Temporal Processing Functions for sapFluxR Package
# ==============================================================================

#' Aggregate Velocity Data Temporally
#'
#' Aggregate heat pulse velocity measurements from pulse-level to specified
#' time intervals (hourly, daily, etc.) with quality control and gap detection.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity() or a data frame
#'   with columns: datetime, method, sensor_position, Vh_cm_hr, quality_flag
#' @param interval Character string specifying aggregation interval.
#'   Options: "hourly", "daily", "weekly", "monthly" (default: "hourly")
#' @param agg_method Character string specifying aggregation method for velocities.
#'   Options: "mean", "median", "max" (default: "mean")
#' @param primary_method Character string specifying which HPV method to use as primary.
#'   If NULL, uses all available methods (default: NULL)
#' @param min_observations Integer minimum number of observations required per interval
#'   for valid aggregation (default: 1)
#' @param filter_quality Logical whether to filter out poor quality measurements
#'   before aggregation (default: TRUE)
#' @param quality_flags Character vector of quality flags to include. Only used if
#'   filter_quality = TRUE (default: c("OK", "good"))
#'
#' @return A data frame with aggregated velocity data containing:
#'   \describe{
#'     \item{datetime}{Aggregation period timestamp (POSIXct)}
#'     \item{method}{HPV calculation method used}
#'     \item{sensor_position}{Sensor position ("outer" or "inner")}
#'     \item{Vh_cm_hr}{Aggregated velocity (cm/hr)}
#'     \item{Vh_sd}{Standard deviation of velocities in interval}
#'     \item{n_observations}{Number of observations in interval}
#'     \item{min_velocity}{Minimum velocity in interval}
#'     \item{max_velocity}{Maximum velocity in interval}
#'     \item{data_coverage}{Proportion of expected observations present (0-1)}
#'     \item{quality_score}{Overall quality score for interval (0-100)}
#'     \item{aggregation_interval}{Aggregation interval used}
#'     \item{aggregation_method}{Aggregation method used}
#'   }
#'
#' @details
#' This function provides temporal aggregation following scientific best practices
#' for sap flow data processing. Data coverage is calculated based on expected
#' measurement frequency (typically 30-minute intervals for ICT sensors).
#'
#' Quality scores incorporate data coverage, sample size, and measurement variability
#' to provide an overall assessment of interval reliability.
#'
#' @examples
#' \dontrun{
#' # Basic hourly aggregation
#' hourly_data <- aggregate_velocity_temporal(vh_results, interval = "hourly")
#'
#' # Daily aggregation with HRM method only
#' daily_hrm <- aggregate_velocity_temporal(
#'   vh_results,
#'   interval = "daily",
#'   agg_method = "median",
#'   primary_method = "HRM",
#'   min_observations = 10
#' )
#' }
#'
#' @seealso \code{\link{calc_heat_pulse_velocity}}, \code{\link{filter_velocity_results}}
#' @export
aggregate_velocity_temporal <- function(vh_results,
                                        interval = "hourly",
                                        agg_method = "mean",
                                        primary_method = NULL,
                                        min_observations = 1,
                                        filter_quality = TRUE,
                                        quality_flags = c("OK", "good")) {

  # Input validation
  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame")
  }

  required_cols <- c("datetime", "method", "sensor_position", "Vh_cm_hr")
  missing_cols <- setdiff(required_cols, names(vh_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!interval %in% c("hourly", "daily", "weekly", "monthly")) {
    stop("interval must be one of: hourly, daily, weekly, monthly")
  }

  if (!agg_method %in% c("mean", "median", "max")) {
    stop("agg_method must be one of: mean, median, max")
  }

  if (!is.numeric(min_observations) || min_observations < 1) {
    stop("min_observations must be a positive integer")
  }

  # Convert datetime if needed
  if (!inherits(vh_results$datetime, "POSIXct")) {
    vh_results$datetime <- as.POSIXct(vh_results$datetime)
    if (any(is.na(vh_results$datetime))) {
      stop("Unable to convert datetime column to POSIXct format")
    }
  }

  # Filter for data quality
  if (filter_quality && "quality_flag" %in% names(vh_results)) {
    original_rows <- nrow(vh_results)
    vh_results <- vh_results[vh_results$quality_flag %in% quality_flags, ]

    if (nrow(vh_results) == 0) {
      stop("No data remaining after quality filtering")
    }

    filtered_rows <- original_rows - nrow(vh_results)
    if (filtered_rows > 0) {
      message("Filtered out ", filtered_rows, " rows due to quality flags")
    }
  }

  # Remove missing velocity values
  vh_results <- vh_results[!is.na(vh_results$Vh_cm_hr), ]

  if (nrow(vh_results) == 0) {
    stop("No valid velocity data available for aggregation")
  }

  # Filter for primary method if specified
  if (!is.null(primary_method)) {
    if (!primary_method %in% vh_results$method) {
      stop("Primary method '", primary_method, "' not found in data. Available methods: ",
           paste(unique(vh_results$method), collapse = ", "))
    }
    vh_results <- vh_results[vh_results$method == primary_method, ]
  }

  # Create time grouping variable using base R for compatibility
  vh_results$time_group <- switch(interval,
                                  "hourly" = {
                                    as.POSIXct(format(vh_results$datetime, "%Y-%m-%d %H:00:00"), tz = "UTC")
                                  },
                                  "daily" = {
                                    as.POSIXct(format(vh_results$datetime, "%Y-%m-%d"), tz = "UTC")
                                  },
                                  "weekly" = {
                                    # Start of week (Monday)
                                    days_since_monday <- as.numeric(format(vh_results$datetime, "%u")) - 1
                                    week_start <- vh_results$datetime - days_since_monday * 86400
                                    as.POSIXct(format(week_start, "%Y-%m-%d"), tz = "UTC")
                                  },
                                  "monthly" = {
                                    as.POSIXct(format(vh_results$datetime, "%Y-%m-01"), tz = "UTC")
                                  }
  )

  # Calculate expected observations per interval for coverage assessment
  time_range <- range(vh_results$datetime, na.rm = TRUE)
  total_duration <- as.numeric(difftime(time_range[2], time_range[1], units = "hours"))

  expected_obs_per_interval <- switch(interval,
                                      "hourly" = 2,    # 30-minute intervals
                                      "daily" = 48,    # 30-minute intervals per day
                                      "weekly" = 336,  # 30-minute intervals per week
                                      "monthly" = 1440 # Approximate 30-minute intervals per month
  )

  # Aggregate data using base R for compatibility
  unique_combinations <- unique(vh_results[, c("time_group", "method", "sensor_position")])

  # Initialize results list
  results_list <- list()

  # Process each unique combination
  for (i in 1:nrow(unique_combinations)) {
    combo <- unique_combinations[i, ]

    # Subset data for this combination
    subset_data <- vh_results[
      vh_results$time_group == combo$time_group &
        vh_results$method == combo$method &
        vh_results$sensor_position == combo$sensor_position, ]

    # Check minimum observations requirement
    if (nrow(subset_data) >= min_observations) {

      # Calculate aggregated value
      agg_value <- switch(agg_method,
                          "mean" = mean(subset_data$Vh_cm_hr, na.rm = TRUE),
                          "median" = median(subset_data$Vh_cm_hr, na.rm = TRUE),
                          "max" = max(subset_data$Vh_cm_hr, na.rm = TRUE)
      )

      # Calculate additional statistics
      velocity_sd <- sd(subset_data$Vh_cm_hr, na.rm = TRUE)
      min_vel <- min(subset_data$Vh_cm_hr, na.rm = TRUE)
      max_vel <- max(subset_data$Vh_cm_hr, na.rm = TRUE)
      n_obs <- nrow(subset_data)

      # Calculate data coverage
      data_coverage <- min(1.0, n_obs / expected_obs_per_interval)

      # Calculate quality score (0-100)
      # Based on: data coverage (40%), sample size adequacy (30%), variability (30%)
      coverage_score <- data_coverage * 40
      sample_score <- min(30, (n_obs / min_observations) * 10)
      variability_score <- if (is.na(velocity_sd) || agg_value == 0) {
        30
      } else {
        max(0, 30 - (velocity_sd / abs(agg_value)) * 30)
      }
      quality_score <- coverage_score + sample_score + variability_score

      # Create result row
      result_row <- data.frame(
        datetime = combo$time_group,
        method = combo$method,
        sensor_position = combo$sensor_position,
        Vh_cm_hr = agg_value,
        Vh_sd = velocity_sd,
        n_observations = n_obs,
        min_velocity = min_vel,
        max_velocity = max_vel,
        data_coverage = data_coverage,
        quality_score = quality_score,
        aggregation_interval = interval,
        aggregation_method = agg_method,
        stringsAsFactors = FALSE
      )

      results_list[[length(results_list) + 1]] <- result_row
    }
  }

  # Combine results
  if (length(results_list) == 0) {
    warning("No intervals met the minimum observations requirement")
    return(data.frame())
  }

  aggregated <- do.call(rbind, results_list)
  rownames(aggregated) <- NULL

  # Sort by datetime, method, and sensor position
  aggregated <- aggregated[order(aggregated$datetime, aggregated$method, aggregated$sensor_position), ]

  # Set class for method dispatch
  class(aggregated) <- c("vh_temporal", "data.frame")

  return(aggregated)
}

#' Interpolate Missing Velocity Data
#'
#' Interpolate missing or erroneous velocity values using linear interpolation
#' or other methods suitable for sap flow time series data.
#'
#' @param vh_results A vh_results data frame or similar structure
#' @param method Character string specifying interpolation method.
#'   Currently only "linear" is implemented (default: "linear")
#' @param max_gap_hours Maximum gap size in hours to interpolate. Gaps larger
#'   than this will be left as missing (default: 2)
#' @param by_group Logical whether to interpolate separately for each method/sensor
#'   combination (default: TRUE)
#'
#' @return Data frame with interpolated values and added columns:
#'   \describe{
#'     \item{interpolated}{Logical indicating which values were interpolated}
#'     \item{gap_size_hours}{Size of gap that was interpolated (hours)}
#'   }
#'
#' @details
#' Interpolation is performed separately for each method and sensor position
#' combination to avoid cross-contamination between different measurement types.
#'
#' Only gaps smaller than max_gap_hours are interpolated to maintain data integrity.
#' Large gaps likely indicate systematic problems that should not be interpolated over.
#'
#' @examples
#' \dontrun{
#' # Basic linear interpolation
#' interpolated_data <- interpolate_missing_velocity(vh_results)
#'
#' # More conservative interpolation (smaller gaps only)
#' interpolated_data <- interpolate_missing_velocity(
#'   vh_results,
#'   max_gap_hours = 1
#' )
#' }
#'
#' @seealso \code{\link{aggregate_velocity_temporal}}
#' @export
interpolate_missing_velocity <- function(vh_results,
                                         method = "linear",
                                         max_gap_hours = 2,
                                         by_group = TRUE) {

  # Input validation
  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame")
  }

  required_cols <- c("datetime", "Vh_cm_hr")
  missing_cols <- setdiff(required_cols, names(vh_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!method %in% c("linear")) {
    stop("Currently only 'linear' interpolation method is supported")
  }

  if (!is.numeric(max_gap_hours) || max_gap_hours <= 0) {
    stop("max_gap_hours must be a positive number")
  }

  # Convert datetime if needed
  if (!inherits(vh_results$datetime, "POSIXct")) {
    vh_results$datetime <- as.POSIXct(vh_results$datetime)
  }

  # Sort by datetime
  vh_results <- vh_results[order(vh_results$datetime), ]

  # Initialize interpolation tracking columns
  vh_results$interpolated <- FALSE
  vh_results$gap_size_hours <- NA

  if (by_group && all(c("method", "sensor_position") %in% names(vh_results))) {
    # Interpolate separately for each method/sensor combination
    unique_groups <- unique(vh_results[, c("method", "sensor_position")])

    for (i in 1:nrow(unique_groups)) {
      group <- unique_groups[i, ]
      group_indices <- which(vh_results$method == group$method &
                               vh_results$sensor_position == group$sensor_position)

      if (length(group_indices) > 2) {  # Need at least 3 points for interpolation
        vh_results[group_indices, ] <- interpolate_group_data(
          vh_results[group_indices, ], max_gap_hours)
      }
    }
  } else {
    # Interpolate all data together
    vh_results <- interpolate_group_data(vh_results, max_gap_hours)
  }

  # Update quality flags if present
  if ("quality_flag" %in% names(vh_results)) {
    vh_results$quality_flag[vh_results$interpolated] <- "INTERPOLATED"
  }

  return(vh_results)
}

#' Interpolate data for a single group
#' @param group_data Data frame for a single method/sensor combination
#' @param max_gap_hours Maximum gap size to interpolate
#' @return Data frame with interpolated values
#' @keywords internal
interpolate_group_data <- function(group_data, max_gap_hours) {

  # Identify missing values
  missing_indices <- which(is.na(group_data$Vh_cm_hr))

  if (length(missing_indices) == 0) {
    return(group_data)  # No missing values
  }

  valid_indices <- which(!is.na(group_data$Vh_cm_hr))

  if (length(valid_indices) < 2) {
    return(group_data)  # Not enough valid points for interpolation
  }

  # Process each missing value
  for (idx in missing_indices) {

    # Find nearest valid points before and after
    before_indices <- valid_indices[valid_indices < idx]
    after_indices <- valid_indices[valid_indices > idx]

    if (length(before_indices) > 0 && length(after_indices) > 0) {
      before_idx <- max(before_indices)
      after_idx <- min(after_indices)

      # Check gap size
      gap_duration <- as.numeric(difftime(
        group_data$datetime[after_idx],
        group_data$datetime[before_idx],
        units = "hours"))

      if (gap_duration <= max_gap_hours) {
        # Perform linear interpolation
        time_weight <- as.numeric(difftime(
          group_data$datetime[idx],
          group_data$datetime[before_idx],
          units = "hours")) / gap_duration

        interpolated_value <- group_data$Vh_cm_hr[before_idx] * (1 - time_weight) +
          group_data$Vh_cm_hr[after_idx] * time_weight

        # Update the data
        group_data$Vh_cm_hr[idx] <- interpolated_value
        group_data$interpolated[idx] <- TRUE
        group_data$gap_size_hours[idx] <- gap_duration

        # Add this index to valid indices for subsequent interpolations
        valid_indices <- c(valid_indices, idx)
        valid_indices <- sort(valid_indices)
      }
    }
  }

  return(group_data)
}