# R/03b_changepoint_detection.R
# Changepoint Detection for Baseline Shifts in Sap Flow Data
# Used to identify periods requiring separate zero-offset calibration

#' Changepoint Detection Functions
#'
#' Functions for detecting baseline shifts in sap flow velocity data that
#' indicate changes in probe alignment requiring separate zero-offset corrections.
#' Uses PELT (Pruned Exact Linear Time) changepoint detection on daily minimum values.
#'
#' @name changepoint_detection
NULL


#' Calculate Daily Minimum Velocities
#'
#' Extracts daily minimum sap velocities from heat pulse velocity results,
#' typically used as input for changepoint detection to identify baseline shifts.
#'
#' @param vh_data Data frame containing velocity data with columns: datetime,
#'   sensor_position, method, and velocity column (specified by vh_col)
#' @param sensor_position Sensor position to analyse ("outer" or "inner", default: "outer")
#' @param method_col Name of method column (default: "method")
#' @param method Value of method to analyse (default: "HRM")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#'
#' @return A data frame with columns:
#'   \item{date}{Date (as Date class)}
#'   \item{min_value}{Minimum velocity for that day (cm/hr)}
#'   \item{n_obs}{Number of observations that day}
#'
#' @details
#' **Why Daily Minima?**
#'
#' Daily minimum velocities are used because:
#' \itemize{
#'   \item They typically occur at pre-dawn when transpiration is lowest
#'   \item They represent the baseline offset from zero
#'   \item Changes in daily minima indicate probe movement or alignment shifts
#'   \item Less affected by environmental variation than means or maxima
#' }
#'
#' **Default: HRM Outer Probe**
#'
#' The outer probe is preferred because:
#' \itemize{
#'   \item More reliably positioned in sapwood
#'   \item Less affected by heartwood interference
#'   \item HRM method validated by Burgess et al. (2001) for low flows
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate daily minima
#' daily_min <- calculate_daily_minima(
#'   vh_data = vh_results,
#'   sensor_position = "outer",
#'   method = "HRM"
#' )
#'
#' # Plot to visualise baseline shifts
#' plot(daily_min$date, daily_min$min_value, type = "l")
#' }
#'
#' @family changepoint detection functions
#' @export
calculate_daily_minima <- function(vh_data,
                                    sensor_position = "outer",
                                    method_col = "method",
                                    method = "HRM",
                                    vh_col = "Vh_cm_hr") {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!sensor_position %in% c("outer", "inner")) {
    stop("sensor_position must be 'outer' or 'inner'")
  }

  # Filter data for specified sensor and method
  filtered_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method &
    !is.na(vh_data[[vh_col]]),
  ]

  if (nrow(filtered_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
  }

  # Ensure datetime is POSIXct
  if (!inherits(filtered_data$datetime, "POSIXct")) {
    filtered_data$datetime <- as.POSIXct(filtered_data$datetime)
  }

  # Extract date component
  filtered_data$date <- as.Date(filtered_data$datetime)

  # Calculate daily minima using dplyr (load if needed)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed")
  }

  daily_min <- filtered_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      min_value = min(.data[[vh_col]], na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date)

  # Convert to regular data frame
  daily_min <- as.data.frame(daily_min)

  return(daily_min)
}


#' Detect Changepoints in Daily Minimum Velocities
#'
#' Uses PELT (Pruned Exact Linear Time) changepoint detection to identify
#' times when baseline sap flow velocities shift, indicating probe movement
#' or alignment changes requiring separate zero-offset calibration.
#'
#' @param daily_min Data frame with columns \code{date} and \code{min_value}
#'   (typically from \code{\link{calculate_daily_minima}})
#' @param penalty Penalty type for PELT algorithm. Options:
#'   \itemize{
#'     \item "MBIC" - Modified Bayesian Information Criterion (default, most conservative)
#'     \item "BIC" - Bayesian Information Criterion (moderate)
#'     \item "Manual" - Manual penalty value (requires \code{penalty_value})
#'   }
#' @param penalty_value Numeric penalty value when \code{penalty = "Manual"}.
#'   Range typically 0-100. Higher values = fewer changepoints detected.
#' @param detection_type Type of change to detect:
#'   \itemize{
#'     \item "mean" - Changes in mean only (default, recommended)
#'     \item "meanvar" - Changes in mean and/or variance
#'   }
#' @param min_segment_days Minimum number of days for a segment (default: 7).
#'   Segments shorter than this will be merged with adjacent segments.
#' @param merge_short_segments Logical, whether to merge segments shorter than
#'   \code{min_segment_days} (default: TRUE)
#'
#' @return A list containing:
#'   \item{changepoints}{Vector of changepoint dates (Date class)}
#'   \item{changepoint_indices}{Vector of row indices in daily_min where changes occur}
#'   \item{segments}{Data frame describing segments between changepoints}
#'   \item{daily_min_with_segments}{Original daily_min with added \code{segment_id} column}
#'   \item{parameters}{List of detection parameters used}
#'
#' @details
#' **PELT Algorithm:**
#'
#' PELT efficiently detects multiple changepoints by:
#' \itemize{
#'   \item Searching for points where statistical properties change
#'   \item Balancing model fit vs. number of changepoints (via penalty)
#'   \item Guaranteeing optimal solution under model assumptions
#' }
#'
#' **Penalty Selection:**
#' \itemize{
#'   \item **MBIC**: Most conservative, fewer changepoints (recommended)
#'   \item **BIC**: Moderate, more changepoints than MBIC
#'   \item **Manual**: Full control, values 0-100 (0 = many changes, 100 = few changes)
#' }
#'
#' **Detection Types:**
#' \itemize{
#'   \item **mean**: Detects baseline shifts (probe movement, swelling/shrinkage)
#'   \item **meanvar**: Also detects changes in variability (environmental changes)
#' }
#'
#' **Short Segment Merging:**
#'
#' Segments < min_segment_days are merged with adjacent segments because:
#' \itemize{
#'   \item Too few observations for reliable zero-offset calculation
#'   \item Often spurious detections from outliers
#'   \item Practical: need sufficient data for Burgess correction
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate daily minima
#' daily_min <- calculate_daily_minima(vh_results)
#'
#' # Detect changepoints with default MBIC
#' result <- detect_changepoints(daily_min)
#'
#' # View detected changepoints
#' print(result$changepoints)
#'
#' # View segments
#' print(result$segments)
#'
#' # Try BIC for more changepoints
#' result_bic <- detect_changepoints(daily_min, penalty = "BIC")
#'
#' # Manual penalty for fine control
#' result_manual <- detect_changepoints(
#'   daily_min,
#'   penalty = "Manual",
#'   penalty_value = 50
#' )
#' }
#'
#' @references
#' Killick, R., Fearnhead, P., & Eckley, I. A. (2012). Optimal detection of
#'   changepoints with a linear computational cost. *Journal of the American
#'   Statistical Association*, 107(500), 1590-1598.
#'
#' @family changepoint detection functions
#' @export
detect_changepoints <- function(daily_min,
                                 penalty = "MBIC",
                                 penalty_value = NULL,
                                 detection_type = "mean",
                                 min_segment_days = 7,
                                 merge_short_segments = TRUE) {

  # Input validation
  if (!is.data.frame(daily_min)) {
    stop("daily_min must be a data frame")
  }

  required_cols <- c("date", "min_value")
  missing_cols <- setdiff(required_cols, names(daily_min))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!penalty %in% c("MBIC", "BIC", "Manual")) {
    stop("penalty must be 'MBIC', 'BIC', or 'Manual'")
  }

  if (penalty == "Manual" && is.null(penalty_value)) {
    stop("penalty_value must be specified when penalty = 'Manual'")
  }

  if (!detection_type %in% c("mean", "meanvar")) {
    stop("detection_type must be 'mean' or 'meanvar'")
  }

  # Load changepoint package
  if (!requireNamespace("changepoint", quietly = TRUE)) {
    stop("Package 'changepoint' is required but not installed.\n",
         "Install with: install.packages('changepoint')")
  }

  # Ensure data is sorted by date
  daily_min <- daily_min[order(daily_min$date), ]

  # Extract values for changepoint detection
  x <- daily_min$min_value

  if (length(x) < 10) {
    warning("Very few data points (", length(x), " days) - changepoint detection may be unreliable")
  }

  # Run PELT changepoint detection
  if (detection_type == "mean") {
    # Detect changes in mean only
    if (penalty == "Manual") {
      cpt_result <- changepoint::cpt.mean(
        x,
        method = "PELT",
        penalty = "Manual",
        pen.value = penalty_value
      )
    } else {
      cpt_result <- changepoint::cpt.mean(
        x,
        method = "PELT",
        penalty = penalty
      )
    }
  } else {
    # Detect changes in mean and/or variance
    if (penalty == "Manual") {
      cpt_result <- changepoint::cpt.meanvar(
        x,
        method = "PELT",
        penalty = "Manual",
        pen.value = penalty_value
      )
    } else {
      cpt_result <- changepoint::cpt.meanvar(
        x,
        method = "PELT",
        penalty = penalty
      )
    }
  }

  # Extract changepoint indices
  cpt_indices <- changepoint::cpts(cpt_result)

  # Convert to dates
  if (length(cpt_indices) == 0) {
    # No changepoints detected - entire dataset is one segment
    segments <- data.frame(
      segment_id = 1,
      start_date = min(daily_min$date),
      end_date = max(daily_min$date),
      start_idx = 1,
      end_idx = nrow(daily_min),
      n_days = nrow(daily_min),
      stringsAsFactors = FALSE
    )

    daily_min$segment_id <- 1

    return(list(
      changepoints = as.Date(character(0)),
      changepoint_indices = integer(0),
      segments = segments,
      daily_min_with_segments = daily_min,
      parameters = list(
        penalty = penalty,
        penalty_value = penalty_value,
        detection_type = detection_type,
        min_segment_days = min_segment_days,
        merge_short_segments = merge_short_segments,
        n_changepoints_detected = 0,
        n_segments = 1
      )
    ))
  }

  changepoint_dates <- daily_min$date[cpt_indices]

  # Build segments data frame
  # n changepoints creates n+1 segments
  # Seg 1: 1 → cpt[1], Seg 2: cpt[1]+1 → cpt[2], ..., Seg n+1: cpt[n]+1 → end
  segment_starts <- c(1, cpt_indices + 1)
  segment_ends <- c(cpt_indices, nrow(daily_min))

  segments <- data.frame(
    segment_id = seq_along(segment_starts),
    start_date = daily_min$date[segment_starts],
    end_date = daily_min$date[segment_ends],
    start_idx = segment_starts,
    end_idx = segment_ends,
    stringsAsFactors = FALSE
  )

  segments$n_days <- segments$end_idx - segments$start_idx + 1

  # Assign segment IDs to daily_min
  daily_min$segment_id <- NA_integer_
  for (i in seq_len(nrow(segments))) {
    daily_min$segment_id[segments$start_idx[i]:segments$end_idx[i]] <- segments$segment_id[i]
  }

  # Merge short segments if requested
  if (merge_short_segments && any(segments$n_days < min_segment_days)) {

    short_segs <- which(segments$n_days < min_segment_days)

    for (i in short_segs) {
      if (i > nrow(segments)) next  # Already merged

      # Try to merge with previous segment
      if (i > 1 && segments$segment_id[i - 1] == (i - 1)) {
        # Merge with previous
        segments$end_date[i - 1] <- segments$end_date[i]
        segments$end_idx[i - 1] <- segments$end_idx[i]
        segments$n_days[i - 1] <- segments$end_idx[i - 1] - segments$start_idx[i - 1] + 1
        segments <- segments[-i, ]

      } else if (i < nrow(segments) && segments$segment_id[i + 1] == (i + 1)) {
        # Merge with next
        segments$start_date[i + 1] <- segments$start_date[i]
        segments$start_idx[i + 1] <- segments$start_idx[i]
        segments$n_days[i + 1] <- segments$end_idx[i + 1] - segments$start_idx[i + 1] + 1
        segments <- segments[-i, ]
      }
    }

    # Reassign segment IDs after merging
    segments$segment_id <- seq_len(nrow(segments))

    # Reassign segment IDs in daily_min
    daily_min$segment_id <- NA_integer_
    for (i in seq_len(nrow(segments))) {
      daily_min$segment_id[segments$start_idx[i]:segments$end_idx[i]] <- segments$segment_id[i]
    }

    # Update changepoint indices and dates based on new segments
    if (nrow(segments) > 1) {
      cpt_indices <- segments$end_idx[-nrow(segments)]  # All segment ends except last
      changepoint_dates <- daily_min$date[cpt_indices]
    } else {
      cpt_indices <- integer(0)
      changepoint_dates <- as.Date(character(0))
    }
  }

  # Return results
  list(
    changepoints = changepoint_dates,
    changepoint_indices = cpt_indices,
    segments = segments,
    daily_min_with_segments = daily_min,
    parameters = list(
      penalty = penalty,
      penalty_value = penalty_value,
      detection_type = detection_type,
      min_segment_days = min_segment_days,
      merge_short_segments = merge_short_segments,
      n_changepoints_detected = length(changepoint_dates),
      n_segments = nrow(segments)
    )
  )
}


#' Extract Segment Baseline Values
#'
#' Calculates baseline (minimum or low quantile) value for each segment
#' identified by changepoint detection.
#'
#' @param changepoint_result Output from \code{\link{detect_changepoints}}
#' @param use_quantile Logical, whether to use quantile instead of minimum (default: FALSE)
#' @param quantile_prob Quantile probability if \code{use_quantile = TRUE} (default: 0.05)
#'
#' @return The segments data frame with added columns:
#'   \item{baseline_value}{Baseline velocity for that segment (cm/hr)}
#'   \item{mean_value}{Mean velocity for that segment}
#'   \item{sd_value}{Standard deviation of velocities}
#'
#' @examples
#' \dontrun{
#' # Detect changepoints
#' cpt_result <- detect_changepoints(daily_min)
#'
#' # Get segment baselines
#' segments_with_baselines <- extract_segment_baselines(cpt_result)
#'
#' print(segments_with_baselines)
#' }
#'
#' @family changepoint detection functions
#' @export
extract_segment_baselines <- function(changepoint_result,
                                       use_quantile = FALSE,
                                       quantile_prob = 0.05) {

  if (!is.list(changepoint_result) ||
      !all(c("segments", "daily_min_with_segments") %in% names(changepoint_result))) {
    stop("changepoint_result must be output from detect_changepoints()")
  }

  segments <- changepoint_result$segments
  daily_min <- changepoint_result$daily_min_with_segments

  # Calculate baseline for each segment
  segments$baseline_value <- NA_real_
  segments$mean_value <- NA_real_
  segments$sd_value <- NA_real_

  for (i in seq_len(nrow(segments))) {
    segment_data <- daily_min[daily_min$segment_id == segments$segment_id[i], ]

    if (use_quantile) {
      segments$baseline_value[i] <- quantile(
        segment_data$min_value,
        probs = quantile_prob,
        na.rm = TRUE
      )
    } else {
      segments$baseline_value[i] <- min(segment_data$min_value, na.rm = TRUE)
    }

    segments$mean_value[i] <- mean(segment_data$min_value, na.rm = TRUE)
    segments$sd_value[i] <- sd(segment_data$min_value, na.rm = TRUE)
  }

  return(segments)
}
