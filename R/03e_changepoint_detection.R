# R/03e_changepoint_detection.R
# Automatic Changepoint Detection for Zero-Flow Baselines

#' Calculate Daily Minimum Velocities
#'
#' Aggregates heat pulse velocity data to daily minima, which are used as
#' proxies for the zero-flow baseline in changepoint detection and spacing
#' correction.
#'
#' @param vh_data Data frame containing velocity measurements.
#' @param sensor_position Character, "outer" (default) or "inner".
#' @param method_col Character, name of method column (default: "method").
#' @param method Character, which HPV method to use (default: "HRM").
#' @param vh_col Character, name of velocity column (default: "Vh_cm_hr").
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{date} - Date of measurement
#'     \item \code{min_value} - Minimum velocity recorded that day
#'     \item \code{n_obs} - Number of observations that day
#'   }
#'
#' @examples
#' \dontrun{
#' daily_min <- calculate_daily_minima(vh_results, sensor_position = "outer")
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

  # Filter to specific sensor and method
  filtered_data <- vh_data[vh_data$sensor_position == sensor_position &
                             vh_data[[method_col]] == method, ]

  if (nrow(filtered_data) == 0) {
    stop("No data found for sensor_position='", sensor_position, "' and method='", method, "'")
  }

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


#' Detect Changepoints in Sap Flow Data
#'
#' Automatically identifies dates where the baseline (zero-flow offset) shifts,
#' which typically indicates physical probe movement or stem swelling/shrinkage.
#'
#' @param vh_data Data frame containing velocity data (e.g., from `calc_heat_pulse_velocity()`).
#' @param daily_min Optional. A data frame of pre-calculated daily minima. If NULL,
#'   this is automatically calculated from \code{vh_data}.
#' @param sensor_position Character, "outer" or "inner" (default: "outer"). Used if calculating daily minima.
#' @param hpv_method Character, HPV method to use for detection (default: "HRM").
#' @param penalty Penalty method for PELT algorithm: "MBIC" (default, most conservative),
#'   "BIC" (moderate), or "Manual" (custom value).
#' @param penalty_value Numeric value required if \code{penalty = "Manual"}.
#' @param detection_type What to detect: "mean" (shifts in baseline, default) or
#'   "meanvar" (shifts in baseline and variance).
#' @param min_segment_days Minimum days required between changepoints (default: 7).
#'   Segments shorter than this will be merged with adjacent segments.
#' @param merge_short_segments Logical, whether to merge segments shorter than
#'   \code{min_segment_days} (default: TRUE)
#' @param method Deprecated argument (use hpv_method).
#' @param ... Additional arguments passed to \code{\link{calculate_daily_minima}}.
#'
#' @return A list with class \code{"pelt_changepoints"} containing:
#'   \item{changepoints}{Vector of detected Date objects}
#'   \item{segments}{Data frame detailing each segment}
#'   \item{daily_min}{The daily minima data used for detection}
#'   \item{parameters}{List of detection parameters}
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
#' # Pass the full data directly - it will calculate minima automatically
#' result <- detect_changepoints(vh_results)
#'
#' # View detected changepoints
#' print(result$changepoints)
#'
#' # View segments
#' print(result$segments)
#'
#' # Try BIC for more changepoints
#' result_bic <- detect_changepoints(vh_results, penalty = "BIC")
#'
#' # Manual penalty for fine control
#' result_manual <- detect_changepoints(
#'   vh_results,
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
detect_changepoints <- function(vh_data = NULL,
                                 daily_min = NULL,
                                 sensor_position = "outer",
                                 hpv_method = "HRM",
                                 penalty = "MBIC",
                                 penalty_value = NULL,
                                 detection_type = "mean",
                                 min_segment_days = 7,
                                 merge_short_segments = TRUE,
                                 method = NULL,
                                 ...) {

  # Handle legacy calling pattern where daily_min was the first unnamed argument
  if (!is.null(vh_data) && is.null(daily_min) && is.data.frame(vh_data) && 
      "min_value" %in% names(vh_data) && !("sensor_position" %in% names(vh_data))) {
    daily_min <- vh_data
    vh_data <- NULL
  }

  if (is.null(daily_min)) {
    if (is.null(vh_data)) stop("Must provide either vh_data or daily_min")
    daily_min <- calculate_daily_minima(
      vh_data = vh_data,
      sensor_position = sensor_position,
      method = if (!is.null(method)) method else hpv_method,
      ...
    )
  } else if (!is.data.frame(daily_min)) {
    stop("daily_min must be a data frame")
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
    # Detect changes in mean and variance
    if (penalty == "Manual") {
      cpt_result <- changepoint::cpt.meanvar(
        x,
        test.stat = "Normal",
        method = "PELT",
        penalty = "Manual",
        pen.value = penalty_value
      )
    } else {
      cpt_result <- changepoint::cpt.meanvar(
        x,
        test.stat = "Normal",
        method = "PELT",
        penalty = penalty
      )
    }
  }

  # Extract results
  cpt_indices <- changepoint::cpts(cpt_result)
  changepoints <- daily_min$date[cpt_indices]

  # Post-process: Merge short segments if requested
  if (merge_short_segments && length(changepoints) > 0) {
    # Calculate segment lengths (days)
    seg_ends <- c(cpt_indices, nrow(daily_min))
    seg_starts <- c(1, cpt_indices + 1)
    seg_days <- seg_ends - seg_starts + 1

    # Identify short segments
    short_segs <- which(seg_days < min_segment_days)

    if (length(short_segs) > 0) {
      # Keep track of indices to remove
      to_remove <- numeric(0)

      for (i in short_segs) {
        # If it's a middle segment or last segment, remove the changepoint before it
        if (i > 1) {
          to_remove <- c(to_remove, i - 1)
        } else {
          # If it's the first segment, remove the changepoint after it
          to_remove <- c(to_remove, 1)
        }
      }

      # Clean up indices and remove duplicates
      to_remove <- unique(to_remove)
      to_remove <- to_remove[to_remove <= length(cpt_indices)]

      if (length(to_remove) > 0) {
        cpt_indices <- cpt_indices[-to_remove]
        changepoints <- daily_min$date[cpt_indices]
      }
    }
  }

  # Build segment summary
  seg_ends <- c(cpt_indices, nrow(daily_min))
  seg_starts <- c(1, cpt_indices + 1)
  n_segments <- length(seg_starts)

  segments_df <- data.frame(
    segment_id = 1:n_segments,
    start_date = daily_min$date[seg_starts],
    end_date = daily_min$date[seg_ends],
    n_days = seg_ends - seg_starts + 1,
    stringsAsFactors = FALSE
  )

  # Calculate segment statistics
  segments_df$baseline_value <- sapply(1:n_segments, function(i) {
    mean(daily_min$min_value[seg_starts[i]:seg_ends[i]], na.rm = TRUE)
  })

  segments_df$sd_value <- sapply(1:n_segments, function(i) {
    sd(daily_min$min_value[seg_starts[i]:seg_ends[i]], na.rm = TRUE)
  })

  # Prepare result list
  result <- list(
    changepoints = changepoints,
    changepoint_indices = cpt_indices,
    segments = segments_df,
    daily_min = daily_min,
    parameters = list(
      penalty = penalty,
      penalty_value = penalty_value,
      detection_type = detection_type,
      min_segment_days = min_segment_days,
      merge_short_segments = merge_short_segments,
      n_changepoints = length(changepoints),
      n_segments = n_segments
    )
  )

  class(result) <- c("pelt_changepoints", "list")

  return(result)
}


#' Extract Segment Baselines from Changepoint Result
#'
#' Helper function to extract baselines in a format compatible with
#' \code{\link{apply_spacing_correction}}.
#'
#' @param cpt_result Result from \code{\link{detect_changepoints}}.
#'
#' @return A named list where names are segment IDs and values are baselines.
#'
#' @export
extract_segment_baselines <- function(cpt_result) {
  if (!inherits(cpt_result, "pelt_changepoints")) {
    stop("Input must be a pelt_changepoints object")
  }

  baselines <- as.list(cpt_result$segments$baseline_value)
  names(baselines) <- as.character(cpt_result$segments$segment_id)

  return(baselines)
}


#' Print PELT Changepoint Result
#' @export
#' @keywords internal
print.pelt_changepoints <- function(x, ...) {
  cat("\nPELT CHANGEPOINT DETECTION RESULT\n")
  cat(strrep("=", 40), "\n")
  cat(sprintf("Penalty type:     %s\n", x$parameters$penalty))
  cat(sprintf("Detection type:   %s\n", x$parameters$detection_type))
  cat(sprintf("Days processed:   %d\n", nrow(x$daily_min)))
  cat(sprintf("Changepoints:     %d\n", x$parameters$n_changepoints))
  cat(sprintf("Segments created: %d\n", x$parameters$n_segments))

  if (x$parameters$n_changepoints > 0) {
    cat("\nChangepoint Dates:\n")
    print(x$changepoints)
  }

  cat("\nSegment Summary:\n")
  print(x$segments[, c("segment_id", "start_date", "end_date", "n_days", "baseline_value")],
        row.names = FALSE)

  invisible(x)
}
