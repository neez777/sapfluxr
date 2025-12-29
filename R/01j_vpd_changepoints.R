# VPD-Based Changepoint Detection for Spacing Correction
#
# Identifies suitable dates for spacing correction based on stable environmental
# conditions (low VPD). These dates serve as "changepoints" for applying spacing
# corrections when environmental demand is minimal and stable.

#' Calculate Daily Minimum VPD
#'
#' Extracts daily minimum VPD values from weather data, used to identify
#' days with low atmospheric demand suitable for spacing correction.
#'
#' @param weather_data Data frame containing weather data with columns:
#'   \code{datetime} and \code{vpd_kpa} (typically from \code{\link{calc_vpd}})
#' @param vpd_col Name of VPD column (default: "vpd_kpa")
#'
#' @return A data frame with columns:
#'   \item{date}{Date (as Date class)}
#'   \item{min_vpd}{Minimum VPD for that day (kPa)}
#'   \item{mean_vpd}{Mean VPD for that day (kPa)}
#'   \item{max_vpd}{Maximum VPD for that day (kPa)}
#'   \item{sd_vpd}{Standard deviation of VPD for that day (kPa)}
#'   \item{n_obs}{Number of observations that day}
#'
#' @details
#' **Why Daily Minimum VPD?**
#'
#' Daily minimum VPD values are used because:
#' \itemize{
#'   \item They occur during periods of lowest atmospheric demand (typically pre-dawn)
#'   \item Low VPD indicates high humidity and minimal transpiration stress
#'   \item Stable low VPD suggests consistent environmental conditions
#'   \item Sap flow during low VPD is less influenced by environmental drivers
#' }
#'
#' **For Spacing Correction:**
#'
#' Low VPD periods are ideal for spacing correction because:
#' \itemize{
#'   \item Minimal transpiration = low sap flow rates
#'   \item Stable conditions = consistent probe thermal environment
#'   \item Reduced environmental noise in baseline measurements
#'   \item Better detection of probe spacing effects vs. environmental effects
#' }
#'
#' @examples
#' \dontrun{
#' # Import and process weather data
#' weather <- read_weather_data("weather_station.csv")
#' weather_vpd <- calc_vpd(weather)
#'
#' # Calculate daily minimum VPD
#' daily_vpd <- calculate_daily_vpd_minima(weather_vpd)
#'
#' # View days with lowest VPD
#' head(daily_vpd[order(daily_vpd$min_vpd), ])
#' }
#'
#' @family VPD changepoint functions
#' @seealso \code{\link{detect_vpd_changepoints}} for identifying suitable dates
#' @export
calculate_daily_vpd_minima <- function(weather_data,
                                        vpd_col = "vpd_kpa") {

  # Input validation
  if (!is.data.frame(weather_data)) {
    stop("weather_data must be a data frame")
  }

  required_cols <- c("datetime", vpd_col)
  missing_cols <- setdiff(required_cols, names(weather_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure datetime is POSIXct
  if (!inherits(weather_data$datetime, "POSIXct")) {
    weather_data$datetime <- as.POSIXct(weather_data$datetime)
  }

  # Extract date component
  weather_data$date <- as.Date(weather_data$datetime)

  # Calculate daily VPD statistics
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed")
  }

  # Filter out rows with missing dates before grouping
  daily_vpd <- weather_data %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      min_vpd = min(.data[[vpd_col]], na.rm = TRUE),
      mean_vpd = mean(.data[[vpd_col]], na.rm = TRUE),
      max_vpd = max(.data[[vpd_col]], na.rm = TRUE),
      sd_vpd = sd(.data[[vpd_col]], na.rm = TRUE),
      n_obs = sum(!is.na(.data[[vpd_col]])),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date)

  # Convert to regular data frame
  daily_vpd <- as.data.frame(daily_vpd)

  # Add attributes for metadata
  attr(daily_vpd, "vpd_col") <- vpd_col
  attr(daily_vpd, "calculation_time") <- Sys.time()

  return(daily_vpd)
}


#' Detect VPD-Based Changepoints for Spacing Correction
#'
#' Identifies dates when daily minimum VPD is at or below a threshold, indicating
#' suitable environmental conditions for spacing correction. These dates serve as
#' "changepoints" analogous to PELT-detected baseline shifts, but based on
#' environmental stability rather than statistical change detection.
#'
#' @param daily_vpd Data frame with columns \code{date} and \code{min_vpd}
#'   (typically from \code{\link{calculate_daily_vpd_minima}})
#' @param vpd_threshold Maximum VPD threshold (kPa). Days with minimum VPD at or
#'   below this value are selected as changepoints. Default is 0.5 kPa (low atmospheric demand).
#'   Typical values:
#'   \itemize{
#'     \item 0.3 kPa - Very conservative, very low VPD only
#'     \item 0.5 kPa - Moderate, recommended default
#'     \item 0.8 kPa - Permissive, allows higher VPD
#'   }
#' @param min_segment_days Minimum number of days between selected changepoints
#'   (default: 7). If changepoints are closer than this, only the day with lowest
#'   VPD in that period is retained. This prevents over-sampling nearby dates.
#' @param require_consecutive Logical. If TRUE, requires at least
#'   \code{min_consecutive_days} consecutive days below threshold to select the
#'   middle day as a changepoint (default: FALSE). Use TRUE for stricter stability requirement.
#' @param min_consecutive_days If \code{require_consecutive = TRUE}, the minimum
#'   number of consecutive days below threshold required (default: 3).
#' @param max_changepoints Maximum number of changepoints to return. If more days
#'   meet criteria, those with lowest VPD are selected (default: NULL, no limit).
#'
#' @return A list containing:
#'   \item{changepoints}{Vector of changepoint dates (Date class) when VPD conditions suitable}
#'   \item{changepoint_indices}{Vector of row indices in daily_vpd corresponding to changepoints}
#'   \item{vpd_values}{VPD values at each changepoint (kPa)}
#'   \item{segments}{Data frame describing periods between changepoints}
#'   \item{daily_vpd_with_segments}{Original daily_vpd with added \code{segment_id} column}
#'   \item{parameters}{List of detection parameters used}
#'   \item{n_days_below_threshold}{Total number of days meeting VPD threshold}
#'   \item{n_changepoints_selected}{Number of changepoints after applying min_segment_days filter}
#'
#' @details
#' **VPD Threshold Approach:**
#'
#' Unlike PELT changepoint detection which identifies statistical changes in baseline,
#' this function identifies dates based on environmental suitability:
#' \itemize{
#'   \item Selects days when VPD ≤ threshold (low atmospheric demand)
#'   \item Ensures minimum spacing between selected dates
#'   \item Optionally requires consecutive stable days
#'   \item Returns dates as "changepoints" for spacing correction workflow
#' }
#'
#' **Threshold Selection:**
#'
#' Choose threshold based on your site and objectives:
#' \itemize{
#'   \item **0.3 kPa**: Very stable, low demand conditions (few dates selected)
#'   \item **0.5 kPa**: Moderate stability, suitable for most applications (recommended)
#'   \item **0.8 kPa**: Permissive, allows moderate VPD (more dates available)
#' }
#'
#' **Minimum Segment Days:**
#'
#' Spacing between changepoints (\code{min_segment_days}) ensures:
#' \itemize{
#'   \item Independent calibration periods
#'   \item Sufficient time between corrections
#'   \item Avoids over-fitting to short-term VPD fluctuations
#' }
#'
#' **Consecutive Days Requirement:**
#'
#' If \code{require_consecutive = TRUE}:
#' \itemize{
#'   \item Ensures sustained low VPD, not just isolated days
#'   \item More conservative selection
#'   \item Selects middle day of consecutive period as changepoint
#'   \item Better for sites with variable weather
#' }
#'
#' **Integration with Spacing Correction:**
#'
#' The detected changepoint dates can be passed to spacing correction functions
#' to define periods for separate calibration:
#' \itemize{
#'   \item Each segment between changepoints gets its own spacing correction
#'   \item Corrections applied when environmental conditions are stable
#'   \item Analogous to PELT-based segmentation but environmentally driven
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate daily VPD minima
#' weather <- read_weather_data("weather_station.csv")
#' weather_vpd <- calc_vpd(weather)
#' daily_vpd <- calculate_daily_vpd_minima(weather_vpd)
#'
#' # Detect changepoints with default threshold (0.5 kPa)
#' vpd_cpts <- detect_vpd_changepoints(daily_vpd)
#'
#' # View selected dates
#' print(vpd_cpts$changepoints)
#' print(vpd_cpts$vpd_values)
#'
#' # Stricter criteria: lower threshold, require consecutive days
#' vpd_cpts_strict <- detect_vpd_changepoints(
#'   daily_vpd,
#'   vpd_threshold = 0.3,
#'   require_consecutive = TRUE,
#'   min_consecutive_days = 3,
#'   min_segment_days = 14
#' )
#'
#' # Permissive criteria: higher threshold
#' vpd_cpts_permissive <- detect_vpd_changepoints(
#'   daily_vpd,
#'   vpd_threshold = 0.8,
#'   min_segment_days = 7
#' )
#'
#' # Limit number of changepoints, select lowest VPD days
#' vpd_cpts_limited <- detect_vpd_changepoints(
#'   daily_vpd,
#'   vpd_threshold = 0.5,
#'   max_changepoints = 10
#' )
#' }
#'
#' @family VPD changepoint functions
#' @seealso \code{\link{calculate_daily_vpd_minima}} for preparing daily VPD data
#' @export
detect_vpd_changepoints <- function(daily_vpd,
                                     vpd_threshold = 0.5,
                                     min_segment_days = 7,
                                     require_consecutive = FALSE,
                                     min_consecutive_days = 3,
                                     max_changepoints = NULL) {

  # Input validation
  if (!is.data.frame(daily_vpd)) {
    stop("daily_vpd must be a data frame")
  }

  required_cols <- c("date", "min_vpd")
  missing_cols <- setdiff(required_cols, names(daily_vpd))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (vpd_threshold <= 0) {
    stop("vpd_threshold must be positive")
  }

  if (min_segment_days < 1) {
    stop("min_segment_days must be at least 1")
  }

  if (require_consecutive && min_consecutive_days < 2) {
    stop("min_consecutive_days must be at least 2 when require_consecutive = TRUE")
  }

  # Ensure date column is Date class
  if (!inherits(daily_vpd$date, "Date")) {
    daily_vpd$date <- as.Date(daily_vpd$date)
  }

  # Sort by date
  daily_vpd <- daily_vpd[order(daily_vpd$date), ]

  # Identify days below threshold
  below_threshold <- daily_vpd$min_vpd <= vpd_threshold & !is.na(daily_vpd$min_vpd)
  n_days_below <- sum(below_threshold)

  if (n_days_below == 0) {
    warning("No days found with VPD <= ", vpd_threshold, " kPa. ",
            "Consider increasing vpd_threshold. ",
            "Minimum VPD in data: ", sprintf("%.3f", min(daily_vpd$min_vpd, na.rm = TRUE)), " kPa")

    return(list(
      changepoints = as.Date(character(0)),
      changepoint_indices = integer(0),
      vpd_values = numeric(0),
      segments = data.frame(
        segment_id = integer(0),
        start_date = as.Date(character(0)),
        end_date = as.Date(character(0)),
        n_days = integer(0)
      ),
      daily_vpd_with_segments = daily_vpd,
      parameters = list(
        vpd_threshold = vpd_threshold,
        min_segment_days = min_segment_days,
        require_consecutive = require_consecutive,
        min_consecutive_days = if (require_consecutive) min_consecutive_days else NA,
        max_changepoints = max_changepoints
      ),
      n_days_below_threshold = 0,
      n_changepoints_selected = 0
    ))
  }

  # Get candidate indices
  candidate_indices <- which(below_threshold)

  # If require_consecutive, filter to middle days of consecutive sequences
  if (require_consecutive) {
    # Find consecutive sequences
    consecutive_groups <- find_consecutive_sequences(
      candidate_indices,
      min_length = min_consecutive_days
    )

    if (length(consecutive_groups) == 0) {
      warning("No sequences of ", min_consecutive_days, " consecutive days below threshold found. ",
              "Consider reducing min_consecutive_days or increasing vpd_threshold.")

      return(list(
        changepoints = as.Date(character(0)),
        changepoint_indices = integer(0),
        vpd_values = numeric(0),
        segments = data.frame(
          segment_id = integer(0),
          start_date = as.Date(character(0)),
          end_date = as.Date(character(0)),
          n_days = integer(0)
        ),
        daily_vpd_with_segments = daily_vpd,
        parameters = list(
          vpd_threshold = vpd_threshold,
          min_segment_days = min_segment_days,
          require_consecutive = require_consecutive,
          min_consecutive_days = min_consecutive_days,
          max_changepoints = max_changepoints
        ),
        n_days_below_threshold = n_days_below,
        n_changepoints_selected = 0
      ))
    }

    # Select middle day of each consecutive sequence
    candidate_indices <- sapply(consecutive_groups, function(seq_indices) {
      seq_indices[ceiling(length(seq_indices) / 2)]
    })
  }

  # Apply minimum segment spacing
  if (length(candidate_indices) > 1 && min_segment_days > 1) {
    candidate_indices <- filter_by_minimum_spacing(
      candidate_indices,
      daily_vpd$date,
      daily_vpd$min_vpd,
      min_spacing_days = min_segment_days
    )
  }

  # Limit to max_changepoints if specified
  if (!is.null(max_changepoints) && length(candidate_indices) > max_changepoints) {
    # Select days with lowest VPD
    vpd_at_candidates <- daily_vpd$min_vpd[candidate_indices]
    keep_indices <- order(vpd_at_candidates)[1:max_changepoints]
    candidate_indices <- candidate_indices[keep_indices]
    candidate_indices <- sort(candidate_indices)  # Re-sort by date
  }

  # Extract changepoint dates and VPD values
  changepoint_dates <- daily_vpd$date[candidate_indices]
  vpd_values <- daily_vpd$min_vpd[candidate_indices]

  # Create segments between changepoints
  segments <- create_vpd_segments(
    changepoint_dates = changepoint_dates,
    date_range = range(daily_vpd$date)
  )

  # Assign segment IDs to daily_vpd
  daily_vpd_with_segments <- assign_segment_ids(
    daily_vpd = daily_vpd,
    changepoint_dates = changepoint_dates
  )

  # Return results in same format as PELT changepoints
  result <- list(
    changepoints = changepoint_dates,
    changepoint_indices = candidate_indices,
    vpd_values = vpd_values,
    segments = segments,
    daily_vpd_with_segments = daily_vpd_with_segments,
    parameters = list(
      vpd_threshold = vpd_threshold,
      min_segment_days = min_segment_days,
      require_consecutive = require_consecutive,
      min_consecutive_days = if (require_consecutive) min_consecutive_days else NA,
      max_changepoints = max_changepoints
    ),
    n_days_below_threshold = n_days_below,
    n_changepoints_selected = length(changepoint_dates)
  )

  class(result) <- c("vpd_changepoints", "list")

  return(result)
}


#' Find Consecutive Sequences
#'
#' Helper function to identify consecutive sequences of indices.
#'
#' @param indices Vector of indices
#' @param min_length Minimum length of sequence to return
#'
#' @return List of integer vectors, each containing a consecutive sequence
#' @keywords internal
find_consecutive_sequences <- function(indices, min_length = 1) {
  if (length(indices) == 0) {
    return(list())
  }

  # Find breaks in consecutive indices
  breaks <- which(diff(indices) > 1)

  # Create sequence groups
  if (length(breaks) == 0) {
    # All indices are consecutive
    sequences <- list(indices)
  } else {
    # Split at breaks
    start_points <- c(1, breaks + 1)
    end_points <- c(breaks, length(indices))

    sequences <- mapply(
      function(s, e) indices[s:e],
      start_points,
      end_points,
      SIMPLIFY = FALSE
    )
  }

  # Filter to minimum length
  sequences <- Filter(function(seq) length(seq) >= min_length, sequences)

  return(sequences)
}


#' Filter Indices by Minimum Spacing
#'
#' Ensures minimum spacing between selected indices, keeping those with lowest VPD.
#'
#' @param indices Vector of candidate indices
#' @param dates Vector of dates corresponding to indices
#' @param vpd_values VPD values corresponding to indices
#' @param min_spacing_days Minimum days between selected indices
#'
#' @return Filtered vector of indices
#' @keywords internal
filter_by_minimum_spacing <- function(indices, dates, vpd_values, min_spacing_days) {
  if (length(indices) <= 1) {
    return(indices)
  }

  # Start with all candidates
  selected <- integer(0)
  candidates_remaining <- indices

  while (length(candidates_remaining) > 0) {
    # Select candidate with lowest VPD
    vpd_remaining <- vpd_values[candidates_remaining]
    best_idx <- candidates_remaining[which.min(vpd_remaining)[1]]

    # Add to selected
    selected <- c(selected, best_idx)

    # Remove candidates within min_spacing_days
    best_date <- dates[best_idx]
    date_diffs <- abs(as.numeric(difftime(dates[candidates_remaining], best_date, units = "days")))

    candidates_remaining <- candidates_remaining[date_diffs >= min_spacing_days]
  }

  # Sort by date
  selected <- sort(selected)

  return(selected)
}


#' Create VPD Segments
#'
#' Creates segment data frame from changepoint dates.
#'
#' @param changepoint_dates Vector of changepoint dates
#' @param date_range Range of all dates (min, max)
#'
#' @return Data frame with segment information
#' @keywords internal
create_vpd_segments <- function(changepoint_dates, date_range) {
  if (length(changepoint_dates) == 0) {
    # Single segment covering entire range
    return(data.frame(
      segment_id = 1,
      start_date = date_range[1],
      end_date = date_range[2],
      n_days = as.numeric(difftime(date_range[2], date_range[1], units = "days")) + 1
    ))
  }

  # Create segment boundaries
  n_segments <- length(changepoint_dates) + 1

  segment_starts <- c(date_range[1], changepoint_dates)
  segment_ends <- c(changepoint_dates - 1, date_range[2])

  segments <- data.frame(
    segment_id = 1:n_segments,
    start_date = segment_starts,
    end_date = segment_ends,
    n_days = as.numeric(difftime(segment_ends, segment_starts, units = "days")) + 1
  )

  return(segments)
}


#' Assign Segment IDs to Daily VPD Data
#'
#' Assigns segment IDs based on changepoint dates.
#'
#' @param daily_vpd Data frame with date column
#' @param changepoint_dates Vector of changepoint dates
#'
#' @return daily_vpd with added segment_id column
#' @keywords internal
assign_segment_ids <- function(daily_vpd, changepoint_dates) {
  if (length(changepoint_dates) == 0) {
    daily_vpd$segment_id <- 1
    return(daily_vpd)
  }

  # Use findInterval to assign segments
  # Segments: [start, cpt1), [cpt1, cpt2), ..., [cptN, end]
  segment_id <- findInterval(daily_vpd$date, changepoint_dates) + 1

  daily_vpd$segment_id <- segment_id

  return(daily_vpd)
}


#' Print Method for VPD Changepoints
#'
#' @param x A vpd_changepoints object
#' @param ... Additional arguments (not used)
#' @export
print.vpd_changepoints <- function(x, ...) {
  cat("VPD-Based Changepoints\n")
  cat("======================\n\n")

  cat("Parameters:\n")
  cat("  VPD threshold:", x$parameters$vpd_threshold, "kPa\n")
  cat("  Min segment days:", x$parameters$min_segment_days, "\n")
  if (x$parameters$require_consecutive) {
    cat("  Consecutive days required:", x$parameters$min_consecutive_days, "\n")
  }
  if (!is.null(x$parameters$max_changepoints)) {
    cat("  Max changepoints:", x$parameters$max_changepoints, "\n")
  }
  cat("\n")

  cat("Results:\n")
  cat("  Days below threshold:", x$n_days_below_threshold, "\n")
  cat("  Changepoints selected:", x$n_changepoints_selected, "\n")
  cat("  Segments created:", nrow(x$segments), "\n\n")

  if (x$n_changepoints_selected > 0) {
    cat("Changepoint Dates and VPD Values:\n")
    cpt_df <- data.frame(
      Date = format(x$changepoints),
      VPD_kPa = sprintf("%.3f", x$vpd_values)
    )
    print(cpt_df, row.names = FALSE)
    cat("\n")

    cat("Segments:\n")
    print(x$segments)
  } else {
    cat("No changepoints detected with current parameters.\n")
  }

  invisible(x)
}


# Stable VPD Period Detection (Full-Resolution Analysis) ----------------------

#' Detect Stable Low VPD Periods for Zero-Flow Calibration
#'
#' Identifies dates when VPD during pre-dawn hours is both consistently low
#' AND stable, indicating suitable conditions for zero-flow calibration.
#' Unlike \code{\link{detect_vpd_changepoints}} which uses daily minimum values,
#' this function analyses full-resolution weather data during specific hours
#' to assess both magnitude and stability.
#'
#' @param weather_data Data frame containing weather data with columns:
#'   \code{datetime} (POSIXct) and \code{vpd_kpa} (numeric)
#' @param predawn_window Integer vector of hours (0-23) to analyse (default: \code{c(2, 3, 4, 5)}).
#'   These hours are typically pre-dawn when VPD is lowest and most stable.
#' @param vpd_threshold Maximum acceptable mean VPD during pre-dawn window (kPa, default: 0.5).
#'   Days with mean pre-dawn VPD above this value are rejected.
#' @param stability_threshold Maximum acceptable standard deviation of VPD during
#'   pre-dawn window (kPa, default: 0.1). Higher values indicate unstable conditions.
#' @param min_n_points Minimum number of data points required in the pre-dawn window
#'   for a day to be valid (default: 3). Days with fewer points are skipped.
#' @param min_segment_days Minimum number of days between selected dates (default: 7).
#'   If valid dates are closer than this, only the most stable day in that period
#'   is retained.
#' @param max_changepoints Maximum number of dates to return (default: NULL, no limit).
#'   If more days meet criteria, those with lowest mean VPD are selected.
#' @param vpd_col Name of VPD column (default: "vpd_kpa")
#'
#' @return A list (S3 class "stable_vpd_periods") containing:
#'   \item{valid_dates}{Vector of Date objects that passed both magnitude and stability checks}
#'   \item{daily_stats}{Data frame with statistics for every day:
#'     \itemize{
#'       \item \code{date} - Date
#'       \item \code{n_points} - Number of observations in pre-dawn window
#'       \item \code{mean_predawn_vpd} - Mean VPD during pre-dawn hours (kPa)
#'       \item \code{sd_predawn_vpd} - Standard deviation of VPD (kPa)
#'       \item \code{min_predawn_vpd} - Minimum VPD in window (kPa)
#'       \item \code{max_predawn_vpd} - Maximum VPD in window (kPa)
#'       \item \code{passed_magnitude} - Did mean VPD meet threshold?
#'       \item \code{passed_stability} - Did SD meet threshold?
#'       \item \code{passed_both} - Passed both checks?
#'     }
#'   }
#'   \item{segments}{Data frame describing periods between selected dates}
#'   \item{parameters}{List of detection parameters used}
#'   \item{n_days_analysed}{Total number of days with sufficient data}
#'   \item{n_days_passed_both}{Number of days passing both checks}
#'   \item{n_dates_selected}{Number of dates selected after spacing filter}
#'
#' @details
#' **Methodology:**
#'
#' This function implements a "stability-based" approach inspired by legacy
#' zero-flow detection methods. For each date:
#'
#' 1. **Extract pre-dawn data**: Filter to specified hours (e.g., 02:00-06:00)
#' 2. **Check magnitude**: Is mean VPD consistently low? (mean ≤ threshold)
#' 3. **Check stability**: Is VPD stable? (SD ≤ stability_threshold)
#' 4. **Validate**: Sufficient data points? (n ≥ min_n_points)
#'
#' Only dates passing ALL criteria are considered valid.
#'
#' **Differences from detect_vpd_changepoints():**
#'
#' \itemize{
#'   \item \strong{Daily minima} (changepoints): Uses single minimum value per day
#'   \item \strong{Stability periods} (this function): Analyses full-resolution data during specific hours
#' }
#'
#' The stability approach is more conservative - it rejects days where VPD briefly
#' drops to a low value but is otherwise unstable or elevated.
#'
#' **Pre-dawn Window Selection:**
#'
#' Default hours (02:00-06:00) are typically pre-dawn when:
#' \itemize{
#'   \item VPD is at daily minimum (high humidity, low temperature)
#'   \item Atmospheric conditions are most stable
#'   \item Sap flow is minimal or zero
#'   \item Environmental noise is minimal
#' }
#'
#' Adjust \code{predawn_window} based on your site's sunrise time and climate.
#'
#' **Threshold Selection:**
#'
#' \itemize{
#'   \item \strong{vpd_threshold}: 0.3 kPa (strict), 0.5 kPa (moderate), 0.8 kPa (permissive)
#'   \item \strong{stability_threshold}: 0.05 kPa (very stable), 0.1 kPa (moderate), 0.15 kPa (permissive)
#' }
#'
#' Stricter thresholds yield fewer but higher-quality calibration dates.
#'
#' **Minimum Segment Days:**
#'
#' The \code{min_segment_days} parameter ensures temporal independence between
#' selected dates. When multiple valid dates occur within this window, only
#' the date with lowest mean VPD is retained.
#'
#' **Use Cases:**
#'
#' \itemize{
#'   \item Zero-flow calibration (detecting true zero-flow conditions)
#'   \item Spacing correction (stable environmental baseline)
#'   \item Method comparison (periods with minimal environmental influence)
#'   \item Quality control (identifying stable measurement periods)
#' }
#'
#' @examples
#' \dontrun{
#' # Import and process weather data
#' weather <- read_weather_data("weather_station.csv")
#' weather_vpd <- calc_vpd(weather)
#'
#' # Detect stable pre-dawn periods (default settings)
#' stable_periods <- detect_stable_vpd_periods(weather_vpd)
#'
#' # View selected dates and statistics
#' print(stable_periods)
#' print(stable_periods$valid_dates)
#' View(stable_periods$daily_stats)
#'
#' # Stricter criteria: lower thresholds, more data required
#' stable_strict <- detect_stable_vpd_periods(
#'   weather_vpd,
#'   vpd_threshold = 0.3,
#'   stability_threshold = 0.05,
#'   min_n_points = 6,
#'   min_segment_days = 14
#' )
#'
#' # Custom pre-dawn window (earlier sunrise site)
#' stable_custom <- detect_stable_vpd_periods(
#'   weather_vpd,
#'   predawn_window = c(1, 2, 3, 4),
#'   vpd_threshold = 0.5,
#'   stability_threshold = 0.1
#' )
#'
#' # Compare with daily minima approach
#' daily_vpd <- calculate_daily_vpd_minima(weather_vpd)
#' minima_cpts <- detect_vpd_changepoints(daily_vpd, vpd_threshold = 0.5)
#'
#' # Compare selected dates
#' stable_dates <- stable_periods$valid_dates
#' minima_dates <- minima_cpts$changepoints
#' both <- intersect(stable_dates, minima_dates)
#' stability_only <- setdiff(stable_dates, minima_dates)
#' minima_only <- setdiff(minima_dates, stable_dates)
#' }
#'
#' @family VPD changepoint functions
#' @seealso
#'   \code{\link{detect_vpd_changepoints}} for daily minima approach,
#'   \code{\link{calculate_daily_vpd_minima}} for daily VPD statistics
#' @export
detect_stable_vpd_periods <- function(weather_data,
                                       predawn_window = c(2, 3, 4, 5),
                                       vpd_threshold = 0.5,
                                       stability_threshold = 0.1,
                                       min_n_points = 3,
                                       min_segment_days = 7,
                                       max_changepoints = NULL,
                                       vpd_col = "vpd_kpa") {

  # Input validation
  if (!is.data.frame(weather_data)) {
    stop("weather_data must be a data frame")
  }

  required_cols <- c("datetime", vpd_col)
  missing_cols <- setdiff(required_cols, names(weather_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!inherits(weather_data$datetime, "POSIXct")) {
    weather_data$datetime <- as.POSIXct(weather_data$datetime)
  }

  if (!is.numeric(predawn_window) || any(predawn_window < 0) || any(predawn_window > 23)) {
    stop("predawn_window must contain hours between 0 and 23")
  }

  if (vpd_threshold <= 0) {
    stop("vpd_threshold must be positive")
  }

  if (stability_threshold < 0) {
    stop("stability_threshold must be non-negative")
  }

  if (min_n_points < 1) {
    stop("min_n_points must be at least 1")
  }

  if (min_segment_days < 1) {
    stop("min_segment_days must be at least 1")
  }

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed")
  }

  # Extract date and hour
  weather_data$date <- as.Date(weather_data$datetime)
  weather_data$hour <- lubridate::hour(weather_data$datetime)

  # Filter to pre-dawn window
  predawn_data <- weather_data[weather_data$hour %in% predawn_window, ]

  if (nrow(predawn_data) == 0) {
    stop("No data found in specified predawn_window hours: ",
         paste(predawn_window, collapse = ", "))
  }

  # Calculate daily statistics for pre-dawn window
  daily_stats <- predawn_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_points = sum(!is.na(.data[[vpd_col]])),
      mean_predawn_vpd = mean(.data[[vpd_col]], na.rm = TRUE),
      sd_predawn_vpd = sd(.data[[vpd_col]], na.rm = TRUE),
      min_predawn_vpd = min(.data[[vpd_col]], na.rm = TRUE),
      max_predawn_vpd = max(.data[[vpd_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()

  # Filter to days with sufficient data
  daily_stats <- daily_stats[daily_stats$n_points >= min_n_points, ]

  n_days_analysed <- nrow(daily_stats)

  if (n_days_analysed == 0) {
    warning("No days have sufficient data (min_n_points = ", min_n_points, ") ",
            "in pre-dawn window. Consider reducing min_n_points or checking data.")

    return(create_empty_stable_vpd_result(
      vpd_threshold = vpd_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints
    ))
  }

  # Check magnitude criterion (mean VPD ≤ threshold)
  daily_stats$passed_magnitude <- daily_stats$mean_predawn_vpd <= vpd_threshold &
    !is.na(daily_stats$mean_predawn_vpd)

  # Check stability criterion (SD ≤ stability_threshold)
  # Handle case where SD is NA (only 1 point, or all values identical)
  daily_stats$passed_stability <- (daily_stats$sd_predawn_vpd <= stability_threshold |
                                      is.na(daily_stats$sd_predawn_vpd)) &
    !is.na(daily_stats$mean_predawn_vpd)

  # Both criteria must pass
  daily_stats$passed_both <- daily_stats$passed_magnitude & daily_stats$passed_stability

  # Count days passing both checks
  n_days_passed_both <- sum(daily_stats$passed_both, na.rm = TRUE)

  if (n_days_passed_both == 0) {
    warning("No days passed both magnitude and stability checks. ",
            "Minimum mean VPD: ", sprintf("%.3f", min(daily_stats$mean_predawn_vpd, na.rm = TRUE)), " kPa. ",
            "Consider increasing vpd_threshold or stability_threshold.")

    result <- create_empty_stable_vpd_result(
      vpd_threshold = vpd_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints
    )
    result$daily_stats <- daily_stats
    result$n_days_analysed <- n_days_analysed

    return(result)
  }

  # Get candidate dates
  candidate_dates <- daily_stats$date[daily_stats$passed_both]
  candidate_vpd <- daily_stats$mean_predawn_vpd[daily_stats$passed_both]

  # Apply minimum segment spacing
  if (length(candidate_dates) > 1 && min_segment_days > 1) {
    selected_indices <- filter_stable_vpd_by_spacing(
      dates = candidate_dates,
      vpd_values = candidate_vpd,
      min_spacing_days = min_segment_days
    )
    candidate_dates <- candidate_dates[selected_indices]
    candidate_vpd <- candidate_vpd[selected_indices]
  }

  # Limit to max_changepoints if specified
  if (!is.null(max_changepoints) && length(candidate_dates) > max_changepoints) {
    # Select days with lowest mean VPD
    keep_indices <- order(candidate_vpd)[1:max_changepoints]
    candidate_dates <- candidate_dates[keep_indices]
    candidate_vpd <- candidate_vpd[keep_indices]

    # Re-sort by date
    sort_order <- order(candidate_dates)
    candidate_dates <- candidate_dates[sort_order]
    candidate_vpd <- candidate_vpd[sort_order]
  }

  # Create segments
  date_range <- range(weather_data$date)
  segments <- create_vpd_segments(
    changepoint_dates = candidate_dates,
    date_range = date_range
  )

  # Assemble result
  result <- list(
    valid_dates = candidate_dates,
    daily_stats = daily_stats,
    segments = segments,
    parameters = list(
      vpd_threshold = vpd_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints,
      vpd_col = vpd_col
    ),
    n_days_analysed = n_days_analysed,
    n_days_passed_both = n_days_passed_both,
    n_dates_selected = length(candidate_dates)
  )

  class(result) <- c("stable_vpd_periods", "list")

  return(result)
}


#' Filter Stable VPD Dates by Minimum Spacing
#'
#' Ensures minimum spacing between selected dates, keeping those with lowest mean VPD.
#'
#' @param dates Vector of candidate dates
#' @param vpd_values VPD values corresponding to dates
#' @param min_spacing_days Minimum days between selected dates
#'
#' @return Integer vector of indices to keep
#' @keywords internal
filter_stable_vpd_by_spacing <- function(dates, vpd_values, min_spacing_days) {
  if (length(dates) <= 1) {
    return(seq_along(dates))
  }

  # Start with all candidates
  selected_indices <- integer(0)
  candidates_remaining <- seq_along(dates)

  while (length(candidates_remaining) > 0) {
    # Select candidate with lowest VPD
    vpd_remaining <- vpd_values[candidates_remaining]
    best_local_idx <- which.min(vpd_remaining)[1]
    best_idx <- candidates_remaining[best_local_idx]

    # Add to selected
    selected_indices <- c(selected_indices, best_idx)

    # Remove candidates within min_spacing_days
    best_date <- dates[best_idx]
    date_diffs <- abs(as.numeric(difftime(dates[candidates_remaining], best_date, units = "days")))

    candidates_remaining <- candidates_remaining[date_diffs >= min_spacing_days]
  }

  # Sort by date order
  selected_indices <- sort(selected_indices)

  return(selected_indices)
}


#' Create Empty Stable VPD Result
#'
#' Helper function to create empty result structure when no dates found.
#'
#' @param vpd_threshold VPD threshold parameter
#' @param stability_threshold Stability threshold parameter
#' @param predawn_window Pre-dawn window parameter
#' @param min_n_points Minimum points parameter
#' @param min_segment_days Minimum segment days parameter
#' @param max_changepoints Maximum changepoints parameter
#'
#' @return Empty stable_vpd_periods object
#' @keywords internal
create_empty_stable_vpd_result <- function(vpd_threshold,
                                            stability_threshold,
                                            predawn_window,
                                            min_n_points,
                                            min_segment_days,
                                            max_changepoints) {
  result <- list(
    valid_dates = as.Date(character(0)),
    daily_stats = data.frame(
      date = as.Date(character(0)),
      n_points = integer(0),
      mean_predawn_vpd = numeric(0),
      sd_predawn_vpd = numeric(0),
      min_predawn_vpd = numeric(0),
      max_predawn_vpd = numeric(0),
      passed_magnitude = logical(0),
      passed_stability = logical(0),
      passed_both = logical(0)
    ),
    segments = data.frame(
      segment_id = integer(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      n_days = integer(0)
    ),
    parameters = list(
      vpd_threshold = vpd_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints
    ),
    n_days_analysed = 0,
    n_days_passed_both = 0,
    n_dates_selected = 0
  )

  class(result) <- c("stable_vpd_periods", "list")

  return(result)
}


#' Print Method for Stable VPD Periods
#'
#' @param x A stable_vpd_periods object
#' @param ... Additional arguments (not used)
#' @export
print.stable_vpd_periods <- function(x, ...) {
  cat("Stable VPD Periods Detection\n")
  cat("============================\n\n")

  cat("Parameters:\n")
  cat("  Pre-dawn window:", paste(x$parameters$predawn_window, collapse = ", "), "hours\n")
  cat("  VPD threshold:", x$parameters$vpd_threshold, "kPa (mean)\n")
  cat("  Stability threshold:", x$parameters$stability_threshold, "kPa (SD)\n")
  cat("  Min data points:", x$parameters$min_n_points, "\n")
  cat("  Min segment days:", x$parameters$min_segment_days, "\n")
  if (!is.null(x$parameters$max_changepoints)) {
    cat("  Max dates:", x$parameters$max_changepoints, "\n")
  }
  cat("\n")

  cat("Results:\n")
  cat("  Days analysed:", x$n_days_analysed, "\n")
  cat("  Days passed both checks:", x$n_days_passed_both, "\n")
  cat("  Dates selected:", x$n_dates_selected, "\n")
  cat("  Segments created:", nrow(x$segments), "\n\n")

  if (x$n_dates_selected > 0) {
    cat("Selected Dates and Pre-dawn VPD:\n")
    selected_stats <- x$daily_stats[x$daily_stats$passed_both, ]
    # Further filter to those actually selected (after spacing)
    selected_stats <- selected_stats[selected_stats$date %in% x$valid_dates, ]

    date_df <- data.frame(
      Date = format(selected_stats$date),
      Mean_VPD_kPa = sprintf("%.3f", selected_stats$mean_predawn_vpd),
      SD_VPD_kPa = sprintf("%.3f", selected_stats$sd_predawn_vpd),
      N_Points = selected_stats$n_points
    )
    print(date_df, row.names = FALSE)
    cat("\n")

    cat("Summary Statistics:\n")
    cat("  Days passed magnitude only:",
        sum(x$daily_stats$passed_magnitude & !x$daily_stats$passed_stability, na.rm = TRUE), "\n")
    cat("  Days passed stability only:",
        sum(!x$daily_stats$passed_magnitude & x$daily_stats$passed_stability, na.rm = TRUE), "\n")
    cat("  Days passed both:",
        sum(x$daily_stats$passed_both, na.rm = TRUE), "\n")
    cat("  After spacing filter:",
        x$n_dates_selected, "\n")
  } else {
    cat("No dates met both magnitude and stability criteria.\n")
    if (x$n_days_analysed > 0) {
      cat("\nDiagnostic Information:\n")
      cat("  Days passed magnitude check:",
          sum(x$daily_stats$passed_magnitude, na.rm = TRUE), "\n")
      cat("  Days passed stability check:",
          sum(x$daily_stats$passed_stability, na.rm = TRUE), "\n")
      cat("  Minimum mean VPD:",
          sprintf("%.3f", min(x$daily_stats$mean_predawn_vpd, na.rm = TRUE)), "kPa\n")
      cat("  Minimum SD VPD:",
          sprintf("%.3f", min(x$daily_stats$sd_predawn_vpd, na.rm = TRUE)), "kPa\n")
    }
  }

  invisible(x)
}
