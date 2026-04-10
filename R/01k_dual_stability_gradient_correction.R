# R/01k_dual_stability_gradient_correction.R
# Dual-Criterion Stability Detection with Gradient Correction
#
# Enhanced zero-offset correction that requires BOTH stable VPD AND stable sap flow
# during predawn periods. Applies gradient (linear interpolation) between changepoints
# rather than step-wise correction.

#' Find Stable Sap Flow Periods
#'
#' Identifies dates when predawn sap flow is both low and stable, analogous to
#' VPD stability detection (see \code{\link{detect_stable_vpd_periods}}) but
#' applied to sap flow data.
#'
#' @param vh_data Data frame containing sap flow data with columns:
#'   \code{datetime} and a sap flow column (specified by \code{vh_col})
#' @param vh_col Name of sap flow column (e.g., "Vh_cm_hr")
#' @param predawn_window Vector of two integers defining predawn hours (e.g., c(2, 6))
#'   to analyse hours 2, 3, 4, 5. Default: c(2, 6)
#' @param vh_threshold Maximum mean sap flow during predawn (cm/hr). Default: 2.0
#' @param stability_threshold Maximum SD of sap flow during predawn (cm/hr). Default: 0.5
#' @param min_n_points Minimum number of observations required per day. Default: 4
#' @param min_segment_days Minimum days between selected dates. Default: 7
#' @param max_changepoints Maximum number of dates to select. Default: NULL (no limit)
#'
#' @return A list (S3 class "stable_vh_periods") containing:
#'   \item{valid_dates}{Vector of dates meeting stability criteria}
#'   \item{daily_stats}{Data frame with daily statistics and pass/fail flags}
#'   \item{segments}{Data frame describing periods between changepoints}
#'   \item{parameters}{List of detection parameters used}
#'   \item{n_days_analysed}{Total days with sufficient data}
#'   \item{n_days_passed_both}{Days passing both magnitude and stability}
#'   \item{n_dates_selected}{Final number of dates after spacing filter}
#'
#' @details
#' This function mirrors the VPD stability detection logic but applies it to
#' sap flow measurements. It identifies predawn periods when:
#' \itemize{
#'   \item Sap flow is low (minimal water movement)
#'   \item Sap flow is stable (consistent baseline)
#'   \item Sufficient data points available
#' }
#'
#' **Methodology:**
#'
#' For each date:
#' 1. Extract predawn window data (e.g., 02:00-06:00)
#' 2. Calculate mean and SD of sap flow
#' 3. Check magnitude criterion: mean <= vh_threshold
#' 4. Check stability criterion: SD <= stability_threshold
#' 5. Require both criteria to pass
#'
#' **Parameter Tuning:**
#'
#' Adjust thresholds based on species and flow rates:
#' \itemize{
#'   \item Conifers: Lower vh_threshold (0.5-1.5 cm/hr)
#'   \item Fast-growing hardwoods: Higher vh_threshold (2.5-4.0 cm/hr)
#'   \item Very stable: Lower stability_threshold (0.3 cm/hr)
#'   \item Permissive: Higher stability_threshold (0.8 cm/hr)
#' }
#'
#' @examples
#' \dontrun{
#' # Detect stable sap flow periods
#' stable_vh <- find_stable_vh_dates(
#'   vh_data = vh_results,
#'   vh_col = "Vh_cm_hr",
#'   predawn_window = c(2, 6),
#'   vh_threshold = 2.0,
#'   stability_threshold = 0.5
#' )
#'
#' # View results
#' print(stable_vh)
#' print(stable_vh$valid_dates)
#' }
#'
#' @family dual stability functions
#' @seealso
#'   \code{\link{detect_stable_vpd_periods}} for VPD stability detection,
#'   \code{\link{find_dual_stable_periods}} for combining VPD and vh criteria
#' @export
find_stable_vh_dates <- function(vh_data,
                                  vh_col,
                                  predawn_window = c(2, 6),
                                  vh_threshold = 2.0,
                                  stability_threshold = 0.5,
                                  min_n_points = 4,
                                  min_segment_days = 7,
                                  max_changepoints = NULL) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed")
  }

  # Extract hour and date
  vh_data$hour <- lubridate::hour(vh_data$datetime)
  vh_data$date <- as.Date(vh_data$datetime)

  # Filter to predawn window (e.g., hours 2, 3, 4, 5 for window c(2, 6))
  predawn_hours <- seq(predawn_window[1], predawn_window[2] - 1)
  predawn_data <- vh_data[vh_data$hour %in% predawn_hours, ]

  if (nrow(predawn_data) == 0) {
    return(create_empty_stable_vh_result(
      vh_threshold, stability_threshold, predawn_window,
      min_n_points, min_segment_days, max_changepoints, vh_col
    ))
  }

  # Calculate daily statistics for predawn period
  daily_stats <- predawn_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_points = sum(!is.na(.data[[vh_col]])),
      mean_predawn_vh = mean(.data[[vh_col]], na.rm = TRUE),
      sd_predawn_vh = sd(.data[[vh_col]], na.rm = TRUE),
      min_predawn_vh = min(.data[[vh_col]], na.rm = TRUE),
      max_predawn_vh = max(.data[[vh_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(date) %>%
    as.data.frame()

  # Filter by minimum data points
  daily_stats <- daily_stats[daily_stats$n_points >= min_n_points, ]
  n_days_analysed <- nrow(daily_stats)

  if (n_days_analysed == 0) {
    return(create_empty_stable_vh_result(
      vh_threshold, stability_threshold, predawn_window,
      min_n_points, min_segment_days, max_changepoints, vh_col
    ))
  }

  # Apply magnitude and stability criteria
  daily_stats$passed_magnitude <- daily_stats$mean_predawn_vh <= vh_threshold &
    !is.na(daily_stats$mean_predawn_vh)

  # Handle case where SD is NA (only 1 point or all identical)
  daily_stats$passed_stability <- (daily_stats$sd_predawn_vh <= stability_threshold |
                                      is.na(daily_stats$sd_predawn_vh)) &
    !is.na(daily_stats$mean_predawn_vh)

  daily_stats$passed_both <- daily_stats$passed_magnitude & daily_stats$passed_stability

  n_days_passed_both <- sum(daily_stats$passed_both, na.rm = TRUE)

  # Extract candidate dates
  candidate_dates <- daily_stats$date[daily_stats$passed_both]
  candidate_vh <- daily_stats$mean_predawn_vh[daily_stats$passed_both]

  if (length(candidate_dates) == 0) {
    warning("No days passed both magnitude and stability checks for sap flow. ",
            "Minimum mean vh: ", sprintf("%.3f", min(daily_stats$mean_predawn_vh, na.rm = TRUE)), " cm/hr. ",
            "Consider increasing vh_threshold or stability_threshold.")

    result <- create_empty_stable_vh_result(
      vh_threshold, stability_threshold, predawn_window,
      min_n_points, min_segment_days, max_changepoints, vh_col
    )
    result$daily_stats <- daily_stats
    result$n_days_analysed <- n_days_analysed

    return(result)
  }

  # Filter by minimum spacing
  if (min_segment_days > 0 && length(candidate_dates) > 1) {
    selected_indices <- filter_stable_by_spacing(
      candidate_dates, candidate_vh, min_segment_days
    )
    candidate_dates <- candidate_dates[selected_indices]
    candidate_vh <- candidate_vh[selected_indices]
  }

  # Limit to max_changepoints if specified
  if (!is.null(max_changepoints) && length(candidate_dates) > max_changepoints) {
    keep_indices <- order(candidate_vh)[1:max_changepoints]
    candidate_dates <- candidate_dates[keep_indices]
    candidate_vh <- candidate_vh[keep_indices]

    # Re-sort by date
    sort_order <- order(candidate_dates)
    candidate_dates <- candidate_dates[sort_order]
    candidate_vh <- candidate_vh[sort_order]
  }

  # Create segments
  date_range <- range(vh_data$date)
  segments <- create_segments_from_dates(
    changepoint_dates = candidate_dates,
    date_range = date_range
  )

  # Assemble result
  result <- list(
    valid_dates = candidate_dates,
    daily_stats = daily_stats,
    segments = segments,
    parameters = list(
      vh_threshold = vh_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints,
      vh_col = vh_col
    ),
    n_days_analysed = n_days_analysed,
    n_days_passed_both = n_days_passed_both,
    n_dates_selected = length(candidate_dates)
  )

  class(result) <- c("stable_vh_periods", "list")

  return(result)
}


#' Find Dual-Stable Periods (VPD + Sap Flow)
#'
#' Identifies dates when BOTH VPD and sap flow are stable during predawn periods.
#' This dual-criterion approach reduces false positives from nocturnal hydraulic
#' redistribution or other processes that can occur even when VPD is low.
#'
#' @param vh_data Data frame with datetime and sap flow columns
#' @param weather_data Data frame with datetime and VPD columns
#' @param vh_col Name of sap flow column
#' @param vpd_col Name of VPD column (default: "vpd_kpa")
#' @param predawn_window Predawn hours window (default: c(2, 6))
#' @param vpd_threshold Maximum mean VPD (kPa, default: 0.5)
#' @param vpd_stability Maximum SD VPD (kPa, default: 0.1)
#' @param vh_threshold Maximum mean sap flow (cm/hr, default: 2.0)
#' @param vh_stability Maximum SD sap flow (cm/hr, default: 0.5)
#' @param min_n_points Minimum observations per day (default: 4)
#' @param min_segment_days Minimum spacing between dates (default: 7)
#' @param max_changepoints Maximum dates to select (default: NULL)
#'
#' @return A list (S3 class "dual_stable_periods") containing:
#'   \item{dual_stable_dates}{Dates meeting BOTH VPD and vh criteria}
#'   \item{changepoints}{Data frame with timestamp, date, vh_value for each changepoint}
#'   \item{vpd_results}{Full VPD stability detection results}
#'   \item{vh_results}{Full sap flow stability detection results}
#'   \item{daily_stats}{Combined daily statistics}
#'   \item{parameters}{Detection parameters}
#'   \item{n_dates_dual_stable}{Number of dual-stable dates}
#'   \item{n_changepoints}{Number of changepoints identified}
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Detects VPD-stable dates using \code{\link{detect_stable_vpd_periods}}
#'   \item Detects vh-stable dates using \code{\link{find_stable_vh_dates}}
#'   \item Finds intersection (dates passing BOTH tests)
#'   \item Within each dual-stable date, identifies the exact timestamp with minimum vh
#' }
#'
#' These timestamps serve as changepoints for gradient-based zero offset correction.
#'
#' **Why Dual-Criterion?**
#'
#' VPD-only approaches can't distinguish:
#' \itemize{
#'   \item Stem refilling (capacitance-driven flow) - legitimate nighttime flow
#'   \item Hydraulic redistribution - root-to-root water transport
#'   \item Nocturnal transpiration - stomata partially open at night
#' }
#'
#' Requiring BOTH low VPD AND stable sap flow ensures true hydraulic equilibrium.
#'
#' **Scientific Basis:**
#'
#' \itemize{
#'   \item Phillips et al. (2010): Capacitance flow = 10-40% of daily water use
#'   \item Goldstein et al. (1998): Nocturnal transpiration = 5-25% of daily water loss
#'   \item VPD-only correction systematically underestimates tree water use
#' }
#'
#' @examples
#' \dontrun{
#' # Find dual-stable periods
#' dual_results <- find_dual_stable_periods(
#'   vh_data = vh_results,
#'   weather_data = weather,
#'   vh_col = "Vh_cm_hr",
#'   vpd_col = "vpd_kpa",
#'   predawn_window = c(2, 6),
#'   vpd_threshold = 0.5,
#'   vpd_stability = 0.1,
#'   vh_threshold = 2.0,
#'   vh_stability = 0.5
#' )
#'
#' # View results
#' print(dual_results)
#' print(dual_results$dual_stable_dates)
#' print(dual_results$changepoints)
#' }
#'
#' @family dual stability functions
#' @seealso
#'   \code{\link{detect_stable_vpd_periods}} for VPD detection,
#'   \code{\link{find_stable_vh_dates}} for sap flow detection,
#'   \code{\link{apply_gradient_offset_correction}} for applying corrections
#' @export
find_dual_stable_periods <- function(vh_data,
                                     weather_data,
                                     vh_col,
                                     vpd_col = "vpd_kpa",
                                     predawn_window = c(2, 6),
                                     vpd_threshold = 0.5,
                                     vpd_stability = 0.1,
                                     vh_threshold = 2.0,
                                     vh_stability = 0.5,
                                     min_n_points = 4,
                                     min_segment_days = 7,
                                     max_changepoints = NULL) {

  # Convert predawn_window to hours vector for detect_stable_vpd_periods
  predawn_hours <- seq(predawn_window[1], predawn_window[2] - 1)

  # 1. Find VPD-stable dates using existing function
  vpd_results <- detect_stable_vpd_periods(
    weather_data = weather_data,
    predawn_window = predawn_hours,
    vpd_threshold = vpd_threshold,
    stability_threshold = vpd_stability,
    min_n_points = min_n_points,
    min_segment_days = min_segment_days,
    max_changepoints = NULL,  # Don't limit yet
    vpd_col = vpd_col
  )

  # 2. Find vh-stable dates
  vh_results <- find_stable_vh_dates(
    vh_data = vh_data,
    vh_col = vh_col,
    predawn_window = predawn_window,
    vh_threshold = vh_threshold,
    stability_threshold = vh_stability,
    min_n_points = min_n_points,
    min_segment_days = min_segment_days,
    max_changepoints = NULL  # Don't limit yet
  )

  # 3. Find intersection of dates
  dual_stable_dates <- intersect(vpd_results$valid_dates, vh_results$valid_dates)

  if (length(dual_stable_dates) == 0) {
    warning("No dates passed both VPD and sap flow stability criteria. ",
            "VPD-stable: ", length(vpd_results$valid_dates), ", ",
            "vh-stable: ", length(vh_results$valid_dates), ". ",
            "Consider relaxing thresholds.")

    return(create_empty_dual_stable_result(
      vpd_results, vh_results, predawn_window, min_segment_days
    ))
  }

  # 4. Apply spacing filter to dual-stable dates
  if (min_segment_days > 0 && length(dual_stable_dates) > 1) {
    # Get mean vh values for these dates
    vh_values <- vh_results$daily_stats$mean_predawn_vh[
      match(dual_stable_dates, vh_results$daily_stats$date)
    ]

    selected_indices <- filter_stable_by_spacing(
      dual_stable_dates, vh_values, min_segment_days
    )
    dual_stable_dates <- dual_stable_dates[selected_indices]
  }

  # 5. Limit to max_changepoints
  if (!is.null(max_changepoints) && length(dual_stable_dates) > max_changepoints) {
    vh_values <- vh_results$daily_stats$mean_predawn_vh[
      match(dual_stable_dates, vh_results$daily_stats$date)
    ]
    keep_indices <- order(vh_values)[1:max_changepoints]
    dual_stable_dates <- dual_stable_dates[keep_indices]
    dual_stable_dates <- sort(dual_stable_dates)
  }

  # 6. For each dual-stable date, find timestamp of minimum vh within predawn window
  changepoints <- find_min_vh_timestamps(
    vh_data = vh_data,
    dates = dual_stable_dates,
    vh_col = vh_col,
    predawn_window = predawn_window
  )

  # 7. Merge daily statistics
  daily_stats <- merge(
    vpd_results$daily_stats,
    vh_results$daily_stats,
    by = "date",
    suffixes = c("_vpd", "_vh")
  )
  daily_stats$passed_dual <- daily_stats$date %in% dual_stable_dates

  # Assemble result
  result <- list(
    dual_stable_dates = dual_stable_dates,
    changepoints = changepoints,
    vpd_results = vpd_results,
    vh_results = vh_results,
    daily_stats = daily_stats,
    parameters = list(
      predawn_window = predawn_window,
      vpd_threshold = vpd_threshold,
      vpd_stability = vpd_stability,
      vh_threshold = vh_threshold,
      vh_stability = vh_stability,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints
    ),
    n_dates_dual_stable = length(dual_stable_dates),
    n_changepoints = nrow(changepoints)
  )

  class(result) <- c("dual_stable_periods", "list")

  return(result)
}


#' Find Minimum vh Timestamps Within Dates
#'
#' For each specified date, finds the exact timestamp during the predawn window
#' where sap flow is at its minimum. These serve as changepoints for gradient correction.
#'
#' @param vh_data Data frame with datetime and sap flow data
#' @param dates Vector of dates to process
#' @param vh_col Name of sap flow column
#' @param predawn_window Vector of predawn hours
#'
#' @return Data frame with columns:
#'   \item{date}{Date}
#'   \item{timestamp}{POSIXct timestamp of minimum vh}
#'   \item{vh_value}{Sap flow value at that timestamp}
#'
#' @keywords internal
find_min_vh_timestamps <- function(vh_data, dates, vh_col, predawn_window) {

  # Load required packages
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed")
  }

  # Prepare data
  vh_data$hour <- lubridate::hour(vh_data$datetime)
  vh_data$date <- as.Date(vh_data$datetime)

  # Filter to predawn window
  predawn_hours <- seq(predawn_window[1], predawn_window[2] - 1)
  predawn_data <- vh_data[vh_data$hour %in% predawn_hours, ]

  # For each date, find minimum
  changepoints_list <- lapply(dates, function(d) {
    day_data <- predawn_data[predawn_data$date == d, ]

    if (nrow(day_data) == 0) {
      return(NULL)
    }

    # Find index of minimum vh
    min_idx <- which.min(day_data[[vh_col]])

    data.frame(
      date = d,
      timestamp = day_data$datetime[min_idx],
      vh_value = day_data[[vh_col]][min_idx],
      stringsAsFactors = FALSE
    )
  })

  # Combine and remove NULLs
  changepoints <- do.call(rbind, changepoints_list)

  return(changepoints)
}


#' Apply Gradient Zero Offset Correction
#'
#' Applies a gradient-based zero offset correction by linearly interpolating
#' between changepoints. This creates a "sawtooth" pattern that represents
#' gradual probe spacing changes over time, rather than step-wise corrections.
#'
#' @param vh_data Data frame with datetime and sap flow columns
#' @param changepoints Data frame from \code{\link{find_dual_stable_periods}} with
#'   columns: timestamp, vh_value
#' @param vh_col Name of sap flow column to correct
#' @param new_col_suffix Suffix for corrected column name (default: "_gradient_corrected")
#' @param edge_handling How to handle edges before first/after last changepoint.
#'   Options: "extend" (use nearest), "zero" (no correction). Default: "extend"
#'
#' @return Data frame with additional column containing gradient-corrected values
#'
#' @details
#' **Gradient Correction Method:**
#'
#' Between consecutive changepoints:
#' \enumerate{
#'   \item Offset at changepoint A = vh_value at A
#'   \item Offset at changepoint B = vh_value at B
#'   \item Linear interpolation between A and B
#'   \item Subtract interpolated offset from original vh
#' }
#'
#' **Edge Handling:**
#'
#' \itemize{
#'   \item Before first changepoint: Use first changepoint offset (if "extend")
#'   \item After last changepoint: Use last changepoint offset (if "extend")
#'   \item Alternative ("zero"): No correction at edges
#' }
#'
#' **Physical Interpretation:**
#'
#' The gradient represents:
#' \itemize{
#'   \item Gradual probe movement due to wood expansion/contraction
#'   \item Cumulative wounding effects developing over time
#'   \item Thermal property changes in measurement zone
#' }
#'
#' **Why Gradient vs Step-Wise?**
#'
#' \itemize{
#'   \item Probe spacing changes are gradual, not instantaneous
#'   \item Eliminates artificial "jumps" at segment boundaries
#'   \item More accurate integration for daily/seasonal water use totals
#'   \item Slope between changepoints has physical meaning
#' }
#'
#' @examples
#' \dontrun{
#' # Find dual-stable periods
#' dual_results <- find_dual_stable_periods(
#'   vh_data = vh_results,
#'   weather_data = weather,
#'   vh_col = "Vh_cm_hr"
#' )
#'
#' # Apply gradient correction
#' vh_corrected <- apply_gradient_offset_correction(
#'   vh_data = vh_results,
#'   changepoints = dual_results$changepoints,
#'   vh_col = "Vh_cm_hr",
#'   edge_handling = "extend"
#' )
#'
#' # View corrected data
#' head(vh_corrected[, c("datetime", "Vh_cm_hr", "Vh_cm_hr_gradient_corrected")])
#' }
#'
#' @family dual stability functions
#' @seealso
#'   \code{\link{find_dual_stable_periods}} for identifying changepoints
#' @export
apply_gradient_offset_correction <- function(vh_data,
                                             changepoints,
                                             vh_col,
                                             new_col_suffix = "_gradient_corrected",
                                             edge_handling = "extend") {

  # Input validation
  if (!is.data.frame(vh_data) || !is.data.frame(changepoints)) {
    stop("Both vh_data and changepoints must be data frames")
  }

  required_vh_cols <- c("datetime", vh_col)
  missing <- setdiff(required_vh_cols, names(vh_data))
  if (length(missing) > 0) {
    stop("vh_data missing columns: ", paste(missing, collapse = ", "))
  }

  required_cp_cols <- c("timestamp", "vh_value")
  missing <- setdiff(required_cp_cols, names(changepoints))
  if (length(missing) > 0) {
    stop("changepoints missing columns: ", paste(missing, collapse = ", "))
  }

  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  if (!inherits(changepoints$timestamp, "POSIXct")) {
    changepoints$timestamp <- as.POSIXct(changepoints$timestamp)
  }

  # Sort changepoints by time
  changepoints <- changepoints[order(changepoints$timestamp), ]

  # Create new column name
  new_col <- paste0(vh_col, new_col_suffix)

  # If no changepoints, return original data
  if (nrow(changepoints) == 0) {
    warning("No changepoints provided; returning original data uncorrected")
    vh_data[[new_col]] <- vh_data[[vh_col]]
    return(vh_data)
  }

  # Initialize offset vector
  offset <- rep(NA_real_, nrow(vh_data))

  # Convert to numeric for interpolation
  data_time <- as.numeric(vh_data$datetime)
  cp_time <- as.numeric(changepoints$timestamp)
  cp_offset <- changepoints$vh_value

  # Linear interpolation between changepoints
  offset <- approx(
    x = cp_time,
    y = cp_offset,
    xout = data_time,
    method = "linear",
    rule = ifelse(edge_handling == "extend", 2, 1)
  )$y

  # If edge_handling is "zero", set NAs to 0
  if (edge_handling == "zero") {
    offset[is.na(offset)] <- 0
  }

  # Apply correction
  vh_data[[new_col]] <- vh_data[[vh_col]] - offset

  # Add metadata attributes
  attr(vh_data[[new_col]], "correction_method") <- "gradient_offset"
  attr(vh_data[[new_col]], "n_changepoints") <- nrow(changepoints)
  attr(vh_data[[new_col]], "edge_handling") <- edge_handling

  return(vh_data)
}


# Helper Functions --------------------------------------------------------

#' Filter by Minimum Spacing
#'
#' Ensures minimum spacing between selected dates, keeping those with lowest values.
#'
#' @param dates Vector of candidate dates
#' @param values Values corresponding to dates (lower is better)
#' @param min_spacing_days Minimum days between selected dates
#'
#' @return Integer vector of indices to keep
#' @keywords internal
filter_stable_by_spacing <- function(dates, values, min_spacing_days) {
  if (length(dates) <= 1) {
    return(seq_along(dates))
  }

  selected_indices <- integer(0)
  candidates_remaining <- seq_along(dates)

  while (length(candidates_remaining) > 0) {
    # Select candidate with lowest value
    values_remaining <- values[candidates_remaining]
    best_local_idx <- which.min(values_remaining)[1]
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


#' Create Segments from Dates
#'
#' Creates segment data frame from changepoint dates.
#'
#' @param changepoint_dates Vector of changepoint dates
#' @param date_range Range of all dates (min, max)
#'
#' @return Data frame with segment information
#' @keywords internal
create_segments_from_dates <- function(changepoint_dates, date_range) {
  if (length(changepoint_dates) == 0) {
    return(data.frame(
      segment_id = 1,
      start_date = date_range[1],
      end_date = date_range[2],
      n_days = as.integer(difftime(date_range[2], date_range[1], units = "days")) + 1
    ))
  }

  # Sort changepoints
  changepoint_dates <- sort(changepoint_dates)

  # Create segment boundaries
  n_segments <- length(changepoint_dates) + 1
  segments <- data.frame(
    segment_id = 1:n_segments,
    start_date = c(date_range[1], changepoint_dates),
    end_date = c(changepoint_dates, date_range[2])
  )

  segments$n_days <- as.integer(difftime(segments$end_date, segments$start_date, units = "days")) + 1

  return(segments)
}


#' Create Empty vh Results
#'
#' Helper function to create empty result structure when no dates found.
#'
#' @keywords internal
create_empty_stable_vh_result <- function(vh_threshold, stability_threshold,
                                          predawn_window, min_n_points,
                                          min_segment_days, max_changepoints,
                                          vh_col) {
  result <- list(
    valid_dates = as.Date(character(0)),
    daily_stats = data.frame(
      date = as.Date(character(0)),
      n_points = integer(0),
      mean_predawn_vh = numeric(0),
      sd_predawn_vh = numeric(0),
      min_predawn_vh = numeric(0),
      max_predawn_vh = numeric(0),
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
      vh_threshold = vh_threshold,
      stability_threshold = stability_threshold,
      predawn_window = predawn_window,
      min_n_points = min_n_points,
      min_segment_days = min_segment_days,
      max_changepoints = max_changepoints,
      vh_col = vh_col
    ),
    n_days_analysed = 0,
    n_days_passed_both = 0,
    n_dates_selected = 0
  )

  class(result) <- c("stable_vh_periods", "list")
  return(result)
}


#' Create Empty Dual Results
#'
#' Helper function to create empty dual-stable result structure.
#'
#' @keywords internal
create_empty_dual_stable_result <- function(vpd_results, vh_results,
                                            predawn_window, min_segment_days) {
  result <- list(
    dual_stable_dates = as.Date(character(0)),
    changepoints = data.frame(
      date = as.Date(character(0)),
      timestamp = as.POSIXct(character(0)),
      vh_value = numeric(0)
    ),
    vpd_results = vpd_results,
    vh_results = vh_results,
    daily_stats = data.frame(date = as.Date(character(0))),
    parameters = list(
      predawn_window = predawn_window,
      min_segment_days = min_segment_days
    ),
    n_dates_dual_stable = 0,
    n_changepoints = 0
  )

  class(result) <- c("dual_stable_periods", "list")
  return(result)
}


# Print Methods -----------------------------------------------------------

#' Print Method for Stable Sap Flow Periods
#'
#' @param x A stable_vh_periods object
#' @param ... Additional arguments (not used)
#' @export
print.stable_vh_periods <- function(x, ...) {
  cat("Stable Sap Flow Periods Detection\n")
  cat("==================================\n\n")

  cat("Parameters:\n")
  cat("  Pre-dawn window:", paste(x$parameters$predawn_window, collapse = ", "), "hours\n")
  cat("  vh threshold:", x$parameters$vh_threshold, "cm/hr (mean)\n")
  cat("  Stability threshold:", x$parameters$stability_threshold, "cm/hr (SD)\n")
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
    cat("Selected Dates and Pre-dawn Sap Flow:\n")
    selected_stats <- x$daily_stats[x$daily_stats$passed_both, ]
    selected_stats <- selected_stats[selected_stats$date %in% x$valid_dates, ]

    date_df <- data.frame(
      Date = format(selected_stats$date),
      Mean_vh_cm_hr = sprintf("%.3f", selected_stats$mean_predawn_vh),
      SD_vh_cm_hr = sprintf("%.3f", selected_stats$sd_predawn_vh),
      N_Points = selected_stats$n_points
    )
    print(date_df, row.names = FALSE)
  } else {
    cat("No dates met both magnitude and stability criteria.\n")
    if (x$n_days_analysed > 0) {
      cat("\nDiagnostic Information:\n")
      cat("  Days passed magnitude check:",
          sum(x$daily_stats$passed_magnitude, na.rm = TRUE), "\n")
      cat("  Days passed stability check:",
          sum(x$daily_stats$passed_stability, na.rm = TRUE), "\n")
      cat("  Minimum mean vh:",
          sprintf("%.3f", min(x$daily_stats$mean_predawn_vh, na.rm = TRUE)), "cm/hr\n")
    }
  }

  invisible(x)
}


#' Print Method for Dual Stable Periods
#'
#' @param x A dual_stable_periods object
#' @param ... Additional arguments (not used)
#' @export
print.dual_stable_periods <- function(x, ...) {
  cat("Dual-Criterion Stability Detection (VPD + Sap Flow)\n")
  cat("===================================================\n\n")

  cat("Parameters:\n")
  cat("  Pre-dawn window:", paste(x$parameters$predawn_window, collapse = ", "), "hours\n")
  cat("  VPD threshold:", x$parameters$vpd_threshold, "kPa (mean)\n")
  cat("  VPD stability:", x$parameters$vpd_stability, "kPa (SD)\n")
  cat("  vh threshold:", x$parameters$vh_threshold, "cm/hr (mean)\n")
  cat("  vh stability:", x$parameters$vh_stability, "cm/hr (SD)\n")
  cat("  Min spacing:", x$parameters$min_segment_days, "days\n\n")

  cat("Results:\n")
  cat("  VPD-stable dates:", x$vpd_results$n_dates_selected, "\n")
  cat("  vh-stable dates:", x$vh_results$n_dates_selected, "\n")
  cat("  DUAL-stable dates:", x$n_dates_dual_stable, "\n")
  cat("  Changepoints identified:", x$n_changepoints, "\n\n")

  if (x$n_changepoints > 0) {
    cat("Changepoints (Minimum vh in Dual-Stable Predawn Periods):\n")
    cp_df <- data.frame(
      Date = format(x$changepoints$date),
      Timestamp = format(x$changepoints$timestamp, "%Y-%m-%d %H:%M"),
      vh_cm_hr = sprintf("%.3f", x$changepoints$vh_value)
    )
    print(cp_df, row.names = FALSE)
  } else {
    cat("No dual-stable periods identified.\n")
    cat("Consider relaxing thresholds or check data availability.\n")
  }

  invisible(x)
}
