# R/03a_zero_offset_detection.R
# Zero-Flow Detection for Heat Pulse Velocity Data
# Calculates zero offset during known zero-flow periods

#' Spacing Correction Functions
#'
#' Functions for detecting and correcting probe spacing errors using zero-flow
#' calibration periods. Implements the Burgess et al. (2001) methodology for
#' correcting systematic errors caused by probe misalignment.
#'
#' @name spacing_correction
NULL


#' Calculate Zero Offset During Known Zero-Flow Periods
#'
#' Determines mean Vh when true sap flow = 0, indicating probe misalignment.
#' The zero offset is used to determine spacing correction coefficients.
#'
#' @param vh_data Data frame containing validated/filtered velocity data.
#'   Must have columns: datetime, sensor_position, and a velocity column.
#' @param zero_periods List of zero-flow periods. Each element should be a list
#'   with \code{start} and \code{end} (datetime strings or POSIXct).
#'   Example: \code{list(list(start = "2024-05-01 00:00:00", end = "2024-05-05 23:59:59"))}
#' @param sensor_position Sensor position to analyse ("outer" or "inner")
#' @param method_col Name of method column (default: "method")
#' @param method Value of method to analyse (default: "HRM")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#'
#' @return A list containing:
#'   \item{zero_vh}{Zero offset value rounded to nearest 0.1 cm/hr}
#'   \item{mean_vh_raw}{Raw mean without rounding}
#'   \item{n_observations}{Total number of observations in zero-flow periods}
#'   \item{overall_sd}{Standard deviation across all zero-flow observations}
#'   \item{overall_cv}{Coefficient of variation (CV = SD/mean)}
#'   \item{period_summary}{Data frame summarising each zero-flow period}
#'   \item{sensor}{Sensor position analysed}
#'   \item{method}{Method analysed}
#'
#' @details
#' **Zero-Flow Period Selection:**
#'
#' Zero-flow periods should represent times when sap flow is genuinely zero:
#' \itemize{
#'   \item **Ideal**: Extended periods of wet, cold, overcast weather
#'   \item **Winter**: Leafless deciduous trees in winter
#'   \item **Cut trees**: Freshly cut trees (first few hours)
#'   \item **Night**: Extended night periods (less reliable - low flow ≠ zero flow)
#' }
#'
#' **Quality Indicators:**
#' \itemize{
#'   \item **CV < 0.3**: Excellent stability, high confidence
#'   \item **CV 0.3-0.5**: Acceptable, moderate confidence
#'   \item **CV > 0.5**: High variability, consider longer/different periods
#' }
#'
#' **Recommendations:**
#' \itemize{
#'   \item Use multiple periods (2-5) across different conditions
#'   \item Each period should be at least 6-12 hours
#'   \item Avoid periods immediately after rain (potential bark expansion)
#'   \item Visually inspect Vh time series to confirm stability
#' }
#'
#' @examples
#' \dontrun{
#' # Define zero-flow periods
#' zero_periods <- list(
#'   list(start = "2024-05-01 00:00:00", end = "2024-05-05 23:59:59"),
#'   list(start = "2024-08-15 00:00:00", end = "2024-08-18 23:59:59")
#' )
#'
#' # Calculate zero offset for outer sensor
#' zero_result <- calculate_zero_offset(
#'   vh_data = vh_cleaned,
#'   zero_periods = zero_periods,
#'   sensor_position = "outer",
#'   method = "HRM"
#' )
#'
#' print(zero_result$zero_vh)  # e.g., 0.8 cm/hr
#' print(zero_result$overall_cv)  # e.g., 0.25 (good)
#' print(zero_result$period_summary)  # Check each period
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., Beverly, C.R., Ong, C.K.,
#'   Khan, A.A.H., & Bleby, T.M. (2001). An improved heat pulse method to
#'   measure low and reverse rates of sap flow in woody plants.
#'   *Tree Physiology*, 21(9), 589-598.
#'
#' @family spacing correction functions
#' @export
calculate_zero_offset <- function(vh_data,
                                   zero_periods,
                                   sensor_position,
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

  if (!is.list(zero_periods) || length(zero_periods) == 0) {
    stop("zero_periods must be a non-empty list of period specifications")
  }

  # Filter data for specified sensor and method
  sensor_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method,
  ]

  if (nrow(sensor_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
  }

  # Convert datetime if needed
  if (!inherits(sensor_data$datetime, "POSIXct")) {
    sensor_data$datetime <- as.POSIXct(sensor_data$datetime)
  }

  # Initialise storage
  period_summaries <- list()
  all_zero_values <- c()

  # Process each zero-flow period
  for (i in seq_along(zero_periods)) {
    period <- zero_periods[[i]]

    # Validate period structure
    if (!all(c("start", "end") %in% names(period))) {
      warning("Period ", i, " missing start/end specification - skipping")
      next
    }

    # Convert dates
    start_date <- as.POSIXct(period$start)
    end_date <- as.POSIXct(period$end)

    if (is.na(start_date) || is.na(end_date)) {
      warning("Period ", i, " has invalid start/end dates - skipping")
      next
    }

    if (start_date >= end_date) {
      warning("Period ", i, ": start date must be before end date - skipping")
      next
    }

    # Filter data for this period
    period_data <- sensor_data[
      sensor_data$datetime >= start_date &
      sensor_data$datetime <= end_date,
    ]

    # Extract Vh values
    vh_values <- period_data[[vh_col]]
    vh_values <- vh_values[!is.na(vh_values) & is.finite(vh_values)]

    if (length(vh_values) == 0) {
      warning("No valid data in period ", i, " from ",
              format(start_date), " to ", format(end_date))
      next
    }

    # Calculate statistics
    period_summary <- list(
      period = i,
      start = format(start_date),
      end = format(end_date),
      mean_vh = mean(vh_values),
      median_vh = median(vh_values),
      sd_vh = sd(vh_values),
      min_vh = min(vh_values),
      max_vh = max(vh_values),
      n_obs = length(vh_values),
      stability = if (mean(vh_values) != 0) {
        sd(vh_values) / abs(mean(vh_values))  # CV
      } else {
        NA_real_
      }
    )

    period_summaries[[i]] <- period_summary
    all_zero_values <- c(all_zero_values, vh_values)
  }

  # Calculate overall zero offset
  if (length(all_zero_values) == 0) {
    stop("No valid zero-flow data found in any period")
  }

  mean_vh_raw <- mean(all_zero_values)
  zero_vh_rounded <- round(mean_vh_raw, 1)  # Round to nearest 0.1 cm/hr

  # Quality check
  overall_sd <- sd(all_zero_values)
  overall_cv <- if (mean_vh_raw != 0) {
    overall_sd / abs(mean_vh_raw)
  } else {
    NA_real_
  }

  if (!is.na(overall_cv) && overall_cv > 0.5) {
    warning(
      "High variability in zero-flow period (CV = ", round(overall_cv, 2), ")\n",
      "  Consider using longer or different zero-flow periods"
    )
  }

  # Convert period summaries to data frame
  period_df <- if (length(period_summaries) > 0) {
    do.call(rbind, lapply(period_summaries, function(x) {
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  } else {
    data.frame()
  }

  return(list(
    zero_vh = zero_vh_rounded,
    mean_vh_raw = mean_vh_raw,
    n_observations = length(all_zero_values),
    overall_sd = overall_sd,
    overall_cv = overall_cv,
    period_summary = period_df,
    sensor = sensor_position,
    method = method
  ))
}



#' Calculate Burgess Correction Coefficients Using Original Equations
#'
#' Implements the Burgess et al. (2001) correction methodology to calculate
#' correction coefficients (a, b) for a range of zero offsets.
#'
#' @param zero_vh_range Numeric vector of zero offset values (cm/hr).
#'   Default: seq(-10, 10, by = 0.1)
#' @param k Thermal diffusivity (cm²/s). Default: 0.0025
#' @param x Probe spacing (cm). Default: 0.5
#' @param t Measurement time (sec). Default: 80
#'
#' @return A data frame with columns:
#'   \item{zero_vh}{Zero offset value (cm/hr)}
#'   \item{coef_a}{Slope coefficient}
#'   \item{coef_b}{Intercept coefficient}
#'   \item{range_type}{"modeled" (|zero_vh| ≤ 5) or "extrapolated" (|zero_vh| > 5)}
#'
#' @details
#' **Correction Formula:**
#'
#' \code{Corrected_Vh = a * Vh + b}
#'
#' **Coefficient Calculation:**
#'
#' For |zero_vh| ≤ 5 cm/hr (modeled range):
#' \itemize{
#'   \item Uses Burgess equations to simulate probe misalignment
#'   \item Calculates corrected velocities for range of test values
#'   \item Fits linear model to derive coefficients
#' }
#'
#' For |zero_vh| > 5 cm/hr (extrapolated range):
#' \itemize{
#'   \item Uses simple 1:1 offset correction
#'   \item \code{a = 1, b = -zero_vh}
#'   \item Less reliable - major probe misalignment indicated
#' }
#'
#' **Default Parameters:**
#'
#' Based on ICT SFM1x standard configuration:
#' \itemize{
#'   \item Probe spacing: ±0.5 cm
#'   \item Thermal diffusivity: 0.0025 cm²/s (typical sapwood)
#'   \item Measurement time: 80 seconds (HRM analysis window)
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., Beverly, C.R., Ong, C.K.,
#'   Khan, A.A.H., & Bleby, T.M. (2001). An improved heat pulse method to
#'   measure low and reverse rates of sap flow in woody plants.
#'   *Tree Physiology*, 21(9), 589-598.
#'
#' @family spacing correction functions
