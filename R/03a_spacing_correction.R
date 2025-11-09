# R/03a_spacing_correction.R
# Spacing Correction and Zero-Flow Calibration for Heat Pulse Velocity Data
# Implements Burgess et al. (2001) probe misalignment correction

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
#' @export
calculate_burgess_coefficients <- function(zero_vh_range = seq(-10, 10, by = 0.1),
                                           k = 0.0025,
                                           x = 0.5,
                                           t = 80) {

  # Input validation
  if (!is.numeric(zero_vh_range) || length(zero_vh_range) == 0) {
    stop("zero_vh_range must be a non-empty numeric vector")
  }

  if (k <= 0) stop("k (thermal diffusivity) must be positive")
  if (x <= 0) stop("x (probe spacing) must be positive")
  if (t <= 0) stop("t (measurement time) must be positive")

  n <- length(zero_vh_range)
  coef_a <- numeric(n)
  coef_b <- numeric(n)

  for (i in seq_along(zero_vh_range)) {

    zero_vh <- zero_vh_range[i]

    if (abs(zero_vh) > 5) {
      # Extrapolated range: use 1:1 offset
      coef_a[i] <- 1
      coef_b[i] <- -zero_vh

    } else {
      # Modeled range: use Burgess equations

      # Calculate erroneous v1/v2 ratio at zero flow
      # Note: Using safe exponential to avoid overflow
      exponent <- zero_vh * x / (k * 3600)
      if (abs(exponent) > 100) {
        # Extreme value - use extrapolation instead
        coef_a[i] <- 1
        coef_b[i] <- -zero_vh
        next
      }
      v_ratio <- exp(exponent)

      # Scenario 1: Assume x1 (downstream) is misaligned
      x2 <- -x  # Assume upstream correctly placed
      term <- 4 * k * t * log(v_ratio) + x2^2
      if (term < 0) {
        # Invalid configuration - use extrapolation
        coef_a[i] <- 1
        coef_b[i] <- -zero_vh
        next
      }
      x1_calc <- sqrt(term)

      # Calculate corrected Vh for range of test values
      test_vh <- seq(-5, 30, by = 0.5)
      corrected_vh_s1 <- sapply(test_vh, function(vh) {
        # Safe exponential calculation
        exponent <- vh * x / (k * 3600)
        if (abs(exponent) > 100) return(NA_real_)

        v_ratio_test <- exp(exponent)
        if (!is.finite(v_ratio_test) || v_ratio_test <= 0) return(NA_real_)

        log_ratio <- log(v_ratio_test)
        if (!is.finite(log_ratio)) return(NA_real_)

        numerator <- 4 * k * t * log_ratio - (x1_calc^2 - x2^2)
        denominator <- t * (x1_calc + x2)

        if (abs(denominator) < 1e-10) return(NA_real_)

        vh_corr <- numerator * 3600 / denominator
        if (!is.finite(vh_corr)) return(NA_real_)

        return(vh_corr)
      })

      # Remove NAs for linear fit
      valid_idx <- is.finite(corrected_vh_s1)
      if (sum(valid_idx) < 2) {
        # Not enough valid points - use extrapolation
        coef_a[i] <- 1
        coef_b[i] <- -zero_vh
        next
      }

      # Scenario 2: Assume x2 (upstream) is misaligned
      x1 <- x  # Assume downstream correctly placed
      term <- 4 * k * t * log(v_ratio) + x1^2
      if (term < 0) {
        # Use scenario 1 only
        fit <- lm(corrected_vh_s1[valid_idx] ~ test_vh[valid_idx])
        coef_a[i] <- coef(fit)[2]  # Slope
        coef_b[i] <- coef(fit)[1]  # Intercept
        next
      }
      x2_calc <- -sqrt(term)

      corrected_vh_s2 <- sapply(test_vh, function(vh) {
        # Safe exponential calculation
        exponent <- vh * x / (k * 3600)
        if (abs(exponent) > 100) return(NA_real_)

        v_ratio_test <- exp(exponent)
        if (!is.finite(v_ratio_test) || v_ratio_test <= 0) return(NA_real_)

        log_ratio <- log(v_ratio_test)
        if (!is.finite(log_ratio)) return(NA_real_)

        numerator <- 4 * k * t * log_ratio - (x2_calc^2 - x1^2)
        denominator <- t * (x1 + x2_calc)

        if (abs(denominator) < 1e-10) return(NA_real_)

        vh_corr <- numerator * 3600 / denominator
        if (!is.finite(vh_corr)) return(NA_real_)

        return(vh_corr)
      })

      # Average the two scenarios
      corrected_vh_avg <- (corrected_vh_s1 + corrected_vh_s2) / 2

      # Check for sufficient valid data
      valid_avg_idx <- is.finite(corrected_vh_avg)
      if (sum(valid_avg_idx) < 2) {
        # Not enough valid points - use extrapolation
        coef_a[i] <- 1
        coef_b[i] <- -zero_vh
        next
      }

      # Fit linear model: corrected = a * original + b
      fit <- lm(corrected_vh_avg[valid_avg_idx] ~ test_vh[valid_avg_idx])
      coef_a[i] <- coef(fit)[2]  # Slope
      coef_b[i] <- coef(fit)[1]  # Intercept
    }
  }

  lookup_table <- data.frame(
    zero_vh = zero_vh_range,
    coef_a = coef_a,
    coef_b = coef_b,
    range_type = ifelse(abs(zero_vh_range) > 5, "extrapolated", "modeled"),
    stringsAsFactors = FALSE
  )

  # Add metadata as attributes
  attr(lookup_table, "k_assumed") <- k
  attr(lookup_table, "x_nominal") <- x
  attr(lookup_table, "t_measurement") <- t
  attr(lookup_table, "method") <- "Burgess et al. (2001)"

  class(lookup_table) <- c("burgess_lookup", "data.frame")

  return(lookup_table)
}


#' Get Spacing Correction Coefficients for Zero Offset
#'
#' Looks up Burgess correction coefficients for a given zero offset value.
#' Provides warnings based on severity of probe misalignment.
#'
#' @param zero_vh Zero offset value (cm/hr)
#' @param lookup_table Burgess coefficient lookup table from
#'   \code{\link{calculate_burgess_coefficients}}
#'
#' @return A list containing:
#'   \item{coef_a}{Slope coefficient for correction}
#'   \item{coef_b}{Intercept coefficient for correction}
#'   \item{zero_vh}{Input zero offset}
#'   \item{range_type}{"modeled" or "extrapolated"}
#'   \item{severity}{"none", "minor", "moderate", or "severe"}
#'   \item{warning}{Warning message (if applicable)}
#'   \item{correction_formula}{Formula string for reference}
#'
#' @details
#' **Severity Assessment:**
#'
#' \describe{
#'   \item{|zero_vh| ≤ 1 cm/hr}{**None/Minor** - Typical for field installations}
#'   \item{|zero_vh| 1-3 cm/hr}{**Minor** - Acceptable, correction reliable}
#'   \item{|zero_vh| 3-5 cm/hr}{**Moderate** - Significant misalignment, elevated uncertainty}
#'   \item{|zero_vh| 5-10 cm/hr}{**Severe** - Major misalignment, correction uses extrapolation}
#'   \item{|zero_vh| > 10 cm/hr}{**Critical** - Data should be discarded, reinstall probes}
#' }
#'
#' @examples
#' \dontrun{
#' # Create lookup table
#' lookup <- calculate_burgess_coefficients()
#'
#' # Get coefficients for zero offset = 0.8 cm/hr
#' correction <- get_correction_coefficients(0.8, lookup)
#'
#' print(correction$correction_formula)
#' # "Vh_corrected = 1.0234 * Vh - 0.8192"
#' }
#'
#' @family spacing correction functions
#' @export
get_correction_coefficients <- function(zero_vh, lookup_table) {

  # Input validation
  if (!is.numeric(zero_vh) || length(zero_vh) != 1) {
    stop("zero_vh must be a single numeric value")
  }

  if (!inherits(lookup_table, "burgess_lookup") && !is.data.frame(lookup_table)) {
    stop("lookup_table must be a burgess_lookup object or data frame")
  }

  required_cols <- c("zero_vh", "coef_a", "coef_b", "range_type")
  missing_cols <- setdiff(required_cols, names(lookup_table))
  if (length(missing_cols) > 0) {
    stop("lookup_table missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate zero offset range
  if (abs(zero_vh) > 10) {
    stop(
      "Zero offset ", zero_vh, " cm/hr exceeds maximum correctable range (±10 cm/hr).\n",
      "  Probe misalignment is too severe - data should be discarded.\n",
      "  Recommendation: Reinstall probes and recollect data."
    )
  }

  # Find matching row (allow small tolerance for floating point comparison)
  tolerance <- 0.05
  match_idx <- which(abs(lookup_table$zero_vh - zero_vh) < tolerance)

  if (length(match_idx) == 0) {
    stop("Zero offset ", zero_vh, " not found in lookup table.\n",
         "  Available range: ", min(lookup_table$zero_vh), " to ",
         max(lookup_table$zero_vh), " cm/hr")
  }

  # Use closest match
  match_row <- lookup_table[match_idx[1], ]

  # Extract coefficients
  coef_a <- match_row$coef_a
  coef_b <- match_row$coef_b
  range_type <- match_row$range_type

  # Generate warnings based on severity
  warning_msg <- NULL
  severity <- "none"

  if (abs(zero_vh) > 5) {
    severity <- "severe"
    warning_msg <- paste(
      "Zero offset", zero_vh, "cm/hr exceeds modeled range (±5 cm/hr).",
      "\n  Correction uses extrapolated 1:1 offset.",
      "\n  Treat results with caution - major probe misalignment indicated.",
      "\n  Recommendation: Verify installation and consider reinstalling probes."
    )
    warning(warning_msg, call. = FALSE)

  } else if (abs(zero_vh) > 3) {
    severity <- "moderate"
    warning_msg <- paste(
      "Zero offset", zero_vh, "cm/hr indicates significant probe misalignment.",
      "\n  Correction is within modeled range but uncertainty is elevated.",
      "\n  Recommendation: Verify installation quality and document in metadata."
    )
    warning(warning_msg, call. = FALSE)

  } else if (abs(zero_vh) > 1) {
    severity <- "minor"
    warning_msg <- paste(
      "Zero offset", zero_vh, "cm/hr indicates minor probe misalignment.",
      "\n  Correction is reliable - typical for field installations."
    )
    message(warning_msg)
  }

  return(list(
    coef_a = coef_a,
    coef_b = coef_b,
    zero_vh = zero_vh,
    range_type = range_type,
    severity = severity,
    warning = warning_msg,
    correction_formula = paste0(
      "Vh_corrected = ",
      round(coef_a, 4), " * Vh + ",
      round(coef_b, 4)
    )
  ))
}


#' Apply Spacing Correction to Velocity Data
#'
#' Applies linear correction (Corrected_Vh = a * Vh + b) to velocity data
#' using sensor-specific correction coefficients.
#'
#' @param vh_data Data frame containing velocity data to correct
#' @param correction_params Named list of correction parameters per sensor.
#'   Each element should contain: \code{coef_a}, \code{coef_b}, \code{sensor_position}
#' @param method Method to correct (default: "HRM")
#' @param method_col Name of method column (default: "method")
#' @param vh_col Name of velocity column to correct (default: "Vh_cm_hr")
#' @param create_new_col Logical, whether to create new corrected column
#'   (default: TRUE). If TRUE, creates "Vh_cm_hr_sc" column. If FALSE,
#'   replaces values in vh_col.
#'
#' @return Data frame with corrections applied. If \code{create_new_col = TRUE},
#'   adds columns:
#'   \itemize{
#'     \item \code{Vh_cm_hr_sc}: Spacing-corrected velocity
#'     \item \code{spacing_correction_applied}: Logical flag
#'   }
#'
#' @details
#' The correction formula is:
#'
#' \code{Vh_corrected = a * Vh_original + b}
#'
#' where \code{a} and \code{b} are sensor-specific coefficients determined
#' from zero-flow calibration.
#'
#' @examples
#' \dontrun{
#' # Define correction parameters for each sensor
#' correction_params <- list(
#'   outer = list(
#'     sensor_position = "outer",
#'     coef_a = 1.0234,
#'     coef_b = -0.8192,
#'     zero_vh = 0.8
#'   ),
#'   inner = list(
#'     sensor_position = "inner",
#'     coef_a = 0.9876,
#'     coef_b = -0.5432,
#'     zero_vh = 0.5
#'   )
#' )
#'
#' # Apply corrections
#' vh_corrected <- apply_spacing_correction(
#'   vh_data = vh_cleaned,
#'   correction_params = correction_params,
#'   method = "HRM"
#' )
#' }
#'
#' @family spacing correction functions
#' @export
apply_spacing_correction <- function(vh_data,
                                     correction_params,
                                     method = "HRM",
                                     method_col = "method",
                                     vh_col = "Vh_cm_hr",
                                     create_new_col = TRUE) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.list(correction_params) || length(correction_params) == 0) {
    stop("correction_params must be a non-empty list")
  }

  # Create output dataframe
  corrected_data <- vh_data

  # Create new column or prepare to replace
  if (create_new_col) {
    corrected_col <- paste0(vh_col, "_sc")
    corrected_data[[corrected_col]] <- NA_real_
    corrected_data$spacing_correction_applied <- FALSE
  } else {
    corrected_col <- vh_col
    if (!"spacing_correction_applied" %in% names(corrected_data)) {
      corrected_data$spacing_correction_applied <- FALSE
    }
  }

  # Apply correction for each sensor
  for (sensor_name in names(correction_params)) {

    params <- correction_params[[sensor_name]]

    # Validate params
    required_param_fields <- c("sensor_position", "coef_a", "coef_b")
    missing_fields <- setdiff(required_param_fields, names(params))
    if (length(missing_fields) > 0) {
      warning("Skipping ", sensor_name, ": missing fields ",
              paste(missing_fields, collapse = ", "))
      next
    }

    # Filter data for this sensor and method
    sensor_rows <- corrected_data$sensor_position == params$sensor_position &
                   corrected_data[[method_col]] == method

    if (sum(sensor_rows) == 0) {
      message("No data found for sensor '", params$sensor_position,
              "' and method '", method, "' - skipping")
      next
    }

    # Get original Vh values
    vh_original <- corrected_data[[vh_col]][sensor_rows]

    # Apply correction: Corrected_Vh = a * Vh + b
    vh_corrected <- params$coef_a * vh_original + params$coef_b

    # Store corrected values
    corrected_data[[corrected_col]][sensor_rows] <- vh_corrected
    corrected_data$spacing_correction_applied[sensor_rows] <- TRUE
  }

  return(corrected_data)
}


#' Apply Spacing Correction Workflow
#'
#' Complete workflow for spacing correction using zero-flow calibration.
#' Orchestrates zero offset calculation, coefficient lookup, and correction application.
#'
#' @param vh_data Data frame containing validated/filtered velocity data
#' @param zero_periods List of zero-flow periods (see \code{\link{calculate_zero_offset}})
#' @param sensors Vector of sensor positions to process (default: c("outer", "inner"))
#' @param method Method to correct (default: "HRM")
#' @param method_col Name of method column (default: "method")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#' @param k_assumed Assumed thermal diffusivity (cm²/s) (default: 0.0025)
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param measurement_time Measurement time (sec) (default: 80)
#' @param lookup_table Optional pre-calculated Burgess lookup table. If NULL,
#'   will calculate using \code{\link{calculate_burgess_coefficients}}
#' @param create_new_col Logical, whether to create new corrected column (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{vh_corrected}{Data frame with spacing corrections applied}
#'   \item{zero_offset_results}{List of zero offset results per sensor}
#'   \item{correction_coefficients}{List of correction coefficients per sensor}
#'   \item{metadata}{List containing correction metadata}
#'
#' @details
#' **Workflow Steps:**
#'
#' For each sensor position:
#' \enumerate{
#'   \item Calculate zero offset from zero-flow periods
#'   \item Look up Burgess correction coefficients
#'   \item Apply linear correction to all data
#'   \item Store metadata for reproducibility
#' }
#'
#' **Metadata Stored:**
#' \itemize{
#'   \item Zero-flow period specifications
#'   \item Zero offset values and quality metrics (CV)
#'   \item Correction coefficients (a, b)
#'   \item Thermal diffusivity assumed
#'   \item Date/time of correction
#' }
#'
#' @examples
#' \dontrun{
#' # Complete spacing correction workflow
#' zero_periods <- list(
#'   list(start = "2024-05-01 00:00:00", end = "2024-05-05 23:59:59"),
#'   list(start = "2024-08-15 00:00:00", end = "2024-08-18 23:59:59")
#' )
#'
#' correction_result <- apply_spacing_correction_workflow(
#'   vh_data = vh_cleaned,
#'   zero_periods = zero_periods,
#'   sensors = c("outer", "inner"),
#'   method = "HRM",
#'   k_assumed = 0.0025
#' )
#'
#' # Extract corrected data
#' vh_corrected <- correction_result$vh_corrected
#'
#' # View correction summary
#' print_spacing_correction_summary(correction_result)
#'
#' # Check zero offset quality
#' print(correction_result$zero_offset_results$outer$overall_cv)
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
apply_spacing_correction_workflow <- function(vh_data,
                                               zero_periods,
                                               sensors = c("outer", "inner"),
                                               method = "HRM",
                                               method_col = "method",
                                               vh_col = "Vh_cm_hr",
                                               k_assumed = 0.0025,
                                               probe_spacing = 0.5,
                                               measurement_time = 80,
                                               lookup_table = NULL,
                                               create_new_col = TRUE,
                                               verbose = TRUE) {

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SPACING CORRECTION WORKFLOW (Assumed k =", k_assumed, "cm²/s)\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  # Create or validate lookup table
  if (is.null(lookup_table)) {
    if (verbose) {
      cat("Generating Burgess coefficient lookup table...\n")
    }
    lookup_table <- calculate_burgess_coefficients(
      k = k_assumed,
      x = probe_spacing,
      t = measurement_time
    )
    if (verbose) {
      cat("  ✓ Lookup table generated (", nrow(lookup_table), " coefficient pairs)\n\n")
    }
  }

  # Initialise storage
  correction_params <- list()
  zero_offset_results <- list()

  # Process each sensor
  for (sensor in sensors) {

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing sensor:", toupper(sensor), "\n")
      cat(strrep("-", 72), "\n")
    }

    # Step 1: Calculate zero offset
    tryCatch({
      zero_result <- calculate_zero_offset(
        vh_data = vh_data,
        zero_periods = zero_periods,
        sensor_position = sensor,
        method_col = method_col,
        method = method,
        vh_col = vh_col
      )

      if (verbose) {
        cat("  Zero offset:", zero_result$zero_vh, "cm/hr\n")
        cat("  Based on", zero_result$n_observations, "observations\n")
        cat("  Variability (CV):", round(zero_result$overall_cv, 3), "\n")

        # Quality assessment
        if (!is.na(zero_result$overall_cv)) {
          if (zero_result$overall_cv < 0.3) {
            cat("  Quality: ✓ EXCELLENT (CV < 0.3)\n")
          } else if (zero_result$overall_cv < 0.5) {
            cat("  Quality: ✓ ACCEPTABLE (CV < 0.5)\n")
          } else {
            cat("  Quality: ⚠ HIGH VARIABILITY (CV > 0.5)\n")
          }
        }
      }

      # Step 2: Get correction coefficients
      coef_result <- get_correction_coefficients(
        zero_vh = zero_result$zero_vh,
        lookup_table = lookup_table
      )

      if (verbose) {
        cat("  Correction formula:", coef_result$correction_formula, "\n")
        cat("  Severity:", toupper(coef_result$severity), "\n")
      }

      # Store parameters
      correction_params[[sensor]] <- list(
        sensor_position = sensor,
        zero_vh = zero_result$zero_vh,
        coef_a = coef_result$coef_a,
        coef_b = coef_result$coef_b,
        range_type = coef_result$range_type,
        severity = coef_result$severity,
        warning = coef_result$warning,
        n_observations = zero_result$n_observations,
        cv = zero_result$overall_cv
      )

      zero_offset_results[[sensor]] <- zero_result

    }, error = function(e) {
      if (verbose) {
        cat("  ✗ Error processing sensor", sensor, ":", e$message, "\n")
      }
      warning("Failed to process sensor ", sensor, ": ", e$message, call. = FALSE)
    })

    if (verbose) cat("\n")
  }

  # Check if any sensors were successfully processed
  if (length(correction_params) == 0) {
    stop("No sensors were successfully processed")
  }

  # Step 3: Apply corrections
  if (verbose) {
    cat(strrep("-", 72), "\n")
    cat("Applying corrections to data...\n")
  }

  vh_corrected <- apply_spacing_correction(
    vh_data = vh_data,
    correction_params = correction_params,
    method = method,
    method_col = method_col,
    vh_col = vh_col,
    create_new_col = create_new_col
  )

  if (verbose) {
    n_corrected <- sum(vh_corrected$spacing_correction_applied, na.rm = TRUE)
    cat("  ✓ Corrections applied to", n_corrected, "observations\n")
  }

  # Step 4: Create metadata
  metadata <- list(
    method = method,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    zero_periods = zero_periods,
    date_applied = Sys.time(),
    phase = "Phase 1: Assumed k",
    sapfluxr_version = packageVersion("sapfluxr")
  )

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("✓ Spacing correction workflow complete!\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  result <- list(
    vh_corrected = vh_corrected,
    zero_offset_results = zero_offset_results,
    correction_coefficients = correction_params,
    metadata = metadata
  )

  class(result) <- c("spacing_correction_result", "list")

  return(result)
}


#' Print Spacing Correction Summary
#'
#' Generates a comprehensive summary report of spacing correction results.
#'
#' @param correction_result Output from \code{\link{apply_spacing_correction_workflow}}
#'
#' @details
#' Prints a formatted summary including:
#' \itemize{
#'   \item Correction metadata (method, thermal diffusivity, date)
#'   \item Zero-flow calibration periods
#'   \item Correction coefficients by sensor
#'   \item Quality assessment and warnings
#' }
#'
#' @family spacing correction functions
#' @export
print_spacing_correction_summary <- function(correction_result) {

  if (!inherits(correction_result, "spacing_correction_result")) {
    stop("Input must be a spacing_correction_result object from apply_spacing_correction_workflow()")
  }

  metadata <- correction_result$metadata
  coeffs <- correction_result$correction_coefficients

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("SPACING CORRECTION SUMMARY REPORT\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  cat("Implementation Phase:", metadata$phase, "\n")
  cat("Method Corrected:", metadata$method, "\n")
  cat("Thermal Diffusivity (assumed):", metadata$k_assumed, "cm²/s\n")
  cat("Probe Spacing:", metadata$probe_spacing, "cm\n")
  cat("Measurement Time:", metadata$measurement_time, "sec\n")
  cat("Date Applied:", format(metadata$date_applied, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("\n")

  cat("ZERO-FLOW CALIBRATION PERIODS\n")
  cat(strrep("-", 72), "\n")
  for (i in seq_along(metadata$zero_periods)) {
    period <- metadata$zero_periods[[i]]
    cat("  Period", i, ":", period$start, "to", period$end, "\n")
  }
  cat("\n")

  cat("CORRECTION COEFFICIENTS BY SENSOR\n")
  cat(strrep("-", 72), "\n")

  for (sensor_name in names(coeffs)) {
    coef <- coeffs[[sensor_name]]

    cat("\n")
    cat("╔", strrep("═", 70), "╗\n", sep = "")
    cat("║ SENSOR:", toupper(sensor_name), strrep(" ", 60 - nchar(sensor_name)), "║\n", sep = "")
    cat("╠", strrep("═", 70), "╣\n", sep = "")

    cat("║  Zero Offset:", sprintf("%6.1f cm/hr", coef$zero_vh),
        strrep(" ", 48), "║\n", sep = "")
    cat("║  Observations:", sprintf("%5d", coef$n_observations),
        strrep(" ", 52), "║\n", sep = "")
    cat("║  Variability (CV):", sprintf("%5.3f", coef$cv),
        strrep(" ", 47), "║\n", sep = "")
    cat("║", strrep(" ", 70), "║\n", sep = "")
    cat("║  Correction Formula:", strrep(" ", 49), "║\n", sep = "")
    cat("║    Vh_corrected = ", sprintf("%7.4f × Vh %+7.4f",
                                         coef$coef_a, coef$coef_b),
        strrep(" ", 28), "║\n", sep = "")
    cat("║", strrep(" ", 70), "║\n", sep = "")
    cat("║  Range Type:", coef$range_type,
        strrep(" ", 57 - nchar(coef$range_type)), "║\n", sep = "")
    cat("║  Severity:", coef$severity,
        strrep(" ", 59 - nchar(coef$severity)), "║\n", sep = "")

    if (!is.null(coef$warning) && nchar(coef$warning) > 0) {
      cat("║", strrep(" ", 70), "║\n", sep = "")
      cat("║  ⚠ WARNING:", strrep(" ", 57), "║\n", sep = "")
      # Wrap warning text
      wrapped <- strwrap(coef$warning, width = 64)
      for (line in wrapped) {
        cat("║    ", line, strrep(" ", 66 - nchar(line)), "║\n", sep = "")
      }
    }

    cat("╚", strrep("═", 70), "╝\n", sep = "")
  }

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  invisible(correction_result)
}


#' Print Method for Spacing Correction Results
#'
#' @param x A spacing_correction_result object
#' @param ... Additional arguments (ignored)
#' @export
print.spacing_correction_result <- function(x, ...) {
  print_spacing_correction_summary(x)
}

