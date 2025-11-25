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


#' Apply Spacing Correction Per Segment (Changepoint-Based)
#'
#' Applies spacing correction separately to each segment defined by changepoints,
#' calculating separate zero offsets and Burgess coefficients for each period.
#' This accounts for probe movement or alignment changes over time.
#'
#' @param vh_data Data frame containing velocity data
#' @param changepoints Vector of changepoint dates (Date class) dividing the time series
#' @param sensor_position Sensor position to correct ("outer" or "inner")
#' @param method Method to correct (default: "HRM")
#' @param method_col Name of method column (default: "method")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#' @param k_assumed Assumed thermal diffusivity (cm²/s) (default: 0.0025)
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param measurement_time Measurement time (sec) (default: 80)
#' @param lookup_table Optional pre-calculated Burgess lookup table
#' @param baseline_overrides Optional named list of manual baseline values per segment.
#'   List names should be segment IDs ("1", "2", etc.). If a segment ID is not in
#'   the list, baseline is auto-detected as minimum Vh in that segment.
#'   Example: \code{list("1" = 0.8, "3" = 1.2)} overrides segments 1 and 3,
#'   auto-detects segment 2. Default: NULL (all segments auto-detect).
#' @param create_new_col Logical, whether to create new corrected column (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{vh_corrected}{Data frame with spacing corrections applied}
#'   \item{segment_results}{List of correction results per segment}
#'   \item{metadata}{List containing correction metadata}
#'
#' @details
#' **Changepoint-Based Correction:**
#'
#' This function divides the time series into segments at changepoints and applies
#' independent spacing corrections to each segment. This is necessary when:
#' \itemize{
#'   \item Tree swelling after rain causes probe misalignment
#'   \item Tree shrinkage during drought moves probes
#'   \item Physical disturbance shifts probe positions
#'   \item Seasonal changes affect probe contact
#' }
#'
#' **Workflow Per Segment:**
#' \enumerate{
#'   \item Identify data within segment boundaries
#'   \item Calculate baseline (minimum) velocity for that segment
#'   \item Look up Burgess correction coefficients
#'   \item Apply segment-specific correction
#' }
#'
#' @examples
#' \dontrun{
#' # Detect changepoints
#' daily_min <- calculate_daily_minima(vh_results)
#' cpt_result <- detect_changepoints(daily_min)
#'
#' # Apply segment-based correction
#' correction_result <- apply_spacing_correction_per_segment(
#'   vh_data = vh_results,
#'   changepoints = cpt_result$changepoints,
#'   sensor_position = "outer",
#'   method = "HRM"
#' )
#' }
#'
#' @family spacing correction functions
#' @export
apply_spacing_correction_per_segment <- function(vh_data,
                                                  changepoints,
                                                  sensor_position,
                                                  method = "HRM",
                                                  method_col = "method",
                                                  vh_col = "Vh_cm_hr",
                                                  k_assumed = 0.0025,
                                                  probe_spacing = 0.5,
                                                  measurement_time = 80,
                                                  lookup_table = NULL,
                                                  baseline_overrides = NULL,
                                                  create_new_col = TRUE,
                                                  verbose = TRUE) {

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

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Filter data for specified sensor and method
  sensor_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method,
  ]

  if (nrow(sensor_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
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
  }

  # Define segment boundaries from changepoints
  # Convert changepoints to POSIXct for comparison with datetime
  if (!is.null(changepoints) && length(changepoints) > 0) {
    cpt_datetimes <- as.POSIXct(as.character(changepoints))
    segment_starts <- c(min(sensor_data$datetime), cpt_datetimes)
    segment_ends <- c(cpt_datetimes, max(sensor_data$datetime))
  } else {
    # No changepoints - single segment
    segment_starts <- min(sensor_data$datetime)
    segment_ends <- max(sensor_data$datetime)
  }

  n_segments <- length(segment_starts)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SEGMENT-BASED SPACING CORRECTION (k =", k_assumed, "cm²/s)\n")
    cat(strrep("=", 72), "\n")
    cat("Sensor:", toupper(sensor_position), "| Method:", method, "\n")
    cat("Number of segments:", n_segments, "\n")
    cat("\n")
  }

  # Initialise storage
  segment_results <- list()
  corrected_vh <- numeric(nrow(sensor_data))

  # Process each segment
  for (seg_id in seq_len(n_segments)) {

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing Segment", seg_id, "of", n_segments, "\n")
      cat(strrep("-", 72), "\n")
      cat("Period:", format(segment_starts[seg_id], "%Y-%m-%d %H:%M"), "to",
          format(segment_ends[seg_id], "%Y-%m-%d %H:%M"), "\n")
    }

    # Extract segment data
    seg_mask <- sensor_data$datetime >= segment_starts[seg_id] &
                sensor_data$datetime <= segment_ends[seg_id]
    seg_data <- sensor_data[seg_mask, ]

    if (nrow(seg_data) == 0) {
      if (verbose) {
        cat("  ✗ No data in this segment - skipping\n\n")
      }
      next
    }

    if (verbose) {
      cat("  Observations:", nrow(seg_data), "\n")
    }

    # Calculate zero offset as minimum Vh in this segment (baseline)
    vh_values <- seg_data[[vh_col]]
    vh_values_clean <- vh_values[!is.na(vh_values) & is.finite(vh_values)]

    if (length(vh_values_clean) == 0) {
      if (verbose) {
        cat("  ✗ No valid Vh values in segment - skipping\n\n")
      }
      next
    }

    # Check for manual baseline override
    if (!is.null(baseline_overrides) && seg_id %in% names(baseline_overrides)) {
      zero_vh <- baseline_overrides[[seg_id]]
      if (verbose) {
        cat("  Using MANUAL baseline override\n")
      }
    } else {
      zero_vh <- round(min(vh_values_clean, na.rm = TRUE), 1)
      if (verbose) {
        cat("  Using AUTO-DETECTED baseline (minimum)\n")
      }
    }
    mean_vh <- mean(vh_values_clean, na.rm = TRUE)
    sd_vh <- sd(vh_values_clean, na.rm = TRUE)
    cv <- sd_vh / abs(mean_vh)

    if (verbose) {
      cat("  Baseline (min Vh):", zero_vh, "cm/hr\n")
      cat("  Mean Vh:", round(mean_vh, 2), "cm/hr\n")
      cat("  SD:", round(sd_vh, 2), "| CV:", round(cv, 3), "\n")
    }

    # Get correction coefficients
    tryCatch({
      coef_result <- get_correction_coefficients(
        zero_vh = zero_vh,
        lookup_table = lookup_table
      )

      if (verbose) {
        cat("  Correction formula:", coef_result$correction_formula, "\n")
        cat("  Severity:", toupper(coef_result$severity), "\n")
      }

      # Apply correction to this segment
      corrected_vh[seg_mask] <- coef_result$coef_a * vh_values + coef_result$coef_b

      # Store segment results
      segment_results[[seg_id]] <- list(
        segment_id = seg_id,
        start_datetime = segment_starts[seg_id],
        end_datetime = segment_ends[seg_id],
        n_observations = nrow(seg_data),
        zero_vh = zero_vh,
        mean_vh = mean_vh,
        sd_vh = sd_vh,
        cv = cv,
        coef_a = coef_result$coef_a,
        coef_b = coef_result$coef_b,
        range_type = coef_result$range_type,
        severity = coef_result$severity,
        correction_formula = coef_result$correction_formula
      )

    }, error = function(e) {
      if (verbose) {
        cat("  ✗ Error getting correction coefficients:", e$message, "\n")
      }
      warning("Segment ", seg_id, ": ", e$message, call. = FALSE)
      corrected_vh[seg_mask] <- vh_values  # Use uncorrected
    })

    if (verbose) cat("\n")
  }

  # Apply corrections to full dataset
  corrected_data <- vh_data

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

  # Insert corrected values for this sensor/method
  sensor_method_mask <- corrected_data$sensor_position == sensor_position &
                        corrected_data[[method_col]] == method

  corrected_data[[corrected_col]][sensor_method_mask] <- corrected_vh
  corrected_data$spacing_correction_applied[sensor_method_mask] <- TRUE

  # Create metadata
  metadata <- list(
    method = method,
    sensor_position = sensor_position,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_segments = length(segment_results),
    changepoints = if (!is.null(changepoints)) as.character(changepoints) else character(0),
    date_applied = Sys.time(),
    approach = "Changepoint-based segmentation",
    sapfluxr_version = packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("✓ Segment-based spacing correction complete!\n")
    cat("  ", length(segment_results), "segments processed\n")
    n_corrected <- sum(corrected_data$spacing_correction_applied, na.rm = TRUE)
    cat("  ", n_corrected, "observations corrected\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    segment_results = segment_results,
    metadata = metadata
  )

  class(result) <- c("segment_spacing_correction_result", "list")

  return(result)
}

#' Apply Manual Spacing Correction with User-Specified Changepoints
#'
#' Applies spacing correction using manually specified changepoints and optional
#' baseline overrides. This gives users full control over segment boundaries and
#' zero offsets when they have domain knowledge about probe conditions.
#'
#' @param vh_data Data frame containing velocity data
#' @param manual_changepoints Vector of changepoint dates defining segment boundaries.
#'   Can be character strings ("2024-03-15") or Date objects. These divide the
#'   time series into n+1 segments.
#' @param baseline_overrides Optional named list specifying zero offset values for
#'   specific date ranges. Names must be in format "start_date/end_date" and values
#'   are the baseline Vh (cm/hr) to use for that segment.
#'   Example: \code{list("2024-01-01/2024-03-15" = 0.8, "2024-03-16/2024-06-10" = 1.2)}
#'   If a segment is not specified, its baseline will be auto-detected as the minimum.
#' @param sensor_position Sensor position to correct ("outer" or "inner")
#' @param method Method to correct (default: "HRM")
#' @param method_col Name of method column (default: "method")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#' @param k_assumed Assumed thermal diffusivity (cm²/s) (default: 0.0025)
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param measurement_time Measurement time (sec) (default: 80)
#' @param lookup_table Optional pre-calculated Burgess lookup table
#' @param create_new_col Logical, whether to create new corrected column (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{vh_corrected}{Data frame with spacing corrections applied}
#'   \item{segment_results}{List of correction results per segment}
#'   \item{metadata}{List containing correction metadata}
#'
#' @details
#' **Manual Changepoint Specification:**
#'
#' This function allows complete user control over:
#' \itemize{
#'   \item Segment boundaries (via \code{manual_changepoints})
#'   \item Zero offset values (via \code{baseline_overrides})
#' }
#'
#' **Workflow:**
#' \enumerate{
#'   \item Parse manual changepoints to create segments
#'   \item For each segment:
#'     \itemize{
#'       \item Check if baseline_overrides specifies a value for this date range
#'       \item If yes, use the specified baseline
#'       \item If no, auto-detect minimum Vh as baseline
#'     }
#'   \item Look up Burgess coefficients for each baseline
#'   \item Apply segment-specific corrections
#' }
#'
#' **Date Range Format:**
#'
#' The \code{baseline_overrides} parameter uses "start_date/end_date" format:
#' \itemize{
#'   \item Dates should match the segment boundaries from changepoints
#'   \item Format: "YYYY-MM-DD/YYYY-MM-DD"
#'   \item Example: "2024-01-01/2024-03-15" = 0.8
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Manual changepoints, auto-detect all baselines
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15", "2024-06-10"),
#'   sensor_position = "outer",
#'   method = "HRM"
#' )
#'
#' # Example 2: Manual changepoints with some baseline overrides
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15", "2024-06-10"),
#'   baseline_overrides = list(
#'     "2024-01-01/2024-03-15" = 0.8,  # Segment 1: user-specified
#'     # Segment 2 will auto-detect (not specified)
#'     "2024-06-11/2024-12-31" = 1.5   # Segment 3: user-specified
#'   ),
#'   sensor_position = "outer"
#' )
#'
#' # Example 3: Specify all baselines manually
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15"),
#'   baseline_overrides = list(
#'     "2024-01-01/2024-03-15" = 0.8,
#'     "2024-03-16/2024-12-31" = 1.2
#'   ),
#'   sensor_position = "outer"
#' )
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
apply_manual_spacing_correction <- function(vh_data,
                                             manual_changepoints,
                                             baseline_overrides = NULL,
                                             sensor_position,
                                             method = "HRM",
                                             method_col = "method",
                                             vh_col = "Vh_cm_hr",
                                             k_assumed = 0.0025,
                                             probe_spacing = 0.5,
                                             measurement_time = 80,
                                             lookup_table = NULL,
                                             create_new_col = TRUE,
                                             verbose = TRUE) {

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

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Filter data for specified sensor and method
  sensor_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method,
  ]

  if (nrow(sensor_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
  }

  # Convert manual changepoints to Date
  if (is.character(manual_changepoints)) {
    manual_changepoints <- as.Date(manual_changepoints)
  } else if (!inherits(manual_changepoints, "Date")) {
    stop("manual_changepoints must be character vector or Date objects")
  }

  # Parse baseline overrides if provided
  segment_baselines <- list()
  if (!is.null(baseline_overrides)) {
    if (!is.list(baseline_overrides) || is.null(names(baseline_overrides))) {
      stop("baseline_overrides must be a named list with date range names like '2024-01-01/2024-03-15'")
    }

    for (range_name in names(baseline_overrides)) {
      # Parse "start_date/end_date" format
      date_parts <- strsplit(range_name, "/")[[1]]
      if (length(date_parts) != 2) {
        warning("Skipping invalid date range format: '", range_name, "' (expected 'start/end')")
        next
      }

      start_date <- as.Date(date_parts[1])
      end_date <- as.Date(date_parts[2])

      if (is.na(start_date) || is.na(end_date)) {
        warning("Skipping invalid dates in range: '", range_name, "'")
        next
      }

      if (start_date >= end_date) {
        warning("Skipping invalid date range (start >= end): '", range_name, "'")
        next
      }

      # Store baseline for this range
      segment_baselines[[range_name]] <- list(
        start = start_date,
        end = end_date,
        baseline = baseline_overrides[[range_name]]
      )
    }
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
  }

  # Define segment boundaries from manual changepoints
  cpt_datetimes <- as.POSIXct(as.character(manual_changepoints))
  segment_starts <- c(min(sensor_data$datetime), cpt_datetimes)
  segment_ends <- c(cpt_datetimes, max(sensor_data$datetime))
  n_segments <- length(segment_starts)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("MANUAL SPACING CORRECTION (k =", k_assumed, "cm²/s)\n")
    cat(strrep("=", 72), "\n")
    cat("Sensor:", toupper(sensor_position), "| Method:", method, "\n")
    cat("Manual changepoints:", length(manual_changepoints), "\n")
    cat("Number of segments:", n_segments, "\n")
    if (length(segment_baselines) > 0) {
      cat("Baseline overrides:", length(segment_baselines), "segment(s)\n")
    }
    cat("\n")
  }

  # Initialise storage
  segment_results <- list()
  corrected_vh <- numeric(nrow(sensor_data))

  # Process each segment
  for (seg_id in seq_len(n_segments)) {

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing Segment", seg_id, "of", n_segments, "\n")
      cat(strrep("-", 72), "\n")
      cat("Period:", format(segment_starts[seg_id], "%Y-%m-%d %H:%M"), "to",
          format(segment_ends[seg_id], "%Y-%m-%d %H:%M"), "\n")
    }

    # Extract segment data
    seg_mask <- sensor_data$datetime >= segment_starts[seg_id] &
                sensor_data$datetime <= segment_ends[seg_id]
    seg_data <- sensor_data[seg_mask, ]

    if (nrow(seg_data) == 0) {
      if (verbose) {
        cat("  ✗ No data in this segment - skipping\n\n")
      }
      next
    }

    if (verbose) {
      cat("  Observations:", nrow(seg_data), "\n")
    }

    # Get Vh values for this segment
    vh_values <- seg_data[[vh_col]]
    vh_values_clean <- vh_values[!is.na(vh_values) & is.finite(vh_values)]

    if (length(vh_values_clean) == 0) {
      if (verbose) {
        cat("  ✗ No valid Vh values in segment - skipping\n\n")
      }
      next
    }

    # Determine zero offset for this segment
    # Check if user specified a baseline for this segment
    seg_start_date <- as.Date(segment_starts[seg_id])
    seg_end_date <- as.Date(segment_ends[seg_id])

    user_baseline <- NULL
    baseline_source <- "auto-detected"

    # Search for matching baseline override
    for (range_name in names(segment_baselines)) {
      override <- segment_baselines[[range_name]]
      # Check if this segment falls within the specified range
      if (seg_start_date >= override$start && seg_end_date <= override$end) {
        user_baseline <- override$baseline
        baseline_source <- "user-specified"
        break
      }
    }

    # Use user baseline if specified, otherwise auto-detect
    if (!is.null(user_baseline)) {
      zero_vh <- round(user_baseline, 1)
      if (verbose) {
        cat("  Baseline (user-specified):", zero_vh, "cm/hr\n")
      }
    } else {
      zero_vh <- round(min(vh_values_clean, na.rm = TRUE), 1)
      if (verbose) {
        cat("  Baseline (auto-detected min):", zero_vh, "cm/hr\n")
      }
    }

    # Calculate segment statistics
    mean_vh <- mean(vh_values_clean, na.rm = TRUE)
    sd_vh <- sd(vh_values_clean, na.rm = TRUE)
    cv <- sd_vh / abs(mean_vh)

    if (verbose) {
      cat("  Mean Vh:", round(mean_vh, 2), "cm/hr\n")
      cat("  SD:", round(sd_vh, 2), "| CV:", round(cv, 3), "\n")
    }

    # Get correction coefficients
    tryCatch({
      coef_result <- get_correction_coefficients(
        zero_vh = zero_vh,
        lookup_table = lookup_table
      )

      if (verbose) {
        cat("  Correction formula:", coef_result$correction_formula, "\n")
        cat("  Severity:", toupper(coef_result$severity), "\n")
      }

      # Apply correction to this segment
      corrected_vh[seg_mask] <- coef_result$coef_a * vh_values + coef_result$coef_b

      # Store segment results
      segment_results[[seg_id]] <- list(
        segment_id = seg_id,
        start_datetime = segment_starts[seg_id],
        end_datetime = segment_ends[seg_id],
        n_observations = nrow(seg_data),
        zero_vh = zero_vh,
        baseline_source = baseline_source,
        mean_vh = mean_vh,
        sd_vh = sd_vh,
        cv = cv,
        coef_a = coef_result$coef_a,
        coef_b = coef_result$coef_b,
        range_type = coef_result$range_type,
        severity = coef_result$severity,
        correction_formula = coef_result$correction_formula
      )

    }, error = function(e) {
      if (verbose) {
        cat("  ✗ Error getting correction coefficients:", e$message, "\n")
      }
      warning("Segment ", seg_id, ": ", e$message, call. = FALSE)
      corrected_vh[seg_mask] <- vh_values  # Use uncorrected
    })

    if (verbose) cat("\n")
  }

  # Apply corrections to full dataset
  corrected_data <- vh_data

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

  # Insert corrected values for this sensor/method
  sensor_method_mask <- corrected_data$sensor_position == sensor_position &
                        corrected_data[[method_col]] == method

  corrected_data[[corrected_col]][sensor_method_mask] <- corrected_vh
  corrected_data$spacing_correction_applied[sensor_method_mask] <- TRUE

  # Create metadata
  metadata <- list(
    method = method,
    sensor_position = sensor_position,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_segments = length(segment_results),
    manual_changepoints = as.character(manual_changepoints),
    n_baseline_overrides = length(segment_baselines),
    date_applied = Sys.time(),
    approach = "Manual changepoint specification",
    sapfluxr_version = packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("✓ Manual spacing correction complete!\n")
    cat("  ", length(segment_results), "segments processed\n")
    n_user_specified <- sum(sapply(segment_results, function(x) x$baseline_source == "user-specified"))
    n_auto_detected <- sum(sapply(segment_results, function(x) x$baseline_source == "auto-detected"))
    cat("  ", n_user_specified, "user-specified baseline(s)\n")
    cat("  ", n_auto_detected, "auto-detected baseline(s)\n")
    n_corrected <- sum(corrected_data$spacing_correction_applied, na.rm = TRUE)
    cat("  ", n_corrected, "observations corrected\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    segment_results = segment_results,
    metadata = metadata
  )

  class(result) <- c("manual_spacing_correction_result", "list")

  return(result)
}


# =============================================================================
# HEARTWOOD REFERENCE CORRECTION
# =============================================================================
# Alternative zero-flow correction method using the inner sensor as a
# continuous reference when it is positioned in the heartwood (no sap flow).
# =============================================================================

#' Check if Heartwood Reference Correction is Available
#'
#' Determines whether the inner temperature sensor is positioned deep enough
#' in the heartwood to serve as a continuous zero-flow reference. This method
#' relies on the principle that heartwood conducts no sap, so any heat pulse
#' velocity measured at the inner sensor should theoretically be zero.
#'
#' @param probe_config Probe configuration object (ProbeConfig) or named list
#'   containing probe geometry. Must include:
#'   \itemize{
#'     \item \code{length} or \code{probe_length}: Total probe length (mm)
#'     \item \code{inner_sensor}: Distance from probe tip to inner sensor (mm)
#'   }
#' @param sapwood_depth Sapwood depth from cambium to heartwood boundary (cm).
#'   This is the depth of conducting sapwood.
#' @param bark_thickness Bark thickness (cm). Default is 0 (probe handle at
#'   cambium surface).
#' @param field_of_influence Radial extent of heat pulse influence (cm).
#'   Default is 1.0 cm (10mm). The inner sensor must be at least half this
#'   distance beyond the sapwood/heartwood boundary to avoid convective
#'   influence from sap flow.
#'
#' @return A list with class \code{"heartwood_reference_check"} containing:
#'   \item{available}{Logical: TRUE if inner sensor is in heartwood}
#'   \item{inner_depth_cm}{Depth of inner sensor from cambium (cm)}
#'   \item{sapwood_depth_cm}{Provided sapwood depth (cm)}
#'   \item{margin_cm}{Safety margin (how far past sapwood boundary, cm)}
#'   \item{required_margin_cm}{Minimum required margin (field_of_influence/2)}
#'   \item{probe_config_used}{Summary of probe configuration}
#'   \item{recommendation}{Text recommendation for user}
#'
#' @details
#' **Scientific Basis:**
#'
#' Heat pulse velocity methods measure sap flow by tracking heat movement
#' through the sapwood. In heartwood, there is no water transport, so heat
#' transfer is purely conductive. If the inner sensor is positioned entirely
#' in heartwood (beyond the influence of sapwood convection), it provides a
#' continuous "true zero" reference.
#'
#' **Geometry Calculation:**
#'
#' The inner sensor depth from cambium is calculated as:
#' \deqn{d_{inner} = (L_{probe} - d_{tip}) / 10 - t_{bark}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{L_{probe}} = probe length (mm)
#'   \item \eqn{d_{tip}} = inner sensor distance from probe tip (mm)
#'   \item \eqn{t_{bark}} = bark thickness (cm)
#' }
#'
#' **Field of Influence:**
#'
#' The heat pulse field of influence is typically ~10mm radially. To ensure
#' the inner sensor is not affected by sapwood convection, it should be at
#' least 5mm (half the field) beyond the sapwood/heartwood boundary.
#'
#' @examples
#' \dontrun{
#' # Check with explicit values
#' check <- check_heartwood_reference_available(
#'   probe_config = list(length = 35, inner_sensor = 7.5),
#'   sapwood_depth = 2.0,  # 2 cm sapwood
#'   bark_thickness = 0.3   # 3 mm bark
#' )
#'
#' if (check$available) {
#'   cat("Heartwood reference available!\n")
#'   cat("Inner sensor is", check$margin_cm, "cm into heartwood\n")
#' }
#'
#' # Using probe config object
#' probe_config <- load_probe_config("symmetrical")
#' check <- check_heartwood_reference_available(
#'   probe_config = probe_config,
#'   sapwood_depth = 1.5
#' )
#' }
#'
#' @family spacing correction functions
#' @export
check_heartwood_reference_available <- function(probe_config,
                                                 sapwood_depth,
                                                 bark_thickness = 0,
                                                 field_of_influence = 1.0) {

  # Helper for null-coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Extract probe parameters
  if (inherits(probe_config, "ProbeConfig")) {
    # R6 ProbeConfig object
    probe_length_mm <- probe_config$length %||% probe_config$probe_length %||% 35
    inner_sensor_mm <- probe_config$inner_sensor %||% 7.5
  } else if (is.list(probe_config)) {
    # Named list
    probe_length_mm <- probe_config$length %||% probe_config$probe_length %||% 35
    inner_sensor_mm <- probe_config$inner_sensor %||% 7.5
  } else {
    stop("probe_config must be a ProbeConfig object or a named list")
  }

  # Validate inputs
  if (!is.numeric(sapwood_depth) || sapwood_depth <= 0) {
    stop("sapwood_depth must be a positive number (in cm)")
  }
  if (!is.numeric(bark_thickness) || bark_thickness < 0) {
    stop("bark_thickness must be a non-negative number (in cm)")
  }
  if (!is.numeric(field_of_influence) || field_of_influence <= 0) {
    stop("field_of_influence must be a positive number (in cm)")
  }

  # Calculate inner sensor depth from cambium (cm)
  # Inner sensor position from probe handle = probe_length - inner_sensor_from_tip
  # Depth from cambium = that value (in mm) / 10 - bark_thickness
  inner_position_from_handle_mm <- probe_length_mm - inner_sensor_mm
  inner_depth_cm <- (inner_position_from_handle_mm / 10) - bark_thickness

  # Required margin: half the field of influence
  required_margin_cm <- field_of_influence / 2

  # Calculate margin (how far past sapwood boundary)
  margin_cm <- inner_depth_cm - sapwood_depth

  # Determine availability
  available <- margin_cm >= required_margin_cm

  # Build recommendation
  if (available) {
    recommendation <- sprintf(
      paste0("Heartwood reference correction IS available. ",
             "Inner sensor is %.2f cm past the sapwood boundary ",
             "(required: >= %.2f cm). The inner sensor can be used as a ",
             "continuous zero-flow reference."),
      margin_cm, required_margin_cm
    )
  } else if (margin_cm > 0) {
    recommendation <- sprintf(
      paste0("Heartwood reference correction NOT recommended. ",
             "Inner sensor is only %.2f cm past sapwood boundary ",
             "(required: >= %.2f cm for reliable reference). ",
             "Consider using changepoint-based correction instead, or ",
             "reduce field_of_influence if you are confident in sensor placement."),
      margin_cm, required_margin_cm
    )
  } else if (margin_cm == 0) {
    recommendation <- paste0(
      "Inner sensor is exactly at the sapwood/heartwood boundary. ",
      "Heartwood reference correction is NOT available. ",
      "Use changepoint-based correction methods instead."
    )
  } else {
    recommendation <- sprintf(
      paste0("Inner sensor is %.2f cm WITHIN sapwood (not in heartwood). ",
             "Heartwood reference correction is NOT available. ",
             "Use changepoint-based correction methods instead."),
      abs(margin_cm)
    )
  }

  result <- list(
    available = available,
    inner_depth_cm = inner_depth_cm,
    sapwood_depth_cm = sapwood_depth,
    margin_cm = margin_cm,
    required_margin_cm = required_margin_cm,
    probe_config_used = list(
      probe_length_mm = probe_length_mm,
      inner_sensor_from_tip_mm = inner_sensor_mm,
      bark_thickness_cm = bark_thickness
    ),
    field_of_influence_cm = field_of_influence,
    recommendation = recommendation
  )

  class(result) <- c("heartwood_reference_check", "list")

  return(result)
}

#' Print Method for Heartwood Reference Check
#'
#' @param x A heartwood_reference_check object
#' @param ... Additional arguments (ignored)
#' @export
print.heartwood_reference_check <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("HEARTWOOD REFERENCE AVAILABILITY CHECK\n")
  cat(strrep("=", 60), "\n\n")

  cat("Probe Configuration:\n")
  cat("  Probe length:        ", x$probe_config_used$probe_length_mm, " mm\n", sep = "")
  cat("  Inner sensor (from tip): ", x$probe_config_used$inner_sensor_from_tip_mm, " mm\n", sep = "")
  cat("  Bark thickness:      ", x$probe_config_used$bark_thickness_cm, " cm\n", sep = "")
  cat("\n")

  cat("Depth Analysis:\n")
  cat("  Inner sensor depth:  ", sprintf("%.2f", x$inner_depth_cm), " cm (from cambium)\n", sep = "")
  cat("  Sapwood depth:       ", sprintf("%.2f", x$sapwood_depth_cm), " cm\n", sep = "")
  cat("  Margin into heartwood: ", sprintf("%.2f", x$margin_cm), " cm\n", sep = "")
  cat("  Required margin:     ", sprintf("%.2f", x$required_margin_cm), " cm\n", sep = "")
  cat("\n")

  if (x$available) {
    cat("Status: AVAILABLE\n")
  } else {
    cat("Status: NOT AVAILABLE\n")
  }
  cat("\n")
  cat("Recommendation:\n")
  cat(strwrap(x$recommendation, width = 58, indent = 2, exdent = 2), sep = "\n")
  cat("\n")
  cat(strrep("=", 60), "\n")

  invisible(x)
}


#' Apply Heartwood Reference Correction
#'
#' Applies spacing correction using the inner sensor as a continuous zero-flow
#' reference. This method is applicable when the inner sensor is positioned
#' in the heartwood (verified by \code{check_heartwood_reference_available()}).
#'
#' For each measurement, the inner sensor Vh is used as the instantaneous
#' zero offset, and Burgess correction coefficients are applied to correct
#' the outer sensor reading.
#'
#' @param vh_data Data frame containing heat pulse velocity data with both
#'   inner and outer sensor readings. Must have columns: datetime, pulse_id,
#'   sensor_position, and a velocity column.
#' @param method HPV method to correct (default: "HRM").
#' @param method_col Name of method column (default: "method").
#' @param vh_col Name of velocity column (default: "Vh_cm_hr").
#' @param k_assumed Assumed thermal diffusivity (cm2/s, default: 0.0025).
#' @param probe_spacing Probe spacing x (cm, default: 0.5).
#' @param measurement_time Time after heat pulse for HRM measurement (seconds,
#'   default: 80).
#' @param lookup_table Pre-computed Burgess lookup table. If NULL, will be
#'   generated using \code{calculate_burgess_coefficients()}.
#' @param create_new_col If TRUE (default), creates new column with "_hrc"
#'   suffix. If FALSE, overwrites vh_col.
#' @param verbose Print progress messages (default: TRUE).
#'
#' @return A list with class \code{"heartwood_reference_correction_result"}:
#'   \item{vh_corrected}{Data frame with corrected velocities}
#'   \item{offset_summary}{Summary statistics of observed offsets}
#'   \item{metadata}{Correction metadata and parameters}
#'
#' @details
#' **Method Overview:**
#'
#' When the inner sensor is in heartwood, it experiences no convective heat
#' transfer from sap flow. Any Vh measured at the inner sensor represents
#' the instantaneous probe misalignment (zero offset). This offset can be
#' used to correct the outer sensor reading continuously.
#'
#' **Correction Process:**
#'
#' For each measurement:
#' \enumerate{
#'   \item Extract inner sensor Vh = zero_offset
#'   \item Look up Burgess correction coefficients (a, b) for that offset
#'   \item Apply to outer sensor: Vh_corrected = a * Vh_outer + b
#' }
#'
#' **Comparison with Changepoint Method:**
#'
#' The heartwood reference method provides continuous (per-measurement)
#' correction, whereas the changepoint method applies segment-based
#' correction. Use heartwood reference when:
#' \itemize{
#'   \item Inner sensor is confirmed to be in heartwood
#'   \item You want real-time offset tracking
#'   \item Probe alignment may drift within segments
#' }
#'
#' **Diagnostic Output:**
#'
#' The \code{offset_summary} in the result provides statistics on the
#' observed offsets:
#' \itemize{
#'   \item \code{mean_offset}: Average inner sensor Vh
#'   \item \code{sd_offset}: Standard deviation of offsets
#'   \item \code{range_offset}: Min/max observed offsets
#'   \item \code{quality}: Assessment of offset stability
#' }
#'
#' @examples
#' \dontrun{
#' # First check if heartwood reference is available
#' check <- check_heartwood_reference_available(
#'   probe_config = list(length = 35, inner_sensor = 7.5),
#'   sapwood_depth = 2.0
#' )
#'
#' if (check$available) {
#'   # Apply heartwood reference correction
#'   result <- apply_heartwood_reference_correction(
#'     vh_data = my_vh_data,
#'     method = "HRM"
#'   )
#'
#'   # View corrected data
#'   head(result$vh_corrected)
#'
#'   # Check offset statistics
#'   print(result$offset_summary)
#' }
#' }
#'
#' @seealso \code{\link{check_heartwood_reference_available}},
#'   \code{\link{apply_spacing_correction_per_segment}},
#'   \code{\link{calculate_burgess_coefficients}}
#'
#' @family spacing correction functions
#' @export
apply_heartwood_reference_correction <- function(vh_data,
                                                  method = "HRM",
                                                  method_col = "method",
                                                  vh_col = "Vh_cm_hr",
                                                  k_assumed = 0.0025,
                                                  probe_spacing = 0.5,
                                                  measurement_time = 80,
                                                  lookup_table = NULL,
                                                  create_new_col = TRUE,
                                                  verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "pulse_id", "sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for both inner and outer sensors
  sensor_positions <- unique(vh_data$sensor_position)
  if (!all(c("inner", "outer") %in% sensor_positions)) {
    stop("vh_data must contain both 'inner' and 'outer' sensor positions")
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("HEARTWOOD REFERENCE CORRECTION\n")
    cat(strrep("=", 72), "\n")
    cat("Method:", method, "\n")
    cat("Velocity column:", vh_col, "\n")
    cat(strrep("-", 72), "\n\n")
  }

  # Generate or validate lookup table
  if (is.null(lookup_table)) {
    if (verbose) cat("Generating Burgess lookup table...\n")
    lookup_table <- calculate_burgess_coefficients(
      zero_offset_range = c(-10, 10),
      resolution = 0.1,
      k = k_assumed,
      x = probe_spacing,
      t = measurement_time
    )
  }

  # Filter for specified method
  method_data <- vh_data[vh_data[[method_col]] == method, ]

  if (nrow(method_data) == 0) {
    stop("No data found for method '", method, "'")
  }

  # Separate inner and outer sensor data
  inner_data <- method_data[method_data$sensor_position == "inner", ]
  outer_data <- method_data[method_data$sensor_position == "outer", ]

  if (nrow(inner_data) == 0 || nrow(outer_data) == 0) {
    stop("Both inner and outer sensor data are required for heartwood reference correction")
  }

  if (verbose) {
    cat("Inner sensor observations:", nrow(inner_data), "\n")
    cat("Outer sensor observations:", nrow(outer_data), "\n\n")
  }

  # Match inner and outer by pulse_id
  # Create a lookup for inner sensor Vh by pulse_id
  inner_lookup <- stats::setNames(inner_data[[vh_col]], inner_data$pulse_id)

  # Get matching inner Vh for each outer observation
  outer_data$inner_vh_offset <- inner_lookup[as.character(outer_data$pulse_id)]

  # Check for unmatched pulses
  n_unmatched <- sum(is.na(outer_data$inner_vh_offset))
  if (n_unmatched > 0) {
    if (verbose) {
      cat("Warning:", n_unmatched, "outer observations have no matching inner sensor reading\n")
      cat("  These will not be corrected.\n\n")
    }
  }

  # Calculate offset summary statistics
  valid_offsets <- outer_data$inner_vh_offset[!is.na(outer_data$inner_vh_offset)]
  offset_summary <- list(
    n_observations = length(valid_offsets),
    mean_offset = mean(valid_offsets, na.rm = TRUE),
    median_offset = stats::median(valid_offsets, na.rm = TRUE),
    sd_offset = stats::sd(valid_offsets, na.rm = TRUE),
    min_offset = min(valid_offsets, na.rm = TRUE),
    max_offset = max(valid_offsets, na.rm = TRUE),
    range_offset = diff(range(valid_offsets, na.rm = TRUE))
  )

  # Assess offset quality
  if (abs(offset_summary$mean_offset) < 1 && offset_summary$sd_offset < 1) {
    offset_summary$quality <- "GOOD"
    offset_summary$interpretation <- "Probe well-aligned, stable offset"
  } else if (abs(offset_summary$mean_offset) < 3 && offset_summary$sd_offset < 2) {
    offset_summary$quality <- "ACCEPTABLE"
    offset_summary$interpretation <- "Moderate misalignment, correction recommended"
  } else if (abs(offset_summary$mean_offset) >= 3) {
    offset_summary$quality <- "WARNING"
    offset_summary$interpretation <- "Significant misalignment detected - check probe installation"
  } else {
    offset_summary$quality <- "VARIABLE"
    offset_summary$interpretation <- "High offset variability - may indicate installation issues"
  }

  if (verbose) {
    cat("Offset Statistics:\n")
    cat("  Mean offset: ", sprintf("%.2f", offset_summary$mean_offset), " cm/hr\n", sep = "")
    cat("  SD offset:   ", sprintf("%.2f", offset_summary$sd_offset), " cm/hr\n", sep = "")
    cat("  Range:       ", sprintf("%.2f to %.2f", offset_summary$min_offset, offset_summary$max_offset), " cm/hr\n", sep = "")
    cat("  Quality:     ", offset_summary$quality, "\n", sep = "")
    cat("  ", offset_summary$interpretation, "\n\n", sep = "")
  }

  # Apply correction for each observation
  corrected_vh <- numeric(nrow(outer_data))

  for (i in seq_len(nrow(outer_data))) {
    offset <- outer_data$inner_vh_offset[i]
    original_vh <- outer_data[[vh_col]][i]

    if (is.na(offset) || is.na(original_vh)) {
      corrected_vh[i] <- NA_real_
      next
    }

    # Get correction coefficients for this offset
    coef <- get_correction_coefficients(
      zero_offset = offset,
      lookup_table = lookup_table,
      verbose = FALSE
    )

    # Apply correction: Vh_corrected = a * Vh + b
    corrected_vh[i] <- coef$a * original_vh + coef$b
  }

  # Create output column name
  if (create_new_col) {
    corrected_col <- paste0(vh_col, "_hrc")
  } else {
    corrected_col <- vh_col
  }

  # Add corrected values to outer_data
  outer_data[[corrected_col]] <- corrected_vh
  outer_data$heartwood_ref_applied <- !is.na(corrected_vh)

  # Also mark inner data (no correction needed/applied)
  inner_data[[corrected_col]] <- inner_data[[vh_col]]  # Inner stays same
  inner_data$heartwood_ref_applied <- FALSE
  inner_data$inner_vh_offset <- NA_real_

  # Combine back together
  corrected_data <- rbind(inner_data, outer_data)

  # Sort by datetime and pulse_id
  corrected_data <- corrected_data[order(corrected_data$datetime, corrected_data$pulse_id), ]

  # Also include non-method data (uncorrected)
  other_data <- vh_data[vh_data[[method_col]] != method, ]
  if (nrow(other_data) > 0) {
    other_data[[corrected_col]] <- other_data[[vh_col]]
    other_data$heartwood_ref_applied <- FALSE
    other_data$inner_vh_offset <- NA_real_
    corrected_data <- rbind(corrected_data, other_data)
    corrected_data <- corrected_data[order(corrected_data$datetime, corrected_data$pulse_id), ]
  }

  # Create metadata
  metadata <- list(
    method = method,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_outer_corrected = sum(outer_data$heartwood_ref_applied, na.rm = TRUE),
    n_outer_uncorrected = sum(!outer_data$heartwood_ref_applied, na.rm = TRUE),
    date_applied = Sys.time(),
    approach = "Heartwood reference (continuous)",
    sapfluxr_version = utils::packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("-", 72), "\n")
    cat("Correction applied to", metadata$n_outer_corrected, "outer sensor observations\n")
    if (metadata$n_outer_uncorrected > 0) {
      cat("  (", metadata$n_outer_uncorrected, " could not be corrected due to missing inner data)\n", sep = "")
    }
    cat(strrep("=", 72), "\n\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    offset_summary = offset_summary,
    lookup_table = lookup_table,
    metadata = metadata
  )

  class(result) <- c("heartwood_reference_correction_result", "list")

  return(result)
}

#' Print Method for Heartwood Reference Correction Result
#'
#' @param x A heartwood_reference_correction_result object
#' @param ... Additional arguments (ignored)
#' @export
print.heartwood_reference_correction_result <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("HEARTWOOD REFERENCE CORRECTION RESULT\n")
  cat(strrep("=", 60), "\n\n")

  cat("Method:", x$metadata$method, "\n")
  cat("Approach:", x$metadata$approach, "\n\n")

  cat("Observations Corrected:\n")
  cat("  Outer sensor:", x$metadata$n_outer_corrected, "\n")
  if (x$metadata$n_outer_uncorrected > 0) {
    cat("  Uncorrected: ", x$metadata$n_outer_uncorrected, " (missing inner data)\n", sep = "")
  }
  cat("\n")

  cat("Offset Summary:\n")
  cat("  Mean:   ", sprintf("%.2f", x$offset_summary$mean_offset), " cm/hr\n", sep = "")
  cat("  SD:     ", sprintf("%.2f", x$offset_summary$sd_offset), " cm/hr\n", sep = "")
  cat("  Range:  ", sprintf("%.2f to %.2f", x$offset_summary$min_offset, x$offset_summary$max_offset), " cm/hr\n", sep = "")
  cat("  Quality:", x$offset_summary$quality, "\n")
  cat("  ", x$offset_summary$interpretation, "\n", sep = "")
  cat("\n")
  cat(strrep("=", 60), "\n")

  invisible(x)
}
