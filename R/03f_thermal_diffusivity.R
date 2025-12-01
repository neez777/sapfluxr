# R/03b_thermal_diffusivity.R
# Thermal Diffusivity Estimation from Tmax at Zero Flow
# Implements k estimation for validating assumed thermal diffusivity values

#' Thermal Diffusivity Estimation Functions
#'
#' Functions for estimating thermal diffusivity (k) from time to maximum
#' temperature (Tmax) during zero-flow periods. Used to validate assumed
#' k values and detect probe asymmetry.
#'
#' @name thermal_diffusivity
NULL


#' Estimate Thermal Diffusivity from Tmax at Zero Flow
#'
#' Estimates thermal diffusivity (k) from time to maximum temperature during
#' zero-flow periods. At zero flow, heat spreads symmetrically by conduction,
#' allowing k to be calculated from: k = x² / (4 * t_max)
#'
#' @param heat_pulse_data Heat pulse data object from \code{\link{read_heat_pulse_data}}
#' @param zero_periods List of zero-flow periods (see \code{\link{calculate_zero_offset}})
#' @param sensors Vector of sensor positions (default: c("outer", "inner"))
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param k_nominal Nominal k for comparison (default: 0.0025)
#' @param pre_pulse Pre-pulse period (sec) (default: 30)
#'
#' @return A list containing:
#'   \item{k_mean}{Overall mean estimated k}
#'   \item{k_sd}{Standard deviation of k estimates}
#'   \item{k_cv}{Coefficient of variation}
#'   \item{k_by_sensor}{List of k estimates per sensor}
#'   \item{k_nominal}{Nominal k value for comparison}
#'   \item{k_difference}{Difference from nominal (k_mean - k_nominal)}
#'   \item{k_difference_percent}{Percent difference from nominal}
#'   \item{symmetry_results}{Upstream/downstream symmetry checks}
#'   \item{recommendation}{Action recommendation based on difference}
#'   \item{probe_spacing}{Probe spacing used}
#'   \item{calculation_method}{Description of calculation method}
#'
#' @details
#' **Calculation Method:**
#'
#' At zero flow, heat spreads symmetrically:
#'
#' k = x^2 / (4 * t_max)
#'
#' where:
#' \itemize{
#'   \item x = probe spacing (cm)
#'   \item t_max = time to maximum temperature (seconds)
#' }
#'
#' **Symmetry Check:**
#'
#' At zero flow, upstream and downstream probes should reach maximum
#' temperature at the same time. Asymmetry indicates:
#' \itemize{
#'   \item Less than 3 sec difference: Good probe alignment
#'   \item 3-5 sec difference: Moderate asymmetry (caution)
#'   \item Greater than 5 sec difference: Severe asymmetry (likely misalignment)
#' }
#'
#' **Difference Assessment:**
#'
#' Percent difference from nominal k:
#' \itemize{
#'   \item Less than 5 percent: No action needed - k values agree well
#'   \item 5-10 percent: Within typical range - reprocessing optional
#'   \item 10-20 percent: Reprocessing recommended for publication quality
#'   \item Greater than 20 percent: Reprocessing critical - substantial systematic error
#' }
#'
#' @examples
#' \dontrun{
#' # Estimate k from zero-flow periods
#' zero_periods <- list(
#'   list(start = "2024-05-01 00:00:00", end = "2024-05-05 23:59:59")
#' )
#'
#' k_result <- estimate_k_from_tmax(
#'   heat_pulse_data = heat_pulse_data,
#'   zero_periods = zero_periods,
#'   sensors = c("outer", "inner"),
#'   k_nominal = 0.0025
#' )
#'
#' print(k_result$k_mean)
#' print(k_result$recommendation)
#' }
#'
#' @family thermal diffusivity functions
#' @export
estimate_k_from_tmax <- function(heat_pulse_data,
                                  zero_periods,
                                  sensors = c("outer", "inner"),
                                  probe_spacing = 0.5,
                                  k_nominal = 0.0025,
                                  pre_pulse = 30) {

  # Input validation
  if (!inherits(heat_pulse_data, "heat_pulse_data")) {
    stop("heat_pulse_data must be a heat_pulse_data object from read_heat_pulse_data()")
  }

  if (!is.list(zero_periods) || length(zero_periods) == 0) {
    stop("zero_periods must be a non-empty list")
  }

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("THERMAL DIFFUSIVITY ESTIMATION (from Tmax at zero flow)\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  k_estimates <- list()
  symmetry_results <- list()

  for (sensor in sensors) {

    cat("\n")
    cat("Processing sensor:", toupper(sensor), "\n")
    cat(strrep("-", 72), "\n")

    # Extract zero-flow temperature data
    tryCatch({
      zero_temps <- extract_zero_flow_temperatures(
        heat_pulse_data = heat_pulse_data,
        zero_periods = zero_periods,
        sensor_position = sensor
      )

      if (is.null(zero_temps) || nrow(zero_temps) == 0) {
        warning("No temperature data for sensor ", sensor, " - skipping")
        cat("  ✗ No temperature data available\n")
        next
      }

      # Calculate time to max for each pulse in zero periods
      tmax_results <- calculate_time_to_max_temperatures(
        zero_temps,
        pre_pulse = pre_pulse
      )

      if (is.null(tmax_results)) {
        warning("Could not calculate Tmax for sensor ", sensor)
        cat("  ✗ Could not calculate time to maximum\n")
        next
      }

      # Extract times
      t_max_down <- tmax_results$t_max_downstream
      t_max_up <- tmax_results$t_max_upstream

      # Remove invalid values
      valid_down <- t_max_down > 0 & is.finite(t_max_down)
      valid_up <- t_max_up > 0 & is.finite(t_max_up)

      t_max_down <- t_max_down[valid_down]
      t_max_up <- t_max_up[valid_up]

      if (length(t_max_down) == 0 || length(t_max_up) == 0) {
        warning("No valid Tmax data for sensor ", sensor)
        cat("  ✗ No valid time to maximum data\n")
        next
      }

      # Report Tmax statistics
      cat("  Downstream t_max:", round(mean(t_max_down), 2), "±",
          round(sd(t_max_down), 2), "s (n =", length(t_max_down), ")\n")
      cat("  Upstream t_max:", round(mean(t_max_up), 2), "±",
          round(sd(t_max_up), 2), "s (n =", length(t_max_up), ")\n")

      # Check symmetry
      t_max_down_mean <- mean(t_max_down)
      t_max_up_mean <- mean(t_max_up)
      symmetry_diff <- abs(t_max_down_mean - t_max_up_mean)
      symmetry_pct <- (symmetry_diff / mean(c(t_max_down_mean, t_max_up_mean))) * 100

      cat("  Symmetry difference:", round(symmetry_diff, 2), "s (",
          round(symmetry_pct, 1), "%)\n")

      # Assess symmetry
      symmetry_status <- "PASS"
      if (symmetry_diff > 5) {
        symmetry_status <- "FAIL"
        cat("  ⚠ WARNING: Probes show >5s asymmetry - likely misalignment\n")
      } else if (symmetry_diff > 3) {
        symmetry_status <- "CAUTION"
        cat("  ⚠ CAUTION: Moderate asymmetry detected\n")
      } else {
        cat("  ✓ Symmetry check passed\n")
      }

      symmetry_results[[sensor]] <- list(
        t_max_downstream = t_max_down_mean,
        t_max_upstream = t_max_up_mean,
        difference_seconds = symmetry_diff,
        difference_percent = symmetry_pct,
        status = symmetry_status
      )

      # Estimate k using average t_max (cm²/s)
      t_max_mean <- mean(c(t_max_down_mean, t_max_up_mean))
      k_estimated <- (probe_spacing^2) / (4 * t_max_mean)

      cat("  Estimated k:", round(k_estimated, 6), "cm²/s\n")

      k_estimates[[sensor]] <- k_estimated

    }, error = function(e) {
      warning("Error processing sensor ", sensor, ": ", e$message)
      cat("  ✗ Error:", e$message, "\n")
    })
  }

  # Check if we got any estimates
  if (length(k_estimates) == 0) {
    stop("No k estimates could be calculated from any sensor")
  }

  # Calculate overall statistics
  k_mean <- mean(unlist(k_estimates))
  k_sd <- sd(unlist(k_estimates))
  k_cv <- k_sd / k_mean

  # Compare with nominal k
  k_diff <- k_mean - k_nominal
  k_diff_pct <- (k_diff / k_nominal) * 100

  cat("\n")
  cat(strrep("-", 72), "\n")
  cat("\n")
  cat("OVERALL RESULTS\n")
  cat("  Nominal k (assumed):", k_nominal, "cm²/s\n")
  cat("  Estimated k (mean):", round(k_mean, 6), "cm²/s\n")
  cat("  Estimated k (SD):", round(k_sd, 6), "cm²/s\n")
  cat("  Difference:", sprintf("%+.6f", k_diff), "cm²/s (",
      sprintf("%+.1f", k_diff_pct), "%)\n")
  cat("\n")

  # Assess significance and provide recommendation
  recommendation <- "no_action"
  if (abs(k_diff_pct) > 20) {
    cat("  🚨 CRITICAL: k differs by >20% - REPROCESSING STRONGLY RECOMMENDED\n")
    cat("     Current results may have substantial systematic error\n")
    recommendation <- "reprocess_critical"

  } else if (abs(k_diff_pct) > 10) {
    cat("  ⚠ WARNING: k differs by >10% - reprocessing recommended\n")
    cat("     For publication-quality results, consider reprocessing\n")
    recommendation <- "reprocess_recommended"

  } else if (abs(k_diff_pct) > 5) {
    cat("  ℹ NOTE: k differs by 5-10% - within typical range\n")
    cat("     Current results acceptable for most applications\n")
    cat("     Reprocessing optional for highest precision\n")
    recommendation <- "reprocess_optional"

  } else {
    cat("  ✓ GOOD: k values agree within 5% - no reprocessing needed\n")
    recommendation <- "no_action"
  }

  # Check for radial variation
  if (length(k_estimates) > 1) {
    cat("\n")
    cat("RADIAL VARIATION CHECK\n")
    for (sensor_name in names(k_estimates)) {
      cat("  ", toupper(sensor_name), ":", round(k_estimates[[sensor_name]], 6),
          "cm²/s\n", sep = "")
    }

    if (k_cv > 0.15) {
      cat("  ⚠ High variability between sensors (CV =", round(k_cv, 3), ")\n")
      cat("     Consider using sensor-specific k values\n")
    }
  }

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  result <- list(
    k_mean = k_mean,
    k_sd = k_sd,
    k_cv = k_cv,
    k_by_sensor = k_estimates,
    k_nominal = k_nominal,
    k_difference = k_diff,
    k_difference_percent = k_diff_pct,
    symmetry_results = symmetry_results,
    recommendation = recommendation,
    probe_spacing = probe_spacing,
    calculation_method = "Tmax at zero flow: k = x²/(4*t_max)"
  )

  class(result) <- c("k_estimation_result", "list")

  return(result)
}


#' Extract Temperature Data During Zero-Flow Periods
#'
#' Filters heat pulse measurements to zero-flow calibration periods.
#'
#' @param heat_pulse_data Heat pulse data object
#' @param zero_periods List of zero-flow periods
#' @param sensor_position Sensor position ("outer" or "inner")
#'
#' @return Data frame of temperature measurements during zero-flow periods,
#'   or NULL if no data found
#'
#' @keywords internal
extract_zero_flow_temperatures <- function(heat_pulse_data,
                                           zero_periods,
                                           sensor_position) {

  measurements <- heat_pulse_data$measurements

  if (is.null(measurements) || nrow(measurements) == 0) {
    return(NULL)
  }

  # Convert datetime if needed
  if (!inherits(measurements$datetime, "POSIXct")) {
    measurements$datetime <- as.POSIXct(measurements$datetime)
  }

  # Extract measurements for all zero periods
  zero_data <- data.frame()

  for (period in zero_periods) {
    start_date <- as.POSIXct(period$start)
    end_date <- as.POSIXct(period$end)

    # Get pulse IDs in this period
    pulse_ids_in_period <- unique(
      measurements$pulse_id[
        measurements$datetime >= start_date &
        measurements$datetime <= end_date
      ]
    )

    if (length(pulse_ids_in_period) == 0) next

    # Extract all measurements for these pulses
    period_measurements <- measurements[
      measurements$pulse_id %in% pulse_ids_in_period,
    ]

    zero_data <- rbind(zero_data, period_measurements)
  }

  if (nrow(zero_data) == 0) {
    return(NULL)
  }

  # Add sensor position identifier
  zero_data$sensor_position <- sensor_position

  return(zero_data)
}


#' Calculate Time to Maximum Temperature
#'
#' Finds time to maximum temperature for downstream and upstream probes
#' during zero-flow periods.
#'
#' @param zero_temps Data frame of temperature measurements
#' @param pre_pulse Pre-pulse period (sec)
#'
#' @return List with:
#'   \item{t_max_downstream}{Vector of times to max for downstream (outer: do, inner: di)}
#'   \item{t_max_upstream}{Vector of times to max for upstream (outer: uo, inner: ui)}
#'
#' @keywords internal
calculate_time_to_max_temperatures <- function(zero_temps, pre_pulse = 30) {

  # Determine which columns to use based on sensor position
  if (!("sensor_position" %in% names(zero_temps))) {
    warning("sensor_position column missing")
    return(NULL)
  }

  sensor_pos <- unique(zero_temps$sensor_position)[1]

  # Set column names based on sensor position
  if (sensor_pos == "outer") {
    down_col <- "do"
    up_col <- "uo"
  } else if (sensor_pos == "inner") {
    down_col <- "di"
    up_col <- "ui"
  } else {
    warning("Unknown sensor position: ", sensor_pos)
    return(NULL)
  }

  # Check columns exist
  if (!all(c(down_col, up_col) %in% names(zero_temps))) {
    warning("Required temperature columns not found")
    return(NULL)
  }

  # Get unique pulse IDs
  pulse_ids <- unique(zero_temps$pulse_id)

  t_max_down <- numeric(length(pulse_ids))
  t_max_up <- numeric(length(pulse_ids))

  for (i in seq_along(pulse_ids)) {
    pulse_data <- zero_temps[zero_temps$pulse_id == pulse_ids[i], ]

    # Sort by row order (time sequence within pulse)
    pulse_data <- pulse_data[order(pulse_data$datetime), ]

    if (nrow(pulse_data) < pre_pulse + 10) {
      # Not enough data points
      t_max_down[i] <- NA_real_
      t_max_up[i] <- NA_real_
      next
    }

    # Calculate baseline (pre-pulse mean)
    pre_pulse_idx <- 1:min(pre_pulse, nrow(pulse_data))
    baseline_down <- mean(pulse_data[[down_col]][pre_pulse_idx], na.rm = TRUE)
    baseline_up <- mean(pulse_data[[up_col]][pre_pulse_idx], na.rm = TRUE)

    # Calculate delta T
    delta_down <- pulse_data[[down_col]] - baseline_down
    delta_up <- pulse_data[[up_col]] - baseline_up

    # Exclude pre-pulse period
    delta_down[pre_pulse_idx] <- NA
    delta_up[pre_pulse_idx] <- NA

    # Find index of maximum
    idx_max_down <- which.max(delta_down)
    idx_max_up <- which.max(delta_up)

    # Convert to time after pulse (seconds)
    # Index 1 = time 0 (pre-pulse start)
    # Index (pre_pulse + 1) = time 0 after pulse injection
    t_max_down[i] <- idx_max_down - pre_pulse
    t_max_up[i] <- idx_max_up - pre_pulse
  }

  return(list(
    t_max_downstream = t_max_down,
    t_max_upstream = t_max_up
  ))
}


#' Print Method for k Estimation Results
#'
#' @param x A k_estimation_result object
#' @param ... Additional arguments (ignored)
#' @export
print.k_estimation_result <- function(x, ...) {

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("THERMAL DIFFUSIVITY ESTIMATION SUMMARY\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  cat("Calculation Method:", x$calculation_method, "\n")
  cat("Probe Spacing:", x$probe_spacing, "cm\n")
  cat("\n")

  cat("RESULTS\n")
  cat(strrep("-", 72), "\n")
  cat("  Nominal k (assumed):", x$k_nominal, "cm²/s\n")
  cat("  Estimated k (mean):", round(x$k_mean, 6), "cm²/s\n")
  cat("  Estimated k (SD):", round(x$k_sd, 6), "cm²/s\n")
  cat("  Difference:", sprintf("%+.6f cm²/s (%+.1f%%)",
                               x$k_difference, x$k_difference_percent), "\n")
  cat("\n")

  cat("RECOMMENDATION:", toupper(gsub("_", " ", x$recommendation)), "\n")
  cat("\n")

  if (length(x$k_by_sensor) > 1) {
    cat("BY SENSOR\n")
    cat(strrep("-", 72), "\n")
    for (sensor in names(x$k_by_sensor)) {
      cat("  ", toupper(sensor), ": ", round(x$k_by_sensor[[sensor]], 6),
          " cm²/s\n", sep = "")
    }
    cat("\n")
  }

  cat("SYMMETRY CHECK\n")
  cat(strrep("-", 72), "\n")
  for (sensor in names(x$symmetry_results)) {
    sym <- x$symmetry_results[[sensor]]
    cat("  ", toupper(sensor), ":\n", sep = "")
    cat("    Downstream t_max:", round(sym$t_max_downstream, 2), "s\n")
    cat("    Upstream t_max:", round(sym$t_max_upstream, 2), "s\n")
    cat("    Difference:", round(sym$difference_seconds, 2), "s\n")
    cat("    Status:", sym$status, "\n")
  }

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  invisible(x)
}
