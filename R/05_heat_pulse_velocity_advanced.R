#' Advanced Heat Pulse Velocity Methods
#'
#' Implementation of Compensation Heat Pulse Method (CHPM) and
#' Double Ratio Method (DRM) for sap flow analysis
#'
#' @name advanced_hpv_methods
NULL

#' Compensation Heat Pulse Method (CHPM)
#'
#' Calculate sap velocity using the Compensation Heat Pulse Method, which determines
#' velocity based on the time when temperature rises at two probes are equal.
#' This method is suitable for asymmetric probe configurations and moderate to high
#' sap velocities.
#'
#' @param temp_data data.frame with columns: time, temp_up, temp_down, temp_far
#' @param probe_spacing list with upstream, downstream, and far probe distances (cm)
#' @param pulse_time numeric, duration of heat pulse (seconds)
#' @param max_time numeric, maximum time to search for crossover (seconds, default 400)
#' @param min_velocity numeric, minimum detectable velocity (cm/h, default 6.75)
#' @param validate logical, whether to perform data validation (default TRUE)
#'
#' @return list containing:
#'   \item{velocity_chpm}{calculated CHPM velocity (cm/h)}
#'   \item{crossover_time}{time when temp_up equals temp_far (seconds)}
#'   \item{method_applicable}{logical, whether CHPM is applicable for this data}
#'   \item{quality_metrics}{data quality indicators}
#'
#' @details
#' The CHPM calculates velocity as: V = (x1 + x3) / (2 * t_crossover)
#' where x1 and x3 are probe positions and t_crossover is when delta_1 = delta_3.
#'
#' Method limitations:
#' - Cannot detect negative (reverse) flows
#' - Requires precise crossover point determination
#' - Minimum detectable velocity depends on probe spacing and recording time
#' - Less reliable at very low or very high velocities
#'
#' Based on theory from:
#' - Deng et al. (2021) "A double-ratio method to measure fast, slow and reverse sap flows"
#' - Marshall (1958) heat pulse theory
#'
#' @references
#' Deng, Z., Vice, H., Gilbert, M., Adams, M., & Buckley, T. (2021).
#' A double-ratio method to measure fast, slow and reverse sap flows.
#'
#' @examples
#' \dontrun{
#' # Typical probe configuration for CHPM
#' spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)
#'
#' # Calculate CHPM velocity
#' result <- calc_chpm(temp_data, spacing, pulse_time = 8)
#'
#' # Check if method is applicable
#' if (result$method_applicable) {
#'   velocity <- result$velocity_chpm
#' }
#' }
#'
#' @export
calc_chpm <- function(temp_data, probe_spacing, pulse_time,
                      max_time = 400, min_velocity = 6.75, validate = TRUE) {

  # Input validation
  if (validate) {
    validate_chpm_inputs(temp_data, probe_spacing, pulse_time, max_time)
  }

  # Extract probe positions
  x1 <- probe_spacing$upstream     # negative value
  x3 <- probe_spacing$far          # positive value

  # TODO: Implement actual crossover detection algorithm
  # This requires real temperature data to calibrate the detection method

  # Placeholder implementation based on theoretical equations
  # From Deng et al. (2021): V_CHPM = (x1 + x3) / (2 * t_C(1,3))

  # Calculate minimum detectable velocity based on configuration
  min_detectable <- abs(x1 + x3) / (2 * max_time) * 3600  # convert to cm/h

  # Placeholder crossover detection
  # TODO: Replace with actual temperature curve analysis
  crossover_time <- find_temperature_crossover(temp_data)

  if (is.na(crossover_time) || crossover_time > max_time) {
    return(list(
      velocity_chpm = NA,
      crossover_time = NA,
      method_applicable = FALSE,
      quality_metrics = list(
        reason = "No crossover detected within time window",
        min_detectable_velocity = min_detectable
      )
    ))
  }

  # Calculate CHPM velocity (cm/h)
  velocity_chpm <- (abs(x1) + x3) / (2 * crossover_time) * 3600

  # Check if velocity is within detectable range
  method_applicable <- velocity_chpm >= min_velocity && velocity_chpm <= get_max_chpm_velocity(probe_spacing)

  # Quality assessment
  quality_metrics <- assess_chpm_quality(temp_data, crossover_time, velocity_chpm)
  quality_metrics$min_detectable_velocity <- min_detectable

  # If method is not applicable, return NA for velocity
  if (!method_applicable) {
    velocity_chpm <- NA
  }

  return(list(
    velocity_chpm = velocity_chpm,
    crossover_time = crossover_time,
    method_applicable = method_applicable,
    quality_metrics = quality_metrics
  ))
}

#' Double Ratio Method (DRM)
#'
#' Calculate sap velocity using the Double Ratio Method, which combines
#' two ratio-based velocity estimates and selects the one with lower uncertainty.
#' This method provides robust measurements across a wide range of velocities.
#'
#' @param temp_data data.frame with columns: time, temp_up, temp_down, temp_far
#' @param probe_spacing list with upstream, downstream, and far probe distances (cm)
#' @param thermal_diffusivity numeric, thermal diffusivity (cm²/s, default 0.0025)
#' @param sensor_noise numeric, standard deviation of temperature sensor noise (K, default 0.02)
#' @param time_window numeric vector, time window for averaging (seconds, default c(60, 100))
#' @param validate logical, whether to perform data validation (default TRUE)
#'
#' @return list containing:
#'   \item{velocity_drm}{final DRM velocity estimate (cm/h)}
#'   \item{velocity_12}{velocity from probes 1-2 (cm/h)}
#'   \item{velocity_23}{velocity from probes 2-3 (cm/h)}
#'   \item{uncertainty_12}{uncertainty in V12 estimate}
#'   \item{uncertainty_23}{uncertainty in V23 estimate}
#'   \item{selected_method}{which estimate was selected ("V12" or "V23")}
#'   \item{thermal_diffusivity_realtime}{real-time k estimate if available}
#'   \item{quality_metrics}{comprehensive quality assessment}
#'
#' @details
#' The DRM calculates two velocity estimates:
#' - V12: Using temperature ratio between upstream and downstream probes
#' - V23: Using temperature ratio between downstream and far probes
#'
#' Selection criteria based on uncertainty:
#' - Choose V12 if σ12 ≤ σ23 (typically at low velocities)
#' - Choose V23 if σ23 < σ12 (typically at high velocities)
#'
#' Advantages over single methods:
#' - Works across unprecedented velocity range (-10 to 80+ cm/h)
#' - Objective method selection based on uncertainty
#' - Can provide real-time thermal diffusivity estimates
#' - Robust against sensor noise at high velocities
#'
#' @references
#' Deng, Z., Vice, H., Gilbert, M., Adams, M., & Buckley, T. (2021).
#' A double-ratio method to measure fast, slow and reverse sap flows.
#'
#' @examples
#' \dontrun{
#' # Standard DRM probe configuration
#' spacing <- list(upstream = -0.75, downstream = 0.75, far = 2.25)
#'
#' # Calculate DRM velocity
#' result <- calc_drm(temp_data, spacing, thermal_diffusivity = 0.0025)
#'
#' # Extract results
#' velocity <- result$velocity_drm
#' method_used <- result$selected_method
#' }
#'
#' @export
calc_drm <- function(temp_data, probe_spacing, thermal_diffusivity = 0.0025,
                     sensor_noise = 0.02, time_window = c(60, 100), validate = TRUE) {

  # Input validation
  if (validate) {
    validate_drm_inputs(temp_data, probe_spacing, thermal_diffusivity, time_window)
  }

  # Extract probe positions
  x1 <- probe_spacing$upstream     # typically -0.75 cm
  x2 <- probe_spacing$downstream   # typically +0.75 cm
  x3 <- probe_spacing$far          # typically +2.25 cm

  # Extract temperature rises within time window
  time_mask <- temp_data$time >= time_window[1] & temp_data$time <= time_window[2]
  temp_window <- temp_data[time_mask, ]

  if (nrow(temp_window) == 0) {
    stop("No data points in specified time window")
  }

  # Calculate average temperature rises in window
  delta1 <- mean(temp_window$temp_up, na.rm = TRUE)
  delta2 <- mean(temp_window$temp_down, na.rm = TRUE)
  delta3 <- mean(temp_window$temp_far, na.rm = TRUE)

  # Average time for calculations
  t_avg <- mean(temp_window$time)

  # Calculate V12 (HRM-style between probes 1 and 2)
  # From Deng et al.: V12 = (2k)/(x2-x1) * ln(δ2/δ1) + (x1+x2)/(2t)
  if (delta1 > 0 && delta2 > 0) {
    v12 <- (2 * thermal_diffusivity) / (x2 - x1) * log(delta2 / delta1) +
      (x1 + x2) / (2 * t_avg)
    v12 <- v12 * 3600  # convert to cm/h

    # Calculate uncertainty for V12
    sigma12 <- (2 * thermal_diffusivity * sensor_noise) / (x2 - x1) *
      sqrt(1/delta2^2 + 1/delta1^2)
  } else {
    v12 <- NA
    sigma12 <- Inf
  }

  # Calculate V23 (between probes 2 and 3)
  # V23 = (2k)/(x3-x2) * ln(δ3/δ2) + (x2+x3)/(2t)
  if (delta2 > 0 && delta3 > 0) {
    v23 <- (2 * thermal_diffusivity) / (x3 - x2) * log(delta3 / delta2) +
      (x2 + x3) / (2 * t_avg)
    v23 <- v23 * 3600  # convert to cm/h

    # Calculate uncertainty for V23
    sigma23 <- (2 * thermal_diffusivity * sensor_noise) / (x3 - x2) *
      sqrt(1/delta3^2 + 1/delta2^2)
  } else {
    v23 <- NA
    sigma23 <- Inf
  }

  # Select method based on uncertainty (Equation 7 from Deng et al.)
  if (!is.na(v12) && !is.na(v23)) {
    if (sigma12 <= sigma23) {
      velocity_drm <- v12
      selected_method <- "V12"
    } else {
      velocity_drm <- v23
      selected_method <- "V23"
    }
  } else if (!is.na(v12)) {
    velocity_drm <- v12
    selected_method <- "V12"
  } else if (!is.na(v23)) {
    velocity_drm <- v23
    selected_method <- "V23"
  } else {
    velocity_drm <- NA
    selected_method <- "None"
  }

  # Attempt real-time thermal diffusivity estimation
  # TODO: Implement using CHPM crossover when velocity is high enough
  k_realtime <- estimate_realtime_diffusivity(temp_data, probe_spacing, velocity_drm)

  # Quality assessment
  quality_metrics <- assess_drm_quality(
    delta1, delta2, delta3, v12, v23, sigma12, sigma23, selected_method
  )

  return(list(
    velocity_drm = velocity_drm,
    velocity_12 = v12,
    velocity_23 = v23,
    uncertainty_12 = sigma12,
    uncertainty_23 = sigma23,
    selected_method = selected_method,
    thermal_diffusivity_realtime = k_realtime,
    quality_metrics = quality_metrics
  ))
}

#' Estimate Real-time Thermal Diffusivity
#'
#' Estimate thermal diffusivity in real-time by combining DRM and CHPM estimates
#' when sap velocity is in the appropriate range.
#'
#' @param temp_data temperature data
#' @param probe_spacing probe configuration
#' @param velocity_drm current DRM velocity estimate
#'
#' @return numeric, estimated thermal diffusivity or NA if not applicable
#'
#' @details
#' From Deng et al. (2021), Equation 14:
#' k = ((x3-x2)(x1-x2)) / (4*t_C(1,3) * ln(δ3/δ2))
#'
#' This requires velocity to be high enough for CHPM applicability
#' but not so high that crossover detection becomes unreliable.
#'
#' @keywords internal
estimate_realtime_diffusivity <- function(temp_data, probe_spacing, velocity_drm) {

  # TODO: Implement real-time k estimation using CHPM crossover
  # This requires integration with CHPM crossover detection

  # Check if velocity is in applicable range for k estimation
  if (is.na(velocity_drm) || velocity_drm < 6.75 || velocity_drm > 50) {
    return(NA)
  }

  # Placeholder - needs real crossover detection implementation
  return(NA)
}

# Helper functions for validation and quality assessment
# These are placeholders that need refinement with real data

validate_chpm_inputs <- function(temp_data, probe_spacing, pulse_time, max_time) {
  # Input validation for CHPM function
  if (!is.data.frame(temp_data)) {
    stop("temp_data must be a data.frame")
  }

  required_cols <- c("time", "temp_up", "temp_down", "temp_far")
  missing_cols <- setdiff(required_cols, names(temp_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!is.list(probe_spacing)) {
    stop("probe_spacing must be a list")
  }

  if (pulse_time <= 0) {
    stop("pulse_time must be positive")
  }

  if (max_time <= 0) {
    stop("max_time must be positive")
  }
}

validate_drm_inputs <- function(temp_data, probe_spacing, thermal_diffusivity, time_window) {
  # Input validation for DRM function
  if (!is.data.frame(temp_data)) {
    stop("temp_data must be a data.frame")
  }

  required_cols <- c("time", "temp_up", "temp_down", "temp_far")
  missing_cols <- setdiff(required_cols, names(temp_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (thermal_diffusivity <= 0) {
    stop("thermal_diffusivity must be positive")
  }

  if (length(time_window) != 2 || time_window[1] >= time_window[2]) {
    stop("time_window must be a vector of length 2 with start < end")
  }
}

find_temperature_crossover <- function(temp_data) {
  # Implement robust crossover detection algorithm
  # This needs careful implementation with real temperature data
  # Should detect when temp_up equals temp_far within noise tolerance

  if (!"temp_up" %in% names(temp_data) || !"temp_far" %in% names(temp_data)) {
    return(NA)
  }

  # Simple implementation: find where curves are closest
  temp_diff <- abs(temp_data$temp_up - temp_data$temp_far)
  min_diff_idx <- which.min(temp_diff)

  if (length(min_diff_idx) > 0 && min_diff_idx <= nrow(temp_data)) {
    return(temp_data$time[min_diff_idx])
  }

  return(NA)
}

assess_chpm_quality <- function(temp_data, crossover_time, velocity) {
  # Implement quality metrics specific to CHPM

  # Basic quality indicators
  crossover_confidence <- ifelse(is.na(crossover_time), 0, 0.5)  # Placeholder

  # Estimate noise level from first few data points
  if (nrow(temp_data) > 10) {
    temp_noise <- sd(temp_data$temp_up[1:10], na.rm = TRUE)
  } else {
    temp_noise <- 0.02  # Default assumption
  }

  list(
    crossover_confidence = crossover_confidence,
    temperature_noise_level = temp_noise,
    method_reliability = ifelse(is.na(velocity), "poor", "needs_validation")
  )
}

assess_drm_quality <- function(delta1, delta2, delta3, v12, v23, sigma12, sigma23, selected_method) {
  # TODO: Implement comprehensive quality assessment
  list(
    temperature_signal_quality = min(delta1, delta2, delta3, na.rm = TRUE),
    uncertainty_ratio = ifelse(is.finite(sigma12) && is.finite(sigma23),
                               min(sigma12, sigma23) / max(sigma12, sigma23), NA),
    velocity_consistency = ifelse(!is.na(v12) && !is.na(v23),
                                  abs(v12 - v23) / (abs(v12) + abs(v23)) * 2, NA),
    selected_method = selected_method,
    data_quality_flag = "pending_validation"
  )
}

get_max_chpm_velocity <- function(probe_spacing) {
  # Calculate maximum detectable velocity for CHPM
  # This depends on probe spacing, heat pulse strength, and sensor resolution
  # For typical configurations, maximum is around 50 cm/h
  return(50)  # placeholder value
}#' Advanced HPV Method Utilities
#'
#' Supporting functions for CHPM and DRM implementations
#'
#' @name advanced_hpv_utilities
NULL

#' Optimise Time Window for DRM
#'
#' Determine the optimal time window for DRM calculations based on
#' uncertainty minimization principle from Deng et al. (2021).
#'
#' @param temp_data data.frame with temperature time series
#' @param probe_spacing probe configuration
#' @param thermal_diffusivity thermal diffusivity estimate
#' @param sensor_noise sensor noise level
#' @param window_sizes vector of window sizes to test (default 5 to 100 seconds)
#'
#' @return list with optimal window parameters
#'
#' @details
#' From Deng et al. (2021), Equation 10:
#' SE_DRM = (1/n_t) * sqrt(sum(σ²_DRM(t)))
#'
#' This finds the time window that minimises the standard error of the mean
#' estimated velocity based on theoretical uncertainty.
#'
#' @export
optimize_drm_window <- function(temp_data, probe_spacing, thermal_diffusivity = 0.0025,
                                sensor_noise = 0.02, window_sizes = seq(5, 100, 5)) {

  # TODO: Implement window optimization algorithm
  # This requires iterating through different window sizes and positions
  # to find minimum uncertainty

  # Placeholder implementation
  # According to paper, optimal window varies with velocity:
  # - 80-100s for V = 30 cm/h
  # - 20-40s for V = 80 cm/h
  # - Recommended default: 40s (same as HRM)

  return(list(
    optimal_start = 60,
    optimal_end = 100,
    optimal_size = 40,
    min_uncertainty = NA,
    window_analysis = "requires_implementation"
  ))
}

#' Detect Optimal Heat Pulse Method
#'
#' Automatically determine which heat pulse method is most appropriate
#' for given conditions and data quality.
#'
#' @param temp_data temperature data
#' @param probe_config probe configuration object
#' @param velocity_estimate rough velocity estimate if available
#'
#' @return character indicating recommended method(s)
#'
#' @details
#' Decision tree based on:
#' - Probe configuration (symmetric vs asymmetric)
#' - Estimated velocity range
#' - Data quality indicators
#' - Temperature signal strength
#'
#' @export
detect_optimal_method <- function(temp_data, probe_config, velocity_estimate = NULL) {

  # Determine probe configuration type
  # Handle both config_name and configuration_type
  config_type <- if (!is.null(probe_config$config_name)) {
    probe_config$config_name
  } else if (!is.null(probe_config$configuration_type)) {
    probe_config$configuration_type
  } else {
    probe_config$name
  }

  # Assess data quality
  data_quality <- assess_temp_data_quality(temp_data)

  # Method selection logic
  methods <- character(0)

  # Always consider DRM for 3+ probe configurations
  if (config_type %in% c("three_probe_symmetric", "three_probe_asymmetric")) {
    methods <- c(methods, "DRM")
  }

  # Consider CHPM for appropriate configurations and velocities
  if (config_type == "three_probe_asymmetric" &&
      (is.null(velocity_estimate) || (velocity_estimate >= 6.75 && velocity_estimate <= 50))) {
    methods <- c(methods, "CHPM")
  }

  # Consider HRM for basic configurations or low velocities
  if (is.null(velocity_estimate) || velocity_estimate <= 40) {
    methods <- c(methods, "HRM")
  }

  # Consider Tmax for single probe configurations
  if (config_type == "single_probe") {
    methods <- c(methods, "Tmax")
  }

  return(list(
    recommended_methods = methods,
    primary_method = methods[1],
    data_quality = data_quality,
    reasoning = generate_method_reasoning(config_type, velocity_estimate, data_quality)
  ))
}

#' Calculate Method Compatibility Matrix
#'
#' Generate a matrix showing which methods are compatible with
#' different probe configurations and conditions.
#'
#' @param probe_configs list of probe configuration objects
#'
#' @return data.frame with method compatibility
#'
#' @export
calc_method_compatibility <- function(probe_configs) {

  methods <- c("HRM", "MHR", "Tmax_Coh", "Tmax_Klu", "CHPM", "DRM")

  compatibility <- data.frame(
    configuration = character(0),
    HRM = logical(0),
    MHR = logical(0),
    Tmax_Coh = logical(0),
    Tmax_Klu = logical(0),
    CHPM = logical(0),
    DRM = logical(0),
    stringsAsFactors = FALSE
  )

  # Handle single config or list of configs
  if (!is.list(probe_configs) || (is.list(probe_configs) && !is.null(names(probe_configs)) && "configuration_type" %in% names(probe_configs))) {
    probe_configs <- list(probe_configs)
  }

  for (i in seq_along(probe_configs)) {
    config <- probe_configs[[i]]

    # Safely extract configuration name
    config_name <- tryCatch({
      if (!is.null(config$name)) {
        config$name
      } else if (!is.null(config$configuration_type)) {
        config$configuration_type
      } else {
        paste("config", i)
      }
    }, error = function(e) {
      paste("config", i)
    })

    # Safely extract configuration properties
    probe_count <- tryCatch({
      if (!is.null(config$probe_count)) config$probe_count else 2
    }, error = function(e) 2)

    symmetric <- tryCatch({
      if (!is.null(config$symmetric)) config$symmetric else TRUE
    }, error = function(e) TRUE)

    asymmetric <- tryCatch({
      if (!is.null(config$asymmetric)) config$asymmetric else FALSE
    }, error = function(e) FALSE)

    # Create compatibility row
    row <- data.frame(
      configuration = config_name,
      HRM = probe_count >= 2 && symmetric,
      MHR = probe_count >= 2,
      Tmax_Coh = probe_count >= 1,
      Tmax_Klu = probe_count >= 1,
      CHPM = probe_count >= 2 && asymmetric,
      DRM = probe_count >= 3,
      stringsAsFactors = FALSE
    )

    compatibility <- rbind(compatibility, row)
  }

  return(compatibility)
}

#' Assess Temperature Data Quality for HPV Methods
#'
#' Evaluate the quality of temperature data for heat pulse calculations.
#'
#' @param temp_data temperature time series data
#' @param min_signal_ratio minimum signal-to-noise ratio (default 3)
#'
#' @return list with quality metrics
#'
#' @export
assess_temp_data_quality <- function(temp_data, min_signal_ratio = 3) {

  # Calculate basic statistics
  temp_cols <- grep("temp_", names(temp_data), value = TRUE)

  quality_metrics <- list()

  for (col in temp_cols) {
    if (col %in% names(temp_data)) {
      temp_values <- temp_data[[col]]

      # Remove baseline (first few values)
      baseline <- mean(temp_values[1:min(5, length(temp_values))], na.rm = TRUE)
      signal <- temp_values - baseline
      peak_signal <- max(signal, na.rm = TRUE)

      # Handle case where all values are NA or invalid
      if (!is.finite(peak_signal)) {
        peak_signal <- 0
      }

      # Calculate noise from variation in baseline period
      noise_level <- sd(temp_values[1:10], na.rm = TRUE)

      # Ensure noise_level is not zero to avoid division by zero
      if (noise_level == 0 || is.na(noise_level)) {
        noise_level <- 0.001  # Small default value
      }

      signal_to_noise <- peak_signal / noise_level

      quality_metrics[[col]] <- list(
        peak_signal = peak_signal,
        noise_level = noise_level,
        signal_to_noise = signal_to_noise,
        adequate_signal = signal_to_noise >= min_signal_ratio
      )
    }
  }

  # Overall quality assessment
  if (length(quality_metrics) > 0) {
    all_adequate <- all(sapply(quality_metrics, function(x) x$adequate_signal))
    min_snr <- min(sapply(quality_metrics, function(x) x$signal_to_noise), na.rm = TRUE)
  } else {
    all_adequate <- FALSE
    min_snr <- 0
  }

  overall_quality <- list(
    individual_sensors = quality_metrics,
    overall_adequate = all_adequate,
    min_snr = min_snr,
    quality_flag = ifelse(all_adequate, "good", "poor")
  )

  return(overall_quality)
}

#' Generate Method Selection Reasoning
#'
#' Provide human-readable explanation for method selection.
#'
#' @param config_type probe configuration type
#' @param velocity_estimate velocity estimate
#' @param data_quality data quality assessment
#'
#' @return character with reasoning
#'
#' @keywords internal
generate_method_reasoning <- function(config_type, velocity_estimate, data_quality) {

  reasons <- character(0)

  # Configuration-based reasoning
  if (config_type == "three_probe_asymmetric") {
    reasons <- c(reasons, "Three-probe asymmetric configuration supports DRM and CHPM")
  } else if (config_type == "three_probe_symmetric") {
    reasons <- c(reasons, "Three-probe symmetric configuration supports DRM")
  } else if (config_type == "two_probe_symmetric") {
    reasons <- c(reasons, "Two-probe symmetric configuration supports HRM and MHR")
  } else if (config_type == "single_probe") {
    reasons <- c(reasons, "Single-probe configuration limited to Tmax methods")
  }

  # Velocity-based reasoning
  if (!is.null(velocity_estimate)) {
    if (velocity_estimate < 0) {
      reasons <- c(reasons, "Negative velocity detected: only DRM and HRM can measure reverse flows")
    } else if (velocity_estimate < 20) {
      reasons <- c(reasons, "Low velocity range: HRM and DRM recommended")
    } else if (velocity_estimate > 40) {
      reasons <- c(reasons, "High velocity range: DRM recommended, HRM may plateau")
    }
  }

  # Data quality reasoning
  if (!is.null(data_quality$quality_flag) && data_quality$quality_flag == "poor") {
    reasons <- c(reasons, "Poor signal quality may affect method reliability")
  }

  return(paste(reasons, collapse = "; "))
}

#' Unified Heat Pulse Velocity Calculator
#'
#' Main function that automatically selects and applies the most appropriate
#' heat pulse method(s) based on probe configuration and data characteristics.
#'
#' @param temp_data temperature data
#' @param probe_config probe configuration
#' @param methods character vector of methods to try (default "auto")
#' @param thermal_diffusivity thermal diffusivity estimate
#' @param auto_optimise logical, whether to optimise parameters automatically
#'
#' @return list with results from all applicable methods
#'
#' @details
#' This function serves as the main interface for advanced heat pulse calculations.
#' It automatically determines the best method(s) to use and provides comprehensive
#' results with quality assessments.
#'
#' @examples
#' \dontrun{
#' # Automatic method selection
#' result <- calc_unified_hpv(temp_data, probe_config)
#'
#' # Manual method specification
#' result <- calc_unified_hpv(temp_data, probe_config, methods = c("DRM", "CHPM"))
#' }
#'
#' @export
calc_unified_hpv <- function(temp_data, probe_config, methods = "auto",
                             thermal_diffusivity = 0.0025, auto_optimise = TRUE) {

  # Automatic method detection if requested
  if (length(methods) == 1 && methods == "auto") {
    method_detection <- detect_optimal_method(temp_data, probe_config)
    methods <- method_detection$recommended_methods
  }

  # Results container
  results <- list(
    metadata = list(
      probe_config = probe_config,
      methods_attempted = methods,
      thermal_diffusivity = thermal_diffusivity,
      timestamp = Sys.time()
    ),
    data_quality = assess_temp_data_quality(temp_data),
    method_results = list()
  )

  # Apply each method
  for (method in methods) {

    method_result <- tryCatch({

      switch(method,
             "DRM" = {
               if (probe_config$probe_count >= 3) {
                 calc_drm(temp_data, probe_config$spacing, thermal_diffusivity)
               } else {
                 list(error = "Insufficient probes for DRM")
               }
             },

             "CHPM" = {
               if (probe_config$probe_count >= 2 && probe_config$asymmetric) {
                 calc_chpm(temp_data, probe_config$spacing, probe_config$pulse_time)
               } else {
                 list(error = "Inappropriate configuration for CHPM")
               }
             },

             "HRM" = {
               if (probe_config$probe_count >= 2) {
                 # Call existing HRM function
                 calc_hrm(temp_data, probe_config$spacing, thermal_diffusivity)
               } else {
                 list(error = "Insufficient probes for HRM")
               }
             },

             # Add other methods as needed
             {
               list(error = paste("Method", method, "not implemented"))
             }
      )

    }, error = function(e) {
      list(error = paste("Error in", method, ":", e$message))
    })

    results$method_results[[method]] <- method_result
  }

  # Select best result based on quality metrics
  results$recommended_result <- select_best_result(results$method_results)

  return(results)
}

#' Select Best Result from Multiple Methods
#'
#' Choose the most reliable result when multiple methods are available.
#'
#' @param method_results list of results from different methods
#'
#' @return list with best result selection
#'
#' @keywords internal
select_best_result <- function(method_results) {

  # TODO: Implement sophisticated result selection logic
  # Consider factors like:
  # - Method reliability for given conditions
  # - Uncertainty estimates
  # - Data quality indicators
  # - Velocity range appropriateness

  # Placeholder implementation - prefer DRM if available
  if ("DRM" %in% names(method_results) &&
      !is.null(method_results$DRM$velocity_drm) &&
      !is.na(method_results$DRM$velocity_drm)) {
    return(list(
      method = "DRM",
      velocity = method_results$DRM$velocity_drm,
      uncertainty = min(method_results$DRM$uncertainty_12,
                        method_results$DRM$uncertainty_23, na.rm = TRUE),
      quality = "pending_validation"
    ))
  }

  # Fallback to first available result
  for (method in names(method_results)) {
    result <- method_results[[method]]
    if (!is.null(result) && !"error" %in% names(result)) {
      velocity_field <- grep("velocity", names(result), value = TRUE)[1]
      if (!is.null(velocity_field) && !is.na(result[[velocity_field]])) {
        return(list(
          method = method,
          velocity = result[[velocity_field]],
          uncertainty = NA,
          quality = "fallback_selection"
        ))
      }
    }
  }

  return(list(
    method = "none",
    velocity = NA,
    uncertainty = NA,
    quality = "no_valid_results"
  ))
}