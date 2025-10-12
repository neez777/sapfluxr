#' Advanced Sensor Diagnostic Functions
#'
#' Comprehensive sensor diagnostic functions for detecting sensor issues,
#' calibration problems, and data quality issues in sap flow measurements.
#'
#' @name sensor_diagnostics
NULL

#' Diagnose Sensor Performance
#'
#' @description
#' Comprehensive sensor diagnostic analysis including drift detection,
#' calibration assessment, and sensor-specific quality metrics.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param reference_period Integer, number of initial pulses to use as reference (default: 5)
#' @param drift_threshold Numeric, maximum acceptable drift in degrees per hour (default: 0.1)
#' @param noise_window Integer, number of measurements for noise calculation (default: 50)
#' @param detailed Logical, whether to return detailed diagnostic information (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{sensor_status}{Overall status for each sensor}
#'     \item{drift_analysis}{Sensor drift rates and trends}
#'     \item{noise_analysis}{Signal-to-noise ratios and noise characteristics}
#'     \item{calibration_assessment}{Calibration quality indicators}
#'     \item{recommendations}{Sensor-specific recommendations}
#'     \item{diagnostics}{Detailed diagnostic data if requested}
#'   }
#'
#' @details
#' Performs comprehensive sensor diagnostics including:
#' - **Drift Detection**: Identifies long-term sensor drift patterns
#' - **Noise Analysis**: Calculates signal-to-noise ratios and precision metrics
#' - **Calibration Assessment**: Evaluates sensor calibration quality
#' - **Response Analysis**: Assesses sensor response characteristics
#' - **Cross-Sensor Validation**: Compares sensors for consistency
#'
#' @examples
#' \dontrun{
#' # Basic sensor diagnostics
#' sensor_diag <- diagnose_sensor_performance(sap_data)
#' print(sensor_diag$sensor_status)
#'
#' # Detailed diagnostics with custom parameters
#' detailed_diag <- diagnose_sensor_performance(
#'   sap_data,
#'   drift_threshold = 0.05,
#'   detailed = TRUE
#' )
#' }
#'
#' @seealso \code{\link{validate_sap_data}}, \code{\link{assess_data_quality}}
#' @export
diagnose_sensor_performance <- function(sap_data, reference_period = 5,
                                        drift_threshold = 0.1, noise_window = 50,
                                        detailed = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  measurements <- sap_data$measurements
  temp_sensors <- c("do", "di", "uo", "ui")
  available_sensors <- intersect(temp_sensors, names(measurements))

  if (length(available_sensors) < 2) {
    stop("Need at least 2 temperature sensors for diagnostic analysis")
  }

  # Initialize results
  sensor_status <- character(length(available_sensors))
  names(sensor_status) <- available_sensors
  drift_analysis <- list()
  noise_analysis <- list()
  calibration_assessment <- list()
  recommendations <- character(0)
  diagnostics <- list()

  # Get pulse structure
  pulse_col <- if ("pulse_id" %in% names(measurements)) "pulse_id" else "id"
  pulses <- unique(measurements[[pulse_col]])

  if (length(pulses) < reference_period) {
    warning("Insufficient pulses for comprehensive diagnostics")
    reference_period <- max(1, length(pulses) - 1)
  }

  # 1. Drift Analysis
  for (sensor in available_sensors) {

    # Calculate baseline temperatures for each pulse
    baseline_temps <- numeric(length(pulses))

    for (i in seq_along(pulses)) {
      pulse_data <- measurements[measurements[[pulse_col]] == pulses[i], ]

      if (nrow(pulse_data) >= 30) {
        # Use first 30 measurements as baseline
        baseline_temps[i] <- mean(pulse_data[[sensor]][1:30], na.rm = TRUE)
      } else {
        baseline_temps[i] <- NA
      }
    }

    # Remove NA values
    valid_temps <- baseline_temps[!is.na(baseline_temps)]
    valid_indices <- which(!is.na(baseline_temps))

    if (length(valid_temps) >= 3) {
      # Linear regression to detect drift
      time_hours <- (valid_indices - 1) * median(diff(measurements$datetime), na.rm = TRUE) / 3600
      drift_model <- lm(valid_temps ~ time_hours)
      drift_rate <- coef(drift_model)[2]  # degrees per hour

      drift_analysis[[sensor]] <- list(
        drift_rate = drift_rate,
        drift_significant = abs(drift_rate) > drift_threshold,
        r_squared = summary(drift_model)$r.squared,
        baseline_range = diff(range(valid_temps, na.rm = TRUE))
      )

      # Sensor status based on drift
      if (abs(drift_rate) > drift_threshold) {
        sensor_status[sensor] <- "DRIFT_WARNING"
        recommendations <- c(recommendations,
                             paste("Sensor", sensor, "shows significant drift:",
                                   round(drift_rate * 24, 3), "°C/day"))
      } else {
        sensor_status[sensor] <- "OK"
      }

    } else {
      drift_analysis[[sensor]] <- list(
        drift_rate = NA,
        drift_significant = NA,
        r_squared = NA,
        baseline_range = NA
      )
      sensor_status[sensor] <- "INSUFFICIENT_DATA"
    }
  }

  # 2. Noise Analysis
  for (sensor in available_sensors) {

    # Calculate noise from first pulse baseline period
    first_pulse_data <- measurements[measurements[[pulse_col]] == pulses[1], ]

    if (nrow(first_pulse_data) >= noise_window) {
      baseline_period <- first_pulse_data[[sensor]][1:noise_window]
      baseline_period <- baseline_period[!is.na(baseline_period)]

      if (length(baseline_period) >= 30) {
        noise_std <- sd(baseline_period)
        signal_mean <- mean(baseline_period)
        snr <- signal_mean / noise_std

        # Calculate measurement precision (typical noise level)
        noise_analysis[[sensor]] <- list(
          noise_std = noise_std,
          signal_to_noise = snr,
          precision = noise_std * 2,  # 95% confidence interval
          measurement_quality = ifelse(snr > 100, "EXCELLENT",
                                       ifelse(snr > 50, "GOOD",
                                              ifelse(snr > 20, "FAIR", "POOR")))
        )

        # Update sensor status if noise is excessive
        if (snr < 20) {
          sensor_status[sensor] <- "HIGH_NOISE"
          recommendations <- c(recommendations,
                               paste("Sensor", sensor, "has high noise (SNR =",
                                     round(snr, 1), ")"))
        }

      } else {
        noise_analysis[[sensor]] <- list(
          noise_std = NA, signal_to_noise = NA, precision = NA,
          measurement_quality = "UNKNOWN"
        )
      }
    } else {
      noise_analysis[[sensor]] <- list(
        noise_std = NA, signal_to_noise = NA, precision = NA,
        measurement_quality = "INSUFFICIENT_DATA"
      )
    }
  }

  # 3. Calibration Assessment
  if (length(available_sensors) >= 2) {

    # Compare sensor baselines for calibration assessment
    baseline_comparison <- matrix(NA, length(available_sensors), length(available_sensors))
    rownames(baseline_comparison) <- available_sensors
    colnames(baseline_comparison) <- available_sensors

    # Calculate average baselines for each sensor
    sensor_baselines <- sapply(available_sensors, function(sensor) {
      pulse_baselines <- sapply(pulses[1:min(5, length(pulses))], function(pulse) {
        pulse_data <- measurements[measurements[[pulse_col]] == pulse, ]
        if (nrow(pulse_data) >= 30) {
          mean(pulse_data[[sensor]][1:30], na.rm = TRUE)
        } else {
          NA
        }
      })
      mean(pulse_baselines, na.rm = TRUE)
    })

    # Calculate pairwise differences
    for (i in 1:length(available_sensors)) {
      for (j in 1:length(available_sensors)) {
        if (i != j) {
          baseline_comparison[i, j] <- sensor_baselines[i] - sensor_baselines[j]
        }
      }
    }

    calibration_assessment$baseline_differences <- baseline_comparison
    calibration_assessment$max_difference <- max(abs(baseline_comparison), na.rm = TRUE)
    calibration_assessment$calibration_quality <- ifelse(
      calibration_assessment$max_difference > 1.0, "POOR",
      ifelse(calibration_assessment$max_difference > 0.5, "FAIR", "GOOD")
    )

    # Add calibration warnings
    if (calibration_assessment$max_difference > 1.0) {
      recommendations <- c(recommendations,
                           paste("Large baseline differences detected - check sensor calibration"))
    }
  }

  # 4. Detailed diagnostics (if requested)
  if (detailed) {
    diagnostics$sensor_data_summary <- lapply(available_sensors, function(sensor) {
      sensor_data <- measurements[[sensor]]
      list(
        n_measurements = length(sensor_data),
        n_missing = sum(is.na(sensor_data)),
        range = range(sensor_data, na.rm = TRUE),
        mean = mean(sensor_data, na.rm = TRUE),
        median = median(sensor_data, na.rm = TRUE),
        std = sd(sensor_data, na.rm = TRUE)
      )
    })
    names(diagnostics$sensor_data_summary) <- available_sensors

    diagnostics$reference_period <- reference_period
    diagnostics$analysis_parameters <- list(
      drift_threshold = drift_threshold,
      noise_window = noise_window
    )
  }

  return(list(
    sensor_status = sensor_status,
    drift_analysis = drift_analysis,
    noise_analysis = noise_analysis,
    calibration_assessment = calibration_assessment,
    recommendations = recommendations,
    diagnostics = if (detailed) diagnostics else NULL
  ))
}

#' Detect Sensor Outliers
#'
#' @description
#' Advanced outlier detection for temperature sensor data using multiple
#' statistical methods and domain-specific criteria.
#'
#' @param sap_data A sap_data object
#' @param methods Character vector of detection methods: "zscore", "iqr", "isolation", "temporal"
#' @param zscore_threshold Numeric, Z-score threshold for outlier detection (default: 3)
#' @param iqr_multiplier Numeric, IQR multiplier for outlier detection (default: 1.5)
#' @param temporal_window Integer, window size for temporal outlier detection (default: 10)
#' @param flag_outliers Logical, whether to add outlier flags to data (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{outlier_summary}{Summary of outliers detected by each method}
#'     \item{outlier_indices}{Indices of detected outliers for each sensor}
#'     \item{outlier_flags}{Logical matrix indicating outliers (if flag_outliers = TRUE)}
#'     \item{method_comparison}{Comparison of methods and their agreement}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic outlier detection
#' outliers <- detect_sensor_outliers(sap_data)
#' print(outliers$outlier_summary)
#'
#' # Multiple methods with custom parameters
#' outliers <- detect_sensor_outliers(
#'   sap_data,
#'   methods = c("zscore", "iqr", "temporal"),
#'   zscore_threshold = 2.5
#' )
#' }
#'
#' @export
detect_sensor_outliers <- function(sap_data,
                                   methods = c("zscore", "iqr", "temporal"),
                                   zscore_threshold = 3,
                                   iqr_multiplier = 1.5,
                                   temporal_window = 10,
                                   flag_outliers = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  measurements <- sap_data$measurements
  temp_sensors <- c("do", "di", "uo", "ui")
  available_sensors <- intersect(temp_sensors, names(measurements))

  if (length(available_sensors) == 0) {
    stop("No temperature sensors found for outlier detection")
  }

  # Initialize results
  outlier_summary <- list()
  outlier_indices <- list()
  outlier_flags <- matrix(FALSE, nrow = nrow(measurements), ncol = length(available_sensors))
  colnames(outlier_flags) <- available_sensors
  method_comparison <- list()

  for (sensor in available_sensors) {
    sensor_data <- measurements[[sensor]]
    valid_data <- !is.na(sensor_data)
    sensor_outliers <- list()

    # Method 1: Z-score based detection
    if ("zscore" %in% methods) {
      if (sum(valid_data) >= 10) {
        z_scores <- abs(scale(sensor_data[valid_data])[, 1])
        zscore_outliers <- which(valid_data)[z_scores > zscore_threshold]
        sensor_outliers$zscore <- zscore_outliers
      } else {
        sensor_outliers$zscore <- integer(0)
      }
    }

    # Method 2: IQR based detection
    if ("iqr" %in% methods) {
      if (sum(valid_data) >= 10) {
        q1 <- quantile(sensor_data[valid_data], 0.25, na.rm = TRUE)
        q3 <- quantile(sensor_data[valid_data], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - iqr_multiplier * iqr
        upper_bound <- q3 + iqr_multiplier * iqr

        iqr_outliers <- which(sensor_data < lower_bound | sensor_data > upper_bound)
        sensor_outliers$iqr <- iqr_outliers
      } else {
        sensor_outliers$iqr <- integer(0)
      }
    }

    # Method 3: Temporal outlier detection
    if ("temporal" %in% methods) {
      temporal_outliers <- integer(0)

      if (sum(valid_data) >= temporal_window * 2) {
        # Use rolling median for temporal context
        for (i in (temporal_window + 1):(length(sensor_data) - temporal_window)) {
          if (!is.na(sensor_data[i])) {
            window_data <- sensor_data[(i - temporal_window):(i + temporal_window)]
            window_median <- median(window_data, na.rm = TRUE)
            window_mad <- mad(window_data, na.rm = TRUE)

            if (window_mad > 0) {
              deviation <- abs(sensor_data[i] - window_median) / window_mad
              if (deviation > 3) {
                temporal_outliers <- c(temporal_outliers, i)
              }
            }
          }
        }
      }
      sensor_outliers$temporal <- temporal_outliers
    }

    # Combine results for this sensor
    all_outliers <- unique(unlist(sensor_outliers))
    outlier_indices[[sensor]] <- all_outliers

    if (flag_outliers && length(all_outliers) > 0) {
      outlier_flags[all_outliers, sensor] <- TRUE
    }

    # Summary for this sensor
    outlier_summary[[sensor]] <- list(
      total_outliers = length(all_outliers),
      outlier_percentage = length(all_outliers) / nrow(measurements) * 100,
      methods_used = methods,
      outliers_by_method = lapply(sensor_outliers, length)
    )
  }

  # Method comparison analysis
  if (length(methods) > 1) {
    method_agreement <- matrix(0, length(methods), length(methods))
    dimnames(method_agreement) <- list(methods, methods)

    for (sensor in available_sensors) {
      sensor_method_outliers <- list()

      for (method in methods) {
        if (method %in% names(outlier_indices[[sensor]])) {
          # This is a bug in the original logic - let me fix it
          if ("zscore" %in% methods && method == "zscore") {
            sensor_data <- measurements[[sensor]]
            valid_data <- !is.na(sensor_data)
            if (sum(valid_data) >= 10) {
              z_scores <- abs(scale(sensor_data[valid_data])[, 1])
              sensor_method_outliers$zscore <- which(valid_data)[z_scores > zscore_threshold]
            } else {
              sensor_method_outliers$zscore <- integer(0)
            }
          }

          if ("iqr" %in% methods && method == "iqr") {
            sensor_data <- measurements[[sensor]]
            valid_data <- !is.na(sensor_data)
            if (sum(valid_data) >= 10) {
              q1 <- quantile(sensor_data[valid_data], 0.25, na.rm = TRUE)
              q3 <- quantile(sensor_data[valid_data], 0.75, na.rm = TRUE)
              iqr <- q3 - q1
              lower_bound <- q1 - iqr_multiplier * iqr
              upper_bound <- q3 + iqr_multiplier * iqr
              sensor_method_outliers$iqr <- which(sensor_data < lower_bound | sensor_data > upper_bound)
            } else {
              sensor_method_outliers$iqr <- integer(0)
            }
          }

          if ("temporal" %in% methods && method == "temporal") {
            # Use the temporal outliers calculated above
            temporal_outliers <- integer(0)
            sensor_data <- measurements[[sensor]]
            valid_data <- !is.na(sensor_data)

            if (sum(valid_data) >= temporal_window * 2) {
              for (i in (temporal_window + 1):(length(sensor_data) - temporal_window)) {
                if (!is.na(sensor_data[i])) {
                  window_data <- sensor_data[(i - temporal_window):(i + temporal_window)]
                  window_median <- median(window_data, na.rm = TRUE)
                  window_mad <- mad(window_data, na.rm = TRUE)

                  if (window_mad > 0) {
                    deviation <- abs(sensor_data[i] - window_median) / window_mad
                    if (deviation > 3) {
                      temporal_outliers <- c(temporal_outliers, i)
                    }
                  }
                }
              }
            }
            sensor_method_outliers$temporal <- temporal_outliers
          }
        }
      }

      # Calculate pairwise agreement for this sensor
      for (i in seq_along(methods)) {
        for (j in seq_along(methods)) {
          if (i != j && methods[i] %in% names(sensor_method_outliers) &&
              methods[j] %in% names(sensor_method_outliers)) {
            outliers_i <- sensor_method_outliers[[methods[i]]]
            outliers_j <- sensor_method_outliers[[methods[j]]]
            overlap <- length(intersect(outliers_i, outliers_j))
            union_size <- length(union(outliers_i, outliers_j))
            if (union_size > 0) {
              method_agreement[i, j] <- method_agreement[i, j] + overlap / union_size
            }
          }
        }
      }
    }

    # Average agreement across sensors
    method_agreement <- method_agreement / length(available_sensors)
    method_comparison$agreement_matrix <- method_agreement
  }

  return(list(
    outlier_summary = outlier_summary,
    outlier_indices = outlier_indices,
    outlier_flags = if (flag_outliers) outlier_flags else NULL,
    method_comparison = method_comparison
  ))
}

#' Validate Probe Alignment Advanced
#'
#' @description
#' Advanced probe alignment validation using multiple statistical and
#' physical criteria specific to heat pulse probe configurations.
#'
#' @param sap_data A sap_data object
#' @param config ProbeConfiguration object (optional)
#' @param alignment_tolerance Numeric, maximum acceptable temperature difference for alignment (default: 1.0)
#' @param symmetry_check Logical, whether to check upstream/downstream symmetry (default: TRUE)
#' @param depth_check Logical, whether to check inner/outer sensor depth consistency (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{alignment_status}{Overall probe alignment status}
#'     \item{baseline_analysis}{Baseline temperature analysis}
#'     \item{symmetry_analysis}{Upstream/downstream symmetry analysis}
#'     \item{depth_analysis}{Inner/outer sensor depth analysis}
#'     \item{recommendations}{Specific alignment recommendations}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic alignment check
#' alignment <- validate_probe_alignment_advanced(sap_data)
#' print(alignment$alignment_status)
#'
#' # With probe configuration
#' config <- detect_probe_config(sap_data)
#' alignment <- validate_probe_alignment_advanced(sap_data, config)
#' }
#'
#' @export
validate_probe_alignment_advanced <- function(sap_data, config = NULL,
                                              alignment_tolerance = 1.0,
                                              symmetry_check = TRUE,
                                              depth_check = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  measurements <- sap_data$measurements
  required_sensors <- c("do", "di", "uo", "ui")
  available_sensors <- intersect(required_sensors, names(measurements))

  if (length(available_sensors) < 2) {
    stop("Need at least 2 temperature sensors for alignment analysis")
  }

  # Initialize results
  alignment_status <- "UNKNOWN"
  baseline_analysis <- list()
  symmetry_analysis <- list()
  depth_analysis <- list()
  recommendations <- character(0)

  # Get baseline temperatures from multiple pulses
  pulse_col <- if ("pulse_id" %in% names(measurements)) "pulse_id" else "id"
  pulses <- unique(measurements[[pulse_col]])

  if (length(pulses) < 2) {
    warning("Need at least 2 pulses for comprehensive alignment analysis")
    return(list(
      alignment_status = "INSUFFICIENT_DATA",
      baseline_analysis = list(),
      symmetry_analysis = list(),
      depth_analysis = list(),
      recommendations = "Need more measurement pulses for analysis"
    ))
  }

  # Calculate baseline temperatures for each sensor and pulse
  baseline_matrix <- matrix(NA, length(pulses), length(available_sensors))
  colnames(baseline_matrix) <- available_sensors
  rownames(baseline_matrix) <- as.character(pulses)

  for (i in seq_along(pulses)) {
    pulse_data <- measurements[measurements[[pulse_col]] == pulses[i], ]

    if (nrow(pulse_data) >= 30) {
      for (sensor in available_sensors) {
        baseline_matrix[i, sensor] <- mean(pulse_data[[sensor]][1:30], na.rm = TRUE)
      }
    }
  }

  # Remove pulses with insufficient data
  valid_pulses <- apply(baseline_matrix, 1, function(x) sum(!is.na(x)) >= 2)
  baseline_matrix <- baseline_matrix[valid_pulses, , drop = FALSE]

  if (nrow(baseline_matrix) < 2) {
    return(list(
      alignment_status = "INSUFFICIENT_DATA",
      baseline_analysis = list(),
      symmetry_analysis = list(),
      depth_analysis = list(),
      recommendations = "Insufficient valid baseline data"
    ))
  }

  # 1. Baseline Analysis
  baseline_means <- colMeans(baseline_matrix, na.rm = TRUE)
  baseline_stds <- apply(baseline_matrix, 2, sd, na.rm = TRUE)
  baseline_range <- max(baseline_means, na.rm = TRUE) - min(baseline_means, na.rm = TRUE)

  baseline_analysis$mean_temperatures <- baseline_means
  baseline_analysis$temperature_range <- baseline_range
  baseline_analysis$temporal_stability <- baseline_stds
  baseline_analysis$alignment_quality <- ifelse(baseline_range < alignment_tolerance,
                                                "GOOD", ifelse(baseline_range < 2 * alignment_tolerance, "FAIR", "POOR"))

  # 2. Symmetry Analysis (if all four sensors available)
  if (symmetry_check && all(c("do", "di", "uo", "ui") %in% available_sensors)) {

    # Upstream vs Downstream comparison
    downstream_temps <- rowMeans(baseline_matrix[, c("do", "di"), drop = FALSE], na.rm = TRUE)
    upstream_temps <- rowMeans(baseline_matrix[, c("uo", "ui"), drop = FALSE], na.rm = TRUE)
    ud_differences <- downstream_temps - upstream_temps

    symmetry_analysis$upstream_downstream <- list(
      mean_difference = mean(ud_differences, na.rm = TRUE),
      difference_std = sd(ud_differences, na.rm = TRUE),
      symmetry_quality = ifelse(abs(mean(ud_differences, na.rm = TRUE)) < 0.5, "GOOD",
                                ifelse(abs(mean(ud_differences, na.rm = TRUE)) < 1.0, "FAIR", "POOR"))
    )

    if (abs(mean(ud_differences, na.rm = TRUE)) > 1.0) {
      recommendations <- c(recommendations,
                           "Large upstream-downstream temperature difference - check probe insertion depth")
    }
  }

  # 3. Depth Analysis (inner vs outer sensors)
  if (depth_check && all(c("do", "uo") %in% available_sensors) &&
      all(c("di", "ui") %in% available_sensors)) {

    # Outer vs Inner comparison
    outer_temps <- rowMeans(baseline_matrix[, c("do", "uo"), drop = FALSE], na.rm = TRUE)
    inner_temps <- rowMeans(baseline_matrix[, c("di", "ui"), drop = FALSE], na.rm = TRUE)
    radial_differences <- outer_temps - inner_temps

    depth_analysis$inner_outer <- list(
      mean_difference = mean(radial_differences, na.rm = TRUE),
      difference_std = sd(radial_differences, na.rm = TRUE),
      depth_quality = ifelse(abs(mean(radial_differences, na.rm = TRUE)) < 0.3, "GOOD",
                             ifelse(abs(mean(radial_differences, na.rm = TRUE)) < 0.7, "FAIR", "POOR"))
    )

    if (abs(mean(radial_differences, na.rm = TRUE)) > 0.7) {
      recommendations <- c(recommendations,
                           "Large inner-outer temperature difference - check probe installation depth")
    }
  }

  # 4. Overall Alignment Status
  quality_scores <- c()

  if (baseline_analysis$alignment_quality == "GOOD") quality_scores <- c(quality_scores, 3)
  else if (baseline_analysis$alignment_quality == "FAIR") quality_scores <- c(quality_scores, 2)
  else quality_scores <- c(quality_scores, 1)

  if (length(symmetry_analysis) > 0) {
    if (symmetry_analysis$upstream_downstream$symmetry_quality == "GOOD") quality_scores <- c(quality_scores, 3)
    else if (symmetry_analysis$upstream_downstream$symmetry_quality == "FAIR") quality_scores <- c(quality_scores, 2)
    else quality_scores <- c(quality_scores, 1)
  }

  if (length(depth_analysis) > 0) {
    if (depth_analysis$inner_outer$depth_quality == "GOOD") quality_scores <- c(quality_scores, 3)
    else if (depth_analysis$inner_outer$depth_quality == "FAIR") quality_scores <- c(quality_scores, 2)
    else quality_scores <- c(quality_scores, 1)
  }

  overall_score <- mean(quality_scores)

  if (overall_score >= 2.5) {
    alignment_status <- "GOOD"
  } else if (overall_score >= 2.0) {
    alignment_status <- "FAIR"
    recommendations <- c(recommendations, "Consider minor probe alignment adjustments")
  } else {
    alignment_status <- "POOR"
    recommendations <- c(recommendations, "Probe alignment needs significant improvement")
  }

  return(list(
    alignment_status = alignment_status,
    baseline_analysis = baseline_analysis,
    symmetry_analysis = symmetry_analysis,
    depth_analysis = depth_analysis,
    recommendations = recommendations
  ))
}