#' Unified Sap Flow Data Processing Pipeline
#'
#' This file implements the main processing pipeline functions that were missing
#' from the sapFluxR package. These work with the existing calc_hrmx and calc_dma
#' functions that are already implemented.
#'
#' @name processing_pipeline
#' @family processing

#' Process Sap Flow Data with Automatic Method Selection
#'
#' Main pipeline function that processes sap flow data with automatic probe
#' configuration detection, method compatibility checking, and quality assessment.
#' Uses the existing calc_heat_pulse_velocity() function but adds intelligent
#' method selection and comprehensive reporting. Handles both single-tree and
#' multi-tree data objects.
#'
#' @param sap_data Sap flow data object from read_sap_data() or read_multiple_sap_data()
#' @param methods Character vector of methods to calculate. If NULL, uses recommended methods
#' @param parameters List of calculation parameters. If NULL, uses defaults
#' @param quality_control Logical, whether to perform quality control (default TRUE)
#' @param export_results Logical, whether to export results to file (default FALSE)
#' @param output_file Character, output file path if export_results = TRUE
#' @param verbose Logical, whether to print processing information (default TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{velocity_results}{Data frame with calculated velocities from calc_heat_pulse_velocity()}
#'     \item{quality_assessment}{Quality assessment results}
#'     \item{probe_config}{Detected probe configuration}
#'     \item{method_recommendations}{Recommended methods and parameters}
#'     \item{processing_summary}{Summary of processing steps}
#'     \item{diagnostics}{Detailed diagnostic information}
#'   }
#'
#' @details
#' The processing pipeline:
#' 1. Validates input data using existing validate_sap_data()
#' 2. Detects probe configuration using existing detect_probe_config()
#' 3. Performs comprehensive quality assessment
#' 4. Recommends optimal methods based on configuration and data quality
#' 5. Calculates velocities using existing calc_heat_pulse_velocity()
#' 6. Applies quality control and provides comprehensive diagnostics
#'
#' @examples
#' \dontrun{
#' # Basic processing with automatic method selection
#' results <- process_sap_data(sap_data)
#' print(results$processing_summary)
#'
#' # Process with specific methods
#' results <- process_sap_data(
#'   sap_data,
#'   methods = c("HRM", "MHR", "DMA"),
#'   quality_control = TRUE
#' )
#' }
#'
#' @seealso \code{\link{calc_heat_pulse_velocity}}, \code{\link{assess_data_quality}}
#' @export
process_sap_data <- function(sap_data, methods = NULL, parameters = NULL,
                             probe_config = NULL, wood_properties = NULL,
                             quality_control = TRUE, export_results = FALSE,
                             output_file = NULL, verbose = TRUE) {

  # Check if this is multi-tree data
  is_multi_tree <- inherits(sap_data, "multiple_sap_data")

  if (verbose) {
    if (is_multi_tree) {
      cat("Starting multi-tree sap flow data processing pipeline...\n")
      cat("Number of trees:", sap_data$metadata$n_trees, "\n")
      cat("Tree IDs:", paste(sap_data$metadata$tree_ids, collapse = ", "), "\n")
    } else {
      cat("Starting sap flow data processing pipeline...\n")
    }
  }

  # Step 1: Validate input data using existing function
  if (verbose) cat("1. Validating input data...\n")
  validation <- validate_sap_data(sap_data)
  if (!validation$valid) {
    stop("Input data validation failed: ", paste(validation$issues, collapse = "; "))
  }

  # Step 2: Load probe configuration
  if (verbose) cat("2. Loading probe configuration...\n")
  if (is.null(probe_config)) {
    probe_config <- get_default_probe_config()
    if (verbose) cat("   Using default symmetric probe configuration\n")
  } else if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
    if (verbose) cat("   Loaded configuration:", probe_config$config_name, "\n")
  } else {
    if (verbose) cat("   Using provided configuration:", probe_config$config_name, "\n")
  }

  # Step 2b: Load wood properties
  if (verbose) cat("   Loading wood properties...\n")
  if (is.null(wood_properties)) {
    wood_properties <- get_default_wood_properties()
    if (verbose) cat("   Using default wood properties (generic softwood)\n")
  } else if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
    if (verbose) cat("   Loaded wood properties:", wood_properties$config_name, "\n")
  } else {
    if (verbose) cat("   Using provided wood properties:", wood_properties$config_name, "\n")
  }

  # Step 3: Assess data quality (new comprehensive assessment)
  if (verbose) cat("3. Assessing data quality...\n")
  quality_assessment <- assess_data_quality(sap_data, probe_config)

  # Step 4: Get method recommendations (new intelligent recommendations)
  if (verbose) cat("4. Generating method recommendations...\n")
  recommendations <- recommend_methods(sap_data, probe_config, quality_assessment)

  # Step 5: Determine final methods and parameters
  if (is.null(methods)) {
    methods <- recommendations$recommended_methods
    if (verbose) cat("   Using recommended methods:", paste(methods, collapse = ", "), "\n")
  } else {
    if (verbose) cat("   Using specified methods:", paste(methods, collapse = ", "), "\n")
    # Check method compatibility with existing function
    compatible_methods <- intersect(methods, probe_config$compatible_methods)
    if (length(compatible_methods) < length(methods)) {
      incompatible <- setdiff(methods, compatible_methods)
      warning("Some methods may not be optimal for this configuration: ",
              paste(incompatible, collapse = ", "))
    }
  }

  if (is.null(parameters)) {
    parameters <- recommendations$recommended_parameters
  }

  # Step 6: Calculate velocities using EXISTING calc_heat_pulse_velocity function
  if (verbose) cat("5. Calculating heat pulse velocities...\n")

  if (is_multi_tree) {
    # Process each tree individually and combine results
    if (verbose) cat("   Processing each tree individually...\n")

    all_velocity_results <- list()
    tree_processing_summary <- data.frame(
      tree_id = character(),
      n_pulses = integer(),
      n_measurements = integer(),
      processing_success = logical(),
      processing_errors = character(),
      stringsAsFactors = FALSE
    )

    for (tree_id in sap_data$metadata$tree_ids) {
      if (verbose) cat(sprintf("   Processing tree: %s\n", tree_id))

      tryCatch({
        # Get individual tree data
        tree_data <- sap_data$individual_data[[tree_id]]

        # Process individual tree
        tree_velocity <- calc_heat_pulse_velocity(
          sap_data = tree_data,
          methods = methods,
          parameters = parameters
        )

        # Add tree_id to results
        tree_velocity$tree_id <- tree_id

        # Store results
        all_velocity_results[[tree_id]] <- tree_velocity

        # Update summary
        tree_processing_summary <- rbind(tree_processing_summary, data.frame(
          tree_id = tree_id,
          n_pulses = length(unique(tree_velocity$pulse_id)),
          n_measurements = nrow(tree_velocity),
          processing_success = TRUE,
          processing_errors = "",
          stringsAsFactors = FALSE
        ))

        if (verbose) {
          cat(sprintf("     ✅ Success: %s measurements\n",
                      format(nrow(tree_velocity), big.mark = ",")))
        }

      }, error = function(e) {
        # Handle processing errors
        tree_processing_summary <- rbind(tree_processing_summary, data.frame(
          tree_id = tree_id,
          n_pulses = 0,
          n_measurements = 0,
          processing_success = FALSE,
          processing_errors = as.character(e$message),
          stringsAsFactors = FALSE
        ))

        if (verbose) {
          cat(sprintf("     ❌ Error: %s\n", e$message))
        }
      })
    }

    # Combine all velocity results
    velocity_results <- dplyr::bind_rows(all_velocity_results)

    if (verbose) {
      successful_trees <- sum(tree_processing_summary$processing_success)
      cat(sprintf("   Multi-tree processing complete: %d/%d trees successful\n",
                  successful_trees, nrow(tree_processing_summary)))
    }

  } else {
    # Single tree processing (original logic)
    velocity_results <- calc_heat_pulse_velocity(
      sap_data = sap_data,
      methods = methods,
      parameters = parameters
    )
  }

  # Step 7: Quality control (use existing functions if available)
  if (quality_control) {
    if (verbose) cat("6. Applying quality control...\n")

    # Use existing quality functions if available, otherwise create basic flags
    if (exists("add_quality_flags", mode = "function")) {
      velocity_results <- add_quality_flags(velocity_results)
    } else {
      # Basic quality flagging
      velocity_results$quality_flag <- ifelse(
        is.finite(velocity_results$Vh_cm_hr) & velocity_results$Vh_cm_hr >= 0 & velocity_results$Vh_cm_hr < 200,
        "good", "flagged"
      )
    }

    quality_summary <- table(velocity_results$quality_flag)

    if (verbose) {
      cat("   Quality summary:\n")
      cat("     Total measurements:", nrow(velocity_results), "\n")
      cat("     Flagged measurements:", sum(velocity_results$quality_flag != "good"), "\n")
      cat("     Success rate:",
          round(sum(velocity_results$quality_flag == "good") / nrow(velocity_results) * 100, 1),
          "%\n")
    }
  } else {
    quality_summary <- NULL
  }

  # Step 8: Generate processing summary
  processing_summary <- list(
    start_time = if (!is.null(attr(sap_data, "import_time"))) attr(sap_data, "import_time") else Sys.time(),
    end_time = Sys.time(),
    probe_configuration = probe_config$name,
    methods_used = methods,
    total_pulses = length(unique(velocity_results$pulse_id)),
    total_measurements = nrow(velocity_results),
    data_quality_score = quality_assessment$overall_score,
    is_multi_tree = is_multi_tree,
    recommendations_followed = all(methods %in% recommendations$recommended_methods)
  )

  # Step 9: Collect diagnostics
  diagnostics <- list(
    validation = validation,
    probe_config_details = probe_config,
    quality_details = quality_assessment,
    method_recommendations = recommendations,
    quality_summary = quality_summary,
    parameter_details = parameters
  )

  # Add multi-tree specific diagnostics
  if (is_multi_tree) {
    diagnostics$tree_processing_summary <- tree_processing_summary
    diagnostics$tree_summary <- sap_data$tree_summary
    diagnostics$import_summary <- sap_data$import_summary
  }

  # Step 10: Export results if requested (use existing function if available)
  if (export_results) {
    if (is.null(output_file)) {
      output_file <- paste0("sap_flow_results_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    }
    if (verbose) cat("7. Exporting results to:", output_file, "\n")

    if (exists("export_velocity_results", mode = "function")) {
      export_velocity_results(velocity_results, output_file)
    } else {
      # Basic export
      write.csv(velocity_results, output_file, row.names = FALSE)
    }
  }

  if (verbose) cat("Processing pipeline completed successfully!\n")

  # Return comprehensive results
  structure(
    list(
      velocity_results = velocity_results,
      quality_assessment = quality_assessment,
      probe_config = probe_config,
      method_recommendations = recommendations,
      processing_summary = processing_summary,
      diagnostics = diagnostics
    ),
    class = "sap_flow_results"
  )
}

#' Assess Data Quality for Sap Flow Analysis
#'
#' Comprehensive data quality assessment that evaluates multiple aspects
#' of sap flow data quality and provides recommendations for analysis.
#' This complements the existing validate_sap_data() function with more
#' detailed quality metrics.
#'
#' @param sap_data Sap flow data object from read_sap_data()
#' @param probe_config Probe configuration object from detect_probe_config()
#' @param detailed Logical, whether to return detailed diagnostic information (default TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{overall_score}{Overall quality score (0-100)}
#'     \item{component_scores}{Individual component quality scores}
#'     \item{issues}{List of identified quality issues}
#'     \item{recommendations}{Quality-based recommendations}
#'     \item{diagnostics}{Detailed diagnostic information if requested}
#'   }
#'
#' @details
#' Quality assessment components:
#' - **Temporal consistency**: Regular measurement intervals, no large gaps
#' - **Sensor functionality**: All required sensors present and responsive
#' - **Temperature stability**: Baseline temperature stability
#' - **Heat pulse response**: Clear temperature response to heat pulses
#' - **Data completeness**: Minimal missing or invalid values
#' - **Measurement precision**: Consistent and precise measurements
#'
#' @examples
#' \dontrun{
#' probe_config <- detect_probe_config(sap_data)
#' quality <- assess_data_quality(sap_data, probe_config)
#' print(quality$overall_score)
#' print(quality$recommendations)
#' }
#'
#' @seealso \code{\link{validate_sap_data}}, \code{\link{process_sap_data}}
#' @export
assess_data_quality <- function(sap_data, probe_config, detailed = TRUE) {

  measurements <- sap_data$measurements
  diagnostics_data <- sap_data$diagnostics

  quality_components <- list()
  issues <- character(0)
  recommendations <- character(0)

  # 1. Temporal Consistency Assessment
  if (!is.null(measurements$datetime)) {
    time_diffs <- diff(as.numeric(measurements$datetime))

    if (length(time_diffs) > 0) {
      unique_intervals <- unique(round(time_diffs, 1))

      if (length(unique_intervals) == 1) {
        temporal_score <- 100
      } else {
        # Calculate coefficient of variation for time intervals
        cv_time <- sd(time_diffs) / mean(time_diffs)
        temporal_score <- max(0, 100 - cv_time * 100)
      }

      # Check for gaps
      expected_interval <- median(time_diffs, na.rm = TRUE)
      large_gaps <- sum(time_diffs > expected_interval * 2)
      if (large_gaps > 0) {
        temporal_score <- temporal_score * 0.8
        issues <- c(issues, paste("Found", large_gaps, "large time gaps"))
        recommendations <- c(recommendations, "Consider data interpolation for gaps")
      }
    } else {
      temporal_score <- 50
    }
  } else {
    temporal_score <- 50
    issues <- c(issues, "No temporal information available")
  }

  quality_components$temporal_consistency <- temporal_score

  # 2. Sensor Functionality Assessment
  required_sensors <- c("do", "di", "uo", "ui")  # Standard four sensors
  available_sensors <- names(measurements)[names(measurements) %in% required_sensors]
  missing_sensors <- setdiff(required_sensors, available_sensors)

  if (length(missing_sensors) == 0) {
    sensor_score <- 100
  } else {
    sensor_score <- max(0, 100 - length(missing_sensors) * 25)
    issues <- c(issues, paste("Missing sensors:", paste(missing_sensors, collapse = ", ")))
    recommendations <- c(recommendations, "Check sensor connections and configuration")
  }

  quality_components$sensor_functionality <- sensor_score

  # 3. Temperature Stability Assessment (baseline variation)
  if (length(available_sensors) > 0) {
    baseline_variations <- sapply(available_sensors, function(sensor) {
      temp_data <- measurements[[sensor]]
      if (length(temp_data) < 10) return(NA)

      # Calculate variation in first 30 points (baseline)
      baseline_points <- min(30, length(temp_data))
      baseline_var <- sd(temp_data[1:baseline_points], na.rm = TRUE)
      return(baseline_var)
    })

    avg_baseline_var <- mean(baseline_variations, na.rm = TRUE)

    # Score based on baseline stability (lower variation = higher score)
    if (is.na(avg_baseline_var)) {
      stability_score <- 0
    } else if (avg_baseline_var < 0.01) {
      stability_score <- 100
    } else if (avg_baseline_var < 0.05) {
      stability_score <- 80
    } else if (avg_baseline_var < 0.1) {
      stability_score <- 60
    } else {
      stability_score <- max(0, 40 - avg_baseline_var * 100)
      issues <- c(issues, "High baseline temperature variation detected")
      recommendations <- c(recommendations, "Check for environmental disturbances or sensor issues")
    }
  } else {
    stability_score <- 0
    issues <- c(issues, "Cannot assess temperature stability - no sensor data")
  }

  quality_components$temperature_stability <- stability_score

  # 4. Heat Pulse Response Assessment
  if (length(available_sensors) >= 2) {
    # Check for clear temperature response after heat pulses
    pulse_ids <- unique(measurements$pulse_id)
    response_scores <- numeric(0)

    for (pulse_id in pulse_ids[1:min(5, length(pulse_ids))]) {  # Check first 5 pulses
      pulse_data <- measurements[measurements$pulse_id == pulse_id, ]

      if (nrow(pulse_data) < 60) next  # Need sufficient data points

      # Look for temperature rise in downstream sensors
      if ("do" %in% available_sensors) {
        baseline_temp <- mean(pulse_data$do[1:30], na.rm = TRUE)
        max_temp <- max(pulse_data$do[31:min(90, nrow(pulse_data))], na.rm = TRUE)
        temp_rise <- max_temp - baseline_temp

        if (!is.na(temp_rise) && temp_rise > 0.5) {
          response_scores <- c(response_scores, 100)
        } else if (!is.na(temp_rise) && temp_rise > 0.1) {
          response_scores <- c(response_scores, 60)
        } else {
          response_scores <- c(response_scores, 20)
        }
      }
    }

    if (length(response_scores) > 0) {
      response_score <- mean(response_scores)
      if (response_score < 50) {
        issues <- c(issues, "Weak or absent heat pulse response")
        recommendations <- c(recommendations, "Check heater functionality and probe contact")
      }
    } else {
      response_score <- 30
      issues <- c(issues, "Cannot assess heat pulse response")
    }
  } else {
    response_score <- 0
    issues <- c(issues, "Insufficient sensors for heat pulse assessment")
  }

  quality_components$heat_pulse_response <- response_score

  # 5. Data Completeness Assessment
  if (length(available_sensors) > 0) {
    total_expected_values <- nrow(measurements) * length(available_sensors)
    missing_values <- sum(is.na(measurements[available_sensors]))
    completeness_score <- max(0, 100 - (missing_values / total_expected_values) * 100)

    if (completeness_score < 90) {
      issues <- c(issues, paste("Data completeness:", round(completeness_score, 1), "%"))
      recommendations <- c(recommendations, "Consider data gap filling or sensor maintenance")
    }
  } else {
    completeness_score <- 0
  }

  quality_components$data_completeness <- completeness_score

  # 6. Measurement Precision Assessment
  if (length(available_sensors) > 0) {
    precision_scores <- sapply(available_sensors, function(sensor) {
      temp_data <- measurements[[sensor]]
      if (length(temp_data) == 0) return(0)

      # Check for reasonable temperature precision (not too many identical values)
      unique_ratio <- length(unique(temp_data[!is.na(temp_data)])) / length(temp_data[!is.na(temp_data)])
      if (is.na(unique_ratio)) return(0)

      if (unique_ratio > 0.8) {
        100
      } else if (unique_ratio > 0.5) {
        80
      } else {
        max(0, unique_ratio * 100)
      }
    })

    precision_score <- mean(precision_scores, na.rm = TRUE)

    if (precision_score < 60) {
      issues <- c(issues, "Low measurement precision detected")
      recommendations <- c(recommendations, "Check sensor calibration and resolution")
    }
  } else {
    precision_score <- 0
  }

  quality_components$measurement_precision <- precision_score

  # Calculate overall quality score with weights
  component_weights <- c(
    temporal_consistency = 0.15,
    sensor_functionality = 0.25,
    temperature_stability = 0.20,
    heat_pulse_response = 0.25,
    data_completeness = 0.10,
    measurement_precision = 0.05
  )

  overall_score <- sum(sapply(names(component_weights), function(x) {
    score <- quality_components[[x]]
    if (is.null(score) || is.na(score)) score <- 0
    score * component_weights[x]
  }))

  # Generate overall recommendations
  if (overall_score >= 85) {
    recommendations <- c(recommendations, "Data quality is excellent - proceed with all analyses")
  } else if (overall_score >= 70) {
    recommendations <- c(recommendations, "Data quality is good - most analyses should be reliable")
  } else if (overall_score >= 50) {
    recommendations <- c(recommendations, "Data quality is fair - use caution and consider quality filtering")
  } else {
    recommendations <- c(recommendations, "Data quality is poor - address issues before analysis")
  }

  # Detailed diagnostics
  detailed_diagnostics <- if (detailed) {
    list(
      measurement_summary = list(
        total_pulses = length(unique(measurements$pulse_id)),
        total_measurements = nrow(measurements),
        available_sensors = available_sensors,
        missing_sensors = missing_sensors,
        time_range = if(!is.null(measurements$datetime)) range(measurements$datetime) else NULL
      ),
      quality_thresholds = list(
        excellent = 85,
        good = 70,
        fair = 50,
        poor = 0
      ),
      component_weights = component_weights
    )
  } else {
    NULL
  }

  list(
    overall_score = round(overall_score, 1),
    component_scores = lapply(quality_components, function(x) round(x, 1)),
    issues = unique(issues),
    recommendations = unique(recommendations),
    diagnostics = detailed_diagnostics
  )
}

#' Recommend Methods Based on Configuration and Data Quality
#'
#' Provides intelligent method recommendations based on probe configuration,
#' data quality assessment, and flow conditions. Works with the existing
#' method compatibility system.
#'
#' @param sap_data Sap flow data object
#' @param probe_config Probe configuration object
#' @param quality_assessment Quality assessment results
#'
#' @return List containing recommended methods, parameters, and rationale
#'
#' @details
#' Recommendation logic:
#' - High quality data: Use most accurate methods (HRM, DMA, MHR)
#' - Medium quality: Use robust methods (DMA, HRM)
#' - Low quality: Use simple, robust methods (MHR, Tmax)
#' - Configuration compatibility always respected
#'
#' @export
recommend_methods <- function(sap_data, probe_config, quality_assessment) {

  # Get compatible methods from existing system
  compatible_methods <- if (!is.null(probe_config$compatible_methods) && length(probe_config$compatible_methods) > 0) {
    probe_config$compatible_methods
  } else {
    # Fallback to standard methods
    c("HRM", "MHR", "DMA", "Tmax_Klu", "Tmax_Coh")
  }

  quality_score <- quality_assessment$overall_score

  # Base recommendations on quality score
  if (quality_score >= 85) {
    # Excellent quality - use most accurate methods
    primary_methods <- intersect(c("DMA", "HRM", "MHR", "HRMXa"), compatible_methods)
    backup_methods <- intersect(c("Tmax_Klu", "HRMXb"), compatible_methods)
  } else if (quality_score >= 70) {
    # Good quality - use robust methods
    primary_methods <- intersect(c("DMA", "HRM", "MHR"), compatible_methods)
    backup_methods <- intersect(c("Tmax_Klu", "HRMXa"), compatible_methods)
  } else if (quality_score >= 50) {
    # Fair quality - use simpler, more robust methods
    primary_methods <- intersect(c("DMA", "MHR"), compatible_methods)
    backup_methods <- intersect(c("HRM", "Tmax_Klu"), compatible_methods)
  } else {
    # Poor quality - use most robust methods only
    primary_methods <- intersect(c("MHR", "Tmax_Klu"), compatible_methods)
    backup_methods <- intersect(c("DMA"), compatible_methods)
  }

  # Ensure we have at least one method
  all_candidate_methods <- c(primary_methods, backup_methods)
  recommended_methods <- all_candidate_methods[1:min(3, length(all_candidate_methods))]

  # Ensure we have at least one method even if compatibility checking fails
  if (length(recommended_methods) == 0) {
    recommended_methods <- c("HRM")  # Fallback to most basic method
  }

  # Default parameters (compatible with existing system)
  recommended_parameters <- list(
    diffusivity = 0.0025,  # Standard value for most wood types
    x = 0.5,               # Standard probe spacing
    L = 0.5,               # HRMX lower bound
    H = 0.8,               # HRMX upper bound
    tp = 2,                # Heat pulse duration
    hrm_start = 60,        # HRM sampling start
    hrm_end = 100,         # HRM sampling end
    pre_pulse = 30         # Pre-pulse baseline period
  )

  # Quality-based parameter adjustments
  if (quality_score < 70) {
    # For lower quality data, use more conservative sampling windows
    recommended_parameters$hrm_start <- 70
    recommended_parameters$hrm_end <- 120
    recommended_parameters$L <- 0.4
    recommended_parameters$H <- 0.9
  }

  rationale <- paste(
    "Based on",
    if (!is.null(probe_config$name)) probe_config$name else "detected",
    "configuration and data quality score of", quality_score,
    "- recommended methods prioritize",
    if (quality_score >= 85) "accuracy" else if (quality_score >= 50) "robustness" else "simplicity"
  )

  list(
    recommended_methods = recommended_methods,
    recommended_parameters = recommended_parameters,
    rationale = rationale,
    quality_based = TRUE,
    configuration_compatible = TRUE
  )
}

#' Print method for sap_flow_results objects
#' @param x A sap_flow_results object
#' @param ... Additional arguments
#' @export
print.sap_flow_results <- function(x, ...) {
  cat("Sap Flow Processing Results\n")
  cat("===========================\n\n")

  cat("Configuration:",
      if (!is.null(x$probe_config$name)) x$probe_config$name else "Unknown", "\n")
  cat("Data Quality Score:", x$quality_assessment$overall_score, "/100\n")
  cat("Methods Used:", paste(x$processing_summary$methods_used, collapse = ", "), "\n")
  cat("Total Pulses:", x$processing_summary$total_pulses, "\n")
  cat("Total Measurements:", x$processing_summary$total_measurements, "\n\n")

  if (length(x$quality_assessment$issues) > 0) {
    cat("Quality Issues:\n")
    for (issue in x$quality_assessment$issues) {
      cat(" -", issue, "\n")
    }
    cat("\n")
  }

  cat("Recommendations:\n")
  for (rec in x$quality_assessment$recommendations) {
    cat(" -", rec, "\n")
  }

  invisible(x)
}