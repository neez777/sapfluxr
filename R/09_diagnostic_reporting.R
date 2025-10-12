#' Comprehensive Diagnostic Reporting Functions
#'
#' Functions for generating comprehensive diagnostic reports that combine
#' all validation, quality control, and method comparison analyses.
#'
#' @name diagnostic_reporting
NULL

#' Generate Comprehensive Diagnostic Report
#'
#' @description
#' Creates a comprehensive diagnostic report combining sensor diagnostics,
#' probe alignment validation, method comparison, and quality assessment.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param vh_results Optional, vh_results object from calc_heat_pulse_velocity()
#' @param probe_config Optional, ProbeConfiguration object
#' @param include_plots Logical, whether to include diagnostic plots (default: TRUE)
#' @param output_format Character, output format: "text", "html", "markdown" (default: "text")
#' @param output_file Character, optional file path for saving report
#' @param detailed Logical, whether to include detailed technical information (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{report_summary}{Executive summary of all diagnostics}
#'     \item{data_quality}{Data quality assessment results}
#'     \item{sensor_diagnostics}{Sensor performance diagnostics}
#'     \item{probe_alignment}{Probe alignment validation results}
#'     \item{method_comparison}{Method comparison analysis (if vh_results provided)}
#'     \item{recommendations}{Prioritized recommendations}
#'     \item{report_text}{Formatted report text}
#'   }
#'
#' @details
#' The comprehensive diagnostic report includes:
#' - **Executive Summary**: High-level assessment and key findings
#' - **Data Quality Assessment**: Completeness, consistency, and temporal quality
#' - **Sensor Diagnostics**: Drift analysis, noise assessment, calibration quality
#' - **Probe Alignment**: Alignment validation and symmetry analysis
#' - **Method Comparison**: Cross-method validation and agreement analysis
#' - **Quality Control**: Outlier detection and quality flag analysis
#' - **Recommendations**: Prioritized action items and improvements
#'
#' @examples
#' \dontrun{
#' # Basic diagnostic report
#' report <- generate_diagnostic_report(sap_data)
#' print(report$report_summary)
#'
#' # Comprehensive report with velocity results
#' vh_results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR", "DMA"))
#' report <- generate_diagnostic_report(sap_data, vh_results)
#'
#' # Save report to file
#' generate_diagnostic_report(sap_data, vh_results,
#'                           output_file = "diagnostic_report.txt")
#' }
#'
#' @seealso \code{\link{diagnose_sensor_performance}}, \code{\link{validate_probe_alignment_advanced}}, \code{\link{compare_hpv_methods}}
#' @export
generate_diagnostic_report <- function(sap_data, vh_results = NULL, probe_config = NULL,
                                       include_plots = TRUE, output_format = "text",
                                       output_file = NULL, detailed = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  # Initialize report components
  report_summary <- list()
  data_quality <- list()
  sensor_diagnostics <- list()
  probe_alignment <- list()
  method_comparison <- list()
  recommendations <- character(0)

  cat("Generating comprehensive diagnostic report...\n")

  # 1. Basic Data Quality Assessment
  cat("1. Assessing data quality...\n")

  # Use existing validate_sap_data function
  if (exists("validate_sap_data", mode = "function")) {
    validation_results <- validate_sap_data(sap_data)
    data_quality$validation <- validation_results

    if (!validation_results$valid) {
      recommendations <- c(recommendations, "CRITICAL: Data validation failed - address validation issues first")
    }
  } else {
    # Basic validation
    measurements <- sap_data$measurements
    temp_sensors <- c("do", "di", "uo", "ui")
    available_sensors <- intersect(temp_sensors, names(measurements))

    data_quality$validation <- list(
      valid = length(available_sensors) >= 2,
      n_sensors = length(available_sensors),
      n_measurements = nrow(measurements)
    )
  }

  # Calculate basic quality metrics
  measurements <- sap_data$measurements
  data_quality$completeness <- list(
    total_measurements = nrow(measurements),
    missing_data_percent = if (nrow(measurements) > 0) {
      sum(is.na(measurements)) / (nrow(measurements) * ncol(measurements)) * 100
    } else { 100 }
  )

  # 2. Sensor Performance Diagnostics
  cat("2. Analyzing sensor performance...\n")

  tryCatch({
    sensor_diagnostics <- diagnose_sensor_performance(sap_data, detailed = detailed)

    # Add sensor issues to recommendations
    if (length(sensor_diagnostics$recommendations) > 0) {
      recommendations <- c(recommendations,
                           paste("SENSOR:", sensor_diagnostics$recommendations))
    }

    # Check for critical sensor issues
    critical_sensors <- names(sensor_diagnostics$sensor_status)[
      sensor_diagnostics$sensor_status %in% c("DRIFT_WARNING", "HIGH_NOISE")
    ]

    if (length(critical_sensors) > 0) {
      recommendations <- c(recommendations,
                           paste("URGENT: Critical sensor issues detected in:",
                                 paste(critical_sensors, collapse = ", ")))
    }

  }, error = function(e) {
    sensor_diagnostics$error <- paste("Sensor diagnostics failed:", e$message)
    recommendations <- c(recommendations, "WARNING: Could not complete sensor diagnostics")
  })

  # 3. Probe Alignment Validation
  cat("3. Validating probe alignment...\n")

  tryCatch({
    probe_alignment <- validate_probe_alignment_advanced(sap_data)

    if (probe_alignment$alignment_status == "POOR") {
      recommendations <- c(recommendations,
                           "CRITICAL: Poor probe alignment detected - reinstallation may be required")
    } else if (probe_alignment$alignment_status == "FAIR") {
      recommendations <- c(recommendations,
                           "WARNING: Probe alignment could be improved")
    }

    # Add specific alignment recommendations
    if (length(probe_alignment$recommendations) > 0) {
      recommendations <- c(recommendations,
                           paste("ALIGNMENT:", probe_alignment$recommendations))
    }

  }, error = function(e) {
    probe_alignment$error <- paste("Alignment validation failed:", e$message)
    recommendations <- c(recommendations, "WARNING: Could not validate probe alignment")
  })

  # 4. Method Comparison (if velocity results provided)
  if (!is.null(vh_results)) {
    cat("4. Comparing calculation methods...\n")

    tryCatch({
      method_comparison <- compare_hpv_methods(vh_results)

      # Add method-specific recommendations
      if (length(method_comparison$recommendations) > 0) {
        recommendations <- c(recommendations,
                             paste("METHOD:", method_comparison$recommendations))
      }

      # Check for method agreement issues
      if (length(method_comparison$bias_analysis) > 0) {
        recommendations <- c(recommendations,
                             "WARNING: Systematic bias detected between methods")
      }

    }, error = function(e) {
      method_comparison$error <- paste("Method comparison failed:", e$message)
      recommendations <- c(recommendations, "WARNING: Could not complete method comparison")
    })
  }

  # 5. Outlier Detection
  cat("5. Detecting outliers...\n")

  tryCatch({
    outlier_results <- detect_sensor_outliers(sap_data)

    # Check for excessive outliers
    total_outliers <- sum(sapply(outlier_results$outlier_summary, function(x) x$total_outliers))
    total_measurements <- nrow(sap_data$measurements)
    outlier_percent <- total_outliers / total_measurements * 100

    if (outlier_percent > 10) {
      recommendations <- c(recommendations,
                           paste("WARNING: High outlier rate detected (",
                                 round(outlier_percent, 1), "%)"))
    }

    data_quality$outliers <- outlier_results$outlier_summary

  }, error = function(e) {
    recommendations <- c(recommendations, "WARNING: Could not complete outlier detection")
  })

  # 6. Generate Report Summary
  cat("6. Generating report summary...\n")

  # Overall data quality score
  quality_scores <- c()

  # Data completeness score
  completeness_score <- 100 - data_quality$completeness$missing_data_percent
  quality_scores <- c(quality_scores, completeness_score)

  # Sensor quality score
  if (length(sensor_diagnostics) > 0 && "sensor_status" %in% names(sensor_diagnostics)) {
    sensor_ok <- sum(sensor_diagnostics$sensor_status == "OK")
    sensor_total <- length(sensor_diagnostics$sensor_status)
    sensor_score <- (sensor_ok / sensor_total) * 100
    quality_scores <- c(quality_scores, sensor_score)
  }

  # Alignment quality score
  if (length(probe_alignment) > 0 && "alignment_status" %in% names(probe_alignment)) {
    alignment_score <- switch(probe_alignment$alignment_status,
                              "GOOD" = 100,
                              "FAIR" = 70,
                              "POOR" = 30,
                              50)
    quality_scores <- c(quality_scores, alignment_score)
  }

  overall_score <- if (length(quality_scores) > 0) mean(quality_scores) else 50

  report_summary <- list(
    overall_quality_score = overall_score,
    data_completeness = completeness_score,
    sensor_status = if (length(sensor_diagnostics) > 0) sensor_diagnostics$sensor_status else "Unknown",
    probe_alignment = if (length(probe_alignment) > 0) probe_alignment$alignment_status else "Unknown",
    methods_analysed = if (!is.null(vh_results)) unique(vh_results$method) else "None",
    total_recommendations = length(recommendations),
    critical_issues = sum(grepl("CRITICAL|URGENT", recommendations)),
    analysis_timestamp = Sys.time()
  )

  # 7. Format Report Text
  cat("7. Formatting report...\n")

  report_text <- format_diagnostic_report(
    report_summary, data_quality, sensor_diagnostics,
    probe_alignment, method_comparison, recommendations,
    output_format, detailed
  )

  # 8. Save to file if requested
  if (!is.null(output_file)) {
    cat("8. Saving report to file...\n")
    writeLines(report_text, output_file)
    cat("Report saved to:", output_file, "\n")
  }

  cat("Diagnostic report generation complete!\n")

  return(list(
    report_summary = report_summary,
    data_quality = data_quality,
    sensor_diagnostics = sensor_diagnostics,
    probe_alignment = probe_alignment,
    method_comparison = method_comparison,
    recommendations = recommendations,
    report_text = report_text
  ))
}

#' Format Diagnostic Report Text
#'
#' @description
#' Internal function to format diagnostic report components into readable text.
#'
#' @param report_summary List with report summary
#' @param data_quality List with data quality results
#' @param sensor_diagnostics List with sensor diagnostic results
#' @param probe_alignment List with probe alignment results
#' @param method_comparison List with method comparison results
#' @param recommendations Character vector of recommendations
#' @param output_format Character, format type
#' @param detailed Logical, include detailed information
#'
#' @return Character vector with formatted report
#' @keywords internal
format_diagnostic_report <- function(report_summary, data_quality, sensor_diagnostics,
                                     probe_alignment, method_comparison, recommendations,
                                     output_format = "text", detailed = TRUE) {

  lines <- c()

  # Header
  lines <- c(lines, "")
  lines <- c(lines, "==================================================")
  lines <- c(lines, "       SAP FLOW DATA DIAGNOSTIC REPORT")
  lines <- c(lines, "==================================================")
  lines <- c(lines, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  lines <- c(lines, "")

  # Executive Summary
  lines <- c(lines, "EXECUTIVE SUMMARY")
  lines <- c(lines, "-----------------")
  lines <- c(lines, paste("Overall Quality Score:", round(report_summary$overall_quality_score, 1), "/100"))
  lines <- c(lines, paste("Data Completeness:", round(report_summary$data_completeness, 1), "%"))
  lines <- c(lines, paste("Critical Issues:", report_summary$critical_issues))
  lines <- c(lines, paste("Total Recommendations:", report_summary$total_recommendations))
  lines <- c(lines, "")

  # Data Quality Section
  lines <- c(lines, "DATA QUALITY ASSESSMENT")
  lines <- c(lines, "-----------------------")

  if ("validation" %in% names(data_quality)) {
    validation <- data_quality$validation
    lines <- c(lines, paste("Validation Status:", ifelse(validation$valid, "PASSED", "FAILED")))

    if ("issues" %in% names(validation) && length(validation$issues) > 0) {
      lines <- c(lines, "Issues Found:")
      for (issue in validation$issues) {
        lines <- c(lines, paste("  -", issue))
      }
    }

    if ("warnings" %in% names(validation) && length(validation$warnings) > 0) {
      lines <- c(lines, "Warnings:")
      for (warning in validation$warnings) {
        lines <- c(lines, paste("  -", warning))
      }
    }
  }

  if ("completeness" %in% names(data_quality)) {
    completeness <- data_quality$completeness
    lines <- c(lines, paste("Total Measurements:", completeness$total_measurements))
    lines <- c(lines, paste("Missing Data:", round(completeness$missing_data_percent, 2), "%"))
  }

  lines <- c(lines, "")

  # Sensor Diagnostics Section
  if (length(sensor_diagnostics) > 0 && !"error" %in% names(sensor_diagnostics)) {
    lines <- c(lines, "SENSOR DIAGNOSTICS")
    lines <- c(lines, "------------------")

    if ("sensor_status" %in% names(sensor_diagnostics)) {
      lines <- c(lines, "Sensor Status:")
      for (sensor in names(sensor_diagnostics$sensor_status)) {
        status <- sensor_diagnostics$sensor_status[sensor]
        lines <- c(lines, paste("  ", sensor, ":", status))
      }
    }

    if (detailed && "drift_analysis" %in% names(sensor_diagnostics)) {
      lines <- c(lines, "")
      lines <- c(lines, "Drift Analysis:")
      for (sensor in names(sensor_diagnostics$drift_analysis)) {
        drift <- sensor_diagnostics$drift_analysis[[sensor]]
        if (!is.na(drift$drift_rate)) {
          lines <- c(lines, paste("  ", sensor, "drift rate:",
                                  round(drift$drift_rate * 24, 4), "°C/day"))
        }
      }
    }

    if (detailed && "noise_analysis" %in% names(sensor_diagnostics)) {
      lines <- c(lines, "")
      lines <- c(lines, "Noise Analysis:")
      for (sensor in names(sensor_diagnostics$noise_analysis)) {
        noise <- sensor_diagnostics$noise_analysis[[sensor]]
        if (!is.na(noise$signal_to_noise)) {
          lines <- c(lines, paste("  ", sensor, "SNR:", round(noise$signal_to_noise, 1),
                                  "Quality:", noise$measurement_quality))
        }
      }
    }

    lines <- c(lines, "")
  } else if ("error" %in% names(sensor_diagnostics)) {
    lines <- c(lines, "SENSOR DIAGNOSTICS")
    lines <- c(lines, "------------------")
    lines <- c(lines, paste("Error:", sensor_diagnostics$error))
    lines <- c(lines, "")
  }

  # Probe Alignment Section
  if (length(probe_alignment) > 0 && !"error" %in% names(probe_alignment)) {
    lines <- c(lines, "PROBE ALIGNMENT VALIDATION")
    lines <- c(lines, "--------------------------")
    lines <- c(lines, paste("Alignment Status:", probe_alignment$alignment_status))

    if ("baseline_analysis" %in% names(probe_alignment)) {
      baseline <- probe_alignment$baseline_analysis
      if ("temperature_range" %in% names(baseline)) {
        lines <- c(lines, paste("Temperature Range:", round(baseline$temperature_range, 2), "°C"))
        lines <- c(lines, paste("Alignment Quality:", baseline$alignment_quality))
      }
    }

    lines <- c(lines, "")
  } else if ("error" %in% names(probe_alignment)) {
    lines <- c(lines, "PROBE ALIGNMENT VALIDATION")
    lines <- c(lines, "--------------------------")
    lines <- c(lines, paste("Error:", probe_alignment$error))
    lines <- c(lines, "")
  }

  # Method Comparison Section
  if (length(method_comparison) > 0 && !"error" %in% names(method_comparison)) {
    lines <- c(lines, "METHOD COMPARISON ANALYSIS")
    lines <- c(lines, "--------------------------")

    if ("method_summary" %in% names(method_comparison)) {
      lines <- c(lines, "Methods Analysed:")
      for (method in names(method_comparison$method_summary)) {
        summary <- method_comparison$method_summary[[method]]
        lines <- c(lines, paste("  ", method, "- Mean:", round(summary$mean, 2),
                                "cm/hr, SD:", round(summary$std, 2),
                                "cm/hr, N:", summary$n_observations))
      }
    }

    if ("bias_analysis" %in% names(method_comparison) &&
        length(method_comparison$bias_analysis) > 0) {
      lines <- c(lines, "")
      lines <- c(lines, "Bias Analysis:")
      for (bias in method_comparison$bias_analysis) {
        lines <- c(lines, paste("  -", bias))
      }
    }

    lines <- c(lines, "")
  } else if ("error" %in% names(method_comparison)) {
    lines <- c(lines, "METHOD COMPARISON ANALYSIS")
    lines <- c(lines, "--------------------------")
    lines <- c(lines, paste("Error:", method_comparison$error))
    lines <- c(lines, "")
  }

  # Recommendations Section
  if (length(recommendations) > 0) {
    lines <- c(lines, "RECOMMENDATIONS")
    lines <- c(lines, "---------------")

    # Separate by priority
    critical_recs <- recommendations[grepl("CRITICAL|URGENT", recommendations)]
    warning_recs <- recommendations[grepl("WARNING", recommendations)]
    other_recs <- recommendations[!grepl("CRITICAL|URGENT|WARNING", recommendations)]

    if (length(critical_recs) > 0) {
      lines <- c(lines, "CRITICAL/URGENT:")
      for (rec in critical_recs) {
        lines <- c(lines, paste("  •", rec))
      }
      lines <- c(lines, "")
    }

    if (length(warning_recs) > 0) {
      lines <- c(lines, "WARNINGS:")
      for (rec in warning_recs) {
        lines <- c(lines, paste("  •", rec))
      }
      lines <- c(lines, "")
    }

    if (length(other_recs) > 0) {
      lines <- c(lines, "OTHER RECOMMENDATIONS:")
      for (rec in other_recs) {
        lines <- c(lines, paste("  •", rec))
      }
      lines <- c(lines, "")
    }
  }

  # Footer
  lines <- c(lines, "==================================================")
  lines <- c(lines, "End of Diagnostic Report")
  lines <- c(lines, "Generated by sapFluxR package")
  lines <- c(lines, "==================================================")

  return(lines)
}

#' Create Diagnostic Report Summary
#'
#' @description
#' Create a concise summary of diagnostic results for quick assessment.
#'
#' @param diagnostic_report Results from generate_diagnostic_report()
#'
#' @return Character vector with summary text
#'
#' @examples
#' \dontrun{
#' report <- generate_diagnostic_report(sap_data)
#' summary <- create_diagnostic_summary(report)
#' cat(summary, sep = "\n")
#' }
#'
#' @export
create_diagnostic_summary <- function(diagnostic_report) {

  if (!is.list(diagnostic_report) || !"report_summary" %in% names(diagnostic_report)) {
    stop("diagnostic_report must be output from generate_diagnostic_report()")
  }

  summary <- diagnostic_report$report_summary
  recommendations <- diagnostic_report$recommendations

  lines <- c()
  lines <- c(lines, "=== DIAGNOSTIC SUMMARY ===")
  lines <- c(lines, paste("Overall Quality:", round(summary$overall_quality_score, 1), "/100"))

  # Status indicators
  status_indicators <- c()

  if (summary$overall_quality_score >= 80) {
    status_indicators <- c(status_indicators, "✓ Good overall quality")
  } else if (summary$overall_quality_score >= 60) {
    status_indicators <- c(status_indicators, "⚠ Fair quality - some issues")
  } else {
    status_indicators <- c(status_indicators, "✗ Poor quality - significant issues")
  }

  if (summary$critical_issues == 0) {
    status_indicators <- c(status_indicators, "✓ No critical issues")
  } else {
    status_indicators <- c(status_indicators, paste("✗", summary$critical_issues, "critical issues"))
  }

  for (indicator in status_indicators) {
    lines <- c(lines, indicator)
  }

  # Priority recommendations
  if (length(recommendations) > 0) {
    critical_recs <- recommendations[grepl("CRITICAL|URGENT", recommendations)]
    if (length(critical_recs) > 0) {
      lines <- c(lines, "")
      lines <- c(lines, "IMMEDIATE ACTION REQUIRED:")
      for (rec in critical_recs[1:min(3, length(critical_recs))]) {
        lines <- c(lines, paste("•", gsub("CRITICAL:|URGENT:", "", rec)))
      }
    }
  }

  lines <- c(lines, paste("Full report contains", summary$total_recommendations, "recommendations."))
  lines <- c(lines, "========================")

  return(lines)
}# R/report_generation_components.R
# Chat 9: Report Generation Components
# sapFluxR Package Development

#' Report Generation Components
#'
#' @description
#' Functions for generating report components including plots, tables, content,
#' and templates for the comprehensive reporting system.
#'
#' @name report_generation_components
#' @keywords internal
NULL

# =============================================================================
# REPORT CONTENT GENERATION
# =============================================================================

#' Generate Report Components
#'
#' @description Internal function to generate all report components based on type
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @param report_type Character string: "executive", "technical", "full"
#' @param include_plots Logical
#' @param include_raw_data Logical
#' @return List with report components
#' @keywords internal
generate_report_components <- function(sap_data, vh_results, report_type,
                                       include_plots, include_raw_data) {

  components <- list()

  # Executive summary (all report types)
  components$executive_summary <- generate_executive_summary(sap_data, vh_results)

  # Data overview
  components$data_overview <- generate_data_overview(sap_data, vh_results)

  # Method analysis
  if (report_type %in% c("technical", "full")) {
    components$method_analysis <- generate_method_analysis(vh_results)
  }

  # Quality assessment
  components$quality_assessment <- generate_quality_assessment(vh_results)

  # Statistical summary
  components$statistical_summary <- generate_statistical_summary(vh_results)

  # Temporal analysis
  if (report_type == "full") {
    components$temporal_analysis <- generate_temporal_analysis(vh_results)
  }

  # Recommendations
  components$recommendations <- generate_recommendations(sap_data, vh_results)

  # Technical details (technical and full reports)
  if (report_type %in% c("technical", "full")) {
    components$technical_details <- generate_technical_details(sap_data, vh_results)
  }

  # Raw data tables (if requested)
  if (include_raw_data) {
    components$raw_data_tables <- generate_raw_data_tables(sap_data, vh_results)
  }

  return(components)
}

#' Generate Executive Summary
#'
#' @description Internal function to generate executive summary section
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @return Character vector with summary text
#' @keywords internal
generate_executive_summary <- function(sap_data, vh_results) {

  # Calculate key metrics
  total_measurements <- nrow(vh_results)
  methods_used <- unique(vh_results$method)
  date_range <- range(vh_results$datetime, na.rm = TRUE)
  mean_velocity <- round(mean(vh_results$Vh_cm_hr, na.rm = TRUE), 2)

  # Quality assessment
  if ("quality_flag" %in% names(vh_results)) {
    quality_summary <- table(vh_results$quality_flag)
    good_data_percent <- round(quality_summary["OK"] / sum(quality_summary) * 100, 1)
  } else {
    good_data_percent <- "Not assessed"
  }

  summary_text <- c(
    "## Executive Summary",
    "",
    paste("This report presents the analysis of sap flow velocity data collected between",
          format(date_range[1], "%Y-%m-%d"), "and", format(date_range[2], "%Y-%m-%d"), "."),
    "",
    "### Key Findings",
    "",
    paste("- **Total Measurements:** ", total_measurements, "velocity calculations"),
    paste("- **Methods Applied:** ", paste(methods_used, collapse = ", ")),
    paste("- **Mean Velocity:** ", mean_velocity, "cm/hr"),
    paste("- **Data Quality:** ", good_data_percent, "% of measurements flagged as good quality"),
    "",
    "### Data Quality Assessment",
    "",
    if (is.numeric(good_data_percent) && good_data_percent >= 90) {
      "The dataset shows **excellent quality** with minimal issues detected."
    } else if (is.numeric(good_data_percent) && good_data_percent >= 70) {
      "The dataset shows **good quality** with some minor issues that should be investigated."
    } else if (is.numeric(good_data_percent)) {
      "The dataset shows **quality concerns** that require attention before analysis."
    } else {
      "Data quality assessment was not performed."
    },
    "",
    "### Recommendations",
    "",
    if (length(methods_used) > 1) {
      "Multiple calculation methods were applied, allowing for cross-validation of results."
    } else {
      "Consider applying additional calculation methods for result validation."
    }
  )

  return(summary_text)
}

#' Generate Data Overview
#'
#' @description Internal function to generate data overview section
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @return Character vector with overview text
#' @keywords internal
generate_data_overview <- function(sap_data, vh_results) {

  # Extract metadata
  metadata <- sap_data$metadata

  overview_text <- c(
    "## Data Overview",
    "",
    "### Dataset Information",
    "",
    paste("- **Original File:** ", metadata$file_path %||% "Not specified"),
    paste("- **Data Format:** ", metadata$format %||% "Not specified"),
    paste("- **Import Date:** ", metadata$import_time %||% "Not specified"),
    paste("- **File Size:** ",
          if (!is.null(metadata$file_size)) {
            paste(round(metadata$file_size / 1024^2, 2), "MB")
          } else {
            "Not specified"
          }),
    "",
    "### Measurement Summary",
    "",
    paste("- **Raw Measurements:** ", nrow(sap_data$measurements)),
    paste("- **Velocity Calculations:** ", nrow(vh_results)),
    paste("- **Heat Pulses:** ", length(unique(sap_data$measurements$pulse_id))),
    paste("- **Measurement Period:** ",
          round(as.numeric(diff(range(vh_results$datetime)), units = "days"), 1), "days"),
    "",
    "### Sensor Configuration",
    "",
    if (length(unique(vh_results$sensor_position)) > 1) {
      paste("- **Sensor Positions:** ", paste(unique(vh_results$sensor_position), collapse = ", "))
    } else {
      paste("- **Sensor Position:** ", unique(vh_results$sensor_position)[1])
    },
    paste("- **Temperature Sensors:** ",
          paste(intersect(c("do", "di", "uo", "ui"), names(sap_data$measurements)),
                collapse = ", "))
  )

  return(overview_text)
}

#' Generate Method Analysis
#'
#' @description Internal function to generate method analysis section
#' @param vh_results A vh_results object
#' @return Character vector with method analysis text
#' @keywords internal
generate_method_analysis <- function(vh_results) {

  methods <- unique(vh_results$method)

  if (length(methods) == 1) {
    return(c(
      "## Method Analysis",
      "",
      paste("Only one calculation method (", methods[1], ") was applied to this dataset."),
      "Consider applying additional methods for validation and comparison."
    ))
  }

  # Calculate method-specific statistics
  method_stats <- data.frame()
  for (method in methods) {
    method_data <- vh_results[vh_results$method == method, "Vh_cm_hr"]
    method_data <- method_data[is.finite(method_data)]

    if (length(method_data) > 0) {
      stats <- data.frame(
        method = method,
        n = length(method_data),
        mean = round(mean(method_data), 2),
        median = round(median(method_data), 2),
        sd = round(sd(method_data), 2),
        min = round(min(method_data), 2),
        max = round(max(method_data), 2)
      )
      method_stats <- rbind(method_stats, stats)
    }
  }

  analysis_text <- c(
    "## Method Analysis",
    "",
    paste("This analysis applied", length(methods), "calculation methods:"),
    paste("-", methods, collapse = "\n- "),
    "",
    "### Method Comparison",
    "",
    "Statistical comparison of methods:"
  )

  # Add method statistics table
  for (i in 1:nrow(method_stats)) {
    stats <- method_stats[i, ]
    analysis_text <- c(
      analysis_text,
      "",
      paste("**", stats$method, ":**"),
      paste("- Measurements:", stats$n),
      paste("- Mean velocity:", stats$mean, "cm/hr"),
      paste("- Median velocity:", stats$median, "cm/hr"),
      paste("- Standard deviation:", stats$sd, "cm/hr"),
      paste("- Range:", stats$min, "to", stats$max, "cm/hr")
    )
  }

  # Method recommendations
  if (nrow(method_stats) > 1) {
    highest_mean <- method_stats[which.max(method_stats$mean), "method"]
    lowest_variability <- method_stats[which.min(method_stats$sd), "method"]

    analysis_text <- c(
      analysis_text,
      "",
      "### Method Performance Summary",
      "",
      paste("- **Highest mean velocity:** ", highest_mean),
      paste("- **Lowest variability:** ", lowest_variability),
      "",
      "**Note:** Method selection should consider measurement conditions, ",
      "flow rates, and sensor configuration. Consult the literature for ",
      "method-specific applicability ranges."
    )
  }

  return(analysis_text)
}

#' Generate Quality Assessment
#'
#' @description Internal function to generate quality assessment section
#' @param vh_results A vh_results object
#' @return Character vector with quality assessment text
#' @keywords internal
generate_quality_assessment <- function(vh_results) {

  assessment_text <- c(
    "## Data Quality Assessment",
    ""
  )

  if (!"quality_flag" %in% names(vh_results)) {
    return(c(
      assessment_text,
      "Quality flags were not generated for this dataset.",
      "Consider running quality control analysis for comprehensive assessment."
    ))
  }

  # Quality flag summary
  quality_summary <- table(vh_results$quality_flag, useNA = "ifany")
  total_measurements <- nrow(vh_results)

  assessment_text <- c(
    assessment_text,
    "### Quality Flag Distribution",
    ""
  )

  for (flag in names(quality_summary)) {
    count <- quality_summary[flag]
    percentage <- round(count / total_measurements * 100, 1)

    flag_description <- switch(
      flag,
      "OK" = "Normal measurements",
      "HIGH_VELOCITY" = "Unusually high velocity values",
      "NEGATIVE_FLOW" = "Reverse flow detected",
      "INFINITE" = "Mathematical calculation issues",
      "MISSING" = "Missing or invalid data",
      "Unknown flag type"
    )

    assessment_text <- c(
      assessment_text,
      paste("- **", flag, ":** ", count, " measurements (", percentage, "%) - ", flag_description)
    )
  }

  # Overall quality assessment
  good_quality_percent <- if ("OK" %in% names(quality_summary)) {
    round(quality_summary["OK"] / total_measurements * 100, 1)
  } else {
    0
  }

  assessment_text <- c(
    assessment_text,
    "",
    "### Overall Quality Rating",
    "",
    if (good_quality_percent >= 95) {
      "**Excellent:** > 95% of measurements pass quality checks."
    } else if (good_quality_percent >= 85) {
      "**Good:** 85-95% of measurements pass quality checks."
    } else if (good_quality_percent >= 70) {
      "**Fair:** 70-85% of measurements pass quality checks. Some data issues present."
    } else {
      "**Poor:** < 70% of measurements pass quality checks. Significant data quality issues detected."
    },
    "",
    if (good_quality_percent < 85) {
      c(
        "### Quality Improvement Recommendations",
        "",
        "- Review sensor installation and alignment",
        "- Check for environmental interference",
        "- Verify calculation parameters",
        "- Consider sensor recalibration"
      )
    } else {
      "The dataset meets quality standards for analysis."
    }
  )

  return(assessment_text)
}

#' Generate Statistical Summary
#'
#' @description Internal function to generate statistical summary section
#' @param vh_results A vh_results object
#' @return Character vector with statistical summary text
#' @keywords internal
generate_statistical_summary <- function(vh_results) {

  # Calculate overall statistics
  velocity_data <- vh_results$Vh_cm_hr[is.finite(vh_results$Vh_cm_hr)]

  if (length(velocity_data) == 0) {
    return(c(
      "## Statistical Summary",
      "",
      "No valid velocity measurements available for statistical analysis."
    ))
  }

  # Descriptive statistics
  stats <- list(
    n = length(velocity_data),
    mean = round(mean(velocity_data), 2),
    median = round(median(velocity_data), 2),
    sd = round(sd(velocity_data), 2),
    min = round(min(velocity_data), 2),
    max = round(max(velocity_data), 2),
    q25 = round(quantile(velocity_data, 0.25), 2),
    q75 = round(quantile(velocity_data, 0.75), 2),
    iqr = round(IQR(velocity_data), 2)
  )

  summary_text <- c(
    "## Statistical Summary",
    "",
    "### Descriptive Statistics (All Methods Combined)",
    "",
    paste("- **Sample Size:** ", stats$n, "measurements"),
    paste("- **Mean Velocity:** ", stats$mean, "cm/hr"),
    paste("- **Median Velocity:** ", stats$median, "cm/hr"),
    paste("- **Standard Deviation:** ", stats$sd, "cm/hr"),
    paste("- **Minimum Velocity:** ", stats$min, "cm/hr"),
    paste("- **Maximum Velocity:** ", stats$max, "cm/hr"),
    paste("- **25th Percentile:** ", stats$q25, "cm/hr"),
    paste("- **75th Percentile:** ", stats$q75, "cm/hr"),
    paste("- **Interquartile Range:** ", stats$iqr, "cm/hr"),
    "",
    "### Data Distribution Characteristics",
    ""
  )

  # Distribution characteristics
  skewness <- (stats$mean - stats$median) / stats$sd
  cv <- stats$sd / stats$mean * 100

  if (abs(skewness) < 0.1) {
    distribution_desc <- "approximately symmetric"
  } else if (skewness > 0.1) {
    distribution_desc <- "right-skewed (tail extends toward higher values)"
  } else {
    distribution_desc <- "left-skewed (tail extends toward lower values)"
  }

  summary_text <- c(
    summary_text,
    paste("- **Distribution Shape:** ", distribution_desc),
    paste("- **Coefficient of Variation:** ", round(cv, 1), "%"),
    "",
    if (cv < 20) {
      "Low variability in the data."
    } else if (cv < 50) {
      "Moderate variability in the data."
    } else {
      "High variability in the data - consider investigating sources of variation."
    }
  )

  # Velocity ranges interpretation
  summary_text <- c(
    summary_text,
    "",
    "### Velocity Range Interpretation",
    "",
    if (stats$max < 5) {
      "Low flow conditions predominate (< 5 cm/hr)."
    } else if (stats$max < 20) {
      "Moderate flow conditions (5-20 cm/hr range)."
    } else {
      "High flow conditions observed (> 20 cm/hr)."
    },
    "",
    if (stats$min < 0) {
      "**Note:** Negative velocities detected, indicating reverse flow periods."
    } else {
      "No reverse flow detected in this dataset."
    }
  )

  return(summary_text)
}

#' Generate Temporal Analysis
#'
#' @description Internal function to generate temporal analysis section
#' @param vh_results A vh_results object
#' @return Character vector with temporal analysis text
#' @keywords internal
generate_temporal_analysis <- function(vh_results) {

  # Calculate temporal patterns
  vh_results$date <- as.Date(vh_results$datetime)
  vh_results$hour <- as.numeric(format(vh_results$datetime, "%H"))

  # Daily averages
  daily_stats <- aggregate(
    Vh_cm_hr ~ date,
    data = vh_results[is.finite(vh_results$Vh_cm_hr), ],
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
  )

  # Hourly patterns
  hourly_stats <- aggregate(
    Vh_cm_hr ~ hour,
    data = vh_results[is.finite(vh_results$Vh_cm_hr), ],
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
  )

  temporal_text <- c(
    "## Temporal Analysis",
    "",
    "### Time Series Overview",
    "",
    paste("- **Analysis Period:** ", nrow(daily_stats), "days"),
    paste("- **Daily Measurements:** ",
          round(mean(daily_stats$Vh_cm_hr[, "n"]), 1), "average per day"),
    "",
    "### Daily Patterns",
    ""
  )

  # Daily variability assessment
  daily_means <- daily_stats$Vh_cm_hr[, "mean"]
  if (length(daily_means) > 1) {
    daily_cv <- sd(daily_means, na.rm = TRUE) / mean(daily_means, na.rm = TRUE) * 100

    temporal_text <- c(
      temporal_text,
      paste("- **Day-to-day variability:** ", round(daily_cv, 1), "%"),
      "",
      if (daily_cv < 25) {
        "Consistent daily patterns with low day-to-day variation."
      } else if (daily_cv < 50) {
        "Moderate day-to-day variation - may reflect environmental conditions."
      } else {
        "High day-to-day variation - investigate environmental or measurement factors."
      }
    )
  }

  # Hourly patterns
  if (nrow(hourly_stats) > 12) {  # Only analyse if sufficient hourly data
    peak_hour <- hourly_stats$hour[which.max(hourly_stats$Vh_cm_hr[, "mean"])]
    min_hour <- hourly_stats$hour[which.min(hourly_stats$Vh_cm_hr[, "mean"])]

    temporal_text <- c(
      temporal_text,
      "",
      "### Diurnal Patterns",
      "",
      paste("- **Peak velocity hour:** ", sprintf("%02d:00", peak_hour)),
      paste("- **Minimum velocity hour:** ", sprintf("%02d:00", min_hour)),
      "",
      if (peak_hour >= 10 && peak_hour <= 16) {
        "Peak flows occur during typical transpiration hours (10:00-16:00)."
      } else {
        "Peak flows occur outside typical transpiration hours - investigate environmental factors."
      }
    )
  }

  return(temporal_text)
}

#' Generate Recommendations
#'
#' @description Internal function to generate recommendations section
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @return Character vector with recommendations text
#' @keywords internal
generate_recommendations <- function(sap_data, vh_results) {

  recommendations <- c(
    "## Recommendations",
    ""
  )

  # Method-specific recommendations
  methods_used <- unique(vh_results$method)

  if (length(methods_used) == 1) {
    recommendations <- c(
      recommendations,
      "### Method Selection",
      "",
      "- Consider applying additional calculation methods (HRM, MHR, DMA, T-max) for result validation",
      "- Cross-method comparison can help identify optimal methods for your conditions"
    )
  }

  # Quality-based recommendations
  if ("quality_flag" %in% names(vh_results)) {
    quality_summary <- table(vh_results$quality_flag)
    good_percent <- if ("OK" %in% names(quality_summary)) {
      quality_summary["OK"] / sum(quality_summary) * 100
    } else {
      0
    }

    if (good_percent < 85) {
      recommendations <- c(
        recommendations,
        "",
        "### Data Quality Improvements",
        "",
        "- Review sensor installation and probe alignment",
        "- Check for environmental interference (electromagnetic, thermal)",
        "- Verify probe spacing measurements and configuration",
        "- Consider sensor recalibration or replacement if issues persist"
      )
    }
  }

  # Flow range recommendations
  velocity_data <- vh_results$Vh_cm_hr[is.finite(vh_results$Vh_cm_hr)]
  if (length(velocity_data) > 0) {
    max_vel <- max(velocity_data)
    min_vel <- min(velocity_data)

    if (max_vel > 100) {
      recommendations <- c(
        recommendations,
        "",
        "### High Flow Conditions",
        "",
        "- Very high velocities detected (> 100 cm/hr)",
        "- Verify sensor installation and probe spacing",
        "- Consider T-max methods for high flow conditions",
        "- Check for sensor saturation effects"
      )
    }

    if (min_vel < -10) {
      recommendations <- c(
        recommendations,
        "",
        "### Reverse Flow Conditions",
        "",
        "- Significant reverse flow detected",
        "- Normal for nighttime conditions in some species",
        "- HRM method recommended for accurate reverse flow measurement",
        "- Consider environmental factors (wind, soil moisture)"
      )
    }
  }

  # General best practices
  recommendations <- c(
    recommendations,
    "",
    "### Best Practices",
    "",
    "- Regular sensor maintenance and calibration checks",
    "- Environmental monitoring (temperature, humidity, wind)",
    "- Documentation of installation parameters and site conditions",
    "- Backup data storage and processing procedures",
    "- Periodic validation with independent measurement methods"
  )

  return(recommendations)
}

#' Generate Technical Details
#'
#' @description Internal function to generate technical details section
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @return Character vector with technical details text
#' @keywords internal
generate_technical_details <- function(sap_data, vh_results) {

  technical_text <- c(
    "## Technical Details",
    "",
    "### Processing Parameters",
    ""
  )

  # Add processing information
  metadata <- sap_data$metadata

  if (!is.null(metadata)) {
    technical_text <- c(
      technical_text,
      paste("- **sapFluxR Version:** ", metadata$export_version %||% "Unknown"),
      paste("- **Processing Date:** ", Sys.time()),
      paste("- **Original Data Format:** ", metadata$format %||% "Unknown")
    )
  }

  # Sensor configuration details
  sensor_cols <- intersect(c("do", "di", "uo", "ui"), names(sap_data$measurements))

  technical_text <- c(
    technical_text,
    "",
    "### Sensor Configuration",
    "",
    paste("- **Temperature Sensors:** ", paste(sensor_cols, collapse = ", ")),
    paste("- **Sensor Positions:** ", paste(unique(vh_results$sensor_position), collapse = ", ")),
    paste("- **Heat Pulses Processed:** ", length(unique(vh_results$pulse_id)))
  )

  # Method-specific technical details
  for (method in unique(vh_results$method)) {
    method_data <- vh_results[vh_results$method == method, ]

    technical_text <- c(
      technical_text,
      "",
      paste("### ", method, "Method Details"),
      "",
      paste("- **Calculations:** ", nrow(method_data)),
      paste("- **Success Rate:** ",
            round(sum(is.finite(method_data$Vh_cm_hr)) / nrow(method_data) * 100, 1), "%")
    )

    # Add method-specific technical information
    method_info <- switch(
      method,
      "HRM" = c(
        "- **Principle:** Heat ratio between upstream and downstream sensors",
        "- **Optimal Range:** Low to moderate flows (-10 to 45 cm/hr)",
        "- **Advantages:** Accurate for reverse flows, less sensitive to probe spacing errors"
      ),
      "MHR" = c(
        "- **Principle:** Maximum heat ratio from temperature curves",
        "- **Optimal Range:** Low to moderate flows (0 to 30 cm/hr)",
        "- **Advantages:** Reduced noise sensitivity, good for low flows"
      ),
      "DMA" = c(
        "- **Principle:** Dual method approach with automatic switching",
        "- **Optimal Range:** Full range with method optimization",
        "- **Advantages:** Adaptive method selection based on flow conditions"
      ),
      c("- **Details:** Method-specific information not available")
    )

    technical_text <- c(technical_text, method_info)
  }

  return(technical_text)
}

#' Generate Raw Data Tables
#'
#' @description Internal function to generate raw data tables section
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @return Character vector with raw data tables text
#' @keywords internal
generate_raw_data_tables <- function(sap_data, vh_results) {

  tables_text <- c(
    "## Raw Data Tables",
    "",
    "*Note: This section contains tabular data that would be formatted appropriately in the final report output.*",
    "",
    "### Velocity Results Sample",
    ""
  )

  # Sample of velocity results (first 10 rows)
  if (nrow(vh_results) > 0) {
    sample_size <- min(10, nrow(vh_results))
    sample_data <- head(vh_results, sample_size)

    tables_text <- c(
      tables_text,
      paste("First", sample_size, "velocity measurements:"),
      "",
      "*[Tabular data would be formatted here]*"
    )
  }

  return(tables_text)
}

# =============================================================================
# PLOT GENERATION FUNCTIONS
# =============================================================================

#' Generate Report Plots
#'
#' @description Internal function to generate all plots for report
#' @param vh_results A vh_results object
#' @param figures_dir Directory to save figures
#' @return List with figure file paths
#' @keywords internal
generate_report_plots <- function(vh_results, figures_dir) {

  figures_created <- list()

  tryCatch({
    # Time series plot
    ts_plot_path <- file.path(figures_dir, "velocity_timeseries.png")
    create_timeseries_plot(vh_results, ts_plot_path)
    figures_created[["timeseries"]] <- ts_plot_path

    # Method comparison plot
    if (length(unique(vh_results$method)) > 1) {
      comp_plot_path <- file.path(figures_dir, "method_comparison.png")
      create_method_comparison_plot(vh_results, comp_plot_path)
      figures_created[["method_comparison"]] <- comp_plot_path
    }

    # Distribution histogram
    hist_plot_path <- file.path(figures_dir, "velocity_histogram.png")
    create_histogram_plot(vh_results, hist_plot_path)
    figures_created[["histogram"]] <- hist_plot_path

    # Quality flags plot
    if ("quality_flag" %in% names(vh_results)) {
      quality_plot_path <- file.path(figures_dir, "quality_flags.png")
      create_quality_plot(vh_results, quality_plot_path)
      figures_created[["quality_flags"]] <- quality_plot_path
    }

  }, error = function(e) {
    warning("Error generating plots: ", e$message)
  })

  return(figures_created)
}

#' Create Time Series Plot
#'
#' @description Internal function to create time series plot
#' @param vh_results A vh_results object
#' @param file_path Output file path
#' @keywords internal
create_timeseries_plot <- function(vh_results, file_path) {

  # Use base R plotting to avoid ggplot2 dependency
  png(file_path, width = 800, height = 600, res = 100)

  # Filter finite values
  valid_data <- vh_results[is.finite(vh_results$Vh_cm_hr), ]

  if (nrow(valid_data) == 0) {
    plot.new()
    text(0.5, 0.5, "No valid data for plotting", cex = 1.2, adj = 0.5)
    dev.off()
    return()
  }

  # Create plot
  plot(valid_data$datetime, valid_data$Vh_cm_hr,
       type = "p", pch = 16, cex = 0.5,
       xlab = "Date/Time", ylab = "Velocity (cm/hr)",
       main = "Sap Flow Velocity Time Series")

  # Add trend line if enough data
  if (nrow(valid_data) > 10) {
    trend_line <- smooth.spline(as.numeric(valid_data$datetime), valid_data$Vh_cm_hr,
                                spar = 0.6)
    lines(as.POSIXct(trend_line$x, origin = "1970-01-01"), trend_line$y,
          col = "red", lwd = 2)
  }

  dev.off()
}

#' Create Method Comparison Plot
#'
#' @description Internal function to create method comparison plot
#' @param vh_results A vh_results object
#' @param file_path Output file path
#' @keywords internal
create_method_comparison_plot <- function(vh_results, file_path) {

  png(file_path, width = 800, height = 600, res = 100)

  # Filter valid data
  valid_data <- vh_results[is.finite(vh_results$Vh_cm_hr), ]

  if (nrow(valid_data) == 0 || length(unique(valid_data$method)) < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient data for method comparison", cex = 1.2, adj = 0.5)
    dev.off()
    return()
  }

  # Create boxplot
  boxplot(Vh_cm_hr ~ method, data = valid_data,
          xlab = "Method", ylab = "Velocity (cm/hr)",
          main = "Method Comparison",
          col = rainbow(length(unique(valid_data$method))))

  dev.off()
}

#' Create Histogram Plot
#'
#' @description Internal function to create histogram plot
#' @param vh_results A vh_results object
#' @param file_path Output file path
#' @keywords internal
create_histogram_plot <- function(vh_results, file_path) {

  png(file_path, width = 800, height = 600, res = 100)

  # Filter valid data
  valid_velocities <- vh_results$Vh_cm_hr[is.finite(vh_results$Vh_cm_hr)]

  if (length(valid_velocities) == 0) {
    plot.new()
    text(0.5, 0.5, "No valid data for histogram", cex = 1.2, adj = 0.5)
    dev.off()
    return()
  }

  # Create histogram
  hist(valid_velocities,
       breaks = min(30, max(5, length(valid_velocities) / 10)),
       xlab = "Velocity (cm/hr)", ylab = "Frequency",
       main = "Velocity Distribution",
       col = "lightblue", border = "black")

  # Add vertical lines for mean and median
  abline(v = mean(valid_velocities), col = "red", lwd = 2, lty = 1)
  abline(v = median(valid_velocities), col = "blue", lwd = 2, lty = 2)

  # Add legend
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = c(1, 2), lwd = 2)

  dev.off()
}

#' Create Quality Plot
#'
#' @description Internal function to create quality flags plot
#' @param vh_results A vh_results object
#' @param file_path Output file path
#' @keywords internal
create_quality_plot <- function(vh_results, file_path) {

  png(file_path, width = 800, height = 600, res = 100)

  if (!"quality_flag" %in% names(vh_results)) {
    plot.new()
    text(0.5, 0.5, "No quality flags available", cex = 1.2, adj = 0.5)
    dev.off()
    return()
  }

  # Create quality flags summary
  quality_counts <- table(vh_results$quality_flag, useNA = "ifany")

  # Create pie chart
  pie(quality_counts,
      main = "Data Quality Flag Distribution",
      col = rainbow(length(quality_counts)))

  dev.off()
}

# =============================================================================
# TABLE GENERATION FUNCTIONS
# =============================================================================

#' Generate Report Tables
#'
#' @description Internal function to generate all tables for report
#' @param sap_data A sap_data object
#' @param vh_results A vh_results object
#' @param report_type Character string
#' @return List with table data
#' @keywords internal
generate_report_tables <- function(sap_data, vh_results, report_type) {

  tables <- list()

  # Method summary table
  tables$method_summary <- create_method_summary_table(vh_results)

  # Quality summary table
  if ("quality_flag" %in% names(vh_results)) {
    tables$quality_summary <- create_quality_summary_table(vh_results)
  }

  # Daily statistics table (for full reports)
  if (report_type == "full") {
    tables$daily_statistics <- create_daily_statistics_table(vh_results)
  }

  return(tables)
}

#' Create Method Summary Table
#'
#' @description Internal function to create method summary table
#' @param vh_results A vh_results object
#' @return Data frame with method summary
#' @keywords internal
create_method_summary_table <- function(vh_results) {

  methods <- unique(vh_results$method)
  summary_table <- data.frame()

  for (method in methods) {
    method_data <- vh_results[vh_results$method == method, "Vh_cm_hr"]
    method_data <- method_data[is.finite(method_data)]

    if (length(method_data) > 0) {
      method_summary <- data.frame(
        Method = method,
        N = length(method_data),
        Mean = round(mean(method_data), 2),
        Median = round(median(method_data), 2),
        SD = round(sd(method_data), 2),
        Min = round(min(method_data), 2),
        Max = round(max(method_data), 2),
        stringsAsFactors = FALSE
      )
      summary_table <- rbind(summary_table, method_summary)
    }
  }

  return(summary_table)
}

#' Create Quality Summary Table
#'
#' @description Internal function to create quality summary table
#' @param vh_results A vh_results object
#' @return Data frame with quality summary
#' @keywords internal
create_quality_summary_table <- function(vh_results) {

  quality_summary <- table(vh_results$quality_flag, useNA = "ifany")
  total <- sum(quality_summary)

  quality_table <- data.frame(
    Quality_Flag = names(quality_summary),
    Count = as.numeric(quality_summary),
    Percentage = round(as.numeric(quality_summary) / total * 100, 1),
    stringsAsFactors = FALSE
  )

  return(quality_table)
}

#' Create Daily Statistics Table
#'
#' @description Internal function to create daily statistics table
#' @param vh_results A vh_results object
#' @return Data frame with daily statistics
#' @keywords internal
create_daily_statistics_table <- function(vh_results) {

  # Extract date from datetime
  vh_results$date <- as.Date(vh_results$datetime)

  # Calculate daily statistics
  daily_stats <- aggregate(
    Vh_cm_hr ~ date,
    data = vh_results[is.finite(vh_results$Vh_cm_hr), ],
    FUN = function(x) {
      c(n = length(x),
        mean = round(mean(x), 2),
        median = round(median(x), 2),
        sd = round(sd(x), 2),
        min = round(min(x), 2),
        max = round(max(x), 2))
    }
  )

  # Convert to proper data frame format
  stats_matrix <- daily_stats$Vh_cm_hr
  daily_table <- data.frame(
    Date = daily_stats$date,
    N = stats_matrix[, "n"],
    Mean = stats_matrix[, "mean"],
    Median = stats_matrix[, "median"],
    SD = stats_matrix[, "sd"],
    Min = stats_matrix[, "min"],
    Max = stats_matrix[, "max"],
    stringsAsFactors = FALSE
  )

  # Limit to first 30 days for readability
  if (nrow(daily_table) > 30) {
    daily_table <- head(daily_table, 30)
  }

  return(daily_table)
}

# =============================================================================
# REPORT CONTENT AND RENDERING
# =============================================================================

#' Create Report Content
#'
#' @description Internal function to create complete report content
#' @param report_components List with report components
#' @param tables_created List with table data
#' @param figures_created List with figure paths
#' @param report_type Character string
#' @param custom_title Character string
#' @param author_info List with author information
#' @return Character vector with complete report content
#' @keywords internal
create_report_content <- function(report_components, tables_created, figures_created,
                                  report_type, custom_title, author_info) {

  # Create title and metadata
  report_title <- custom_title %||%
    paste("Sap Flow Analysis Report -", Sys.Date())

  content <- c(
    paste("#", report_title),
    ""
  )

  # Add author information if provided
  if (!is.null(author_info)) {
    content <- c(
      content,
      paste("**Author:** ", author_info$name %||% "Not specified"),
      paste("**Affiliation:** ", author_info$affiliation %||% "Not specified"),
      paste("**Contact:** ", author_info$contact %||% "Not specified"),
      paste("**Report Generated:** ", Sys.time()),
      ""
    )
  }

  # Add report components
  for (component_name in names(report_components)) {
    component_content <- report_components[[component_name]]
    if (!is.null(component_content) && length(component_content) > 0) {
      content <- c(content, component_content, "")
    }
  }

  # Add figures section if plots were created
  if (length(figures_created) > 0) {
    content <- c(
      content,
      "## Figures",
      ""
    )

    for (figure_name in names(figures_created)) {
      figure_path <- figures_created[[figure_name]]
      figure_title <- switch(
        figure_name,
        "timeseries" = "Velocity Time Series",
        "method_comparison" = "Method Comparison",
        "histogram" = "Velocity Distribution",
        "quality_flags" = "Quality Flags Distribution",
        tools::toTitleCase(gsub("_", " ", figure_name))
      )

      content <- c(
        content,
        paste("###", figure_title),
        "",
        paste("![", figure_title, "](", basename(figure_path), ")"),
        ""
      )
    }
  }

  return(content)
}

#' Render Report
#'
#' @description Internal function to render report to specified format
#' @param report_content Character vector with report content
#' @param output_file Output file path
#' @param output_format Output format
#' @param template_path Optional template path
#' @keywords internal
render_report <- function(report_content, output_file, output_format, template_path) {

  # For now, create simple text/markdown output
  # In a full implementation, this would use rmarkdown for HTML/PDF/Word

  if (output_format == "html") {
    # Simple HTML conversion
    html_content <- convert_markdown_to_html(report_content)
    writeLines(html_content, output_file)
  } else {
    # Write as markdown/text
    writeLines(report_content, output_file)
  }
}

#' Convert Markdown to HTML
#'
#' @description Internal function for basic markdown to HTML conversion
#' @param markdown_content Character vector with markdown content
#' @return Character vector with HTML content
#' @keywords internal
convert_markdown_to_html <- function(markdown_content) {

  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>Sap Flow Analysis Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 40px; }",
    "h1 { colour: #2c3e50; }",
    "h2 { colour: #34495e; border-bottom: 2px solid #ecf0f1; }",
    "h3 { colour: #7f8c8d; }",
    "code { background-colour: #f8f9fa; padding: 2px 4px; }",
    "pre { background-colour: #f8f9fa; padding: 10px; border-left: 3px solid #007bff; }",
    "</style>",
    "</head>",
    "<body>"
  )

  # Simple markdown parsing (basic implementation)
  for (line in markdown_content) {
    if (grepl("^# ", line)) {
      html_line <- paste0("<h1>", gsub("^# ", "", line), "</h1>")
    } else if (grepl("^## ", line)) {
      html_line <- paste0("<h2>", gsub("^## ", "", line), "</h2>")
    } else if (grepl("^### ", line)) {
      html_line <- paste0("<h3>", gsub("^### ", "", line), "</h3>")
    } else if (grepl("^\\*\\*.*\\*\\*", line)) {
      html_line <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", line)
    } else if (line == "") {
      html_line <- "<br>"
    } else {
      html_line <- paste0("<p>", line, "</p>")
    }

    html_content <- c(html_content, html_line)
  }

  html_content <- c(html_content, "</body>", "</html>")

  return(html_content)
}# R/report_templates.R
# Chat 9: Report Template Creation Functions
# sapFluxR Package Development

#' Report Template Creation Functions
#'
#' @description
#' Functions for creating standardised report templates for different use cases
#' including research papers, technical memos, monitoring reports, and troubleshooting guides.
#'
#' @name report_templates
#' @keywords internal
NULL

#' Create Research Paper Template
#'
#' @description Internal function to create research paper template
#' @param include_sample_data Logical, whether to include sample data sections
#' @param custom_sections List of custom sections
#' @return Character vector with template content
#' @keywords internal
create_research_template <- function(include_sample_data = TRUE, custom_sections = NULL) {

  template_content <- c(
    "---",
    "title: \"Sap Flow Analysis: [INSERT TITLE]\"",
    "author: \"[INSERT AUTHOR NAME]\"",
    "date: \"`r Sys.Date()`\"",
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    number_sections: true",
    "  pdf_document:",
    "    toc: true",
    "    number_sections: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(sapFluxR)",
    "```",
    "",
    "# Abstract",
    "",
    "[INSERT ABSTRACT - Summary of research objectives, methods, key findings, and conclusions]",
    "",
    "# Introduction",
    "",
    "## Research Objectives",
    "",
    "[INSERT RESEARCH OBJECTIVES]",
    "",
    "## Study Site Description",
    "",
    "[INSERT SITE DESCRIPTION]",
    "",
    "- **Location:** [GPS coordinates, elevation]",
    "- **Species:** [Tree species studied]",
    "- **Environmental conditions:** [Climate, soil type, etc.]",
    "- **Measurement period:** [Start date - End date]",
    "",
    "# Methods",
    "",
    "## Sensor Installation",
    "",
    "[DESCRIBE SENSOR INSTALLATION METHOD]",
    "",
    "## Data Collection",
    "",
    "```{r data-import, eval=FALSE}",
    "# Data import and initial processing",
    "sap_data <- read_sap_data(\"path/to/data/file.txt\")",
    "print(sap_data)",
    "```",
    "",
    "## Data Processing and Analysis",
    "",
    "### Heat Pulse Velocity Calculations",
    "",
    "```{r velocity-calculations, eval=FALSE}",
    "# Calculate velocities using multiple methods",
    "vh_results <- calc_heat_pulse_velocity(",
    "  sap_data,",
    "  methods = c(\"HRM\", \"MHR\", \"DMA\"),",
    "  parameters = list(",
    "    diffusivity = 0.0025,  # Species-specific thermal diffusivity",
    "    x = 0.5                # Probe spacing correction factor",
    "  )",
    ")",
    "```",
    "",
    "### Quality Control",
    "",
    "```{r quality-control, eval=FALSE}",
    "# Apply quality control filters",
    "clean_results <- filter_velocity_results(",
    "  vh_results,",
    "  quality_flags = \"OK\",",
    "  velocity_range = c(-10, 200)",
    ")",
    "```",
    "",
    "### Statistical Analysis",
    "",
    "```{r statistical-analysis, eval=FALSE}",
    "# Calculate enhanced statistics",
    "enhanced_stats <- calc_enhanced_statistics(",
    "  clean_results,",
    "  group_by = c(\"method\", \"temporal\"),",
    "  temporal_aggregation = \"daily\",",
    "  include_confidence_intervals = TRUE",
    ")",
    "```",
    "",
    "# Results",
    "",
    "## Data Quality Assessment",
    "",
    "```{r data-quality, eval=FALSE}",
    "# Generate diagnostic report",
    "diagnostic_report <- generate_diagnostic_report(",
    "  sap_data, vh_results,",
    "  report_type = \"technical\",",
    "  include_plots = TRUE",
    ")",
    "",
    "print(diagnostic_report$report_summary)",
    "```",
    "",
    "## Velocity Measurements",
    "",
    "### Descriptive Statistics",
    "",
    "[INSERT STATISTICAL RESULTS TABLE]",
    "",
    "### Temporal Patterns",
    "",
    "```{r temporal-plots, eval=FALSE}",
    "# Create time series plots",
    "plot_velocity_diagnostics(clean_results, \"time_series\")",
    "plot_velocity_diagnostics(clean_results, \"methods_comparison\")",
    "```",
    "",
    "## Method Comparison",
    "",
    "### Inter-method Correlation",
    "",
    "[INSERT CORRELATION ANALYSIS]",
    "",
    "### Method Performance Evaluation",
    "",
    "[INSERT METHOD EVALUATION RESULTS]",
    "",
    "# Discussion",
    "",
    "## Key Findings",
    "",
    "[DISCUSS MAIN RESULTS]",
    "",
    "## Method Performance",
    "",
    "[DISCUSS METHOD COMPARISON RESULTS]",
    "",
    "## Environmental Influences",
    "",
    "[DISCUSS ENVIRONMENTAL FACTORS]",
    "",
    "## Limitations and Sources of Uncertainty",
    "",
    "[DISCUSS LIMITATIONS]",
    "",
    "# Conclusions",
    "",
    "[INSERT CONCLUSIONS AND IMPLICATIONS]",
    "",
    "# Acknowledgments",
    "",
    "[INSERT ACKNOWLEDGMENTS]",
    "",
    "# References",
    "",
    "[INSERT REFERENCES]",
    "",
    "# Appendices",
    "",
    "## Appendix A: Technical Specifications",
    "",
    "```{r technical-specs, eval=FALSE}",
    "# Export detailed technical information",
    "export_sap_data(",
    "  sap_data, vh_results,",
    "  \"technical_data.xlsx\",",
    "  format = \"research_standard\",",
    "  include_metadata = TRUE",
    ")",
    "```",
    "",
    "## Appendix B: Quality Control Plots",
    "",
    "```{r quality-plots, eval=FALSE}",
    "# Generate comprehensive diagnostic plots",
    "plot_velocity_diagnostics(vh_results, \"quality_flags\")",
    "plot_velocity_diagnostics(vh_results, \"histogram\")",
    "```"
  )

  # Add sample data sections if requested
  if (include_sample_data) {
    sample_sections <- c(
      "",
      "## Appendix C: Sample Data",
      "",
      "```{r sample-data, eval=FALSE}",
      "# Display sample of processed data",
      "head(clean_results, 20)",
      "```"
    )
    template_content <- c(template_content, sample_sections)
  }

  # Add custom sections if provided
  if (!is.null(custom_sections)) {
    for (section_name in names(custom_sections)) {
      template_content <- c(
        template_content,
        "",
        custom_sections[[section_name]]
      )
    }
  }

  return(template_content)
}

#' Create Technical Memo Template
#'
#' @description Internal function to create technical memo template
#' @param include_sample_data Logical, whether to include sample data sections
#' @param custom_sections List of custom sections
#' @return Character vector with template content
#' @keywords internal
create_technical_memo_template <- function(include_sample_data = TRUE, custom_sections = NULL) {

  template_content <- c(
    "---",
    "title: \"Technical Memo: Sap Flow Analysis\"",
    "author: \"[INSERT NAME]\"",
    "date: \"`r Sys.Date()`\"",
    "output:",
    "  html_document:",
    "    toc: true",
    "    theme: flatly",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(sapFluxR)",
    "```",
    "",
    "# Executive Summary",
    "",
    "**TO:** [INSERT RECIPIENT]  ",
    "**FROM:** [INSERT SENDER]  ",
    "**DATE:** `r Sys.Date()`  ",
    "**RE:** Sap Flow Data Analysis Results  ",
    "",
    "## Key Findings",
    "",
    "- [INSERT KEY FINDING 1]",
    "- [INSERT KEY FINDING 2]",
    "- [INSERT KEY FINDING 3]",
    "",
    "## Recommendations",
    "",
    "- [INSERT RECOMMENDATION 1]",
    "- [INSERT RECOMMENDATION 2]",
    "- [INSERT RECOMMENDATION 3]",
    "",
    "# Data Summary",
    "",
    "```{r data-summary}",
    "# Load and summarize data",
    "# sap_data <- read_sap_data(\"data_file.txt\")",
    "# vh_results <- calc_heat_pulse_velocity(sap_data, methods = c(\"HRM\", \"MHR\"))",
    "",
    "# Summary statistics",
    "# knitr::kable(calc_velocity_stats(vh_results, group_by = \"method\"))",
    "```",
    "",
    "## Data Quality",
    "",
    "```{r quality-assessment}",
    "# Quality flag summary",
    "# quality_summary <- table(vh_results$quality_flag)",
    "# knitr::kable(quality_summary)",
    "```",
    "",
    "# Analysis Results",
    "",
    "## Velocity Measurements",
    "",
    "```{r velocity-analysis}",
    "# Create summary plots",
    "# plot_velocity_diagnostics(vh_results, \"methods_comparison\")",
    "```",
    "",
    "## Temporal Patterns",
    "",
    "```{r temporal-analysis}",
    "# Time series analysis",
    "# plot_velocity_diagnostics(vh_results, \"time_series\")",
    "```",
    "",
    "# Technical Details",
    "",
    "## Processing Parameters",
    "",
    "- **Analysis Date:** `r Sys.Date()`",
    "- **sapFluxR Version:** `r packageVersion(\"sapFluxR\")`",
    "- **Methods Applied:** [INSERT METHODS]",
    "- **Quality Control:** [INSERT QC PROCEDURES]",
    "",
    "## Data Processing Steps",
    "",
    "1. **Data Import:** Automatic format detection and parsing",
    "2. **Validation:** Comprehensive data quality checks",
    "3. **Calculations:** Heat pulse velocity calculations using multiple methods",
    "4. **Quality Control:** Automated quality flagging and filtering",
    "5. **Analysis:** Statistical analysis and visualization",
    "",
    "# Issues and Recommendations",
    "",
    "## Data Quality Issues",
    "",
    "[INSERT ANY DATA QUALITY CONCERNS]",
    "",
    "## Method Performance",
    "",
    "[INSERT METHOD PERFORMANCE ASSESSMENT]",
    "",
    "## Next Steps",
    "",
    "1. [INSERT NEXT STEP 1]",
    "2. [INSERT NEXT STEP 2]",
    "3. [INSERT NEXT STEP 3]",
    "",
    "# Appendices",
    "",
    "## Appendix A: Detailed Statistics",
    "",
    "```{r detailed-stats}",
    "# Enhanced statistics",
    "# enhanced_stats <- calc_enhanced_statistics(vh_results)",
    "# knitr::kable(enhanced_stats$summary_stats)",
    "```"
  )

  # Add sample data if requested
  if (include_sample_data) {
    sample_sections <- c(
      "",
      "## Appendix B: Sample Data Export",
      "",
      "```{r data-export}",
      "# Export results for further analysis",
      "# export_sap_data(sap_data, vh_results, \"analysis_results.csv\")",
      "```"
    )
    template_content <- c(template_content, sample_sections)
  }

  # Add custom sections if provided
  if (!is.null(custom_sections)) {
    for (section_name in names(custom_sections)) {
      template_content <- c(
        template_content,
        "",
        custom_sections[[section_name]]
      )
    }
  }

  return(template_content)
}

#' Create Monitoring Report Template
#'
#' @description Internal function to create monitoring report template
#' @param include_sample_data Logical, whether to include sample data sections
#' @param custom_sections List of custom sections
#' @return Character vector with template content
#' @keywords internal
create_monitoring_template <- function(include_sample_data = TRUE, custom_sections = NULL) {

  template_content <- c(
    "---",
    "title: \"Sap Flow Monitoring Report\"",
    "subtitle: \"Site: [INSERT SITE NAME]\"",
    "author: \"[INSERT MONITORING TEAM]\"",
    "date: \"`r Sys.Date()`\"",
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: cerulean",
    "    code_folding: hide",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(sapFluxR)",
    "library(knitr)",
    "```",
    "",
    "# Monitoring Summary",
    "",
    "**Monitoring Period:** [INSERT PERIOD]  ",
    "**Site Location:** [INSERT LOCATION]  ",
    "**Species Monitored:** [INSERT SPECIES]  ",
    "**Report Generated:** `r Sys.Date()`  ",
    "",
    "## Status Overview",
    "",
    "```{r status-overview}",
    "# Load current monitoring data",
    "# current_data <- read_sap_data(\"current_monitoring_data.txt\")",
    "# vh_results <- calc_heat_pulse_velocity(current_data, methods = c(\"HRM\", \"DMA\"))",
    "",
    "# System status check",
    "# diagnostic_report <- generate_diagnostic_report(current_data, vh_results)",
    "# cat(\"Overall System Status:\", diagnostic_report$report_summary$overall_quality_score, \"/100\")",
    "```",
    "",
    "### Current Conditions",
    "",
    "- **System Status:** [OPERATIONAL/WARNING/ALERT]",
    "- **Data Quality:** [EXCELLENT/GOOD/FAIR/POOR]",
    "- **Last Maintenance:** [INSERT DATE]",
    "- **Next Scheduled Maintenance:** [INSERT DATE]",
    "",
    "# Recent Trends",
    "",
    "## Weekly Summary",
    "",
    "```{r weekly-trends}",
    "# Calculate weekly trends",
    "# weekly_stats <- calc_enhanced_statistics(",
    "#   vh_results,",
    "#   group_by = c(\"method\", \"temporal\"),",
    "#   temporal_aggregation = \"weekly\"",
    "# )",
    "# knitr::kable(weekly_stats$temporal_patterns)",
    "```",
    "",
    "## Daily Patterns",
    "",
    "```{r daily-patterns}",
    "# Daily pattern analysis",
    "# plot_velocity_diagnostics(vh_results, \"time_series\")",
    "```",
    "",
    "# System Performance",
    "",
    "## Sensor Status",
    "",
    "```{r sensor-diagnostics}",
    "# Sensor performance diagnostics",
    "# sensor_diag <- diagnose_sensor_performance(current_data)",
    "# knitr::kable(data.frame(",
    "#   Sensor = names(sensor_diag$sensor_status),",
    "#   Status = sensor_diag$sensor_status",
    "# ))",
    "```",
    "",
    "## Data Quality Metrics",
    "",
    "```{r quality-metrics}",
    "# Quality assessment",
    "# quality_assessment <- validate_sap_data(current_data)",
    "# cat(\"Data Completeness:\", round(quality_assessment$summary$overall_completeness * 100, 1), \"%\")",
    "```",
    "",
    "# Environmental Conditions",
    "",
    "## Weather Summary",
    "",
    "[INSERT WEATHER CONDITIONS FOR MONITORING PERIOD]",
    "",
    "- **Temperature Range:** [MIN - MAX °C]",
    "- **Precipitation:** [TOTAL mm]",
    "- **Humidity:** [AVERAGE %]",
    "- **Wind:** [AVERAGE m/s]",
    "",
    "## Environmental Influences",
    "",
    "```{r environmental-analysis}",
    "# Correlate sap flow with environmental conditions",
    "# This section would include environmental data analysis if available",
    "```",
    "",
    "# Alerts and Issues",
    "",
    "## Current Alerts",
    "",
    "```{r current-alerts}",
    "# Check for system alerts",
    "# alerts <- diagnostic_report$recommendations[grepl(\"CRITICAL|URGENT\", diagnostic_report$recommendations)]",
    "# if(length(alerts) > 0) {",
    "#   cat(\"Active Alerts:\"))",
    "#   for(alert in alerts) cat(\"-\", alert)",
    "# } else {",
    "#   cat(\"No active alerts.\")",
    "# }",
    "```",
    "",
    "## Maintenance Requirements",
    "",
    "- [ ] [INSERT MAINTENANCE TASK 1]",
    "- [ ] [INSERT MAINTENANCE TASK 2]",
    "- [ ] [INSERT MAINTENANCE TASK 3]",
    "",
    "# Historical Comparison",
    "",
    "## Long-term Trends",
    "",
    "```{r long-term-trends}",
    "# Compare with historical data",
    "# This section would compare current period with historical averages",
    "```",
    "",
    "## Seasonal Patterns",
    "",
    "```{r seasonal-analysis}",
    "# Seasonal pattern analysis",
    "# This section would show seasonal comparisons",
    "```",
    "",
    "# Data Export and Archiving",
    "",
    "## Current Period Data",
    "",
    "```{r data-export}",
    "# Export current monitoring data",
    "# export_sap_data(",
    "#   current_data, vh_results,",
    "#   paste0(\"monitoring_export_\", Sys.Date(), \".xlsx\"),",
    "#   format = \"xlsx\",",
    "#   include_metadata = TRUE",
    "# )",
    "```",
    "",
    "## Archive Summary",
    "",
    "- **Current Database Size:** [INSERT SIZE]",
    "- **Records Archived:** [INSERT COUNT]",
    "- **Backup Status:** [CURRENT/OUTDATED]",
    "",
    "# Next Monitoring Period",
    "",
    "## Scheduled Activities",
    "",
    "1. **Data Collection:** Continue automated monitoring",
    "2. **System Check:** [INSERT NEXT CHECK DATE]",
    "3. **Calibration:** [INSERT NEXT CALIBRATION DATE]",
    "4. **Report Generation:** [INSERT NEXT REPORT DATE]",
    "",
    "## Expected Conditions",
    "",
    "[INSERT EXPECTED CONDITIONS FOR NEXT PERIOD]",
    "",
    "# Contact Information",
    "",
    "**Primary Contact:** [INSERT NAME AND CONTACT]  ",
    "**Technical Support:** [INSERT TECHNICAL CONTACT]  ",
    "**Emergency Contact:** [INSERT EMERGENCY CONTACT]  "
  )

  # Add sample data if requested
  if (include_sample_data) {
    sample_sections <- c(
      "",
      "# Appendix: Sample Data Tables",
      "",
      "```{r sample-data-tables}",
      "# Display recent measurements",
      "# head(vh_results, 50)",
      "```"
    )
    template_content <- c(template_content, sample_sections)
  }

  # Add custom sections if provided
  if (!is.null(custom_sections)) {
    for (section_name in names(custom_sections)) {
      template_content <- c(
        template_content,
        "",
        custom_sections[[section_name]]
      )
    }
  }

  return(template_content)
}

#' Create Troubleshooting Template
#'
#' @description Internal function to create troubleshooting template
#' @param include_sample_data Logical, whether to include sample data sections
#' @param custom_sections List of custom sections
#' @return Character vector with template content
#' @keywords internal
create_troubleshooting_template <- function(include_sample_data = TRUE, custom_sections = NULL) {

  template_content <- c(
    "---",
    "title: \"Sap Flow System Troubleshooting Report\"",
    "author: \"[INSERT TECHNICIAN NAME]\"",
    "date: \"`r Sys.Date()`\"",
    "output:",
    "  html_document:",
    "    toc: true",
    "    theme: united",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(sapFluxR)",
    "```",
    "",
    "# Problem Report",
    "",
    "**System ID:** [INSERT SYSTEM ID]  ",
    "**Site Location:** [INSERT LOCATION]  ",
    "**Problem Reported:** [INSERT PROBLEM DESCRIPTION]  ",
    "**Report Date:** `r Sys.Date()`  ",
    "**Technician:** [INSERT NAME]  ",
    "",
    "## Problem Description",
    "",
    "[DETAILED DESCRIPTION OF THE PROBLEM]",
    "",
    "### Symptoms Observed",
    "",
    "- [INSERT SYMPTOM 1]",
    "- [INSERT SYMPTOM 2]",
    "- [INSERT SYMPTOM 3]",
    "",
    "### When Problem Occurred",
    "",
    "- **First Noticed:** [INSERT DATE/TIME]",
    "- **Frequency:** [CONTINUOUS/INTERMITTENT/SPECIFIC CONDITIONS]",
    "- **Environmental Conditions:** [INSERT CONDITIONS]",
    "",
    "# Diagnostic Analysis",
    "",
    "## Data Quality Assessment",
    "",
    "```{r data-diagnostics}",
    "# Load problematic data",
    "# problem_data <- read_sap_data(\"problematic_data.txt\")",
    "# vh_results <- calc_heat_pulse_velocity(problem_data, methods = c(\"HRM\", \"MHR\", \"DMA\"))",
    "",
    "# Comprehensive diagnostics",
    "# diagnostic_report <- generate_diagnostic_report(",
    "#   problem_data, vh_results,",
    "#   report_type = \"full\",",
    "#   include_plots = TRUE",
    "# )",
    "",
    "# print(diagnostic_report$report_summary)",
    "```",
    "",
    "## Sensor Performance",
    "",
    "```{r sensor-diagnostics}",
    "# Detailed sensor analysis",
    "# sensor_diag <- diagnose_sensor_performance(problem_data, detailed = TRUE)",
    "# knitr::kable(data.frame(",
    "#   Sensor = names(sensor_diag$sensor_status),",
    "#   Status = sensor_diag$sensor_status,",
    "#   Issues = sensor_diag$sensor_issues[names(sensor_diag$sensor_status)]",
    "# ))",
    "```",
    "",
    "## Probe Alignment Check",
    "",
    "```{r alignment-check}",
    "# Probe alignment validation",
    "# alignment_check <- validate_probe_alignment_advanced(problem_data)",
    "# cat(\"Alignment Status:\", alignment_check$alignment_status)",
    "# cat(\"Alignment Quality:\", alignment_check$alignment_quality)",
    "```",
    "",
    "## Method Comparison",
    "",
    "```{r method-comparison}",
    "# Compare calculation methods",
    "# method_comparison <- compare_hpv_methods(vh_results)",
    "# knitr::kable(method_comparison$correlation_matrix)",
    "```",
    "",
    "# Problem Analysis",
    "",
    "## Root Cause Investigation",
    "",
    "### Hypothesis 1: [INSERT HYPOTHESIS]",
    "",
    "**Evidence:**",
    "- [INSERT EVIDENCE 1]",
    "- [INSERT EVIDENCE 2]",
    "",
    "**Testing Results:**",
    "```{r hypothesis-1-test}",
    "# Test hypothesis 1",
    "# [INSERT ANALYSIS CODE]",
    "```",
    "",
    "### Hypothesis 2: [INSERT HYPOTHESIS]",
    "",
    "**Evidence:**",
    "- [INSERT EVIDENCE 1]",
    "- [INSERT EVIDENCE 2]",
    "",
    "**Testing Results:**",
    "```{r hypothesis-2-test}",
    "# Test hypothesis 2",
    "# [INSERT ANALYSIS CODE]",
    "```",
    "",
    "## Likely Causes",
    "",
    "Based on diagnostic analysis, the most likely causes are:",
    "",
    "1. **Primary Cause:** [INSERT MOST LIKELY CAUSE]",
    "   - **Confidence Level:** [HIGH/MEDIUM/LOW]",
    "   - **Supporting Evidence:** [INSERT EVIDENCE]",
    "",
    "2. **Secondary Cause:** [INSERT SECONDARY CAUSE]",
    "   - **Confidence Level:** [HIGH/MEDIUM/LOW]",
    "   - **Supporting Evidence:** [INSERT EVIDENCE]",
    "",
    "# Recommended Solutions",
    "",
    "## Immediate Actions (0-24 hours)",
    "",
    "- [ ] [INSERT IMMEDIATE ACTION 1]",
    "- [ ] [INSERT IMMEDIATE ACTION 2]",
    "- [ ] [INSERT IMMEDIATE ACTION 3]",
    "",
    "## Short-term Solutions (1-7 days)",
    "",
    "- [ ] [INSERT SHORT-TERM SOLUTION 1]",
    "- [ ] [INSERT SHORT-TERM SOLUTION 2]",
    "- [ ] [INSERT SHORT-TERM SOLUTION 3]",
    "",
    "## Long-term Improvements (1-4 weeks)",
    "",
    "- [ ] [INSERT LONG-TERM IMPROVEMENT 1]",
    "- [ ] [INSERT LONG-TERM IMPROVEMENT 2]",
    "- [ ] [INSERT LONG-TERM IMPROVEMENT 3]",
    "",
    "# Implementation Plan",
    "",
    "## Resource Requirements",
    "",
    "- **Personnel:** [INSERT PERSONNEL NEEDED]",
    "- **Equipment:** [INSERT EQUIPMENT NEEDED]",
    "- **Time Estimate:** [INSERT TIME ESTIMATE]",
    "- **Cost Estimate:** [INSERT COST ESTIMATE]",
    "",
    "## Implementation Steps",
    "",
    "1. **Step 1:** [INSERT DETAILED STEP]",
    "   - **Responsible:** [INSERT PERSON]",
    "   - **Timeline:** [INSERT TIMELINE]",
    "   - **Resources:** [INSERT RESOURCES]",
    "",
    "2. **Step 2:** [INSERT DETAILED STEP]",
    "   - **Responsible:** [INSERT PERSON]",
    "   - **Timeline:** [INSERT TIMELINE]",
    "   - **Resources:** [INSERT RESOURCES]",
    "",
    "3. **Step 3:** [INSERT DETAILED STEP]",
    "   - **Responsible:** [INSERT PERSON]",
    "   - **Timeline:** [INSERT TIMELINE]",
    "   - **Resources:** [INSERT RESOURCES]",
    "",
    "# Follow-up and Monitoring",
    "",
    "## Success Criteria",
    "",
    "The solution will be considered successful when:",
    "",
    "- [INSERT SUCCESS CRITERION 1]",
    "- [INSERT SUCCESS CRITERION 2]",
    "- [INSERT SUCCESS CRITERION 3]",
    "",
    "## Monitoring Plan",
    "",
    "```{r monitoring-setup}",
    "# Set up monitoring for solution effectiveness",
    "# This code would establish ongoing monitoring",
    "```",
    "",
    "## Follow-up Schedule",
    "",
    "- **24 Hour Check:** [INSERT DATE/TIME]",
    "- **1 Week Review:** [INSERT DATE]",
    "- **1 Month Assessment:** [INSERT DATE]",
    "",
    "# Prevention Measures",
    "",
    "## System Improvements",
    "",
    "To prevent similar issues in the future:",
    "",
    "- [INSERT PREVENTION MEASURE 1]",
    "- [INSERT PREVENTION MEASURE 2]",
    "- [INSERT PREVENTION MEASURE 3]",
    "",
    "## Maintenance Schedule Updates",
    "",
    "- [INSERT MAINTENANCE UPDATE 1]",
    "- [INSERT MAINTENANCE UPDATE 2]",
    "",
    "# Documentation and Reporting",
    "",
    "## Data Archive",
    "",
    "```{r data-archive}",
    "# Archive troubleshooting data",
    "# export_sap_data(",
    "#   problem_data, vh_results,",
    "#   paste0(\"troubleshooting_data_\", Sys.Date(), \".xlsx\"),",
    "#   format = \"xlsx\",",
    "#   include_metadata = TRUE",
    "# )",
    "```",
    "",
    "## Report Distribution",
    "",
    "This report should be distributed to:",
    "",
    "- [INSERT RECIPIENT 1]",
    "- [INSERT RECIPIENT 2]",
    "- [INSERT RECIPIENT 3]",
    "",
    "## Knowledge Base Update",
    "",
    "Add this case to the troubleshooting knowledge base with:",
    "",
    "- **Problem Category:** [INSERT CATEGORY]",
    "- **Keywords:** [INSERT KEYWORDS]",
    "- **Solution Summary:** [INSERT SOLUTION SUMMARY]"
  )

  # Add sample data if requested
  if (include_sample_data) {
    sample_sections <- c(
      "",
      "# Appendix: Diagnostic Data",
      "",
      "```{r diagnostic-data}",
      "# Display problematic data samples",
      "# head(problem_data$measurements, 20)",
      "# head(vh_results, 20)",
      "```"
    )
    template_content <- c(template_content, sample_sections)
  }

  # Add custom sections if provided
  if (!is.null(custom_sections)) {
    for (section_name in names(custom_sections)) {
      template_content <- c(
        template_content,
        "",
        custom_sections[[section_name]]
      )
    }
  }

  return(template_content)
}