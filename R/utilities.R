#' Utility Functions for sapFluxR Package
#'
#' Collection of helper functions for data processing and analysis
#'
#' @name utilities
NULL

#' Convert Heat Pulse Velocity Units
#'
#' Converts heat pulse velocity between different units (cm/hr, mm/hr, m/day, etc.)
#'
#' @param velocity Numeric vector of velocity values
#' @param from Character string specifying input units. Options: "cm_hr", "mm_hr", "m_day", "mm_s"
#' @param to Character string specifying output units. Same options as 'from'
#'
#' @return Numeric vector of converted velocity values
#'
#' @examples
#' \dontrun{
#' # Convert from cm/hr to mm/hr
#' convert_velocity_units(c(10, 20, 30), from = "cm_hr", to = "mm_hr")
#'
#' # Convert from cm/hr to m/day
#' convert_velocity_units(15.5, from = "cm_hr", to = "m_day")
#' }
#'
#' @export
convert_velocity_units <- function(velocity, from = "cm_hr", to = "mm_hr") {

  # Define conversion factors to cm/hr (base unit)
  to_cm_hr <- list(
    cm_hr = 1,
    mm_hr = 0.1,
    m_day = 1 / 24,
    mm_s = 0.1 * 3600,
    cm_s = 3600,
    m_hr = 0.01
  )

  # Define conversion factors from cm/hr
  from_cm_hr <- list(
    cm_hr = 1,
    mm_hr = 10,
    m_day = 24,
    mm_s = 10 / 3600,
    cm_s = 1 / 3600,
    m_hr = 100
  )

  # Validate input units
  if (!from %in% names(to_cm_hr)) {
    stop("Unsupported 'from' unit: ", from,
         ". Supported units: ", paste(names(to_cm_hr), collapse = ", "))
  }

  if (!to %in% names(from_cm_hr)) {
    stop("Unsupported 'to' unit: ", to,
         ". Supported units: ", paste(names(from_cm_hr), collapse = ", "))
  }

  # Convert via cm/hr as intermediate unit
  velocity_cm_hr <- velocity * to_cm_hr[[from]]
  result <- velocity_cm_hr * from_cm_hr[[to]]

  return(result)
}

#' Calculate Basic Statistics for Velocity Results
#'
#' Calculates summary statistics for heat pulse velocity measurements
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param group_by Character vector specifying grouping variables.
#'   Options: "method", "sensor_position", "pulse_id"
#' @param exclude_quality_flags Character vector of quality flags to exclude from analysis
#'
#' @return A tibble with summary statistics
#'
#' @examples
#' \dontrun{
#' # Calculate overall statistics
#' stats <- calc_velocity_stats(vh_results)
#'
#' # Calculate statistics by method
#' stats_by_method <- calc_velocity_stats(vh_results, group_by = "method")
#' }
#'
#' @export
calc_velocity_stats <- function(vh_results,
                                group_by = NULL,
                                exclude_quality_flags = c("INFINITE", "MISSING")) {

  if (!inherits(vh_results, "vh_results")) {
    stop("Input must be a vh_results object")
  }

  # Filter out problematic quality flags
  if (length(exclude_quality_flags) > 0) {
    vh_results <- vh_results[!vh_results$quality_flag %in% exclude_quality_flags, ]
  }

  if (nrow(vh_results) == 0) {
    stop("No valid data remaining after filtering")
  }

  # Define grouping variables
  if (is.null(group_by)) {
    group_vars <- NULL
  } else {
    # Validate grouping variables
    invalid_vars <- setdiff(group_by, names(vh_results))
    if (length(invalid_vars) > 0) {
      stop("Invalid grouping variables: ", paste(invalid_vars, collapse = ", "))
    }
    group_vars <- group_by
  }

  # Calculate statistics
  if (is.null(group_vars)) {
    # Overall statistics
    result <- tibble::tibble(
      n = nrow(vh_results),
      mean_velocity = mean(vh_results$Vh_cm_hr, na.rm = TRUE),
      median_velocity = median(vh_results$Vh_cm_hr, na.rm = TRUE),
      sd_velocity = sd(vh_results$Vh_cm_hr, na.rm = TRUE),
      min_velocity = min(vh_results$Vh_cm_hr, na.rm = TRUE),
      max_velocity = max(vh_results$Vh_cm_hr, na.rm = TRUE),
      q25_velocity = quantile(vh_results$Vh_cm_hr, 0.25, na.rm = TRUE),
      q75_velocity = quantile(vh_results$Vh_cm_hr, 0.75, na.rm = TRUE),
      n_negative = sum(vh_results$Vh_cm_hr < 0, na.rm = TRUE),
      prop_negative = mean(vh_results$Vh_cm_hr < 0, na.rm = TRUE)
    )
  } else {
    # Grouped statistics
    result <- vh_results %>%
      dplyr::group_by(!!!syms(group_vars)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE),
        median_velocity = median(.data$Vh_cm_hr, na.rm = TRUE),
        sd_velocity = sd(.data$Vh_cm_hr, na.rm = TRUE),
        min_velocity = min(.data$Vh_cm_hr, na.rm = TRUE),
        max_velocity = max(.data$Vh_cm_hr, na.rm = TRUE),
        q25_velocity = quantile(.data$Vh_cm_hr, 0.25, na.rm = TRUE),
        q75_velocity = quantile(.data$Vh_cm_hr, 0.75, na.rm = TRUE),
        n_negative = sum(.data$Vh_cm_hr < 0, na.rm = TRUE),
        prop_negative = mean(.data$Vh_cm_hr < 0, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(result)
}

#' Filter Velocity Results by Quality and Range
#'
#' Filters heat pulse velocity results based on quality flags and value ranges
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param quality_flags Character vector of acceptable quality flags (default: "OK")
#' @param velocity_range Numeric vector of length 2 specifying min/max velocity range
#' @param methods Character vector of methods to include (default: all)
#' @param sensor_positions Character vector of sensor positions to include (default: all)
#'
#' @return Filtered vh_results object
#'
#' @examples
#' \dontrun{
#' # Keep only high-quality results
#' clean_results <- filter_velocity_results(vh_results, quality_flags = "OK")
#'
#' # Filter by velocity range and methods
#' filtered_results <- filter_velocity_results(
#'   vh_results,
#'   velocity_range = c(-10, 100),
#'   methods = c("HRM", "MHR")
#' )
#' }
#'
#' @export
filter_velocity_results <- function(vh_results,
                                    quality_flags = "OK",
                                    velocity_range = NULL,
                                    methods = NULL,
                                    sensor_positions = NULL) {

  if (!inherits(vh_results, "vh_results")) {
    stop("Input must be a vh_results object")
  }

  result <- vh_results

  # Filter by quality flags
  if (!is.null(quality_flags)) {
    result <- result[result$quality_flag %in% quality_flags, ]
  }

  # Filter by velocity range
  if (!is.null(velocity_range)) {
    if (length(velocity_range) != 2) {
      stop("velocity_range must be a numeric vector of length 2")
    }
    result <- result[result$Vh_cm_hr >= velocity_range[1] &
                       result$Vh_cm_hr <= velocity_range[2], ]
  }

  # Filter by methods
  if (!is.null(methods)) {
    available_methods <- unique(result$method)
    invalid_methods <- setdiff(methods, available_methods)
    if (length(invalid_methods) > 0) {
      warning("Methods not found in data: ", paste(invalid_methods, collapse = ", "))
    }
    result <- result[result$method %in% methods, ]
  }

  # Filter by sensor positions
  if (!is.null(sensor_positions)) {
    available_positions <- unique(result$sensor_position)
    invalid_positions <- setdiff(sensor_positions, available_positions)
    if (length(invalid_positions) > 0) {
      warning("Sensor positions not found in data: ", paste(invalid_positions, collapse = ", "))
    }
    result <- result[result$sensor_position %in% sensor_positions, ]
  }

  # Maintain class
  class(result) <- class(vh_results)

  return(result)
}

#' Export Velocity Results to CSV
#'
#' Exports heat pulse velocity results to CSV file with optional preprocessing
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param file_path Character string specifying output file path
#' @param include_quality_summary Logical indicating whether to include quality summary
#' @param filter_results Logical indicating whether to pre-filter results
#' @param ... Additional arguments passed to filter_velocity_results() if filter_results = TRUE
#'
#' @return Invisibly returns the exported data
#'
#' @examples
#' \dontrun{
#' # Export all results
#' export_velocity_results(vh_results, "sap_flow_results.csv")
#'
#' # Export filtered results with quality summary
#' export_velocity_results(
#'   vh_results,
#'   "filtered_results.csv",
#'   include_quality_summary = TRUE,
#'   filter_results = TRUE,
#'   quality_flags = "OK"
#' )
#' }
#'
#' @export
export_velocity_results <- function(vh_results,
                                    file_path,
                                    include_quality_summary = FALSE,
                                    filter_results = FALSE,
                                    ...) {

  if (!inherits(vh_results, "vh_results")) {
    stop("Input must be a vh_results object")
  }

  # Filter results if requested
  if (filter_results) {
    vh_results <- filter_velocity_results(vh_results, ...)
  }

  # Prepare export data
  export_data <- as.data.frame(vh_results)

  # Add quality summary if requested
  if (include_quality_summary) {
    quality_summary <- table(export_data$quality_flag)

    # Add summary as comments at the top of the file
    summary_text <- c(
      "# Sap Flow Velocity Results Export",
      paste("#", "Export Date:", Sys.time()),
      paste("#", "Total Records:", nrow(export_data)),
      paste("#", "Quality Flag Summary:"),
      paste("#", names(quality_summary), "=", quality_summary, collapse = " "),
      ""
    )

    # Write summary first
    writeLines(summary_text, file_path)

    # Append data
    readr::write_csv(export_data, file_path, append = TRUE)
  } else {
    # Write data directly
    readr::write_csv(export_data, file_path)
  }

  message("Exported ", nrow(export_data), " records to ", file_path)

  invisible(export_data)
}

#' Create Diagnostic Plots for Velocity Results
#'
#' Creates diagnostic plots to assess data quality and method performance
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param plot_type Character string specifying plot type: "methods_comparison",
#'   "quality_flags", "time_series", "histogram"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object (if ggplot2 is available) or base R plot
#'
#' @examples
#' \dontrun{
#' # Compare methods
#' plot_velocity_diagnostics(vh_results, "methods_comparison")
#'
#' # Show quality flag distribution
#' plot_velocity_diagnostics(vh_results, "quality_flags")
#' }
#'
#' @export
plot_velocity_diagnostics <- function(vh_results,
                                      plot_type = "methods_comparison",
                                      ...) {

  if (!inherits(vh_results, "vh_results")) {
    stop("Input must be a vh_results object")
  }

  # Use base R plotting since ggplot2 is not a required dependency
  switch(plot_type,
         "methods_comparison" = {
           # Box plot comparing methods
           methods <- unique(vh_results$method)
           velocities <- split(vh_results$Vh_cm_hr, vh_results$method)

           graphics::boxplot(velocities,
                             main = "Heat Pulse Velocity by Method",
                             xlab = "Method",
                             ylab = "Velocity (cm/hr)",
                             las = 2)
           graphics::grid()
         },

         "quality_flags" = {
           # Bar plot of quality flags
           flag_counts <- table(vh_results$quality_flag)
           graphics::barplot(flag_counts,
                             main = "Quality Flag Distribution",
                             xlab = "Quality Flag",
                             ylab = "Count",
                             las = 2,
                             col = rainbow(length(flag_counts)))
         },

         "time_series" = {
           # Time series plot
           graphics::plot(vh_results$datetime, vh_results$Vh_cm_hr,
                          type = "p",
                          main = "Velocity Time Series",
                          xlab = "Time",
                          ylab = "Velocity (cm/hr)",
                          pch = 19,
                          cex = 0.5)
           graphics::grid()
         },

         "histogram" = {
           # Histogram of velocities
           graphics::hist(vh_results$Vh_cm_hr,
                          main = "Distribution of Heat Pulse Velocities",
                          xlab = "Velocity (cm/hr)",
                          ylab = "Frequency",
                          breaks = 30,
                          col = "lightblue",
                          border = "black")
           graphics::grid()
         },

         stop("Unsupported plot_type: ", plot_type,
              ". Options: methods_comparison, quality_flags, time_series, histogram")
  )
}

#' Check Package Version and Dependencies
#'
#' Utility function to check package version and dependency status
#'
#' @return A list with version information and dependency status
#'
#' @examples
#' \dontrun{
#' check_package_status()
#' }
#'
#' @export
check_package_status <- function() {

  # Get package version
  pkg_version <- utils::packageVersion("sapFluxR")

  # Check key dependencies
  required_packages <- c("dplyr", "stringr", "lubridate", "readr", "tibble", "jsonlite")
  suggested_packages <- c("testthat", "knitr", "rmarkdown", "xts", "dygraphs", "htmltools")

  check_packages <- function(packages) {
    sapply(packages, function(pkg) {
      tryCatch({
        utils::packageVersion(pkg)
        TRUE
      }, error = function(e) {
        FALSE
      })
    })
  }

  required_status <- check_packages(required_packages)
  suggested_status <- check_packages(suggested_packages)

  result <- list(
    package_version = as.character(pkg_version),
    r_version = R.version.string,
    required_packages = required_status,
    suggested_packages = suggested_status,
    all_required_available = all(required_status)
  )

  # Print summary
  cat("sapFluxR Package Status\n")
  cat("======================\n")
  cat("Package Version:", result$package_version, "\n")
  cat("R Version:", result$r_version, "\n")
  cat("Required Dependencies:", ifelse(result$all_required_available, "OK", "MISSING"), "\n")

  if (!result$all_required_available) {
    missing <- names(required_status)[!required_status]
    cat("Missing required packages:", paste(missing, collapse = ", "), "\n")
  }

  invisible(result)
}