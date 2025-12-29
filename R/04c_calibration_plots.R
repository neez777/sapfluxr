# R/04c_calibration_plots.R
# Plotting Functions for Method Calibration
# Visualise calibration transformations and comparisons

#' Calibration Plotting Functions
#'
#' Functions for visualising method calibration results, including time series
#' comparisons of original vs calibrated secondary methods.
#'
#' @name calibration_plots
NULL


#' Plot Calibration Comparison Time Series
#'
#' Creates a time series plot showing the effect of method calibration by
#' comparing original secondary method values, calibrated secondary method
#' values, and primary method values. Provides interactive prompts for date
#' selection if not specified.
#'
#' @param vh_original Original velocity data (before calibration transformation).
#' @param vh_calibrated Calibrated velocity data (after transformation).
#' @param calibration Calibration object from \code{\link{calibrate_method_to_primary}},
#'   OR a multi-method calibration object from \code{\link{calibrate_multiple_methods}}.
#'   If multi-method and \code{method} not specified, prompts user interactively.
#' @param method Character string specifying which secondary method to plot when
#'   \code{calibration} is a multi-method calibration object. If NULL and
#'   interactive, prompts user to select. Ignored if \code{calibration} is a
#'   single calibration object. Default: NULL.
#' @param start_date Start date/datetime (POSIXct, character "YYYY-MM-DD", or
#'   numeric day number). If NULL and interactive, prompts user. Default: NULL.
#' @param end_date End date/datetime (POSIXct, character "YYYY-MM-DD", numeric
#'   day number, or number of days to plot from start). If NULL and interactive,
#'   prompts user. Default: NULL.
#' @param aggregation Optional aggregation period for clearer visualisation:
#'   "hourly", "daily", or "none" (default: "none").
#'
#' @return A ggplot2 plot object showing:
#'   \itemize{
#'     \item Primary method (solid line, steelblue colour)
#'     \item Original secondary method (dashed line, coral colour)
#'     \item Calibrated secondary method (solid line, coral colour)
#'   }
#'
#' @details
#' **Plot Interpretation:**
#'
#' This plot helps assess the calibration transformation:
#' \itemize{
#'   \item **Good calibration**: Calibrated secondary aligns closely with primary
#'   \item **Original vs calibrated**: Shows magnitude of transformation
#'   \item **Temporal patterns**: Verifies waveform similarity between methods
#' }
#'
#' **Interactive Date Selection:**
#'
#' When run in an interactive session without specifying dates, the function
#' prompts for:
#' \itemize{
#'   \item Start date (YYYY-MM-DD format or day number, press Enter for data start)
#'   \item End date (YYYY-MM-DD format or number of days to plot, press Enter for data end)
#' }
#'
#' **Aggregation:**
#'
#' For long time series, use aggregation to reduce overplotting:
#' \itemize{
#'   \item \code{"hourly"}: Mean per hour
#'   \item \code{"daily"}: Mean per day
#'   \item \code{"none"}: All data points (may be slow for large datasets)
#' }
#'
#' @examples
#' \dontrun{
#' # === SINGLE METHOD CALIBRATION ===
#' 
#' # Transform method
#' calibration <- calibrate_method_to_primary(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer",
#'   threshold_velocity = 10
#' )
#' vh_calibrated <- transform_secondary_method(vh_corrected, calibration)
#'
#' # Interactive mode - prompts for dates
#' plot_calibration_comparison(
#'   vh_original = vh_corrected,
#'   vh_calibrated = vh_calibrated,
#'   calibration = calibration
#' )
#'
#' # === MULTI-METHOD CALIBRATION ===
#' 
#' # Calibrate multiple methods
#' calibrations <- calibrate_multiple_methods(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu", "HRMXa"),
#'   sensor_position = "outer"
#' )
#' vh_calibrated <- transform_multiple_methods(vh_corrected, calibrations)
#'
#' # Interactive: Prompts to select which method to plot
#' plot_calibration_comparison(
#'   vh_original = vh_corrected,
#'   vh_calibrated = vh_calibrated,
#'   calibration = calibrations  # Will prompt: "Enter method number to plot [1-3]:"
#' )
#'
#' # Non-interactive: Specify method explicitly
#' plot_calibration_comparison(
#'   vh_original = vh_corrected,
#'   vh_calibrated = vh_calibrated,
#'   calibration = calibrations,
#'   method = "MHR"  # Plot MHR calibration
#' )
#'
#' # Specify dates explicitly
#' plot_calibration_comparison(
#'   vh_original = vh_corrected,
#'   vh_calibrated = vh_calibrated,
#'   calibration = calibrations,
#'   method = "Tmax_Klu",
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-31",
#'   aggregation = "daily"
#' )
#'
#' # Use day numbers and relative days
#' plot_calibration_comparison(
#'   vh_original = vh_corrected,
#'   vh_calibrated = vh_calibrated,
#'   calibration = calibrations,
#'   method = "HRMXa",
#'   start_date = 1,    # Day 1
#'   end_date = 30      # 30 days from start
#' )
#' }
#'
#' @family calibration plotting functions
#' @export
plot_calibration_comparison <- function(vh_original,
                                         vh_calibrated,
                                         calibration,
                                         method = NULL,
                                         start_date = NULL,
                                         end_date = NULL,
                                         aggregation = "none") {

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function. Install with: install.packages('ggplot2')")
  }

  # ============================================================================
  # HANDLE MULTI-METHOD CALIBRATION: Prompt user to select method
  # ============================================================================
  
  if (inherits(calibration, "multi_method_calibration")) {
    # Multi-method calibration provided
    available_methods <- names(calibration)
    
    if (length(available_methods) == 0) {
      stop("calibration is an empty multi-method calibration object")
    }
    
    # If method not specified, prompt user (or error if non-interactive)
    if (is.null(method)) {
      if (interactive()) {
        cat("\n")
        cat(strrep("=", 67), "\n")
        cat("MULTIPLE CALIBRATIONS AVAILABLE\n")
        cat(strrep("=", 67), "\n")
        cat("\n")
        cat("Available secondary methods:\n")
        for (i in seq_along(available_methods)) {
          m <- available_methods[i]
          calib <- calibration[[m]]$optimal_calibration
          cat(sprintf("  [%d] %s (threshold: %.1f cm/hr, R\u00b2: %.4f)\n",
                      i, m, calib$threshold, calib$r_squared))
        }
        cat("\n")
        cat("Enter method number to plot [1-", length(available_methods), "]: ", sep = "")
        
        method_input <- readline()
        method_num <- suppressWarnings(as.integer(method_input))
        
        if (is.na(method_num) || method_num < 1 || method_num > length(available_methods)) {
          stop("Invalid method number. Must be between 1 and ", length(available_methods))
        }
        
        method <- available_methods[method_num]
        cat("\nPlotting calibration for:", method, "\n\n")
      } else {
        stop("calibration is a multi-method object but 'method' not specified.\n",
             "Available methods: ", paste(available_methods, collapse = ", "), "\n",
             "Use: plot_calibration_comparison(..., calibration = calibrations, method = '",
             available_methods[1], "')")
      }
    }
    
    # Validate specified method
    if (!method %in% available_methods) {
      stop("method '", method, "' not found in calibration.\n",
           "Available methods: ", paste(available_methods, collapse = ", "))
    }
    
    # Extract the specific calibration
    calibration <- calibration[[method]]$optimal_calibration
  }
  
  # ============================================================================
  # VALIDATE SINGLE CALIBRATION OBJECT
  # ============================================================================
  
  # Input validation
  if (!inherits(calibration, "method_calibration")) {
    stop("calibration must be a method_calibration object or multi_method_calibration object")
  }

  if (!"datetime" %in% names(vh_original)) {
    stop("vh_original must have a 'datetime' column")
  }

  # Extract relevant data
  primary_method <- calibration$primary_method
  secondary_method <- calibration$secondary_method
  sensor_position <- calibration$sensor_position

  # Filter to relevant method and sensor
  primary_data <- vh_original[
    vh_original$method == primary_method &
    vh_original$sensor_position == sensor_position,
    c("datetime", "Vh_cm_hr")
  ]
  names(primary_data)[2] <- "value"
  primary_data$series <- primary_method

  secondary_original <- vh_original[
    vh_original$method == secondary_method &
    vh_original$sensor_position == sensor_position,
    c("datetime", "Vh_cm_hr")
  ]
  names(secondary_original)[2] <- "value"
  secondary_original$series <- paste0(secondary_method, " (original)")

  secondary_calibrated <- vh_calibrated[
    vh_calibrated$method == secondary_method &
    vh_calibrated$sensor_position == sensor_position,
    c("datetime", "Vh_cm_hr")
  ]
  names(secondary_calibrated)[2] <- "value"
  secondary_calibrated$series <- paste0(secondary_method, " (calibrated)")

  # Combine data
  plot_data <- rbind(primary_data, secondary_original, secondary_calibrated)

  # Convert datetime if needed
  if (!inherits(plot_data$datetime, "POSIXct")) {
    plot_data$datetime <- as.POSIXct(plot_data$datetime)
  }

  # Get data range for interactive prompts
  data_start <- min(plot_data$datetime, na.rm = TRUE)
  data_end <- max(plot_data$datetime, na.rm = TRUE)
  total_days <- as.numeric(difftime(data_end, data_start, units = "days"))

  # ============================================================================
  # INTERACTIVE DATE SELECTION (like other sapfluxr plots)
  # ============================================================================

  if (is.null(start_date) && interactive()) {
    cat("\n")
    cat(strrep("=", 67), "\n")
    cat(sprintf("You have %.1f days of data\n", total_days))
    cat(sprintf("Start: %s\n", format(data_start, "%Y-%m-%d %H:%M:%S")))
    cat(sprintf("End:   %s\n", format(data_end, "%Y-%m-%d %H:%M:%S")))
    cat(strrep("=", 67), "\n\n")

    cat("Enter start date (YYYY-MM-DD) or day number [press Enter for data start]: ")
    start_input <- readline()

    # Default to data start if empty input
    if (trimws(start_input) == "") {
      start_date <- data_start
      cat(sprintf("Using data start: %s\n", format(data_start, "%Y-%m-%d %H:%M:%S")))
    } else {
      start_date <- parse_date_input(start_input, data_start)
    }
  } else if (is.null(start_date)) {
    # Non-interactive: use data start
    start_date <- data_start
  } else if (is.numeric(start_date)) {
    # Convert day number to date
    start_date <- data_start + (start_date - 1) * 86400
  } else if (is.character(start_date)) {
    start_date <- as.POSIXct(start_date)
  }

  if (is.null(end_date) && interactive()) {
    cat("\nEnter end date (YYYY-MM-DD) or number of days to plot [press Enter for data end]: ")
    end_input <- readline()

    # Default to data end if empty input
    if (trimws(end_input) == "") {
      end_date <- data_end
      cat(sprintf("Using data end: %s\n", format(data_end, "%Y-%m-%d %H:%M:%S")))
    } else {
      end_date <- parse_date_input(end_input, start_date)
    }
  } else if (is.null(end_date)) {
    # Non-interactive: use data end
    end_date <- data_end
  } else if (is.numeric(end_date)) {
    # Convert number of days to end date
    end_date <- start_date + end_date * 86400
  } else if (is.character(end_date)) {
    end_date <- as.POSIXct(end_date)
  }

  # Validate date range (with 1-second tolerance for boundary comparisons)
  tolerance <- 1  # 1 second tolerance
  if (as.numeric(start_date) < (as.numeric(data_start) - tolerance) ||
      as.numeric(start_date) > (as.numeric(data_end) + tolerance)) {
    stop(sprintf("Start date %s is outside data range [%s to %s]",
                 format(start_date, "%Y-%m-%d %H:%M:%S"),
                 format(data_start, "%Y-%m-%d %H:%M:%S"),
                 format(data_end, "%Y-%m-%d %H:%M:%S")))
  }

  if (as.numeric(end_date) < (as.numeric(data_start) - tolerance) ||
      as.numeric(end_date) > (as.numeric(data_end) + tolerance)) {
    stop(sprintf("End date %s is outside data range [%s to %s]",
                 format(end_date, "%Y-%m-%d %H:%M:%S"),
                 format(data_start, "%Y-%m-%d %H:%M:%S"),
                 format(data_end, "%Y-%m-%d %H:%M:%S")))
  }

  # Apply date range filter
  plot_data <- plot_data[
    plot_data$datetime >= start_date & plot_data$datetime <= end_date,
  ]

  if (nrow(plot_data) == 0) {
    stop("No data available for plotting after filtering")
  }

  # ============================================================================
  # AGGREGATION
  # ============================================================================

  # Apply aggregation if requested
  if (aggregation != "none") {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      warning("dplyr required for aggregation. Plotting without aggregation.")
    } else {
      if (aggregation == "hourly") {
        plot_data$time_bin <- lubridate::floor_date(plot_data$datetime, "hour")
      } else if (aggregation == "daily") {
        plot_data$time_bin <- lubridate::floor_date(plot_data$datetime, "day")
      } else {
        warning("Unknown aggregation type. Use 'hourly', 'daily', or 'none'. Using 'none'.")
        plot_data$time_bin <- plot_data$datetime
      }

      plot_data <- plot_data %>%
        dplyr::group_by(time_bin, series) %>%
        dplyr::summarise(
          datetime = mean(datetime),
          value = mean(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::select(datetime, value, series)
    }
  }

  # ============================================================================
  # CREATE PLOT
  # ============================================================================

  # Create line type and colour mappings
  # Primary: solid, distinct colour
  # Secondary original: dashed
  # Secondary calibrated: solid, same colour as original

  line_types <- c(
    setNames("solid", primary_method),
    setNames("dashed", paste0(secondary_method, " (original)")),
    setNames("solid", paste0(secondary_method, " (calibrated)"))
  )

  colours <- c(
    setNames("steelblue", primary_method),
    setNames("coral", paste0(secondary_method, " (original)")),
    setNames("coral", paste0(secondary_method, " (calibrated)"))
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = value,
                                                color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_linetype_manual(values = line_types) +
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::labs(
      title = paste("Method Calibration Comparison:", secondary_method, "→", primary_method),
      subtitle = paste0("Sensor: ", toupper(sensor_position), " | ",
                        "Threshold: ", calibration$threshold, " cm/hr | ",
                        "R² = ", round(calibration$r_squared, 4)),
      x = "Date/Time",
      y = "Velocity (cm/hr)",
      color = "Method",
      linetype = "Method"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10)
    )

  return(p)
}


#' Parse Date Input from User
#'
#' Helper function to parse user input for dates. Handles YYYY-MM-DD format
#' and numeric day numbers.
#'
#' @param input Character string input from user
#' @param reference_date Reference date (data start for start_date, start_date for end_date)
#'
#' @return POSIXct datetime
#'
#' @keywords internal
parse_date_input <- function(input, reference_date) {

  # Try to parse as number first (day number or number of days)
  numeric_input <- suppressWarnings(as.numeric(input))

  if (!is.na(numeric_input)) {
    # Numeric input: convert to date relative to reference
    result_date <- reference_date + (numeric_input - 1) * 86400
    return(result_date)
  }

  # Try to parse as date string
  tryCatch({
    result_date <- as.POSIXct(input)
    if (is.na(result_date)) {
      stop("Invalid date format")
    }
    return(result_date)
  }, error = function(e) {
    stop("Could not parse date input: ", input,
         "\nUse format YYYY-MM-DD or numeric day number")
  })
}


#' Plot sDMA Transition Zone
#'
#' Creates a diagnostic plot zooming in on the method transition zone to verify
#' smooth handover between primary and calibrated secondary methods. Visually
#' confirms there are no discontinuities ("steps") at the switching threshold.
#'
#' @param data Data frame containing sDMA results with columns: datetime, method,
#'   sensor_position, Vh_cm_hr (or velocity_col), Vh_sdma, sdma_source.
#' @param threshold Numeric. The velocity or Peclet threshold where switching occurs.
#' @param window_days Numeric. Number of days around the transition to display
#'   (default: 7). The plot will show data from threshold ± window_days.
#' @param sensor_position Character. Sensor position to plot ("outer" or "inner").
#'   Default: "outer".
#' @param primary_method Character. Name of primary method (default: "HRM").
#' @param secondary_method Character. Name of secondary method (e.g., "MHR").
#' @param velocity_col Character. Name of velocity column (default: "Vh_cm_hr").
#' @param mode Character. Switching mode: "velocity" or "peclet" (default: "velocity").
#' @param peclet_col Character. Name of Peclet column if mode = "peclet"
#'   (default: "hrm_peclet_number").
#'
#' @return A ggplot2 object showing:
#'   \itemize{
#'     \item Primary method values (fading out above threshold) - blue
#'     \item Calibrated secondary values (fading in above threshold) - red
#'     \item Combined sDMA velocity - green (should be continuous)
#'     \item Vertical dashed line at threshold
#'     \item Shaded region showing transition zone
#'   }
#'
#' @details
#' **Purpose:**
#'
#' This diagnostic plot addresses the "attenuated data" problem where secondary
#' methods produce lower values than primary even in valid ranges. Without
#' proper handover calibration, this creates visible discontinuities ("steps")
#' at the switching threshold.
#'
#' **What to Look For:**
#'
#' \itemize{
#'   \item **Good handover**: The green sDMA line should be continuous with no
#'         visible jump at the threshold
#'   \item **Smooth transition**: Primary (blue) and calibrated secondary (red)
#'         should converge at the threshold
#'   \item **No "volcano effect"**: Avoid sudden drops in velocity timeseries
#' }
#'
#' **Interpreting the Plot:**
#'
#' \itemize{
#'   \item Left of threshold: Blue line dominant (primary method used)
#'   \item Right of threshold: Red line dominant (calibrated secondary used)
#'   \item Green line (sDMA): Should show smooth transition through threshold
#'   \item Any gap in green line = discontinuity problem (poor calibration)
#' }
#'
#' @examples
#' \dontrun{
#' # After applying sDMA with calibration
#' vh_sdma <- apply_sdma_switching(
#'   data = vh_corrected,
#'   secondary = "MHR",
#'   mode = "velocity",
#'   threshold = 10,
#'   calibrate = TRUE
#' )
#'
#' # Check transition zone
#' plot_sdma_transition(
#'   data = vh_sdma,
#'   threshold = 10,
#'   window_days = 7,
#'   secondary_method = "MHR"
#' )
#' }
#'
#' @family calibration plotting functions
#' @export
plot_sdma_transition <- function(data,
                                  threshold,
                                  window_days = 7,
                                  sensor_position = "outer",
                                  primary_method = "HRM",
                                  secondary_method,
                                  velocity_col = "Vh_cm_hr",
                                  mode = c("velocity", "peclet"),
                                  peclet_col = "hrm_peclet_number") {

  mode <- match.arg(mode)

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting. Install with: install.packages('ggplot2')")
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("datetime", "method", "sensor_position", velocity_col)
  if ("Vh_sdma" %in% names(data)) {
    required_cols <- c(required_cols, "Vh_sdma", "sdma_source")
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter to sensor position
  plot_data <- data[data$sensor_position == sensor_position, ]

  if (nrow(plot_data) == 0) {
    stop("No data found for sensor_position = '", sensor_position, "'")
  }

  # Determine switch variable (velocity or Peclet)
  if (mode == "peclet") {
    if (!peclet_col %in% names(plot_data)) {
      stop("Peclet column '", peclet_col, "' not found in data")
    }
    switch_var <- peclet_col
    switch_label <- "Peclet Number"
  } else {
    switch_var <- velocity_col
    switch_label <- "Velocity (cm/hr)"
  }

  # Find data around the threshold
  # Get primary method data to identify when we're near threshold
  primary_data <- plot_data[plot_data$method == primary_method, ]

  if (nrow(primary_data) == 0) {
    stop("No primary method data found for '", primary_method, "'")
  }

  # Calculate velocity buffer around threshold for temporal windowing
  # This ensures we capture transitions even if they're spread over time
  threshold_window <- threshold * 0.2  # ±20% of threshold

  # Find times when we're near the threshold
  near_threshold <- primary_data[[switch_var]] >= (threshold - threshold_window) &
                    primary_data[[switch_var]] <= (threshold + threshold_window)

  if (!any(near_threshold, na.rm = TRUE)) {
    warning("No data found near threshold (", threshold, " ± ", round(threshold_window, 2), ")")
    # Fall back to showing all data
    transition_times <- primary_data$datetime
  } else {
    transition_times <- primary_data$datetime[near_threshold]
  }

  # Define temporal window around transitions
  if (length(transition_times) > 0) {
    min_time <- min(transition_times, na.rm = TRUE) - (window_days * 86400 / 2)
    max_time <- max(transition_times, na.rm = TRUE) + (window_days * 86400 / 2)

    plot_data <- plot_data[
      plot_data$datetime >= min_time & plot_data$datetime <= max_time,
    ]
  }

  if (nrow(plot_data) == 0) {
    stop("No data in transition window. Try increasing window_days.")
  }

  # Prepare data for plotting
  # Extract primary and secondary values
  primary_vals <- plot_data[
    plot_data$method == primary_method,
    c("datetime", velocity_col, switch_var)
  ]
  names(primary_vals) <- c("datetime", "velocity", "switch_val")
  primary_vals$series <- paste0(primary_method, " (primary)")

  secondary_vals <- plot_data[
    plot_data$method == secondary_method,
    c("datetime", velocity_col)
  ]
  names(secondary_vals) <- c("datetime", "velocity")
  secondary_vals$series <- paste0(secondary_method, " (calibrated)")
  secondary_vals$switch_val <- NA_real_

  # Get sDMA combined values if available
  if ("Vh_sdma" %in% names(plot_data)) {
    sdma_vals <- plot_data[
      plot_data$method == primary_method & !is.na(plot_data$Vh_sdma),
      c("datetime", "Vh_sdma", switch_var)
    ]
    names(sdma_vals) <- c("datetime", "velocity", "switch_val")
    sdma_vals$series <- "sDMA (combined)"

    # Combine all series
    plot_df <- rbind(primary_vals, secondary_vals, sdma_vals)
  } else {
    # No sDMA column, just show primary and secondary
    plot_df <- rbind(primary_vals, secondary_vals)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = datetime, y = velocity, color = series)) +
    # Shaded transition zone
    ggplot2::annotate("rect",
                      xmin = -Inf, xmax = Inf,
                      ymin = threshold - threshold_window,
                      ymax = threshold + threshold_window,
                      alpha = 0.1, fill = "grey50") +

    # Data lines
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
    ggplot2::geom_point(size = 1.5, alpha = 0.5) +

    # Threshold line
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed",
                        color = "black", linewidth = 1) +

    # Threshold annotation
    ggplot2::annotate("text",
                      x = min(plot_df$datetime, na.rm = TRUE),
                      y = threshold,
                      label = paste0("Threshold: ", threshold, if (mode == "velocity") " cm/hr" else ""),
                      hjust = 0, vjust = -0.5, color = "black", size = 3.5) +

    # Colors
    ggplot2::scale_color_manual(
      values = c(
        "HRM (primary)" = "steelblue",
        "MHR (calibrated)" = "coral",
        "sDMA (combined)" = "darkgreen"
      )
    ) +

    # Labels and theme
    ggplot2::labs(
      title = paste("sDMA Transition Zone:", primary_method, "↔", secondary_method),
      subtitle = paste0("Threshold: ", threshold, if (mode == "velocity") " cm/hr" else "",
                        " | Window: ±", window_days, " days | ",
                        "Check for discontinuities at threshold"),
      x = "Date/Time",
      y = "Velocity (cm/hr)",
      color = "Method"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "bottom",
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}
