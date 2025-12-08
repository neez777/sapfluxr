# ============================================================================
# 04g_sap_flux_plots.R
# ============================================================================
# Plotting functions for sap flux analysis
#
# This module provides visualisation functions for sap flux data:
# - Timeseries plots (hourly and daily)
# - Diurnal patterns
# - Sap flux as proportion of maximum
# - Environmental response curves
# - Seasonal trends
# ============================================================================

#' Plot sap flux timeseries
#'
#' Creates a timeseries plot of sap flux metrics (total flux, flux density,
#' or normalised values) with optional environmental overlays.
#'
#' @param data Data frame containing sap flux data with datetime column.
#' @param y_col Character. Name of column to plot on y-axis. Default: "Q_cm3_hr".
#' @param datetime_col Character. Name of column containing datetime. Default: "datetime".
#' @param y_label Character. Y-axis label. If NULL, will be auto-generated from
#'   y_col. Default: NULL.
#' @param title Character. Plot title. Default: "Sap Flux Timeseries".
#' @param color Character. Line color. Default: "steelblue".
#' @param add_points Logical. Add points to line plot. Default: FALSE.
#' @param smooth Logical. Add smoothed trend line (LOESS). Default: FALSE.
#' @param smooth_span Numeric. Span for LOESS smoothing (0-1). Default: 0.1.
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' # Basic timeseries
#' plot_sap_flux_timeseries(flux_data)
#'
#' # Flux density
#' plot_sap_flux_timeseries(
#'   flux_data,
#'   y_col = "Qps_cm_hr",
#'   y_label = "Flux Density (cm/hr)"
#' )
#'
#' # With smoothing
#' plot_sap_flux_timeseries(
#'   flux_data,
#'   smooth = TRUE,
#'   smooth_span = 0.2
#' )
#' }
#'
#' @family sap flux plots
#' @export
plot_sap_flux_timeseries <- function(data, y_col = "Q_cm3_hr",
                                      datetime_col = "datetime",
                                      y_label = NULL,
                                      title = "Sap Flux Timeseries",
                                      color = "steelblue",
                                      add_points = FALSE,
                                      smooth = FALSE,
                                      smooth_span = 0.1) {
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.\n",
         "  Install with: install.packages('ggplot2')")
  }

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!datetime_col %in% names(data)) {
    stop("Column '", datetime_col, "' not found in data")
  }

  if (!y_col %in% names(data)) {
    stop("Column '", y_col, "' not found in data")
  }

  # Auto-generate y-axis label if not provided
  if (is.null(y_label)) {
    y_label <- auto_label(y_col)
  }

  # Create plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[datetime_col]],
                                           y = .data[[y_col]])) +
    ggplot2::geom_line(color = color, linewidth = 0.6) +
    ggplot2::labs(
      title = title,
      x = "Date/Time",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Add points if requested
  if (add_points) {
    p <- p + ggplot2::geom_point(color = color, size = 1, alpha = 0.5)
  }

  # Add smoothing if requested
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess", span = smooth_span,
                                   se = TRUE, color = "red", linewidth = 0.8)
  }

  return(p)
}


#' Plot sap flux as proportion of maximum
#'
#' Creates a timeseries plot showing sap flux as a proportion (0-1) of the
#' maximum value, similar to Tim's SF/Max_Qp plot. This is useful for comparing
#' relative temporal patterns across trees or periods.
#'
#' @param data Data frame containing sap flux data.
#' @param flux_col Character. Name of column containing flux values (e.g.,
#'   "Q_cm3_hr", "Qps_cm_hr"). Default: "Q_cm3_hr".
#' @param datetime_col Character. Name of datetime column. Default: "datetime".
#' @param max_value Numeric. Maximum value to use for normalisation. If NULL,
#'   uses max from data. Default: NULL.
#' @param title Character. Plot title. Default: "Sap Flux as Proportion of Maximum".
#' @param color Character. Line color. Default: "darkgreen".
#' @param add_reference_lines Logical. Add horizontal lines at 0.25, 0.5, 0.75.
#'   Default: TRUE.
#'
#' @return ggplot object
#'
#' @details
#' This function calculates SF/Max_Qp for each measurement:
#'   proportion = flux / max_flux
#'
#' Where max_flux is either provided or calculated from the data. The resulting
#' plot shows values from 0 (no flux) to 1.0 (maximum flux).
#'
#' This is particularly useful for:
#' - Comparing relative flux patterns across trees of different sizes
#' - Identifying consistent diurnal patterns
#' - Visualising response to environmental changes
#'
#' @examples
#' \dontrun{
#' # Basic proportion plot
#' plot_flux_proportion(flux_data)
#'
#' # With custom maximum (e.g., from calibration period)
#' plot_flux_proportion(
#'   flux_data,
#'   max_value = 2500
#' )
#'
#' # Using flux density instead of total flux
#' plot_flux_proportion(
#'   flux_data,
#'   flux_col = "Qps_cm_hr"
#' )
#' }
#'
#' @family sap flux plots
#' @export
plot_flux_proportion <- function(data, flux_col = "Q_cm3_hr",
                                  datetime_col = "datetime",
                                  max_value = NULL,
                                  title = "Sap Flux as Proportion of Maximum",
                                  color = "darkgreen",
                                  add_reference_lines = TRUE) {
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.\n",
         "  Install with: install.packages('ggplot2')")
  }

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!datetime_col %in% names(data)) {
    stop("Column '", datetime_col, "' not found in data")
  }

  if (!flux_col %in% names(data)) {
    stop("Column '", flux_col, "' not found in data")
  }

  # Calculate maximum
  if (is.null(max_value)) {
    max_value <- max(data[[flux_col]], na.rm = TRUE)
  }

  # Calculate proportion
  data$proportion <- data[[flux_col]] / max_value

  # Create plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[datetime_col]],
                                           y = .data$proportion)) +
    ggplot2::geom_line(color = color, linewidth = 0.7) +
    ggplot2::labs(
      title = title,
      x = "Date/Time",
      y = "Proportion of Maximum Flux (SF/Max)"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                 breaks = seq(0, 1, 0.25),
                                 labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Add reference lines
  if (add_reference_lines) {
    p <- p + ggplot2::geom_hline(yintercept = c(0.25, 0.5, 0.75),
                                  linetype = "dashed", color = "grey60",
                                  alpha = 0.5)
  }

  return(p)
}


#' Plot diurnal pattern of sap flux
#'
#' Creates a plot showing the average diurnal (24-hour) pattern of sap flux,
#' with optional confidence intervals.
#'
#' @param data Data frame containing sap flux data with datetime stamps.
#' @param y_col Character. Name of column to plot. Default: "Qps_cm_hr".
#' @param datetime_col Character. Name of datetime column. Default: "datetime".
#' @param stat Character. Statistic to plot: "mean" or "median". Default: "mean".
#' @param add_ci Logical. Add confidence interval (95% for mean, IQR for median).
#'   Default: TRUE.
#' @param y_label Character. Y-axis label. If NULL, auto-generated. Default: NULL.
#' @param title Character. Plot title. Default: "Diurnal Sap Flux Pattern".
#' @param color Character. Line color. Default: "steelblue".
#'
#' @return ggplot object
#'
#' @details
#' This function extracts the hour-of-day from each measurement, groups by hour,
#' and calculates the mean or median flux for each hour. This reveals the
#' typical diurnal pattern averaged across all days in the dataset.
#'
#' Confidence intervals:
#' - If stat = "mean": 95% confidence interval (mean ± 1.96 × SE)
#' - If stat = "median": Interquartile range (25th-75th percentile)
#'
#' @examples
#' \dontrun{
#' # Basic diurnal pattern
#' plot_diurnal_pattern(flux_data)
#'
#' # Using median instead of mean
#' plot_diurnal_pattern(
#'   flux_data,
#'   stat = "median"
#' )
#'
#' # Total flux instead of flux density
#' plot_diurnal_pattern(
#'   flux_data,
#'   y_col = "Q_cm3_hr",
#'   y_label = "Total Flux (cm³/hr)"
#' )
#' }
#'
#' @family sap flux plots
#' @export
plot_diurnal_pattern <- function(data, y_col = "Qps_cm_hr",
                                  datetime_col = "datetime",
                                  stat = "mean",
                                  add_ci = TRUE,
                                  y_label = NULL,
                                  title = "Diurnal Sap Flux Pattern",
                                  color = "steelblue") {
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.\n",
         "  Install with: install.packages('ggplot2')")
  }

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!datetime_col %in% names(data)) {
    stop("Column '", datetime_col, "' not found in data")
  }

  if (!y_col %in% names(data)) {
    stop("Column '", y_col, "' not found in data")
  }

  if (!stat %in% c("mean", "median")) {
    stop("stat must be 'mean' or 'median'")
  }

  # Auto-generate y-axis label
  if (is.null(y_label)) {
    y_label <- auto_label(y_col)
  }

  # Extract hour of day
  data$hour <- as.numeric(format(data[[datetime_col]], "%H"))

  # Calculate diurnal statistics
  diurnal_stats <- data.frame()

  for (h in 0:23) {
    hour_data <- data[data$hour == h, y_col]
    hour_data <- hour_data[!is.na(hour_data)]

    if (length(hour_data) == 0) {
      next
    }

    if (stat == "mean") {
      center <- mean(hour_data)
      se <- sd(hour_data) / sqrt(length(hour_data))
      lower <- center - 1.96 * se
      upper <- center + 1.96 * se
    } else {
      center <- median(hour_data)
      lower <- quantile(hour_data, 0.25)
      upper <- quantile(hour_data, 0.75)
    }

    diurnal_stats <- rbind(diurnal_stats, data.frame(
      hour = h,
      value = center,
      lower = lower,
      upper = upper
    ))
  }

  # Create plot
  p <- ggplot2::ggplot(diurnal_stats, ggplot2::aes(x = hour, y = value)) +
    ggplot2::geom_line(color = color, linewidth = 1) +
    ggplot2::geom_point(color = color, size = 2) +
    ggplot2::labs(
      title = title,
      x = "Hour of Day",
      y = y_label
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, 4)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Add confidence interval
  if (add_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                   alpha = 0.3, fill = color)
  }

  return(p)
}


#' Plot daily sap flux timeseries
#'
#' Creates a timeseries plot of daily aggregated sap flux data.
#'
#' @param data Data frame containing daily sap flux data (from aggregate_daily()).
#' @param y_col Character. Name of column to plot. Default: "Qp_daily_L_day".
#' @param date_col Character. Name of date column. Default: "date".
#' @param y_label Character. Y-axis label. If NULL, auto-generated. Default: NULL.
#' @param title Character. Plot title. Default: "Daily Total Sap Flux".
#' @param color Character. Point/line color. Default: "darkblue".
#' @param show_points Logical. Show points. Default: TRUE.
#' @param show_line Logical. Show connecting line. Default: TRUE.
#' @param add_trend Logical. Add linear trend line. Default: FALSE.
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' # Basic daily plot
#' daily_data <- aggregate_daily(hourly_data)
#' plot_daily_timeseries(daily_data)
#'
#' # Daily flux density
#' plot_daily_timeseries(
#'   daily_data,
#'   y_col = "Jvm_daily_mm_day",
#'   y_label = "Daily Flux Density (mm/day)"
#' )
#'
#' # With trend line
#' plot_daily_timeseries(
#'   daily_data,
#'   add_trend = TRUE
#' )
#' }
#'
#' @family sap flux plots
#' @export
plot_daily_timeseries <- function(data, y_col = "Qp_daily_L_day",
                                   date_col = "date",
                                   y_label = NULL,
                                   title = "Daily Total Sap Flux",
                                   color = "darkblue",
                                   show_points = TRUE,
                                   show_line = TRUE,
                                   add_trend = FALSE) {
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.\n",
         "  Install with: install.packages('ggplot2')")
  }

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!date_col %in% names(data)) {
    stop("Column '", date_col, "' not found in data")
  }

  if (!y_col %in% names(data)) {
    stop("Column '", y_col, "' not found in data")
  }

  # Auto-generate y-axis label
  if (is.null(y_label)) {
    y_label <- auto_label(y_col)
  }

  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[date_col]],
                                           y = .data[[y_col]]))

  # Add line
  if (show_line) {
    p <- p + ggplot2::geom_line(color = color, linewidth = 0.8)
  }

  # Add points
  if (show_points) {
    p <- p + ggplot2::geom_point(color = color, size = 2)
  }

  # Add trend line
  if (add_trend) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE,
                                   color = "red", linewidth = 0.8)
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      title = title,
      x = "Date",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}


# ============================================================================
# Internal helper functions
# ============================================================================

#' Auto-generate axis label from column name
#'
#' @param col_name Character. Column name
#' @return Character. Formatted label
#' @keywords internal
auto_label <- function(col_name) {
  # Common column name patterns
  labels <- list(
    "Q_cm3_hr" = "Total Sap Flux (cm³/hr)",
    "Q_L_hr" = "Total Sap Flux (L/hr)",
    "Q_L_day" = "Total Sap Flux (L/day)",
    "Qps_cm_hr" = "Flux Density (cm/hr)",
    "Qps_cm3_hr_cm2" = "Flux Density (cm³/hr/cm²)",
    "Qpsn" = "Normalised Flux Density",
    "Qpl_cm3_hr_m2" = "Leaf-Area-Specific Flux (cm³/hr/m²)",
    "Jvm_daily_cm3_cm2_day" = "Daily Flux Density (cm³/cm²/day)",
    "Jvm_daily_mm_day" = "Daily Flux Density (mm/day)",
    "Qp_daily_L_day" = "Daily Total Flux (L/day)",
    "Qp_daily_cm3_day" = "Daily Total Flux (cm³/day)"
  )

  # Return matching label or formatted column name
  if (col_name %in% names(labels)) {
    return(labels[[col_name]])
  } else {
    # Format column name (replace _ with space, capitalize)
    return(gsub("_", " ", col_name))
  }
}
