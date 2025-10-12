# =============================================================================
# FILE: R/23_plotting_functions.R
# Comprehensive ggplot2 Plotting Functions for sapFluxR Package
# =============================================================================

#' @importFrom dplyr n
NULL

#' Publication-Ready Theme for sapFluxR Plots
#'
#' Creates a clean, publication-ready theme for all sapFluxR plots with consistent
#' styling suitable for scientific publications and presentations.
#'
#' @param base_size Base font size for text elements (default: 12)
#' @param base_family Font family to use (default: "")
#' @param grid_major Show major grid lines (default: TRUE)
#' @param grid_minor Show minor grid lines (default: FALSE)
#' @param axis_text_angle Angle for x-axis text (default: 0)
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_sapfluxr()
#' }
#'
#' @export
theme_sapfluxr <- function(base_size = 12, base_family = "",
                           grid_major = TRUE, grid_minor = FALSE,
                           axis_text_angle = 0) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for sapFluxR plotting functions. ",
         "Please install it with install.packages('ggplot2')")
  }

  # Base theme
  theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot styling
      plot.title = ggplot2::element_text(size = base_size + 2, face = "bold",
                                         hjust = 0.5, margin = ggplot2::margin(b = 20)),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0.5,
                                            colour = "grey40", margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(size = base_size - 2, colour = "grey50",
                                           hjust = 1, margin = ggplot2::margin(t = 15)),

      # Axis styling
      axis.title = ggplot2::element_text(size = base_size + 1, colour = "black"),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "grey20"),
      axis.text.x = ggplot2::element_text(angle = axis_text_angle, hjust = ifelse(axis_text_angle != 0, 1, 0.5)),
      axis.line = ggplot2::element_line(colour = "black", size = 0.5),
      axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),

      # Panel styling
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.8),

      # Legend styling
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = "grey80", size = 0.5),
      legend.margin = ggplot2::margin(6, 6, 6, 6),

      # Strip text for facets
      strip.text = ggplot2::element_text(size = base_size, face = "bold", colour = "black"),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = "black", size = 0.5),

      # Remove clutter
      panel.grid.major = if (grid_major) ggplot2::element_line(colour = "grey90", size = 0.3) else ggplot2::element_blank(),
      panel.grid.minor = if (grid_minor) ggplot2::element_line(colour = "grey95", size = 0.2) else ggplot2::element_blank()
    )

  return(theme)
}

#' Get Colour Palette for sapFluxR Methods
#'
#' Returns a consistent colour palette for different HPV calculation methods
#'
#' @param methods Character vector of method names
#' @param palette Character string specifying palette type: "default", "colourblind", "print" (default: "default")
#'
#' @return Named vector of colours
#' @export
get_method_colours <- function(methods = NULL, palette = "default") {

  # Define colour palettes for different methods
  method_colours <- list(
    default = c(
      "HRM" = "#E31A1C",      # Red
      "MHR" = "#1F78B4",      # Blue
      "HRMXa" = "#33A02C",    # Green
      "HRMXb" = "#FF7F00",    # Orange
      "Tmax_COH" = "#6A3D9A", # Purple
      "Tmax_KLU" = "#A6CEE3", # Light blue
      "CHPM" = "#FB9A99",     # Pink
      "DRM" = "#B2DF8A",      # Light green
      "DMA" = "#FDBF6F",      # Light orange
      "HRMX" = "#CAB2D6"      # Light purple
    ),

    colourblind = c(
      "HRM" = "#D55E00",      # Orange-red
      "MHR" = "#0173B2",      # Blue
      "HRMXa" = "#029E73",    # Green
      "HRMXb" = "#CC78BC",    # Pink
      "Tmax_COH" = "#ECE133", # Yellow
      "Tmax_KLU" = "#56B4E9", # Sky blue
      "CHPM" = "#F0E442",     # Yellow
      "DRM" = "#009E73",      # Teal
      "DMA" = "#E69F00",      # Orange
      "HRMX" = "#999999"      # Grey
    ),

    print = c(
      "HRM" = "#000000",      # Black
      "MHR" = "#404040",      # Dark grey
      "HRMXa" = "#808080",    # Medium grey
      "HRMXb" = "#A0A0A0",    # Light grey
      "Tmax_COH" = "#C0C0C0", # Very light grey
      "Tmax_KLU" = "#000000", # Black (dashed)
      "CHPM" = "#404040",     # Dark grey (dotted)
      "DRM" = "#808080",      # Medium grey (dotdash)
      "DMA" = "#A0A0A0",      # Light grey (longdash)
      "HRMX" = "#C0C0C0"      # Very light grey (twodash)
    )
  )

  colours <- method_colours[[palette]]

  if (is.null(methods)) {
    return(colours)
  }

  # Return colours for requested methods, use default colours for unknown methods
  result <- colours[methods]
  unknown <- is.na(result)
  if (any(unknown)) {
    n_unknown <- sum(unknown)
    default_colours <- rainbow(n_unknown)
    result[unknown] <- default_colours
    names(result)[unknown] <- methods[unknown]
  }

  return(result)
}

#' Plot Raw Temperature Data
#'
#' Create comprehensive plots of raw temperature measurements from heat pulse sensors
#' with multiple visualization options and publication-ready formatting.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param pulse_ids Optional vector of pulse IDs to plot. If NULL, plots all pulses (default: NULL)
#' @param plot_type Character string specifying plot type: "timeseries", "pulse_overlay", "temperature_differences", "diagnostic" (default: "timeseries")
#' @param sensors Character vector specifying which sensors to plot: "all", "outer", "inner", c("do", "di", "uo", "ui") (default: "all")
#' @param time_window Optional time window to plot. Vector of 2 POSIXct objects (default: NULL)
#' @param facet_by Character string for faceting: "pulse", "sensor", "none" (default: "none")
#' @param smooth_data Logical whether to add smoothed trend lines (default: FALSE)
#' @param interactive Logical whether to create interactive plot (requires plotly) (default: FALSE)
#' @param theme_options List of theme options to override defaults (default: NULL)
#'
#' @return A ggplot2 object (or plotly if interactive = TRUE)
#'
#' @examples
#' \dontrun{
#' # Basic time series plot
#' plot_temperature_data(sap_data)
#'
#' # Pulse overlay comparison
#' plot_temperature_data(sap_data, plot_type = "pulse_overlay", pulse_ids = 1:5)
#'
#' # Temperature differences diagnostic
#' plot_temperature_data(sap_data, plot_type = "temperature_differences")
#'
#' # Interactive time series
#' plot_temperature_data(sap_data, interactive = TRUE)
#' }
#'
#' @export
plot_temperature_data <- function(sap_data, pulse_ids = NULL, plot_type = "timeseries",
                                  sensors = "all", time_window = NULL, facet_by = "none",
                                  smooth_data = FALSE, interactive = FALSE, theme_options = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting functions. Please install it with install.packages('ggplot2')")
  }

  # Validate inputs
  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object from read_sap_data()")
  }

  plot_type <- match.arg(plot_type, c("timeseries", "pulse_overlay", "temperature_differences", "diagnostic"))
  facet_by <- match.arg(facet_by, c("pulse", "sensor", "none"))

  # Extract measurements
  measurements <- sap_data$measurements

  if (is.null(measurements) || nrow(measurements) == 0) {
    stop("No measurement data found in sap_data object")
  }

  # Filter by pulse IDs if specified
  if (!is.null(pulse_ids)) {
    measurements <- measurements[measurements$pulse_id %in% pulse_ids, ]
    if (nrow(measurements) == 0) {
      stop("No data found for specified pulse IDs")
    }
  }

  # Filter by time window if specified
  if (!is.null(time_window)) {
    measurements <- measurements[measurements$datetime >= time_window[1] &
                                   measurements$datetime <= time_window[2], ]
    if (nrow(measurements) == 0) {
      stop("No data found in specified time window")
    }
  }

  # Prepare data based on plot type
  if (plot_type == "temperature_differences") {
    # Calculate pre-pulse baselines and temperature differences
    temp_diff_data <- calculate_temperature_differences(measurements)
    plot_data <- temp_diff_data
    y_var <- "delta_temp"
    y_label <- expression(paste(Delta, "T (°C)"))
  } else {
    # Reshape data for temperature plotting
    plot_data <- reshape_temperature_data(measurements, sensors)
    y_var <- "temperature"
    y_label <- "Temperature (°C)"
  }

  # Create base plot
  if (plot_type == "pulse_overlay") {
    # Create relative time since pulse start
    plot_data <- add_relative_time(plot_data)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = relative_time, y = get(y_var),
                                                 colour = sensor, group = interaction(pulse_id, sensor)))
    x_label <- "Time since pulse start (seconds)"
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = get(y_var), colour = sensor))
    x_label <- "Date/Time"
  }

  # Add geoms based on plot type
  if (plot_type %in% c("timeseries", "temperature_differences")) {
    p <- p + ggplot2::geom_line(alpha = 0.8, size = 0.7)
    if (plot_type == "timeseries") {
      p <- p + ggplot2::geom_point(alpha = 0.6, size = 0.5)
    }
  } else if (plot_type == "pulse_overlay") {
    p <- p + ggplot2::geom_line(alpha = 0.7, size = 0.5)
  } else if (plot_type == "diagnostic") {
    p <- p +
      ggplot2::geom_line(alpha = 0.7, size = 0.5) +
      ggplot2::geom_point(alpha = 0.5, size = 0.3)
  }

  # Add smoothed trends if requested
  if (smooth_data) {
    p <- p + ggplot2::geom_smooth(se = TRUE, alpha = 0.3, size = 1)
  }

  # Add colours
  sensor_colours <- c("do" = "#E31A1C", "di" = "#1F78B4", "uo" = "#33A02C", "ui" = "#FF7F00")
  p <- p + ggplot2::scale_colour_manual(values = sensor_colours, name = "Sensor")

  # Add faceting if requested
  if (facet_by == "pulse" && length(unique(plot_data$pulse_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~pulse_id, scales = "free_x", labeller = ggplot2::label_both)
  } else if (facet_by == "sensor") {
    p <- p + ggplot2::facet_wrap(~sensor, scales = "free_y", labeller = ggplot2::label_both)
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      title = create_temperature_plot_title(plot_type, sensors),
      subtitle = create_temperature_plot_subtitle(sap_data, pulse_ids, time_window),
      x = x_label,
      y = y_label,
      caption = "Generated by sapFluxR"
    ) +
    theme_sapfluxr(grid_major = TRUE, grid_minor = FALSE)

  # Apply theme options if provided
  if (!is.null(theme_options)) {
    p <- p + do.call(ggplot2::theme, theme_options)
  }

  # Convert to interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("plotly package not available. Returning static ggplot.")
    } else {
      p <- plotly::ggplotly(p)
    }
  }

  return(p)
}

#' Plot Heat Pulse Velocity Time Series
#'
#' Create comprehensive time series plots of calculated heat pulse velocities
#' with multiple display options and statistical overlays.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param methods Character vector of methods to plot. If NULL, plots all available methods (default: NULL)
#' @param sensors Character vector of sensor positions to plot: "all", "outer", "inner" (default: "all")
#' @param quality_filter Logical whether to filter by quality flags (default: TRUE)
#' @param quality_flags Character vector of quality flags to include if quality_filter = TRUE (default: c("OK"))
#' @param plot_type Character string: "points", "lines", "both", "boxplot_daily", "violin_daily" (default: "both")
#' @param time_aggregation Character string: "none", "hourly", "daily", "weekly" (default: "none")
#' @param add_statistics Logical whether to add statistical overlays (mean, median, trends) (default: FALSE)
#' @param facet_by Character string: "method", "sensor", "both", "none" (default: "method")
#' @param colour_by Character string: "method", "sensor", "quality" (default: "method")
#' @param time_window Optional time window as vector of 2 POSIXct objects (default: NULL)
#' @param y_limits Optional y-axis limits as numeric vector of length 2 (default: NULL)
#' @param interactive Logical whether to create interactive plot (requires plotly) (default: FALSE)
#' @param theme_options List of theme options to override defaults (default: NULL)
#'
#' @return A ggplot2 object (or plotly if interactive = TRUE)
#'
#' @examples
#' \dontrun{
#' # Basic time series
#' plot_hpv_timeseries(vh_results)
#'
#' # Method comparison with statistics
#' plot_hpv_timeseries(vh_results, add_statistics = TRUE, facet_by = "none")
#'
#' # Daily aggregation with violin plots
#' plot_hpv_timeseries(vh_results, plot_type = "violin_daily", time_aggregation = "daily")
#'
#' # Interactive plot
#' plot_hpv_timeseries(vh_results, interactive = TRUE)
#' }
#'
#' @export
plot_hpv_timeseries <- function(vh_results, methods = NULL, sensors = "all",
                                quality_filter = TRUE, quality_flags = c("OK"),
                                plot_type = "both", time_aggregation = "none",
                                add_statistics = FALSE, facet_by = "method",
                                colour_by = "method", time_window = NULL,
                                y_limits = NULL, interactive = FALSE, theme_options = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting functions. Please install it with install.packages('ggplot2')")
  }

  # Validate inputs
  if (!inherits(vh_results, "vh_results") && !is.data.frame(vh_results)) {
    stop("vh_results must be a vh_results object or data frame with required columns")
  }

  # Convert to data frame if needed
  if (!is.data.frame(vh_results)) {
    vh_results <- as.data.frame(vh_results)
  }

  # Check required columns
  required_cols <- c("datetime", "method", "Vh_cm_hr")
  missing_cols <- setdiff(required_cols, names(vh_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  plot_type <- match.arg(plot_type, c("points", "lines", "both", "boxplot_daily", "violin_daily"))
  time_aggregation <- match.arg(time_aggregation, c("none", "hourly", "daily", "weekly"))
  facet_by <- match.arg(facet_by, c("method", "sensor", "both", "none"))
  colour_by <- match.arg(colour_by, c("method", "sensor", "quality"))

  # Filter data
  plot_data <- filter_velocity_data(vh_results, methods, sensors, quality_filter, quality_flags, time_window)

  if (nrow(plot_data) == 0) {
    stop("No data remaining after filtering")
  }

  # Apply time aggregation if requested
  if (time_aggregation != "none") {
    plot_data <- aggregate_for_plotting(plot_data, time_aggregation)
  }

  # Create base plot
  p <- ggplot2::ggplot(plot_data, create_hpv_aes(colour_by, plot_type, time_aggregation))

  # Add geoms based on plot type
  p <- add_hpv_geoms(p, plot_type, time_aggregation)

  # Add colours
  p <- add_hpv_colours(p, colour_by, plot_data)

  # Add statistical overlays if requested
  if (add_statistics && time_aggregation == "none") {
    p <- add_statistical_overlays(p, plot_data)
  }

  # Add faceting
  p <- add_hpv_faceting(p, facet_by, plot_data)

  # Set y-axis limits if specified
  if (!is.null(y_limits)) {
    p <- p + ggplot2::ylim(y_limits)
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      title = create_hpv_plot_title(methods, sensors, plot_type, time_aggregation),
      subtitle = create_hpv_plot_subtitle(plot_data, quality_filter),
      x = ifelse(time_aggregation == "none", "Date/Time", paste(stringr::str_to_title(time_aggregation), "Period")),
      y = "Heat Pulse Velocity (cm/hr)",
      caption = "Generated by sapFluxR"
    ) +
    theme_sapfluxr(grid_major = TRUE, grid_minor = FALSE)

  # Apply theme options if provided
  if (!is.null(theme_options)) {
    p <- p + do.call(ggplot2::theme, theme_options)
  }

  # Convert to interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("plotly package not available. Returning static ggplot.")
    } else {
      p <- plotly::ggplotly(p)
    }
  }

  return(p)
}

#' Plot Method Comparison
#'
#' Create comprehensive comparison plots between different HPV calculation methods
#' including scatter plots, Bland-Altman plots, and method performance analysis.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param comparison_type Character string: "scatter", "bland_altman", "correlation", "performance", "agreement" (default: "scatter")
#' @param reference_method Character string specifying reference method for comparison (default: "HRM")
#' @param methods Character vector of methods to compare. If NULL, uses all available methods (default: NULL)
#' @param sensors Character vector of sensor positions: "all", "outer", "inner" (default: "all")
#' @param quality_filter Logical whether to filter by quality flags (default: TRUE)
#' @param quality_flags Character vector of quality flags to include (default: c("OK"))
#' @param add_statistics Logical whether to add correlation/agreement statistics (default: TRUE)
#' @param confidence_level Numeric confidence level for agreement limits (default: 0.95)
#' @param outlier_detection Logical whether to identify and highlight outliers (default: TRUE)
#' @param facet_by Character string: "sensor", "none" (default: "none")
#' @param interactive Logical whether to create interactive plot (default: FALSE)
#' @param theme_options List of theme options to override defaults (default: NULL)
#'
#' @return A ggplot2 object (or plotly if interactive = TRUE)
#'
#' @examples
#' \dontrun{
#' # Basic scatter plot comparison
#' plot_method_comparison(vh_results)
#'
#' # Bland-Altman plot
#' plot_method_comparison(vh_results, comparison_type = "bland_altman")
#'
#' # Correlation matrix
#' plot_method_comparison(vh_results, comparison_type = "correlation")
#'
#' # Interactive performance comparison
#' plot_method_comparison(vh_results, comparison_type = "performance", interactive = TRUE)
#' }
#'
#' @export
plot_method_comparison <- function(vh_results, comparison_type = "scatter",
                                   reference_method = "HRM", methods = NULL,
                                   sensors = "all", quality_filter = TRUE,
                                   quality_flags = c("OK"), add_statistics = TRUE,
                                   confidence_level = 0.95, outlier_detection = TRUE,
                                   facet_by = "none", interactive = FALSE, theme_options = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting functions. Please install it with install.packages('ggplot2')")
  }

  comparison_type <- match.arg(comparison_type, c("scatter", "bland_altman", "correlation", "performance", "agreement"))
  facet_by <- match.arg(facet_by, c("sensor", "none"))

  # Validate and prepare data
  plot_data <- prepare_method_comparison_data(vh_results, reference_method, methods, sensors,
                                              quality_filter, quality_flags)

  if (nrow(plot_data) == 0) {
    stop("Insufficient data for method comparison after filtering")
  }

  # Create plot based on comparison type
  if (comparison_type == "scatter") {
    p <- create_method_scatter_plot(plot_data, reference_method, add_statistics, outlier_detection)
  } else if (comparison_type == "bland_altman") {
    p <- create_bland_altman_plot(plot_data, reference_method, confidence_level, add_statistics)
  } else if (comparison_type == "correlation") {
    p <- create_correlation_matrix_plot(plot_data)
  } else if (comparison_type == "performance") {
    p <- create_method_performance_plot(plot_data, reference_method)
  } else if (comparison_type == "agreement") {
    p <- create_method_agreement_plot(plot_data, reference_method, confidence_level)
  }

  # Add faceting if requested
  if (facet_by == "sensor" && "sensor_position" %in% names(plot_data)) {
    p <- p + ggplot2::facet_wrap(~sensor_position, labeller = ggplot2::label_both)
  }

  # Add theme and labels
  p <- p +
    theme_sapfluxr(grid_major = TRUE, grid_minor = FALSE) +
    ggplot2::labs(caption = "Generated by sapFluxR")

  # Apply theme options if provided
  if (!is.null(theme_options)) {
    p <- p + do.call(ggplot2::theme, theme_options)
  }

  # Convert to interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("plotly package not available. Returning static ggplot.")
    } else {
      p <- plotly::ggplotly(p)
    }
  }

  return(p)
}

#' Create Diagnostic Plots
#'
#' Generate comprehensive diagnostic plots for data quality assessment,
#' sensor performance evaluation, and method validation.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param sap_data Optional sap_data object for additional diagnostics (default: NULL)
#' @param diagnostic_type Character string: "quality_overview", "sensor_performance", "method_diagnostics", "temporal_patterns", "distribution_analysis" (default: "quality_overview")
#' @param methods Character vector of methods to analyse (default: NULL for all methods)
#' @param sensors Character vector of sensor positions (default: "all")
#' @param time_window Optional time window for analysis (default: NULL)
#' @param combine_plots Logical whether to combine multiple diagnostic plots into one figure (default: TRUE)
#' @param interactive Logical whether to create interactive plots (default: FALSE)
#' @param theme_options List of theme options (default: NULL)
#'
#' @return A ggplot2 object, list of plots, or plotly object
#'
#' @examples
#' \dontrun{
#' # Quality overview
#' plot_diagnostics(vh_results, diagnostic_type = "quality_overview")
#'
#' # Sensor performance analysis
#' plot_diagnostics(vh_results, sap_data, diagnostic_type = "sensor_performance")
#'
#' # Method diagnostics
#' plot_diagnostics(vh_results, diagnostic_type = "method_diagnostics")
#'
#' # Combined diagnostic dashboard
#' plot_diagnostics(vh_results, diagnostic_type = "quality_overview", combine_plots = TRUE)
#' }
#'
#' @export
plot_diagnostics <- function(vh_results, sap_data = NULL,
                             diagnostic_type = "quality_overview",
                             methods = NULL, sensors = "all", time_window = NULL,
                             combine_plots = TRUE, interactive = FALSE, theme_options = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting functions. Please install it with install.packages('ggplot2')")
  }

  diagnostic_type <- match.arg(diagnostic_type, c("quality_overview", "sensor_performance",
                                                  "method_diagnostics", "temporal_patterns",
                                                  "distribution_analysis"))

  # Create diagnostic plots based on type
  if (diagnostic_type == "quality_overview") {
    plots <- create_quality_diagnostic_plots(vh_results, methods, sensors, time_window)
  } else if (diagnostic_type == "sensor_performance") {
    plots <- create_sensor_diagnostic_plots(vh_results, sap_data, methods, sensors)
  } else if (diagnostic_type == "method_diagnostics") {
    plots <- create_method_diagnostic_plots(vh_results, methods, sensors)
  } else if (diagnostic_type == "temporal_patterns") {
    plots <- create_temporal_diagnostic_plots(vh_results, methods, sensors, time_window)
  } else if (diagnostic_type == "distribution_analysis") {
    plots <- create_distribution_diagnostic_plots(vh_results, methods, sensors)
  }

  # Apply theme to all plots
  base_theme <- theme_sapfluxr(grid_major = TRUE, grid_minor = FALSE)
  if (!is.null(theme_options)) {
    base_theme <- base_theme + do.call(ggplot2::theme, theme_options)
  }

  plots <- lapply(plots, function(p) p + base_theme + ggplot2::labs(caption = "Generated by sapFluxR"))

  # Combine plots if requested
  if (combine_plots && length(plots) > 1) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      warning("patchwork package not available for combining plots. Returning list of plots.")
      result <- plots
    } else {
      result <- patchwork::wrap_plots(plots, ncol = 2)
    }
  } else {
    result <- if (length(plots) == 1) plots[[1]] else plots
  }

  # Convert to interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("plotly package not available. Returning static plots.")
    } else {
      if (is.list(result)) {
        result <- lapply(result, plotly::ggplotly)
      } else {
        result <- plotly::ggplotly(result)
      }
    }
  }

  return(result)
}

# =============================================================================
# HELPER FUNCTIONS FOR PLOTTING
# =============================================================================

# Temperature data processing helpers
calculate_temperature_differences <- function(measurements) {
  # Calculate pre-pulse baseline and temperature differences
  # This is a simplified implementation - would need full logic from heat_pulse_velocity.R

  # Group by pulse and calculate differences
  result <- measurements %>%
    dplyr::group_by(pulse_id) %>%
    dplyr::mutate(
      # Simple difference calculation for plotting
      delta_do = do - mean(do[1:min(25, n())], na.rm = TRUE),
      delta_di = di - mean(di[1:min(25, n())], na.rm = TRUE),
      delta_uo = uo - mean(uo[1:min(25, n())], na.rm = TRUE),
      delta_ui = ui - mean(ui[1:min(25, n())], na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Reshape to long format
  result_long <- result %>%
    tidyr::pivot_longer(cols = c(delta_do, delta_di, delta_uo, delta_ui),
                        names_to = "sensor", values_to = "delta_temp") %>%
    dplyr::mutate(sensor = stringr::str_remove(sensor, "delta_"))

  return(result_long)
}

reshape_temperature_data <- function(measurements, sensors) {
  # Reshape temperature data for plotting
  if (sensors == "all") {
    sensor_cols <- c("do", "di", "uo", "ui")
  } else if (sensors == "outer") {
    sensor_cols <- c("do", "uo")
  } else if (sensors == "inner") {
    sensor_cols <- c("di", "ui")
  } else {
    sensor_cols <- intersect(sensors, c("do", "di", "uo", "ui"))
  }

  measurements %>%
    dplyr::select(datetime, pulse_id, all_of(sensor_cols)) %>%
    tidyr::pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "temperature")
}

add_relative_time <- function(plot_data) {
  # Add relative time since pulse start
  plot_data %>%
    dplyr::group_by(pulse_id) %>%
    dplyr::mutate(relative_time = as.numeric(datetime - min(datetime))) %>%
    dplyr::ungroup()
}

create_temperature_plot_title <- function(plot_type, sensors) {
  base_title <- switch(plot_type,
                       "timeseries" = "Temperature Time Series",
                       "pulse_overlay" = "Heat Pulse Temperature Response",
                       "temperature_differences" = "Temperature Differences from Pre-pulse Baseline",
                       "diagnostic" = "Temperature Diagnostic Analysis")

  if (sensors != "all") {
    base_title <- paste(base_title, "-", stringr::str_to_title(paste(sensors, collapse = ", ")), "Sensors")
  }

  return(base_title)
}

create_temperature_plot_subtitle <- function(sap_data, pulse_ids, time_window) {
  subtitle_parts <- c()

  if (!is.null(pulse_ids)) {
    subtitle_parts <- c(subtitle_parts, paste("Pulses:", paste(pulse_ids, collapse = ", ")))
  }

  if (!is.null(time_window)) {
    subtitle_parts <- c(subtitle_parts, paste("Time window:", format(time_window[1], "%Y-%m-%d %H:%M"),
                                              "to", format(time_window[2], "%Y-%m-%d %H:%M")))
  }

  if (length(subtitle_parts) > 0) {
    return(paste(subtitle_parts, collapse = " | "))
  } else {
    return(NULL)
  }
}

# =============================================================================
# HPV TIME SERIES PLOTTING HELPERS
# =============================================================================

filter_velocity_data <- function(vh_results, methods, sensors, quality_filter, quality_flags, time_window) {
  plot_data <- vh_results

  # Filter by methods
  if (!is.null(methods)) {
    plot_data <- plot_data[plot_data$method %in% methods, ]
  }

  # Filter by sensors
  if (sensors != "all" && "sensor_position" %in% names(plot_data)) {
    if (sensors == "outer") {
      plot_data <- plot_data[plot_data$sensor_position == "outer", ]
    } else if (sensors == "inner") {
      plot_data <- plot_data[plot_data$sensor_position == "inner", ]
    } else if (is.character(sensors)) {
      plot_data <- plot_data[plot_data$sensor_position %in% sensors, ]
    }
  }

  # Filter by quality
  if (quality_filter && "quality_flag" %in% names(plot_data)) {
    plot_data <- plot_data[plot_data$quality_flag %in% quality_flags, ]
  }

  # Filter by time window
  if (!is.null(time_window)) {
    plot_data <- plot_data[plot_data$datetime >= time_window[1] &
                             plot_data$datetime <= time_window[2], ]
  }

  # Remove NA values in key columns
  plot_data <- plot_data[!is.na(plot_data$Vh_cm_hr) & !is.na(plot_data$datetime), ]

  return(plot_data)
}

aggregate_for_plotting <- function(plot_data, time_aggregation) {
  # Create aggregation period column
  plot_data$period <- switch(time_aggregation,
                             "hourly" = lubridate::floor_date(plot_data$datetime, "hour"),
                             "daily" = lubridate::floor_date(plot_data$datetime, "day"),
                             "weekly" = lubridate::floor_date(plot_data$datetime, "week")
  )

  # Aggregate data
  grouping_vars <- c("period", "method")
  if ("sensor_position" %in% names(plot_data)) {
    grouping_vars <- c(grouping_vars, "sensor_position")
  }

  aggregated <- plot_data %>%
    dplyr::group_by(across(all_of(grouping_vars))) %>%
    dplyr::summarise(
      datetime = first(period),
      Vh_cm_hr = mean(Vh_cm_hr, na.rm = TRUE),
      Vh_sd = sd(Vh_cm_hr, na.rm = TRUE),
      n_obs = n(),
      min_vh = min(Vh_cm_hr, na.rm = TRUE),
      max_vh = max(Vh_cm_hr, na.rm = TRUE),
      .groups = "drop"
    )

  return(aggregated)
}

create_hpv_aes <- function(colour_by, plot_type, time_aggregation) {
  if (time_aggregation %in% c("boxplot_daily", "violin_daily")) {
    return(ggplot2::aes(x = as.Date(datetime), y = Vh_cm_hr, fill = get(colour_by)))
  } else {
    return(ggplot2::aes(x = datetime, y = Vh_cm_hr, colour = get(colour_by)))
  }
}

add_hpv_geoms <- function(p, plot_type, time_aggregation) {
  if (plot_type == "points") {
    p <- p + ggplot2::geom_point(alpha = 0.6, size = 1)
  } else if (plot_type == "lines") {
    p <- p + ggplot2::geom_line(alpha = 0.8, size = 0.7)
  } else if (plot_type == "both") {
    p <- p +
      ggplot2::geom_line(alpha = 0.7, size = 0.5) +
      ggplot2::geom_point(alpha = 0.6, size = 0.8)
  } else if (plot_type == "boxplot_daily") {
    p <- p + ggplot2::geom_boxplot(alpha = 0.7)
  } else if (plot_type == "violin_daily") {
    p <- p + ggplot2::geom_violin(alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, outlier.shape = NA)
  }

  return(p)
}

add_hpv_colours <- function(p, colour_by, plot_data) {
  if (colour_by == "method") {
    methods <- unique(plot_data$method)
    colours <- get_method_colours(methods)
    p <- p + ggplot2::scale_colour_manual(values = colours, name = "Method") +
      ggplot2::scale_fill_manual(values = colours, name = "Method")
  } else if (colour_by == "sensor") {
    sensor_colours <- c("outer" = "#E31A1C", "inner" = "#1F78B4")
    p <- p + ggplot2::scale_colour_manual(values = sensor_colours, name = "Sensor Position") +
      ggplot2::scale_fill_manual(values = sensor_colours, name = "Sensor Position")
  } else if (colour_by == "quality") {
    quality_colours <- c("OK" = "#2ECC71", "HIGH_VELOCITY" = "#F39C12",
                        "NEGATIVE_FLOW" = "#E74C3C", "INFINITE" = "#9B59B6",
                        "MISSING" = "#95A5A6")
    p <- p + ggplot2::scale_colour_manual(values = quality_colours, name = "Quality Flag") +
      ggplot2::scale_fill_manual(values = quality_colours, name = "Quality Flag")
  }

  return(p)
}

add_statistical_overlays <- function(p, plot_data) {
  # Add overall mean line
  overall_mean <- mean(plot_data$Vh_cm_hr, na.rm = TRUE)
  p <- p + ggplot2::geom_hline(yintercept = overall_mean, linetype = "dashed",
                               colour = "red", alpha = 0.7, size = 1)

  # Add smoothed trend if enough data points
  if (nrow(plot_data) > 10) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2, size = 1.2)
  }

  return(p)
}

add_hpv_faceting <- function(p, facet_by, plot_data) {
  if (facet_by == "method") {
    p <- p + ggplot2::facet_wrap(~method, scales = "free_y", labeller = ggplot2::label_both)
  } else if (facet_by == "sensor" && "sensor_position" %in% names(plot_data)) {
    p <- p + ggplot2::facet_wrap(~sensor_position, labeller = ggplot2::label_both)
  } else if (facet_by == "both" && "sensor_position" %in% names(plot_data)) {
    p <- p + ggplot2::facet_grid(sensor_position ~ method, labeller = ggplot2::label_both)
  }

  return(p)
}

create_hpv_plot_title <- function(methods, sensors, plot_type, time_aggregation) {
  title_base <- "Heat Pulse Velocity"

  if (!is.null(methods) && length(methods) == 1) {
    title_base <- paste(title_base, "-", methods)
  } else if (!is.null(methods)) {
    title_base <- paste(title_base, "- Method Comparison")
  }

  if (time_aggregation != "none") {
    title_base <- paste(title_base, "- ", stringr::str_to_title(time_aggregation), "Aggregation")
  }

  if (plot_type %in% c("boxplot_daily", "violin_daily")) {
    title_base <- paste(title_base, "- Distribution Analysis")
  }

  return(title_base)
}

create_hpv_plot_subtitle <- function(plot_data, quality_filter) {
  subtitle_parts <- c()

  # Add data summary
  n_points <- nrow(plot_data)
  n_methods <- length(unique(plot_data$method))

  subtitle_parts <- c(subtitle_parts, paste(n_points, "measurements"))

  if (n_methods > 1) {
    subtitle_parts <- c(subtitle_parts, paste(n_methods, "methods"))
  }

  # Add time range
  time_range <- range(plot_data$datetime, na.rm = TRUE)
  date_range <- paste(format(time_range[1], "%Y-%m-%d"), "to", format(time_range[2], "%Y-%m-%d"))
  subtitle_parts <- c(subtitle_parts, date_range)

  # Add quality filter info
  if (quality_filter) {
    subtitle_parts <- c(subtitle_parts, "Quality filtered")
  }

  return(paste(subtitle_parts, collapse = " | "))
}

# =============================================================================
# METHOD COMPARISON PLOTTING HELPERS
# =============================================================================

prepare_method_comparison_data <- function(vh_results, reference_method, methods, sensors,
                                           quality_filter, quality_flags) {
  # Convert to data frame if needed
  if (!is.data.frame(vh_results)) {
    vh_results <- as.data.frame(vh_results)
  }

  # Filter data
  plot_data <- filter_velocity_data(vh_results, methods, sensors, quality_filter, quality_flags, NULL)

  # Check if reference method exists
  available_methods <- unique(plot_data$method)
  if (!reference_method %in% available_methods) {
    reference_method <- available_methods[1]
    warning("Reference method not found. Using '", reference_method, "' as reference.")
  }

  # Ensure we have at least 2 methods for comparison
  if (length(available_methods) < 2) {
    stop("Need at least 2 methods for comparison. Available methods: ",
         paste(available_methods, collapse = ", "))
  }

  # Create wide format for method comparison
  wide_data <- plot_data %>%
    dplyr::select(datetime, pulse_id, method, sensor_position, Vh_cm_hr) %>%
    tidyr::pivot_wider(names_from = method, values_from = Vh_cm_hr, names_prefix = "method_")

  # Only keep rows with reference method data
  reference_col <- paste0("method_", reference_method)
  wide_data <- wide_data[!is.na(wide_data[[reference_col]]), ]

  return(wide_data)
}

create_method_scatter_plot <- function(plot_data, reference_method, add_statistics, outlier_detection) {
  reference_col <- paste0("method_", reference_method)
  comparison_methods <- setdiff(names(plot_data)[startsWith(names(plot_data), "method_")], reference_col)

  if (length(comparison_methods) == 0) {
    stop("No comparison methods available")
  }

  # Create long format for scatter plot
  scatter_data <- plot_data %>%
    tidyr::pivot_longer(cols = all_of(comparison_methods),
                        names_to = "comparison_method", values_to = "comparison_value") %>%
    dplyr::mutate(comparison_method = stringr::str_remove(comparison_method, "method_")) %>%
    dplyr::filter(!is.na(comparison_value))

  # Rename reference column for consistent naming
  names(scatter_data)[names(scatter_data) == reference_col] <- "reference_value"

  # Create base plot
  p <- ggplot2::ggplot(scatter_data, ggplot2::aes(x = reference_value, y = comparison_value,
                                                  colour = comparison_method)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "black", alpha = 0.7) +
    ggplot2::labs(
      title = paste("Method Comparison vs", reference_method),
      x = paste(reference_method, "Velocity (cm/hr)"),
      y = "Comparison Method Velocity (cm/hr)",
      colour = "Method"
    )

  # Add method colours
  methods <- unique(scatter_data$comparison_method)
  colours <- get_method_colours(methods)
  p <- p + ggplot2::scale_colour_manual(values = colours)

  # Add statistics if requested
  if (add_statistics) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3)

    # Calculate and add correlation text
    # Only calculate if there are complete pairs
    correlations <- scatter_data %>%
      dplyr::filter(!is.na(reference_value) & !is.na(comparison_value)) %>%
      dplyr::group_by(comparison_method) %>%
      dplyr::summarise(
        n = dplyr::n(),
        r = if (n() > 1) cor(reference_value, comparison_value, use = "complete.obs") else NA_real_,
        .groups = "drop"
      ) %>%
      dplyr::filter(n > 0)

    # Add correlation labels (only if we have valid correlations)
    if (nrow(correlations) > 0) {
      max_x <- max(scatter_data$reference_value, na.rm = TRUE)
      max_y <- max(scatter_data$comparison_value, na.rm = TRUE)

      for (i in 1:nrow(correlations)) {
        method <- correlations$comparison_method[i]
        r_value <- correlations$r[i]

        if (!is.na(r_value)) {
          r_value <- round(r_value, 3)
          p <- p + ggplot2::annotate("text", x = max_x * 0.8, y = max_y * (0.9 - (i-1)*0.1),
                                     label = paste(method, ": r =", r_value),
                                     colour = colours[[method]], size = 3, hjust = 0)
        }
      }
    }
  }

  # Add outlier detection if requested
  if (outlier_detection) {
    # Simple outlier detection based on residuals
    scatter_data <- scatter_data %>%
      dplyr::group_by(comparison_method) %>%
      dplyr::mutate(
        residual = comparison_value - reference_value,
        is_outlier = abs(residual) > 2 * sd(residual, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    outliers <- scatter_data[scatter_data$is_outlier, ]
    if (nrow(outliers) > 0) {
      p <- p + ggplot2::geom_point(data = outliers,
                                   ggplot2::aes(x = reference_value, y = comparison_value),
                                   colour = "red", size = 2, shape = 1, stroke = 2)
    }
  }

  return(p)
}

create_bland_altman_plot <- function(plot_data, reference_method, confidence_level, add_statistics) {
  reference_col <- paste0("method_", reference_method)
  comparison_methods <- setdiff(names(plot_data)[startsWith(names(plot_data), "method_")], reference_col)

  if (length(comparison_methods) != 1) {
    stop("Bland-Altman plot requires exactly one comparison method. Use method filtering to select specific method.")
  }

  comparison_col <- comparison_methods[1]
  comparison_method_name <- stringr::str_remove(comparison_col, "method_")

  # Calculate Bland-Altman statistics
  ba_data <- plot_data %>%
    dplyr::mutate(
      mean_value = (get(reference_col) + get(comparison_col)) / 2,
      difference = get(comparison_col) - get(reference_col)
    ) %>%
    dplyr::filter(!is.na(mean_value) & !is.na(difference))

  if (nrow(ba_data) == 0) {
    stop("No valid data points for Bland-Altman analysis")
  }

  # Calculate agreement limits
  mean_diff <- mean(ba_data$difference, na.rm = TRUE)
  sd_diff <- sd(ba_data$difference, na.rm = TRUE)

  alpha <- 1 - confidence_level
  t_value <- qt(1 - alpha/2, df = nrow(ba_data) - 1)

  upper_limit <- mean_diff + t_value * sd_diff
  lower_limit <- mean_diff - t_value * sd_diff

  # Create plot
  p <- ggplot2::ggplot(ba_data, ggplot2::aes(x = mean_value, y = difference)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_hline(yintercept = mean_diff, colour = "blue", linetype = "solid", size = 1) +
    ggplot2::geom_hline(yintercept = upper_limit, colour = "red", linetype = "dashed", size = 1) +
    ggplot2::geom_hline(yintercept = lower_limit, colour = "red", linetype = "dashed", size = 1) +
    ggplot2::geom_hline(yintercept = 0, colour = "black", alpha = 0.3) +
    ggplot2::labs(
      title = paste("Bland-Altman Plot:", comparison_method_name, "vs", reference_method),
      x = "Mean of Methods (cm/hr)",
      y = "Difference (cm/hr)",
      subtitle = paste0("Mean difference: ", round(mean_diff, 2),
                        " cm/hr, Limits of agreement: ", round(lower_limit, 2),
                        " to ", round(upper_limit, 2), " cm/hr")
    )

  # Add statistics if requested
  if (add_statistics) {
    # Add proportional bias assessment
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3, colour = "green")

    # Calculate bias statistics
    bias_model <- lm(difference ~ mean_value, data = ba_data)
    slope_p <- summary(bias_model)$coefficients[2, 4]

    if (slope_p < 0.05) {
      bias_text <- paste("Significant proportional bias detected (p =", round(slope_p, 4), ")")
    } else {
      bias_text <- "No significant proportional bias detected"
    }

    p <- p + ggplot2::annotate("text", x = max(ba_data$mean_value) * 0.7,
                               y = max(ba_data$difference) * 0.9,
                               label = bias_text, size = 3, colour = "green")
  }

  return(p)
}

create_correlation_matrix_plot <- function(plot_data) {
  # Extract method columns
  method_cols <- names(plot_data)[startsWith(names(plot_data), "method_")]

  if (length(method_cols) < 2) {
    stop("Need at least 2 methods for correlation matrix")
  }

  # Calculate correlation matrix
  cor_data <- plot_data[, method_cols]
  names(cor_data) <- stringr::str_remove(names(cor_data), "method_")
  cor_matrix <- cor(cor_data, use = "complete.obs")

  # Convert to long format for plotting
  cor_long <- as.data.frame(as.table(cor_matrix))
  names(cor_long) <- c("Method1", "Method2", "Correlation")

  # Create heatmap
  p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = Method1, y = Method2, fill = Correlation)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), colour = "white", size = 4, fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = "#E31A1C", mid = "white", high = "#1F78B4",
                                  midpoint = 0, limit = c(-1, 1), name = "Correlation") +
    ggplot2::labs(
      title = "Method Correlation Matrix",
      x = "Method",
      y = "Method"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.background = ggplot2::element_blank()
    )

  return(p)
}

create_method_performance_plot <- function(plot_data, reference_method) {
  # Calculate performance metrics for each method
  method_cols <- names(plot_data)[startsWith(names(plot_data), "method_")]
  reference_col <- paste0("method_", reference_method)

  performance_data <- data.frame()

  for (col in method_cols) {
    if (col == reference_col) next

    method_name <- stringr::str_remove(col, "method_")
    valid_pairs <- complete.cases(plot_data[, c(reference_col, col)])

    if (sum(valid_pairs) > 5) {  # Need at least 5 points
      ref_vals <- plot_data[valid_pairs, reference_col]
      comp_vals <- plot_data[valid_pairs, col]

      # Calculate performance metrics
      rmse <- sqrt(mean((comp_vals - ref_vals)^2, na.rm = TRUE))
      mae <- mean(abs(comp_vals - ref_vals), na.rm = TRUE)
      bias <- mean(comp_vals - ref_vals, na.rm = TRUE)
      r <- cor(ref_vals, comp_vals, use = "complete.obs")

      performance_data <- rbind(performance_data, data.frame(
        Method = method_name,
        RMSE = rmse,
        MAE = mae,
        Bias = bias,
        Correlation = r,
        N = sum(valid_pairs)
      ))
    }
  }

  if (nrow(performance_data) == 0) {
    stop("No valid method comparisons found")
  }

  # Create performance comparison plot
  perf_long <- performance_data %>%
    tidyr::pivot_longer(cols = c(RMSE, MAE, Bias, Correlation),
                        names_to = "Metric", values_to = "Value")

  p <- ggplot2::ggplot(perf_long, ggplot2::aes(x = Method, y = Value, fill = Method)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = paste("Method Performance vs", reference_method),
      x = "Method",
      y = "Value"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  # Add method colours
  methods <- unique(perf_long$Method)
  colours <- get_method_colours(methods)
  p <- p + ggplot2::scale_fill_manual(values = colours)

  return(p)
}

create_method_agreement_plot <- function(plot_data, reference_method, confidence_level) {
  # This creates a plot showing agreement statistics between methods
  reference_col <- paste0("method_", reference_method)
  method_cols <- setdiff(names(plot_data)[startsWith(names(plot_data), "method_")], reference_col)

  agreement_stats <- data.frame()

  for (col in method_cols) {
    method_name <- stringr::str_remove(col, "method_")

    # Calculate various agreement metrics
    valid_data <- plot_data[complete.cases(plot_data[, c(reference_col, col)]), ]

    if (nrow(valid_data) > 5) {
      ref_vals <- valid_data[[reference_col]]
      comp_vals <- valid_data[[col]]

      # Calculate agreement metrics
      mean_diff <- mean(comp_vals - ref_vals)
      sd_diff <- sd(comp_vals - ref_vals)

      # Limits of agreement
      alpha <- 1 - confidence_level
      t_value <- qt(1 - alpha/2, df = nrow(valid_data) - 1)
      loa_upper <- mean_diff + t_value * sd_diff
      loa_lower <- mean_diff - t_value * sd_diff

      # Percent within limits
      within_limits <- sum(abs(comp_vals - ref_vals - mean_diff) <= t_value * sd_diff) / nrow(valid_data) * 100

      agreement_stats <- rbind(agreement_stats, data.frame(
        Method = method_name,
        Mean_Difference = mean_diff,
        LoA_Upper = loa_upper,
        LoA_Lower = loa_lower,
        Percent_Within_LoA = within_limits,
        N = nrow(valid_data)
      ))
    }
  }

  # Create agreement plot
  p <- ggplot2::ggplot(agreement_stats, ggplot2::aes(x = Method)) +
    ggplot2::geom_point(ggplot2::aes(y = Mean_Difference), size = 3, colour = "blue") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = LoA_Lower, ymax = LoA_Upper),
                           width = 0.2, colour = "red", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = paste("Agreement Analysis vs", reference_method),
      subtitle = paste0(confidence_level * 100, "% Limits of Agreement"),
      x = "Method",
      y = "Difference (cm/hr)",
      caption = "Points: mean difference, Error bars: limits of agreement"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}

# =============================================================================
# DIAGNOSTIC PLOTTING HELPERS
# =============================================================================

create_quality_diagnostic_plots <- function(vh_results, methods, sensors, time_window) {
  # Filter data
  plot_data <- filter_velocity_data(vh_results, methods, sensors, FALSE, NULL, time_window)

  plots <- list()

  # Quality flag distribution
  if ("quality_flag" %in% names(plot_data)) {
    quality_summary <- plot_data %>%
      dplyr::count(quality_flag) %>%
      dplyr::mutate(percentage = n / sum(n) * 100)

    plots$quality_distribution <- ggplot2::ggplot(quality_summary,
                                                  ggplot2::aes(x = quality_flag, y = percentage, fill = quality_flag)) +
      ggplot2::geom_col(alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage, 1), "%")),
                         vjust = -0.5, size = 3) +
      ggplot2::labs(
        title = "Quality Flag Distribution",
        x = "Quality Flag",
        y = "Percentage of Measurements (%)"
      ) +
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  # Velocity range analysis
  plots$velocity_range <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Vh_cm_hr)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue", colour = "black") +
    ggplot2::geom_vline(xintercept = 0, colour = "red", linetype = "dashed", alpha = 0.7) +
    ggplot2::labs(
      title = "Velocity Distribution",
      x = "Heat Pulse Velocity (cm/hr)",
      y = "Frequency"
    )

  # Time series overview with quality flags
  if ("quality_flag" %in% names(plot_data) && nrow(plot_data) > 0) {
    plots$temporal_quality <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = Vh_cm_hr, colour = quality_flag)) +
      ggplot2::geom_point(alpha = 0.6, size = 0.8) +
      ggplot2::labs(
        title = "Temporal Quality Assessment",
        x = "Date/Time",
        y = "Heat Pulse Velocity (cm/hr)",
        colour = "Quality Flag"
      )
  }

  return(plots)
}

create_sensor_diagnostic_plots <- function(vh_results, sap_data, methods, sensors) {
  plots <- list()

  # If sap_data is available, create sensor-specific diagnostics
  if (!is.null(sap_data) && !is.null(sap_data$diagnostics)) {
    diagnostics <- sap_data$diagnostics

    # Battery voltage over time
    plots$battery_voltage <- ggplot2::ggplot(diagnostics, ggplot2::aes(x = datetime, y = batt_volt)) +
      ggplot2::geom_line(alpha = 0.8, colour = "blue") +
      ggplot2::geom_hline(yintercept = 4.0, colour = "red", linetype = "dashed", alpha = 0.7) +
      ggplot2::labs(
        title = "Battery Voltage Over Time",
        x = "Date/Time",
        y = "Battery Voltage (V)",
        subtitle = "Red line indicates minimum recommended voltage"
      )

    # External power analysis
    if ("external_volt" %in% names(diagnostics)) {
      plots$external_power <- ggplot2::ggplot(diagnostics, ggplot2::aes(x = datetime, y = external_volt)) +
        ggplot2::geom_line(alpha = 0.8, colour = "green") +
        ggplot2::labs(
          title = "External Power Voltage",
          x = "Date/Time",
          y = "External Voltage (V)"
        )
    }
  }

  # Sensor position comparison (if available)
  plot_data <- filter_velocity_data(vh_results, methods, sensors, FALSE, NULL, NULL)

  if ("sensor_position" %in% names(plot_data) && length(unique(plot_data$sensor_position)) > 1) {
    plots$sensor_comparison <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sensor_position, y = Vh_cm_hr, fill = sensor_position)) +
      ggplot2::geom_boxplot(alpha = 0.8) +
      ggplot2::labs(
        title = "Sensor Position Comparison",
        x = "Sensor Position",
        y = "Heat Pulse Velocity (cm/hr)"
      ) +
      ggplot2::theme(legend.position = "none")
  }

  return(plots)
}

create_method_diagnostic_plots <- function(vh_results, methods, sensors) {
  plot_data <- filter_velocity_data(vh_results, methods, sensors, FALSE, NULL, NULL)

  plots <- list()

  # Method performance comparison
  if (length(unique(plot_data$method)) > 1) {
    plots$method_boxplot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = Vh_cm_hr, fill = method)) +
      ggplot2::geom_boxplot(alpha = 0.8) +
      ggplot2::geom_jitter(alpha = 0.3, width = 0.2, size = 0.5) +
      ggplot2::labs(
        title = "Method Performance Comparison",
        x = "Method",
        y = "Heat Pulse Velocity (cm/hr)"
      ) +
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    # Method colours
    methods_available <- unique(plot_data$method)
    colours <- get_method_colours(methods_available)
    plots$method_boxplot <- plots$method_boxplot + ggplot2::scale_fill_manual(values = colours)
  }

  # Method reliability over time
  if (nrow(plot_data) > 50) {  # Only if sufficient data
    plots$method_reliability <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = Vh_cm_hr, colour = method)) +
      ggplot2::geom_smooth(se = TRUE, alpha = 0.3) +
      ggplot2::labs(
        title = "Method Reliability Over Time",
        x = "Date/Time",
        y = "Heat Pulse Velocity (cm/hr)",
        colour = "Method"
      )

    # Add method colours
    methods_available <- unique(plot_data$method)
    colours <- get_method_colours(methods_available)
    plots$method_reliability <- plots$method_reliability + ggplot2::scale_colour_manual(values = colours)
  }

  return(plots)
}

create_temporal_diagnostic_plots <- function(vh_results, methods, sensors, time_window) {
  plot_data <- filter_velocity_data(vh_results, methods, sensors, TRUE, c("OK"), time_window)

  plots <- list()

  # Diurnal patterns
  if (nrow(plot_data) > 24) {  # Need at least 24 hours of data
    plot_data$hour <- lubridate::hour(plot_data$datetime)

    hourly_stats <- plot_data %>%
      dplyr::group_by(hour) %>%
      dplyr::summarise(
        mean_vh = mean(Vh_cm_hr, na.rm = TRUE),
        sd_vh = sd(Vh_cm_hr, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(n >= 3)  # Only hours with at least 3 measurements

    if (nrow(hourly_stats) > 12) {
      plots$diurnal_pattern <- ggplot2::ggplot(hourly_stats, ggplot2::aes(x = hour, y = mean_vh)) +
        ggplot2::geom_line(size = 1, colour = "blue") +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_vh - sd_vh, ymax = mean_vh + sd_vh),
                             alpha = 0.3, fill = "blue") +
        ggplot2::scale_x_continuous(breaks = seq(0, 23, 3), labels = paste0(seq(0, 23, 3), ":00")) +
        ggplot2::labs(
          title = "Diurnal Pattern of Sap Flow",
          x = "Hour of Day",
          y = "Mean Heat Pulse Velocity (cm/hr)",
          subtitle = "Ribbon shows ± 1 standard deviation"
        )
    }
  }

  # Seasonal trends (if data spans multiple months)
  time_span <- diff(range(plot_data$datetime, na.rm = TRUE))
  if (time_span > 60) {  # More than 60 days
    plot_data$month <- lubridate::month(plot_data$datetime)

    monthly_stats <- plot_data %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(
        mean_vh = mean(Vh_cm_hr, na.rm = TRUE),
        median_vh = median(Vh_cm_hr, na.rm = TRUE),
        sd_vh = sd(Vh_cm_hr, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(n >= 10)

    if (nrow(monthly_stats) > 2) {
      plots$seasonal_trend <- ggplot2::ggplot(monthly_stats, ggplot2::aes(x = factor(month))) +
        ggplot2::geom_col(ggplot2::aes(y = mean_vh), alpha = 0.7, fill = "forestgreen") +
        ggplot2::scale_x_discrete(labels = month.abb[1:12]) +
        ggplot2::labs(
          title = "Seasonal Sap Flow Patterns",
          x = "Month",
          y = "Mean Heat Pulse Velocity (cm/hr)"
        )
    }
  }

  return(plots)
}

create_distribution_diagnostic_plots <- function(vh_results, methods, sensors) {
  plot_data <- filter_velocity_data(vh_results, methods, sensors, TRUE, c("OK"), NULL)

  plots <- list()

  # Overall distribution
  plots$overall_distribution <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Vh_cm_hr)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue", colour = "black") +
    ggplot2::geom_density(ggplot2::aes(y = ..scaled..), colour = "red", size = 1) +
    ggplot2::labs(
      title = "Distribution of Heat Pulse Velocities",
      x = "Heat Pulse Velocity (cm/hr)",
      y = "Density / Frequency"
    )

  # Q-Q plot for normality assessment
  plots$qq_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = Vh_cm_hr)) +
    ggplot2::stat_qq(alpha = 0.6) +
    ggplot2::stat_qq_line(colour = "red", size = 1) +
    ggplot2::labs(
      title = "Q-Q Plot for Normality Assessment",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )

  # Distribution by method (if multiple methods)
  if (length(unique(plot_data$method)) > 1) {
    plots$method_distributions <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Vh_cm_hr, fill = method)) +
      ggplot2::geom_density(alpha = 0.6) +
      ggplot2::labs(
        title = "Velocity Distributions by Method",
        x = "Heat Pulse Velocity (cm/hr)",
        y = "Density",
        fill = "Method"
      )

    # Add method colours
    methods_available <- unique(plot_data$method)
    colours <- get_method_colours(methods_available)
    plots$method_distributions <- plots$method_distributions + ggplot2::scale_fill_manual(values = colours)
  }

  return(plots)
}