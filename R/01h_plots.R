# R/01h_plots.R
# Plotting functions for heat pulse data visualisation

#' @importFrom dplyr %>% filter select mutate
#' @importFrom patchwork plot_annotation plot_layout
NULL

#' Plot Heat Pulse Trace with Calculation Windows
#'
#' Creates a diagnostic plot showing temperature traces for a single heat pulse
#' with calculation windows and timepoints overlaid for different methods.
#'
#' @param heat_pulse_data A heat_pulse_data object from read_heat_pulse_data()
#' @param vh_results Results tibble from calc_heat_pulse_velocity()
#' @param pulse_id Pulse ID to plot
#' @param show_methods Character vector of methods to show windows for.
#'   If NULL, shows all methods present in results. Default: NULL
#' @param sensor_position Sensor position to show: "inner", "outer", or "both".
#'   If "both" (default), creates side-by-side plots for comparison. Default: "both"
#' @param pre_pulse Number of seconds of pre-pulse data to show. Default: 30
#'
#' @return A ggplot2 object (single plot) or patchwork object (side-by-side plots)
#'
#' @examples
#' \dontrun{
#' heat_pulse_data <- read_heat_pulse_data("data.txt")
#' results <- calc_heat_pulse_velocity(heat_pulse_data)
#'
#' # Show both sensors side-by-side (default)
#' plot_heat_pulse_trace(heat_pulse_data, results, pulse_id = 1)
#'
#' # Show only outer sensor
#' plot_heat_pulse_trace(heat_pulse_data, results, pulse_id = 1,
#'                       sensor_position = "outer")
#'
#' # Show only specific methods
#' plot_heat_pulse_trace(heat_pulse_data, results, pulse_id = 1,
#'                       show_methods = c("HRM", "Tmax_Coh"),
#'                       sensor_position = "both")
#' }
#'
#' @export
plot_heat_pulse_trace <- function(heat_pulse_data,
                                   vh_results,
                                   pulse_id,
                                   show_methods = NULL,
                                   sensor_position = "both",
                                   pre_pulse = 30) {

  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Install with: install.packages('tidyr')")
  }

  # Validate inputs
  if (!inherits(heat_pulse_data, "heat_pulse_data")) {
    stop("heat_pulse_data must be a heat_pulse_data object from read_heat_pulse_data()")
  }
  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be results from calc_heat_pulse_velocity()")
  }

  if (!sensor_position %in% c("inner", "outer", "both")) {
    stop("sensor_position must be 'inner', 'outer', or 'both'")
  }

  # If "both", create side-by-side plots
  if (sensor_position == "both") {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for side-by-side plots. Install with: install.packages('patchwork')")
    }

    p_outer <- plot_heat_pulse_trace(heat_pulse_data, vh_results, pulse_id,
                                      show_methods, "outer", pre_pulse)
    p_inner <- plot_heat_pulse_trace(heat_pulse_data, vh_results, pulse_id,
                                      show_methods, "inner", pre_pulse)

    # Combine side-by-side with shared legend
    combined <- (p_outer | p_inner) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        title = paste0("Heat Pulse Trace - Pulse ID: ", pulse_id),
        theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5))
      ) &
      ggplot2::theme(legend.position = "bottom")

    return(combined)
  }

  # Extract pulse data
  pulse_data <- heat_pulse_data$measurements[heat_pulse_data$measurements$pulse_id == pulse_id, ]

  if (nrow(pulse_data) == 0) {
    stop("Pulse ID ", pulse_id, " not found in heat_pulse_data")
  }

  # Calculate time relative to pulse (seconds)
  pulse_data$time_sec <- seq_len(nrow(pulse_data)) - pre_pulse

  # Calculate baseline temperatures
  pre_pulse_period <- 1:min(pre_pulse, nrow(pulse_data))
  do_baseline <- mean(pulse_data$do[pre_pulse_period], na.rm = TRUE)
  di_baseline <- mean(pulse_data$di[pre_pulse_period], na.rm = TRUE)
  uo_baseline <- mean(pulse_data$uo[pre_pulse_period], na.rm = TRUE)
  ui_baseline <- mean(pulse_data$ui[pre_pulse_period], na.rm = TRUE)

  # Calculate delta temperatures
  pulse_data$deltaT_do <- pulse_data$do - do_baseline
  pulse_data$deltaT_di <- pulse_data$di - di_baseline
  pulse_data$deltaT_uo <- pulse_data$uo - uo_baseline
  pulse_data$deltaT_ui <- pulse_data$ui - ui_baseline

  # Reshape to long format for ggplot
  plot_data <- tidyr::pivot_longer(
    pulse_data,
    cols = c(deltaT_do, deltaT_di, deltaT_uo, deltaT_ui),
    names_to = "sensor",
    values_to = "deltaT",
    names_prefix = "deltaT_"
  )

  # Create sensor labels
  plot_data$sensor_label <- factor(
    plot_data$sensor,
    levels = c("do", "di", "uo", "ui"),
    labels = c("Downstream Outer (do)", "Downstream Inner (di)",
               "Upstream Outer (uo)", "Upstream Inner (ui)")
  )

  # Filter to selected sensor position
  if (sensor_position == "outer") {
    plot_data <- plot_data[plot_data$sensor %in% c("do", "uo"), ]
    plot_data$sensor_label <- droplevels(plot_data$sensor_label)
    sensor_title <- "Outer Sensors"
  } else if (sensor_position == "inner") {
    plot_data <- plot_data[plot_data$sensor %in% c("di", "ui"), ]
    plot_data$sensor_label <- droplevels(plot_data$sensor_label)
    sensor_title <- "Inner Sensors"
  }

  # Get calculation metadata for this pulse
  pulse_results <- vh_results[vh_results$pulse_id == pulse_id, ]

  if (nrow(pulse_results) == 0) {
    warning("No results found for pulse ID ", pulse_id, " in vh_results")
  }

  # Filter to selected sensor position
  if (sensor_position %in% c("inner", "outer")) {
    pulse_results <- pulse_results[pulse_results$sensor_position == sensor_position, ]
  }

  # Filter methods if specified
  if (!is.null(show_methods)) {
    pulse_results <- pulse_results[pulse_results$method %in% show_methods, ]
  }

  # Create base plot
  plot_title <- if (sensor_position == "both") {
    paste0("Heat Pulse Trace - Pulse ID: ", pulse_id)
  } else {
    paste0(sensor_title, " - Pulse ID: ", pulse_id)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time_sec, y = deltaT, color = sensor_label)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
    ggplot2::annotate("text", x = 0, y = max(plot_data$deltaT, na.rm = TRUE) * 0.95,
                      label = "Heat Pulse", hjust = -0.1, size = 3) +
    ggplot2::scale_color_manual(
      values = c("Downstream Outer (do)" = "#E31A1C",
                 "Downstream Inner (di)" = "#FB9A99",
                 "Upstream Outer (uo)" = "#1F78B4",
                 "Upstream Inner (ui)" = "#A6CEE3"),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = plot_title,
      x = "Time (seconds after pulse)",
      y = expression(paste(Delta, "T (\u00B0C)")),
      color = "Sensor"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add calculation windows and timepoints
  if (nrow(pulse_results) > 0) {
    methods_to_annotate <- unique(pulse_results$method)
    y_max <- max(plot_data$deltaT, na.rm = TRUE)
    annotation_y_offset <- 0

    # Determine which sensors to use for annotations
    if (sensor_position == "outer") {
      annotation_sensor_position <- "outer"
      downstream_sensor <- "do"
      upstream_sensor <- "uo"
    } else if (sensor_position == "inner") {
      annotation_sensor_position <- "inner"
      downstream_sensor <- "di"
      upstream_sensor <- "ui"
    } else {
      # Should not reach here due to recursion, but fallback to outer
      annotation_sensor_position <- "outer"
      downstream_sensor <- "do"
      upstream_sensor <- "uo"
    }

    for (method in methods_to_annotate) {
      method_data <- pulse_results[pulse_results$method == method, ]

      if (nrow(method_data) == 0) next

      # Special handling for MHR: show both window and point
      if (method == "MHR" && !is.na(method_data$calc_window_start_sec[1]) &&
          !is.na(method_data$calc_window_end_sec[1])) {

        window_start <- method_data$calc_window_start_sec[1]
        window_end <- method_data$calc_window_end_sec[1]

        # Find the deltaT value at calc_time for the downstream sensor
        temp_at_downstream <- plot_data$deltaT[plot_data$time_sec == window_end &
                                           plot_data$sensor == downstream_sensor]
        if (length(temp_at_downstream) > 0) {
          temp_at_downstream <- temp_at_downstream[1]
        } else {
          temp_at_downstream <- 0
        }

        # Find the deltaT value at calc_time for the upstream sensor
        temp_at_upstream <- plot_data$deltaT[plot_data$time_sec == window_start &
                                           plot_data$sensor == upstream_sensor]
        if (length(temp_at_upstream) > 0) {
          temp_at_upstream <- temp_at_upstream[1]
        } else {
          temp_at_upstream <- 0
        }

        # Add shaded window showing time span between upstream and downstream peaks
        p <- p +
          ggplot2::annotate("rect",
                            xmin = window_start, xmax = window_end,
                            ymin = -Inf, ymax = Inf,
                            fill = "#FFA500", alpha = 0.15) +
          ggplot2::annotate("text",
                            x = (window_start + window_end) / 2,
                            y = y_max * (0.85 - annotation_y_offset * 0.05),
                            label = "MHR window",
                            size = 3, fontface = "bold", color = "#FF8C00") +
          # Add vertical line and point at downstream peak
          ggplot2::annotate("segment",
                            x = window_end, xend = window_end,
                            y = 0, yend = temp_at_downstream,
                            linetype = "dashed", color = "#FF8C00", linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = window_end, y = temp_at_downstream,
                            size = 3, color = "#FF8C00") +
          # Add vertical line and point at upstream peak
          ggplot2::annotate("segment",
                          x = window_start, xend = window_start,
                          y = 0, yend = temp_at_upstream,
                          linetype = "dashed", color = "#FF8C00", linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = window_start, y = temp_at_upstream,
                            size = 3, color = "#FF8C00")


        annotation_y_offset <- annotation_y_offset + 1

      # Special handling for HRMXb: shows separate downstream and upstream windows
      } else if (method == "HRMXb" &&
                 !is.na(method_data$downstream_window_start_sec[1]) &&
                 !is.na(method_data$upstream_window_start_sec[1])) {

        # HRMXb uses two separate windows: one for downstream, one for upstream
        downstream_start <- method_data$downstream_window_start_sec[1]
        downstream_end <- method_data$downstream_window_end_sec[1]
        upstream_start <- method_data$upstream_window_start_sec[1]
        upstream_end <- method_data$upstream_window_end_sec[1]

        # Colors for HRMXb - red for downstream, green for upstream
        fill_color_down <- "#FFB6C6"  # Light red/pink for downstream
        text_color_down <- "#DC143C"  # Crimson for downstream
        fill_color_up <- "#90EE90"    # Light green for upstream
        text_color_up <- "#228B22"    # Forest green for upstream

        # Determine which sensors to use based on sensor_position
        if (sensor_position == "outer") {
          downstream_sensor <- "do"
          upstream_sensor <- "uo"
        } else if (sensor_position == "inner") {
          downstream_sensor <- "di"
          upstream_sensor <- "ui"
        } else {
          # For 'both', use outer sensors
          downstream_sensor <- "do"
          upstream_sensor <- "uo"
        }

        # Add downstream window
        p <- p +
          ggplot2::annotate("rect",
                            xmin = downstream_start, xmax = downstream_end,
                            ymin = -Inf, ymax = Inf,
                            fill = fill_color_down, alpha = 0.3)

        # Add vertical lines and dots for downstream window - use DOWNSTREAM sensor only
        temp_at_down_start <- plot_data$deltaT[plot_data$time_sec == downstream_start & plot_data$sensor == downstream_sensor]
        if (length(temp_at_down_start) == 0 || !is.finite(temp_at_down_start[1])) temp_at_down_start <- 0 else temp_at_down_start <- temp_at_down_start[1]

        temp_at_down_end <- plot_data$deltaT[plot_data$time_sec == downstream_end & plot_data$sensor == downstream_sensor]
        if (length(temp_at_down_end) == 0 || !is.finite(temp_at_down_end[1])) temp_at_down_end <- 0 else temp_at_down_end <- temp_at_down_end[1]

        # Find max temp in downstream window for label positioning
        downstream_window_temps <- plot_data$deltaT[plot_data$time_sec >= downstream_start &
                                                     plot_data$time_sec <= downstream_end &
                                                     plot_data$sensor == downstream_sensor]
        downstream_max_temp <- if (length(downstream_window_temps) > 0) max(downstream_window_temps, na.rm = TRUE) else 0
        if (!is.finite(downstream_max_temp)) downstream_max_temp <- 0

        p <- p +
          ggplot2::annotate("segment",
                            x = downstream_start, xend = downstream_start,
                            y = 0, yend = temp_at_down_start,
                            linetype = "dashed", color = text_color_down, linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = downstream_start, y = temp_at_down_start,
                            size = 3, color = text_color_down) +
          ggplot2::annotate("segment",
                            x = downstream_end, xend = downstream_end,
                            y = 0, yend = temp_at_down_end,
                            linetype = "dashed", color = text_color_down, linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = downstream_end, y = temp_at_down_end,
                            size = 3, color = text_color_down) +
          ggplot2::annotate("text",
                            x = (downstream_start + downstream_end) / 2,
                            y = downstream_max_temp * 1.15,  # Position 15% above max temp in window
                            label = "HRMXb downstream",
                            size = 2.5, fontface = "bold", color = text_color_down)

        annotation_y_offset <- annotation_y_offset + 1

        # Add upstream window
        p <- p +
          ggplot2::annotate("rect",
                            xmin = upstream_start, xmax = upstream_end,
                            ymin = -Inf, ymax = Inf,
                            fill = fill_color_up, alpha = 0.3)

        # Add vertical lines and dots for upstream window - use UPSTREAM sensor only
        temp_at_up_start <- plot_data$deltaT[plot_data$time_sec == upstream_start & plot_data$sensor == upstream_sensor]
        if (length(temp_at_up_start) == 0 || !is.finite(temp_at_up_start[1])) temp_at_up_start <- 0 else temp_at_up_start <- temp_at_up_start[1]

        temp_at_up_end <- plot_data$deltaT[plot_data$time_sec == upstream_end & plot_data$sensor == upstream_sensor]
        if (length(temp_at_up_end) == 0 || !is.finite(temp_at_up_end[1])) temp_at_up_end <- 0 else temp_at_up_end <- temp_at_up_end[1]

        # Find max temp in upstream window for label positioning
        upstream_window_temps <- plot_data$deltaT[plot_data$time_sec >= upstream_start &
                                                   plot_data$time_sec <= upstream_end &
                                                   plot_data$sensor == upstream_sensor]
        upstream_max_temp <- if (length(upstream_window_temps) > 0) max(upstream_window_temps, na.rm = TRUE) else 0
        if (!is.finite(upstream_max_temp)) upstream_max_temp <- 0

        p <- p +
          ggplot2::annotate("segment",
                            x = upstream_start, xend = upstream_start,
                            y = 0, yend = temp_at_up_start,
                            linetype = "dashed", color = text_color_up, linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = upstream_start, y = temp_at_up_start,
                            size = 3, color = text_color_up) +
          ggplot2::annotate("segment",
                            x = upstream_end, xend = upstream_end,
                            y = 0, yend = temp_at_up_end,
                            linetype = "dashed", color = text_color_up, linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = upstream_end, y = temp_at_up_end,
                            size = 3, color = text_color_up) +
          ggplot2::annotate("text",
                            x = (upstream_start + upstream_end) / 2,
                            y = upstream_max_temp * 1.15,  # Position 15% above max temp in window
                            label = "HRMXb upstream",
                            size = 2.5, fontface = "bold", color = text_color_up)

        annotation_y_offset <- annotation_y_offset + 1


      } else if (!is.na(method_data$calc_window_start_sec[1]) && !is.na(method_data$calc_window_end_sec[1])) {
        # Windowed methods (HRM, HRMXa, HRMXb) - color-coded
        window_start <- method_data$calc_window_start_sec[1]
        window_end <- method_data$calc_window_end_sec[1]

        # Determine color based on method
        if (method == "HRM") {
          fill_color <- "grey60"
          text_color <- "black"
          add_lines <- FALSE  # HRM doesn't get vertical lines
        } else if (method == "HRMXa") {
          fill_color <- "#4169E1"  # Royal blue
          text_color <- "#0000CD"  # Medium blue
          add_lines <- TRUE
        } else if (method == "HRMXb") {
          fill_color <- "#32CD32"  # Lime green
          text_color <- "#228B22"  # Forest green
          add_lines <- TRUE
        } else {
          # Fallback for any other windowed method
          fill_color <- "grey60"
          text_color <- "black"
          add_lines <- FALSE
        }

        # Add shaded window
        p <- p +
          ggplot2::annotate("rect",
                            xmin = window_start, xmax = window_end,
                            ymin = -Inf, ymax = Inf,
                            fill = fill_color, alpha = 0.2) +
          ggplot2::annotate("text",
                            x = (window_start + window_end) / 2,
                            y = y_max * (0.85 - annotation_y_offset * 0.05),
                            label = paste0(method, " window"),
                            size = 3, fontface = "bold", color = text_color)

        # Add vertical lines at window boundaries for HRMXa and HRMXb
        if (add_lines) {
          # Find the maximum deltaT across all sensors at window_start
          temp_at_start <- max(plot_data$deltaT[plot_data$time_sec == window_start], na.rm = TRUE)
          if (!is.finite(temp_at_start)) temp_at_start <- 0

          # Find the maximum deltaT across all sensors at window_end
          temp_at_end <- max(plot_data$deltaT[plot_data$time_sec == window_end], na.rm = TRUE)
          if (!is.finite(temp_at_end)) temp_at_end <- 0

          p <- p +
            ggplot2::annotate("segment",
                              x = window_start, xend = window_start,
                              y = 0, yend = temp_at_start,
                              linetype = "dashed", color = text_color, linewidth = 0.7) +
            ggplot2::annotate("point",
                              x = window_start, y = temp_at_start,
                              size = 3, color = text_color) +
            ggplot2::annotate("segment",
                              x = window_end, xend = window_end,
                              y = 0, yend = temp_at_end,
                              linetype = "dashed", color = text_color, linewidth = 0.7) +
            ggplot2::annotate("point",
                              x = window_end, y = temp_at_end,
                              size = 3, color = text_color)
        }

        annotation_y_offset <- annotation_y_offset + 1

      } else if (!is.na(method_data$calc_time_sec[1])) {
        # Point methods (Tmax only now, since MHR handled above)
        calc_time <- method_data$calc_time_sec[1]

        # Find the deltaT value at this time for the selected downstream sensor
        temp_at_calc <- plot_data$deltaT[plot_data$time_sec == calc_time &
                                           plot_data$sensor == downstream_sensor]
        if (length(temp_at_calc) > 0) {
          temp_at_calc <- temp_at_calc[1]
        } else {
          temp_at_calc <- 0
        }

        # Create label - use unicode for delta and subscripts
        if (grepl("Tmax", method)) {
          label_text <- paste0("\u0394Tmax")  # ΔTmax
        } else {
          label_text <- method
        }

        p <- p +
          ggplot2::annotate("segment",
                            x = calc_time, xend = calc_time,
                            y = 0, yend = temp_at_calc,
                            linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
          ggplot2::annotate("point",
                            x = calc_time, y = temp_at_calc,
                            size = 3, color = "darkgreen") +
          ggplot2::annotate("text",
                            x = calc_time,
                            y = temp_at_calc * 1.08,
                            label = label_text,
                            size = 3, fontface = "bold",
                            color = "darkgreen")
      }
    }
  }

  return(p)
}

#' Plot Heat Pulse Velocity Time Series
#'
#' Creates a time series plot of heat pulse velocity calculated by different methods.
#' Provides interactive prompts if dates or methods are not specified. Can optionally
#' display quality flag markers and interpolated points for detailed QC visualisation.
#'
#' @param vh_results Results tibble from calc_heat_pulse_velocity()
#' @param start_date Start date/datetime (POSIXct) or numeric day number.
#'   If NULL, prompts user. Default: NULL
#' @param end_date End date/datetime (POSIXct) or numeric (days to plot from start).
#'   If NULL, prompts user. Default: NULL
#' @param methods Character vector of methods to plot. If NULL, prompts user or uses all.
#'   Default: NULL
#' @param sensor_position Sensor position to plot: "inner" or "outer". Default: "outer"
#' @param quality_filter Logical indicating whether to filter to "OK" quality only.
#'   Default: TRUE
#' @param show_quality_markers Logical indicating whether to display quality flag markers
#'   (outliers, missing data, etc.) on the plot. Default: FALSE
#' @param show_interpolated Logical indicating whether to highlight interpolated points
#'   (requires is_interpolated column from filter_and_interpolate_vh()). Default: FALSE
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Interactive mode - will prompt for dates and methods
#' plot_vh_timeseries(results)
#'
#' # Specify everything
#' plot_vh_timeseries(results,
#'                    start_date = as.POSIXct("2025-09-11"),
#'                    end_date = as.POSIXct("2025-09-18"),
#'                    methods = c("HRM", "MHR"),
#'                    sensor_position = "outer")
#'
#' # Use day numbers (plot days 7-14)
#' plot_vh_timeseries(results, start_date = 7, end_date = 14)
#'
#' # Start from day 7, plot 7 days
#' plot_vh_timeseries(results, start_date = 7, end_date = 7)
#'
#' # Show quality control markers
#' plot_vh_timeseries(results,
#'                    start_date = 7, end_date = 14,
#'                    quality_filter = FALSE,  # Include all quality flags
#'                    show_quality_markers = TRUE)
#'
#' # Highlight interpolated points (after cleaning)
#' cleaned <- filter_and_interpolate_vh(results, ...)
#' plot_vh_timeseries(cleaned,
#'                    show_interpolated = TRUE)
#' }
#'
#' @export
plot_vh_timeseries <- function(vh_results,
                               start_date = NULL,
                               end_date = NULL,
                               methods = NULL,
                               sensor_position = "outer",
                               quality_filter = TRUE,
                               show_quality_markers = FALSE,
                               show_interpolated = FALSE) {

  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }

  # Validate inputs
  if (!inherits(vh_results, "vh_results") && !inherits(vh_results, "data.frame")) {
    stop("vh_results must be a results tibble from calc_heat_pulse_velocity()")
  }

  if (!sensor_position %in% c("inner", "outer")) {
    stop("sensor_position must be 'inner' or 'outer'")
  }

  # Get date range from data
  data_start <- min(vh_results$datetime, na.rm = TRUE)
  data_end <- max(vh_results$datetime, na.rm = TRUE)
  total_days <- as.numeric(difftime(data_end, data_start, units = "days"))

  # Get available methods
  available_methods <- unique(vh_results$method)

  # Interactive prompts if parameters not specified
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

  if (end_date <= start_date) {
    stop("End date must be after start date")
  }

  # Select methods interactively if not specified
  if (is.null(methods) && interactive()) {
    cat("\nAvailable methods:\n")
    for (i in seq_along(available_methods)) {
      cat(sprintf("  [%d] %s\n", i, available_methods[i]))
    }
    cat("\nEnter method numbers (comma-separated, e.g., 1,2) or 'all': ")
    method_input <- readline()

    if (tolower(trimws(method_input)) == "all") {
      methods <- available_methods
    } else {
      method_indices <- as.numeric(strsplit(method_input, ",")[[1]])
      method_indices <- method_indices[!is.na(method_indices)]
      if (length(method_indices) == 0) {
        warning("No valid methods selected, using all methods")
        methods <- available_methods
      } else {
        methods <- available_methods[method_indices]
      }
    }
  } else if (is.null(methods)) {
    # Non-interactive: use all methods
    methods <- available_methods
  }

  # Validate methods
  invalid_methods <- setdiff(methods, available_methods)
  if (length(invalid_methods) > 0) {
    stop("Methods not found in data: ", paste(invalid_methods, collapse = ", "),
         "\nAvailable methods: ", paste(available_methods, collapse = ", "))
  }

  # Filter data
  selected_position <- sensor_position  # Avoid variable name shadowing
  filtered_data <- vh_results %>%
    dplyr::filter(
      datetime >= start_date,
      datetime <= end_date,
      method %in% methods,
      sensor_position == selected_position,
      !is.na(Vh_cm_hr)
    )

  # Apply quality filter if requested
  if (quality_filter) {
    filtered_data <- filtered_data %>%
      dplyr::filter(quality_flag == "OK")
  }

  # Check if any data remains
  if (nrow(filtered_data) == 0) {
    stop("No data available after filtering with specified parameters")
  }

  # Create plot
  p <- ggplot2::ggplot(filtered_data, ggplot2::aes(x = datetime, y = Vh_cm_hr, color = method)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = paste0("Heat Pulse Velocity Time Series: ", sensor_position, " sensor"),
      subtitle = paste0(format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d")),
      x = "Date/Time",
      y = expression(paste("V"[h], " (cm hr"^-1, ")")),
      color = "Method"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Add quality flag markers if requested
  if (show_quality_markers && "quality_flag" %in% names(vh_results)) {
    # Get non-OK quality flags from the displayed data range
    markers <- vh_results %>%
      dplyr::filter(
        datetime >= start_date,
        datetime <= end_date,
        method %in% methods,
        sensor_position == selected_position,
        quality_flag != "OK",
        !is.na(quality_flag)
      )

    if (nrow(markers) > 0) {
      # Define quality flag colours and shapes
      flag_colours <- c(
        "DATA_OUTLIER" = "#d62728",    # Red
        "DATA_SUSPECT" = "#ff7f0e",    # Orange
        "DATA_MISSING" = "#7f7f7f",    # Grey
        "DATA_ILLOGICAL" = "#8b0000",  # Dark red
        "CALC_FAILED" = "#9467bd",     # Purple
        "CALC_INFINITE" = "#e377c2",   # Pink
        "CALC_EXTREME" = "#bcbd22"     # Yellow-green
      )

      flag_shapes <- c(
        "DATA_OUTLIER" = 4,      # X
        "DATA_SUSPECT" = 18,     # Diamond
        "DATA_MISSING" = 17,     # Triangle
        "DATA_ILLOGICAL" = 15,   # Square
        "CALC_FAILED" = 1,       # Circle
        "CALC_INFINITE" = 8,     # Star
        "CALC_EXTREME" = 6       # Triangle down
      )

      # Create friendly labels for quality flags
      flag_labels <- c(
        "DATA_OUTLIER" = "Outlier",
        "DATA_SUSPECT" = "Suspect",
        "DATA_MISSING" = "Missing",
        "DATA_ILLOGICAL" = "Illogical",
        "CALC_FAILED" = "Calc Failed",
        "CALC_INFINITE" = "Calc Infinite",
        "CALC_EXTREME" = "Calc Extreme"
      )

      # Add a flag_label column for legend
      markers <- markers %>%
        dplyr::mutate(
          flag_label = dplyr::case_when(
            quality_flag %in% names(flag_labels) ~ flag_labels[quality_flag],
            TRUE ~ quality_flag
          ),
          # For DATA_MISSING, set Vh to 0 for plotting on x-axis
          Vh_plot = dplyr::if_else(quality_flag == "DATA_MISSING", 0, Vh_cm_hr)
        )

      # Add all quality markers as a single layer with proper legend
      p <- p + ggplot2::geom_point(
        data = markers,
        ggplot2::aes(x = datetime, y = Vh_plot, shape = flag_label, colour = flag_label),
        size = 2.5,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_shape_manual(
        name = "Quality Flags",
        values = setNames(flag_shapes[names(flag_shapes) %in% unique(markers$quality_flag)],
                         flag_labels[names(flag_shapes) %in% unique(markers$quality_flag)])
      ) +
      ggplot2::scale_colour_manual(
        name = "Quality Flags",
        values = setNames(flag_colours[names(flag_colours) %in% unique(markers$quality_flag)],
                         flag_labels[names(flag_colours) %in% unique(markers$quality_flag)])
      )
    }
  }

  # Add interpolated point markers if requested
  if (show_interpolated && "is_interpolated" %in% names(filtered_data)) {
    interp_points <- filtered_data %>%
      dplyr::filter(is_interpolated == TRUE)

    if (nrow(interp_points) > 0) {
      # Add hollow circles for interpolated points
      p <- p + ggplot2::geom_point(
        data = interp_points,
        ggplot2::aes(x = datetime, y = Vh_cm_hr, colour = method),
        shape = 1,  # Hollow circle
        size = 3,
        stroke = 1.5
      )
    }
  }

  # Print summary
  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("PLOT SUMMARY\n")
  cat(strrep("=", 67), "\n")
  cat(sprintf("Date range: %s to %s\n",
              format(start_date, "%Y-%m-%d %H:%M"),
              format(end_date, "%Y-%m-%d %H:%M")))
  cat(sprintf("Duration: %.1f days\n", as.numeric(difftime(end_date, start_date, units = "days"))))
  cat(sprintf("Methods: %s\n", paste(methods, collapse = ", ")))
  cat(sprintf("Sensor position: %s\n", sensor_position))
  cat(sprintf("Data points: %s\n", format(nrow(filtered_data), big.mark = ",")))

  # Show data availability per method
  cat("\nData points per method:\n")
  method_counts <- table(filtered_data$method)
  for (method in names(method_counts)) {
    cat(sprintf("  %-15s: %s\n", method, format(method_counts[method], big.mark = ",")))
  }
  cat(strrep("=", 67), "\n\n")

  return(p)
}


#' Parse Date Input from User
#'
#' Helper function to parse date input which can be a date string or number of days
#'
#' @param input Character string from user input
#' @param reference_date POSIXct reference date for numeric offsets
#' @return POSIXct datetime
#' @keywords internal
parse_date_input <- function(input, reference_date) {
  input <- trimws(input)

  # Try as numeric first (day number or number of days)
  num_input <- suppressWarnings(as.numeric(input))
  if (!is.na(num_input)) {
    return(reference_date + (num_input - 1) * 86400)
  }

  # Try as date string with explicit formats and timezone from reference
  tz <- attr(reference_date, "tzone")
  if (is.null(tz) || tz == "") tz <- "UTC"

  # Try multiple datetime formats
  formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")

  for (fmt in formats) {
    parsed_date <- tryCatch({
      as.POSIXct(input, format = fmt, tz = tz)
    }, error = function(e) {
      NULL
    })

    if (!is.null(parsed_date) && !is.na(parsed_date)) {
      return(parsed_date)
    }
  }

  stop("Could not parse date input: ", input,
       "\nExpected format: YYYY-MM-DD HH:MM:SS, YYYY-MM-DD HH:MM, or YYYY-MM-DD")
}


#' Plot Method Comparison Scatter
#'
#' Creates a scatter plot comparing two methods with 1:1 line and correlation.
#'
#' @param vh_results Results tibble from calc_heat_pulse_velocity()
#' @param method1 First method name
#' @param method2 Second method name
#' @param sensor_position Sensor position: "inner" or "outer". Default: "outer"
#' @param quality_filter Logical indicating whether to filter to "OK" quality only.
#'   Default: TRUE
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' plot_method_comparison(results, "HRM", "MHR")
#' plot_method_comparison(results, "HRM", "Tmax_Coh", sensor_position = "inner")
#' }
#'
#' @export
plot_method_comparison <- function(vh_results,
                                   method1,
                                   method2,
                                   sensor_position = "outer",
                                   quality_filter = TRUE) {

  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Install with: install.packages('tidyr')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }

  # Validate methods exist
  available_methods <- unique(vh_results$method)
  if (!method1 %in% available_methods) {
    stop("Method '", method1, "' not found in data.\nAvailable: ",
         paste(available_methods, collapse = ", "))
  }
  if (!method2 %in% available_methods) {
    stop("Method '", method2, "' not found in data.\nAvailable: ",
         paste(available_methods, collapse = ", "))
  }

  # Filter and reshape data
  selected_position <- sensor_position  # Avoid variable name shadowing
  filtered_data <- vh_results %>%
    dplyr::filter(
      method %in% c(method1, method2),
      sensor_position == selected_position,
      !is.na(Vh_cm_hr)
    )

  if (quality_filter) {
    filtered_data <- filtered_data %>%
      dplyr::filter(quality_flag == "OK")
  }

  # Reshape to wide format
  wide_data <- filtered_data %>%
    dplyr::select(pulse_id, datetime, method, Vh_cm_hr) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = Vh_cm_hr
    ) %>%
    tidyr::drop_na()

  if (nrow(wide_data) == 0) {
    stop("No overlapping data points between ", method1, " and ", method2)
  }

  # Calculate correlation and statistics
  cor_value <- cor(wide_data[[method1]], wide_data[[method2]], use = "complete.obs")
  rmse <- sqrt(mean((wide_data[[method1]] - wide_data[[method2]])^2))
  bias <- mean(wide_data[[method1]] - wide_data[[method2]])

  # Create plot
  axis_max <- max(c(wide_data[[method1]], wide_data[[method2]]), na.rm = TRUE)
  axis_min <- min(c(wide_data[[method1]], wide_data[[method2]]), na.rm = TRUE)

  p <- ggplot2::ggplot(wide_data, ggplot2::aes(x = .data[[method1]], y = .data[[method2]])) +
    ggplot2::geom_point(alpha = 0.5, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8) +
    ggplot2::coord_equal(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
    ggplot2::annotate("text", x = axis_min + (axis_max - axis_min) * 0.05,
                      y = axis_max - (axis_max - axis_min) * 0.05,
                      label = sprintf("r = %.3f\nRMSE = %.2f cm/hr\nBias = %.2f cm/hr\nn = %d",
                                      cor_value, rmse, bias, nrow(wide_data)),
                      hjust = 0, vjust = 1, size = 4) +
    ggplot2::labs(
      title = paste0("Method Comparison: ", method1, " vs ", method2),
      subtitle = paste0(sensor_position, " sensor"),
      x = paste0(method1, " (cm/hr)"),
      y = paste0(method2, " (cm/hr)")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11)
    )

  return(p)
}


# ==============================================================================
# sDMA PLOTTING - REMOVED
#
# plot_sdma_timeseries() has been temporarily extracted from the active workflow
# and parked in R/04j_sdma_methods.R for future implementation.
#
# FUTURE POSITION: After wound correction (Step 4), before flux density (Step 5)
# WILL VISUALIZE: Corrected velocities (Vc) with method switching
#
# The complete plotting implementation is preserved in the parking file and will
# be refined and re-integrated when sDMA is re-implemented.
#
# See: R/04j_sdma_methods.R for the complete parked implementation
# See: SDMA_EXTRACTION_STATUS.md for extraction status and plan
# ==============================================================================

# STUB: Informative error if user tries to call this function
#' @keywords internal
plot_sdma_timeseries <- function(...) {
  stop(
    "plot_sdma_timeseries() is temporarily disabled.\n\n",
    "sDMA functionality has been extracted from the active workflow and is being ",
    "re-implemented in a later workflow stage.\n\n",
    "FUTURE POSITION: sDMA will be available after wound correction and before ",
    "flux density calculation.\n\n",
    "The complete implementation is preserved in R/04j_sdma_methods.R and will be ",
    "re-integrated when the full workflow is complete.\n\n",
    "See SDMA_EXTRACTION_STATUS.md for details and timeline."
  )
}


#' Plot Pulse Temperature Traces (Basic Diagnostic)
#'
#' Simple diagnostic plot showing deltaT traces for all four sensors.
#' This is an internal helper function used for quick diagnostics.
#'
#' @param pulse_data Pulse data subset
#' @param deltaT_do Delta temperature for downstream outer sensor
#' @param deltaT_di Delta temperature for downstream inner sensor
#' @param deltaT_uo Delta temperature for upstream outer sensor
#' @param deltaT_ui Delta temperature for upstream inner sensor
#' @param pulse_id Pulse identifier
#' @keywords internal
plot_pulse_temps <- function(pulse_data, deltaT_do, deltaT_di, deltaT_uo, deltaT_ui, pulse_id) {
  if (!requireNamespace("graphics", quietly = TRUE)) {
    return()
  }

  graphics::plot(pulse_data$datetime, deltaT_do, type = "l", las = 1,
                 xlab = "Time", ylab = expression(paste(Delta, 'T')),
                 main = paste("Pulse", pulse_id),
                 xlim = range(pulse_data$datetime, na.rm = TRUE),
                 ylim = c(0, max(deltaT_do, deltaT_uo, deltaT_di, deltaT_ui, na.rm = TRUE)))
  graphics::lines(pulse_data$datetime, deltaT_uo, lty = 2)
  graphics::lines(pulse_data$datetime, deltaT_di, lty = 3)
  graphics::lines(pulse_data$datetime, deltaT_ui, lty = 4)
  graphics::legend("topleft", lty = c(1, 2, 3, 4), legend = c("DO", "UO", "DI", "UI"), bty = "n")
}
