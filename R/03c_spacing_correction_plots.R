# 03c_spacing_correction_plots.R
# Diagnostic Plotting Functions for Spacing Correction and k Estimation
#
# This file provides comprehensive plotting functions to visualise:
# - Zero-flow period identification and quality
# - Spacing correction impacts
# - Burgess coefficient lookup tables
# - Thermal diffusivity estimation
# - Temperature symmetry checks

#' Plot Zero-Flow Periods Diagnostic
#'
#' Creates a multi-panel diagnostic plot showing velocity time series with
#' zero-flow periods highlighted, allowing visual assessment of zero-flow
#' period selection quality.
#'
#' @param vh_data Data frame containing velocity data with columns:
#'   datetime, sensor_position, Vh_cm_hr
#' @param zero_periods List of zero-flow periods, each with 'start' and 'end'
#' @param sensor_position Character, which sensor to plot ("outer" or "inner")
#' @param method Character, velocity calculation method to filter (default "HRM")
#' @param show_stats Logical, whether to show summary statistics on plot
#' @param title Character, custom plot title (optional)
#'
#' @return A ggplot object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' zero_periods <- list(
#'   list(start = "2024-01-15 22:00:00", end = "2024-01-16 06:00:00")
#' )
#' plot_zero_flow_periods(vh_data, zero_periods, sensor_position = "outer")
#' }
plot_zero_flow_periods <- function(vh_data,
                                    zero_periods,
                                    sensor_position = c("outer", "inner"),
                                    method = "HRM",
                                    show_stats = TRUE,
                                    title = NULL) {

  # Validate inputs
  sensor_position <- match.arg(sensor_position)
  required_cols <- c("datetime", "sensor_position", "Vh_cm_hr")
  if (!all(required_cols %in% names(vh_data))) {
    stop("vh_data must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install with: install.packages('ggplot2')")
  }

  # Filter data
  plot_data <- vh_data[vh_data$sensor_position == sensor_position, ]
  if ("method" %in% names(plot_data)) {
    plot_data <- plot_data[plot_data$method == method, ]
  }

  # Convert zero periods to rectangles for shading
  zero_rects <- data.frame()
  for (i in seq_along(zero_periods)) {
    period <- zero_periods[[i]]

    # Simple conversion - no timezone handling needed
    # Data and period definitions are both in local time
    start_time <- if (inherits(period$start, "POSIXct")) {
      period$start
    } else {
      as.POSIXct(period$start, format = "%Y-%m-%d %H:%M:%S")
    }

    end_time <- if (inherits(period$end, "POSIXct")) {
      period$end
    } else {
      as.POSIXct(period$end, format = "%Y-%m-%d %H:%M:%S")
    }

    zero_rects <- rbind(zero_rects, data.frame(
      xmin = start_time,
      xmax = end_time,
      period_id = i
    ))
  }

  # Calculate zero-flow statistics if requested
  stats_text <- NULL
  if (show_stats && nrow(zero_rects) > 0) {
    zero_data <- data.frame()
    for (i in 1:nrow(zero_rects)) {
      period_data <- plot_data[
        plot_data$datetime >= zero_rects$xmin[i] &
        plot_data$datetime <= zero_rects$xmax[i],
      ]
      if (nrow(period_data) > 0) {
        zero_data <- rbind(zero_data, period_data)
      }
    }

    if (nrow(zero_data) > 0) {
      mean_zero <- mean(zero_data$Vh_cm_hr, na.rm = TRUE)
      sd_zero <- sd(zero_data$Vh_cm_hr, na.rm = TRUE)
      cv_zero <- sd_zero / mean_zero * 100

      stats_text <- sprintf(
        "Zero-flow Vh: %.2f ± %.2f cm/hr (CV: %.1f%%)\nn = %d observations",
        mean_zero, sd_zero, cv_zero, nrow(zero_data)
      )
    }
  }

  # Create title
  if (is.null(title)) {
    title <- sprintf("Zero-Flow Period Diagnostic - %s Sensor (%s)",
                     toupper(sensor_position), method)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = Vh_cm_hr)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.5) +
    ggplot2::labs(
      title = title,
      x = "Date/Time",
      y = "Velocity (cm/hr)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add zero-flow period shading
  if (nrow(zero_rects) > 0) {
    p <- p + ggplot2::geom_rect(
      data = zero_rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "yellow", alpha = 0.2, inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = zero_rects,
      ggplot2::aes(xintercept = xmin),
      color = "orange", linetype = "dashed", linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      data = zero_rects,
      ggplot2::aes(xintercept = xmax),
      color = "orange", linetype = "dashed", linewidth = 0.5
    )
  }

  # Add statistics annotation
  if (!is.null(stats_text)) {
    p <- p + ggplot2::annotate(
      "text",
      x = min(plot_data$datetime, na.rm = TRUE),
      y = max(plot_data$Vh_cm_hr, na.rm = TRUE),
      label = stats_text,
      hjust = 0, vjust = 1,
      size = 3.5,
      color = "darkred",
      fontface = "bold"
    )
  }

  return(p)
}


#' Plot Before/After Spacing Correction Comparison
#'
#' Creates a multi-panel plot comparing velocity data before and after spacing
#' correction, showing the impact of the correction.
#'
#' @param spacing_result A spacing_correction_result object from
#'   \code{\link{apply_spacing_correction_workflow}}
#' @param sensor_position Character, which sensor to plot ("outer" or "inner")
#' @param date_range Optional vector of two POSIXct dates to zoom into
#' @param show_difference Logical, whether to include difference panel (default TRUE)
#'
#' @return A ggplot object (or patchwork composite if available)
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' result <- apply_spacing_correction_workflow(vh_data, zero_periods)
#' plot_spacing_correction_comparison(result, sensor_position = "outer")
#' }
plot_spacing_correction_comparison <- function(spacing_result,
                                                sensor_position = c("outer", "inner"),
                                                date_range = NULL,
                                                show_difference = TRUE) {

  # Validate inputs
  sensor_position <- match.arg(sensor_position)
  if (!inherits(spacing_result, "spacing_correction_result")) {
    stop("spacing_result must be a spacing_correction_result object")
  }

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Extract data
  data <- spacing_result$vh_corrected
  data <- data[data$sensor_position == sensor_position, ]

  # Apply date range filter if specified
  if (!is.null(date_range)) {
    data <- data[data$datetime >= date_range[1] & data$datetime <= date_range[2], ]
  }

  if (nrow(data) == 0) {
    stop("No data available for the specified sensor/date range")
  }

  # Check if correction was applied
  if (!"Vh_cm_hr_sc" %in% names(data)) {
    warning("No spacing correction column found (Vh_cm_hr_sc). Showing original only.")
    show_difference <- FALSE
  }

  # Prepare data for plotting
  if ("Vh_cm_hr_sc" %in% names(data)) {
    plot_data <- data.frame(
      datetime = rep(data$datetime, 2),
      Vh = c(data$Vh_cm_hr, data$Vh_cm_hr_sc),
      Type = rep(c("Original", "Corrected"), each = nrow(data))
    )
  } else {
    plot_data <- data.frame(
      datetime = data$datetime,
      Vh = data$Vh_cm_hr,
      Type = "Original"
    )
  }

  # Calculate summary statistics
  coef_info <- spacing_result$correction_coefficients[[sensor_position]]
  stats_label <- sprintf(
    "Correction: Vh_corr = %.3f × Vh + %.2f\nZero offset: %.2f cm/hr",
    coef_info$coef_a, coef_info$coef_b, coef_info$zero_vh
  )

  # Create comparison plot
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = datetime, y = Vh, color = Type)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_color_manual(
      values = c("Original" = "steelblue", "Corrected" = "darkgreen")
    ) +
    ggplot2::labs(
      title = sprintf("Spacing Correction Comparison - %s Sensor",
                      toupper(sensor_position)),
      subtitle = stats_label,
      x = "Date/Time",
      y = "Velocity (cm/hr)",
      color = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "top"
    )

  # Create difference plot if requested
  if (show_difference && "Vh_cm_hr_sc" %in% names(data)) {
    diff_data <- data.frame(
      datetime = data$datetime,
      difference = data$Vh_cm_hr_sc - data$Vh_cm_hr,
      rel_diff_pct = (data$Vh_cm_hr_sc - data$Vh_cm_hr) / data$Vh_cm_hr * 100
    )

    p2 <- ggplot2::ggplot(diff_data, ggplot2::aes(x = datetime, y = difference)) +
      ggplot2::geom_line(color = "darkred", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = "Correction Impact",
        x = "Date/Time",
        y = "Difference (Corrected - Original, cm/hr)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

    # Try to combine with patchwork if available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(p1 / p2 + patchwork::plot_layout(heights = c(2, 1)))
    } else {
      message("Install 'patchwork' package for combined plot layout")
      return(p1)
    }
  }

  return(p1)
}


#' Plot Burgess Correction Coefficients Lookup
#'
#' Visualises the Burgess et al. (2001) correction coefficient lookup table,
#' showing how coefficients vary with zero-flow offset. Optionally highlights
#' where your data falls on the curves.
#'
#' @param lookup_table Burgess lookup table from
#'   \code{\link{calculate_burgess_coefficients}}
#' @param zero_vh_observed Optional numeric vector of observed zero offsets to
#'   highlight on the plot
#' @param labels Optional character vector of labels for observed points
#'
#' @return A ggplot object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' lookup <- calculate_burgess_coefficients()
#' plot_burgess_coefficients(lookup, zero_vh_observed = c(0.5, 1.2))
#' }
plot_burgess_coefficients <- function(lookup_table,
                                       zero_vh_observed = NULL,
                                       labels = NULL) {

  # Validate input
  if (!inherits(lookup_table, "burgess_lookup")) {
    stop("lookup_table must be a burgess_lookup object")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Get parameters from attributes
  k_val <- attr(lookup_table, "k_assumed")
  x_val <- attr(lookup_table, "x_nominal")
  t_val <- attr(lookup_table, "t_measurement")

  subtitle <- sprintf(
    "k = %.4f cm²/s  |  x = %.1f cm  |  t = %d s",
    k_val, x_val, t_val
  )

  # Create long format for plotting
  plot_data <- data.frame(
    zero_vh = rep(lookup_table$zero_vh, 2),
    value = c(lookup_table$coef_a, lookup_table$coef_b),
    coefficient = rep(c("a (slope)", "b (offset)"), each = nrow(lookup_table)),
    range_type = rep(lookup_table$range_type, 2)
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = zero_vh, y = value, color = coefficient)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(-5, 5), linetype = "dotted", color = "gray50") +
    ggplot2::annotate(
      "rect",
      xmin = -5, xmax = 5, ymin = -Inf, ymax = Inf,
      fill = "green", alpha = 0.05
    ) +
    ggplot2::annotate(
      "text", x = 0, y = max(plot_data$value, na.rm = TRUE),
      label = "Modeled range", size = 3, color = "darkgreen", fontface = "italic"
    ) +
    ggplot2::scale_color_manual(
      values = c("a (slope)" = "steelblue", "b (offset)" = "darkred")
    ) +
    ggplot2::labs(
      title = "Burgess Correction Coefficients Lookup",
      subtitle = subtitle,
      x = "Zero-flow Vh offset (cm/hr)",
      y = "Coefficient value",
      color = "Coefficient",
      caption = "Vh_corrected = a × Vh + b"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "top"
    )

  # Add observed data points if provided
  if (!is.null(zero_vh_observed)) {
    # Get coefficients for observed offsets
    obs_points <- data.frame()
    for (i in seq_along(zero_vh_observed)) {
      coefs <- get_correction_coefficients(zero_vh_observed[i], lookup_table)
      obs_points <- rbind(obs_points, data.frame(
        zero_vh = zero_vh_observed[i],
        coef_a = coefs$coef_a,
        coef_b = coefs$coef_b,
        label = if (!is.null(labels) && i <= length(labels)) labels[i] else paste0("Obs_", i)
      ))
    }

    # Add points for both coefficients
    obs_plot <- data.frame(
      zero_vh = rep(obs_points$zero_vh, 2),
      value = c(obs_points$coef_a, obs_points$coef_b),
      coefficient = rep(c("a (slope)", "b (offset)"), each = nrow(obs_points)),
      label = rep(obs_points$label, 2)
    )

    p <- p +
      ggplot2::geom_point(
        data = obs_plot,
        ggplot2::aes(x = zero_vh, y = value, color = coefficient),
        size = 3, shape = 21, fill = "white", stroke = 2
      ) +
      ggplot2::geom_text(
        data = obs_plot,
        ggplot2::aes(x = zero_vh, y = value, label = label),
        vjust = -1, size = 3, fontface = "bold", color = "black"
      )
  }

  return(p)
}


#' Plot Temperature Traces for k Estimation
#'
#' Visualises heat pulse temperature traces during zero-flow periods, showing
#' time-to-maximum temperature (Tmax) identification and symmetry between
#' upstream and downstream probes.
#'
#' @param heat_pulse_data Heat pulse data object from
#'   \code{\link{read_heat_pulse_data}}
#' @param zero_periods List of zero-flow periods
#' @param sensor_position Character, which sensor ("outer" or "inner")
#' @param n_pulses Integer, maximum number of pulses to plot (default 10)
#' @param pre_pulse Numeric, pre-pulse baseline period in seconds (default 30)
#' @param facet Logical, whether to facet by pulse_id (default TRUE)
#'
#' @return A ggplot object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' plot_temperature_traces(
#'   heat_pulse_data,
#'   zero_periods = list(list(start = "2024-01-15 22:00:00",
#'                            end = "2024-01-16 06:00:00")),
#'   sensor_position = "outer",
#'   n_pulses = 5
#' )
#' }
plot_temperature_traces <- function(heat_pulse_data,
                                     zero_periods,
                                     sensor_position = c("outer", "inner"),
                                     n_pulses = 10,
                                     pre_pulse = 30,
                                     facet = TRUE) {

  # Validate inputs
  sensor_position <- match.arg(sensor_position)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Extract zero-flow temperatures
  zero_temps <- extract_zero_flow_temperatures(
    heat_pulse_data = heat_pulse_data,
    zero_periods = zero_periods,
    sensor_position = sensor_position
  )

  if (is.null(zero_temps) || nrow(zero_temps) == 0) {
    stop("No temperature data found in zero-flow periods")
  }

  # Limit to first n_pulses
  unique_pulses <- unique(zero_temps$pulse_id)
  if (length(unique_pulses) > n_pulses) {
    selected_pulses <- unique_pulses[1:n_pulses]
    zero_temps <- zero_temps[zero_temps$pulse_id %in% selected_pulses, ]
  }

  # Calculate time-to-max for each pulse
  tmax_results <- calculate_time_to_max_temperatures(zero_temps, pre_pulse = pre_pulse)

  # Create relative time column (seconds from start of each pulse)
  zero_temps <- zero_temps[order(zero_temps$pulse_id, zero_temps$datetime), ]
  zero_temps$time_rel <- ave(
    as.numeric(zero_temps$datetime),
    zero_temps$pulse_id,
    FUN = function(x) x - min(x)
  )

  # Select temperature columns based on sensor position
  temp_cols <- if (sensor_position == "outer") c("do", "uo") else c("di", "ui")

  # Reshape to long format
  plot_data <- data.frame()
  for (col in temp_cols) {
    probe_type <- if (grepl("^d", col)) "Downstream" else "Upstream"
    plot_data <- rbind(plot_data, data.frame(
      pulse_id = zero_temps$pulse_id,
      time_rel = zero_temps$time_rel,
      temperature = zero_temps[[col]],
      probe = probe_type
    ))
  }

  # Add Tmax markers (only if tmax_results has valid data)
  tmax_data <- NULL
  if (!is.null(tmax_results) && "pulse_id" %in% names(tmax_results) &&
      length(tmax_results$pulse_id) > 0) {
    tmax_data <- data.frame(
      pulse_id = rep(tmax_results$pulse_id, 2),
      time_tmax = c(tmax_results$t_max_downstream, tmax_results$t_max_upstream),
      probe = rep(c("Downstream", "Upstream"), each = length(tmax_results$pulse_id))
    )
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time_rel, y = temperature, color = probe)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = pre_pulse, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    ggplot2::scale_color_manual(
      values = c("Downstream" = "red", "Upstream" = "blue")
    ) +
    ggplot2::labs(
      title = sprintf("Temperature Traces During Zero Flow - %s Sensor", toupper(sensor_position)),
      subtitle = "Identifying time to maximum temperature (Tmax)",
      x = "Time since pulse start (seconds)",
      y = "Temperature (°C)",
      color = "Probe"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "top"
    )

  # Add Tmax markers
  if (!is.null(tmax_data) && nrow(tmax_data) > 0) {
    p <- p + ggplot2::geom_vline(
      data = tmax_data,
      ggplot2::aes(xintercept = time_tmax, color = probe),
      linetype = "dotted", linewidth = 1, alpha = 0.7
    )
  }

  # Facet by pulse if requested
  if (facet && length(unique(plot_data$pulse_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~ pulse_id, scales = "free_y", ncol = 2)
  }

  return(p)
}


#' Plot k Estimation Summary
#'
#' Creates a diagnostic plot summarising thermal diffusivity (k) estimation
#' results, showing k by sensor, symmetry checks, and comparison to nominal k.
#'
#' @param k_result A k_estimation_result object from
#'   \code{\link{estimate_k_from_tmax}}
#' @param show_individual Logical, whether to show individual pulse estimates
#'   (default FALSE)
#'
#' @return A ggplot object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' k_result <- estimate_k_from_tmax(heat_pulse_data, zero_periods)
#' plot_k_estimation_summary(k_result)
#' }
plot_k_estimation_summary <- function(k_result,
                                       show_individual = FALSE) {

  # Validate input
  if (!inherits(k_result, "k_estimation_result")) {
    stop("k_result must be a k_estimation_result object")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Prepare data for sensor comparison
  k_by_sensor <- k_result$k_by_sensor
  sensor_names <- names(k_by_sensor)

  plot_data <- data.frame(
    sensor = factor(toupper(sensor_names), levels = toupper(sensor_names)),
    k_value = unlist(k_by_sensor),
    k_nominal = k_result$k_nominal
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sensor, y = k_value)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = k_nominal),
      linetype = "dashed", color = "darkred", linewidth = 1
    ) +
    ggplot2::annotate(
      "text",
      x = 1,
      y = k_result$k_nominal,
      label = sprintf("Nominal k = %.4f", k_result$k_nominal),
      vjust = -0.5, hjust = 0, color = "darkred", fontface = "bold"
    ) +
    ggplot2::labs(
      title = "Thermal Diffusivity (k) Estimation Summary",
      subtitle = sprintf(
        "Mean k = %.4f cm²/s (%.1f%% %s from nominal)",
        k_result$k_mean,
        abs(k_result$k_difference_percent),
        if (k_result$k_difference_percent > 0) "higher" else "lower"
      ),
      x = "Sensor",
      y = "k (cm²/s)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )

  # Add error bars if SD is available
  if (!is.null(k_result$k_sd) && !is.na(k_result$k_sd) && k_result$k_sd > 0) {
    plot_data$k_sd <- k_result$k_sd / sqrt(length(k_by_sensor))  # SEM approximation
    p <- p + ggplot2::geom_errorbar(
      data = plot_data,
      ggplot2::aes(ymin = k_value - k_sd, ymax = k_value + k_sd),
      width = 0.2, linewidth = 0.8
    )
  }

  # Add recommendation annotation
  rec_color <- switch(
    k_result$recommendation,
    "no_action" = "darkgreen",
    "reprocess_optional" = "darkorange",
    "reprocess_recommended" = "red",
    "reprocess_critical" = "darkred",
    "black"
  )

  rec_label <- switch(
    k_result$recommendation,
    "no_action" = "✓ No reprocessing needed",
    "reprocess_optional" = "ℹ Reprocessing optional",
    "reprocess_recommended" = "⚠ Reprocessing recommended",
    "reprocess_critical" = "⚠ Reprocessing critical",
    "Unknown recommendation"
  )

  p <- p + ggplot2::annotate(
    "text",
    x = length(sensor_names),
    y = min(plot_data$k_value, na.rm = TRUE),
    label = rec_label,
    hjust = 1, vjust = 0,
    color = rec_color,
    fontface = "bold",
    size = 4
  )

  return(p)
}


#' Plot Symmetry Check Results
#'
#' Visualises the symmetry between upstream and downstream time-to-maximum
#' temperatures, which should be equal at zero flow. Deviations indicate either
#' non-zero flow or probe issues.
#'
#' @param k_result A k_estimation_result object from
#'   \code{\link{estimate_k_from_tmax}}
#'
#' @return A ggplot object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' k_result <- estimate_k_from_tmax(heat_pulse_data, zero_periods)
#' plot_symmetry_check(k_result)
#' }
plot_symmetry_check <- function(k_result) {

  # Validate input
  if (!inherits(k_result, "k_estimation_result")) {
    stop("k_result must be a k_estimation_result object")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Extract symmetry results
  symmetry_results <- k_result$symmetry_results
  if (is.null(symmetry_results) || length(symmetry_results) == 0) {
    stop("No symmetry results found in k_result")
  }

  # Prepare data
  plot_data <- data.frame()
  for (sensor_name in names(symmetry_results)) {
    sym <- symmetry_results[[sensor_name]]
    plot_data <- rbind(plot_data, data.frame(
      sensor = toupper(sensor_name),
      t_max_downstream = sym$t_max_downstream,
      t_max_upstream = sym$t_max_upstream,
      difference = sym$difference_seconds,
      status = sym$status
    ))
  }

  # Create scatter plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = t_max_downstream, y = t_max_upstream)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(
      ggplot2::aes(color = status),
      size = 4, alpha = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sensor),
      vjust = -1, fontface = "bold", size = 3.5
    ) +
    ggplot2::scale_color_manual(
      values = c("PASS" = "darkgreen", "CAUTION" = "orange", "FAIL" = "red")
    ) +
    ggplot2::labs(
      title = "Tmax Symmetry Check (Zero Flow)",
      subtitle = "Points should fall on 1:1 line if flow is truly zero",
      x = "Downstream Tmax (seconds)",
      y = "Upstream Tmax (seconds)",
      color = "Status"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "top"
    )

  return(p)
}


#' Create Combined Spacing Correction Diagnostic Report
#'
#' Generates a comprehensive multi-panel diagnostic report combining all key
#' visualisations for spacing correction assessment.
#'
#' @param spacing_result Spacing correction result object
#' @param vh_data Original velocity data
#' @param zero_periods List of zero-flow periods
#' @param sensor_position Which sensor to plot ("outer" or "inner")
#'
#' @return A patchwork composite plot (or list of plots if patchwork unavailable)
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' report <- plot_spacing_correction_report(
#'   spacing_result,
#'   vh_data,
#'   zero_periods,
#'   sensor_position = "outer"
#' )
#' print(report)
#' }
plot_spacing_correction_report <- function(spacing_result,
                                            vh_data,
                                            zero_periods,
                                            sensor_position = c("outer", "inner")) {

  sensor_position <- match.arg(sensor_position)

  # Generate individual plots
  p1 <- plot_zero_flow_periods(
    vh_data,
    zero_periods,
    sensor_position = sensor_position,
    show_stats = TRUE
  )

  p2 <- plot_spacing_correction_comparison(
    spacing_result,
    sensor_position = sensor_position,
    show_difference = FALSE
  )

  # Get lookup table for Burgess plot
  lookup <- spacing_result$metadata$lookup_table
  if (is.null(lookup)) {
    lookup <- calculate_burgess_coefficients(
      k = spacing_result$metadata$k_assumed,
      x = spacing_result$metadata$probe_spacing
    )
  }

  coef_info <- spacing_result$correction_coefficients[[sensor_position]]
  p3 <- plot_burgess_coefficients(
    lookup,
    zero_vh_observed = coef_info$zero_vh,
    labels = toupper(sensor_position)
  )

  # Combine with patchwork if available
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- (p1 / p2) | p3
    combined <- combined +
      patchwork::plot_annotation(
        title = "Spacing Correction Diagnostic Report",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"))
      )
    return(combined)
  } else {
    message("Install 'patchwork' for combined layout. Returning list of plots.")
    return(list(
      zero_flow_periods = p1,
      correction_comparison = p2,
      burgess_coefficients = p3
    ))
  }
}


#' Plot Interactive Changepoint Detection with Daily Minima
#'
#' Creates an interactive plotly visualization showing daily minimum velocities
#' with detected changepoints as vertical dashed lines. Supports zooming and
#' panning for visual assessment of baseline shifts.
#'
#' @param daily_min Data frame with columns \code{date} and \code{min_value}
#' @param changepoints Vector of changepoint dates (Date class), or NULL for no changepoints
#' @param segments Optional data frame of segments with baseline values. If NULL and
#'   changepoints are provided, segments will be auto-generated from changepoints.
#' @param vh_data Optional full velocity data frame for overlay (must have datetime and Vh_cm_hr)
#' @param title Character, plot title (optional)
#' @param show_baseline_values Logical, whether to show baseline values for each segment (default: TRUE)
#' @param show_original_data Logical, whether to overlay original velocity data (default: FALSE)
#'
#' @return A plotly object
#'
#' @family spacing correction plots
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate daily minima
#' daily_min <- calculate_daily_minima(vh_results)
#'
#' # Detect changepoints
#' cpt_result <- detect_changepoints(daily_min)
#'
#' # Get segments with baselines
#' segments <- extract_segment_baselines(cpt_result)
#'
#' # Create interactive plot
#' plot_changepoints_interactive(
#'   daily_min = cpt_result$daily_min_with_segments,
#'   changepoints = cpt_result$changepoints,
#'   segments = segments
#' )
#' }
plot_changepoints_interactive <- function(daily_min,
                                           changepoints = NULL,
                                           segments = NULL,
                                           vh_data = NULL,
                                           title = "Daily Minimum Velocities with Changepoints",
                                           show_baseline_values = TRUE,
                                           show_original_data = FALSE) {

  # Check for plotly
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly is required for interactive plotting. Install with: install.packages('plotly')")
  }

  # Validate inputs
  if (!is.data.frame(daily_min)) {
    stop("daily_min must be a data frame")
  }

  required_cols <- c("date", "min_value")
  missing_cols <- setdiff(required_cols, names(daily_min))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure date is Date class
  if (!inherits(daily_min$date, "Date")) {
    daily_min$date <- as.Date(daily_min$date)
  }

  # Auto-generate segments from changepoints if not provided
  if (is.null(segments) && !is.null(changepoints) && length(changepoints) > 0) {
    message(sprintf("Auto-generating segments from %d changepoints", length(changepoints)))

    # Create segments: changepoints divide the data into n+1 segments
    n_cpts <- length(changepoints)
    segment_starts <- c(min(daily_min$date), changepoints)
    segment_ends <- c(changepoints, max(daily_min$date))

    segments <- data.frame(
      segment_id = seq_len(n_cpts + 1),
      start_date = segment_starts,
      end_date = segment_ends,
      stringsAsFactors = FALSE
    )

    # Calculate baseline for each segment
    segments$baseline_value <- NA_real_
    segments$n_days <- NA_integer_

    for (i in seq_len(nrow(segments))) {
      seg_data <- daily_min[daily_min$date >= segments$start_date[i] &
                            daily_min$date <= segments$end_date[i], ]
      if (nrow(seg_data) > 0) {
        segments$baseline_value[i] <- min(seg_data$min_value, na.rm = TRUE)
        segments$n_days[i] <- as.integer(diff(range(seg_data$date))) + 1
      }
    }

    message(sprintf("Auto-generated %d segments with baselines", nrow(segments)))
  }

  # Create base plotly figure
  fig <- plotly::plot_ly()

  # Add original data overlay if requested
  if (show_original_data && !is.null(vh_data)) {
    # Ensure datetime column exists
    if ("datetime" %in% names(vh_data) && "Vh_cm_hr" %in% names(vh_data)) {
      vh_data$date_only <- as.Date(vh_data$datetime)

      fig <- fig %>%
        plotly::add_trace(
          data = vh_data,
          x = ~datetime,
          y = ~Vh_cm_hr,
          type = "scatter",
          mode = "markers",
          name = "All Data Points",
          marker = list(size = 2, color = "gray", opacity = 0.5),
          hovertemplate = paste(
            "<b>DateTime:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Vh:</b> %{y:.2f} cm/hr<br>",
            "<extra></extra>"
          )
        )
    }
  }

  # Add daily minima line
  fig <- fig %>%
    plotly::add_trace(
      data = daily_min,
      x = ~date,
      y = ~min_value,
      type = "scatter",
      mode = "lines+markers",
      name = "Daily Minimum Vh",
      line = list(color = "steelblue", width = 2),
      marker = list(size = 4, color = "steelblue"),
      hovertemplate = paste(
        "<b>Date:</b> %{x|%Y-%m-%d}<br>",
        "<b>Min Vh:</b> %{y:.2f} cm/hr<br>",
        "<extra></extra>"
      )
    )

  # Add changepoint vertical lines if provided
  if (!is.null(changepoints) && length(changepoints) > 0) {
    message(sprintf("Adding %d changepoint vertical lines", length(changepoints)))

    for (i in seq_along(changepoints)) {
      cpt_date <- changepoints[i]

      fig <- fig %>%
        plotly::add_trace(
          x = c(cpt_date, cpt_date),
          y = c(min(daily_min$min_value, na.rm = TRUE), max(daily_min$min_value, na.rm = TRUE)),
          type = "scatter",
          mode = "lines",
          name = if (i == 1) "Changepoints" else NULL,
          line = list(color = "red", width = 2, dash = "dash"),
          showlegend = if (i == 1) TRUE else FALSE,
          hovertemplate = paste(
            "<b>Changepoint</b><br>",
            "Date: ", format(cpt_date, "%Y-%m-%d"), "<br>",
            "<extra></extra>"
          )
        )
    }
  }

  # Add segment baseline values as horizontal lines if provided
  if (!is.null(segments) && show_baseline_values && "baseline_value" %in% names(segments)) {
    message(sprintf("Adding %d baseline horizontal lines", nrow(segments)))

    for (i in seq_len(nrow(segments))) {
      seg <- segments[i, ]

      fig <- fig %>%
        plotly::add_trace(
          x = c(seg$start_date, seg$end_date),
          y = c(seg$baseline_value, seg$baseline_value),
          type = "scatter",
          mode = "lines",
          name = if (i == 1) "Segment Baselines" else NULL,
          line = list(color = "darkgreen", width = 1.5, dash = "dot"),
          showlegend = if (i == 1) TRUE else FALSE,
          hovertemplate = paste(
            "<b>Segment", i, "Baseline</b><br>",
            "Baseline: ", round(seg$baseline_value, 2), " cm/hr<br>",
            "Period: ", format(seg$start_date, "%Y-%m-%d"), " to ",
            format(seg$end_date, "%Y-%m-%d"), "<br>",
            "Days: ", seg$n_days, "<br>",
            "<extra></extra>"
          )
        )
    }
  }

  # Configure layout
  fig <- fig %>%
    plotly::layout(
      title = list(text = title, font = list(size = 16, weight = "bold")),
      xaxis = list(
        title = "Date",
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      yaxis = list(
        title = "Velocity (cm/hr)",
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      hovermode = "closest",
      dragmode = "zoom",
      legend = list(
        x = 0.02,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)"
      )
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
      displaylogo = FALSE
    ) %>%
    plotly::event_register("plotly_click")

  # Set source for click events (needed for Shiny)
  fig$x$source <- "changepoint_plot"

  return(fig)
}
