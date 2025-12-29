# R/04b_sdma_enhanced.R
# Enhanced sDMA Implementation - "Smart" Method Switching
#
# Implements thesis-ready Selectable Dual Method Approach (sDMA) with:
# - Empirical (velocity-based) AND theoretical (Peclet-based) switching
# - Optional calibration to eliminate "volcano effect" discontinuities
# - Intelligent auto-selection that only switches when necessary
#
# References:
# - Burgess et al. (2001) for theoretical Peclet limits
# - Tim's Excel template for empirical calibration approach
#
# ==============================================================================


#' Recalculate Peclet Number Using Corrected Velocities
#'
#' Recalculates the Peclet number (Pe) using corrected sap velocities after
#' spacing or wound corrections have been applied. This is CRITICAL for sDMA
#' because the Peclet number determines the theoretical validity limit of HRM,
#' and corrections can significantly change velocity values.
#'
#' @param vh_data Data frame or tibble containing heat pulse velocity results.
#'   Must have HRM velocities and optionally corrected velocities (Vc_cm_hr or
#'   Vh_cm_hr_wc column).
#' @param probe_config Probe configuration object or list with \code{probe_spacing}
#'   (cm). If NULL, will attempt to extract from existing Peclet calculation.
#' @param wood_properties Wood properties object or list with
#'   \code{thermal_diffusivity} (cm²/s). If NULL, will attempt to extract from
#'   existing Peclet calculation.
#' @param velocity_col Character. Name of corrected velocity column to use.
#'   Default: NULL (auto-detect). Will search for "Vh_cm_hr_wc" (wound-corrected),
#'   "Vh_cm_hr_sc" (spacing-corrected), "Vc_cm_hr" (generic corrected), or fall
#'   back to "Vh_cm_hr" (raw).
#' @param peclet_col Character. Name of existing Peclet column to use for
#'   parameter extraction if probe_config/wood_properties not provided.
#'   Default: "hrm_peclet_number".
#' @param output_col Character. Name for output Peclet column.
#'   Default: "Pe_corrected".
#' @param method Character. Which method to recalculate Peclet for.
#'   Default: "HRM".
#' @param verbose Logical. Print calculation details (default: TRUE).
#'
#' @return The input data frame with an additional column containing recalculated
#'   Peclet numbers. The column name is specified by \code{output_col}.
#'
#' @details
#' **Why Recalculation is Critical:**
#'
#' The Peclet number is defined as:
#' \deqn{Pe = \frac{V_c \cdot x}{D}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{V_c} = sap velocity (cm/s)
#'   \item \eqn{x} = probe spacing (cm)
#'   \item \eqn{D} = thermal diffusivity (cm²/s)
#' }
#'
#' When spacing or wound corrections are applied, \eqn{V_c} changes, invalidating
#' the original Peclet number. Using the original (raw) Peclet could:
#' \itemize{
#'   \item Falsely indicate low flow (Pe < 1) when corrected flow is high
#'   \item Incorrectly trigger sDMA switching
#'   \item Violate HRM validity assumptions
#' }
#'
#' **Parameter Extraction:**
#'
#' If \code{probe_config} or \code{wood_properties} are not provided, the function
#' will reverse-calculate them from an existing Peclet column using:
#' \deqn{\frac{x}{D} = \frac{Pe_{raw}}{V_{raw}}}
#'
#' This assumes the original Peclet was correctly calculated.
#'
#' **Auto-Detection of Corrected Velocity:**
#'
#' The function searches for corrected velocity columns in this order:
#' \enumerate{
#'   \item \code{Vh_cm_hr_wc} - Wound-corrected (highest priority)
#'   \item \code{Vh_cm_hr_sc} - Spacing-corrected
#'   \item \code{Vc_cm_hr} - Generic corrected
#'   \item \code{Vh_cm_hr} - Raw velocity (fallback)
#' }
#'
#' @examples
#' \dontrun{
#' # After wound correction, recalculate Peclet
#' vh_corrected <- apply_wound_correction(vh_results, ...)
#' vh_corrected <- recalculate_peclet(
#'   vh_data = vh_corrected,
#'   probe_config = probe_config,
#'   wood_properties = wood_properties
#' )
#'
#' # Use recalculated Peclet for sDMA
#' vh_sdma <- apply_sdma_switching(
#'   data = vh_corrected,
#'   secondary = "MHR",
#'   mode = "peclet",
#'   peclet_col = "Pe_corrected"
#' )
#' }
#'
#' @family sDMA functions
#' @export
recalculate_peclet <- function(vh_data,
                               probe_config = NULL,
                               wood_properties = NULL,
                               velocity_col = NULL,
                               peclet_col = "hrm_peclet_number",
                               output_col = "Pe_corrected",
                               method = "HRM",
                               verbose = TRUE) {

  # Validate input
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame or tibble")
  }

  if (!"method" %in% names(vh_data)) {
    stop("vh_data must have a 'method' column")
  }

  # Auto-detect corrected velocity column if not specified
  if (is.null(velocity_col)) {
    velocity_col <- if ("Vh_cm_hr_wc" %in% names(vh_data)) {
      "Vh_cm_hr_wc"  # Wound-corrected (highest priority)
    } else if ("Vh_cm_hr_sc" %in% names(vh_data)) {
      "Vh_cm_hr_sc"  # Spacing-corrected
    } else if ("Vc_cm_hr" %in% names(vh_data)) {
      "Vc_cm_hr"  # Generic corrected
    } else if ("Vh_cm_hr" %in% names(vh_data)) {
      "Vh_cm_hr"  # Raw velocity (fallback)
    } else {
      stop("No velocity column found. Expected one of: Vh_cm_hr_wc, Vh_cm_hr_sc, Vc_cm_hr, Vh_cm_hr")
    }
  }

  if (!velocity_col %in% names(vh_data)) {
    stop("Specified velocity_col '", velocity_col, "' not found in vh_data")
  }

  if (verbose) {
    cat("\n", strrep("=", 72), "\n")
    cat("RECALCULATING PECLET NUMBER\n")
    cat(strrep("=", 72), "\n")
    cat("Method:", method, "\n")
    cat("Velocity column:", velocity_col, "\n")
  }

  # Extract probe spacing and thermal diffusivity
  x <- NULL  # probe spacing (cm)
  D <- NULL  # thermal diffusivity (cm²/s)

  # Try to get from probe_config and wood_properties
  if (!is.null(probe_config)) {
    if (is.list(probe_config) && "probe_spacing" %in% names(probe_config)) {
      x <- probe_config$probe_spacing
    }
  }

  if (!is.null(wood_properties)) {
    if (is.list(wood_properties) && "thermal_diffusivity" %in% names(wood_properties)) {
      D <- wood_properties$thermal_diffusivity
    }
  }

  # If still missing parameters, try to extract from existing Peclet
  if (is.null(x) || is.null(D)) {
    if (verbose) {
      cat("Probe/wood parameters not provided - extracting from existing Peclet column...\n")
    }

    if (!peclet_col %in% names(vh_data)) {
      stop("Cannot extract parameters: peclet_col '", peclet_col,
           "' not found and probe_config/wood_properties not provided")
    }

    # Get HRM data with valid Peclet and velocity
    hrm_data <- vh_data[vh_data$method == method &
                        !is.na(vh_data[[peclet_col]]) &
                        !is.na(vh_data[["Vh_cm_hr"]]), ]

    if (nrow(hrm_data) == 0) {
      stop("Cannot extract parameters: no valid ", method, " data with both ",
           peclet_col, " and Vh_cm_hr")
    }

    # Use first valid row to extract x/D
    sample_row <- hrm_data[1, ]
    vh_raw_sample <- sample_row[["Vh_cm_hr"]]
    pe_sample <- sample_row[[peclet_col]]

    if (vh_raw_sample > 0 && pe_sample > 0) {
      # Pe_raw = (Vh_raw / 3600) * x / D
      # Therefore: x/D = (Pe_raw * 3600) / Vh_raw
      x_over_D <- (pe_sample * 3600) / vh_raw_sample

      if (verbose) {
        cat("Extracted x/D =", round(x_over_D, 6), "s\n")
      }
    } else {
      stop("Cannot extract parameters: invalid sample values (Vh =", vh_raw_sample,
           ", Pe =", pe_sample, ")")
    }
  } else {
    # Calculate x/D from provided parameters
    x_over_D <- x / D

    if (verbose) {
      cat("Probe spacing (x):", x, "cm\n")
      cat("Thermal diffusivity (D):", D, "cm²/s\n")
      cat("x/D =", round(x_over_D, 6), "s\n")
    }
  }

  # Recalculate Peclet for specified method
  # Pe = (Vc_cm_hr / 3600) * (x / D)
  # Pe = Vc_cm_hr * (x/D) / 3600

  vh_data[[output_col]] <- NA_real_

  method_rows <- vh_data$method == method
  n_method_rows <- sum(method_rows, na.rm = TRUE)

  if (n_method_rows == 0) {
    warning("No rows found for method '", method, "' - no Peclet values recalculated")
    return(vh_data)
  }

  # Calculate Peclet for method rows
  vh_data[[output_col]][method_rows] <-
    (vh_data[[velocity_col]][method_rows] / 3600) * x_over_D

  # Summary statistics
  n_calculated <- sum(!is.na(vh_data[[output_col]]), na.rm = TRUE)
  pe_range <- range(vh_data[[output_col]], na.rm = TRUE)

  if (verbose) {
    cat("\n")
    cat("RESULTS\n")
    cat(strrep("-", 72), "\n")
    cat("Rows processed:", n_method_rows, "\n")
    cat("Peclet values calculated:", n_calculated, "\n")
    cat("Peclet range: [", round(pe_range[1], 3), ",", round(pe_range[2], 3), "]\n")
    cat("Output column:", output_col, "\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  return(vh_data)
}


#' Calibrate Secondary Method to Primary Scale
#'
#' Aligns a secondary heat pulse velocity method (e.g., MHR) to the primary
#' method's scale (typically HRM) using linear or quadratic regression fitted
#' to the overlap region where both methods are valid.
#'
#' @param data Data frame or tibble containing heat pulse velocity results.
#'   Must have columns for pulse_id, method, sensor_position, and velocity values.
#' @param primary Character. Name of primary method (default: "HRM").
#' @param secondary Character. Name of secondary method to calibrate (e.g., "MHR").
#' @param sensor_position Character. Which sensor to calibrate ("outer" or "inner").
#'   Default: "outer".
#' @param velocity_col Character. Name of velocity column to use for SECONDARY method.
#'   Default: "Vh_cm_hr" (raw velocity). The primary method will automatically use
#'   corrected velocity (Vc_cm_hr) if available, falling back to raw if not.
#'   **CRITICAL**: Calibration must use corrected primary velocity to transfer
#'   spacing/wound corrections to the secondary method.
#' @param breakpoint Numeric. Maximum velocity (cm/hr) to use for calibration.
#'   Points above this are excluded. If NULL (default), uses all data where
#'   primary method is available.
#' @param method Character. Regression type: "linear" (default) or "quadratic".
#'   Quadratic matches Tim's Excel approach for MHR calibration.
#' @param min_points Numeric. Minimum number of points required for valid
#'   calibration (default: 50).
#' @param create_plots Logical. Whether to create diagnostic scatter plots
#'   showing the calibration fit (default: TRUE).
#'
#' @return A list (class "method_calibration_sdma") with components:
#'   \code{coefficients} (regression coefficients), \code{fit_type} ("linear"
#'   or "quadratic"), \code{r_squared} (R² of fit), \code{rmse} (root mean
#'   squared error), \code{n_points} (calibration points used), \code{breakpoint}
#'   (velocity threshold if specified), \code{transformation_function} (function
#'   to apply calibration), \code{calibrated_data} (data with calibrated column),
#'   \code{calibration_plot} (diagnostic plot if requested), \code{primary_method},
#'   \code{secondary_method}, and \code{sensor_position}
#'
#' @details
#' **Purpose:**
#'
#' This function solves the "volcano effect" - discontinuities that occur when
#' switching between methods without calibration. The secondary method may measure
#' on a different scale or have an offset relative to the primary method, causing
#' sudden drops/jumps in velocity time series.
#'
#' **Calibration Process:**
#'
#' \enumerate{
#'   \item Detect corrected velocity column (Vc_cm_hr) for primary method
#'   \item Merge primary (corrected) and secondary (raw) method data by pulse_id
#'   \item Filter to calibration region (low-medium flows where both work)
#'   \item Fit regression: Primary_Corrected ~ Secondary_Raw
#'   \item Create transformation function to transfer corrections to secondary
#' }
#'
#' **Correction Transfer:**
#'
#' When the primary method (HRM) has been spacing/wound corrected, calibration
#' against the corrected values mathematically "transfers" these corrections to
#' the secondary method. This ensures continuity when switching between methods.
#'
#' **When to Use Linear vs. Quadratic:**
#'
#' \itemize{
#'   \item **Linear**: Simple offset/scale differences between methods
#'   \item **Quadratic**: When relationship curves (Tim's Excel uses this for MHR)
#' }
#'
#' The function automatically uses the best fit if method = "auto".
#'
#' **Volcano Effect:**
#'
#' Named for the visual pattern in plots where daily peak velocities appear to
#' "drop into a crater" when switching from calibrated HRM to uncalibrated MHR.
#' Calibration eliminates this by ensuring continuity at the switching boundary.
#'
#' @examples
#' \dontrun{
#' # Calibrate MHR to HRM scale using quadratic regression
#' calibration <- calibrate_secondary_method(
#'   data = vh_results,
#'   primary = "HRM",
#'   secondary = "MHR",
#'   breakpoint = 12,  # Use data below 12 cm/hr for calibration
#'   method = "quadratic"
#' )
#'
#' # Examine calibration quality
#' print(calibration$r_squared)  # Should be > 0.95 for good calibration
#' print(calibration$calibration_plot)
#'
#' # Access calibrated data
#' calibrated_vh <- calibration$calibrated_data
#' }
#'
#' @family sDMA functions
#' @export
calibrate_secondary_method <- function(data,
                                       primary = "HRM",
                                       secondary,
                                       sensor_position = "outer",
                                       velocity_col = "Vh_cm_hr",
                                       breakpoint = NULL,
                                       handover_pct = 0.5,
                                       method = c("linear", "quadratic", "auto"),
                                       min_points = 50,
                                       create_plots = TRUE) {

  method <- match.arg(method)

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame or tibble")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", velocity_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter to specified sensor
  data_sensor <- data[data$sensor_position == sensor_position, ]

  if (nrow(data_sensor) == 0) {
    stop("No data found for sensor_position = '", sensor_position, "'")
  }

  # CRITICAL: Detect velocity column for primary method
  # Use corrected velocity (Vc_cm_hr) if available, otherwise fall back to raw
  primary_velocity_col <- if ("Vc_cm_hr" %in% names(data_sensor)) {
    "Vc_cm_hr"  # Corrected velocity (after spacing/wound corrections)
  } else {
    velocity_col  # Raw velocity
  }

  # Warn if using raw when corrected is expected
  if (primary_velocity_col == velocity_col && primary == "HRM") {
    message("NOTE: Using raw velocity for primary method. ",
            "For best results after corrections, ensure Vc_cm_hr column exists.")
  } else if (primary_velocity_col == "Vc_cm_hr") {
    message("INFO: Calibrating secondary method against CORRECTED primary velocity (Vc_cm_hr).\n",
            "      This transfers spacing/wound corrections to the secondary method.")
  }

  # Extract primary and secondary method data
  # PRIMARY: Use corrected velocity if available
  # SECONDARY: Always use raw velocity (specified by velocity_col)
  primary_data <- data_sensor[data_sensor$method == primary,
                              c("pulse_id", primary_velocity_col)]
  secondary_data <- data_sensor[data_sensor$method == secondary,
                                c("pulse_id", velocity_col)]

  if (nrow(primary_data) == 0) {
    stop("No data found for primary method '", primary, "'")
  }

  if (nrow(secondary_data) == 0) {
    stop("No data found for secondary method '", secondary, "'")
  }

  # Merge by pulse_id
  names(primary_data)[2] <- "primary_velocity"
  names(secondary_data)[2] <- "secondary_velocity"

  merged <- merge(primary_data, secondary_data, by = "pulse_id", all = FALSE)

  # Remove NA values
  merged <- merged[!is.na(merged$primary_velocity) &
                   !is.na(merged$secondary_velocity), ]

  # Apply handover window filter if breakpoint specified
  if (!is.null(breakpoint)) {
    handover_lower <- breakpoint * (1 - handover_pct)
    handover_upper <- breakpoint
    merged <- merged[
      merged$primary_velocity >= handover_lower &
      merged$primary_velocity <= handover_upper,
    ]
  }

  # Check sufficient points
  if (nrow(merged) < min_points) {
    error_msg <- sprintf(
      "Insufficient calibration points: %d (minimum required: %d)\n",
      nrow(merged), min_points
    )
    if (!is.null(breakpoint)) {
      error_msg <- paste0(
        error_msg,
        sprintf("  Handover window: %.2f - %.2f cm/hr (%.0f%% of threshold)\n",
                handover_lower, handover_upper, handover_pct * 100),
        "  Try:\n",
        "    - Increasing handover_pct (currently ", handover_pct, ")\n",
        "    - Increasing breakpoint\n",
        "    - Checking data availability"
      )
    }
    stop(error_msg)
  }

  # Fit regression model(s)
  fit_linear <- lm(primary_velocity ~ secondary_velocity, data = merged)
  r2_linear <- summary(fit_linear)$r.squared
  rmse_linear <- sqrt(mean(residuals(fit_linear)^2))

  if (method %in% c("quadratic", "auto")) {
    fit_quad <- lm(primary_velocity ~ secondary_velocity + I(secondary_velocity^2),
                   data = merged)
    r2_quad <- summary(fit_quad)$r.squared
    rmse_quad <- sqrt(mean(residuals(fit_quad)^2))
  }

  # Select best model
  if (method == "auto") {
    # Use quadratic if it improves R² by > 1%
    use_quadratic <- exists("r2_quad") && (r2_quad - r2_linear) > 0.01
    selected_method <- if (use_quadratic) "quadratic" else "linear"
  } else {
    selected_method <- method
  }

  # Get final model and metrics
  if (selected_method == "quadratic") {
    final_model <- fit_quad
    coefficients <- coef(fit_quad)
    r_squared <- r2_quad
    rmse <- rmse_quad
  } else {
    final_model <- fit_linear
    coefficients <- coef(fit_linear)
    r_squared <- r2_linear
    rmse <- rmse_linear
  }

  # Create transformation function
  if (selected_method == "quadratic") {
    transformation_function <- function(x) {
      coefficients[1] + coefficients[2] * x + coefficients[3] * x^2
    }
  } else {
    transformation_function <- function(x) {
      coefficients[1] + coefficients[2] * x
    }
  }

  # Apply calibration to full secondary method dataset
  calibrated_col_name <- paste0(secondary, "_calibrated")
  calibrated_data <- data
  calibrated_data[[calibrated_col_name]] <- NA_real_

  # Only calibrate secondary method rows
  secondary_rows <- calibrated_data$method == secondary &
                    calibrated_data$sensor_position == sensor_position

  calibrated_data[[calibrated_col_name]][secondary_rows] <-
    transformation_function(calibrated_data[[velocity_col]][secondary_rows])

  # Create diagnostic plot if requested
  calibration_plot <- NULL
  if (create_plots && requireNamespace("ggplot2", quietly = TRUE)) {
    merged$predicted <- predict(final_model, merged)

    calibration_plot <- ggplot2::ggplot(merged, ggplot2::aes(x = secondary_velocity,
                                                              y = primary_velocity)) +
      ggplot2::geom_point(alpha = 0.3, colour = "grey40") +
      ggplot2::geom_line(ggplot2::aes(y = predicted), colour = "blue", linewidth = 1) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                           colour = "red", alpha = 0.5) +
      ggplot2::labs(
        title = sprintf("%s Calibration to %s Scale", secondary, primary),
        subtitle = sprintf("%s fit | R² = %.4f | RMSE = %.3f cm/hr | n = %d",
                           tools::toTitleCase(selected_method), r_squared, rmse,
                           nrow(merged)),
        x = paste(secondary, "Velocity (cm/hr)"),
        y = paste(primary, "Velocity (cm/hr)")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11)
      )

    if (!is.null(breakpoint)) {
      calibration_plot <- calibration_plot +
        ggplot2::annotate("text", x = Inf, y = -Inf,
                          label = sprintf("Breakpoint: %.1f cm/hr", breakpoint),
                          hjust = 1.1, vjust = -0.5, size = 3.5, colour = "darkgreen")
    }
  }

  # Return calibration object
  result <- list(
    coefficients = coefficients,
    fit_type = selected_method,
    r_squared = r_squared,
    rmse = rmse,
    n_points = nrow(merged),
    breakpoint = breakpoint,
    transformation_function = transformation_function,
    calibrated_data = calibrated_data,
    calibration_plot = calibration_plot,
    primary_method = primary,
    secondary_method = secondary,
    sensor_position = sensor_position,
    velocity_col = velocity_col
  )

  class(result) <- c("method_calibration_sdma", "list")
  return(result)
}


#' Print Method for sDMA Calibration Results
#'
#' @param x A method_calibration_sdma object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.method_calibration_sdma <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("sDMA METHOD CALIBRATION RESULTS\n")
  cat(strrep("=", 70), "\n\n")

  cat("CALIBRATION SETUP\n")
  cat(strrep("-", 70), "\n")
  cat("Primary method:", x$primary_method, "\n")
  cat("Secondary method:", x$secondary_method, "\n")
  cat("Sensor position:", x$sensor_position, "\n")
  if (!is.null(x$breakpoint)) {
    cat("Breakpoint:", x$breakpoint, "cm/hr\n")
  }
  cat("\n")

  cat("REGRESSION RESULTS\n")
  cat(strrep("-", 70), "\n")
  cat("Fit type:", x$fit_type, "\n")
  cat("R²:", round(x$r_squared, 4), "\n")
  cat("RMSE:", round(x$rmse, 3), "cm/hr\n")
  cat("Calibration points:", x$n_points, "\n")
  cat("\n")

  cat("TRANSFORMATION EQUATION\n")
  cat(strrep("-", 70), "\n")
  if (x$fit_type == "linear") {
    cat(sprintf("  %s_calibrated = %.4f + %.4f × %s\n",
                x$secondary_method,
                x$coefficients[1],
                x$coefficients[2],
                x$secondary_method))
  } else {
    cat(sprintf("  %s_calibrated = %.4f + %.4f × %s + %.6f × %s²\n",
                x$secondary_method,
                x$coefficients[1],
                x$coefficients[2],
                x$secondary_method,
                x$coefficients[3],
                x$secondary_method))
  }
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("\n")

  invisible(x)
}


#' Apply sDMA Switching Logic
#'
#' Applies method switching between primary (HRM) and secondary method based on
#' either Peclet number (theoretical) or velocity threshold (empirical). Optionally
#' calibrates the secondary method first to ensure continuity.
#'
#' @param data Data frame containing heat pulse velocity results with HRM and
#'   at least one secondary method.
#' @param primary Character. Primary method name (default: "HRM").
#' @param secondary Character. Secondary method name (e.g., "MHR").
#' @param mode Character. Switching criterion: "peclet" (default, theory-based)
#'   or "velocity" (empirical, Tim's Excel approach).
#' @param threshold Numeric. Threshold value for switching:
#'   - If mode = "peclet": Peclet number threshold (default: 1.0)
#'   - If mode = "velocity": Velocity threshold in cm/hr (e.g., 11)
#' @param calibrate Logical. Whether to calibrate secondary method to primary
#'   scale before switching (default: TRUE). Highly recommended to avoid
#'   "volcano effect" discontinuities.
#' @param calibration_breakpoint Numeric. Velocity (cm/hr) below which to fit
#'   calibration. If NULL, uses the switching threshold.
#' @param calibration_method Character. "linear", "quadratic", or "auto" (default).
#' @param sensor_position Character. Which sensor to process ("outer" or "inner").
#'   Default: "outer".
#' @param velocity_col Character. Velocity column name (default: "Vh_cm_hr").
#' @param peclet_col Character. Peclet number column name (default: "hrm_peclet_number").
#' @param verbose Logical. Print progress messages (default: TRUE).
#'
#' @return Data frame with three new columns added:
#'   \describe{
#'     \item{Vh_sdma}{The final combined velocity after method switching}
#'     \item{sdma_source}{Which method was used: "HRM", "MHR", "MHR_calibrated", etc.}
#'     \item{sdma_trigger}{Reason for switch: "Pe < 1", "Pe >= 1", "Velocity < threshold", etc.}
#'   }
#'
#' @details
#' **Switching Modes:**
#'
#' \describe{
#'   \item{Peclet ("peclet")}{Theoretical approach. Switches when Peclet number
#'   exceeds threshold (typically 1.0). Based on physical limits of HRM method.}
#'   \item{Velocity ("velocity")}{Empirical approach. Switches when HRM velocity
#'   exceeds threshold (e.g., 11 cm/hr). Matches Tim's Excel template workflow.}
#' }
#'
#' **Calibration:**
#'
#' When \code{calibrate = TRUE}, fits regression between primary and secondary
#' methods in the overlap region, then uses calibrated secondary values above
#' the threshold. This prevents the "volcano effect" discontinuity.
#'
#' **Output Columns:**
#'
#' \itemize{
#'   \item \code{Vh_sdma}: The velocity value to use (either primary or secondary/calibrated)
#'   \item \code{sdma_source}: Data lineage - which method provided this value
#'   \item \code{sdma_trigger}: Why this method was selected (for diagnostics)
#' }
#'
#' @examples
#' \dontrun{
#' # Peclet-based switching with calibration (recommended)
#' vh_sdma <- apply_sdma_switching(
#'   data = vh_results,
#'   primary = "HRM",
#'   secondary = "MHR",
#'   mode = "peclet",
#'   threshold = 1.0,
#'   calibrate = TRUE
#' )
#'
#' # Velocity-based switching (Tim's Excel approach)
#' vh_sdma <- apply_sdma_switching(
#'   data = vh_results,
#'   mode = "velocity",
#'   threshold = 11,  # cm/hr
#'   calibrate = TRUE,
#'   calibration_method = "quadratic"
#' )
#' }
#'
#' @family sDMA functions
#' @export
apply_sdma_switching <- function(data,
                                 vh_data = NULL,  # Alias for data
                                 primary = "HRM",
                                 secondary = NULL,
                                 secondary_method = NULL,  # Alias for secondary
                                 mode = c("peclet", "velocity"),
                                 switching_mode = NULL,  # Alias for mode
                                 threshold = NULL,
                                 calibrate = TRUE,
                                 calibration = NULL,  # Alias for calibrate (TRUE/FALSE) or calibration object
                                 calibration_breakpoint = NULL,
                                 calibration_method = "auto",
                                 sensor_position = "outer",
                                 velocity_col = "Vh_cm_hr",
                                 peclet_col = "hrm_peclet_number",
                                 verbose = TRUE) {

  # Handle aliases
  if (!is.null(vh_data)) data <- vh_data
  if (!is.null(secondary_method)) secondary <- secondary_method
  if (!is.null(switching_mode)) mode <- switching_mode

  # Handle calibration parameter (can be TRUE/FALSE or a calibration object)
  calibration_object <- NULL  # Initialize
  if (!is.null(calibration)) {
    if (is.logical(calibration)) {
      calibrate <- calibration
    } else {
      # Assume it's a calibration object, we'll use it later
      # For now, just set calibrate to TRUE
      calibrate <- TRUE
      # Store the calibration object for later use
      calibration_object <- calibration
    }
  }

  mode <- match.arg(mode)

  # Set default threshold based on mode
  if (is.null(threshold)) {
    threshold <- if (mode == "peclet") 1.0 else stop("threshold required for velocity mode")
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame or tibble")
  }

  if (is.null(secondary)) {
    stop("secondary method must be specified (e.g., secondary = 'MHR')")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", velocity_col)
  if (mode == "peclet") {
    required_cols <- c(required_cols, peclet_col)
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check methods exist
  if (!primary %in% unique(data$method)) {
    stop("Primary method '", primary, "' not found in data")
  }

  if (!secondary %in% unique(data$method)) {
    stop("Secondary method '", secondary, "' not found in data")
  }

  if (verbose) {
    cat("\n", strrep("=", 70), "\n")
    cat("sDMA SWITCHING\n")
    cat(strrep("=", 70), "\n")
    cat("Mode:", mode, "\n")
    cat("Threshold:", threshold, if (mode == "peclet") "" else "cm/hr", "\n")
    cat("Calibrate:", calibrate, "\n")
    cat(strrep("-", 70), "\n\n")
  }

  # Step 1: Calibration (if requested)
  if (calibrate) {
    # Check if a calibration object was already provided
    if (!is.null(calibration_object)) {
      # Use the provided calibration object
      if (verbose) cat("Step 1: Using provided calibration object...\n")
      calibration <- calibration_object

      # Apply the calibration to the data
      # Extract calibration function and apply it
      if (!is.null(calibration$transform_func)) {
        # Apply transformation to secondary method
        secondary_data <- data[data$method == secondary &
                               data$sensor_position == sensor_position, ]
        if (nrow(secondary_data) > 0) {
          transformed_values <- calibration$transform_func(secondary_data[[velocity_col]])
          data[[paste0(secondary, "_calibrated")]] <- NA_real_
          data[[paste0(secondary, "_calibrated")]][data$method == secondary &
                                                    data$sensor_position == sensor_position] <- transformed_values
        }
      } else {
        stop("Provided calibration object does not contain transform_func")
      }

      if (verbose) {
        cat("  Fit type:", calibration$fit_type, "\n")
        cat("  R²:", round(calibration$r_squared, 4), "\n")
        cat("  RMSE:", round(calibration$rmse, 3), "cm/hr\n\n")
      }
    } else {
      # Create new calibration
      if (verbose) cat("Step 1: Calibrating", secondary, "to", primary, "scale...\n")

      # Use threshold as breakpoint if not specified
      if (is.null(calibration_breakpoint)) {
        calibration_breakpoint <- if (mode == "peclet") {
          # Convert Peclet to approximate velocity for breakpoint
          # This is rough - ideally extract from data
          max(data[[velocity_col]][data$method == primary &
                                    data[[peclet_col]] <= threshold], na.rm = TRUE)
        } else {
          threshold
        }
      }

      calibration <- calibrate_secondary_method(
        data = data,
        primary = primary,
        secondary = secondary,
        sensor_position = sensor_position,
        velocity_col = velocity_col,
        breakpoint = calibration_breakpoint,
        method = calibration_method,
        create_plots = FALSE
      )

      if (verbose) {
        cat("  Fit type:", calibration$fit_type, "\n")
        cat("  R²:", round(calibration$r_squared, 4), "\n")
        cat("  RMSE:", round(calibration$rmse, 3), "cm/hr\n\n")
      }

      # Use calibrated data
      data <- calibration$calibrated_data
    }

    secondary_col <- paste0(secondary, "_calibrated")
    secondary_label <- paste0(secondary, "_calibrated")
  } else {
    secondary_col <- velocity_col
    secondary_label <- secondary
  }

  # Step 2: Recalculate Peclet with corrected velocities (CRITICAL)
  if (mode == "peclet") {
    # CRITICAL UPDATE: Peclet must be calculated using CORRECTED velocities
    # Pe_corrected = (Vc * x) / (2 * k)
    # Using raw velocities could falsely indicate low flow when corrected is high

    # Detect if corrected velocity exists
    corrected_vel_col <- if ("Vc_cm_hr" %in% names(data)) "Vc_cm_hr" else velocity_col

    if (corrected_vel_col == "Vc_cm_hr" && verbose) {
      cat("INFO: Recalculating Peclet using CORRECTED velocities (Vc_cm_hr)\n")
      cat("      This ensures switching based on true (corrected) flow conditions.\n\n")
    }

    # Recalculate Peclet for primary method using corrected velocity
    # Extract thermal diffusivity from existing Peclet (reverse calculation)
    # Pe_raw = (Vh * x) / (2 * k)  =>  k = (Vh * x) / (2 * Pe_raw)
    primary_data_for_pe <- data[data$method == primary &
                                 data$sensor_position == sensor_position &
                                 !is.na(data[[peclet_col]]) &
                                 !is.na(data[[velocity_col]]), ]

    if (nrow(primary_data_for_pe) > 0) {
      # Use first valid row to extract parameters
      sample_row <- primary_data_for_pe[1, ]
      vh_sample <- sample_row[[velocity_col]]
      pe_sample <- sample_row[[peclet_col]]

      if (vh_sample > 0 && pe_sample > 0) {
        # Extract x/(2*k) from raw Peclet
        # Pe = Vh * (x/(2*k))  =>  x/(2*k) = Pe / Vh
        x_over_2k <- pe_sample / vh_sample

        # Recalculate Peclet for all HRM rows using corrected velocity
        hrm_rows <- data$method == primary & data$sensor_position == sensor_position
        data$Pe_corrected <- NA_real_
        data$Pe_corrected[hrm_rows] <- data[[corrected_vel_col]][hrm_rows] * x_over_2k

        # Use corrected Peclet for switching
        peclet_col_to_use <- "Pe_corrected"
      } else {
        warning("Could not recalculate Peclet - using original Peclet column")
        peclet_col_to_use <- peclet_col
      }
    } else {
      warning("No valid Peclet data found - using original Peclet column")
      peclet_col_to_use <- peclet_col
    }
  }

  # Step 3: Switching logic
  if (verbose) cat("Step 3: Applying switching logic...\n")

  # Initialise new columns
  data$Vh_sdma <- NA_real_
  data$sdma_source <- NA_character_
  data$sdma_trigger <- NA_character_

  # Filter to sensor position
  sensor_rows <- data$sensor_position == sensor_position

  # Get primary method rows
  primary_rows <- sensor_rows & data$method == primary

  # Get secondary method rows
  secondary_rows <- sensor_rows & data$method == secondary

  if (mode == "peclet") {
    # Peclet-based switching using CORRECTED Peclet
    # Use primary when Pe < threshold
    use_primary <- primary_rows & !is.na(data[[peclet_col_to_use]]) & data[[peclet_col_to_use]] < threshold
    use_secondary <- primary_rows & !is.na(data[[peclet_col_to_use]]) & data[[peclet_col_to_use]] >= threshold

    data$Vh_sdma[use_primary] <- data[[velocity_col]][use_primary]
    data$sdma_source[use_primary] <- primary
    data$sdma_trigger[use_primary] <- paste0("Pe < ", threshold)

    # For secondary, need to match pulse_ids and extract calibrated values
    primary_pe_high <- data[use_secondary, c("pulse_id", velocity_col, peclet_col_to_use)]

    if (nrow(primary_pe_high) > 0) {
      # Get corresponding secondary values
      secondary_data <- data[secondary_rows, c("pulse_id", secondary_col)]
      names(secondary_data)[2] <- "sec_velocity"

      merged <- merge(primary_pe_high, secondary_data, by = "pulse_id", all.x = TRUE)

      # Update Vh_sdma for high Peclet rows
      data$Vh_sdma[use_secondary] <- merged$sec_velocity
      data$sdma_source[use_secondary] <- secondary_label
      data$sdma_trigger[use_secondary] <- paste0("Pe >= ", threshold)
    }

  } else {
    # Velocity-based switching
    use_primary <- primary_rows & !is.na(data[[velocity_col]]) & data[[velocity_col]] < threshold
    use_secondary <- primary_rows & !is.na(data[[velocity_col]]) & data[[velocity_col]] >= threshold

    data$Vh_sdma[use_primary] <- data[[velocity_col]][use_primary]
    data$sdma_source[use_primary] <- primary
    data$sdma_trigger[use_primary] <- paste0("Velocity < ", threshold, " cm/hr")

    # For secondary
    primary_vh_high <- data[use_secondary, c("pulse_id", velocity_col)]

    if (nrow(primary_vh_high) > 0) {
      secondary_data <- data[secondary_rows, c("pulse_id", secondary_col)]
      names(secondary_data)[2] <- "sec_velocity"

      merged <- merge(primary_vh_high, secondary_data, by = "pulse_id", all.x = TRUE)

      data$Vh_sdma[use_secondary] <- merged$sec_velocity
      data$sdma_source[use_secondary] <- secondary_label
      data$sdma_trigger[use_secondary] <- paste0("Velocity >= ", threshold, " cm/hr")
    }
  }

  # Count switches
  n_primary <- sum(data$sdma_source == primary, na.rm = TRUE)
  n_secondary <- sum(data$sdma_source == secondary_label, na.rm = TRUE)

  if (verbose) {
    cat("  Used", primary, ":", n_primary, "measurements\n")
    cat("  Used", secondary_label, ":", n_secondary, "measurements\n\n")
    cat(strrep("=", 70), "\n\n")
  }

  return(data)
}


#' Automatically Select Optimal sDMA Strategy
#'
#' Intelligent wrapper that determines whether method switching is necessary,
#' whether calibration is needed, and automatically configures the sDMA approach.
#' This is the "smart" function that makes decisions for the user.
#'
#' @param data Data frame containing HRM and at least one secondary method.
#' @param secondary Character. Secondary method to consider (e.g., "MHR").
#' @param sensor_position Character. Which sensor ("outer" or "inner"). Default: "outer".
#' @param velocity_col Character. Velocity column name (default: "Vh_cm_hr").
#' @param peclet_col Character. Peclet column name (default: "hrm_peclet_number").
#' @param discontinuity_threshold Numeric. Maximum acceptable difference (as fraction)
#'   between methods at boundary (default: 0.10 = 10%). If difference exceeds this,
#'   calibration will be enabled.
#' @param verbose Logical. Print decision-making process (default: TRUE).
#'
#' @return Either:
#'   \itemize{
#'     \item The original data (if switching not needed)
#'     \item Data with sDMA columns added (if switching applied)
#'   }
#'   Result includes an attribute "sdma_applied" (TRUE/FALSE) and "sdma_reason"
#'   explaining the decision.
#'
#' @details
#' **Decision Tree:**
#'
#' \enumerate{
#'   \item **Check Necessity**: Is max(Peclet) > 1.0?
#'     - If NO: Return original data with message "No switching needed (low flow)"
#'     - If YES: Continue to step 2
#'   \item **Check Discontinuity**: Do methods differ significantly at Pe = 1?
#'     - Compare HRM and secondary values near the boundary (Pe = 0.9 to 1.1)
#'     - If difference > threshold (default 10%): Enable calibration
#'     - If difference <= threshold: No calibration needed
#'   \item **Execute**: Call apply_sdma_switching() with determined settings
#' }
#'
#' **Why This Matters:**
#'
#' \itemize{
#'   \item Avoids unnecessary computation when all flows are low (Pe < 1)
#'   \item Prevents "volcano effect" by detecting discontinuities automatically
#'   \item Provides thesis-defensible decision logic with clear reasoning
#' }
#'
#' @examples
#' \dontrun{
#' # Let the function decide everything
#' result <- auto_select_sdma_strategy(
#'   data = vh_results,
#'   secondary = "MHR"
#' )
#'
#' # Check what was decided
#' attr(result, "sdma_applied")  # TRUE or FALSE
#' attr(result, "sdma_reason")   # Explanation of decision
#' }
#'
#' @family sDMA functions
#' @export
auto_select_sdma_strategy <- function(data,
                                      secondary,
                                      sensor_position = "outer",
                                      velocity_col = "Vh_cm_hr",
                                      peclet_col = "hrm_peclet_number",
                                      discontinuity_threshold = 0.10,
                                      verbose = TRUE) {

  if (verbose) {
    cat("\n", strrep("=", 70), "\n")
    cat("sDMA AUTO-SELECT STRATEGY\n")
    cat(strrep("=", 70), "\n\n")
  }

  # Step 1: Check necessity - is switching even needed?
  primary_data <- data[data$method == "HRM" &
                       data$sensor_position == sensor_position, ]

  if (nrow(primary_data) == 0) {
    stop("No HRM data found for sensor_position = '", sensor_position, "'")
  }

  if (!peclet_col %in% names(data)) {
    stop("Peclet number column '", peclet_col, "' not found in data.\n",
         "  HRM must be calculated with Peclet numbers for auto-select.")
  }

  max_peclet <- max(primary_data[[peclet_col]], na.rm = TRUE)

  if (verbose) {
    cat("STEP 1: NECESSITY CHECK\n")
    cat(strrep("-", 70), "\n")
    cat("Maximum Peclet number:", round(max_peclet, 3), "\n")
  }

  if (max_peclet < 1.0) {
    reason <- sprintf("Low flow dataset (max Pe = %.3f < 1.0). HRM valid for all measurements. No switching required.",
                     max_peclet)

    if (verbose) {
      cat("\n  DECISION: Switching NOT needed\n")
      cat("  Reason:", reason, "\n")
      cat(strrep("=", 70), "\n\n")
    }

    # Return original data with attributes
    result <- data
    attr(result, "sdma_applied") <- FALSE
    attr(result, "sdma_reason") <- reason
    return(result)
  }

  if (verbose) {
    cat("  Result: Switching IS needed (Pe exceeds 1.0)\n\n")
  }

  # Step 1.5: CRITICAL - Check if corrections have been applied
  corrections_applied <- "Vc_cm_hr" %in% names(data)

  if (corrections_applied) {
    if (verbose) {
      cat("STEP 1.5: CORRECTION DETECTION\n")
      cat(strrep("-", 70), "\n")
      cat("CRITICAL: Corrected velocity column (Vc_cm_hr) detected!\n")
      cat("  Spacing/wound corrections have been applied to HRM.\n")
      cat("  Calibration MUST be enabled to transfer corrections to secondary method.\n")
      cat("  Mixing corrected HRM with raw secondary would create invalid timeseries.\n\n")
      cat("  DECISION: Calibration FORCED (corrections applied)\n\n")
    }
    needs_calibration <- TRUE
    calibration_reason <- "corrections_applied"
  } else {
    calibration_reason <- NULL
  }

  # Step 2: Check discontinuity - do we need calibration?
  if (!corrections_applied) {  # Only check discontinuity if not already forced
    if (verbose) {
      cat("STEP 2: DISCONTINUITY CHECK\n")
      cat(strrep("-", 70), "\n")
    }

  # Get data near the switching boundary (Pe = 0.9 to 1.1)
  boundary_data <- data[data$method %in% c("HRM", secondary) &
                        data$sensor_position == sensor_position &
                        data[[peclet_col]] >= 0.9 &
                        data[[peclet_col]] <= 1.1, ]

  # Merge HRM and secondary by pulse_id
  hrm_boundary <- boundary_data[boundary_data$method == "HRM",
                                c("pulse_id", velocity_col)]
  sec_boundary <- boundary_data[boundary_data$method == secondary,
                                c("pulse_id", velocity_col)]

  names(hrm_boundary)[2] <- "hrm_velocity"
  names(sec_boundary)[2] <- "sec_velocity"

  boundary_merged <- merge(hrm_boundary, sec_boundary, by = "pulse_id", all = FALSE)

  if (nrow(boundary_merged) < 10) {
      if (verbose) {
        cat("  Warning: Few points near boundary (n =", nrow(boundary_merged), ")\n")
        cat("  Enabling calibration as precaution\n")
      }
      needs_calibration <- TRUE
      calibration_reason <- "insufficient_boundary_data"
    } else {
      # Calculate mean difference at boundary
      mean_hrm <- mean(boundary_merged$hrm_velocity, na.rm = TRUE)
      mean_sec <- mean(boundary_merged$sec_velocity, na.rm = TRUE)
      rel_difference <- abs(mean_sec - mean_hrm) / mean_hrm

      if (verbose) {
        cat("  Mean HRM at boundary:", round(mean_hrm, 2), "cm/hr\n")
        cat("  Mean", secondary, "at boundary:", round(mean_sec, 2), "cm/hr\n")
        cat("  Relative difference:", round(rel_difference * 100, 1), "%\n")
      }

      needs_calibration <- rel_difference > discontinuity_threshold

      if (verbose) {
        cat("  Threshold:", round(discontinuity_threshold * 100, 1), "%\n")
        if (needs_calibration) {
          cat("\n  DECISION: Calibration NEEDED (discontinuity detected)\n\n")
          calibration_reason <- "discontinuity_detected"
        } else {
          cat("\n  DECISION: Calibration not needed (methods agree at boundary)\n\n")
          calibration_reason <- "no_discontinuity"
        }
      }
    }
  } else {
    # Calibration already forced due to corrections
    if (verbose) {
      cat("STEP 2: DISCONTINUITY CHECK\n")
      cat(strrep("-", 70), "\n")
      cat("  Skipped (calibration already forced due to corrections)\n\n")
    }
  }

  # Step 3: Execute with determined settings
  if (verbose) {
    cat("STEP 3: EXECUTION\n")
    cat(strrep("-", 70), "\n")
    cat("Applying sDMA with:\n")
    cat("  Mode: Peclet-based (Pe threshold = 1.0)\n")
    cat("  Calibration:", if (needs_calibration) "ENABLED" else "DISABLED", "\n")
    if (!is.null(calibration_reason)) {
      cat("  Reason:", calibration_reason, "\n")
    }
    cat("\n")
  }

  result <- apply_sdma_switching(
    data = data,
    primary = "HRM",
    secondary = secondary,
    mode = "peclet",
    threshold = 1.0,
    calibrate = needs_calibration,
    sensor_position = sensor_position,
    velocity_col = velocity_col,
    peclet_col = peclet_col,
    verbose = verbose
  )

  # Add attributes
  calibration_note <- if (needs_calibration) {
    if (!is.null(calibration_reason)) {
      sprintf("calibration (%s)", calibration_reason)
    } else {
      "calibration"
    }
  } else {
    "no calibration"
  }
  reason <- sprintf("sDMA applied: Pe-based switching with %s", calibration_note)
  attr(result, "sdma_applied") <- TRUE
  attr(result, "sdma_reason") <- reason

  return(result)
}
