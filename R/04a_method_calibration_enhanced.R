#' Enhanced Segmented Regression with Non-Linear Detection
#'
#' Wrapper for compare_methods_segmented() that automatically detects and handles
#' non-linear patterns in the second segment (after the breakpoint).
#'
#' @param vh_corrected Data frame of corrected velocity data
#' @param primary_method Primary method name (default: "HRM")
#' @param secondary_method Secondary method name to compare
#' @param sensor_position Sensor position ("outer" or "inner")
#' @param try_quadratic If TRUE, attempts quadratic fit for second segment
#'   when U-shaped residuals are detected (default: TRUE)
#' @param ... Additional arguments passed to compare_methods_segmented()
#'
#' @return Enhanced segmented comparison result with additional diagnostics
#'
#' @details
#' This function extends \code{compare_methods_segmented()} by:
#' \itemize{
#'   \item Automatically detecting non-linear residual patterns (U-shape)
#'   \item Optionally fitting a piecewise linear-quadratic model
#'   \item Providing enhanced diagnostic warnings and recommendations
#' }
#'
#' **When to Use:**
#' \itemize{
#'   \item When residuals show systematic patterns after the breakpoint
#'   \item When the relationship appears to curve at higher velocities
#'   \item When simple segmented regression R² is lower than expected
#' }
#'
#' @family method calibration functions
#' @export
compare_methods_enhanced <- function(vh_corrected,
                                     primary_method = "HRM",
                                     secondary_method,
                                     sensor_position = "outer",
                                     try_quadratic = TRUE,
                                     velocity_col = "Vh_cm_hr",
                                     ...) {

  # First fit standard segmented regression
  result <- compare_methods_segmented(
    vh_corrected = vh_corrected,
    primary_method = primary_method,
    secondary_method = secondary_method,
    sensor_position = sensor_position,
    create_plots = TRUE,
    velocity_col = velocity_col,
    ...
  )

  if (!result$converged) {
    return(result)
  }

  # Diagnose residual patterns
  diagnostics <- diagnose_residual_pattern(result)
  result$residual_diagnostics <- diagnostics

  # If U-shaped pattern detected and quadratic requested, try enhanced model
  if (try_quadratic && diagnostics$pattern_detected) {

    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("NON-LINEAR PATTERN DETECTED IN RESIDUALS\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Pattern type:", diagnostics$pattern_type, "\n")
    cat("Significance: p =", format.pval(diagnostics$quad_pvalue, digits = 3), "\n")
    cat("\n")
    cat("Attempting piecewise linear-quadratic model...\n")
    cat("\n")

    # Fit piecewise linear-quadratic model
    quad_result <- tryCatch({
      fit_piecewise_linear_quadratic(
        merged_data = result$merged_data,
        breakpoint = result$breakpoint,
        primary_method = primary_method,
        secondary_method = secondary_method
      )
    }, error = function(e) {
      cat("Quadratic model failed:", e$message, "\n")
      NULL
    })

    if (!is.null(quad_result)) {
      # Compare models
      r2_improvement <- quad_result$r_squared - result$r_squared

      cat("QUADRATIC MODEL RESULTS\n")
      cat(strrep("-", 72), "\n")
      cat("  Quadratic R²:", round(quad_result$r_squared, 4), "\n")
      cat("  Linear R²:", round(result$r_squared, 4), "\n")
      cat("  Improvement:", round(r2_improvement, 4), "\n")
      cat("\n")

      # Use quadratic model if it improves R² substantially
      if (r2_improvement > 0.01) {
        cat("Quadratic model provides better fit (ΔR² > 0.01)\n")
        cat("Recommendation: Use quadratic model for this comparison\n")
        cat("\n")

        result$quadratic_model <- quad_result
        result$recommended_model <- "quadratic"

        # Create enhanced plots
        result$plots$quadratic_plot <- create_quadratic_plot(quad_result, primary_method, secondary_method)
      } else {
        cat("Quadratic model does not substantially improve fit\n")
        cat("Recommendation: Linear segmented model is adequate\n")
        cat("\n")
        result$recommended_model <- "linear"
      }
    }
  }

  class(result) <- c("segmented_comparison_enhanced", "segmented_comparison", "list")
  return(result)
}


#' Diagnose Residual Patterns
#'
#' Detects systematic patterns in residuals after breakpoint, particularly
#' U-shaped or inverted-U patterns indicating non-linearity.
#'
#' @param result Segmented comparison result object
#'
#' @return List with pattern detection results
#'
#' @keywords internal
diagnose_residual_pattern <- function(result) {

  merged_data <- result$merged_data
  residuals_all <- residuals(result$segmented_model)

  # Focus on residuals after breakpoint
  after_bp <- merged_data$Vh_cm_hr_primary > result$breakpoint

  if (sum(after_bp) < 20) {
    return(list(
      pattern_detected = FALSE,
      pattern_type = "insufficient_data",
      quad_pvalue = NA,
      quad_coef = NA
    ))
  }

  residuals_after <- residuals_all[after_bp]
  x_after <- merged_data$Vh_cm_hr_primary[after_bp]

  # Fit quadratic to residuals
  quad_fit <- tryCatch({
    lm(residuals_after ~ x_after + I(x_after^2))
  }, error = function(e) NULL)

  if (is.null(quad_fit)) {
    return(list(
      pattern_detected = FALSE,
      pattern_type = "fit_failed",
      quad_pvalue = NA,
      quad_coef = NA
    ))
  }

  quad_coef <- coef(quad_fit)[3]
  quad_pvalue <- summary(quad_fit)$coefficients[3, 4]

  # Determine pattern type
  pattern_detected <- !is.na(quad_pvalue) && quad_pvalue < 0.05

  if (pattern_detected) {
    if (quad_coef > 0) {
      pattern_type <- "U-shaped (underestimate at extremes)"
    } else {
      pattern_type <- "Inverted-U (overestimate at extremes)"
    }
  } else {
    pattern_type <- "No significant pattern"
  }

  return(list(
    pattern_detected = pattern_detected,
    pattern_type = pattern_type,
    quad_pvalue = quad_pvalue,
    quad_coef = quad_coef,
    quad_fit = quad_fit
  ))
}


#' Fit Piecewise Linear-Quadratic Model
#'
#' Fits a model where the first segment is linear and the second segment
#' is quadratic. This is useful when the relationship curves after the breakpoint.
#'
#' @param merged_data Data frame with Vh_cm_hr_primary and Vh_cm_hr_secondary
#' @param breakpoint Breakpoint location (cm/hr)
#' @param primary_method Primary method name
#' @param secondary_method Secondary method name
#'
#' @return List with model results
#'
#' @keywords internal
fit_piecewise_linear_quadratic <- function(merged_data, breakpoint,
                                           primary_method, secondary_method) {

  # Create indicator for segment
  merged_data$segment <- ifelse(merged_data$Vh_cm_hr_primary <= breakpoint, 1, 2)

  # Create piecewise variables
  # Segment 1 (before breakpoint): linear
  # Segment 2 (after breakpoint): quadratic

  merged_data$x1 <- ifelse(merged_data$segment == 1, merged_data$Vh_cm_hr_primary, 0)
  merged_data$x2 <- ifelse(merged_data$segment == 2, merged_data$Vh_cm_hr_primary - breakpoint, 0)
  merged_data$x2_sq <- merged_data$x2^2

  # Fit model: y = b0 + b1*x1 + b2*x2 + b3*x2^2
  # This ensures continuity at the breakpoint
  model <- lm(Vh_cm_hr_secondary ~ x1 + x2 + x2_sq, data = merged_data)

  # Calculate R²
  r_squared <- summary(model)$r.squared

  # Extract coefficients
  coefs <- coef(model)

  # Get fitted values
  fitted_values <- fitted(model)
  residuals_values <- residuals(model)

  return(list(
    model = model,
    r_squared = r_squared,
    breakpoint = breakpoint,
    coefficients = coefs,
    fitted = fitted_values,
    residuals = residuals_values,
    merged_data = merged_data,
    primary_method = primary_method,
    secondary_method = secondary_method
  ))
}


#' Create Quadratic Model Plot
#'
#' Creates diagnostic plot for piecewise linear-quadratic model
#'
#' @param quad_result Quadratic model result object
#' @param primary_method Primary method name
#' @param secondary_method Secondary method name
#'
#' @return ggplot object
#'
#' @keywords internal
create_quadratic_plot <- function(quad_result, primary_method, secondary_method) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  merged_data <- quad_result$merged_data
  merged_data$fitted_quad <- quad_result$fitted

  # Create plot
  p <- ggplot2::ggplot(merged_data,
                       ggplot2::aes(x = Vh_cm_hr_primary, y = Vh_cm_hr_secondary)) +
    # Data points
    ggplot2::geom_point(alpha = 0.3, color = "grey40", size = 1.5) +

    # Fitted quadratic line
    ggplot2::geom_line(ggplot2::aes(y = fitted_quad), color = "purple", linewidth = 1.2) +

    # 1:1 reference line
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "blue", alpha = 0.5) +

    # Breakpoint vertical line
    ggplot2::geom_vline(xintercept = quad_result$breakpoint, color = "darkgreen",
                        linetype = "dashed", linewidth = 1) +

    # Labels and theme
    ggplot2::labs(
      title = paste("Piecewise Linear-Quadratic:", secondary_method, "vs", primary_method),
      subtitle = sprintf("R² = %.4f | Linear before BP, Quadratic after BP",
                        quad_result$r_squared),
      x = paste(primary_method, "Velocity (cm/hr)"),
      y = paste(secondary_method, "Velocity (cm/hr)")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}
