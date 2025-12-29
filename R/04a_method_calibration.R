# R/04a_method_calibration.R
# Method Calibration and Transformation
# Aligns secondary methods (e.g., MHR) to primary method scale (e.g., HRM)

#' Method Calibration Functions
#'
#' Functions for calibrating secondary heat pulse velocity methods to a primary
#' method scale. Implements the workflow used in Excel-based processing where
#' methods are aligned using regression in their overlap region.
#'
#' @name method_calibration
NULL


#' Find Optimal Calibration Threshold
#'
#' Automatically determines the best velocity threshold for calibrating a
#' secondary method to a primary method by testing multiple thresholds and
#' comparing R-squared values.
#'
#' @param vh_corrected Data frame of corrected velocity data (after spacing
#'   correction). Must contain columns: pulse_id, method, sensor_position, Vh_cm_hr.
#' @param primary_method Primary method name (default: "HRM"). This is the
#'   "source of truth" method, typically HRM for low flows.
#' @param secondary_method Secondary method name to calibrate (e.g., "MHR").
#' @param sensor_position Sensor position to analyse ("outer" or "inner").
#' @param threshold_start Minimum velocity threshold to test (cm/hr) (default: 0).
#' @param threshold_max Maximum velocity threshold to test (cm/hr) (default: 20).
#' @param threshold_step Increment between thresholds (cm/hr) (default: 0.5).
#' @param fit_type Type of fit: "linear", "quadratic", or "auto" (default: "auto").
#'   "auto" tries both and selects based on R² improvement.
#' @param min_points Minimum number of points required for valid calibration (default: 50).
#' @param create_plots Logical indicating whether to create diagnostic plots (default: TRUE).
#' @param verbose Logical indicating whether to print progress (default: TRUE).
#' @param manual_threshold Numeric value to manually specify threshold (cm/hr).
#'   If provided, skips automatic optimization and uses this threshold directly.
#'   Useful when you want to use a specific threshold based on domain knowledge
#'   or visual inspection of R² vs threshold plot. Default: NULL (automatic).
#'
#' @return A list with components \code{optimal_threshold} (velocity threshold
#'   with highest R²), \code{optimal_r_squared} (R² value at optimal threshold),
#'   \code{optimal_calibration} (full calibration object), \code{threshold_results}
#'   (data frame of tested thresholds with R², RMSE, n_points), \code{plots}
#'   (diagnostic plots if \code{create_plots = TRUE}), \code{primary_method},
#'   \code{secondary_method}, and \code{sensor_position}
#'
#' @details
#' **Threshold Selection Process:**
#'
#' This function automates the process of finding the optimal velocity threshold
#' for method calibration:
#'
#' 1. Tests thresholds from \code{threshold_start} to \code{threshold_max} by
#'    \code{threshold_step} (e.g., 0, 0.5, 1.0, 1.5, ..., 20 cm/hr)
#' 2. At each threshold, filters data to points where primary method <= threshold
#' 3. Fits relationship between primary and secondary methods on this valid region
#' 4. Records R², RMSE, and number of points
#' 5. Identifies the maximum threshold that maintains high R² (where methods still agree)
#'
#' **Interpretation:**
#'
#' The optimal threshold represents the maximum velocity up to which both methods
#' are valid and show strong correlation. This is the "divergence point" where
#' the primary method (typically HRM) begins to fail. Data below the threshold
#' is used for calibration (where both methods are reliable). The resulting
#' calibration coefficients are then applied to transform the secondary method
#' values above the threshold (where the primary fails but secondary continues).
#'
#' **Diagnostic Plots:**
#'
#' When \code{create_plots = TRUE}, generates:
#' \itemize{
#'   \item **R² vs Threshold**: Shows how correlation changes with threshold
#'   \item **Calibration Scatter**: HRM vs MHR with line of best fit at optimal threshold
#' }
#'
#' @examples
#' \dontrun{
#' # Find optimal threshold for MHR calibration
#' threshold_result <- find_optimal_calibration_threshold(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer",
#'   threshold_max = 20,
#'   threshold_step = 0.5
#' )
#'
#' # View results
#' print(threshold_result$optimal_threshold)
#' print(threshold_result$optimal_r_squared)
#' print(threshold_result$threshold_results)
#'
#' # View plots
#' print(threshold_result$plots$r_squared_plot)
#' print(threshold_result$plots$calibration_plot)
#' }
#'
#' @family method calibration functions
#' @export
find_optimal_calibration_threshold <- function(vh_corrected,
                                                primary_method = "HRM",
                                                secondary_method,
                                                sensor_position = "outer",
                                                threshold_start = 0,
                                                threshold_max = 20,
                                                threshold_step = 0.5,
                                                fit_type = "auto",
                                                min_points = 50,
                                                create_plots = TRUE,
                                                verbose = TRUE,
                                                manual_threshold = NULL,
                                                velocity_col = "Vh_cm_hr") {

  # Input validation
  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", velocity_col)
  missing_cols <- setdiff(required_cols, names(vh_corrected))
  if (length(missing_cols) > 0) {
    stop("vh_corrected missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!primary_method %in% vh_corrected$method) {
    stop("primary_method '", primary_method, "' not found in data")
  }

  if (!secondary_method %in% vh_corrected$method) {
    stop("secondary_method '", secondary_method, "' not found in data")
  }

  if (threshold_start < 0) {
    stop("threshold_start must be >= 0")
  }

  if (threshold_max <= threshold_start) {
    stop("threshold_max must be greater than threshold_start")
  }

  if (threshold_step <= 0) {
    stop("threshold_step must be > 0")
  }

  # Generate threshold sequence
  thresholds <- seq(threshold_start, threshold_max, by = threshold_step)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("OPTIMAL CALIBRATION THRESHOLD SEARCH\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Primary method:", primary_method, "\n")
    cat("Secondary method:", secondary_method, "\n")
    cat("Sensor position:", toupper(sensor_position), "\n")
    cat("Threshold range:", threshold_start, "-", threshold_max, "cm/hr\n")
    cat("Step size:", threshold_step, "cm/hr\n")
    cat("Testing", length(thresholds), "thresholds...\n")
    cat("\n")
  }

  # Extract data for both methods
  primary_data <- vh_corrected[
    vh_corrected$method == primary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  secondary_data <- vh_corrected[
    vh_corrected$method == secondary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  if (nrow(primary_data) == 0 || nrow(secondary_data) == 0) {
    stop("No data found for one or both methods at sensor position: ", sensor_position)
  }

  # Merge by pulse_id
  merged_data <- merge(
    primary_data[, c("pulse_id", velocity_col)],
    secondary_data[, c("pulse_id", velocity_col)],
    by = "pulse_id",
    suffixes = c("_primary", "_secondary")
  )

  if (nrow(merged_data) == 0) {
    stop("No matching pulse_ids between methods")
  }

  # ============================================================================
  # MANUAL THRESHOLD MODE: Skip optimization if manual threshold provided
  # ============================================================================
  if (!is.null(manual_threshold)) {
    if (verbose) {
      cat("\n")
      cat(strrep("=", 72), "\n")
      cat("MANUAL CALIBRATION THRESHOLD\n")
      cat(strrep("=", 72), "\n")
      cat("\n")
      cat("Primary method:", primary_method, "\n")
      cat("Secondary method:", secondary_method, "\n")
      cat("Sensor position:", toupper(sensor_position), "\n")
      cat("Manual threshold:", manual_threshold, "cm/hr\n")
      cat("\n")
    }

    # Perform calibration at manual threshold
    manual_calibration <- calibrate_method_to_primary(
      vh_corrected = vh_corrected,
      primary_method = primary_method,
      secondary_method = secondary_method,
      sensor_position = sensor_position,
      threshold_velocity = manual_threshold,
      fit_type = fit_type,
      min_points = min_points,
      verbose = verbose,
      velocity_col = velocity_col
    )

    # Create minimal threshold_results with just the manual threshold
    threshold_results <- data.frame(
      threshold = manual_threshold,
      r_squared = manual_calibration$r_squared,
      rmse = manual_calibration$rmse,
      n_points = manual_calibration$n_points,
      fit_type_used = manual_calibration$fit_type,
      stringsAsFactors = FALSE
    )

    # Create result object
    result <- list(
      optimal_threshold = manual_threshold,
      optimal_r_squared = manual_calibration$r_squared,
      optimal_calibration = manual_calibration,
      threshold_results = threshold_results,
      primary_method = primary_method,
      secondary_method = secondary_method,
      sensor_position = sensor_position,
      manual_mode = TRUE
    )

    # Create plots if requested (no R² vs threshold plot in manual mode)
    if (create_plots) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        warning("ggplot2 not available. Plots not created.")
      } else {
        # Only create calibration scatter plot (no R² vs threshold in manual mode)
        calibration_plot <- create_single_calibration_plot(
          calibration_data = manual_calibration$calibration_data,
          calibration = manual_calibration,
          primary_method = primary_method,
          secondary_method = secondary_method
        )
        result$plots <- list(calibration_plot = calibration_plot)
      }
    }

    if (verbose) {
      cat(strrep("=", 72), "\n")
      cat("\n")
    }

    class(result) <- c("threshold_optimization_result", "list")
    return(result)
  }

  # ============================================================================
  # AUTOMATIC THRESHOLD OPTIMIZATION MODE
  # ============================================================================

  # Initialize results storage
  results <- data.frame(
    threshold = numeric(length(thresholds)),
    r_squared = numeric(length(thresholds)),
    rmse = numeric(length(thresholds)),
    n_points = integer(length(thresholds)),
    fit_type_used = character(length(thresholds)),
    stringsAsFactors = FALSE
  )

  # Test each threshold
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]

    # Filter to VALID calibration range (data BELOW threshold where both methods agree)
    calib_data <- merged_data[merged_data$Vh_cm_hr_primary <= threshold, ]

    # Check minimum points
    if (nrow(calib_data) < min_points) {
      results$threshold[i] <- threshold
      results$r_squared[i] <- NA_real_
      results$rmse[i] <- NA_real_
      results$n_points[i] <- nrow(calib_data)
      results$fit_type_used[i] <- NA_character_
      next
    }

    # Determine fit type for this threshold
    if (fit_type == "auto") {
      # Try both fits
      tryCatch({
        linear_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary, data = calib_data)
        quad_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary + I(Vh_cm_hr_secondary^2),
                       data = calib_data)

        r2_linear <- summary(linear_fit)$r.squared
        r2_quad <- summary(quad_fit)$r.squared

        # Use quadratic if it improves R² by more than 0.02
        if (r2_quad - r2_linear > 0.02) {
          final_fit <- quad_fit
          fit_used <- "quadratic"
          r2 <- r2_quad
        } else {
          final_fit <- linear_fit
          fit_used <- "linear"
          r2 <- r2_linear
        }
      }, error = function(e) {
        # If fitting fails, return NAs
        results$threshold[i] <<- threshold
        results$r_squared[i] <<- NA_real_
        results$rmse[i] <<- NA_real_
        results$n_points[i] <<- nrow(calib_data)
        results$fit_type_used[i] <<- "error"
        return(NULL)
      })

      if (results$fit_type_used[i] == "error") next

    } else if (fit_type == "linear") {
      tryCatch({
        final_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary, data = calib_data)
        r2 <- summary(final_fit)$r.squared
        fit_used <- "linear"
      }, error = function(e) {
        results$threshold[i] <<- threshold
        results$r_squared[i] <<- NA_real_
        results$rmse[i] <<- NA_real_
        results$n_points[i] <<- nrow(calib_data)
        results$fit_type_used[i] <<- "error"
        return(NULL)
      })

      if (results$fit_type_used[i] == "error") next

    } else if (fit_type == "quadratic") {
      tryCatch({
        final_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary + I(Vh_cm_hr_secondary^2),
                        data = calib_data)
        r2 <- summary(final_fit)$r.squared
        fit_used <- "quadratic"
      }, error = function(e) {
        results$threshold[i] <<- threshold
        results$r_squared[i] <<- NA_real_
        results$rmse[i] <<- NA_real_
        results$n_points[i] <<- nrow(calib_data)
        results$fit_type_used[i] <<- "error"
        return(NULL)
      })

      if (results$fit_type_used[i] == "error") next
    }

    # Calculate RMSE
    predictions <- predict(final_fit, calib_data)
    rmse <- sqrt(mean((predictions - calib_data$Vh_cm_hr_primary)^2))

    # Store results
    results$threshold[i] <- threshold
    results$r_squared[i] <- r2
    results$rmse[i] <- rmse
    results$n_points[i] <- nrow(calib_data)
    results$fit_type_used[i] <- fit_used
  }

  # Remove rows with NA R²
  valid_results <- results[!is.na(results$r_squared), ]

  if (nrow(valid_results) == 0) {
    stop("No valid calibrations found. Try:\n",
         "  - Reducing threshold_max\n",
         "  - Reducing min_points\n",
         "  - Checking data quality")
  }

  # Find optimal threshold (maximum R²)
  optimal_idx <- which.max(valid_results$r_squared)
  optimal_threshold <- valid_results$threshold[optimal_idx]
  optimal_r_squared <- valid_results$r_squared[optimal_idx]
  optimal_fit_type <- valid_results$fit_type_used[optimal_idx]

  if (verbose) {
    cat(strrep("-", 72), "\n")
    cat("\n")
    cat("RESULTS\n")
    cat("  Optimal threshold:", optimal_threshold, "cm/hr\n")
    cat("  R² at optimal:", round(optimal_r_squared, 4), "\n")
    cat("  Fit type:", optimal_fit_type, "\n")
    cat("  Points used:", valid_results$n_points[optimal_idx], "\n")
    cat("\n")
  }

  # Perform full calibration at optimal threshold
  optimal_calibration <- calibrate_method_to_primary(
    vh_corrected = vh_corrected,
    primary_method = primary_method,
    secondary_method = secondary_method,
    sensor_position = sensor_position,
    threshold_velocity = optimal_threshold,
    fit_type = optimal_fit_type,
    min_points = min_points,
    verbose = FALSE,
    velocity_col = velocity_col
  )

  # Create result object
  result <- list(
    optimal_threshold = optimal_threshold,
    optimal_r_squared = optimal_r_squared,
    optimal_calibration = optimal_calibration,
    threshold_results = results,
    primary_method = primary_method,
    secondary_method = secondary_method,
    sensor_position = sensor_position
  )

  # Create diagnostic plots if requested
  if (create_plots) {
    plots <- create_calibration_diagnostic_plots(
      threshold_results = valid_results,
      optimal_threshold = optimal_threshold,
      calibration_data = optimal_calibration$calibration_data,
      calibration = optimal_calibration,
      primary_method = primary_method,
      secondary_method = secondary_method
    )
    result$plots <- plots
  }

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  class(result) <- c("threshold_optimization_result", "list")

  return(result)
}


#' Calibrate Secondary Method to Primary Method
#'
#' Fits relationship between primary and secondary methods in their overlap
#' region, creating a transformation function to align the secondary method
#' to the primary method scale.
#'
#' @param vh_corrected Corrected velocity data (after spacing correction).
#' @param primary_method Primary method name (default: "HRM").
#' @param secondary_method Secondary method name (e.g., "MHR").
#' @param sensor_position Sensor position ("outer" or "inner").
#' @param threshold_velocity Minimum velocity for calibration (cm/hr).
#'   This is the breakpoint where method switching occurs.
#' @param handover_pct Proportion of threshold to use for handover window (default: 0.5).
#'   Training data is filtered to points between \code{threshold * (1 - handover_pct)}
#'   and \code{threshold}. This focuses calibration on the transition zone for
#'   smooth handover.
#' @param fit_type Type of fit: "linear", "quadratic", or "auto" (default: "auto").
#' @param min_points Minimum points required for calibration (default: 50).
#' @param verbose Logical indicating whether to print summary (default: TRUE).
#'
#' @return A list (class "method_calibration") with components \code{coefficients}
#'   (fit coefficients), \code{fit_type} ("linear" or "quadratic"), \code{r_squared}
#'   (R-squared of fit), \code{rmse} (root mean squared error), \code{n_points}
#'   (number of calibration points), \code{threshold} (velocity threshold used),
#'   \code{handover_window} (range of velocities used for calibration),
#'   \code{transformation_function} (function to transform secondary method values),
#'   \code{primary_method}, \code{secondary_method}, \code{sensor_position}, and
#'   \code{calibration_data} (data frame for plotting)
#'
#' @details
#' **Calibration Process:**
#'
#' 1. Merges primary and secondary method data by pulse_id
#' 2. Filters to **handover window**: points where primary method is between
#'    \code{threshold * (1 - handover_pct)} and \code{threshold}
#' 3. Fits regression: primary ~ secondary (or primary ~ secondary + secondary²)
#' 4. Creates transformation function to convert secondary values to primary scale
#' 5. This transformation is then applied to ALL secondary data, especially above
#'    the threshold where the primary method becomes unreliable
#'
#' **Handover Window (NEW):**
#'
#' The handover window ensures smooth transitions when methods are attenuated
#' (secondary produces lower values than primary even in valid range):
#' \itemize{
#'   \item With \code{handover_pct = 0.5} and \code{threshold = 10}, calibration
#'         uses data from 5-10 cm/hr
#'   \item This optimises the fit for the transition zone
#'   \item Ensures the calibrated secondary method value at the threshold exactly
#'         matches the primary method value, preventing discontinuities
#'   \item Use \code{handover_pct = 1.0} to use all data below threshold (legacy behaviour)
#' }
#'
#' **Fit Type Selection:**
#'
#' If \code{fit_type = "auto"}:
#' \itemize{
#'   \item Tries both linear and quadratic fits
#'   \item Uses quadratic if R² improves by more than 0.02
#'   \item Otherwise uses simpler linear fit
#' }
#'
#' **Transformation:**
#'
#' The returned transformation function can be used to transform the entire
#' secondary method dataset to the primary scale using
#' \code{\link{transform_secondary_method}}.
#'
#' @examples
#' \dontrun{
#' # Calibrate MHR to HRM at 10 cm/hr threshold with handover window
#' calibration <- calibrate_method_to_primary(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer",
#'   threshold_velocity = 10,
#'   handover_pct = 0.5  # Use 5-10 cm/hr for calibration
#' )
#'
#' print(calibration$r_squared)
#' print(calibration$coefficients)
#' print(calibration$handover_window)
#' }
#'
#' @family method calibration functions
#' @export
calibrate_method_to_primary <- function(vh_corrected,
                                         primary_method = "HRM",
                                         secondary_method,
                                         sensor_position = "outer",
                                         threshold_velocity,
                                         handover_pct = 0.5,
                                         fit_type = "auto",
                                         min_points = 50,
                                         verbose = TRUE,
                                         velocity_col = "Vh_cm_hr") {

  # Input validation
  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", velocity_col)
  missing_cols <- setdiff(required_cols, names(vh_corrected))
  if (length(missing_cols) > 0) {
    stop("vh_corrected missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Extract data for both methods
  primary_data <- vh_corrected[
    vh_corrected$method == primary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  secondary_data <- vh_corrected[
    vh_corrected$method == secondary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  if (nrow(primary_data) == 0 || nrow(secondary_data) == 0) {
    stop("No data found for one or both methods")
  }

  # Merge by pulse_id
  calib_data <- merge(
    primary_data[, c("pulse_id", velocity_col)],
    secondary_data[, c("pulse_id", velocity_col)],
    by = "pulse_id",
    suffixes = c("_primary", "_secondary")
  )

  # CRITICAL FIX: Construct column names based on velocity_col
  # When velocity_col = "Vc_cm_hr", merged columns are "Vc_cm_hr_primary" and "Vc_cm_hr_secondary"
  primary_col <- paste0(velocity_col, "_primary")
  secondary_col <- paste0(velocity_col, "_secondary")

  # Calculate handover window bounds
  handover_lower <- threshold_velocity * (1 - handover_pct)
  handover_upper <- threshold_velocity

  # Filter to HANDOVER WINDOW (transition zone just below threshold)
  # This optimises calibration for smooth method transitions
  calib_data <- calib_data[
    calib_data[[primary_col]] >= handover_lower &
    calib_data[[primary_col]] <= handover_upper,
  ]

  if (nrow(calib_data) < min_points) {
    stop(sprintf(
      "Insufficient data for calibration (n=%d, need %d).\nHandover window: %.2f - %.2f cm/hr\nTry:\n  - Increasing handover_pct (currently %.2f)\n  - Lowering min_points\n  - Adjusting threshold_velocity",
      nrow(calib_data), min_points, handover_lower, handover_upper, handover_pct
    ))
  }

  # Determine fit type
  if (fit_type == "auto") {
    # Try both, pick best R² (using dynamic column names)
    formula_linear <- as.formula(paste(primary_col, "~", secondary_col))
    formula_quad <- as.formula(paste(primary_col, "~", secondary_col, "+ I(", secondary_col, "^2)"))

    linear_fit <- lm(formula_linear, data = calib_data)
    quad_fit <- lm(formula_quad, data = calib_data)

    r2_linear <- summary(linear_fit)$r.squared
    r2_quad <- summary(quad_fit)$r.squared

    # Use quadratic if it improves R² by more than 0.02
    if (r2_quad - r2_linear > 0.02) {
      fit_type <- "quadratic"
      final_fit <- quad_fit
      r_squared <- r2_quad
    } else {
      fit_type <- "linear"
      final_fit <- linear_fit
      r_squared <- r2_linear
    }
  } else if (fit_type == "linear") {
    formula_linear <- as.formula(paste(primary_col, "~", secondary_col))
    final_fit <- lm(formula_linear, data = calib_data)
    r_squared <- summary(final_fit)$r.squared
  } else if (fit_type == "quadratic") {
    formula_quad <- as.formula(paste(primary_col, "~", secondary_col, "+ I(", secondary_col, "^2)"))
    final_fit <- lm(formula_quad, data = calib_data)
    r_squared <- summary(final_fit)$r.squared
  } else {
    stop("fit_type must be 'linear', 'quadratic', or 'auto'")
  }

  coeffs <- coef(final_fit)

  # Calculate RMSE
  predictions <- predict(final_fit, calib_data)
  rmse <- sqrt(mean((predictions - calib_data[[primary_col]])^2))

  # Create transformation function
  if (fit_type == "linear") {
    # y = a + b*x
    transform_fn <- function(x) {
      coeffs[1] + coeffs[2] * x
    }
  } else {
    # y = a + b*x + c*x²
    transform_fn <- function(x) {
      coeffs[1] + coeffs[2] * x + coeffs[3] * x^2
    }
  }

  # Report
  if (verbose) {
    cat("\n", strrep("=", 70), "\n")
    cat("METHOD CALIBRATION:", toupper(secondary_method), "→", toupper(primary_method), "\n")
    cat(strrep("=", 70), "\n\n")
    cat("Sensor:", toupper(sensor_position), "\n")
    cat("Threshold:", threshold_velocity, "cm/hr\n")
    cat("Handover window:", round(handover_lower, 2), "-", round(handover_upper, 2), "cm/hr\n")
    cat("Handover %:", round(handover_pct * 100, 0), "%\n")
    cat("Calibration points:", nrow(calib_data), "\n")
    cat("Fit type:", fit_type, "\n")
    cat("R²:", round(r_squared, 4), "\n")
    cat("RMSE:", round(rmse, 3), "cm/hr\n\n")

    if (fit_type == "linear") {
      cat(sprintf("Transformation: %s = %.4f + %.4f × %s\n",
                  primary_method, coeffs[1], coeffs[2], secondary_method))
    } else {
      cat(sprintf("Transformation: %s = %.4f + %.4f × %s + %.6f × %s²\n",
                  primary_method, coeffs[1], coeffs[2], secondary_method,
                  coeffs[3], secondary_method))
    }
    cat(strrep("=", 70), "\n\n")
  }

  # Create result
  result <- list(
    coefficients = coeffs,
    fit_type = fit_type,
    r_squared = r_squared,
    rmse = rmse,
    n_points = nrow(calib_data),
    threshold = threshold_velocity,
    handover_window = c(handover_lower, handover_upper),
    handover_pct = handover_pct,
    transformation_function = transform_fn,
    transform_func = transform_fn,  # Alias for compatibility with apply_sdma_switching
    primary_method = primary_method,
    secondary_method = secondary_method,
    sensor_position = sensor_position,
    calibration_data = calib_data  # Store for plotting
  )

  class(result) <- c("method_calibration", "list")
  return(result)
}


#' Transform Secondary Method Using Calibration
#'
#' Applies a calibration transformation to align a secondary method to the
#' primary method scale across the entire dataset.
#'
#' @param vh_corrected Corrected velocity data.
#' @param calibration Calibration object from \code{\link{calibrate_method_to_primary}}.
#'
#' @return The \code{vh_corrected} data frame with transformed secondary method
#'   values. A new column \code{calibration_applied} is added (TRUE for
#'   transformed rows, FALSE otherwise).
#'
#' @details
#' This function applies the calibration transformation to ALL data points for
#' the secondary method, not just those above the threshold. This ensures the
#' entire time series is on a consistent scale.
#'
#' The original (uncalibrated) values are replaced with transformed values.
#' To preserve originals, make a copy of \code{vh_corrected} before transformation.
#'
#' @examples
#' \dontrun{
#' # First calibrate
#' calibration <- calibrate_method_to_primary(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer",
#'   threshold_velocity = 10
#' )
#'
#' # Then transform entire MHR dataset
#' vh_transformed <- transform_secondary_method(vh_corrected, calibration)
#' }
#'
#' @family method calibration functions
#' @export
transform_secondary_method <- function(vh_corrected, calibration, velocity_col = "Vh_cm_hr") {

  # Input validation
  if (!inherits(calibration, "method_calibration")) {
    stop("calibration must be a method_calibration object from calibrate_method_to_primary()")
  }

  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  # Find secondary method rows
  secondary_rows <- vh_corrected$method == calibration$secondary_method &
                    vh_corrected$sensor_position == calibration$sensor_position

  if (!any(secondary_rows)) {
    warning("No data found for transformation (method: ", calibration$secondary_method,
            ", position: ", calibration$sensor_position, ")")
    return(vh_corrected)
  }

  # Apply transformation
  original_values <- vh_corrected[[velocity_col]][secondary_rows]
  transformed_values <- calibration$transformation_function(original_values)

  # Update values
  vh_corrected[[velocity_col]][secondary_rows] <- transformed_values

  # Add flag indicating transformation was applied
  if (!"calibration_applied" %in% names(vh_corrected)) {
    vh_corrected$calibration_applied <- FALSE
  }
  vh_corrected$calibration_applied[secondary_rows] <- TRUE

  cat(sprintf("\nTransformed %d %s (%s) values using calibration to %s\n",
              sum(secondary_rows),
              calibration$secondary_method,
              calibration$sensor_position,
              calibration$primary_method))

  return(vh_corrected)
}


#' Calibrate Multiple Secondary Methods to Primary Method
#'
#' Wrapper function that calibrates multiple secondary methods to a primary
#' method in a single call. Each method gets its own optimal threshold and
#' calibration parameters.
#'
#' @param vh_corrected Corrected velocity data (after spacing correction).
#' @param primary_method Primary method name (default: "HRM").
#' @param secondary_methods Character vector of secondary method names to
#'   calibrate (e.g., \code{c("MHR", "Tmax_Klu", "HRMXa")}).
#' @param sensor_position Sensor position ("outer" or "inner").
#' @param threshold_start Minimum velocity threshold to test (cm/hr) (default: 0).
#' @param threshold_max Maximum velocity threshold to test (cm/hr) (default: 20).
#' @param threshold_step Increment between thresholds (cm/hr) (default: 0.5).
#' @param fit_type Type of fit: "linear", "quadratic", or "auto" (default: "auto").
#' @param min_points Minimum points required for calibration (default: 50).
#' @param create_plots Logical indicating whether to create diagnostic plots (default: TRUE).
#' @param verbose Logical indicating whether to print progress (default: TRUE).
#' @param manual_thresholds Optional named list of manual thresholds for specific
#'   methods (e.g., \code{list(MHR = 10.5, Tmax_Klu = 15.0)}). Methods not in
#'   this list will use automatic optimization. Default: NULL (all automatic).
#'
#' @return A named list where each element is a calibration result for one
#'   secondary method. Each element contains:
#'   \itemize{
#'     \item \code{optimal_threshold}: Best threshold for this method
#'     \item \code{optimal_r_squared}: R² at optimal threshold
#'     \item \code{optimal_calibration}: Full calibration object
#'     \item \code{threshold_results}: Data frame of all thresholds tested
#'     \item \code{plots}: Diagnostic plots (if \code{create_plots = TRUE})
#'   }
#'
#' @details
#' **Practical Usage:**
#'
#' This function simplifies calibrating multiple methods:
#'
#' \preformatted{
#' # Calibrate all methods at once
#' calibrations <- calibrate_multiple_methods(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu"),
#'   sensor_position = "outer"
#' )
#'
#' # Access individual results
#' calibrations$MHR$optimal_threshold        # e.g., 10.5 cm/hr
#' calibrations$Tmax_Klu$optimal_threshold   # e.g., 15.0 cm/hr (different!)
#'
#' # View plots
#' print(calibrations$MHR$plots$r_squared_plot)
#' }
#'
#' **Manual Threshold Override:**
#'
#' You can specify manual thresholds for some methods while letting others
#' optimise automatically:
#'
#' \preformatted{
#' calibrations <- calibrate_multiple_methods(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu", "HRMXa"),
#'   sensor_position = "outer",
#'   manual_thresholds = list(MHR = 8.5)  # MHR manual, others automatic
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Calibrate multiple methods for outer sensors
#' calibrations_outer <- calibrate_multiple_methods(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu"),
#'   sensor_position = "outer"
#' )
#'
#' # Then for inner sensors
#' calibrations_inner <- calibrate_multiple_methods(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu"),
#'   sensor_position = "inner"
#' )
#'
#' # Apply transformations
#' vh_calibrated <- transform_multiple_methods(vh_corrected, calibrations_outer)
#' vh_calibrated <- transform_multiple_methods(vh_calibrated, calibrations_inner)
#' }
#'
#' @family method calibration functions
#' @export
calibrate_multiple_methods <- function(vh_corrected,
                                        primary_method = "HRM",
                                        secondary_methods,
                                        sensor_position = "outer",
                                        threshold_start = 0,
                                        threshold_max = 20,
                                        threshold_step = 0.5,
                                        fit_type = "auto",
                                        min_points = 50,
                                        create_plots = TRUE,
                                        verbose = TRUE,
                                        manual_thresholds = NULL,
                                        velocity_col = "Vh_cm_hr") {

  # Input validation
  if (length(secondary_methods) == 0) {
    stop("secondary_methods must contain at least one method name")
  }

  if (!is.null(manual_thresholds) && !is.list(manual_thresholds)) {
    stop("manual_thresholds must be a named list or NULL")
  }

  # Initialize results list
  calibration_results <- list()

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("MULTI-METHOD CALIBRATION\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Primary method:", primary_method, "\n")
    cat("Secondary methods:", paste(secondary_methods, collapse = ", "), "\n")
    cat("Sensor position:", toupper(sensor_position), "\n")
    cat("Methods to calibrate:", length(secondary_methods), "\n")
    cat("\n")
  }

  # Calibrate each secondary method
  for (i in seq_along(secondary_methods)) {
    method <- secondary_methods[i]

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing method", i, "of", length(secondary_methods), ":", method, "\n")
      cat(strrep("-", 72), "\n")
    }

    # Check if manual threshold specified for this method
    manual_thresh <- NULL
    if (!is.null(manual_thresholds) && method %in% names(manual_thresholds)) {
      manual_thresh <- manual_thresholds[[method]]
      if (verbose) {
        cat("Using manual threshold:", manual_thresh, "cm/hr\n\n")
      }
    }

    # Calibrate this method
    tryCatch({
      calibration_results[[method]] <- find_optimal_calibration_threshold(
        vh_corrected = vh_corrected,
        primary_method = primary_method,
        secondary_method = method,
        sensor_position = sensor_position,
        threshold_start = threshold_start,
        threshold_max = threshold_max,
        threshold_step = threshold_step,
        fit_type = fit_type,
        min_points = min_points,
        create_plots = create_plots,
        verbose = verbose,
        manual_threshold = manual_thresh,
        velocity_col = velocity_col
      )
    }, error = function(e) {
      warning("Failed to calibrate ", method, ": ", e$message)
      calibration_results[[method]] <<- NULL
    })
  }

  # Remove NULL entries (failed calibrations)
  calibration_results <- calibration_results[!sapply(calibration_results, is.null)]

  if (length(calibration_results) == 0) {
    stop("All calibrations failed. Check data quality and method availability.")
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("MULTI-METHOD CALIBRATION COMPLETE\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Successfully calibrated", length(calibration_results), "of",
        length(secondary_methods), "methods\n")
    cat("\n")

    # Summary table
    cat("SUMMARY\n")
    cat(strrep("-", 72), "\n")
    cat(sprintf("%-15s %12s %12s %12s\n", "Method", "Threshold", "R\u00b2", "Fit Type"))
    cat(strrep("-", 72), "\n")
    for (method in names(calibration_results)) {
      result <- calibration_results[[method]]
      cat(sprintf("%-15s %12.2f %12.4f %12s\n",
                  method,
                  result$optimal_threshold,
                  result$optimal_r_squared,
                  result$optimal_calibration$fit_type))
    }
    cat(strrep("-", 72), "\n")
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  class(calibration_results) <- c("multi_method_calibration", "list")
  return(calibration_results)
}


#' Transform Multiple Secondary Methods Using Calibrations
#'
#' Applies multiple calibration transformations in a single call. Takes the
#' output from \code{\link{calibrate_multiple_methods}} and applies each
#' calibration to its corresponding method.
#'
#' @param vh_corrected Corrected velocity data.
#' @param calibrations Multi-method calibration object from
#'   \code{\link{calibrate_multiple_methods}} (a named list of calibrations).
#' @param verbose Logical indicating whether to print progress (default: TRUE).
#'
#' @return The \code{vh_corrected} data frame with all secondary methods
#'   transformed to the primary method scale.
#'
#' @details
#' This function loops through each calibration in the \code{calibrations} list
#' and applies the transformation using \code{\link{transform_secondary_method}}.
#'
#' The transformations are applied sequentially in the order they appear in the
#' calibrations list. Each transformation modifies \code{vh_corrected} in place.
#'
#' @examples
#' \dontrun{
#' # First calibrate multiple methods
#' calibrations <- calibrate_multiple_methods(
#'   vh_corrected,
#'   primary_method = "HRM",
#'   secondary_methods = c("MHR", "Tmax_Klu"),
#'   sensor_position = "outer"
#' )
#'
#' # Then transform all at once
#' vh_calibrated <- transform_multiple_methods(vh_corrected, calibrations)
#' }
#'
#' @family method calibration functions
#' @export
transform_multiple_methods <- function(vh_corrected,
                                        calibrations,
                                        verbose = TRUE,
                                        velocity_col = "Vh_cm_hr") {

  # Input validation
  if (!is.list(calibrations)) {
    stop("calibrations must be a list (from calibrate_multiple_methods)")
  }

  if (length(calibrations) == 0) {
    warning("calibrations list is empty. No transformations applied.")
    return(vh_corrected)
  }

  # Extract sensor position and primary method from first calibration
  first_calib <- calibrations[[1]]$optimal_calibration
  sensor_pos <- first_calib$sensor_position
  primary_method <- first_calib$primary_method

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("BATCH TRANSFORMATION\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Primary method:", primary_method, "\n")
    cat("Sensor position:", toupper(sensor_pos), "\n")
    cat("Methods to transform:", length(calibrations), "\n")
    cat("\n")
  }

  # Apply each transformation
  for (method_name in names(calibrations)) {
    if (verbose) {
      cat("Transforming", method_name, "...\n")
    }

    calibration_obj <- calibrations[[method_name]]$optimal_calibration

    vh_corrected <- transform_secondary_method(
      vh_corrected = vh_corrected,
      calibration = calibration_obj,
      velocity_col = velocity_col
    )
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("BATCH TRANSFORMATION COMPLETE\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  return(vh_corrected)
}


#' Create Single Calibration Scatter Plot
#'
#' Creates just the calibration scatter plot (used in manual threshold mode).
#'
#' @param calibration_data Data frame of calibration points.
#' @param calibration Calibration object.
#' @param primary_method Primary method name.
#' @param secondary_method Secondary method name.
#'
#' @return ggplot2 calibration scatter plot
#'
#' @keywords internal
create_single_calibration_plot <- function(calibration_data,
                                            calibration,
                                            primary_method,
                                            secondary_method) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  # Generate fitted line
  x_range <- range(calibration_data$Vh_cm_hr_secondary, na.rm = TRUE)
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  y_pred <- calibration$transformation_function(x_seq)
  fit_line <- data.frame(x = x_seq, y = y_pred)

  calibration_plot <- ggplot2::ggplot(calibration_data,
                                       ggplot2::aes(x = Vh_cm_hr_secondary, y = Vh_cm_hr_primary)) +
    ggplot2::geom_point(alpha = 0.3, color = "grey40") +
    ggplot2::geom_line(data = fit_line, ggplot2::aes(x = x, y = y),
                       color = "red", linewidth = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "blue", alpha = 0.5) +
    ggplot2::labs(
      title = paste("Method Calibration:", secondary_method, "→", primary_method),
      subtitle = paste0("Threshold: ", calibration$threshold, " cm/hr | ",
                        "R² = ", round(calibration$r_squared, 4), " | ",
                        "Fit: ", calibration$fit_type),
      x = paste(secondary_method, "Velocity (cm/hr)"),
      y = paste(primary_method, "Velocity (cm/hr)")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )

  return(calibration_plot)
}


#' Create Calibration Diagnostic Plots
#'
#' Creates diagnostic plots for threshold optimization and method calibration.
#'
#' @param threshold_results Data frame of threshold optimization results.
#' @param optimal_threshold The optimal threshold value.
#' @param calibration_data Data frame of calibration points.
#' @param calibration Calibration object.
#' @param primary_method Primary method name.
#' @param secondary_method Secondary method name.
#'
#' @return List with \code{r_squared_plot} (plot of R² vs threshold velocity)
#'   and \code{calibration_plot} (scatter plot with line of best fit)
#'
#' @keywords internal
create_calibration_diagnostic_plots <- function(threshold_results,
                                                 optimal_threshold,
                                                 calibration_data,
                                                 calibration,
                                                 primary_method,
                                                 secondary_method) {

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available. Plots not created.")
    return(NULL)
  }

  # Plot 1: R² vs Threshold
  r_squared_plot <- ggplot2::ggplot(threshold_results, ggplot2::aes(x = threshold, y = r_squared)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_vline(xintercept = optimal_threshold, linetype = "dashed",
                        color = "red", linewidth = 1) +
    ggplot2::annotate("text", x = optimal_threshold, y = max(threshold_results$r_squared, na.rm = TRUE),
                      label = paste0("Optimal: ", optimal_threshold, " cm/hr\nR² = ",
                                     round(max(threshold_results$r_squared, na.rm = TRUE), 4)),
                      hjust = -0.1, vjust = 1, color = "red") +
    ggplot2::labs(
      title = paste("Threshold Optimization:", secondary_method, "→", primary_method),
      x = "Threshold Velocity (cm/hr)",
      y = "R²"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )

  # Plot 2: Calibration scatter with line of best fit
  # Generate fitted line
  x_range <- range(calibration_data$Vh_cm_hr_secondary, na.rm = TRUE)
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  y_pred <- calibration$transformation_function(x_seq)
  fit_line <- data.frame(x = x_seq, y = y_pred)

  calibration_plot <- ggplot2::ggplot(calibration_data,
                                       ggplot2::aes(x = Vh_cm_hr_secondary, y = Vh_cm_hr_primary)) +
    ggplot2::geom_point(alpha = 0.3, color = "grey40") +
    ggplot2::geom_line(data = fit_line, ggplot2::aes(x = x, y = y),
                       color = "red", linewidth = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "blue", alpha = 0.5) +
    ggplot2::labs(
      title = paste("Method Calibration:", secondary_method, "→", primary_method),
      subtitle = paste0("Threshold: ", optimal_threshold, " cm/hr | ",
                        "R² = ", round(calibration$r_squared, 4), " | ",
                        "Fit: ", calibration$fit_type),
      x = paste(secondary_method, "Velocity (cm/hr)"),
      y = paste(primary_method, "Velocity (cm/hr)")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )

  return(list(
    r_squared_plot = r_squared_plot,
    calibration_plot = calibration_plot
  ))
}


#' Print Method for Threshold Optimization Results
#'
#' @param x A threshold_optimization_result object
#' @param ... Additional arguments (ignored)
#' @export
print.threshold_optimization_result <- function(x, ...) {

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("THRESHOLD OPTIMIZATION RESULTS\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  cat("Methods:", x$secondary_method, "→", x$primary_method, "\n")
  cat("Sensor:", toupper(x$sensor_position), "\n")
  cat("\n")

  cat("OPTIMAL THRESHOLD\n")
  cat(strrep("-", 72), "\n")
  cat("  Velocity:", x$optimal_threshold, "cm/hr\n")
  cat("  R²:", round(x$optimal_r_squared, 4), "\n")
  cat("  Fit type:", x$optimal_calibration$fit_type, "\n")
  cat("  Points used:", x$optimal_calibration$n_points, "\n")
  cat("\n")

  cat("THRESHOLD SEARCH SUMMARY\n")
  cat(strrep("-", 72), "\n")
  valid_results <- x$threshold_results[!is.na(x$threshold_results$r_squared), ]
  cat("  Thresholds tested:", nrow(x$threshold_results), "\n")
  cat("  Valid results:", nrow(valid_results), "\n")
  cat("  R² range:", round(min(valid_results$r_squared), 4), "-",
      round(max(valid_results$r_squared), 4), "\n")
  cat("\n")

  cat("CALIBRATION EQUATION\n")
  cat(strrep("-", 72), "\n")
  coeffs <- x$optimal_calibration$coefficients
  if (x$optimal_calibration$fit_type == "linear") {
    cat(sprintf("  %s = %.4f + %.4f × %s\n",
                x$primary_method, coeffs[1], coeffs[2], x$secondary_method))
  } else {
    cat(sprintf("  %s = %.4f + %.4f × %s + %.6f × %s²\n",
                x$primary_method, coeffs[1], coeffs[2], x$secondary_method,
                coeffs[3], x$secondary_method))
  }
  cat("\n")

  if (!is.null(x$plots)) {
    cat("DIAGNOSTIC PLOTS AVAILABLE\n")
    cat(strrep("-", 72), "\n")
    cat("  - $plots$r_squared_plot: R² vs threshold\n")
    cat("  - $plots$calibration_plot: Calibration scatter plot\n")
    cat("\n")
  }

  cat(strrep("=", 72), "\n")
  cat("\n")

  invisible(x)
}



#' Print Method for Method Calibration Results
#'
#' @param x A method_calibration object
#' @param ... Additional arguments (ignored)
#' @export
print.method_calibration <- function(x, ...) {

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("METHOD CALIBRATION:", toupper(x$secondary_method), "→", toupper(x$primary_method), "\n")
  cat(strrep("=", 70), "\n")
  cat("\n")

  cat("Sensor:", toupper(x$sensor_position), "\n")
  cat("Threshold:", x$threshold, "cm/hr\n")
  cat("Points:", x$n_points, "\n")
  cat("Fit type:", x$fit_type, "\n")
  cat("R²:", round(x$r_squared, 4), "\n")
  cat("RMSE:", round(x$rmse, 3), "cm/hr\n")
  cat("\n")

  cat("TRANSFORMATION EQUATION\n")
  cat(strrep("-", 70), "\n")
  if (x$fit_type == "linear") {
    cat(sprintf("  %s = %.4f + %.4f × %s\n",
                x$primary_method, x$coefficients[1], x$coefficients[2],
                x$secondary_method))
  } else {
    cat(sprintf("  %s = %.4f + %.4f × %s + %.6f × %s²\n",
                x$primary_method, x$coefficients[1], x$coefficients[2],
                x$secondary_method, x$coefficients[3], x$secondary_method))
  }
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("\n")

  invisible(x)
}


#' Compare Methods Using Segmented Regression
#'
#' Identifies the velocity breakpoint where two methods diverge using segmented
#' (piecewise) linear regression. This statistically determines where one method
#' (typically HRM) begins to plateau while the other (e.g., MHR) continues to
#' measure higher velocities - the classic "hockey stick" pattern.
#'
#' @param vh_corrected Data frame of corrected velocity data (after spacing
#'   correction). Must contain columns: pulse_id, method, sensor_position, Vh_cm_hr.
#' @param primary_method Primary method name (default: "HRM"). This is typically
#'   the method that plateaus at higher velocities.
#' @param secondary_method Secondary method name to compare (e.g., "MHR").
#' @param sensor_position Sensor position to analyse ("outer" or "inner").
#' @param initial_breakpoint Initial guess for breakpoint location (cm/hr).
#'   If NULL, uses midpoint of data range. Default: NULL.
#' @param min_points Minimum number of points required for valid analysis (default: 50).
#' @param create_plots Logical indicating whether to create diagnostic plots (default: TRUE).
#' @param verbose Logical indicating whether to print progress (default: TRUE).
#'
#' @return A list with components \code{breakpoint} (estimated breakpoint
#'   velocity in cm/hr), \code{breakpoint_ci} (95\% confidence interval),
#'   \code{breakpoint_se} (standard error), \code{slope_before} (slope before
#'   breakpoint), \code{slope_after} (slope after breakpoint), \code{r_squared}
#'   (overall R² of segmented model), \code{davies_test} (p-value from Davies
#'   test), \code{n_points} (number of points), \code{segmented_model} (full
#'   model object), \code{merged_data} (paired measurements data frame),
#'   \code{plots} (list of diagnostic plots if \code{create_plots = TRUE}),
#'   \code{primary_method}, \code{secondary_method}, and \code{sensor_position}
#'
#' @details
#' **Scientific Background:**
#'
#' Segmented regression (piecewise linear regression) is the gold standard
#' approach for identifying method validity ranges in sap flow research. Unlike
#' testing arbitrary thresholds, this approach:
#'
#' \itemize{
#'   \item Statistically estimates THE breakpoint where methods diverge
#'   \item Provides confidence intervals for the breakpoint location
#'   \item Tests whether the breakpoint is statistically significant (Davies test)
#'   \item Quantifies how slopes change before and after the breakpoint
#' }
#'
#' **Interpretation:**
#'
#' The breakpoint represents the maximum velocity where both methods maintain
#' a linear relationship. Above this point, the primary method (typically HRM)
#' begins to underestimate compared to the secondary method. This identifies:
#'
#' \itemize{
#'   \item The valid measurement range for the primary method
#'   \item The velocity where method switching (sDMA) should occur
#'   \item The calibration region for aligning methods
#' }
#'
#' **Advantages over R² Optimization:**
#'
#' \itemize{
#'   \item \strong{Statistical rigor}: Confidence intervals and significance testing
#'   \item \strong{Objective}: No arbitrary threshold selection
#'   \item \strong{Defensible}: Widely accepted in method comparison studies
#'   \item \strong{Precise}: Identifies the actual divergence point, not maximum correlation
#' }
#'
#' **Diagnostic Plots:**
#'
#' When \code{create_plots = TRUE}, generates:
#' \itemize{
#'   \item **Segmented regression plot**: Shows data, fitted segments, breakpoint,
#'         and confidence interval shading
#'   \item **Residuals plot**: Helps assess model assumptions
#' }
#'
#' @examples
#' \dontrun{
#' # Compare MHR to HRM using segmented regression
#' comparison <- compare_methods_segmented(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer"
#' )
#'
#' # View results
#' print(comparison$breakpoint)  # e.g., 12.3 cm/hr
#' print(comparison$breakpoint_ci)  # e.g., [10.8, 13.8]
#' print(comparison$davies_test)  # p-value for significance
#'
#' # View plot
#' print(comparison$plots$segmented_plot)
#' }
#'
#' @references
#' Muggeo, V.M.R. (2003). Estimating regression models with unknown break-points.
#' Statistics in Medicine, 22(19), 3055-3071.
#'
#' Burgess, S.S.O., et al. (2001). Comparison of sap flow measurement methods.
#' Tree Physiology, 21(9), 589-598.
#'
#' @family method calibration functions
#' @export
compare_methods_segmented <- function(vh_corrected,
                                      primary_method = "HRM",
                                      secondary_method,
                                      sensor_position = "outer",
                                      initial_breakpoint = NULL,
                                      min_points = 50,
                                      create_plots = TRUE,
                                      verbose = TRUE,
                                      velocity_col = "Vh_cm_hr") {

  # Check for segmented package
  if (!requireNamespace("segmented", quietly = TRUE)) {
    stop("Package 'segmented' is required for this function. Install with: install.packages('segmented')")
  }

  # Input validation
  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", velocity_col)
  missing_cols <- setdiff(required_cols, names(vh_corrected))
  if (length(missing_cols) > 0) {
    stop("vh_corrected missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!primary_method %in% vh_corrected$method) {
    stop("primary_method '", primary_method, "' not found in data")
  }

  if (!secondary_method %in% vh_corrected$method) {
    stop("secondary_method '", secondary_method, "' not found in data")
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SEGMENTED REGRESSION METHOD COMPARISON\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Primary method:", primary_method, "\n")
    cat("Secondary method:", secondary_method, "\n")
    cat("Sensor position:", toupper(sensor_position), "\n")
    cat("Approach: Piecewise linear regression (breakpoint detection)\n")
    cat("\n")
  }

  # Extract data for both methods
  primary_data <- vh_corrected[
    vh_corrected$method == primary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  secondary_data <- vh_corrected[
    vh_corrected$method == secondary_method &
    vh_corrected$sensor_position == sensor_position,
  ]

  if (nrow(primary_data) == 0 || nrow(secondary_data) == 0) {
    stop("No data found for one or both methods at sensor position: ", sensor_position)
  }

  # Merge by pulse_id
  merged_data <- merge(
    primary_data[, c("pulse_id", velocity_col)],
    secondary_data[, c("pulse_id", velocity_col)],
    by = "pulse_id",
    suffixes = c("_primary", "_secondary")
  )

  # CRITICAL FIX: Construct column names based on velocity_col
  primary_col <- paste0(velocity_col, "_primary")
  secondary_col <- paste0(velocity_col, "_secondary")

  # Remove NA values
  merged_data <- merged_data[
    !is.na(merged_data[[primary_col]]) &
    !is.na(merged_data[[secondary_col]]),
  ]

  if (nrow(merged_data) < min_points) {
    stop(sprintf(
      "Insufficient data for comparison (n=%d, need %d).",
      nrow(merged_data), min_points
    ))
  }

  # PERFORMANCE OPTIMIZATION: Downsample if dataset is very large
  # Segmented regression is computationally expensive with >50k points
  # Systematic sampling preserves the overall relationship while speeding up fitting
  max_points_for_segmented <- 50000
  if (nrow(merged_data) > max_points_for_segmented) {
    sample_idx <- seq(1, nrow(merged_data), length.out = max_points_for_segmented)
    merged_data_full <- merged_data  # Keep original for reference
    merged_data <- merged_data[sample_idx, ]

    if (verbose) {
      cat("Note: Downsampled from", nrow(merged_data_full), "to",
          nrow(merged_data), "points for faster segmented regression\n")
    }
  }

  if (verbose) {
    cat("Merged data points:", nrow(merged_data), "\n")
    cat("Primary velocity range:", round(min(merged_data[[primary_col]]), 2),
        "to", round(max(merged_data[[primary_col]]), 2), "cm/hr\n")
    cat("Secondary velocity range:", round(min(merged_data[[secondary_col]]), 2),
        "to", round(max(merged_data[[secondary_col]]), 2), "cm/hr\n")
    cat("\n")
  }

  # Fit initial linear model (secondary ~ primary)
  if (verbose) {
    cat("Fitting initial linear model...\n")
  }

  formula_linear <- as.formula(paste(secondary_col, "~", primary_col))
  lm_fit <- lm(formula_linear, data = merged_data)
  initial_r2 <- summary(lm_fit)$r.squared

  if (verbose) {
    cat("Initial linear R²:", round(initial_r2, 4), "\n")
  }

  # Determine initial breakpoint guess
  if (is.null(initial_breakpoint)) {
    # Use midpoint of primary method range as initial guess
    initial_breakpoint <- median(merged_data[[primary_col]], na.rm = TRUE)
  }

  if (verbose) {
    cat("Initial breakpoint guess:", round(initial_breakpoint, 2), "cm/hr\n")
    cat("\n")
    cat("Fitting segmented regression model...\n")
  }

  # CRITICAL FIX: Create formula for segmented regression using dynamic column name
  seg_formula <- as.formula(paste("~", primary_col))

  # Fit segmented regression
  seg_fit <- tryCatch({
    segmented::segmented(
      lm_fit,
      seg.Z = seg_formula,
      psi = initial_breakpoint,
      control = segmented::seg.control(
        display = FALSE,
        it.max = 50,
        n.boot = 0
      )
    )
  }, error = function(e) {
    # If segmented fit fails, try different initial values
    if (verbose) {
      cat("Initial fit failed. Trying alternative starting values...\n")
    }

    # Try quartiles as alternative starting points
    quartiles <- quantile(merged_data[[primary_col]], probs = c(0.25, 0.5, 0.75))

    for (q in quartiles) {
      result <- tryCatch({
        segmented::segmented(
          lm_fit,
          seg.Z = seg_formula,
          psi = q,
          control = segmented::seg.control(
            display = FALSE,
            it.max = 50,
            n.boot = 0
          )
        )
      }, error = function(e2) NULL)

      if (!is.null(result)) {
        return(result)
      }
    }

    # If all attempts fail, return NULL
    return(NULL)
  })

  if (is.null(seg_fit)) {
    warning("Segmented regression failed to converge. This may indicate:\n",
            "  - No clear breakpoint exists (relationship is linear)\n",
            "  - Insufficient data range\n",
            "  - Too much noise in the relationship\n",
            "Returning linear model results with breakpoint = NA")

    result <- list(
      breakpoint = NA_real_,
      breakpoint_ci = c(NA_real_, NA_real_),
      breakpoint_se = NA_real_,
      slope_before = coef(lm_fit)[2],
      slope_after = coef(lm_fit)[2],
      r_squared = initial_r2,
      davies_test = NA_real_,
      n_points = nrow(merged_data),
      segmented_model = NULL,
      linear_model = lm_fit,
      merged_data = merged_data,
      primary_method = primary_method,
      secondary_method = secondary_method,
      sensor_position = sensor_position,
      converged = FALSE
    )

    class(result) <- c("segmented_comparison", "list")
    return(result)
  }

  if (verbose) {
    cat("Segmented regression converged successfully!\n")
    cat("\n")
  }

  # Extract breakpoint information
  breakpoint_summary <- summary(seg_fit)$psi
  breakpoint_est <- breakpoint_summary[, "Est."]
  breakpoint_se <- breakpoint_summary[, "St.Err"]

  # Calculate 95% confidence interval (±1.96 SE)
  breakpoint_ci <- c(
    breakpoint_est - 1.96 * breakpoint_se,
    breakpoint_est + 1.96 * breakpoint_se
  )

  # Extract slopes before and after breakpoint
  slopes <- segmented::slope(seg_fit)
  slope_before <- slopes$Vh_cm_hr_primary[1, 1]  # First segment slope
  slope_after <- slopes$Vh_cm_hr_primary[2, 1]   # Second segment slope

  # Calculate R² for segmented model
  seg_r2 <- 1 - (sum(residuals(seg_fit)^2) / sum((merged_data$Vh_cm_hr_secondary - mean(merged_data$Vh_cm_hr_secondary))^2))

  # Davies test for significant breakpoint
  davies_test <- tryCatch({
    davies_result <- segmented::davies.test(lm_fit, ~ Vh_cm_hr_primary)
    davies_result$p.value
  }, error = function(e) {
    if (verbose) {
      cat("Warning: Davies test failed\n")
    }
    NA_real_
  })

  # Report results
  if (verbose) {
    cat(strrep("-", 72), "\n")
    cat("\n")
    cat("RESULTS\n")
    cat("  Breakpoint:", round(breakpoint_est, 2), "cm/hr\n")
    cat("  95% CI: [", round(breakpoint_ci[1], 2), ",", round(breakpoint_ci[2], 2), "] cm/hr\n")
    cat("  Standard error:", round(breakpoint_se, 2), "cm/hr\n")
    cat("\n")
    cat("  Slope before breakpoint:", round(slope_before, 4), "\n")
    cat("  Slope after breakpoint:", round(slope_after, 4), "\n")
    cat("  Slope change:", round(slope_after - slope_before, 4), "\n")
    cat("\n")
    cat("  Segmented R²:", round(seg_r2, 4), "\n")
    cat("  Linear R² (for comparison):", round(initial_r2, 4), "\n")
    cat("  Improvement:", round(seg_r2 - initial_r2, 4), "\n")
    cat("\n")

    if (!is.na(davies_test)) {
      cat("  Davies test p-value:", format.pval(davies_test, digits = 4), "\n")
      if (davies_test < 0.05) {
        cat("  Breakpoint is STATISTICALLY SIGNIFICANT (p < 0.05)\n")
      } else {
        cat("  Breakpoint is NOT statistically significant (p >= 0.05)\n")
      }
      cat("\n")
    }

    cat("INTERPRETATION\n")
    cat("  Below", round(breakpoint_est, 1), "cm/hr: Both methods maintain linear relationship\n")
    cat("  Above", round(breakpoint_est, 1), "cm/hr: Methods begin to diverge\n")
    cat("  Recommended calibration range: 0 -", round(breakpoint_est, 1), "cm/hr\n")
    cat("\n")
  }

  # Create result object
  result <- list(
    breakpoint = breakpoint_est,
    breakpoint_ci = breakpoint_ci,
    breakpoint_se = breakpoint_se,
    slope_before = slope_before,
    slope_after = slope_after,
    r_squared = seg_r2,
    r_squared_linear = initial_r2,
    davies_test = davies_test,
    n_points = nrow(merged_data),
    segmented_model = seg_fit,
    linear_model = lm_fit,
    merged_data = merged_data,
    primary_method = primary_method,
    secondary_method = secondary_method,
    sensor_position = sensor_position,
    converged = TRUE
  )

  # Create diagnostic plots if requested
  if (create_plots) {
    plots <- create_segmented_regression_plots(
      result = result,
      primary_method = primary_method,
      secondary_method = secondary_method
    )
    result$plots <- plots
  }

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  class(result) <- c("segmented_comparison", "list")
  return(result)
}


#' Create Segmented Regression Diagnostic Plots
#'
#' Creates diagnostic plots showing the segmented regression fit and breakpoint.
#'
#' @param result Segmented comparison result object.
#' @param primary_method Primary method name.
#' @param secondary_method Secondary method name.
#'
#' @return List with \code{segmented_plot} (scatter plot with fitted segments
#'   and breakpoint) and \code{residuals_plot} (residuals for model diagnostics)
#'
#' @keywords internal
create_segmented_regression_plots <- function(result, primary_method, secondary_method) {

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available. Plots not created.")
    return(NULL)
  }

  if (!result$converged) {
    # If model didn't converge, return simple linear plot
    linear_plot <- ggplot2::ggplot(result$merged_data,
                                    ggplot2::aes(x = Vh_cm_hr_primary, y = Vh_cm_hr_secondary)) +
      ggplot2::geom_point(alpha = 0.3, color = "grey40") +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = paste("Method Comparison:", secondary_method, "vs", primary_method),
        subtitle = "Segmented regression did not converge - showing linear fit",
        x = paste(primary_method, "Velocity (cm/hr)"),
        y = paste(secondary_method, "Velocity (cm/hr)")
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11),
        legend.position = "bottom"
      )

    return(list(segmented_plot = linear_plot))
  }

  # Generate fitted values from segmented model
  merged_data <- result$merged_data
  merged_data$fitted <- fitted(result$segmented_model)

  # Create main segmented regression plot
  segmented_plot <- ggplot2::ggplot(merged_data,
                                     ggplot2::aes(x = Vh_cm_hr_primary, y = Vh_cm_hr_secondary)) +
    # Data points
    ggplot2::geom_point(alpha = 0.3, color = "grey40", size = 1.5) +

    # Fitted segmented line
    ggplot2::geom_line(ggplot2::aes(y = fitted), color = "red", linewidth = 1.2) +

    # 1:1 reference line
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "blue", alpha = 0.5) +

    # Breakpoint vertical line
    ggplot2::geom_vline(xintercept = result$breakpoint, color = "darkgreen",
                        linetype = "dashed", linewidth = 1) +

    # Confidence interval shading
    ggplot2::annotate("rect",
                      xmin = result$breakpoint_ci[1],
                      xmax = result$breakpoint_ci[2],
                      ymin = -Inf,
                      ymax = Inf,
                      alpha = 0.1,
                      fill = "green") +

    # Breakpoint annotation
    ggplot2::annotate("text",
                      x = result$breakpoint,
                      y = max(merged_data$Vh_cm_hr_secondary),
                      label = sprintf("Breakpoint: %.1f cm/hr\n95%% CI: [%.1f, %.1f]",
                                     result$breakpoint,
                                     result$breakpoint_ci[1],
                                     result$breakpoint_ci[2]),
                      hjust = -0.05,
                      vjust = 1,
                      color = "darkgreen",
                      size = 3.5) +

    # Labels and theme
    ggplot2::labs(
      title = paste("Segmented Regression:", secondary_method, "vs", primary_method),
      subtitle = sprintf("R² = %.4f | Slopes: %.3f → %.3f | Davies test p = %s",
                        result$r_squared,
                        result$slope_before,
                        result$slope_after,
                        format.pval(result$davies_test, digits = 3)),
      x = paste(primary_method, "Velocity (cm/hr)"),
      y = paste(secondary_method, "Velocity (cm/hr)")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )

  # Create residuals plot with enhanced diagnostics
  merged_data$residuals <- residuals(result$segmented_model)

  # Detect non-linear patterns in residuals after breakpoint
  residuals_after_bp <- merged_data$residuals[merged_data$Vh_cm_hr_primary > result$breakpoint]
  x_after_bp <- merged_data$Vh_cm_hr_primary[merged_data$Vh_cm_hr_primary > result$breakpoint]

  pattern_warning <- NULL
  if (length(residuals_after_bp) > 20) {
    # Fit quadratic to residuals to detect U-shape
    quad_fit <- tryCatch({
      lm(residuals_after_bp ~ x_after_bp + I(x_after_bp^2))
    }, error = function(e) NULL)

    if (!is.null(quad_fit)) {
      quad_coef <- coef(quad_fit)[3]  # Coefficient of x^2 term
      quad_pval <- summary(quad_fit)$coefficients[3, 4]

      # Significant positive quadratic term = U-shape
      # Significant negative quadratic term = inverted U
      if (!is.na(quad_pval) && quad_pval < 0.05) {
        if (quad_coef > 0) {
          pattern_warning <- "U-SHAPED RESIDUALS DETECTED after breakpoint
Second segment may need quadratic term"
        } else {
          pattern_warning <- "INVERTED-U RESIDUALS DETECTED after breakpoint
Second segment may need quadratic term"
        }
      }
    }
  }

  # Residuals plot - NO smoothing (geom_smooth with loess is too slow!)
  residuals_plot <- ggplot2::ggplot(merged_data,
                                     ggplot2::aes(x = Vh_cm_hr_primary, y = residuals)) +
    ggplot2::geom_point(alpha = 0.3, color = "grey40", size = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = result$breakpoint, color = "darkgreen",
                        linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = "Residuals Plot",
      subtitle = if (!is.null(pattern_warning)) pattern_warning else "Check for systematic patterns",
      x = paste(primary_method, "Velocity (cm/hr)"),
      y = "Residuals (cm/hr)"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11,
                                            color = if (!is.null(pattern_warning)) "red" else "grey40"),
      legend.position = "bottom"
    )

  # Store pattern warning in result
  if (!is.null(pattern_warning)) {
    result$residual_pattern_warning <- pattern_warning
  }

  return(list(
    segmented_plot = segmented_plot,
    residuals_plot = residuals_plot
  ))
}


#' Print Method for Segmented Comparison Results
#'
#' @param x A segmented_comparison object
#' @param ... Additional arguments (ignored)
#' @export
print.segmented_comparison <- function(x, ...) {

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("SEGMENTED REGRESSION METHOD COMPARISON\n")
  cat(strrep("=", 72), "\n")
  cat("\n")

  cat("Methods:", x$secondary_method, "vs", x$primary_method, "\n")
  cat("Sensor:", toupper(x$sensor_position), "\n")
  cat("Data points:", x$n_points, "\n")
  cat("\n")

  if (!x$converged) {
    cat("STATUS: Segmented regression did not converge\n")
    cat("No clear breakpoint detected - relationship appears linear\n")
    cat("\n")
    cat("Linear model R²:", round(x$r_squared, 4), "\n")
    cat("\n")
  } else {
    cat("BREAKPOINT ANALYSIS\n")
    cat(strrep("-", 72), "\n")
    cat("  Estimated breakpoint:", round(x$breakpoint, 2), "cm/hr\n")
    cat("  95% Confidence interval: [", round(x$breakpoint_ci[1], 2), ",",
        round(x$breakpoint_ci[2], 2), "] cm/hr\n")
    cat("  Standard error:", round(x$breakpoint_se, 2), "cm/hr\n")
    cat("\n")

    cat("SLOPE ANALYSIS\n")
    cat(strrep("-", 72), "\n")
    cat("  Slope before breakpoint:", round(x$slope_before, 4), "\n")
    cat("  Slope after breakpoint:", round(x$slope_after, 4), "\n")
    cat("  Change in slope:", round(x$slope_after - x$slope_before, 4), "\n")
    cat("\n")

    cat("MODEL FIT\n")
    cat(strrep("-", 72), "\n")
    cat("  Segmented R²:", round(x$r_squared, 4), "\n")
    cat("  Linear R² (comparison):", round(x$r_squared_linear, 4), "\n")
    cat("  Improvement:", round(x$r_squared - x$r_squared_linear, 4), "\n")
    cat("\n")

    if (!is.na(x$davies_test)) {
      cat("STATISTICAL SIGNIFICANCE\n")
      cat(strrep("-", 72), "\n")
      cat("  Davies test p-value:", format.pval(x$davies_test, digits = 4), "\n")
      if (x$davies_test < 0.05) {
        cat("  Result: Breakpoint is SIGNIFICANT (p < 0.05)\n")
      } else {
        cat("  Result: Breakpoint is NOT significant (p >= 0.05)\n")
      }
      cat("\n")
    }

    cat("INTERPRETATION\n")
    cat(strrep("-", 72), "\n")
    cat("  Valid range for", x$primary_method, ": 0 -",
        round(x$breakpoint, 1), "cm/hr\n")
    cat("  Recommended calibration range: 0 -", round(x$breakpoint, 1), "cm/hr\n")
    cat("  Above", round(x$breakpoint, 1), "cm/hr:", x$primary_method,
        "begins to underestimate\n")
    cat("\n")
  }

  if (!is.null(x$plots)) {
    cat("DIAGNOSTIC PLOTS AVAILABLE\n")
    cat(strrep("-", 72), "\n")
    cat("  - $plots$segmented_plot: Main regression with breakpoint\n")
    if (!is.null(x$plots$residuals_plot)) {
      cat("  - $plots$residuals_plot: Residuals diagnostic plot\n")
    }
    cat("\n")
  }

  cat(strrep("=", 72), "\n")
  cat("\n")

  invisible(x)
}
