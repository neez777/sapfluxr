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
#' @return A list containing:
#'   \item{optimal_threshold}{Velocity threshold with highest R²}
#'   \item{optimal_r_squared}{R² value at optimal threshold}
#'   \item{optimal_calibration}{Full calibration object at optimal threshold}
#'   \item{threshold_results}{Data frame of all thresholds tested with R², RMSE, n_points}
#'   \item{plots}{List of diagnostic plots (if create_plots = TRUE)}
#'   \item{primary_method}{Primary method used}
#'   \item{secondary_method}{Secondary method calibrated}
#'   \item{sensor_position}{Sensor position analysed}
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
                                                manual_threshold = NULL) {

  # Input validation
  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", "Vh_cm_hr")
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
    primary_data[, c("pulse_id", "Vh_cm_hr")],
    secondary_data[, c("pulse_id", "Vh_cm_hr")],
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
      verbose = verbose
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
    verbose = FALSE
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
#'   Only points where primary method > threshold are used.
#' @param fit_type Type of fit: "linear", "quadratic", or "auto" (default: "auto").
#' @param min_points Minimum points required for calibration (default: 50).
#' @param verbose Logical indicating whether to print summary (default: TRUE).
#'
#' @return A list (class "method_calibration") containing:
#'   \item{coefficients}{Fit coefficients (intercept, slope, quadratic term if applicable)}
#'   \item{fit_type}{Type of fit used ("linear" or "quadratic")}
#'   \item{r_squared}{R-squared of fit}
#'   \item{rmse}{Root mean squared error}
#'   \item{n_points}{Number of points used for calibration}
#'   \item{threshold}{Velocity threshold used}
#'   \item{transformation_function}{Function to transform secondary method values}
#'   \item{primary_method}{Primary method name}
#'   \item{secondary_method}{Secondary method name}
#'   \item{sensor_position}{Sensor position}
#'   \item{calibration_data}{Data frame of calibration points (for plotting)}
#'
#' @details
#' **Calibration Process:**
#'
#' 1. Merges primary and secondary method data by pulse_id
#' 2. Filters to points where primary method <= threshold_velocity (valid region)
#' 3. Fits regression: primary ~ secondary (or primary ~ secondary + secondary²)
#' 4. Creates transformation function to convert secondary values to primary scale
#' 5. This transformation is then applied to ALL secondary data, especially above
#'    the threshold where the primary method becomes unreliable
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
#' # Calibrate MHR to HRM at 10 cm/hr threshold
#' calibration <- calibrate_method_to_primary(
#'   vh_corrected = vh_corrected,
#'   primary_method = "HRM",
#'   secondary_method = "MHR",
#'   sensor_position = "outer",
#'   threshold_velocity = 10
#' )
#'
#' print(calibration$r_squared)
#' print(calibration$coefficients)
#' }
#'
#' @family method calibration functions
#' @export
calibrate_method_to_primary <- function(vh_corrected,
                                         primary_method = "HRM",
                                         secondary_method,
                                         sensor_position = "outer",
                                         threshold_velocity,
                                         fit_type = "auto",
                                         min_points = 50,
                                         verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_corrected)) {
    stop("vh_corrected must be a data frame")
  }

  required_cols <- c("pulse_id", "method", "sensor_position", "Vh_cm_hr")
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
    primary_data[, c("pulse_id", "Vh_cm_hr")],
    secondary_data[, c("pulse_id", "Vh_cm_hr")],
    by = "pulse_id",
    suffixes = c("_primary", "_secondary")
  )

  # Filter to VALID calibration range (data BELOW threshold where both methods agree)
  calib_data <- calib_data[calib_data$Vh_cm_hr_primary <= threshold_velocity, ]

  if (nrow(calib_data) < min_points) {
    stop(sprintf(
      "Insufficient data for calibration (n=%d, need %d).\nTry lowering threshold_velocity.",
      nrow(calib_data), min_points
    ))
  }

  # Determine fit type
  if (fit_type == "auto") {
    # Try both, pick best R²
    linear_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary, data = calib_data)
    quad_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary + I(Vh_cm_hr_secondary^2),
                   data = calib_data)

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
    final_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary, data = calib_data)
    r_squared <- summary(final_fit)$r.squared
  } else if (fit_type == "quadratic") {
    final_fit <- lm(Vh_cm_hr_primary ~ Vh_cm_hr_secondary + I(Vh_cm_hr_secondary^2),
                    data = calib_data)
    r_squared <- summary(final_fit)$r.squared
  } else {
    stop("fit_type must be 'linear', 'quadratic', or 'auto'")
  }

  coeffs <- coef(final_fit)

  # Calculate RMSE
  predictions <- predict(final_fit, calib_data)
  rmse <- sqrt(mean((predictions - calib_data$Vh_cm_hr_primary)^2))

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
    transformation_function = transform_fn,
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
transform_secondary_method <- function(vh_corrected, calibration) {

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
  original_values <- vh_corrected$Vh_cm_hr[secondary_rows]
  transformed_values <- calibration$transformation_function(original_values)

  # Update values
  vh_corrected$Vh_cm_hr[secondary_rows] <- transformed_values

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
                                        manual_thresholds = NULL) {

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
        manual_threshold = manual_thresh
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
                                        verbose = TRUE) {

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
      calibration = calibration_obj
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
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
#' @return List containing:
#'   \item{r_squared_plot}{Plot of R² vs threshold velocity}
#'   \item{calibration_plot}{Scatter plot with line of best fit}
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
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
