# ============================================================================
# 04c_calibration_workflows.R
# ============================================================================
# Dual-location calibration and sDMA integration workflows
#
# This module provides wrapper functions for applying method calibration
# and sDMA at two different points in the workflow:
#
# 1. EARLY: After velocity calculation, before corrections (Tim's approach)
# 2. LATE: After corrections, before flux conversion (recommended)
#
# Users can test both approaches to compare calibration coefficients and
# sDMA performance on raw vs corrected velocities.
# ============================================================================


#' Apply Early Calibration and sDMA (Before Corrections)
#'
#' Applies method calibration and sDMA immediately after velocity calculation,
#' before spacing and wound corrections. This is Tim's original approach.
#'
#' The workflow is:
#' 1. Calibrate secondary methods to primary (usually HRM)
#' 2. Apply calibration transformations
#' 3. Apply sDMA to create combined methods
#' 4. User then applies corrections to ALL methods (including sDMA)
#'
#' @param vh_results Raw velocity results from calc_heat_pulse_velocity()
#' @param primary_method Character. Primary method for calibration (usually "HRM").
#'   Default: "HRM".
#' @param secondary_methods Character vector. Secondary methods to calibrate to
#'   primary. Default: c("MHR", "HRMXb").
#' @param sdma_methods Character vector. Subset of secondary_methods to use for
#'   creating sDMA combined methods. Set to NULL to skip sDMA. Default: "MHR".
#' @param sensor_position Character. Sensor position for calibration: "outer",
#'   "inner", or "both". Default: "outer".
#' @param manual_thresholds Named list of manual thresholds for specific methods.
#'   Default: NULL (automatic threshold detection).
#' @param peclet_threshold Numeric. Peclet number threshold for sDMA switching.
#'   Pe < threshold uses HRM, Pe >= threshold uses secondary. Default: 1.0.
#'
#' @return List with components:
#'   \item{calibrations}{Calibration results from calibrate_multiple_methods()}
#'   \item{vh_calibrated}{Velocity data after calibration (no sDMA)}
#'   \item{vh_with_sdma}{Velocity data with calibrated methods + sDMA methods}
#'   \item{timing}{"early"}
#'
#' @details
#' **Advantages of early calibration**:
#' - See method relationships in "raw" uncorrected state
#' - sDMA based on original Peclet numbers
#' - All methods (individual + sDMA) get corrected together
#'
#' **Disadvantages**:
#' - Corrections may change method relationships
#' - sDMA threshold may not be optimal for corrected data
#'
#' **Next steps**: Apply spacing and wound corrections to vh_with_sdma, which
#' will correct ALL methods including sDMA variants.
#'
#' @examples
#' \dontrun{
#' # Calculate velocities
#' vh <- calc_heat_pulse_velocity(hp_data, methods = c("HRM", "MHR", "HRMXb"), wood)
#'
#' # Early calibration + sDMA
#' early <- apply_early_calibration_sdma(
#'   vh,
#'   secondary_methods = c("MHR", "HRMXb"),
#'   sdma_methods = "MHR"
#' )
#'
#' # Methods: HRM, MHR, HRMXb (calibrated), sDMA:MHR
#' unique(early$vh_with_sdma$method)
#'
#' # THEN apply corrections (all methods get corrected)
#' corrected <- apply_spacing_correction_both_sensors(early$vh_with_sdma, ...)
#' }
#'
#' @family calibration workflows
#' @export
apply_early_calibration_sdma <- function(vh_results,
                                          primary_method = "HRM",
                                          secondary_methods = c("MHR", "HRMXb"),
                                          sdma_methods = "MHR",
                                          sensor_position = "outer",
                                          manual_thresholds = NULL,
                                          peclet_threshold = 1.0) {

  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("EARLY CALIBRATION + sDMA (BEFORE CORRECTIONS)\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\nApproach: Tim's method - calibrate before spacing/wound corrections\n")

  # 1. Calibrate secondary methods to primary
  cat("\nStep 1: Calibrating methods...\n")
  calibrations <- calibrate_multiple_methods(
    vh_results,
    primary_method = primary_method,
    secondary_methods = secondary_methods,
    sensor_position = sensor_position,
    manual_thresholds = manual_thresholds
  )

  # 2. Apply calibrations
  cat("\nStep 2: Applying calibrations...\n")
  vh_calibrated <- transform_multiple_methods(vh_results, calibrations)

  # 3. Apply sDMA if requested
  if (!is.null(sdma_methods) && length(sdma_methods) > 0) {
    cat("\nStep 3: Creating sDMA methods...\n")

    vh_with_sdma <- vh_calibrated
    for (sdma_method in sdma_methods) {
      cat(sprintf("  Creating sDMA:%s...\n", sdma_method))

      vh_with_sdma <- apply_sdma_single(
        vh_with_sdma,
        secondary_method = sdma_method,
        velocity_col = "Vh_cm_hr",  # Using RAW velocity column
        peclet_col = "hrm_peclet_number",
        peclet_threshold = peclet_threshold
      )
    }
  } else {
    cat("\nStep 3: Skipping sDMA (sdma_methods = NULL)\n")
    vh_with_sdma <- vh_calibrated
  }

  # Summary
  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("EARLY CALIBRATION COMPLETE\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\nMethods available:\n")
  print(table(vh_with_sdma$method))
  cat("\nNext: Apply spacing/wound corrections to ALL methods\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\n")

  return(list(
    calibrations = calibrations,
    vh_calibrated = vh_calibrated,
    vh_with_sdma = vh_with_sdma,
    timing = "early"
  ))
}


#' Apply Late Calibration and sDMA (After Corrections)
#'
#' Applies method calibration and sDMA after spacing and wound corrections,
#' using corrected velocities. This is the recommended approach for most analyses.
#'
#' The workflow is:
#' 1. User applies corrections to raw velocities (generates Vc_cm_hr)
#' 2. Calibrate secondary methods to primary using CORRECTED velocities
#' 3. Apply calibration transformations
#' 4. Apply sDMA using CORRECTED velocities and Peclet numbers
#'
#' @param vh_corrected Corrected velocity results (after spacing/wound corrections).
#'   Must contain Vc_cm_hr column.
#' @param ... All other parameters same as apply_early_calibration_sdma()
#'
#' @return List with same structure as apply_early_calibration_sdma(), but
#'   timing = "late"
#'
#' @details
#' **Advantages of late calibration**:
#' - Calibration on "clean" corrected velocities
#' - sDMA based on corrected Peclet numbers (may be more accurate)
#' - Method relationships clearer after corrections
#'
#' **Disadvantages**:
#' - Can't see uncorrected method relationships
#' - More workflow steps
#'
#' **Velocity column**: Uses Vc_cm_hr (corrected velocity) instead of Vh_cm_hr.
#'
#' @examples
#' \dontrun{
#' # Calculate velocities
#' vh <- calc_heat_pulse_velocity(hp_data, methods = c("HRM", "MHR", "HRMXb"), wood)
#'
#' # Apply corrections FIRST
#' corrected <- apply_spacing_correction_both_sensors(vh, ...)
#' corrected <- apply_wound_correction(corrected$vh_corrected, ...)
#'
#' # THEN calibrate on corrected data
#' late <- apply_late_calibration_sdma(
#'   corrected,
#'   secondary_methods = c("MHR", "HRMXb"),
#'   sdma_methods = "MHR"
#' )
#'
#' # Methods: HRM, MHR, HRMXb (calibrated), sDMA:MHR (all using Vc_cm_hr)
#' unique(late$vh_with_sdma$method)
#' }
#'
#' @family calibration workflows
#' @export
apply_late_calibration_sdma <- function(vh_corrected,
                                         primary_method = "HRM",
                                         secondary_methods = c("MHR", "HRMXb"),
                                         sdma_methods = "MHR",
                                         sensor_position = "outer",
                                         manual_thresholds = NULL,
                                         peclet_threshold = 1.0) {

  # Determine which velocity column to use
  # Preference: Vc_cm_hr (wound-corrected) > Vh_cm_hr_sc (spacing-corrected) > Vh_cm_hr (raw)
  velocity_col <- if ("Vc_cm_hr" %in% names(vh_corrected)) {
    "Vc_cm_hr"
  } else if ("Vh_cm_hr_sc" %in% names(vh_corrected)) {
    "Vh_cm_hr_sc"
  } else {
    "Vh_cm_hr"
  }

  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("LATE CALIBRATION + sDMA (AFTER CORRECTIONS)\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\nApproach: Recommended - calibrate on corrected velocities\n")
  cat(sprintf("Using velocity column: %s\n", velocity_col))

  # 1. Calibrate on corrected data
  cat("\nStep 1: Calibrating methods on CORRECTED velocities...\n")
  calibrations <- calibrate_multiple_methods(
    vh_corrected,
    primary_method = primary_method,
    secondary_methods = secondary_methods,
    sensor_position = sensor_position,
    manual_thresholds = manual_thresholds,
    velocity_col = velocity_col
  )

  # 2. Apply calibrations
  cat("\nStep 2: Applying calibrations...\n")
  vh_calibrated <- transform_multiple_methods(
    vh_corrected,
    calibrations,
    velocity_col = velocity_col
  )

  # 3. Apply sDMA using CORRECTED velocities
  if (!is.null(sdma_methods) && length(sdma_methods) > 0) {
    cat("\nStep 3: Creating sDMA methods using CORRECTED velocities...\n")

    vh_with_sdma <- vh_calibrated
    for (sdma_method in sdma_methods) {
      cat(sprintf("  Creating sDMA:%s...\n", sdma_method))

      vh_with_sdma <- apply_sdma_single(
        vh_with_sdma,
        secondary_method = sdma_method,
        velocity_col = velocity_col,  # Use detected velocity column
        peclet_col = "hrm_peclet_number",
        peclet_threshold = peclet_threshold
      )
    }
  } else {
    cat("\nStep 3: Skipping sDMA (sdma_methods = NULL)\n")
    vh_with_sdma <- vh_calibrated
  }

  # Summary
  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("LATE CALIBRATION COMPLETE\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\nMethods available:\n")
  print(table(vh_with_sdma$method))
  cat("\nNext: Convert to flux density (apply_flux_conversion)\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\n")

  return(list(
    calibrations = calibrations,
    vh_calibrated = vh_calibrated,
    vh_with_sdma = vh_with_sdma,
    timing = "late"
  ))
}


#' Apply sDMA to Single Secondary Method
#'
#' Internal function to apply sDMA switching for one secondary method.
#' Switches between HRM (Pe < threshold) and secondary method (Pe >= threshold).
#'
#' @param vh_data Velocity data containing HRM and secondary method
#' @param secondary_method Character. Secondary method name (e.g., "MHR")
#' @param velocity_col Character. Column containing velocity values.
#'   "Vh_cm_hr" for raw, "Vc_cm_hr" for corrected. Default: "Vh_cm_hr".
#' @param peclet_col Character. Column containing Peclet numbers.
#'   Default: "hrm_peclet_number".
#' @param peclet_threshold Numeric. Threshold for switching. Default: 1.0.
#'
#' @return vh_data with additional rows for sDMA method
#'
#' @keywords internal
apply_sdma_single <- function(vh_data,
                               secondary_method,
                               velocity_col = "Vh_cm_hr",
                               peclet_col = "hrm_peclet_number",
                               peclet_threshold = 1.0) {

  # Extract HRM and secondary method data
  hrm_data <- vh_data[vh_data$method == "HRM", ]
  sec_data <- vh_data[vh_data$method == secondary_method, ]

  if (nrow(hrm_data) == 0) {
    stop("HRM data not found. sDMA requires HRM results.")
  }

  if (nrow(sec_data) == 0) {
    stop(sprintf("Secondary method '%s' not found in data.", secondary_method))
  }

  # Check for required columns
  if (!velocity_col %in% names(hrm_data)) {
    stop(sprintf("Velocity column '%s' not found in HRM data.", velocity_col))
  }

  if (!peclet_col %in% names(hrm_data)) {
    stop(sprintf("Peclet column '%s' not found in HRM data.", peclet_col))
  }

  # Create sDMA results starting from HRM structure
  sdma_result <- hrm_data
  sdma_result$method <- paste0("sDMA:", secondary_method)
  sdma_result$selected_method <- NA_character_

  # Process each sensor position separately
  for (sens_pos in c("outer", "inner")) {
    hrm_pos <- hrm_data[hrm_data$sensor_position == sens_pos, ]
    sec_pos <- sec_data[sec_data$sensor_position == sens_pos, ]

    if (nrow(hrm_pos) == 0) next

    # Match by datetime
    for (i in 1:nrow(hrm_pos)) {
      dt <- hrm_pos$datetime[i]
      pe <- hrm_pos[[peclet_col]][i]

      # Find matching row in sdma_result
      sdma_idx <- which(sdma_result$datetime == dt &
                        sdma_result$sensor_position == sens_pos)

      if (length(sdma_idx) == 0) next

      if (is.na(pe) || pe < peclet_threshold) {
        # Use HRM
        sdma_result$selected_method[sdma_idx] <- "HRM"
        # Velocity already from HRM
      } else {
        # Use secondary method
        sec_match <- sec_pos[sec_pos$datetime == dt, ]

        if (nrow(sec_match) > 0) {
          sdma_result[[velocity_col]][sdma_idx] <- sec_match[[velocity_col]][1]
          sdma_result$selected_method[sdma_idx] <- secondary_method
        } else {
          # No secondary data, keep HRM
          sdma_result$selected_method[sdma_idx] <- "HRM"
        }
      }
    }
  }

  # Bind with original data
  # Add selected_method column to vh_data if it doesn't exist
  if (!"selected_method" %in% names(vh_data)) {
    vh_data$selected_method <- NA_character_
  }

  result <- rbind(vh_data, sdma_result)

  # Print summary
  cat(sprintf("  sDMA:%s created:\n", secondary_method))
  cat(sprintf("    Switching threshold: Pe = %.2f\n", peclet_threshold))

  selection_counts <- table(sdma_result$selected_method)
  for (sel_method in names(selection_counts)) {
    pct <- 100 * selection_counts[sel_method] / nrow(sdma_result)
    cat(sprintf("    %s: %d measurements (%.1f%%)\n",
                sel_method, selection_counts[sel_method], pct))
  }

  return(result)
}


#' Compare Early vs Late Calibration Results
#'
#' Compares calibration coefficients and sDMA performance between early
#' (before corrections) and late (after corrections) calibration approaches.
#'
#' @param early_result Result from apply_early_calibration_sdma()
#' @param late_result Result from apply_late_calibration_sdma()
#'
#' @return Invisibly returns comparison data frame
#'
#' @examples
#' \dontrun{
#' early <- apply_early_calibration_sdma(vh, ...)
#' # Apply corrections...
#' late <- apply_late_calibration_sdma(vh_corrected, ...)
#'
#' compare_calibration_timing(early, late)
#' }
#'
#' @family calibration workflows
#' @export
compare_calibration_timing <- function(early_result, late_result) {

  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("CALIBRATION TIMING COMPARISON\n")
  cat("=", rep("=", 70), "=\n", sep = "")

  # Compare calibration coefficients
  cat("\nCalibration Coefficients:\n")
  cat("-", rep("-", 70), "-\n", sep = "")

  methods <- names(early_result$calibrations)

  for (method in methods) {
    early_cal <- early_result$calibrations[[method]]$calibration
    late_cal <- late_result$calibrations[[method]]$calibration

    cat(sprintf("\n%s → %s:\n", early_cal$primary_method, method))
    cat(sprintf("  Early (before corrections):\n"))
    cat(sprintf("    Slope: %.4f, Intercept: %.4f, R²: %.4f\n",
                early_cal$slope, early_cal$intercept, early_cal$r_squared))

    cat(sprintf("  Late (after corrections):\n"))
    cat(sprintf("    Slope: %.4f, Intercept: %.4f, R²: %.4f\n",
                late_cal$slope, late_cal$intercept, late_cal$r_squared))

    cat(sprintf("  Difference:\n"))
    cat(sprintf("    Δ Slope: %.4f, Δ Intercept: %.4f, Δ R²: %.4f\n",
                late_cal$slope - early_cal$slope,
                late_cal$intercept - early_cal$intercept,
                late_cal$r_squared - early_cal$r_squared))
  }

  # Compare sDMA method selection if available
  if ("selected_method" %in% names(early_result$vh_with_sdma) &&
      "selected_method" %in% names(late_result$vh_with_sdma)) {

    cat("\n")
    cat("-", rep("-", 70), "-\n", sep = "")
    cat("sDMA Method Selection:\n")
    cat("-", rep("-", 70), "-\n", sep = "")

    # Find sDMA methods
    sdma_methods_early <- grep("^sDMA:", unique(early_result$vh_with_sdma$method), value = TRUE)
    sdma_methods_late <- grep("^sDMA:", unique(late_result$vh_with_sdma$method), value = TRUE)

    common_sdma <- intersect(sdma_methods_early, sdma_methods_late)

    for (sdma_method in common_sdma) {
      early_sdma <- early_result$vh_with_sdma[
        early_result$vh_with_sdma$method == sdma_method,
      ]
      late_sdma <- late_result$vh_with_sdma[
        late_result$vh_with_sdma$method == sdma_method,
      ]

      cat(sprintf("\n%s:\n", sdma_method))
      cat("  Early (before corrections):\n")
      print(table(early_sdma$selected_method))

      cat("  Late (after corrections):\n")
      print(table(late_sdma$selected_method))
    }
  }

  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\n")

  invisible(NULL)
}
