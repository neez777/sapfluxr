# R/03k_spacing_correction_unified.R
# Unified Spacing Correction Interface
# Provides a single entry point for applying spacing corrections

#' Apply Spacing Correction (Unified Interface)
#'
#' **Unified interface for applying spacing corrections.** This function focus purely
#' on applying corrections based on pre-identified zero-flow points (changepoints).
#' It separates the model used to connect these points from the math used to
#' calculate the corrected velocity.
#'
#' @param vh_data Data frame containing velocity data with columns:
#'   \code{datetime}, \code{sensor_position}, \code{method}, \code{Vh_cm_hr}
#' @param changepoints Identified zero-flow points.
#'   \itemize{
#'     \item For \code{offset_model = "segment"}: A vector of \code{Date} or character dates.
#'     \item For \code{offset_model = "gradient"}: A data frame with \code{timestamp} and \code{vh_value} columns.
#'   }
#' @param offset_model Character string specifying how to connect zero-flow points.
#'   \itemize{
#'     \item \strong{"segment"} - Constant offset between changepoints (step-wise).
#'     \item \strong{"gradient"} - Linear interpolation between points (continuous).
#'   }
#' @param correction_math Character string specifying the math used for correction.
#'   \itemize{
#'     \item \strong{"burgess"} - Physics-based coefficients (Burgess et al. 2001).
#'     \item \strong{"linear"} - Simple 1:1 offset subtraction.
#'   }
#' @param sensor_position Character, which sensor(s) to correct: \code{"both"}, \code{"outer"}, or \code{"inner"}.
#' @param hpv_method Character, velocity calculation method to correct (default: "HRM").
#' @param wood_properties Wood properties object or list containing thermal diffusivity.
#' @param ... Additional method-specific parameters.
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A data frame containing
#'   the corrected velocity data. Correction metadata is stored as R attributes:
#'   \itemize{
#'     \item \code{Vh_cm_hr_raw} - Original raw values (preserved, locked)
#'     \item \code{Vh_cm_hr_sc} - Spacing-corrected results (NA where not applied)
#'     \item \code{Vs_cm_hr} - Current best estimate (hybrid of raw + corrections)
#'     \item \code{Vh_cm_hr} - Restored to raw after Vs_cm_hr is captured
#'   }
#'
#' @export
apply_spacing_correction <- function(vh_data,
                                      changepoints,
                                      offset_model = c("segment", "gradient"),
                                      correction_math = c("burgess", "linear"),
                                      sensor_position = c("both", "outer", "inner"),
                                      hpv_method = "HRM",
                                      wood_properties = NULL,
                                      ...,
                                      verbose = TRUE) {

  # Match and validate arguments
  offset_model <- match.arg(offset_model)
  correction_math <- match.arg(correction_math)
  sensor_position <- match.arg(sensor_position)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("UNIFIED SPACING CORRECTION\n")
    cat(strrep("=", 72), "\n")
    cat(sprintf("Offset Model:    %s\n", toupper(offset_model)))
    cat(sprintf("Correction Math: %s\n", toupper(correction_math)))
    cat(sprintf("Sensor(s):       %s\n", toupper(sensor_position)))
    cat(sprintf("Method to correct: %s\n", hpv_method))
    cat("\n")
  }

  # Extract additional parameters
  dots <- list(...)

  # Resolve thermal diffusivity (k)
  k_assumed <- if (!is.null(wood_properties)) {
    if ("derived_properties" %in% names(wood_properties)) {
      wood_properties$derived_properties$thermal_diffusivity_actual_cm2_s
    } else if ("thermal_diffusivity" %in% names(wood_properties)) {
      wood_properties$thermal_diffusivity
    } else {
      0.0025
    }
  } else if (!is.null(dots$k_assumed)) {
    dots$k_assumed
  } else {
    0.0025
  }

  # Resolve probe spacing (x)
  probe_spacing <- dots$probe_spacing %||% 0.5

  # Handle multiple sensors if requested
  if (sensor_position == "both") {
    sensors_to_process <- c("outer", "inner")
  } else {
    sensors_to_process <- sensor_position
  }

  # Start with the input data
  vh_corrected <- vh_data
  
  # Ensure Vh_cm_hr_raw exists (preserve original values)
  if (!"Vh_cm_hr_raw" %in% names(vh_corrected)) {
    vh_corrected$Vh_cm_hr_raw <- vh_corrected$Vh_cm_hr
  }

  # Process each sensor
  for (sensor in sensors_to_process) {
    if (verbose && length(sensors_to_process) > 1) {
      cat(sprintf("Processing %s sensor...\n", toupper(sensor)))
    }

    # Identify rows corrected in THIS pass
    # (Used for surgical write-back at the end)
    current_mask <- vh_corrected$sensor_position == sensor & 
                    vh_corrected$method == hpv_method
    current_mask[is.na(current_mask)] <- FALSE
    
    if (!any(current_mask)) next

    # Route to appropriate model
    if (offset_model == "segment") {
      # ===== SEGMENT-BASED (STEP-WISE) MODEL =====
      
      # Use the manual correction function as the engine for segmented logic
      model_result <- apply_manual_spacing_correction(
        vh_data = vh_corrected,
        manual_changepoints = changepoints,
        sensor_position = sensor,
        method = hpv_method,
        vh_col = if ("Vs_cm_hr" %in% names(vh_corrected)) "Vs_cm_hr" else "Vh_cm_hr",
        correction_type = correction_math,
        k_assumed = k_assumed,
        probe_spacing = probe_spacing,
        measurement_time = dots$measurement_time %||% 80,
        create_new_col = TRUE,
        verbose = FALSE
      )
      
      source_col <- paste0(if ("Vs_cm_hr" %in% names(vh_corrected)) "Vs_cm_hr" else "Vh_cm_hr", "_sc")
      # Extract only the corrected rows to ensure lengths match during write-back
      temp_data <- model_result$vh_corrected[current_mask, ]

    } else {
      # ===== GRADIENT (CONTINUOUS) MODEL =====
      
      vh_col_in <- if ("Vs_cm_hr" %in% names(vh_corrected)) "Vs_cm_hr" else "Vh_cm_hr"
      
      lookup <- NULL
      if (correction_math == "burgess") {
        lookup <- calculate_burgess_coefficients(
          k = k_assumed,
          x = probe_spacing,
          t = dots$measurement_time %||% 80
        )
      }

      # Filter to this sensor only for the gradient engine
      sensor_data <- vh_corrected[current_mask, ]

      grad_result <- apply_gradient_offset_correction(
        vh_data = sensor_data,
        changepoints = changepoints,
        vh_col = vh_col_in,
        new_col_suffix = "_gradient_corrected",
        edge_handling = dots$edge_handling %||% "extend",
        correction_type = correction_math,
        lookup_table = lookup
      )
      
      source_col <- paste0(vh_col_in, "_gradient_corrected")
      # grad_result already only contains sensor-specific rows
      temp_data <- grad_result
    }

    # =======================================================================
    # SURGICAL WRITE-BACK (Audit Workflow)
    # =======================================================================
    
    # 1. Update Vh_cm_hr_sc (Audit column: only shows corrected values)
    if (!"Vh_cm_hr_sc" %in% names(vh_corrected)) {
      vh_corrected$Vh_cm_hr_sc <- NA_real_
    }
    vh_corrected$Vh_cm_hr_sc[current_mask] <- temp_data[[source_col]]

    # 2. Update Vs_cm_hr (Current Best Estimate)
    if (!"Vs_cm_hr" %in% names(vh_corrected)) {
      vh_corrected$Vs_cm_hr <- vh_corrected$Vh_cm_hr_raw
    }
    vh_corrected$Vs_cm_hr[current_mask] <- temp_data[[source_col]]
    
    # 3. Transfer any other tracking columns (a, b, baseline)
    cols_to_copy <- c("spacing_correction_a", "spacing_correction_b", "baseline_offset_cm_hr")
    for (col in cols_to_copy) {
      if (col %in% names(temp_data)) {
        if (!col %in% names(vh_corrected)) vh_corrected[[col]] <- NA_real_
        vh_corrected[[col]][current_mask] <- temp_data[[col]]
      }
    }
    
    # Mark as applied
    if (!"spacing_correction_applied" %in% names(vh_corrected)) {
       vh_corrected$spacing_correction_applied <- FALSE
    }
    vh_corrected$spacing_correction_applied[current_mask] <- TRUE
  }

  # Lock Vh_cm_hr back to raw
  vh_corrected$Vh_cm_hr <- vh_corrected$Vh_cm_hr_raw

  # Attach metadata
  attr(vh_corrected, "offset_model")    <- offset_model
  attr(vh_corrected, "correction_math") <- correction_math
  attr(vh_corrected, "changepoints")    <- changepoints
  attr(vh_corrected, "corrections_applied") <- unique(c(attr(vh_data, "corrections_applied"), "spacing"))

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SPACING CORRECTION COMPLETE\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  return(vh_corrected)
}

# Helper function for NULL-coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
