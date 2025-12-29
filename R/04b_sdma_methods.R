# R/04b_sdma_methods.R
# R/04j_sdma_methods.R
#
# PARKED FOR FUTURE IMPLEMENTATION
#
# sDMA (Selectable Dual Method Approach) Method Selection
#
# STATUS: Temporarily extracted from active workflow
# FUTURE POSITION: Between wound correction (Step 4) and flux density (Step 5)
# WILL APPLY TO: Corrected velocities (Vc) after spacing and wound corrections
#
# This code is fully functional and tested. It has been temporarily removed from
# the active workflow to allow implementation of earlier steps (wound correction,
# quality control, etc.) without entanglement.
#
# IMPORTANT: Column Structure Update Needed (2025-12-01)
# This file uses OLD generic column names (peclet_number, calc_window_start_sec, etc.).
# When re-integrating, update to use NEW method-specific columns:
#   - peclet_number → hrm_peclet_number
#   - calc_window_start_sec → method-specific columns (hrm_window_start_sec, etc.)
#   - See 01e_heat_pulse_velocity_core.R for current vh_results schema
#
# When ready to re-integrate:
# 1. Update to use new method-specific column names (CRITICAL)
# 2. Verify it works with corrected velocities (Vc) not just raw (Vh)
# 3. Update documentation examples
# 4. Re-add to NAMESPACE (currently commented out)
# 5. Add UI elements back to Shiny app (currently disabled)
# 6. Test with full workflow
#
# Original extraction date: 2025
# Original location: R/01e_heat_pulse_velocity_core.R (lines 1288-1730)
#                    R/plots.R (plot_sdma_timeseries function)
# ==============================================================================

# NOTE: These functions are currently NOT exported (commented out in NAMESPACE)
# They are preserved here for future use

#' Apply Selectable Dual Method Approach (sDMA) Processing
#'
#' **FUTURE FUNCTIONALITY** - Currently parked for later workflow implementation.
#'
#' Applies method switching based on Peclet number to create sDMA results.
#' Switches between HRM (Pe < 1.0) and a user-specified secondary method (Pe >= 1.0).
#' HRM results must already be calculated with Peclet numbers.
#'
#' @param vh_results Results tibble from calc_heat_pulse_velocity() containing HRM
#'   and at least one secondary method. **FUTURE:** Will accept corrected velocities
#'   (Vc) after spacing and wound corrections.
#' @param secondary_method Character string or vector specifying secondary method(s).
#'   Options: "MHR", "Tmax_Coh", "Tmax_Klu", "HRMXa", "HRMXb".
#'   Can provide multiple methods to create multiple sDMA variants.
#' @param peclet_threshold Numeric threshold for switching between HRM and secondary
#'   method. Default: 1.0 (Pe < 1.0 uses HRM, Pe >= 1.0 uses secondary method).
#' @param skip_low_peclet Logical indicating whether to automatically skip sDMA when
#'   all Peclet numbers are <= threshold. If NULL (default), will prompt user interactively.
#'   Set to TRUE to skip without prompting, FALSE to always calculate.
#' @param show_progress Logical indicating whether to show progress bar. Default: TRUE
#'
#' @details
#' This function requires:
#' \itemize{
#'   \item HRM results with Peclet numbers (from calc_heat_pulse_velocity with methods including "HRM")
#'   \item At least one secondary method already calculated
#' }
#'
#' The switching logic is:
#' \itemize{
#'   \item Pe < 1.0: Use HRM (low flows, HRM is accurate)
#'   \item Pe >= 1.0: Use secondary method (high flows, secondary method more appropriate)
#' }
#'
#' **FUTURE WORKFLOW POSITION:**
#' This will be applied AFTER wound correction and BEFORE flux density calculation:
#' \enumerate{
#'   \item Calculate raw HPV (all methods)
#'   \item Apply spacing correction
#'   \item Apply wound correction → Vc (corrected velocities)
#'   \item **Apply sDMA method selection** ← THIS FUNCTION
#'   \item Calculate flux density
#'   \item Calculate tree water use
#' }
#'
#' @return A vh_results tibble with additional rows for sDMA method(s).
#'   Each sDMA method is labeled as "sDMA:SecondaryMethod" (e.g., "sDMA:MHR").
#'   The selected_method column shows which method was actually used for each measurement.
#'
#' @examples
#' \dontrun{
#' # FUTURE EXAMPLE (not yet implemented in workflow):
#'
#' # Calculate base methods
#' heat_pulse_data <- read_heat_pulse_data("data.txt")
#' vh <- calc_heat_pulse_velocity(heat_pulse_data,
#'                                 methods = c("HRM", "MHR", "Tmax_Klu"))
#'
#' # Apply corrections (spacing + wound)
#' vh_corrected <- apply_spacing_correction(vh, ...)
#' vh_corrected <- apply_wound_correction(vh_corrected, ...)  # NOT YET IMPLEMENTED
#'
#' # THEN apply sDMA to corrected velocities
#' vh_sdma <- apply_sdma_processing(vh_corrected, secondary_method = "MHR")
#'
#' # Continue to flux density calculations
#' flux <- calc_sap_flux_density(vh_sdma, ...)  # NOT YET IMPLEMENTED
#' }
#'
#' @export
apply_sdma_processing <- function(vh_results,
                                  secondary_method,
                                  peclet_threshold = 1.0,
                                  skip_low_peclet = NULL,
                                  show_progress = TRUE) {

  # Validate input
  if (!inherits(vh_results, "vh_results") && !inherits(vh_results, "data.frame")) {
    stop("vh_results must be a results tibble from calc_heat_pulse_velocity()")
  }

  # Check that HRM exists
  if (!"HRM" %in% unique(vh_results$method)) {
    stop("HRM results not found in vh_results.\n",
         "  sDMA requires HRM to be calculated first.\n",
         "  Use: calc_heat_pulse_velocity(..., methods = c(\"HRM\", ...)")
  }

  # Check that HRM has Peclet numbers
  hrm_data <- vh_results[vh_results$method == "HRM", ]
  if (all(is.na(hrm_data$hrm_peclet_number))) {
    stop("HRM results do not contain Peclet numbers.\n",
         "  This may be from an older version. Please recalculate HRM results.")
  }

  # Check Peclet number range to determine if sDMA is necessary
  max_peclet <- max(hrm_data$hrm_peclet_number, na.rm = TRUE)

  if (!is.na(max_peclet) && max_peclet <= peclet_threshold) {
    # All Peclet numbers are <= threshold, so sDMA would never switch to secondary method
    message("\n", strrep("=", 67))
    message("  sDMA PECLET NUMBER CHECK")
    message(strrep("=", 67))
    message(sprintf("\nMaximum Peclet number: %.3f", max_peclet))
    message(sprintf("\nAll Peclet numbers are <= %.2f, which means:", peclet_threshold))
    message("  • HRM is valid for all measurements (low flow conditions)")
    message("  • sDMA would never switch to the secondary method")
    message("  • The sDMA results would be identical to HRM results")
    message("\nCalculating sDMA is unnecessary in this case.")

    # Determine whether to skip based on parameter or user input
    should_skip <- if (!is.null(skip_low_peclet)) {
      skip_low_peclet
    } else if (interactive()) {
      # Interactive prompt
      cat("\nDo you want to skip sDMA calculation? (yes/no): ")
      response <- tolower(trimws(readline()))
      response %in% c("y", "yes")
    } else {
      # Non-interactive default: skip
      message("\nSkipping sDMA calculation (non-interactive mode).")
      message("Set skip_low_peclet = FALSE to force calculation.\n")
      TRUE
    }

    if (should_skip) {
      message("\nsDMA calculation skipped. Returning original results.\n")
      message(strrep("=", 67), "\n")
      return(vh_results)
    } else {
      message("\nProceeding with sDMA calculation as requested.\n")
      message(strrep("=", 67), "\n")
    }
  }

  # Validate secondary methods exist
  available_methods <- unique(vh_results$method)
  valid_secondary <- c("MHR", "Tmax_Coh", "Tmax_Klu", "HRMXa", "HRMXb")

  for (sec_method in secondary_method) {
    if (sec_method == "HRM") {
      stop("Cannot use HRM as secondary method in sDMA.\n",
           "  HRM is always the primary method (used when Pe < 1.0).")
    }

    if (!sec_method %in% valid_secondary) {
      stop(sprintf("Invalid secondary method: '%s'\n  Valid options: %s",
                   sec_method, paste(valid_secondary, collapse = ", ")))
    }

    if (!sec_method %in% available_methods) {
      stop(sprintf("Secondary method '%s' not found in vh_results.\n", sec_method),
           "  Available methods: ", paste(available_methods, collapse = ", "), "\n",
           "  Calculate it first with calc_heat_pulse_velocity()")
    }
  }

  # Get unique pulse IDs
  pulse_ids <- unique(vh_results$pulse_id)
  n_pulses <- length(pulse_ids)
  n_methods <- length(secondary_method)

  # OPTIMISATION: Pre-split results by pulse_id and method once (massive speedup!)
  # This avoids scanning entire dataset for each pulse
  results_by_pulse <- split(vh_results, list(vh_results$pulse_id, vh_results$method))

  # Check if we're in a Shiny session (where caller manages progress wrapping)
  in_shiny <- tryCatch({
    !is.null(shiny::getDefaultReactiveDomain())
  }, error = function(e) FALSE)

  # If not in Shiny and progress is enabled, set up for R console
  if (show_progress && !in_shiny) {
    # R console: set up text progress bar and wrap in with_progress
    progressr::handlers("txtprogressbar")

    return(progressr::with_progress({
      apply_sdma_processing_internal(
        vh_results, results_by_pulse, pulse_ids, n_pulses, n_methods,
        secondary_method, peclet_threshold, show_progress
      )
    }))
  }

  # In Shiny or progress disabled - just run directly
  return(apply_sdma_processing_internal(
    vh_results, results_by_pulse, pulse_ids, n_pulses, n_methods,
    secondary_method, peclet_threshold, show_progress
  ))
}


#' Internal sDMA processing function (called with progress context already set up)
#' @keywords internal
apply_sdma_processing_internal <- function(vh_results, results_by_pulse, pulse_ids,
                                           n_pulses, n_methods, secondary_method,
                                           peclet_threshold, show_progress) {

  # Progress reporting setup
  if (show_progress) {
    p <- progressr::progressor(steps = n_pulses * n_methods)
  }

  # Throttle progress updates
  update_frequency <- 100
  methods_completed <- 0
  methods_since_last_update <- 0

  # Process each secondary method
  all_sdma_results <- list()

  for (sec_method in secondary_method) {
    sdma_method_name <- paste0("sDMA:", sec_method)

    # VECTORIZED APPROACH: Extract all data at once, then apply switching logic
    # This is 100-1000x faster than creating data.frames in a loop

    # Get all HRM and secondary method results
    hrm_all <- vh_results[vh_results$method == "HRM", ]
    sec_all <- vh_results[vh_results$method == sec_method, ]

    # Separate by sensor position
    hrm_outer <- hrm_all[hrm_all$sensor_position == "outer", ]
    hrm_inner <- hrm_all[hrm_all$sensor_position == "inner", ]
    sec_outer <- sec_all[sec_all$sensor_position == "outer", ]
    sec_inner <- sec_all[sec_all$sensor_position == "inner", ]

    # Ensure matching order by pulse_id and same pulse_ids in both
    hrm_outer <- hrm_outer[order(hrm_outer$pulse_id), ]
    hrm_inner <- hrm_inner[order(hrm_inner$pulse_id), ]
    sec_outer <- sec_outer[order(sec_outer$pulse_id), ]
    sec_inner <- sec_inner[order(sec_inner$pulse_id), ]

    # Keep only pulses that exist in both HRM and secondary method
    common_pulses_outer <- intersect(hrm_outer$pulse_id, sec_outer$pulse_id)
    common_pulses_inner <- intersect(hrm_inner$pulse_id, sec_inner$pulse_id)

    hrm_outer <- hrm_outer[hrm_outer$pulse_id %in% common_pulses_outer, ]
    hrm_inner <- hrm_inner[hrm_inner$pulse_id %in% common_pulses_inner, ]
    sec_outer <- sec_outer[sec_outer$pulse_id %in% common_pulses_outer, ]
    sec_inner <- sec_inner[sec_inner$pulse_id %in% common_pulses_inner, ]

    # Check if we have any data to process
    if (nrow(hrm_outer) == 0 && nrow(hrm_inner) == 0) {
      warning(sprintf("No matching pulses found between HRM and %s for any sensor position. Skipping %s.",
                     sec_method, sdma_method_name))
      next  # Skip to next secondary method
    }

    # Process each sensor separately (only if it has data)
    sdma_parts <- list()

    # Process outer sensor if it has data
    if (nrow(hrm_outer) > 0) {
      # Detect Peclet column name
      peclet_col <- if ("hrm_peclet_number" %in% names(hrm_outer)) "hrm_peclet_number" else "peclet_number"
      use_hrm_outer <- !is.na(hrm_outer[[peclet_col]]) & hrm_outer[[peclet_col]] < peclet_threshold

      # Build data frame
      sdma_outer <- data.frame(
        datetime = hrm_outer$datetime,
        pulse_id = hrm_outer$pulse_id,
        method = sdma_method_name,
        sensor_position = "outer",
        Vh_cm_hr = ifelse(use_hrm_outer, hrm_outer$Vh_cm_hr, sec_outer$Vh_cm_hr),
        stringsAsFactors = FALSE
      )

      # Add optional columns if they exist
      if ("temp_ratio" %in% names(hrm_outer)) {
        sdma_outer$temp_ratio <- ifelse(use_hrm_outer, hrm_outer$temp_ratio, sec_outer$temp_ratio)
      }
      if (peclet_col %in% names(hrm_outer)) {
        sdma_outer[[peclet_col]] <- hrm_outer[[peclet_col]]
      }

      # Add required columns
      sdma_outer$selected_method <- ifelse(use_hrm_outer, "HRM", sec_method)
      sdma_outer$Vh_sdma <- sdma_outer$Vh_cm_hr

      sdma_parts[["outer"]] <- sdma_outer
    }

    # Process inner sensor if it has data
    if (nrow(hrm_inner) > 0) {
      # Detect Peclet column name
      peclet_col <- if ("hrm_peclet_number" %in% names(hrm_inner)) "hrm_peclet_number" else "peclet_number"
      use_hrm_inner <- !is.na(hrm_inner[[peclet_col]]) & hrm_inner[[peclet_col]] < peclet_threshold

      # Build data frame
      sdma_inner <- data.frame(
        datetime = hrm_inner$datetime,
        pulse_id = hrm_inner$pulse_id,
        method = sdma_method_name,
        sensor_position = "inner",
        Vh_cm_hr = ifelse(use_hrm_inner, hrm_inner$Vh_cm_hr, sec_inner$Vh_cm_hr),
        stringsAsFactors = FALSE
      )

      # Add optional columns if they exist
      if ("temp_ratio" %in% names(hrm_inner)) {
        sdma_inner$temp_ratio <- ifelse(use_hrm_inner, hrm_inner$temp_ratio, sec_inner$temp_ratio)
      }
      if (peclet_col %in% names(hrm_inner)) {
        sdma_inner[[peclet_col]] <- hrm_inner[[peclet_col]]
      }

      # Add required columns
      sdma_inner$selected_method <- ifelse(use_hrm_inner, "HRM", sec_method)
      sdma_inner$Vh_sdma <- sdma_inner$Vh_cm_hr

      sdma_parts[["inner"]] <- sdma_inner
    }

    # Combine available sensors
    sdma_df <- dplyr::bind_rows(sdma_parts)
    all_sdma_results[[sdma_method_name]] <- sdma_df

    # Update progress
    methods_completed <- methods_completed + n_pulses
    if (show_progress) {
      p(amount = n_pulses,
        message = sprintf("sDMA: Completed %s (%.1f%% complete)",
                         sdma_method_name,
                         100 * methods_completed / (n_pulses * n_methods)))
    }
  }

  # Combine all sDMA results
  sdma_combined <- dplyr::bind_rows(all_sdma_results)

  # Add quality flags to sDMA results
  sdma_combined <- add_quality_flags(sdma_combined)

  # Combine with original results
  result <- dplyr::bind_rows(vh_results, sdma_combined)

  # Preserve class
  class(result) <- class(vh_results)

  return(result)
}

#' Calculate DMA velocities
#'
#' **NOTE:** This appears to be an older DMA variant. May not be needed for sDMA.
#' Kept for reference but likely can be removed.
#'
#' @keywords internal
calc_dma <- function(hrm_results, tmax_klu_results, diffusivity, probe_spacing) {
  Vh_HRM_crit <- diffusivity / probe_spacing * 3600

  # Handle outer sensor with proper NA checking
  if (is.na(hrm_results$outer) || !is.finite(hrm_results$outer)) {
    Vho_DMA <- tmax_klu_results$outer
    use_hrm_outer <- FALSE
  } else if (hrm_results$outer < Vh_HRM_crit) {
    Vho_DMA <- hrm_results$outer
    use_hrm_outer <- TRUE
  } else {
    Vho_DMA <- tmax_klu_results$outer
    use_hrm_outer <- FALSE
  }

  # Handle inner sensor with proper NA checking
  if (is.na(hrm_results$inner) || !is.finite(hrm_results$inner)) {
    Vhi_DMA <- tmax_klu_results$inner
    use_hrm_inner <- FALSE
  } else if (hrm_results$inner < Vh_HRM_crit) {
    Vhi_DMA <- hrm_results$inner
    use_hrm_inner <- TRUE
  } else {
    Vhi_DMA <- tmax_klu_results$inner
    use_hrm_inner <- FALSE
  }

  # Pass through metadata from the selected method
  return(list(
    outer = Vho_DMA,
    inner = Vhi_DMA,
    window_start_outer = if (use_hrm_outer) hrm_results$window_start_outer else tmax_klu_results$window_start_outer,
    window_end_outer = if (use_hrm_outer) hrm_results$window_end_outer else tmax_klu_results$window_end_outer,
    window_start_inner = if (use_hrm_inner) hrm_results$window_start_inner else tmax_klu_results$window_start_inner,
    window_end_inner = if (use_hrm_inner) hrm_results$window_end_inner else tmax_klu_results$window_end_inner,
    calc_time_outer = if (use_hrm_outer) hrm_results$calc_time_outer else tmax_klu_results$calc_time_outer,
    calc_time_inner = if (use_hrm_inner) hrm_results$calc_time_inner else tmax_klu_results$calc_time_inner
  ))
}

#' Parse sDMA Method String
#'
#' Parses a Selectable DMA method string (e.g., "sDMA:MHR") to extract the secondary method.
#' Validates that the secondary method is valid and not HRM.
#'
#' @param method_string Character string of method name
#' @return List with is_sdma (logical) and secondary_method (character or NULL)
#' @keywords internal
parse_sdma_method <- function(method_string) {
  # Check if this is an sDMA method
  if (!grepl("^sDMA:", method_string)) {
    return(list(is_sdma = FALSE, secondary_method = NULL))
  }

  # Extract secondary method
  secondary <- sub("^sDMA:", "", method_string)

  # Validate: cannot use HRM as secondary
  if (secondary == "HRM") {
    stop("sDMA cannot use HRM as secondary method. HRM is always the primary method in sDMA.\n",
         "  Use one of: MHR, Tmax_Coh, Tmax_Klu, HRMXa, HRMXb")
  }

  # Validate: must be a recognised method
  valid_secondary <- c("MHR", "Tmax_Coh", "Tmax_Klu", "HRMXa", "HRMXb")

  if (!secondary %in% valid_secondary) {
    stop(sprintf("Invalid sDMA secondary method: '%s'\n  Valid options: %s",
                 secondary, paste(valid_secondary, collapse = ", ")))
  }

  return(list(is_sdma = TRUE, secondary_method = secondary))
}

#' Calculate Selectable DMA velocities
#'
#' Calculates velocity using Selectable Dual Method Approach (sDMA), which switches
#' between HRM and a user-specified secondary method based on Peclet number.
#' Uses HRM when Pe < 1.0, otherwise uses the secondary method.
#'
#' @param hrm_results Results from calc_hrm()
#' @param secondary_results Results from secondary method (e.g., calc_mhr())
#' @param secondary_method_name Name of secondary method (e.g., "MHR")
#' @param diffusivity Thermal diffusivity (cm²/s)
#' @param probe_spacing Probe spacing (cm)
#' @return List with velocity results, Peclet numbers, and selected methods
#' @keywords internal
calc_sdma <- function(hrm_results,
                      secondary_results,
                      secondary_method_name,
                      diffusivity,
                      probe_spacing) {

  # Calculate Peclet number (dimensionless)
  # Pe = (Vh × x) / (D × 3600)
  # where Vh is in cm/hr, x in cm, D in cm²/s
  # The 3600 converts D from cm²/s to cm²/hr to match Vh units
  Pe_outer <- if (!is.na(hrm_results$outer) && is.finite(hrm_results$outer)) {
    (hrm_results$outer * probe_spacing) / (diffusivity * 3600)
  } else {
    NA_real_
  }

  Pe_inner <- if (!is.na(hrm_results$inner) && is.finite(hrm_results$inner)) {
    (hrm_results$inner * probe_spacing) / (diffusivity * 3600)
  } else {
    NA_real_
  }

  # Switching logic: Pe < 1.0 → HRM; Pe >= 1.0 → secondary method
  # Outer sensor
  if (is.na(hrm_results$outer) || !is.finite(hrm_results$outer)) {
    Vho_sDMA <- secondary_results$outer
    use_hrm_outer <- FALSE
  } else if (Pe_outer < 1.0) {
    Vho_sDMA <- hrm_results$outer
    use_hrm_outer <- TRUE
  } else {
    Vho_sDMA <- secondary_results$outer
    use_hrm_outer <- FALSE
  }

  # Inner sensor
  if (is.na(hrm_results$inner) || !is.finite(hrm_results$inner)) {
    Vhi_sDMA <- secondary_results$inner
    use_hrm_inner <- FALSE
  } else if (Pe_inner < 1.0) {
    Vhi_sDMA <- hrm_results$inner
    use_hrm_inner <- TRUE
  } else {
    Vhi_sDMA <- secondary_results$inner
    use_hrm_inner <- FALSE
  }

  # Return results with Peclet numbers and selected methods
  return(list(
    outer = Vho_sDMA,
    inner = Vhi_sDMA,
    peclet_outer = Pe_outer,
    peclet_inner = Pe_inner,
    selected_method_outer = if (use_hrm_outer) "HRM" else secondary_method_name,
    selected_method_inner = if (use_hrm_inner) "HRM" else secondary_method_name,
    temp_ratio_outer = if (use_hrm_outer) hrm_results$temp_ratio_outer else secondary_results$temp_ratio_outer,
    temp_ratio_inner = if (use_hrm_inner) hrm_results$temp_ratio_inner else secondary_results$temp_ratio_inner,
    window_start_outer = if (use_hrm_outer) hrm_results$window_start_outer else secondary_results$window_start_outer,
    window_end_outer = if (use_hrm_outer) hrm_results$window_end_outer else secondary_results$window_end_outer,
    window_start_inner = if (use_hrm_inner) hrm_results$window_start_inner else secondary_results$window_start_inner,
    window_end_inner = if (use_hrm_inner) hrm_results$window_end_inner else secondary_results$window_end_inner,
    calc_time_outer = if (use_hrm_outer) hrm_results$calc_time_outer else secondary_results$calc_time_outer,
    calc_time_inner = if (use_hrm_inner) hrm_results$calc_time_inner else secondary_results$calc_time_inner
  ))
}

# ==============================================================================
# PLOTTING FUNCTIONS FOR sDMA
# Extracted from R/plots.R
# ==============================================================================

#' Plot sDMA Time Series
#'
#' **FUTURE FUNCTIONALITY** - Currently parked.
#'
#' Creates an interactive time series plot showing sDMA results with method switching.
#' Shows which method (HRM vs. secondary) was selected based on Peclet number.
#'
#' @param vh_results Results from apply_sdma_processing()
#' @param sdma_method Which sDMA method to plot (e.g., "sDMA:MHR")
#' @param start_date Start date for plot (POSIXct or "YYYY-MM-DD" string)
#' @param end_date End date for plot
#' @param sensor_position Which sensor to plot ("outer" or "inner")
#' @param quality_filter Whether to filter out poor quality data
#' @param show_peclet Whether to show Peclet number on secondary axis
#'
#' @keywords internal
plot_sdma_timeseries <- function(vh_results,
                                 sdma_method = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 sensor_position = "outer",
                                 quality_filter = TRUE,
                                 show_peclet = TRUE) {

  # Function is complete but needs updating for future workflow
  # Original implementation from R/plots.R lines 936-1150
  # TODO: Test with corrected velocities when re-integrated

  stop("plot_sdma_timeseries() is currently disabled.\n",
       "  This function will be re-enabled when sDMA is integrated into the workflow.\n",
       "  Expected position: After wound correction, before flux density calculation.")
}

# ==============================================================================
# END OF PARKED sDMA CODE
# ==============================================================================
