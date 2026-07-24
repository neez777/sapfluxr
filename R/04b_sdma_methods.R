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
#   - peclet_number (canonical column name; hrm_peclet_number alias removed)
#   - calc_window_start_sec -> method-specific columns (hrm_window_start_sec, etc.)
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
#' Applies Peclet-number-based method switching to create sDMA results.
#' Switches between HRM (Pe < threshold) and a user-specified secondary method (Pe >= threshold).
#'
#' The Peclet number is computed internally by this function using
#' [recalculate_peclet()]. If a `peclet_number` column already exists and contains
#' non-NA values on the HRM rows it is used as-is; otherwise Pe is computed from
#' the corrected velocities via `probe_config` and `wood_properties`.
#' Pe is computed for **all rows regardless of quality flag**, so interpolated
#' measurements still contribute to the switching decision.
#'
#' @param vh_results Results tibble from `calc_heat_pulse_velocity()` (or the
#'   corrected output after spacing/wound corrections) containing HRM and at least
#'   one secondary method.
#' @param secondary_method Character string or vector specifying secondary method(s).
#'   Options: `"MHR"`, `"Tmax_Coh"`, `"Tmax_Klu"`.
#'   Supply multiple methods to create multiple sDMA variants simultaneously.
#' @param peclet_threshold Numeric threshold for switching. Default: `1.0`
#'   (Pe < 1.0 -> HRM; Pe >= 1.0 -> secondary method).
#' @param probe_config A `ProbeConfiguration` object (from [load_probe_config()]) or
#'   named list containing `probe_spacing`. Required when `peclet_number` is absent
#'   from `vh_results`. The same object passed to `calc_heat_pulse_velocity()` is
#'   appropriate here.
#' @param wood_properties A `WoodProperties` object (from [load_wood_properties()]) or
#'   named list containing `thermal_diffusivity`. Required when `peclet_number` is
#'   absent from `vh_results`.
#' @param skip_low_peclet Logical. Whether to automatically skip sDMA when all Peclet
#'   numbers are <= threshold. `NULL` (default) prompts interactively; `TRUE` skips
#'   without prompting; `FALSE` always calculates.
#' @param show_progress Logical. Show progress bar. Default: `TRUE`.
#'
#' @details
#' **Workflow position:** apply after wound correction, before flux density:
#' \enumerate{
#'   \item Calculate raw HPV (all methods)
#'   \item Apply spacing correction
#'   \item Apply wound correction
#'   \item **Apply sDMA** \eqn{\leftarrow} this function
#'   \item Calculate flux density
#' }
#'
#' **Switching logic:**
#' \itemize{
#'   \item Pe < threshold: use HRM (low sap flow, HRM is accurate)
#'   \item Pe >= threshold: use secondary method (high sap flow)
#' }
#'
#' @return A vh_results tibble with additional rows for each sDMA method, labelled
#'   `"sDMA:SecondaryMethod"` (e.g., `"sDMA:MHR"`). The `selected_method` column
#'   records which method was used for each measurement.
#'
#' @examples
#' \dontrun{
#' probe <- load_probe_config("symmetrical")
#' wood  <- load_wood_properties("eucalyptus")
#'
#' vh <- calc_heat_pulse_velocity(heat_pulse_data, methods = c("HRM", "MHR"),
#'                                probe_config = probe, wood_properties = wood)
#' vh <- apply_spacing_correction(vh, ...)
#' vh <- apply_wound_correction(vh, ...)
#'
#' # Pe is computed automatically from probe_config / wood_properties
#' vh_sdma <- apply_sdma_processing(vh, secondary_method = "MHR",
#'                                  probe_config = probe, wood_properties = wood)
#'
#' # Or run recalculate_peclet() first and let apply_sdma_processing use the column
#' vh <- recalculate_peclet(vh, probe, wood)
#' vh_sdma <- apply_sdma_processing(vh, "MHR")
#' }
#'
#' @export
apply_sdma_processing <- function(vh_results,
                                  secondary_method,
                                  peclet_threshold = 1.0,
                                  probe_config     = NULL,
                                  wood_properties  = NULL,
                                  skip_low_peclet  = NULL,
                                  show_progress    = TRUE) {

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

  # Ensure HRM data exists
  hrm_data <- vh_results[vh_results$method == "HRM", ]
  if (nrow(hrm_data) == 0) {
    stop("HRM results not found. vh_results must contain HRM data for sDMA processing.")
  }

  peclet_col <- "peclet_number"

  # Auto-compute Peclet numbers if the column is absent or all-NA on HRM rows.
  # recalculate_peclet() uses all rows regardless of quality_flag, so interpolated
  # measurements still contribute to the switching decision.
  peclet_present <- peclet_col %in% names(hrm_data) && !all(is.na(hrm_data[[peclet_col]]))

  if (!peclet_present) {
    if (is.null(probe_config) || is.null(wood_properties)) {
      stop(
        "The 'peclet_number' column is absent (or all-NA) in vh_results.\n",
        "  Supply probe_config and wood_properties so apply_sdma_processing() can\n",
        "  compute the Peclet number automatically, or call recalculate_peclet()\n",
        "  on vh_results before calling this function."
      )
    }
    message("apply_sdma_processing: 'peclet_number' not found -- computing via recalculate_peclet().")
    vh_results <- recalculate_peclet(
      vh_results,
      probe_config    = probe_config,
      wood_properties = wood_properties,
      peclet_col      = peclet_col
    )
    hrm_data <- vh_results[vh_results$method == "HRM", ]
  }

  # Validate secondary_method
  if ("HRM" %in% secondary_method) {
    stop("Cannot use HRM as secondary method. HRM is always the primary method in sDMA.")
  }

  missing_methods <- setdiff(secondary_method, unique(vh_results$method))
  if (length(missing_methods) > 0) {
    stop("Secondary method(s) not found in vh_results: ", paste(missing_methods, collapse = ", "),
         "\n  Ensure all requested methods are calculated first.")
  }

  # Check Peclet number range to determine if sDMA is necessary
  max_peclet <- max(hrm_data[[peclet_col]], na.rm = TRUE)

  if (!is.na(max_peclet) && max_peclet <= peclet_threshold) {
    # All Peclet numbers are <= threshold, so sDMA would never switch to secondary method
    message("\n", strrep("=", 67))
    message("  sDMA PECLET NUMBER CHECK")
    message(strrep("=", 67))
    message(sprintf("\nMaximum Peclet number: %.3f", max_peclet))
    message(sprintf("\nAll Peclet numbers are <= %.2f, which means:", peclet_threshold))
    message("  - HRM is valid for all measurements (low flow conditions)")
    message("  - sDMA would never switch to the secondary method")
    message("  - The sDMA results would be identical to HRM results")
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
  valid_secondary <- c("MHR", "Tmax_Coh", "Tmax_Klu")

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
      peclet_col <- "peclet_number"
      use_hrm_outer <- !is.na(hrm_outer[[peclet_col]]) & hrm_outer[[peclet_col]] < peclet_threshold

      # Use best estimate column if available, else fall back to Vh_cm_hr
      vh_col <- if ("Vs_cm_hr" %in% names(hrm_outer)) "Vs_cm_hr" else "Vh_cm_hr"

      # Build data frame
      sdma_outer <- data.frame(
        datetime = hrm_outer$datetime,
        pulse_id = hrm_outer$pulse_id,
        method = sdma_method_name,
        sensor_position = "outer",
        Vh_cm_hr = ifelse(use_hrm_outer, hrm_outer[[vh_col]], sec_outer[[vh_col]]),
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
      sdma_outer$Vs_cm_hr <- sdma_outer$Vh_cm_hr
      sdma_outer$peclet_number <- hrm_outer[[peclet_col]] # Always include for switching audit

      sdma_parts[["outer"]] <- sdma_outer
    }

    # Process inner sensor if it has data
    if (nrow(hrm_inner) > 0) {
      peclet_col <- "peclet_number"
      use_hrm_inner <- !is.na(hrm_inner[[peclet_col]]) & hrm_inner[[peclet_col]] < peclet_threshold

      # Use best estimate column
      vh_col <- if ("Vs_cm_hr" %in% names(hrm_inner)) "Vs_cm_hr" else "Vh_cm_hr"

      # Build data frame
      sdma_inner <- data.frame(
        datetime = hrm_inner$datetime,
        pulse_id = hrm_inner$pulse_id,
        method = sdma_method_name,
        sensor_position = "inner",
        Vh_cm_hr = ifelse(use_hrm_inner, hrm_inner[[vh_col]], sec_inner[[vh_col]]),
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
      sdma_inner$Vs_cm_hr <- sdma_inner$Vh_cm_hr
      sdma_inner$peclet_number <- hrm_inner[[peclet_col]] # Always include for switching audit

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
         "  Use one of: MHR, Tmax_Coh, Tmax_Klu")
  }

  # Validate: must be a recognised method
  valid_secondary <- c("MHR", "Tmax_Coh", "Tmax_Klu")

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
#' @param diffusivity Thermal diffusivity (cm^2/s)
#' @param probe_spacing Probe spacing (cm)
#' @return List with velocity results, Peclet numbers, and selected methods
#' @keywords internal
calc_sdma <- function(hrm_results,
                      secondary_results,
                      secondary_method_name,
                      diffusivity,
                      probe_spacing) {

  # Calculate Peclet number (dimensionless)
  # Pe = (Vh * x) / (D * 3600)
  # where Vh is in cm/hr, x in cm, D in cm^2/s
  # The 3600 converts D from cm^2/s to cm^2/hr to match Vh units
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

  # Switching logic: Pe < 1.0 -> HRM; Pe >= 1.0 -> secondary method
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


# ==============================================================================
# PECLET RECALCULATION
# ==============================================================================

#' Recalculate Peclet Numbers Based on Corrected Velocity
#'
#' Recalculates Peclet numbers after spacing or wound corrections have been
#' applied. This is essential before sDMA switching because corrections change
#' velocity values, invalidating the original Peclet numbers from
#' \code{calc_heat_pulse_velocity()}.
#'
#' @param vh_results Data frame with velocity results (output of
#'   \code{calc_heat_pulse_velocity()} or any downstream correction function).
#' @param probe_config Probe configuration object (\code{ProbeConfiguration}) or
#'   path to a YAML file.
#' @param wood_properties Wood properties object (\code{WoodProperties}) or path
#'   to a YAML file.
#' @param velocity_col Column name containing velocity values (cm/hr) to use for
#'   Peclet calculation. If \code{NULL} (default), auto-detects in priority order:
#'   \code{Vs_cm_hr} (current best estimate), \code{Vh_cm_hr_wc} (wound-corrected),
#'   \code{Vh_cm_hr_sc} (spacing-corrected), \code{Vh_cm_hr} (raw).
#' @param peclet_col Name for the output Peclet column. Default:
#'   \code{"Pe_corrected"}.
#'
#' @return The input data frame with an additional column containing the
#'   recalculated Peclet numbers.
#'
#' @details
#' The Peclet number is calculated as:
#' \deqn{Pe = \frac{V_h \times x}{D \times 3600}}
#' where \eqn{V_h} is sap velocity (cm/hr), \eqn{x} is probe spacing (cm),
#' \eqn{D} is thermal diffusivity (cm\eqn{^2}/s), and 3600 converts hours to
#' seconds.
#'
#' sDMA switching uses the Peclet number to decide which method is valid
#' (\eqn{Pe < 1}: HRM; \eqn{Pe \ge 1}: secondary method). If Peclet numbers
#' are not updated after corrections, the switching decision will be based on
#' raw velocities and may be incorrect.
#'
#' @examples
#' \dontrun{
#' vh_corrected <- recalculate_peclet(
#'   vh_results      = vh_wound_corrected,
#'   probe_config    = probe,
#'   wood_properties = wood,
#'   velocity_col    = "Vh_cm_hr_wc",
#'   peclet_col      = "Pe_corrected"
#' )
#' }
#'
#' @family calibration functions
#' @export
recalculate_peclet <- function(vh_results,
                               probe_config,
                               wood_properties,
                               velocity_col = NULL,
                               peclet_col   = "Pe_corrected") {

  if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
  }
  if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
  }

  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame or tibble.")
  }

  # Auto-detect velocity column -- Vs_cm_hr is highest priority as the current best estimate
  if (is.null(velocity_col)) {
    velocity_col <- if ("Vs_cm_hr" %in% names(vh_results)) {
      "Vs_cm_hr"
    } else if ("Vh_cm_hr_wc" %in% names(vh_results)) {
      "Vh_cm_hr_wc"
    } else if ("Vh_cm_hr_sc" %in% names(vh_results)) {
      "Vh_cm_hr_sc"
    } else if ("Vh_cm_hr" %in% names(vh_results)) {
      "Vh_cm_hr"
    } else {
      stop(
        "No velocity column found. Expected one of: ",
        "Vs_cm_hr, Vh_cm_hr_wc, Vh_cm_hr_sc, Vh_cm_hr. ",
        "Available columns: ", paste(names(vh_results), collapse = ", ")
      )
    }
    message("recalculate_peclet: using velocity column '", velocity_col, "'.")
  }

  if (!velocity_col %in% names(vh_results)) {
    stop("Velocity column '", velocity_col, "' not found in vh_results.")
  }

  # Extract physical parameters
  x <- if (inherits(probe_config, "ProbeConfiguration")) {
    probe_config$probe_spacing
  } else if (is.list(probe_config)) {
    probe_config$probe_spacing
  } else {
    stop("probe_config must be a ProbeConfiguration object or a named list.")
  }

  D <- if (inherits(wood_properties, "WoodProperties")) {
    wood_properties$thermal_diffusivity
  } else if (is.list(wood_properties)) {
    wood_properties$thermal_diffusivity
  } else {
    stop("wood_properties must be a WoodProperties object or a named list.")
  }

  if (is.null(x) || is.na(x) || x <= 0) {
    stop("Invalid probe spacing: ", x)
  }
  if (is.null(D) || is.na(D) || D <= 0) {
    stop("Invalid thermal diffusivity: ", D)
  }

  # Pe = (Vh_cm_hr / 3600 * x) / D  [unitless]
  v_cm_s <- vh_results[[velocity_col]] / 3600
  vh_results[[peclet_col]] <- (v_cm_s * x) / D

  pe_vals <- vh_results[[peclet_col]]
  message(sprintf(
    "recalculate_peclet: %d points recalculated. Pe range: %.3f to %.3f.",
    nrow(vh_results),
    min(pe_vals, na.rm = TRUE),
    max(pe_vals, na.rm = TRUE)
  ))

  vh_results
}
