# R/01e_heat_pulse_velocity_core.R
#' Calculate Heat Pulse Velocity
#'
#' Calculates heat pulse velocity (Vh) from heat pulse temperature data using multiple methods
#' including Heat Ratio Method (HRM), Maximum Heat Ratio (MHR), T-max methods, and others.
#'
#' Progress reporting works through the \code{progressr} package. Wrap calls in
#' \code{progressr::with_progress({})} to see progress bars. Works in both console and Shiny.
#'
#' @param heat_pulse_data A heat_pulse_data object from read_heat_pulse_data()
#' @param pulse_ids Vector of pulse IDs to process. If NULL, processes all pulses.
#' @param methods Character vector of methods to use. Options: "HRM", "MHR", "HRMXa", "HRMXb",
#'   "Tmax_Coh", "Tmax_Klu".
#'
#'   Note: HRM results include Peclet numbers which can be used for method switching.
#'   Use apply_sdma_processing() to apply Dual Method Approach after calculation.
#'
#'   Examples:
#'   \itemize{
#'     \item \code{methods = c("HRM", "MHR")} - Basic comparison
#'     \item \code{methods = c("HRM", "MHR", "Tmax_Klu")} - Multiple methods for post-hoc DMA
#'     \item \code{methods = c("HRM", "HRMXa", "HRMXb")} - HRM variants
#'   }
#'
#'   Default: c("HRM", "MHR")
#' @param probe_config Probe configuration. Can be: ProbeConfiguration object, config name (e.g., "symmetrical"),
#'   path to custom YAML, or NULL (uses default symmetric config)
#' @param wood_properties Wood properties. Can be: WoodProperties object, config name (e.g., "eucalyptus"),
#'   path to custom YAML, or NULL (uses default generic softwood)
#' @param parameters List of calculation parameters. See details.
#' @param diffusivity Thermal diffusivity override (cm²/s). If provided, overrides wood_properties value.
#' @param probe_spacing Probe spacing override (cm). If provided, overrides probe_config value.
#' @param probe_corrections Optional output from apply_hpv_corrections(). If provided, corrections
#'   metadata will be attached to results. Note: This function calculates raw Vh; apply corrections
#'   afterwards using apply_hpv_corrections().
#' @param confirm_parameters Logical indicating whether to prompt for parameter confirmation
#'   in interactive mode (default: TRUE). Set to FALSE for non-interactive scripts.
#' @param plot_results Logical indicating whether to generate diagnostic plots
#' @param show_progress Logical indicating whether to report progress (default: TRUE)
#' @param fill_missing_pulses Logical indicating whether to auto-detect and fill missing pulses
#'   in the time series (default: TRUE). Missing pulses are added as rows with Vh_cm_hr = NA
#'   and quality_flag = "DATA_MISSING". This ensures complete time series for plotting and
#'   interpolation.
#' @param max_gap_hours Numeric maximum gap duration (hours) to fill with DATA_MISSING rows.
#'   Gaps larger than this are reported in messages but not filled to avoid creating excessive
#'   missing rows (default: 24 hours). Only applies if fill_missing_pulses = TRUE.
#' @param interval_tolerance_seconds Numeric tolerance in seconds for matching pulse times.
#'   Allows for clock drift and timing jitter (default: 5 seconds). Only applies if
#'   fill_missing_pulses = TRUE.
#'
#' @details
#' The parameters list should contain:
#' \describe{
#'   \item{diffusivity}{Thermal diffusivity of sapwood (cm²/s), default: 0.0025}
#'   \item{probe_spacing}{Distance from heat source (cm), default: 0.5}
#'   \item{L}{Lower proportion of deltaTmax for HRMX sampling window, default: 0.5}
#'   \item{H}{Higher proportion of deltaTmax for HRMX sampling window, default: 0.8}
#'   \item{tp_1}{Heat pulse duration (sec) for Tmax_Klu, default: 2}
#'   \item{HRM_start}{Start of sampling window for HRM (sec after pulse), default: 60}
#'   \item{HRM_end}{End of sampling window for HRM (sec after pulse), default: 100}
#'   \item{pre_pulse}{Pre-pulse period (sec), default: 30}
#' }
#'
#' @return A tibble containing calculated heat pulse velocities with columns:
#'   \item{datetime}{Timestamp of measurement}
#'   \item{pulse_id}{Pulse identification number}
#'   \item{method}{Calculation method used (e.g., "HRM", "MHR")}
#'   \item{sensor_position}{Inner or outer sensor position}
#'   \item{Vh_cm_hr}{Heat pulse velocity in cm/hr}
#'   \item{temp_ratio}{Temperature ratio used in calculation. For HRM/MHR/HRMX: v1/v2 ratio. NA for Tmax methods (which use calc_time_sec instead). This enables efficient recalculation with different thermal diffusivity values.}
#'   \item{calc_window_start_sec}{Start of calculation window (seconds after pulse). For HRM and HRMX: averaging window start. For MHR: time when upstream sensor reaches maximum. NA for Tmax methods.}
#'   \item{calc_window_end_sec}{End of calculation window (seconds after pulse). For HRM and HRMX: averaging window end. For MHR: time when downstream sensor reaches maximum. NA for Tmax methods.}
#'   \item{calc_time_sec}{Specific time of calculation (seconds after pulse). For Tmax methods: time to peak. For MHR: time when downstream sensor reaches maximum. NA for HRM/HRMX.}
#'   \item{peclet_number}{Peclet number (dimensionless) for HRM results. Pe = (Vh × x) / (D × 3600). Used for method switching in apply_sdma_processing(). NA for non-HRM methods.}
#'   \item{selected_method}{Reserved for future use (currently NA for all methods)}
#'   \item{quality_flag}{Data quality indicator}
#'
#' @examples
#' \dontrun{
#' # Load data and calculate velocities with default methods
#' heat_pulse_data <- read_heat_pulse_data("data.txt")
#' results <- calc_heat_pulse_velocity(heat_pulse_data)
#'
#' # Use specific methods and parameters
#' params <- list(diffusivity = 0.003, probe_spacing = 0.6)
#' results <- calc_heat_pulse_velocity(heat_pulse_data,
#'                                     methods = c("HRM", "MHR"),
#'                                     parameters = params)
#'
#' # Calculate methods for post-hoc DMA processing
#' results <- calc_heat_pulse_velocity(heat_pulse_data,
#'                                     methods = c("HRM", "MHR", "Tmax_Klu"))
#'
#' # Apply DMA switching after calculation
#' results_sdma <- apply_sdma_processing(results, secondary_method = "MHR")
#'
#' # Compare multiple DMA variants
#' results_sdma1 <- apply_sdma_processing(results, secondary_method = "MHR")
#' results_sdma2 <- apply_sdma_processing(results, secondary_method = "Tmax_Klu")
#' }
#'
#' @references
#' \itemize{
#'   \item Burgess et al. (2001) Tree Physiology 21:589-598 (HRM)
#'   \item Lopez et al. (2021) Plant Soil 469:503-523 (MHR)
#'   \item Cohen et al. (1981) Plant, Cell and Environment 4:391-397 (Tmax_Coh)
#'   \item Kluitenberg & Ham (2004) Ag For Met 126:169-173 (Tmax_Klu)
#' }
#'
#' @export
calc_heat_pulse_velocity <- function(heat_pulse_data,
                                     pulse_ids = NULL,
                                     methods = c("HRM", "MHR"),
                                     probe_config = NULL,
                                     wood_properties = NULL,
                                     parameters = NULL,
                                     diffusivity = NULL,
                                     probe_spacing = NULL,
                                     probe_corrections = NULL,
                                     confirm_parameters = TRUE,
                                     plot_results = FALSE,
                                     show_progress = TRUE,
                                     fill_missing_pulses = TRUE,
                                     max_gap_hours = 24,
                                     interval_tolerance_seconds = 5) {

  if (!inherits(heat_pulse_data, "heat_pulse_data")) {
    stop("Input must be a heat_pulse_data object from read_heat_pulse_data()")
  }

  # Load probe configuration
  if (is.null(probe_config)) {
    probe_config <- get_default_probe_config()
  } else if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
  }
  # Otherwise assume probe_config is already a ProbeConfiguration object

  # Load wood properties
  if (is.null(wood_properties)) {
    wood_properties <- get_default_wood_properties()
  } else if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
  }
  # Otherwise assume wood_properties is already a WoodProperties object

  # Build default parameters from configurations
  default_params <- list(
    diffusivity = wood_properties$thermal_diffusivity,  # From wood properties
    probe_spacing = probe_config$required_parameters$x,  # From probe config
    tp_1 = probe_config$heat_pulse_duration,            # From probe config (NEW!)
    L = 0.5,               # Lower proportion of deltaTmax for HRMX
    H = 0.8,               # Higher proportion of deltaTmax for HRMX
    HRM_start = 60,        # Start of sampling window for HRM (sec after pulse)
    HRM_end = 100,         # End of sampling window for HRM (sec after pulse)
    pre_pulse = 30         # Pre-pulse period (sec)
  )

  # Merge user parameters with defaults
  if (!is.null(parameters)) {
    params <- modifyList(default_params, parameters)
  } else {
    params <- default_params
  }

  # Apply individual overrides (highest priority)
  if (!is.null(diffusivity)) {
    params$diffusivity <- diffusivity
  }
  if (!is.null(probe_spacing)) {
    params$probe_spacing <- probe_spacing
  }

  # Get pulse IDs to process
  measurements <- heat_pulse_data$measurements
  if (is.null(pulse_ids)) {
    pulse_ids <- unique(measurements$pulse_id)
  }

  # Validate pulse IDs exist
  missing_ids <- setdiff(pulse_ids, unique(measurements$pulse_id))
  if (length(missing_ids) > 0) {
    warning("Pulse IDs not found in data: ", paste(missing_ids, collapse = ", "))
    pulse_ids <- intersect(pulse_ids, unique(measurements$pulse_id))
  }

  if (length(pulse_ids) == 0) {
    stop("No valid pulse IDs to process")
  }

  # Show parameter summary and get confirmation
  if (confirm_parameters && interactive()) {
    confirmed <- prompt_parameter_confirmation(
      probe_config = probe_config,
      wood_properties = wood_properties,
      params = params,
      methods = methods,
      n_pulses = length(pulse_ids)
    )

    if (!confirmed) {
      message("\nCalculation cancelled by user.")
      return(invisible(NULL))
    }
  }

  # OPTIMISATION: Pre-split measurements by pulse_id once (massive speedup!)
  # This avoids scanning entire dataset for each pulse
  measurements_by_pulse <- split(measurements, measurements$pulse_id)

  # Check if we're in a Shiny session (where caller manages progress wrapping)
  in_shiny <- tryCatch({
    !is.null(shiny::getDefaultReactiveDomain())
  }, error = function(e) FALSE)

  # If not in Shiny and progress is enabled, set up for R console
  if (show_progress && !in_shiny) {
    # R console: set up text progress bar and wrap in with_progress
    progressr::handlers("txtprogressbar")

    return(progressr::with_progress({
      calc_heat_pulse_velocity_internal(
        measurements_by_pulse, pulse_ids, params, methods, plot_results,
        show_progress, fill_missing_pulses, max_gap_hours,
        interval_tolerance_seconds, probe_corrections
      )
    }))
  }

  # In Shiny or progress disabled - just run directly
  return(calc_heat_pulse_velocity_internal(
    measurements_by_pulse, pulse_ids, params, methods, plot_results,
    show_progress, fill_missing_pulses, max_gap_hours,
    interval_tolerance_seconds, probe_corrections
  ))
}


#' Internal calculation function (called with progress context already set up)
#' @keywords internal
calc_heat_pulse_velocity_internal <- function(measurements_by_pulse, pulse_ids,
                                              params, methods, plot_results,
                                              show_progress, fill_missing_pulses,
                                              max_gap_hours, interval_tolerance_seconds,
                                              probe_corrections) {

  # Process each pulse with progress reporting
  all_results <- list()
  successful_pulses <- 0
  n_pulses <- length(pulse_ids)
  n_methods <- length(methods)

  # Create progress reporter with total steps = pulses * methods for granular feedback
  total_steps <- n_pulses * n_methods
  p <- if (show_progress && n_pulses > 0) {
    progressr::progressor(steps = total_steps)
  } else {
    NULL
  }

  # Throttle progress updates to avoid UI slowdown
  update_frequency <- 100  # Update every 100 method calculations
  methods_completed <- 0
  methods_since_last_update <- 0

  for (i in seq_along(pulse_ids)) {
    pid <- pulse_ids[i]

    tryCatch({
      # Pass progress reporter to single pulse function for per-method updates
      pulse_result <- calc_vh_single_pulse(
        measurements_by_pulse[[as.character(pid)]],
        pid, params, methods, plot_results,
        progress_fn = NULL,  # Don't report from inside function
        pulse_num = i,
        total_pulses = n_pulses
      )
      all_results[[i]] <- pulse_result
      successful_pulses <- successful_pulses + 1

      # Track method completion and update progress periodically
      methods_completed <- methods_completed + n_methods
      methods_since_last_update <- methods_since_last_update + n_methods

      # Update progress every 100 methods OR at the end
      if (show_progress && !is.null(p) &&
          (methods_since_last_update >= update_frequency || methods_completed == total_steps)) {
        p(amount = methods_since_last_update,
          message = sprintf("Processed %s / %s method calculations (%.1f%% complete)",
                           format(methods_completed, big.mark = ","),
                           format(total_steps, big.mark = ","),
                           100 * methods_completed / total_steps))
        methods_since_last_update <- 0
      }

    }, error = function(e) {
      message("Error processing pulse ", pid, ": ", e$message)

      # Still track progress for methods that would have been processed
      methods_completed <- methods_completed + n_methods
      methods_since_last_update <- methods_since_last_update + n_methods

      if (show_progress && !is.null(p) &&
          (methods_since_last_update >= update_frequency || methods_completed == total_steps)) {
        p(amount = methods_since_last_update,
          message = sprintf("Processed %s / %s method calculations (%.1f%% complete)",
                           format(methods_completed, big.mark = ","),
                           format(total_steps, big.mark = ","),
                           100 * methods_completed / total_steps))
        methods_since_last_update <- 0
      }
    })
  }

  if (successful_pulses == 0) {
    stop("No pulses were successfully processed")
  }

  # Combine results
  combined_results <- dplyr::bind_rows(all_results)

  # Add quality flags
  combined_results <- add_quality_flags(combined_results)

  # Print comprehensive calculation summary
  print_calculation_summary(combined_results, successful_pulses)

  # Check if probe corrections should be applied
  if (!is.null(probe_corrections)) {
    # Attach correction metadata
    attr(combined_results, "probe_corrections_available") <- TRUE
    attr(combined_results, "correction_metadata") <- attr(probe_corrections, "correction_summary")
    message("Probe correction metadata attached. Apply corrections using apply_hpv_corrections().")
  } else {
    # Warn about missing corrections
    if (!isTRUE(getOption("sapfluxr.suppress_correction_warnings"))) {
      message("\nNote: Probe corrections not applied. For accurate results, consider:\n",
              "  1. Collecting zero-flow calibration data\n",
              "  2. Measuring wood properties from cores\n",
              "  3. Estimating wound diameter\n",
              "  4. Using apply_hpv_corrections() before flux calculations\n",
              "Set options(sapfluxr.suppress_correction_warnings = TRUE) to suppress this message.")
    }
  }

  # Auto-fill missing pulses if requested
  if (fill_missing_pulses && nrow(combined_results) > 1) {
    # Check if there are enough unique datetimes for auto-detection
    n_unique_times <- length(unique(combined_results$datetime))

    if (n_unique_times >= 2) {
      if (show_progress) {
        message("\nDetecting missing pulses...")
      }

      tryCatch({
        missing_pulse_results <- detect_and_fill_missing_pulses(
          vh_results = combined_results,
          expected_interval_hours = NULL,  # Auto-detect
          tolerance_seconds = interval_tolerance_seconds,
          add_rows = TRUE,
          max_gap_to_fill_hours = max_gap_hours,
          verbose = show_progress
        )

        combined_results <- missing_pulse_results$vh_complete

        # Report any large gaps that weren't filled
        if (!is.null(missing_pulse_results$gap_report) && nrow(missing_pulse_results$gap_report) > 0) {
          large_gaps <- missing_pulse_results$gap_report[!missing_pulse_results$gap_report$filled, ]
          if (nrow(large_gaps) > 0) {
            message(sprintf("\nWarning: %d large gap(s) > %.1f hours detected but not filled:",
                           nrow(large_gaps), max_gap_hours))
            for (i in seq_len(nrow(large_gaps))) {
              message(sprintf("  Gap %d: %s to %s (%.1f hours, %d missing pulses)",
                             i,
                             format(large_gaps$gap_start[i]),
                             format(large_gaps$gap_end[i]),
                             large_gaps$duration_hours[i],
                             large_gaps$n_missing[i]))
            }
            message("  Increase max_gap_hours parameter to fill these gaps if desired.")
          }
        }

        # Attach gap metadata as attribute
        attr(combined_results, "missing_pulse_summary") <- missing_pulse_results$summary
        attr(combined_results, "gap_report") <- missing_pulse_results$gap_report

      }, error = function(e) {
        if (show_progress) {
          message(sprintf("Note: Could not auto-detect missing pulses (%s)", e$message))
        }
      })
    } else if (show_progress) {
      message("\nNote: Insufficient data for missing pulse detection (need at least 2 unique timestamps)")
    }
  }

  # CRITICAL: Add vh_results class
  class(combined_results) <- c("vh_results", class(combined_results))

  # Store calculation parameters as attributes for efficient recalculation
  attr(combined_results, "diffusivity") <- params$diffusivity
  attr(combined_results, "probe_spacing") <- params$probe_spacing

  return(combined_results)
}


#' Calculate velocity for single pulse
#'
#' @param pulse_data Pre-filtered data frame for this pulse
#' @param pulse_id ID of pulse to process
#' @param parameters List of parameters
#' @param methods Character vector of methods
#' @param plot_results Whether to plot results
#' @param progress_fn Progress reporter function (optional)
#' @param pulse_num Current pulse number (optional, for progress reporting)
#' @param total_pulses Total number of pulses (optional, for progress reporting)
#' @return Data frame with results for this pulse
#' @keywords internal
calc_vh_single_pulse <- function(pulse_data, pulse_id, parameters, methods, plot_results = FALSE,
                                  progress_fn = NULL, pulse_num = NULL, total_pulses = NULL) {

  # Data is already filtered by pulse_id, just validate
  if (is.null(pulse_data) || nrow(pulse_data) == 0) {
    stop("No data found for pulse ID: ", pulse_id)
  }

  # Check required temperature columns
  temp_cols <- c("do", "di", "uo", "ui")
  missing_cols <- setdiff(temp_cols, names(pulse_data))
  if (length(missing_cols) > 0) {
    stop("Missing temperature columns: ", paste(missing_cols, collapse = ", "))
  }

  # Extract parameters
  diffusivity <- parameters$diffusivity
  probe_spacing <- parameters$probe_spacing
  L <- parameters$L
  H <- parameters$H
  tp_1 <- parameters$tp_1
  HRM_start <- parameters$HRM_start
  HRM_end <- parameters$HRM_end
  pre_pulse <- parameters$pre_pulse

  # Calculate time after heat pulse
  pre_pulse_period <- 1:min(pre_pulse, nrow(pulse_data))
  tp <- pmax(1:nrow(pulse_data) - pre_pulse, 0)
  HRM_period <- tp >= HRM_start & tp < HRM_end

  # Calculate pre-pulse mean temperatures
  do_mu_pre <- mean(pulse_data$do[pre_pulse_period], na.rm = TRUE)
  di_mu_pre <- mean(pulse_data$di[pre_pulse_period], na.rm = TRUE)
  uo_mu_pre <- mean(pulse_data$uo[pre_pulse_period], na.rm = TRUE)
  ui_mu_pre <- mean(pulse_data$ui[pre_pulse_period], na.rm = TRUE)

  # Calculate delta temperatures
  deltaT_do <- pulse_data$do - do_mu_pre
  deltaT_do[pre_pulse_period] <- NA
  deltaT_di <- pulse_data$di - di_mu_pre
  deltaT_di[pre_pulse_period] <- NA
  deltaT_uo <- pulse_data$uo - uo_mu_pre
  deltaT_uo[pre_pulse_period] <- NA
  deltaT_ui <- pulse_data$ui - ui_mu_pre
  deltaT_ui[pre_pulse_period] <- NA

  # Calculate temperature ratios
  dTratio_douo <- deltaT_do / deltaT_uo
  dTratio_diui <- deltaT_di / deltaT_ui

  # OPTIMISATION: Pre-compute peak information once (used by MHR, Tmax, HRMX)
  # This avoids redundant max() and which.max() calls across multiple methods
  peak_info <- list(
    # Maximum values
    dTdo_max = max(deltaT_do, na.rm = TRUE),
    dTdi_max = max(deltaT_di, na.rm = TRUE),
    dTuo_max = max(deltaT_uo, na.rm = TRUE),
    dTui_max = max(deltaT_ui, na.rm = TRUE),

    # Peak indices (which.max returns index in vector)
    idx_do = which.max(deltaT_do),
    idx_di = which.max(deltaT_di),
    idx_uo = which.max(deltaT_uo),
    idx_ui = which.max(deltaT_ui)
  )

  # Calculate peak times relative to pulse injection
  peak_info$time_do <- peak_info$idx_do - pre_pulse
  peak_info$time_di <- peak_info$idx_di - pre_pulse
  peak_info$time_uo <- peak_info$idx_uo - pre_pulse
  peak_info$time_ui <- peak_info$idx_ui - pre_pulse

  # Initialize results
  method_results <- list()

  # Heat Ratio Method (HRM)
  if ("HRM" %in% methods) {
    hrm_results <- calc_hrm(dTratio_douo, dTratio_diui, HRM_period, diffusivity, probe_spacing, tp)
    method_results[["HRM"]] <- hrm_results
  }

  # Maximum Heat Ratio (MHR)
  if ("MHR" %in% methods) {
    mhr_results <- calc_mhr(deltaT_do, deltaT_di, deltaT_uo, deltaT_ui,
                            diffusivity, probe_spacing, pre_pulse, peak_info)
    method_results[["MHR"]] <- mhr_results
  }

  # HRMX methods
  if ("HRMXa" %in% methods || "HRMXb" %in% methods) {
    hrmx_results <- calc_hrmx(deltaT_do, deltaT_di, deltaT_uo, deltaT_ui,
                              dTratio_douo, dTratio_diui, L, H,
                              diffusivity, probe_spacing, tp, peak_info)
    if ("HRMXa" %in% methods) {
      method_results[["HRMXa"]] <- hrmx_results$HRMXa
    }
    if ("HRMXb" %in% methods) {
      method_results[["HRMXb"]] <- hrmx_results$HRMXb
    }
  }

  # T-max methods
  if ("Tmax_Coh" %in% methods) {
    tmax_coh_results <- calc_tmax_coh(deltaT_do, deltaT_di, diffusivity,
                                      probe_spacing, pre_pulse, peak_info)
    method_results[["Tmax_Coh"]] <- tmax_coh_results
  }

  if ("Tmax_Klu" %in% methods) {
    tmax_klu_results <- calc_tmax_klu(deltaT_do, deltaT_di, diffusivity,
                                      probe_spacing, tp_1, pre_pulse, peak_info)
    method_results[["Tmax_Klu"]] <- tmax_klu_results
  }

  # Create output data frame
  datetime_pulse <- pulse_data$datetime[1]
  result_rows <- list()

  for (method_name in names(method_results)) {
    method_result <- method_results[[method_name]]

    # Check if this is an sDMA method (has Peclet number data)
    has_peclet <- !is.null(method_result$peclet_outer)

    result_rows[[paste0(method_name, "_outer")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "outer",
      Vh_cm_hr = method_result$outer,
      temp_ratio = if (!is.null(method_result$temp_ratio_outer)) method_result$temp_ratio_outer else NA_real_,
      calc_window_start_sec = method_result$window_start_outer,
      calc_window_end_sec = method_result$window_end_outer,
      calc_time_sec = method_result$calc_time_outer,
      peclet_number = if (has_peclet) method_result$peclet_outer else NA_real_,
      selected_method = NA_character_,  # Only populated by apply_sdma_processing()
      # HRMXb-specific: separate downstream/upstream windows
      downstream_window_start_sec = if (!is.null(method_result$downstream_window_start_outer)) method_result$downstream_window_start_outer else NA_real_,
      downstream_window_end_sec = if (!is.null(method_result$downstream_window_end_outer)) method_result$downstream_window_end_outer else NA_real_,
      upstream_window_start_sec = if (!is.null(method_result$upstream_window_start_outer)) method_result$upstream_window_start_outer else NA_real_,
      upstream_window_end_sec = if (!is.null(method_result$upstream_window_end_outer)) method_result$upstream_window_end_outer else NA_real_,
      stringsAsFactors = FALSE
    )
    result_rows[[paste0(method_name, "_inner")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "inner",
      Vh_cm_hr = method_result$inner,
      temp_ratio = if (!is.null(method_result$temp_ratio_inner)) method_result$temp_ratio_inner else NA_real_,
      calc_window_start_sec = method_result$window_start_inner,
      calc_window_end_sec = method_result$window_end_inner,
      calc_time_sec = method_result$calc_time_inner,
      peclet_number = if (has_peclet) method_result$peclet_inner else NA_real_,
      selected_method = NA_character_,  # Only populated by apply_sdma_processing()
      # HRMXb-specific: separate downstream/upstream windows
      downstream_window_start_sec = if (!is.null(method_result$downstream_window_start_inner)) method_result$downstream_window_start_inner else NA_real_,
      downstream_window_end_sec = if (!is.null(method_result$downstream_window_end_inner)) method_result$downstream_window_end_inner else NA_real_,
      upstream_window_start_sec = if (!is.null(method_result$upstream_window_start_inner)) method_result$upstream_window_start_inner else NA_real_,
      upstream_window_end_sec = if (!is.null(method_result$upstream_window_end_inner)) method_result$upstream_window_end_inner else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  result_df <- dplyr::bind_rows(result_rows)

  # Plot if requested
  if (plot_results) {
    plot_pulse_temps(pulse_data, deltaT_do, deltaT_di, deltaT_uo, deltaT_ui, pulse_id)
  }

  return(result_df)
}

# Method-specific calculation functions

#' Calculate HRM velocities
#' @keywords internal
calc_hrm <- function(dTratio_douo, dTratio_diui, HRM_period, diffusivity, probe_spacing, tp) {

  # Validate inputs
  if (!is.logical(HRM_period)) {
    stop("HRM_period must be a logical vector")
  }

  if (length(HRM_period) != length(dTratio_douo) || length(HRM_period) != length(dTratio_diui)) {
    stop("HRM_period length must match temperature ratio vector lengths")
  }

  # Check if there are any valid HRM sampling periods
  hrm_indices <- which(HRM_period)
  if (length(hrm_indices) == 0) {
    warning("No valid HRM sampling period")
    return(list(
      outer = NA_real_,
      inner = NA_real_,
      window_start_outer = NA_real_,
      window_end_outer = NA_real_,
      window_start_inner = NA_real_,
      window_end_inner = NA_real_,
      calc_time_outer = NA_real_,
      calc_time_inner = NA_real_
    ))
  }

  # Get window boundaries in seconds (not indices)
  window_start_idx <- min(hrm_indices)
  window_end_idx <- max(hrm_indices)
  window_start <- tp[window_start_idx]
  window_end <- tp[window_end_idx]

  # Get ratios for HRM period
  hrm_ratios_outer <- dTratio_douo[HRM_period]
  hrm_ratios_inner <- dTratio_diui[HRM_period]

  # Check for valid data
  valid_outer <- !is.na(hrm_ratios_outer) & is.finite(hrm_ratios_outer) & hrm_ratios_outer > 0
  valid_inner <- !is.na(hrm_ratios_inner) & is.finite(hrm_ratios_inner) & hrm_ratios_inner > 0

  if (sum(valid_outer) == 0) {
    warning("All temperature ratios are NA for outer sensors in HRM period")
    dTratio_HRM_douo_mean <- NA_real_
  } else {
    dTratio_HRM_douo_mean <- mean(hrm_ratios_outer[valid_outer], na.rm = TRUE)
  }

  if (sum(valid_inner) == 0) {
    warning("All temperature ratios are NA for inner sensors in HRM period")
    dTratio_HRM_diui_mean <- NA_real_
  } else {
    dTratio_HRM_diui_mean <- mean(hrm_ratios_inner[valid_inner], na.rm = TRUE)
  }

  # Calculate velocities
  if (is.na(dTratio_HRM_douo_mean) || dTratio_HRM_douo_mean <= 0) {
    Vho_HRM <- NA_real_
  } else {
    Vho_HRM <- diffusivity / probe_spacing * log(dTratio_HRM_douo_mean) * 3600
  }

  if (is.na(dTratio_HRM_diui_mean) || dTratio_HRM_diui_mean <= 0) {
    Vhi_HRM <- NA_real_
  } else {
    Vhi_HRM <- diffusivity / probe_spacing * log(dTratio_HRM_diui_mean) * 3600
  }

  # Calculate Peclet number (dimensionless)
  # Pe = (Vh × x) / (D × 3600)
  # where Vh is in cm/hr, x in cm, D in cm²/s
  # The 3600 converts D from cm²/s to cm²/hr to match Vh units
  Pe_outer <- if (!is.na(Vho_HRM) && is.finite(Vho_HRM)) {
    (Vho_HRM * probe_spacing) / (diffusivity * 3600)
  } else {
    NA_real_
  }

  Pe_inner <- if (!is.na(Vhi_HRM) && is.finite(Vhi_HRM)) {
    (Vhi_HRM * probe_spacing) / (diffusivity * 3600)
  } else {
    NA_real_
  }

  return(list(
    outer = Vho_HRM,
    inner = Vhi_HRM,
    temp_ratio_outer = dTratio_HRM_douo_mean,
    temp_ratio_inner = dTratio_HRM_diui_mean,
    peclet_outer = Pe_outer,
    peclet_inner = Pe_inner,
    window_start_outer = window_start,
    window_end_outer = window_end,
    window_start_inner = window_start,
    window_end_inner = window_end,
    calc_time_outer = NA_real_,
    calc_time_inner = NA_real_
  ))
}

#' Calculate MHR velocities
#' @param peak_info Pre-computed peak information from calc_vh_single_pulse
#' @keywords internal
calc_mhr <- function(deltaT_do, deltaT_di, deltaT_uo, deltaT_ui, diffusivity, probe_spacing, pre_pulse, peak_info) {

  # Input validation
  if (all(is.na(deltaT_do)) || all(is.na(deltaT_di)) ||
      all(is.na(deltaT_uo)) || all(is.na(deltaT_ui))) {
    warning("All temperature differences are NA")
    return(list(
      outer = NA_real_,
      inner = NA_real_,
      window_start_outer = NA_real_,
      window_end_outer = NA_real_,
      window_start_inner = NA_real_,
      window_end_inner = NA_real_,
      calc_time_outer = NA_real_,
      calc_time_inner = NA_real_
    ))
  }

  # Use pre-computed peak information (OPTIMISATION: avoids redundant scans)
  dTdo_max <- peak_info$dTdo_max
  dTdi_max <- peak_info$dTdi_max
  dTuo_max <- peak_info$dTuo_max
  dTui_max <- peak_info$dTui_max

  time_do <- peak_info$time_do
  time_di <- peak_info$time_di
  time_uo <- peak_info$time_uo
  time_ui <- peak_info$time_ui

  # Check for valid maximums
  if (any(c(dTdo_max, dTdi_max, dTuo_max, dTui_max) <= 0) ||
      any(!is.finite(c(dTdo_max, dTdi_max, dTuo_max, dTui_max)))) {
    warning("Invalid maximum temperature increases for MHR")
    return(list(
      outer = NA_real_,
      inner = NA_real_,
      window_start_outer = NA_real_,
      window_end_outer = NA_real_,
      window_start_inner = NA_real_,
      window_end_inner = NA_real_,
      calc_time_outer = NA_real_,
      calc_time_inner = NA_real_
    ))
  }

  # Calculate ratios and velocities
  dTdo_max_dTuo_max <- dTdo_max / dTuo_max
  dTdi_max_dTui_max <- dTdi_max / dTui_max

  if (dTdo_max_dTuo_max <= 0) {
    warning("Non-positive outer sensor temperature ratio for MHR")
    Vho_MHR <- NA_real_
  } else {
    Vho_MHR <- (diffusivity / probe_spacing) * log(dTdo_max_dTuo_max) * 3600
  }

  if (dTdi_max_dTui_max <= 0) {
    warning("Non-positive inner sensor temperature ratio for MHR")
    Vhi_MHR <- NA_real_
  } else {
    Vhi_MHR <- (diffusivity / probe_spacing) * log(dTdi_max_dTui_max) * 3600
  }

  # For MHR: window_start = upstream peak, window_end = downstream peak
  # (not min/max - specific to probe type per documentation)

  # For outer sensors
  window_start_outer <- time_uo  # Upstream outer peak time
  window_end_outer <- time_do    # Downstream outer peak time

  # For inner sensors
  window_start_inner <- time_ui  # Upstream inner peak time
  window_end_inner <- time_di    # Downstream inner peak time

  return(list(
    outer = Vho_MHR,
    inner = Vhi_MHR,
    temp_ratio_outer = dTdo_max_dTuo_max,
    temp_ratio_inner = dTdi_max_dTui_max,
    window_start_outer = window_start_outer,
    window_end_outer = window_end_outer,
    window_start_inner = window_start_inner,
    window_end_inner = window_end_inner,
    calc_time_outer = time_do,  # Downstream outer peak time
    calc_time_inner = time_di   # Downstream inner peak time
  ))
}

#' Calculate HRMX velocities
#' @param peak_info Pre-computed peak information from calc_vh_single_pulse
#' @keywords internal
calc_hrmx <- function(deltaT_do, deltaT_di, deltaT_uo, deltaT_ui,
                      dTratio_douo, dTratio_diui, L, H, diffusivity, probe_spacing, tp, peak_info) {

  # Input validation
  if (all(is.na(deltaT_do)) || all(is.na(deltaT_di)) ||
      all(is.na(deltaT_uo)) || all(is.na(deltaT_ui))) {
    warning("All temperature differences are NA for HRMX")
    return(list(
      HRMXa = list(
        outer = NA_real_, inner = NA_real_,
        window_start_outer = NA_real_, window_end_outer = NA_real_,
        window_start_inner = NA_real_, window_end_inner = NA_real_,
        calc_time_outer = NA_real_, calc_time_inner = NA_real_
      ),
      HRMXb = list(
        outer = NA_real_, inner = NA_real_,
        window_start_outer = NA_real_, window_end_outer = NA_real_,
        window_start_inner = NA_real_, window_end_inner = NA_real_,
        calc_time_outer = NA_real_, calc_time_inner = NA_real_
      )
    ))
  }

  # Use pre-computed peak information (OPTIMISATION: avoids redundant scans)
  dTdo_max <- peak_info$dTdo_max
  dTdi_max <- peak_info$dTdi_max
  dTuo_max <- peak_info$dTuo_max
  dTui_max <- peak_info$dTui_max

  idx_do_max <- peak_info$idx_do
  idx_di_max <- peak_info$idx_di
  idx_uo_max <- peak_info$idx_uo
  idx_ui_max <- peak_info$idx_ui

  # Calculate pre-max values (only on rising limb BEFORE maximum)
  dTdo_premax <- c(NA, ifelse(diff(deltaT_do) > 0, deltaT_do[-1], NA))
  dTdo_premax[idx_do_max:length(dTdo_premax)] <- NA  # Exclude points at or after max
  
  dTdi_premax <- c(NA, ifelse(diff(deltaT_di) > 0, deltaT_di[-1], NA))
  dTdi_premax[idx_di_max:length(dTdi_premax)] <- NA  # Exclude points at or after max
  
  dTuo_premax <- c(NA, ifelse(diff(deltaT_uo) > 0, deltaT_uo[-1], NA))
  dTuo_premax[idx_uo_max:length(dTuo_premax)] <- NA  # Exclude points at or after max
  
  dTui_premax <- c(NA, ifelse(diff(deltaT_ui) > 0, deltaT_ui[-1], NA))
  dTui_premax[idx_ui_max:length(dTui_premax)] <- NA  # Exclude points at or after max

  # Calculate window bounds
  dTdo_max_L <- dTdo_max * L
  dTdi_max_L <- dTdi_max * L
  dTuo_max_L <- dTuo_max * L
  dTui_max_L <- dTui_max * L

  dTdo_max_H <- dTdo_max * H
  dTdi_max_H <- dTdi_max * H
  dTuo_max_H <- dTuo_max * H
  dTui_max_H <- dTui_max * H

  # Apply HRMX windows
  dTdo_HRMX <- ifelse(dTdo_premax < dTdo_max_L | dTdo_premax > dTdo_max_H, NA, dTdo_premax)
  dTdi_HRMX <- ifelse(dTdi_premax < dTdi_max_L | dTdi_premax > dTdi_max_H, NA, dTdi_premax)
  dTuo_HRMX <- ifelse(dTuo_premax < dTuo_max_L | dTuo_premax > dTuo_max_H, NA, dTuo_premax)
  dTui_HRMX <- ifelse(dTui_premax < dTui_max_L | dTui_premax > dTui_max_H, NA, dTui_premax)

  # Calculate means
  dTdo_HRMX_mean <- mean(dTdo_HRMX, na.rm = TRUE)
  dTdi_HRMX_mean <- mean(dTdi_HRMX, na.rm = TRUE)
  dTuo_HRMX_mean <- mean(dTuo_HRMX, na.rm = TRUE)
  dTui_HRMX_mean <- mean(dTui_HRMX, na.rm = TRUE)

  # HRMXa calculations
  dTo_ratio_HMRX_Window <- dTratio_douo
  dTo_ratio_HMRX_Window[is.na(dTdo_HRMX)] <- NA
  dTi_ratio_HMRX_Window <- dTratio_diui
  dTi_ratio_HMRX_Window[is.na(dTdi_HRMX)] <- NA

  dTo_ratio_HMRX_u <- dTratio_douo
  dTo_ratio_HMRX_u[is.na(dTuo_HRMX)] <- NA
  dTi_ratio_HMRX_u <- dTratio_diui
  dTi_ratio_HMRX_u[is.na(dTui_HRMX)] <- NA

  # Select ratio calculation method
  dTo_ratio_HRMX_mean <- ifelse(dTdo_HRMX_mean > dTuo_HRMX_mean,
                                mean(dTo_ratio_HMRX_Window, na.rm = TRUE),
                                mean(dTo_ratio_HMRX_u, na.rm = TRUE))

  dTi_ratio_HRMX_mean <- ifelse(dTdi_HRMX_mean > dTui_HRMX_mean,
                                mean(dTi_ratio_HMRX_Window, na.rm = TRUE),
                                mean(dTi_ratio_HMRX_u, na.rm = TRUE))

  # HRMXb calculations
  dT_ratio_douo_HRMX_mean <- dTdo_HRMX_mean / dTuo_HRMX_mean
  dT_ratio_diui_HRMX_mean <- dTdi_HRMX_mean / dTui_HRMX_mean

  # Calculate velocities with error checking
  if (is.na(dTo_ratio_HRMX_mean) || dTo_ratio_HRMX_mean <= 0) {
    Vho_HRMXa <- NA_real_
  } else {
    Vho_HRMXa <- (diffusivity / probe_spacing) * log(dTo_ratio_HRMX_mean) * 3600
  }

  if (is.na(dTi_ratio_HRMX_mean) || dTi_ratio_HRMX_mean <= 0) {
    Vhi_HRMXa <- NA_real_
  } else {
    Vhi_HRMXa <- (diffusivity / probe_spacing) * log(dTi_ratio_HRMX_mean) * 3600
  }

  if (is.na(dT_ratio_douo_HRMX_mean) || dT_ratio_douo_HRMX_mean <= 0) {
    Vho_HRMXb <- NA_real_
  } else {
    Vho_HRMXb <- (diffusivity / probe_spacing) * log(dT_ratio_douo_HRMX_mean) * 3600
  }

  if (is.na(dT_ratio_diui_HRMX_mean) || dT_ratio_diui_HRMX_mean <= 0) {
    Vhi_HRMXb <- NA_real_
  } else {
    Vhi_HRMXb <- (diffusivity / probe_spacing) * log(dT_ratio_diui_HRMX_mean) * 3600
  }

  # Track which time points were actually used for HRMXa
  # Outer: uses either downstream or upstream depending on which had higher mean
  hrmxa_outer_indices <- if (!is.na(dTdo_HRMX_mean) && !is.na(dTuo_HRMX_mean) && dTdo_HRMX_mean > dTuo_HRMX_mean) {
    which(!is.na(dTdo_HRMX))
  } else {
    which(!is.na(dTuo_HRMX))
  }

  # Inner: uses either downstream or upstream depending on which had higher mean
  hrmxa_inner_indices <- if (!is.na(dTdi_HRMX_mean) && !is.na(dTui_HRMX_mean) && dTdi_HRMX_mean > dTui_HRMX_mean) {
    which(!is.na(dTdi_HRMX))
  } else {
    which(!is.na(dTui_HRMX))
  }

  # Track which time points were used for HRMXb (uses all valid points from both sensors)
  hrmxb_outer_indices <- unique(c(which(!is.na(dTdo_HRMX)), which(!is.na(dTuo_HRMX))))
  hrmxb_inner_indices <- unique(c(which(!is.na(dTdi_HRMX)), which(!is.na(dTui_HRMX))))

  # Convert indices to seconds
  hrmxa_window_start_outer <- if (length(hrmxa_outer_indices) > 0) tp[min(hrmxa_outer_indices)] else NA_real_
  hrmxa_window_end_outer <- if (length(hrmxa_outer_indices) > 0) tp[max(hrmxa_outer_indices)] else NA_real_
  hrmxa_window_start_inner <- if (length(hrmxa_inner_indices) > 0) tp[min(hrmxa_inner_indices)] else NA_real_
  hrmxa_window_end_inner <- if (length(hrmxa_inner_indices) > 0) tp[max(hrmxa_inner_indices)] else NA_real_

  hrmxb_window_start_outer <- if (length(hrmxb_outer_indices) > 0) tp[min(hrmxb_outer_indices)] else NA_real_
  hrmxb_window_end_outer <- if (length(hrmxb_outer_indices) > 0) tp[max(hrmxb_outer_indices)] else NA_real_
  hrmxb_window_start_inner <- if (length(hrmxb_inner_indices) > 0) tp[min(hrmxb_inner_indices)] else NA_real_
  hrmxb_window_end_inner <- if (length(hrmxb_inner_indices) > 0) tp[max(hrmxb_inner_indices)] else NA_real_

  # For HRMXb, also store separate downstream and upstream windows
  # (since they sample different time periods)
  do_indices <- which(!is.na(dTdo_HRMX))
  di_indices <- which(!is.na(dTdi_HRMX))
  uo_indices <- which(!is.na(dTuo_HRMX))
  ui_indices <- which(!is.na(dTui_HRMX))
  
  hrmxb_downstream_start_outer <- if (length(do_indices) > 0) tp[min(do_indices)] else NA_real_
  hrmxb_downstream_end_outer <- if (length(do_indices) > 0) tp[max(do_indices)] else NA_real_
  hrmxb_upstream_start_outer <- if (length(uo_indices) > 0) tp[min(uo_indices)] else NA_real_
  hrmxb_upstream_end_outer <- if (length(uo_indices) > 0) tp[max(uo_indices)] else NA_real_
  
  hrmxb_downstream_start_inner <- if (length(di_indices) > 0) tp[min(di_indices)] else NA_real_
  hrmxb_downstream_end_inner <- if (length(di_indices) > 0) tp[max(di_indices)] else NA_real_
  hrmxb_upstream_start_inner <- if (length(ui_indices) > 0) tp[min(ui_indices)] else NA_real_
  hrmxb_upstream_end_inner <- if (length(ui_indices) > 0) tp[max(ui_indices)] else NA_real_

  return(list(
    HRMXa = list(
      outer = Vho_HRMXa,
      inner = Vhi_HRMXa,
      temp_ratio_outer = dTo_ratio_HRMX_mean,
      temp_ratio_inner = dTi_ratio_HRMX_mean,
      window_start_outer = hrmxa_window_start_outer,
      window_end_outer = hrmxa_window_end_outer,
      window_start_inner = hrmxa_window_start_inner,
      window_end_inner = hrmxa_window_end_inner,
      calc_time_outer = NA_real_,
      calc_time_inner = NA_real_
    ),
    HRMXb = list(
      outer = Vho_HRMXb,
      inner = Vhi_HRMXb,
      temp_ratio_outer = dT_ratio_douo_HRMX_mean,
      temp_ratio_inner = dT_ratio_diui_HRMX_mean,
      window_start_outer = hrmxb_window_start_outer,
      window_end_outer = hrmxb_window_end_outer,
      window_start_inner = hrmxb_window_start_inner,
      window_end_inner = hrmxb_window_end_inner,
      calc_time_outer = NA_real_,
      calc_time_inner = NA_real_,
      # Separate windows for visualization
      downstream_window_start_outer = hrmxb_downstream_start_outer,
      downstream_window_end_outer = hrmxb_downstream_end_outer,
      upstream_window_start_outer = hrmxb_upstream_start_outer,
      upstream_window_end_outer = hrmxb_upstream_end_outer,
      downstream_window_start_inner = hrmxb_downstream_start_inner,
      downstream_window_end_inner = hrmxb_downstream_end_inner,
      upstream_window_start_inner = hrmxb_upstream_start_inner,
      upstream_window_end_inner = hrmxb_upstream_end_inner
    )
  ))
}

#' Calculate Tmax Cohen velocities
#' @param peak_info Pre-computed peak information from calc_vh_single_pulse
#' @keywords internal
calc_tmax_coh <- function(deltaT_do, deltaT_di, diffusivity, probe_spacing, pre_pulse, peak_info) {

  # Validate inputs
  if (!is.numeric(deltaT_do) || !is.numeric(deltaT_di)) {
    stop("Temperature vectors must be numeric")
  }

  # Remove NA values for finding maximum
  valid_do <- deltaT_do[!is.na(deltaT_do)]
  valid_di <- deltaT_di[!is.na(deltaT_di)]

  if (length(valid_do) == 0 || length(valid_di) == 0) {
    return(list(outer = NA_real_, inner = NA_real_,
                issue = "no_valid_data"))
  }

  # Use pre-computed peak times (OPTIMISATION: avoids redundant which.max() calls)
  tmo <- peak_info$time_do
  tmi <- peak_info$time_di

  # Check for valid time to maximum
  if (tmo <= 0 || tmi <= 0) {
    return(list(outer = NA_real_, inner = NA_real_,
                issue = "peak_in_prepulse"))
  }

  # Calculate discriminants
  discriminant_outer <- (probe_spacing/100)^2 - 4 * diffusivity/100/100 * tmo
  discriminant_inner <- (probe_spacing/100)^2 - 4 * diffusivity/100/100 * tmi

  # Check for negative discriminants (return silently with flag)
  if (discriminant_outer < 0) {
    Vho_Tmax_Coh <- NA_real_
    outer_issue <- "negative_discriminant"
  } else {
    Vho_Tmax_Coh <- sqrt(discriminant_outer) / tmo * 100 * 3600
    outer_issue <- NA_character_
  }

  if (discriminant_inner < 0) {
    Vhi_Tmax_Coh <- NA_real_
    inner_issue <- "negative_discriminant"
  } else {
    Vhi_Tmax_Coh <- sqrt(discriminant_inner) / tmi * 100 * 3600
    inner_issue <- NA_character_
  }

  return(list(
    outer = Vho_Tmax_Coh,
    inner = Vhi_Tmax_Coh,
    outer_issue = outer_issue,
    inner_issue = inner_issue,
    window_start_outer = NA_real_,
    window_end_outer = NA_real_,
    window_start_inner = NA_real_,
    window_end_inner = NA_real_,
    calc_time_outer = tmo,
    calc_time_inner = tmi
  ))
}

#' Calculate Tmax Kluitenberg velocities
#' @param peak_info Pre-computed peak information from calc_vh_single_pulse
#' @keywords internal
calc_tmax_klu <- function(deltaT_do, deltaT_di, diffusivity, probe_spacing, tp_1, pre_pulse, peak_info) {

  # Validate inputs
  if (!is.numeric(deltaT_do) || !is.numeric(deltaT_di)) {
    stop("Temperature vectors must be numeric")
  }

  if (is.null(tp_1) || !is.numeric(tp_1) || tp_1 <= 0) {
    stop("tp_1 (pulse duration) must be a positive number")
  }

  # Remove NA values for finding maximum
  valid_do <- deltaT_do[!is.na(deltaT_do)]
  valid_di <- deltaT_di[!is.na(deltaT_di)]

  if (length(valid_do) == 0 || length(valid_di) == 0) {
    return(list(outer = NA_real_, inner = NA_real_,
                issue = "no_valid_data"))
  }

  # Use pre-computed peak times (OPTIMISATION: avoids redundant which.max() calls)
  tmo <- peak_info$time_do
  tmi <- peak_info$time_di

  # Check if time to max > pulse duration (required for Kluitenberg method)
  if (tmo <= tp_1) {
    Vho_Tmax_Klu <- NA_real_
    outer_issue <- "tmax_too_early"
  } else {
    # Check for valid logarithm argument
    log_arg_outer <- 1 - (tp_1/tmo)
    if (log_arg_outer <= 0) {
      Vho_Tmax_Klu <- NA_real_
      outer_issue <- "invalid_log_arg"
    } else {
      discriminant_outer <- 4 * (diffusivity/100/100/tp_1) * log(log_arg_outer) +
        ((probe_spacing/100)^2) / (tmo * (tmo - tp_1))
      if (discriminant_outer < 0) {
        Vho_Tmax_Klu <- NA_real_
        outer_issue <- "negative_discriminant"
      } else {
        Vho_Tmax_Klu <- sqrt(discriminant_outer) * 100 * 3600
        outer_issue <- NA_character_
      }
    }
  }

  if (tmi <= tp_1) {
    Vhi_Tmax_Klu <- NA_real_
    inner_issue <- "tmax_too_early"
  } else {
    # Check for valid logarithm argument
    log_arg_inner <- 1 - (tp_1/tmi)
    if (log_arg_inner <= 0) {
      Vhi_Tmax_Klu <- NA_real_
      inner_issue <- "invalid_log_arg"
    } else {
      discriminant_inner <- 4 * (diffusivity/100/100)/tp_1 * log(log_arg_inner) +
        ((probe_spacing/100)^2) / (tmi * (tmi - tp_1))
      if (discriminant_inner < 0) {
        Vhi_Tmax_Klu <- NA_real_
        inner_issue <- "negative_discriminant"
      } else {
        Vhi_Tmax_Klu <- sqrt(discriminant_inner) * 100 * 3600
        inner_issue <- NA_character_
      }
    }
  }

  return(list(
    outer = Vho_Tmax_Klu,
    inner = Vhi_Tmax_Klu,
    outer_issue = outer_issue,
    inner_issue = inner_issue,
    window_start_outer = NA_real_,
    window_end_outer = NA_real_,
    window_start_inner = NA_real_,
    window_end_inner = NA_real_,
    calc_time_outer = tmo,
    calc_time_inner = tmi
  ))
}

#' Recalculate Heat Pulse Velocity with New Thermal Diffusivity
#'
#' Efficiently recalculates velocities from existing results using a new thermal
#' diffusivity value, without needing to go back to raw temperature data. This is
#' particularly useful for Phase 3 corrections when estimated k differs from assumed k.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity() containing
#'   the temp_ratio column
#' @param k_new New thermal diffusivity value (cm²/s)
#' @param k_old Original thermal diffusivity value used in initial calculation (cm²/s).
#'   If NULL, attempts to extract from attributes.
#' @param probe_spacing Probe spacing (cm). If NULL, attempts to extract from attributes.
#'
#' @details
#' This function uses stored temperature ratios and times to recalculate velocities:
#'
#' **For ratio-based methods (HRM, MHR, HRMX):**
#' \code{Vh_new = (k_new / x) × ln(temp_ratio) × 3600}
#'
#' **For Tmax_Coh:**
#' \code{Vh_new = sqrt(x² - 4×k_new×t_max) / t_max × 3600}
#'
#' **For Tmax_Klu:**
#' \code{Vh_new = (x/t_max - sqrt(k_new/t_max)) × 3600}
#'
#' Peclet numbers are automatically recalculated for HRM results.
#'
#' @return A vh_results tibble with recalculated velocities and Peclet numbers
#'
#' @examples
#' \dontrun{
#' # Initial calculation with assumed k
#' vh_phase1 <- calc_heat_pulse_velocity(hpd, diffusivity = 0.0025)
#'
#' # Phase 2: estimate actual k
#' k_result <- estimate_k_from_tmax(hpd, zero_periods, ...)
#'
#' # Phase 3: efficiently recalculate with new k
#' vh_phase3 <- recalc_vh_with_k(
#'   vh_results = vh_phase1,
#'   k_new = k_result$k_mean,
#'   k_old = 0.0025,
#'   probe_spacing = 0.5
#' )
#' }
#'
#' @export
recalc_vh_with_k <- function(vh_results, k_new, k_old = NULL, probe_spacing = NULL) {

  # Validate input
  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object from calc_heat_pulse_velocity()")
  }

  if (!is.numeric(k_new) || k_new <= 0) {
    stop("k_new must be a positive number")
  }

  # Try to extract k_old from attributes if not provided
  if (is.null(k_old)) {
    k_old <- attr(vh_results, "diffusivity")
    if (is.null(k_old)) {
      stop("k_old not provided and could not be extracted from vh_results attributes. ",
           "Please provide k_old parameter.")
    }
  }

  # Try to extract probe_spacing from attributes if not provided
  if (is.null(probe_spacing)) {
    probe_spacing <- attr(vh_results, "probe_spacing")
    if (is.null(probe_spacing)) {
      stop("probe_spacing not provided and could not be extracted from vh_results attributes. ",
           "Please provide probe_spacing parameter.")
    }
  }

  # Check for required columns
  if (!"temp_ratio" %in% names(vh_results)) {
    stop("vh_results missing 'temp_ratio' column. ",
         "Ensure you're using results from calc_heat_pulse_velocity() version that includes temp_ratio.")
  }

  if (!"method" %in% names(vh_results)) {
    stop("vh_results missing 'method' column")
  }

  # Create copy for modification
  vh_new <- vh_results

  # Recalculate based on method
  for (i in seq_len(nrow(vh_new))) {
    method <- vh_new$method[i]

    if (method %in% c("HRM", "MHR", "HRMXa", "HRMXb")) {
      # Ratio-based methods: Vh = (k/x) × ln(ratio) × 3600
      if (!is.na(vh_new$temp_ratio[i]) && vh_new$temp_ratio[i] > 0) {
        vh_new$Vh_cm_hr[i] <- (k_new / probe_spacing) * log(vh_new$temp_ratio[i]) * 3600

        # Recalculate Peclet number for HRM
        if (method == "HRM" && !is.na(vh_new$Vh_cm_hr[i])) {
          vh_new$peclet_number[i] <- (vh_new$Vh_cm_hr[i] * probe_spacing) / (k_new * 3600)
        }
      }

    } else if (method == "Tmax_Coh") {
      # Tmax_Coh: Vh = sqrt(x² - 4×k×t_max) / t_max × 3600
      if (!is.na(vh_new$calc_time_sec[i]) && vh_new$calc_time_sec[i] > 0) {
        t_max <- vh_new$calc_time_sec[i]
        discriminant <- probe_spacing^2 - 4 * k_new * t_max

        if (discriminant >= 0) {
          vh_new$Vh_cm_hr[i] <- sqrt(discriminant) / t_max * 3600
        } else {
          vh_new$Vh_cm_hr[i] <- NA_real_
        }
      }

    } else if (method == "Tmax_Klu") {
      # Tmax_Klu: Vh = (x/t_max - sqrt(k/t_max)) × 3600
      # Note: Full Tmax_Klu formula is complex - simplified here
      # For full accuracy, may need tp_1 parameter
      if (!is.na(vh_new$calc_time_sec[i]) && vh_new$calc_time_sec[i] > 0) {
        t_max <- vh_new$calc_time_sec[i]
        vh_new$Vh_cm_hr[i] <- (probe_spacing / t_max - sqrt(k_new / t_max)) * 3600
      }
    }
  }

  # Update attributes
  attr(vh_new, "diffusivity") <- k_new
  attr(vh_new, "recalculated") <- TRUE
  attr(vh_new, "original_k") <- k_old
  attr(vh_new, "k_change_percent") <- ((k_new - k_old) / k_old) * 100

  message(sprintf("Recalculated velocities with k = %.4f cm²/s (%.1f%% change from k = %.4f)",
                 k_new, ((k_new - k_old) / k_old) * 100, k_old))

  return(vh_new)
}

# ==============================================================================
# sDMA (Selectable Dual Method Approach) - REMOVED
#
# sDMA functionality has been temporarily extracted from the active workflow
# and parked in R/04j_sdma_methods.R for future implementation.
#
# FUTURE POSITION: Between wound correction (Step 4) and flux density (Step 5)
# WILL APPLY TO: Corrected velocities (Vc) after spacing and wound corrections
#
# The code is fully functional and preserved. It will be re-integrated after:
# - Quality control implementation
# - Wound correction implementation
# - Gap filling / interpolation
#
# See: R/04j_sdma_methods.R for the complete parked implementation
# See: SDMA_EXTRACTION_STATUS.md for extraction status and plan
# ==============================================================================

#' Add Quality Flags to Results
#'
#' @param results Data frame with velocity results
#' @return Data frame with added quality_flag column
#' @keywords internal
add_quality_flags <- function(results) {
  results$quality_flag <- "OK"

  # Flag calculation issues (these indicate the calculation itself had problems)
  # Using CALC_ prefix to distinguish from data quality issues added later
  results$quality_flag[is.infinite(results$Vh_cm_hr)] <- "CALC_INFINITE"
  results$quality_flag[is.na(results$Vh_cm_hr)] <- "CALC_FAILED"
  results$quality_flag[abs(results$Vh_cm_hr) > 200] <- "CALC_EXTREME"
  results$quality_flag[results$Vh_cm_hr < -50] <- "CALC_EXTREME"

  # Flag data quality issues (only for values not already flagged)
  # DATA_ILLOGICAL: exceeds hard maximum threshold (physically impossible)
  ok_indices <- which(results$quality_flag == "OK")
  illogical_indices <- ok_indices[abs(results$Vh_cm_hr[ok_indices]) > 500]
  if (length(illogical_indices) > 0) {
    results$quality_flag[illogical_indices] <- "DATA_ILLOGICAL"
  }

  return(results)
}
#' Get Default Calculation Parameters
#'
#' Returns a list of default calculation parameters for heat pulse velocity calculations.
#'
#' @return A named list containing default parameter values for HPV calculations
#'
#' @details
#' Default parameters are:
#' \describe{
#'   \item{diffusivity}{Thermal diffusivity of sapwood (0.0025 cm²/s)}
#'   \item{probe_spacing}{Distance from heat source (0.5 cm)}
#'   \item{L}{Lower proportion of deltaTmax for HRMX sampling window (0.5)}
#'   \item{H}{Higher proportion of deltaTmax for HRMX sampling window (0.8)}
#'   \item{tp_1}{Heat pulse duration for Tmax_Klu (2 sec)}
#'   \item{HRM_start}{Start of sampling window for HRM (60 sec after pulse)}
#'   \item{HRM_end}{End of sampling window for HRM (100 sec after pulse)}
#'   \item{pre_pulse}{Pre-pulse period (30 sec)}
#' }
#'
#' @export
get_default_parameters <- function() {
  list(
    diffusivity = 0.0025,  # Thermal diffusivity of sapwood (cm²/s)
    probe_spacing = 0.5,   # Distance from heat source (cm)
    L = 0.5,               # Lower proportion of deltaTmax for HRMX sampling window
    H = 0.8,               # Higher proportion of deltaTmax for HRMX sampling window
    tp_1 = 2,              # Heat pulse duration (sec) for Tmax_Klu
    HRM_start = 60,        # Start of sampling window for HRM (sec after pulse)
    HRM_end = 100,         # End of sampling window for HRM (sec after pulse)
    pre_pulse = 30         # Pre-pulse period (sec)
  )
}


#' Validate Calculation Parameters
#'
#' Validates that calculation parameters are appropriate for heat pulse velocity calculations.
#'
#' @param parameters A list of calculation parameters
#'
#' @return A list containing validation results
#'
#' @details
#' Checks that all required parameters are present and within reasonable ranges.
#'
#' @export
validate_parameters <- function(parameters) {

  required_params <- c("diffusivity", "probe_spacing", "pre_pulse", "HRM_start", "HRM_end")
  missing_params <- setdiff(required_params, names(parameters))

  if (length(missing_params) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required parameters:", paste(missing_params, collapse = ", "))
    ))
  }

  # Check parameter ranges
  checks <- list()

  if (parameters$diffusivity <= 0 || parameters$diffusivity > 0.01) {
    checks <- append(checks, "diffusivity should be between 0 and 0.01 cm²/s")
  }

  if (parameters$probe_spacing <= 0 || parameters$probe_spacing > 2) {
    checks <- append(checks, "probe_spacing should be between 0 and 2 cm")
  }

  if (parameters$pre_pulse <= 0 || parameters$pre_pulse > 60) {
    checks <- append(checks, "pre_pulse should be between 0 and 60 seconds")
  }

  if (parameters$HRM_start <= parameters$pre_pulse) {
    checks <- append(checks, "HRM_start should be greater than pre_pulse period")
  }

  if (parameters$HRM_end <= parameters$HRM_start) {
    checks <- append(checks, "HRM_end should be greater than HRM_start")
  }

  if ("tp_1" %in% names(parameters)) {
    if (parameters$tp_1 <= 0 || parameters$tp_1 > 10) {
      checks <- append(checks, "tp_1 (pulse duration) should be between 0 and 10 seconds")
    }
  }

  if (length(checks) == 0) {
    return(list(valid = TRUE, message = "All parameters are valid"))
  } else {
    return(list(
      valid = FALSE,
      message = paste("Parameter validation issues:", paste(checks, collapse = "; "))
    ))
  }
}

