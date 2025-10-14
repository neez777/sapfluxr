#' Calculate Heat Pulse Velocity
#'
#' Calculates heat pulse velocity (Vh) from sap flow temperature data using multiple methods
#' including Heat Ratio Method (HRM), Maximum Heat Ratio (MHR), T-max methods, and others.
#'
#' Progress reporting works through the \code{progressr} package. Wrap calls in
#' \code{progressr::with_progress({})} to see progress bars. Works in both console and Shiny.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param pulse_ids Vector of pulse IDs to process. If NULL, processes all pulses.
#' @param methods Character vector of methods to use. Options: "HRM", "MHR", "HRMXa", "HRMXb",
#'   "Tmax_Coh", "Tmax_Klu", "DMA". Default: c("HRM", "MHR", "DMA")
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
#' @param plot_results Logical indicating whether to generate diagnostic plots
#' @param show_progress Logical indicating whether to report progress (default: TRUE)
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
#'   \item{method}{Calculation method used}
#'   \item{sensor_position}{Inner or outer sensor position}
#'   \item{Vh_cm_hr}{Heat pulse velocity in cm/hr}
#'   \item{quality_flag}{Data quality indicator}
#'
#' @examples
#' \dontrun{
#' # Load data and calculate velocities with progress bars
#' sap_data <- read_sap_data("data.txt")
#' progressr::with_progress({
#'   results <- calc_heat_pulse_velocity(sap_data)
#' })
#'
#' # Use specific methods and parameters
#' params <- list(diffusivity = 0.003, probe_spacing = 0.6)
#' progressr::with_progress({
#'   results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR"),
#'                                      parameters = params)
#' })
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
calc_heat_pulse_velocity <- function(sap_data,
                                     pulse_ids = NULL,
                                     methods = c("HRM", "MHR", "DMA"),
                                     probe_config = NULL,
                                     wood_properties = NULL,
                                     parameters = NULL,
                                     diffusivity = NULL,
                                     probe_spacing = NULL,
                                     probe_corrections = NULL,
                                     plot_results = FALSE,
                                     show_progress = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("Input must be a sap_data object from read_sap_data()")
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
  measurements <- sap_data$measurements
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

  # OPTIMISATION: Pre-split measurements by pulse_id once (massive speedup!)
  # This avoids scanning entire dataset for each pulse
  measurements_by_pulse <- split(measurements, measurements$pulse_id)

  # Process each pulse with progress reporting
  all_results <- list()
  successful_pulses <- 0
  n_pulses <- length(pulse_ids)

  # Create progress reporter
  p <- if (show_progress && n_pulses > 0) {
    progressr::progressor(steps = n_pulses)
  } else {
    NULL
  }

  # Determine update frequency for progress reporting
  update_interval <- if (n_pulses < 50) {
    1   # Update every pulse for small datasets
  } else if (n_pulses < 500) {
    5   # Update every 5 pulses
  } else if (n_pulses < 2000) {
    20  # Update every 20 pulses
  } else {
    50  # Update every 50 pulses for large datasets
  }

  last_reported <- 0  # Track last reported position for accurate progress updates

  for (i in seq_along(pulse_ids)) {
    pid <- pulse_ids[i]

    tryCatch({
      pulse_result <- calc_vh_single_pulse(measurements_by_pulse[[as.character(pid)]],
                                            pid, params, methods, plot_results)
      all_results[[i]] <- pulse_result
      successful_pulses <- successful_pulses + 1

      # Update progress
      if (show_progress && !is.null(p) && (i %% update_interval == 0 || i == n_pulses)) {
        # Calculate actual amount processed since last update
        amount_to_report <- i - last_reported
        p(amount = amount_to_report,
          message = sprintf("Processing pulse %s / %s (%.0f%% complete, %s methods)",
                           format(i, big.mark = ","),
                           format(n_pulses, big.mark = ","),
                           100 * i / n_pulses,
                           paste(methods, collapse = ", ")))
        last_reported <- i
      }

    }, error = function(e) {
      message("Error processing pulse ", pid, ": ", e$message)

      # Still update progress even on error
      if (show_progress && !is.null(p) && (i %% update_interval == 0 || i == n_pulses)) {
        # Calculate actual amount processed since last update
        amount_to_report <- i - last_reported
        p(amount = amount_to_report,
          message = sprintf("Processing pulse %s / %s (%.0f%% complete)",
                           format(i, big.mark = ","),
                           format(n_pulses, big.mark = ","),
                           100 * i / n_pulses))
        last_reported <- i
      }
    })
  }

  if (successful_pulses == 0) {
    stop("No pulses were successfully processed")
  }

  # Combine results
  combined_results <- dplyr::bind_rows(all_results)

  # Check for T-max method failures and warn once with summary
  if ("Tmax_Coh" %in% methods) {
    tmax_coh_results <- combined_results[combined_results$method == "Tmax_Coh", ]
    na_count <- sum(is.na(tmax_coh_results$Vh_cm_hr))
    total_count <- nrow(tmax_coh_results)
    if (na_count > 0) {
      na_pct <- round(100 * na_count / total_count, 1)
      if (na_pct > 50) {
        message(sprintf("Tmax_Coh: %d/%d (%.1f%%) calculations failed (negative discriminant). ",
                       na_count, total_count, na_pct),
                "This method requires high flow velocities (>50 cm/hr). ",
                "Consider using HRM or MHR for low-moderate flows.")
      }
    }
  }

  if ("Tmax_Klu" %in% methods) {
    tmax_klu_results <- combined_results[combined_results$method == "Tmax_Klu", ]
    na_count <- sum(is.na(tmax_klu_results$Vh_cm_hr))
    total_count <- nrow(tmax_klu_results)
    if (na_count > 0) {
      na_pct <- round(100 * na_count / total_count, 1)
      if (na_pct > 50) {
        message(sprintf("Tmax_Klu: %d/%d (%.1f%%) calculations failed. ",
                       na_count, total_count, na_pct),
                "This method requires high flow velocities (>50 cm/hr). ",
                "Consider using HRM or MHR for low-moderate flows.")
      }
    }
  }

  # Add quality flags
  combined_results <- add_quality_flags(combined_results)

  # Check if probe corrections should be applied
  if (!is.null(probe_corrections)) {
    # Attach correction metadata
    attr(combined_results, "probe_corrections_available") <- TRUE
    attr(combined_results, "correction_metadata") <- attr(probe_corrections, "correction_summary")
    message("Probe correction metadata attached. Apply corrections using apply_hpv_corrections().")
  } else {
    # Warn about missing corrections
    if (!isTRUE(getOption("sapFluxR.suppress_correction_warnings"))) {
      message("\nNote: Probe corrections not applied. For accurate results, consider:\n",
              "  1. Collecting zero-flow calibration data\n",
              "  2. Measuring wood properties from cores\n",
              "  3. Estimating wound diameter\n",
              "  4. Using apply_hpv_corrections() before flux calculations\n",
              "Set options(sapFluxR.suppress_correction_warnings = TRUE) to suppress this message.")
    }
  }

  # CRITICAL: Add vh_results class
  class(combined_results) <- c("vh_results", class(combined_results))

  return(combined_results)
}


#' Calculate velocity for single pulse
#'
#' @param pulse_data Pre-filtered data frame for this pulse
#' @param pulse_id ID of pulse to process
#' @param parameters List of parameters
#' @param methods Character vector of methods
#' @param plot_results Whether to plot results
#' @return Data frame with results for this pulse
#' @keywords internal
calc_vh_single_pulse <- function(pulse_data, pulse_id, parameters, methods, plot_results = FALSE) {

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
  delatT_do <- pulse_data$do - do_mu_pre
  delatT_do[pre_pulse_period] <- NA
  delatT_di <- pulse_data$di - di_mu_pre
  delatT_di[pre_pulse_period] <- NA
  delatT_uo <- pulse_data$uo - uo_mu_pre
  delatT_uo[pre_pulse_period] <- NA
  delatT_ui <- pulse_data$ui - ui_mu_pre
  delatT_ui[pre_pulse_period] <- NA

  # Calculate temperature ratios
  dTratio_douo <- delatT_do / delatT_uo
  dTratio_diui <- delatT_di / delatT_ui

  # Initialize results
  method_results <- list()

  # Heat Ratio Method (HRM)
  if ("HRM" %in% methods) {
    hrm_results <- calc_hrm(dTratio_douo, dTratio_diui, HRM_period, diffusivity, probe_spacing)
    method_results[["HRM"]] <- hrm_results
  }

  # Maximum Heat Ratio (MHR)
  if ("MHR" %in% methods) {
    mhr_results <- calc_mhr(delatT_do, delatT_di, delatT_uo, delatT_ui, diffusivity, probe_spacing)
    method_results[["MHR"]] <- mhr_results
  }

  # HRMX methods
  if ("HRMXa" %in% methods || "HRMXb" %in% methods) {
    hrmx_results <- calc_hrmx(delatT_do, delatT_di, delatT_uo, delatT_ui,
                              dTratio_douo, dTratio_diui, L, H, diffusivity, probe_spacing)
    if ("HRMXa" %in% methods) method_results[["HRMXa"]] <- hrmx_results$HRMXa
    if ("HRMXb" %in% methods) method_results[["HRMXb"]] <- hrmx_results$HRMXb
  }

  # T-max methods
  if ("Tmax_Coh" %in% methods) {
    tmax_coh_results <- calc_tmax_coh(delatT_do, delatT_di, diffusivity, probe_spacing, pre_pulse)
    method_results[["Tmax_Coh"]] <- tmax_coh_results
  }

  if ("Tmax_Klu" %in% methods) {
    tmax_klu_results <- calc_tmax_klu(delatT_do, delatT_di, diffusivity, probe_spacing, tp_1, pre_pulse)
    method_results[["Tmax_Klu"]] <- tmax_klu_results
  }

  # Dual Method Approach (DMA)
  if ("DMA" %in% methods) {
    # Requires both HRM and Tmax_Klu
    if (!"HRM" %in% method_results) {
      hrm_results <- calc_hrm(dTratio_douo, dTratio_diui, HRM_period, diffusivity, probe_spacing)
      method_results[["HRM"]] <- hrm_results
    }
    if (!"Tmax_Klu" %in% method_results) {
      tmax_klu_results <- calc_tmax_klu(delatT_do, delatT_di, diffusivity, probe_spacing, tp_1, pre_pulse)
      method_results[["Tmax_Klu"]] <- tmax_klu_results
    }

    dma_results <- calc_dma(method_results[["HRM"]], method_results[["Tmax_Klu"]], diffusivity, probe_spacing)
    method_results[["DMA"]] <- dma_results
  }

  # Create output data frame
  datetime_pulse <- pulse_data$datetime[1]
  result_rows <- list()

  for (method_name in names(method_results)) {
    method_result <- method_results[[method_name]]
    result_rows[[paste0(method_name, "_outer")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "outer",
      Vh_cm_hr = method_result$outer,
      stringsAsFactors = FALSE
    )
    result_rows[[paste0(method_name, "_inner")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "inner",
      Vh_cm_hr = method_result$inner,
      stringsAsFactors = FALSE
    )
  }

  result_df <- dplyr::bind_rows(result_rows)

  # Plot if requested
  if (plot_results) {
    plot_pulse_temps(pulse_data, delatT_do, delatT_di, delatT_uo, delatT_ui, pulse_id)
  }

  return(result_df)
}

# Method-specific calculation functions

#' Calculate HRM velocities
#' @keywords internal
calc_hrm <- function(dTratio_douo, dTratio_diui, HRM_period, diffusivity, probe_spacing) {

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
    return(list(outer = NA_real_, inner = NA_real_))
  }

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

  return(list(outer = Vho_HRM, inner = Vhi_HRM))
}

#' Calculate MHR velocities
#' @keywords internal
calc_mhr <- function(delatT_do, delatT_di, delatT_uo, delatT_ui, diffusivity, probe_spacing) {

  # Input validation
  if (all(is.na(delatT_do)) || all(is.na(delatT_di)) ||
      all(is.na(delatT_uo)) || all(is.na(delatT_ui))) {
    warning("All temperature differences are NA")
    return(list(outer = NA_real_, inner = NA_real_))
  }

  # Find maximum temperature increases
  dTdo_max <- max(delatT_do, na.rm = TRUE)
  dTdi_max <- max(delatT_di, na.rm = TRUE)
  dTuo_max <- max(delatT_uo, na.rm = TRUE)
  dTui_max <- max(delatT_ui, na.rm = TRUE)

  # Check for valid maximums
  if (any(c(dTdo_max, dTdi_max, dTuo_max, dTui_max) <= 0) ||
      any(!is.finite(c(dTdo_max, dTdi_max, dTuo_max, dTui_max)))) {
    warning("Invalid maximum temperature increases for MHR")
    return(list(outer = NA_real_, inner = NA_real_))
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

  return(list(outer = Vho_MHR, inner = Vhi_MHR))
}

#' Calculate HRMX velocities
#' @keywords internal
calc_hrmx <- function(delatT_do, delatT_di, delatT_uo, delatT_ui,
                      dTratio_douo, dTratio_diui, L, H, diffusivity, probe_spacing) {

  # Input validation
  if (all(is.na(delatT_do)) || all(is.na(delatT_di)) ||
      all(is.na(delatT_uo)) || all(is.na(delatT_ui))) {
    warning("All temperature differences are NA for HRMX")
    return(list(HRMXa = list(outer = NA_real_, inner = NA_real_),
                HRMXb = list(outer = NA_real_, inner = NA_real_)))
  }

  # Calculate max temperatures
  dTdo_max <- max(delatT_do, na.rm = TRUE)
  dTdi_max <- max(delatT_di, na.rm = TRUE)
  dTuo_max <- max(delatT_uo, na.rm = TRUE)
  dTui_max <- max(delatT_ui, na.rm = TRUE)

  # Calculate pre-max values
  dTdo_premax <- c(NA, ifelse(diff(delatT_do) > 0, delatT_do[-1], NA))
  dTdi_premax <- c(NA, ifelse(diff(delatT_di) > 0, delatT_di[-1], NA))
  dTuo_premax <- c(NA, ifelse(diff(delatT_uo) > 0, delatT_uo[-1], NA))
  dTui_premax <- c(NA, ifelse(diff(delatT_ui) > 0, delatT_ui[-1], NA))

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

  return(list(
    HRMXa = list(outer = Vho_HRMXa, inner = Vhi_HRMXa),
    HRMXb = list(outer = Vho_HRMXb, inner = Vhi_HRMXb)
  ))
}

#' Calculate Tmax Cohen velocities
#' @keywords internal
calc_tmax_coh <- function(delatT_do, delatT_di, diffusivity, probe_spacing, pre_pulse) {

  # Validate inputs
  if (!is.numeric(delatT_do) || !is.numeric(delatT_di)) {
    stop("Temperature vectors must be numeric")
  }

  # Remove NA values for finding maximum
  valid_do <- delatT_do[!is.na(delatT_do)]
  valid_di <- delatT_di[!is.na(delatT_di)]

  if (length(valid_do) == 0 || length(valid_di) == 0) {
    return(list(outer = NA_real_, inner = NA_real_,
                issue = "no_valid_data"))
  }

  # Find time to maximum (adjust for pre-pulse period)
  tmo <- which.max(delatT_do) - pre_pulse
  tmi <- which.max(delatT_di) - pre_pulse

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

  return(list(outer = Vho_Tmax_Coh, inner = Vhi_Tmax_Coh,
              outer_issue = outer_issue, inner_issue = inner_issue))
}

#' Calculate Tmax Kluitenberg velocities
#' @keywords internal
calc_tmax_klu <- function(delatT_do, delatT_di, diffusivity, probe_spacing, tp_1, pre_pulse) {

  # Validate inputs
  if (!is.numeric(delatT_do) || !is.numeric(delatT_di)) {
    stop("Temperature vectors must be numeric")
  }

  if (is.null(tp_1) || !is.numeric(tp_1) || tp_1 <= 0) {
    stop("tp_1 (pulse duration) must be a positive number")
  }

  # Remove NA values for finding maximum
  valid_do <- delatT_do[!is.na(delatT_do)]
  valid_di <- delatT_di[!is.na(delatT_di)]

  if (length(valid_do) == 0 || length(valid_di) == 0) {
    return(list(outer = NA_real_, inner = NA_real_,
                issue = "no_valid_data"))
  }

  # Find time to maximum (adjust for pre-pulse period)
  tmo <- which.max(delatT_do) - pre_pulse
  tmi <- which.max(delatT_di) - pre_pulse

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

  return(list(outer = Vho_Tmax_Klu, inner = Vhi_Tmax_Klu,
              outer_issue = outer_issue, inner_issue = inner_issue))
}

#' Calculate DMA velocities
#' @keywords internal
calc_dma <- function(hrm_results, tmax_klu_results, diffusivity, probe_spacing) {
  Vh_HRM_crit <- diffusivity / probe_spacing * 3600

  # Handle outer sensor with proper NA checking
  if (is.na(hrm_results$outer) || !is.finite(hrm_results$outer)) {
    Vho_DMA <- tmax_klu_results$outer
  } else if (hrm_results$outer < Vh_HRM_crit) {
    Vho_DMA <- hrm_results$outer
  } else {
    Vho_DMA <- tmax_klu_results$outer
  }

  # Handle inner sensor with proper NA checking
  if (is.na(hrm_results$inner) || !is.finite(hrm_results$inner)) {
    Vhi_DMA <- tmax_klu_results$inner
  } else if (hrm_results$inner < Vh_HRM_crit) {
    Vhi_DMA <- hrm_results$inner
  } else {
    Vhi_DMA <- tmax_klu_results$inner
  }

  return(list(outer = Vho_DMA, inner = Vhi_DMA))
}

#' Add Quality Flags to Results
#'
#' @param results Data frame with velocity results
#' @return Data frame with added quality_flag column
#' @keywords internal
add_quality_flags <- function(results) {
  results$quality_flag <- "OK"

  # Flag extreme values
  results$quality_flag[abs(results$Vh_cm_hr) > 200] <- "HIGH_VELOCITY"
  results$quality_flag[is.infinite(results$Vh_cm_hr)] <- "INFINITE"
  results$quality_flag[is.na(results$Vh_cm_hr)] <- "MISSING"
  results$quality_flag[results$Vh_cm_hr < -50] <- "NEGATIVE_FLOW"

  return(results)
}

#' Plot Temperature Differences for Diagnostic Purposes
#'
#' @param pulse_data Data for single pulse
#' @param delatT_do Delta temperatures for downstream outer
#' @param delatT_di Delta temperatures for downstream inner
#' @param delatT_uo Delta temperatures for upstream outer
#' @param delatT_ui Delta temperatures for upstream inner
#' @param pulse_id Pulse ID for title
#' @keywords internal
plot_pulse_temps <- function(pulse_data, delatT_do, delatT_di, delatT_uo, delatT_ui, pulse_id) {
  if (!requireNamespace("graphics", quietly = TRUE)) {
    return()
  }

  graphics::plot(pulse_data$datetime, delatT_do, type = "l", las = 1,
                 xlab = "Time", ylab = expression(paste(Delta, 'T')),
                 main = paste("Pulse", pulse_id),
                 xlim = range(pulse_data$datetime, na.rm = TRUE),
                 ylim = c(0, max(delatT_do, delatT_uo, delatT_di, delatT_ui, na.rm = TRUE)))
  graphics::lines(pulse_data$datetime, delatT_uo, lty = 2)
  graphics::lines(pulse_data$datetime, delatT_di, lty = 3)
  graphics::lines(pulse_data$datetime, delatT_ui, lty = 4)
  graphics::legend("topleft", lty = c(1, 2, 3, 4), legend = c("DO", "UO", "DI", "UI"), bty = "n")
}#' Get Default Calculation Parameters
#'
#' Returns a list of default parameters for heat pulse velocity calculations.
#'
#' @return A list containing default calculation parameters
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


#' Create Test Sap Data Object
#'
#' Creates a properly formatted sap_data object for testing purposes.
#'
#' @param n_points Number of temperature measurements per pulse
#' @param n_pulses Number of pulses to simulate
#' @param add_noise Whether to add random noise to temperature readings
#'
#' @return A sap_data object compatible with package functions
#'
#' @keywords internal
create_test_sap_data <- function(n_points = 120, n_pulses = 1, add_noise = TRUE) {

  # Create test data for multiple pulses
  all_measurements <- list()
  all_diagnostics <- list()

  for (pulse_num in 1:n_pulses) {
    # Simulate a heat pulse response
    time <- 1:n_points

    # Pre-pulse temperatures (stable)
    baseline_do <- 18.8
    baseline_di <- 18.6
    baseline_uo <- 18.9
    baseline_ui <- 18.7

    # Simulate heat pulse response (starts at point 31)
    pulse_start <- 31

    # Temperature responses
    do <- rep(baseline_do, n_points)
    di <- rep(baseline_di, n_points)
    uo <- rep(baseline_uo, n_points)
    ui <- rep(baseline_ui, n_points)

    # Add heat pulse response
    for (i in pulse_start:n_points) {
      t_after_pulse <- i - pulse_start
      if (t_after_pulse > 0) {
        # Downstream sensors (higher response)
        do[i] <- baseline_do + 1.5 * exp(-0.02 * t_after_pulse) * (1 - exp(-0.1 * t_after_pulse))
        di[i] <- baseline_di + 1.2 * exp(-0.02 * t_after_pulse) * (1 - exp(-0.1 * t_after_pulse))

        # Upstream sensors (delayed, lower response)
        if (t_after_pulse > 5) {
          uo[i] <- baseline_uo + 0.8 * exp(-0.015 * (t_after_pulse - 5)) * (1 - exp(-0.08 * (t_after_pulse - 5)))
          ui[i] <- baseline_ui + 0.6 * exp(-0.015 * (t_after_pulse - 5)) * (1 - exp(-0.08 * (t_after_pulse - 5)))
        }
      }
    }

    # Add noise if requested
    if (add_noise) {
      noise_level <- 0.01
      do <- do + rnorm(n_points, 0, noise_level)
      di <- di + rnorm(n_points, 0, noise_level)
      uo <- uo + rnorm(n_points, 0, noise_level)
      ui <- ui + rnorm(n_points, 0, noise_level)
    }

    # Create timestamp
    start_time <- as.POSIXct("2024-01-01 10:00:00") + (pulse_num - 1) * 1800  # 30 min apart
    datetime <- start_time + time

    # Create measurements data frame
    measurements <- data.frame(
      pulse_id = pulse_num,
      datetime = datetime,
      do = do,
      di = di,
      uo = uo,
      ui = ui
    )

    # Create diagnostics data frame
    diagnostics <- data.frame(
      pulse_id = pulse_num,
      batt_volt = 4.1,
      batt_current = 15.0,
      batt_temp = 25.0,
      external_volt = 23.0,
      external_current = 50.0
    )

    all_measurements[[pulse_num]] <- measurements
    all_diagnostics[[pulse_num]] <- diagnostics
  }

  # Combine all data
  combined_measurements <- do.call(rbind, all_measurements)
  combined_diagnostics <- do.call(rbind, all_diagnostics)

  # Create sap_data object structure
  sap_data <- list(
    measurements = combined_measurements,
    diagnostics = combined_diagnostics,
    metadata = list(
      format = "test_format",
      file_path = "test_data.txt",
      import_time = Sys.time(),
      n_pulses = n_pulses
    ),
    validation = list(
      valid = TRUE,
      issues = character(0)
    )
  )

  class(sap_data) <- "sap_data"
  return(sap_data)
}