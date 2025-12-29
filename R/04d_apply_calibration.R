#' Apply Method Calibration to Create Unified Dataset
#'
#' Creates a calibrated velocity dataset by applying method-specific thresholds
#' to switch between HRM and secondary methods. Results in one row per pulse/sensor
#' with the calibrated velocity and recalculated Peclet number.
#'
#' @param vh_corrected Data frame with corrected velocities (all methods)
#' @param thresholds_outer Named list of thresholds for outer sensor (e.g., list(MHR = 15.2, HRMXa = 12.8))
#' @param thresholds_inner Named list of thresholds for inner sensor
#' @param probe_config Probe configuration (for Peclet number calculation)
#' @param wood_properties Wood properties (for Peclet number calculation)
#' @param velocity_col Column name containing velocity values (default: "Vh_cm_hr")
#'
#' @return Tibble with calibrated velocities, one row per pulse/sensor:
#'   \itemize{
#'     \item datetime - Timestamp
#'     \item pulse_id - Pulse identifier
#'     \item sensor_position - "outer" or "inner"
#'     \item Vh_calibrated - Calibrated velocity (cm/hr)
#'     \item method_used - Which method was used ("HRM", "MHR", etc.)
#'     \item peclet_number - Recalculated Peclet number
#'     \item quality_flag - Quality assessment
#'   }
#'
#' @details
#' For each pulse and sensor position:
#' \enumerate{
#'   \item Start with HRM velocity
#'   \item Check if HRM >= threshold for any secondary method
#'   \item If yes, use the secondary method with the lowest threshold that HRM exceeds
#'   \item Recalculate Peclet number based on calibrated velocity
#' }
#'
#' @family calibration functions
#' @export
apply_method_calibration <- function(vh_corrected,
                                     thresholds_outer,
                                     thresholds_inner,
                                     probe_config = NULL,
                                     wood_properties = NULL,
                                     velocity_col = "Vh_cm_hr") {

  # Validate inputs
  if (!velocity_col %in% names(vh_corrected)) {
    stop(sprintf("Velocity column '%s' not found in data", velocity_col))
  }

  # Ensure we have required columns
  required_cols <- c("datetime", "pulse_id", "sensor_position", "method", velocity_col)
  missing_cols <- setdiff(required_cols, names(vh_corrected))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("APPLYING METHOD CALIBRATION (VECTORIZED)\n")
  cat(strrep("=", 72), "\n")
  cat("Total pulses:", length(unique(vh_corrected$pulse_id)), "\n")
  cat("Sensors:", paste(unique(vh_corrected$sensor_position), collapse = ", "), "\n")
  cat("\n")

  # =========================================================================
  # VECTORIZED APPROACH: Pivot methods to columns
  # =========================================================================

  # Pivot so each method becomes a column
  calibrated_data <- vh_corrected %>%
    dplyr::select(datetime, pulse_id, sensor_position, method, !!velocity_col) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = !!velocity_col,
      names_prefix = "vel_"
    )

  # =========================================================================
  # FULLY VECTORIZED: Use case_when to select method based on thresholds
  # =========================================================================
  # Instead of rowwise(), we build a series of case_when conditions based on
  # the thresholds provided for each sensor position

  # Get all available method columns
  method_cols <- grep("^vel_", names(calibrated_data), value = TRUE)
  available_methods <- gsub("^vel_", "", method_cols)

  # Build selection logic for outer sensor
  outer_conditions <- list()
  if (!is.null(thresholds_outer) && length(thresholds_outer) > 0) {
    # Sort thresholds by value (ascending) so we check highest threshold first
    thresh_sorted <- sort(thresholds_outer)

    for (method in names(thresh_sorted)) {
      threshold_value <- thresh_sorted[method]
      vel_col <- paste0("vel_", method)

      # Create condition: if HRM >= threshold AND method column exists
      if (vel_col %in% names(calibrated_data)) {
        outer_conditions[[method]] <- list(
          threshold = threshold_value,
          vel_col = vel_col
        )
      }
    }
  }

  # Build selection logic for inner sensor
  inner_conditions <- list()
  if (!is.null(thresholds_inner) && length(thresholds_inner) > 0) {
    thresh_sorted <- sort(thresholds_inner)

    for (method in names(thresh_sorted)) {
      threshold_value <- thresh_sorted[method]
      vel_col <- paste0("vel_", method)

      if (vel_col %in% names(calibrated_data)) {
        inner_conditions[[method]] <- list(
          threshold = threshold_value,
          vel_col = vel_col
        )
      }
    }
  }

  # Apply vectorized selection for outer sensor
  outer_data <- calibrated_data %>%
    dplyr::filter(sensor_position == "outer") %>%
    dplyr::mutate(
      # Use case_when to select method based on thresholds
      method_used = dplyr::case_when(
        # Check each method's threshold in order (highest to lowest)
        !!!purrr::map2(
          names(outer_conditions),
          outer_conditions,
          ~ rlang::expr(!is.na(vel_HRM) & vel_HRM >= !!.y$threshold ~ !!.x)
        ),
        # Default to HRM if no thresholds met
        TRUE ~ "HRM"
      )
    )

  # Apply vectorized selection for inner sensor
  inner_data <- calibrated_data %>%
    dplyr::filter(sensor_position == "inner") %>%
    dplyr::mutate(
      method_used = dplyr::case_when(
        !!!purrr::map2(
          names(inner_conditions),
          inner_conditions,
          ~ rlang::expr(!is.na(vel_HRM) & vel_HRM >= !!.y$threshold ~ !!.x)
        ),
        TRUE ~ "HRM"
      )
    )

  # Combine back together
  calibrated_data <- dplyr::bind_rows(outer_data, inner_data) %>%
    dplyr::arrange(pulse_id, sensor_position)

  # Extract the velocity value from the selected method (VECTORIZED)
  # Create a helper function to extract velocity based on method_used
  calibrated_data <- calibrated_data %>%
    dplyr::mutate(
      Vh_calibrated = dplyr::case_when(
        !!!purrr::map(
          available_methods,
          ~ rlang::expr(method_used == !!.x ~ !!rlang::sym(paste0("vel_", .x)))
        ),
        TRUE ~ vel_HRM  # Default to HRM if method not found
      )
    )

  # Recalculate Peclet numbers (VECTORIZED)
  if (!is.null(probe_config) && !is.null(wood_properties)) {
    tryCatch({
      x <- probe_config$probe_spacing  # cm
      D <- wood_properties$thermal_diffusivity  # cm²/s

      if (!is.null(x) && !is.null(D) && D > 0) {
        calibrated_data <- calibrated_data %>%
          dplyr::mutate(
            peclet_number = (Vh_calibrated / 3600 * x) / D
          )
      } else {
        calibrated_data$peclet_number <- NA_real_
      }
    }, error = function(e) {
      calibrated_data$peclet_number <- NA_real_
    })
  } else {
    calibrated_data$peclet_number <- NA_real_
  }

  # Add quality flag
  calibrated_data <- calibrated_data %>%
    dplyr::mutate(
      quality_flag = dplyr::if_else(method_used != "HRM", "CALIBRATED", "HRM_ONLY")
    )

  # Select final columns
  calibrated_data <- calibrated_data %>%
    dplyr::select(datetime, pulse_id, sensor_position, Vh_calibrated, method_used, peclet_number, quality_flag)

  # Summary
  cat("\n")
  cat("CALIBRATION SUMMARY\n")
  cat(strrep("-", 72), "\n")
  cat("Total calibrated points:", nrow(calibrated_data), "\n")

  for (sensor in unique(calibrated_data$sensor_position)) {
    sensor_data <- calibrated_data %>% dplyr::filter(sensor_position == sensor)
    cat(sprintf("\n%s sensor:\n", toupper(sensor)))

    method_counts <- table(sensor_data$method_used)
    for (method in names(method_counts)) {
      pct <- 100 * method_counts[method] / nrow(sensor_data)
      cat(sprintf("  %s: %d points (%.1f%%)\n", method, method_counts[method], pct))
    }
  }

  cat(strrep("=", 72), "\n")
  cat("\n")

  # Return as tibble
  tibble::as_tibble(calibrated_data)
}


#' Recalculate Peclet Numbers Based on Corrected Velocity
#'
#' Recalculates Peclet numbers for velocity data after corrections have been
#' applied (spacing, wound, calibration, etc.). This is essential before applying
#' sDMA switching, as Peclet numbers must reflect the corrected velocities.
#'
#' @param vh_results Data frame with velocity results (from calc_heat_pulse_velocity()
#'   or after corrections)
#' @param probe_config Probe configuration object or path to YAML file
#' @param wood_properties Wood properties object or path to YAML file
#' @param velocity_col Column name containing velocity values to use for Peclet
#'   calculation. Default: auto-detect in order of preference: Vc_cm_hr (corrected),
#'   then Vh_cm_hr_sc (spacing corrected), then Vh_cm_hr (raw)
#' @param peclet_col Column name to store Peclet numbers. Default: "peclet_number"
#'
#' @return Data frame with updated Peclet number column
#'
#' @details
#' **Peclet Number Formula:**
#' \deqn{Pe = \frac{V_h \times x}{D \times 3600}}
#'
#' where:
#' \itemize{
#'   \item \eqn{V_h} = sap velocity (cm/hr)
#'   \item \eqn{x} = probe spacing (cm)
#'   \item \eqn{D} = thermal diffusivity (cm²/s)
#'   \item 3600 = conversion factor (seconds to hours)
#' }
#'
#' **When to Use:**
#' \enumerate{
#'   \item After spacing correction (Burgess et al. 2001)
#'   \item After wound correction
#'   \item Before applying sDMA (Selectable Dual Method Approach)
#' }
#'
#' **Why Recalculate?**
#' Spacing and wound corrections modify the velocity values, so Peclet numbers
#' calculated from raw velocities are no longer valid. sDMA switching relies on
#' accurate Peclet numbers to determine which method to use (Pe < 1.0: HRM,
#' Pe >= 1.0: secondary method).
#'
#' @examples
#' \dontrun{
#' # Calculate initial velocities
#' vh <- calc_heat_pulse_velocity(hp_data, methods = c("HRM", "MHR"), wood)
#'
#' # Apply spacing correction
#' vh_corrected <- apply_spacing_correction(vh, ...)
#'
#' # Recalculate Peclet numbers based on corrected velocity
#' vh_corrected <- recalculate_peclet(
#'   vh_corrected,
#'   probe_config = probe,
#'   wood_properties = wood,
#'   velocity_col = "Vc_cm_hr"  # Use corrected velocity
#' )
#'
#' # Now Peclet numbers reflect corrected velocities
#' # Ready for sDMA switching
#' }
#'
#' @family calibration functions
#' @export
recalculate_peclet <- function(vh_results,
                                probe_config,
                                wood_properties,
                                velocity_col = NULL,
                                peclet_col = "peclet_number") {

  # Load configurations if needed
  if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
  }

  if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
  }

  # Validate inputs
  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame")
  }

  # Auto-detect velocity column if not specified
  if (is.null(velocity_col)) {
    velocity_col <- if ("Vc_cm_hr" %in% names(vh_results)) {
      "Vc_cm_hr"  # Corrected velocity (after wound/spacing)
    } else if ("Vh_cm_hr_sc" %in% names(vh_results)) {
      "Vh_cm_hr_sc"  # Spacing corrected velocity
    } else if ("Vh_cm_hr" %in% names(vh_results)) {
      "Vh_cm_hr"  # Raw velocity
    } else {
      stop("No velocity column found. Available columns: ", paste(names(vh_results), collapse = ", "))
    }

    message(sprintf("Auto-detected velocity column: %s", velocity_col))
  }

  # Verify velocity column exists
  if (!velocity_col %in% names(vh_results)) {
    stop(sprintf("Velocity column '%s' not found in data", velocity_col))
  }

  # Extract parameters
  x <- if (inherits(probe_config, "ProbeConfiguration")) {
    probe_config$probe_spacing
  } else if (is.list(probe_config)) {
    probe_config$probe_spacing
  } else {
    stop("Invalid probe_config object")
  }

  D <- if (inherits(wood_properties, "WoodProperties")) {
    wood_properties$thermal_diffusivity
  } else if (is.list(wood_properties)) {
    wood_properties$thermal_diffusivity
  } else {
    stop("Invalid wood_properties object")
  }

  # Validate parameters
  if (is.null(x) || is.na(x) || x <= 0) {
    stop("Invalid probe spacing: ", x)
  }

  if (is.null(D) || is.na(D) || D <= 0) {
    stop("Invalid thermal diffusivity: ", D)
  }

  # Recalculate Peclet numbers (VECTORIZED)
  # Pe = (Vh × x) / (D × 3600)
  # Convert velocity from cm/hr to cm/s, then calculate Pe
  v_cm_s <- vh_results[[velocity_col]] / 3600
  vh_results[[peclet_col]] <- (v_cm_s * x) / D

  cat("\n")
  cat(strrep("=", 72), "\n")
  cat("PECLET NUMBER RECALCULATION\n")
  cat(strrep("=", 72), "\n")
  cat(sprintf("Velocity column used: %s\n", velocity_col))
  cat(sprintf("Probe spacing: %.4f cm\n", x))
  cat(sprintf("Thermal diffusivity: %.6f cm²/s\n", D))
  cat(sprintf("Total points recalculated: %d\n", nrow(vh_results)))
  cat(sprintf("Peclet range: %.3f to %.3f\n",
              min(vh_results[[peclet_col]], na.rm = TRUE),
              max(vh_results[[peclet_col]], na.rm = TRUE)))
  cat(strrep("=", 72), "\n")
  cat("\n")

  return(vh_results)
}
