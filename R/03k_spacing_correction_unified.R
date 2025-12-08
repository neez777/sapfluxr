# R/03k_spacing_correction_unified.R
# Unified Spacing Correction Interface
# Provides a single entry point for all spacing correction methods

#' Apply Spacing Correction (Unified Interface)
#'
#' **Unified interface for all spacing correction methods.** This function provides
#' a single entry point for applying spacing corrections using any of four detection
#' methods: Manual, PELT, VPD-based, or Heartwood continuous reference.
#'
#' @param vh_data Data frame containing velocity data with columns:
#'   \code{datetime}, \code{sensor_position}, \code{method}, \code{Vh_cm_hr}
#' @param method Character string specifying the detection method. One of:
#'   \itemize{
#'     \item \strong{"manual"} - User-specified changepoint dates
#'     \item \strong{"pelt"} - Automatic PELT changepoint detection from daily minima
#'     \item \strong{"vpd"} - Environmental suitability based on low VPD periods
#'     \item \strong{"heartwood"} - Continuous correction using heartwood inner sensor
#'   }
#' @param hpv_method Character, velocity calculation method to correct (default: "HRM").
#'   Burgess spacing correction is only validated for HRM.
#' @param wood_properties Wood properties object or list containing:
#'   \itemize{
#'     \item \code{derived_properties$thermal_diffusivity_actual_cm2_s} - Thermal diffusivity (cm²/s)
#'     \item \code{tree_measurements$sapwood_depth} - Sapwood depth (cm, required for heartwood method)
#'     \item \code{tree_measurements$bark_thickness} - Bark thickness (cm, optional)
#'   }
#' @param probe_config Probe configuration object (required for heartwood method)
#' @param weather_data Weather data frame with \code{datetime} and \code{vpd_kpa} columns
#'   (required for VPD method)
#' @param ... Additional method-specific parameters. See Details.
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list with class \code{"spacing_correction_result"} containing:
#'   \item{vh_corrected}{Data frame with corrected velocities}
#'   \item{method_used}{Character, which detection method was used}
#'   \item{changepoints}{Detected or specified changepoints (Date vector)}
#'   \item{segments}{Data frame describing correction segments}
#'   \item{correction_info}{Method-specific correction information}
#'   \item{metadata}{Metadata about correction parameters}
#'
#' @details
#' ## Method-Specific Parameters
#'
#' ### Manual Method (\code{method = "manual"})
#'
#' **Required:**
#' \itemize{
#'   \item \code{manual_changepoints} - Vector of changepoint dates (Date or character "YYYY-MM-DD")
#' }
#'
#' **Optional:**
#' \itemize{
#'   \item \code{baseline_overrides_outer} - Named list of manual baselines for outer sensor per segment
#'   \item \code{baseline_overrides_inner} - Named list of manual baselines for inner sensor per segment
#' }
#'
#' **Example:**
#' \preformatted{
#' result <- apply_spacing_correction(
#'   vh_data,
#'   method = "manual",
#'   manual_changepoints = c("2024-03-15", "2024-06-10"),
#'   baseline_overrides_outer = list("1" = 0.8, "2" = 1.2)
#' )
#' }
#'
#' ### PELT Method (\code{method = "pelt"})
#'
#' **Automatic changepoint detection from daily velocity minima.**
#'
#' **Optional:**
#' \itemize{
#'   \item \code{sensor_position} - Which sensor to use for detection ("outer" or "inner", default: "outer")
#'   \item \code{penalty} - PELT penalty: "MBIC" (most conservative), "BIC", or numeric value (default: "MBIC")
#'   \item \code{min_segment_days} - Minimum days per segment (default: 7)
#'   \item \code{merge_short_segments} - Whether to merge short segments (default: TRUE)
#' }
#'
#' **Example:**
#' \preformatted{
#' result <- apply_spacing_correction(
#'   vh_data,
#'   method = "pelt",
#'   penalty = "BIC",
#'   min_segment_days = 10
#' )
#' }
#'
#' ### VPD Method (\code{method = "vpd"})
#'
#' **Identifies suitable correction dates based on low VPD (stable environmental conditions).**
#'
#' **Required:**
#' \itemize{
#'   \item \code{weather_data} - Data frame with \code{datetime} and \code{vpd_kpa} columns
#' }
#'
#' **Optional:**
#' \itemize{
#'   \item \code{vpd_threshold} - Maximum VPD for changepoints (kPa, default: 0.5)
#'     \itemize{
#'       \item 0.3 kPa - Very conservative, low VPD only
#'       \item 0.5 kPa - Moderate (recommended)
#'       \item 0.8 kPa - Permissive
#'     }
#'   \item \code{min_segment_days} - Minimum days between changepoints (default: 7)
#'   \item \code{require_consecutive} - Require consecutive low-VPD days (default: FALSE)
#'   \item \code{min_consecutive_days} - If require_consecutive, how many days (default: 3)
#'   \item \code{max_changepoints} - Maximum number of changepoints (default: NULL, no limit)
#' }
#'
#' **Example:**
#' \preformatted{
#' result <- apply_spacing_correction(
#'   vh_data,
#'   method = "vpd",
#'   weather_data = weather,
#'   vpd_threshold = 0.5,
#'   min_segment_days = 10
#' )
#' }
#'
#' ### Heartwood Method (\code{method = "heartwood"})
#'
#' **Continuous correction using inner sensor as heartwood reference (no segmentation).**
#'
#' **Required:**
#' \itemize{
#'   \item \code{wood_properties} - Must contain \code{tree_measurements$sapwood_depth}
#'   \item \code{probe_config} - Probe configuration with geometry information
#' }
#'
#' **Optional:**
#' \itemize{
#'   \item \code{field_of_influence} - Radial extent of heat pulse (cm, default: 1.0)
#' }
#'
#' **Example:**
#' \preformatted{
#' result <- apply_spacing_correction(
#'   vh_data,
#'   method = "heartwood",
#'   wood_properties = wood,
#'   probe_config = probe
#' )
#' }
#'
#' ## Method Comparison
#'
#' | Method | When to Use | Pros | Cons |
#' |--------|-------------|------|------|
#' | **Manual** | Known probe movement events | Full control, custom baselines | Requires prior knowledge |
#' | **PELT** | Unknown baseline shifts | Automatic, statistically rigorous | May detect spurious changes |
#' | **VPD** | Environmentally-driven baseline | Uses stable conditions | Requires weather data |
#' | **Heartwood** | Deep heartwood sensor | Continuous, no segmentation | Requires specific probe depth |
#'
#' ## Scientific Basis
#'
#' ### Spacing Correction (Burgess et al. 2001)
#'
#' All methods apply the Burgess correction equation:
#' \deqn{V_c = a \times V_h + b}
#'
#' Where coefficients \eqn{a} and \eqn{b} depend on the zero-flow offset detected.
#'
#' ### Segmentation vs Continuous
#'
#' - **Segmented** (manual, PELT, VPD): Divides time series into periods with
#'   separate corrections per segment
#' - **Continuous** (heartwood): Single correction using heartwood sensor as
#'   continuous zero reference
#'
#' @examples
#' \dontrun{
#' # ===== MANUAL METHOD =====
#' result_manual <- apply_spacing_correction(
#'   vh_data,
#'   method = "manual",
#'   hpv_method = "HRM",
#'   wood_properties = wood,
#'   manual_changepoints = c("2024-03-15", "2024-06-10")
#' )
#'
#' # ===== PELT METHOD =====
#' result_pelt <- apply_spacing_correction(
#'   vh_data,
#'   method = "pelt",
#'   hpv_method = "HRM",
#'   wood_properties = wood,
#'   penalty = "MBIC",
#'   min_segment_days = 7
#' )
#'
#' # ===== VPD METHOD =====
#' result_vpd <- apply_spacing_correction(
#'   vh_data,
#'   method = "vpd",
#'   hpv_method = "HRM",
#'   wood_properties = wood,
#'   weather_data = weather,
#'   vpd_threshold = 0.5
#' )
#'
#' # ===== HEARTWOOD METHOD =====
#' result_heartwood <- apply_spacing_correction(
#'   vh_data,
#'   method = "heartwood",
#'   hpv_method = "HRM",
#'   wood_properties = wood,
#'   probe_config = probe
#' )
#'
#' # View results
#' print(result_pelt)
#' plot_spacing_correction_comparison(result_pelt, sensor_position = "outer")
#' }
#'
#' @family spacing correction functions
#' @seealso
#' \code{\link{apply_spacing_correction_both_sensors}} for PELT method details,
#' \code{\link{apply_manual_spacing_correction}} for manual method,
#' \code{\link{detect_vpd_changepoints}} for VPD method,
#' \code{\link{check_heartwood_reference_available}} for heartwood method
#'
#' @export
apply_spacing_correction <- function(vh_data,
                                      method = c("pelt", "manual", "vpd", "heartwood"),
                                      hpv_method = "HRM",
                                      wood_properties = NULL,
                                      probe_config = NULL,
                                      weather_data = NULL,
                                      ...,
                                      verbose = TRUE) {

  # Match and validate method
  method <- match.arg(method)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("UNIFIED SPACING CORRECTION\n")
    cat(strrep("=", 72), "\n")
    cat(sprintf("Detection method: %s\n", toupper(method)))
    cat(sprintf("HPV method to correct: %s\n", hpv_method))
    cat("\n")
  }

  # Extract additional parameters
  dots <- list(...)

  # Get thermal diffusivity from wood properties
  k_assumed <- if (!is.null(wood_properties)) {
    if ("derived_properties" %in% names(wood_properties)) {
      wood_properties$derived_properties$thermal_diffusivity_actual_cm2_s
    } else if ("thermal_diffusivity" %in% names(wood_properties)) {
      wood_properties$thermal_diffusivity
    } else {
      0.0025  # Default fallback
    }
  } else {
    0.0025  # Default
  }

  if (verbose) {
    cat(sprintf("Thermal diffusivity: %.6f cm²/s\n", k_assumed))
    cat("\n")
  }

  # Route to appropriate method
  result <- switch(method,

    # ===== MANUAL METHOD =====
    "manual" = {
      if (!"manual_changepoints" %in% names(dots)) {
        stop("Manual method requires 'manual_changepoints' parameter")
      }

      if (verbose) {
        cat("Using MANUAL changepoint specification\n")
        cat(sprintf("  Changepoints provided: %d\n",
                    length(dots$manual_changepoints)))
        cat("\n")
      }

      # Call manual spacing correction
      manual_result <- apply_manual_spacing_correction(
        vh_data = vh_data,
        manual_changepoints = dots$manual_changepoints,
        sensor_position = dots$sensor_position %||% "outer",
        method = hpv_method,
        k_assumed = k_assumed,
        probe_spacing = dots$probe_spacing %||% 0.5,
        measurement_time = dots$measurement_time %||% 80,
        baseline_overrides = dots$baseline_overrides %||% NULL,
        create_new_col = dots$create_new_col %||% FALSE,
        verbose = verbose
      )

      # Convert to unified format
      list(
        vh_corrected = manual_result$vh_corrected,
        method_used = "manual",
        changepoints = dots$manual_changepoints,
        segments = manual_result$segment_results,
        correction_info = manual_result$metadata,
        metadata = list(
          hpv_method = hpv_method,
          k_assumed = k_assumed,
          manual_changepoints = dots$manual_changepoints
        )
      )
    },

    # ===== PELT METHOD =====
    "pelt" = {
      if (verbose) {
        cat("Using PELT automatic changepoint detection\n")
        cat(sprintf("  Penalty: %s\n", dots$penalty %||% "MBIC"))
        cat(sprintf("  Min segment days: %d\n", dots$min_segment_days %||% 7))
        cat("\n")
      }

      # Calculate daily minima
      sensor_pos <- dots$sensor_position %||% "outer"
      daily_minima <- calculate_daily_minima(
        vh_data,
        sensor_position = sensor_pos,
        method = hpv_method
      )

      if (verbose) {
        cat("Step 1: Detecting changepoints from daily minima...\n")
      }

      # Detect changepoints
      cpt_result <- detect_changepoints(
        daily_minima,
        penalty = dots$penalty %||% "MBIC",
        min_segment_days = dots$min_segment_days %||% 7,
        merge_short_segments = dots$merge_short_segments %||% TRUE
      )

      if (verbose) {
        cat(sprintf("  Detected %d changepoints\n", length(cpt_result$changepoints)))
        cat("\nStep 2: Applying spacing correction to both sensors...\n")
      }

      # Apply correction using detected changepoints
      correction_result <- apply_spacing_correction_both_sensors(
        vh_data = vh_data,
        changepoints = cpt_result$changepoints,
        method = hpv_method,
        baseline_overrides_outer = dots$baseline_overrides_outer %||% NULL,
        baseline_overrides_inner = dots$baseline_overrides_inner %||% NULL,
        k_assumed = k_assumed,
        probe_spacing = dots$probe_spacing %||% 0.5,
        measurement_time = dots$measurement_time %||% 80,
        create_new_col = dots$create_new_col %||% FALSE,
        verbose = verbose
      )

      # Return unified format
      list(
        vh_corrected = correction_result$vh_corrected,
        method_used = "pelt",
        changepoints = cpt_result$changepoints,
        segments = cpt_result$segments,
        correction_info = correction_result$metadata,
        metadata = list(
          hpv_method = hpv_method,
          k_assumed = k_assumed,
          penalty = dots$penalty %||% "MBIC",
          min_segment_days = dots$min_segment_days %||% 7,
          cpt_detection = cpt_result$parameters
        )
      )
    },

    # ===== VPD METHOD =====
    "vpd" = {
      if (is.null(weather_data)) {
        stop("VPD method requires 'weather_data' parameter with datetime and vpd_kpa columns")
      }

      if (verbose) {
        cat("Using VPD-based changepoint detection\n")
        cat(sprintf("  VPD threshold: %.2f kPa\n", dots$vpd_threshold %||% 0.5))
        cat(sprintf("  Min segment days: %d\n", dots$min_segment_days %||% 7))
        cat("\n")
      }

      # Calculate daily VPD minima
      if (verbose) {
        cat("Step 1: Calculating daily VPD minima...\n")
      }

      daily_vpd <- calculate_daily_vpd_minima(
        weather_data,
        vpd_col = dots$vpd_col %||% "vpd_kpa"
      )

      if (verbose) {
        cat("Step 2: Detecting suitable low-VPD dates...\n")
      }

      # Detect VPD-based changepoints
      vpd_result <- detect_vpd_changepoints(
        daily_vpd,
        vpd_threshold = dots$vpd_threshold %||% 0.5,
        min_segment_days = dots$min_segment_days %||% 7,
        require_consecutive = dots$require_consecutive %||% FALSE,
        min_consecutive_days = dots$min_consecutive_days %||% 3,
        max_changepoints = dots$max_changepoints %||% NULL
      )

      if (verbose) {
        cat(sprintf("  Identified %d suitable dates\n", length(vpd_result$changepoints)))
        cat("\nStep 3: Applying spacing correction to both sensors...\n")
      }

      # Apply correction using VPD-based changepoints
      correction_result <- apply_spacing_correction_both_sensors(
        vh_data = vh_data,
        changepoints = vpd_result$changepoints,
        method = hpv_method,
        baseline_overrides_outer = dots$baseline_overrides_outer %||% NULL,
        baseline_overrides_inner = dots$baseline_overrides_inner %||% NULL,
        k_assumed = k_assumed,
        probe_spacing = dots$probe_spacing %||% 0.5,
        measurement_time = dots$measurement_time %||% 80,
        create_new_col = dots$create_new_col %||% FALSE,
        verbose = verbose
      )

      # Return unified format
      list(
        vh_corrected = correction_result$vh_corrected,
        method_used = "vpd",
        changepoints = vpd_result$changepoints,
        segments = vpd_result$segments,
        correction_info = correction_result$metadata,
        vpd_values = vpd_result$vpd_values,
        metadata = list(
          hpv_method = hpv_method,
          k_assumed = k_assumed,
          vpd_threshold = dots$vpd_threshold %||% 0.5,
          vpd_detection = vpd_result$parameters,
          n_days_below_threshold = vpd_result$n_days_below_threshold
        )
      )
    },

    # ===== HEARTWOOD METHOD =====
    "heartwood" = {
      if (is.null(wood_properties)) {
        stop("Heartwood method requires 'wood_properties' with sapwood_depth")
      }
      if (is.null(probe_config)) {
        stop("Heartwood method requires 'probe_config'")
      }

      # Extract sapwood depth
      sapwood_depth <- if ("tree_measurements" %in% names(wood_properties)) {
        wood_properties$tree_measurements$sapwood_depth
      } else if ("sapwood_depth" %in% names(wood_properties)) {
        wood_properties$sapwood_depth
      } else {
        stop("wood_properties must contain sapwood_depth")
      }

      # Extract bark thickness
      bark_thickness <- if ("tree_measurements" %in% names(wood_properties)) {
        wood_properties$tree_measurements$bark_thickness %||% 0
      } else if ("bark_thickness" %in% names(wood_properties)) {
        wood_properties$bark_thickness
      } else {
        0
      }

      if (verbose) {
        cat("Using HEARTWOOD continuous reference correction\n")
        cat(sprintf("  Sapwood depth: %.2f cm\n", sapwood_depth))
        cat(sprintf("  Bark thickness: %.2f cm\n", bark_thickness))
        cat("\n")
      }

      # Check if heartwood reference is available
      hw_check <- check_heartwood_reference_available(
        probe_config = probe_config,
        sapwood_depth = sapwood_depth,
        bark_thickness = bark_thickness,
        field_of_influence = dots$field_of_influence %||% 1.0
      )

      if (!hw_check$available) {
        stop("Heartwood reference not available:\n  ", hw_check$recommendation)
      }

      if (verbose) {
        cat("✓ Heartwood reference available\n")
        cat(sprintf("  Inner sensor depth: %.2f cm\n", hw_check$inner_depth_cm))
        cat(sprintf("  Margin into heartwood: %.2f cm\n", hw_check$margin_cm))
        cat("\n")
      }

      # Apply heartwood-based correction
      # Note: This is a continuous correction, not segmented
      hw_result <- apply_heartwood_reference_correction(
        vh_data = vh_data,
        method = hpv_method,
        probe_config = probe_config,
        sapwood_depth = sapwood_depth,
        bark_thickness = bark_thickness,
        k_assumed = k_assumed,
        probe_spacing = dots$probe_spacing %||% 0.5,
        measurement_time = dots$measurement_time %||% 80,
        create_new_col = dots$create_new_col %||% FALSE,
        verbose = verbose
      )

      # Return unified format
      list(
        vh_corrected = hw_result$vh_corrected,
        method_used = "heartwood",
        changepoints = NULL,  # Continuous, no changepoints
        segments = NULL,      # Continuous, no segments
        correction_info = hw_result$metadata,
        heartwood_check = hw_check,
        metadata = list(
          hpv_method = hpv_method,
          k_assumed = k_assumed,
          sapwood_depth = sapwood_depth,
          bark_thickness = bark_thickness,
          inner_sensor_depth = hw_check$inner_depth_cm,
          heartwood_margin = hw_check$margin_cm
        )
      )
    }
  )

  # Add class
  class(result) <- c("unified_spacing_correction_result", "list")

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SPACING CORRECTION COMPLETE\n")
    cat(strrep("=", 72), "\n")
    cat(sprintf("Method used: %s\n", toupper(result$method_used)))
    if (!is.null(result$changepoints)) {
      cat(sprintf("Changepoints detected: %d\n", length(result$changepoints)))
      cat(sprintf("Segments created: %d\n", length(result$changepoints) + 1))
    } else {
      cat("Correction type: Continuous (heartwood reference)\n")
    }
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  return(result)
}


#' @rdname apply_spacing_correction
#' @export
print.unified_spacing_correction_result <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("Unified Spacing Correction Result\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("Method used: %s\n", toupper(x$method_used)))
  cat(sprintf("HPV method corrected: %s\n", x$metadata$hpv_method))
  cat(sprintf("Thermal diffusivity: %.6f cm²/s\n", x$metadata$k_assumed))

  if (!is.null(x$changepoints)) {
    cat(sprintf("\nChangepoints: %d\n", length(x$changepoints)))
    if (length(x$changepoints) > 0) {
      cat("  Dates:", paste(as.character(head(x$changepoints, 5)), collapse = ", "))
      if (length(x$changepoints) > 5) {
        cat(sprintf(", ... (%d more)", length(x$changepoints) - 5))
      }
      cat("\n")
    }
    cat(sprintf("Segments: %d\n", length(x$changepoints) + 1))
  } else {
    cat("\nCorrection type: Continuous (heartwood reference)\n")
  }

  cat(sprintf("\nCorrected observations: %d\n", nrow(x$vh_corrected)))

  cat(strrep("=", 60), "\n")
  cat("\n")

  invisible(x)
}


# Helper function for NULL-coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
