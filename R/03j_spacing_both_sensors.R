# R/03j_spacing_both_sensors.R
# Spacing Correction for Both Sensors Simultaneously
# Handles both sensors with same segments, junction smoothing (HRM only)

#' Enhanced Spacing Correction Functions
#'
#' Functions for applying spacing correction to both inner and outer sensors
#' simultaneously, and smoothing discontinuities at segment junctions.
#'
#' @name spacing_correction_enhanced
NULL


#' Apply Spacing Correction to Both Sensors
#'
#' Wrapper function that applies spacing correction to both inner and outer sensors
#' using the same changepoints (detected from outer sensor). Each sensor position
#' calculates its own baseline within each segment, but uses the same segment boundaries.
#'
#' @param vh_data Data frame containing velocity data for both sensors
#' @param changepoints Vector of changepoint dates (detected from outer sensor)
#' @param method Method to correct (default: "HRM"). Burgess correction only validated for HRM.
#' @param baseline_overrides_outer Optional named list of manual baselines for OUTER sensor per segment
#' @param baseline_overrides_inner Optional named list of manual baselines for INNER sensor per segment
#' @param k_assumed Assumed thermal diffusivity (cm²/s) (default: 0.0025)
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param measurement_time Measurement time (sec) (default: 80)
#' @param create_new_col Logical, whether to create new corrected column (Vh_cm_hr_sc)
#'   or overwrite existing column (Vh_cm_hr). Default: FALSE (overwrite in-place).
#'   Set to TRUE to preserve original values.
#' @param verbose Logical, whether to print progress (default: TRUE)
#'
#' @return A list containing:
#'   \item{vh_corrected}{Combined data frame with corrections applied to both sensors}
#'   \item{outer_results}{Correction results for outer sensor}
#'   \item{inner_results}{Correction results for inner sensor}
#'   \item{changepoints}{Changepoints used (from outer sensor)}
#'   \item{metadata}{Metadata about the correction process}
#'
#' @details
#' **Workflow:**
#'
#' 1. Detect changepoints from outer sensor data (more reliable)
#' 2. Apply same changepoints to both inner and outer sensors
#' 3. For each segment, calculate separate baselines for inner and outer
#' 4. Apply Burgess correction per segment per sensor
#' 5. Combine results
#'
#' **Manual Baseline Override:**
#'
#' You can manually specify baselines for specific segments and sensors:
#'
#' \preformatted{
#' # Segment 1: outer = 0.8, inner = 1.2 (manual)
#' # Segment 2: auto-detect for both
#' # Segment 3: outer = 0.6, inner auto-detect
#'
#' result <- apply_spacing_correction_both_sensors(
#'   vh_data,
#'   changepoints,
#'   baseline_overrides_outer = list("1" = 0.8, "3" = 0.6),
#'   baseline_overrides_inner = list("1" = 1.2)
#' )
#' }
#'
#' **Why Same Segments for Both Sensors:**
#'
#' Physical probe movement (tree swelling/shrinking) affects both sensor depths
#' simultaneously, so we use the same segment boundaries but allow different
#' baselines since the amount of misalignment can vary by depth.
#'
#' @examples
#' \dontrun{
#' # Automatic baseline detection for both sensors
#' correction <- apply_spacing_correction_both_sensors(
#'   vh_data = vh_results,
#'   changepoints = cpt_result$changepoints
#' )
#'
#' # Manual baseline override for specific segments/sensors
#' correction <- apply_spacing_correction_both_sensors(
#'   vh_data = vh_results,
#'   changepoints = cpt_result$changepoints,
#'   baseline_overrides_outer = list("1" = 0.8, "2" = 1.5),
#'   baseline_overrides_inner = list("1" = 1.2, "2" = 1.8)
#' )
#' }
#'
#' @family spacing correction functions
#' @export
apply_spacing_correction_both_sensors <- function(vh_data,
                                                   changepoints,
                                                   method = "HRM",
                                                   baseline_overrides_outer = NULL,
                                                   baseline_overrides_inner = NULL,
                                                   k_assumed = 0.0025,
                                                   probe_spacing = 0.5,
                                                   measurement_time = 80,
                                                   create_new_col = FALSE,
                                                   verbose = TRUE) {

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SPACING CORRECTION: BOTH SENSORS\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Method:", method, "(Burgess correction validated for HRM only)\n")
    cat("Changepoints:", length(changepoints), "\n")
    cat("Segments:", length(changepoints) + 1, "\n")
    cat("\n")
  }

  # Generate shared lookup table (used for both sensors)
  if (verbose) {
    cat("Generating Burgess coefficient lookup table...\n")
  }
  lookup_table <- calculate_burgess_coefficients(
    k = k_assumed,
    x = probe_spacing,
    t = measurement_time
  )

  # Process OUTER sensor
  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("PROCESSING OUTER SENSOR\n")
    cat(strrep("=", 72), "\n")
  }

  outer_correction <- apply_spacing_correction_per_segment(
    vh_data = vh_data,
    changepoints = changepoints,
    sensor_position = "outer",
    method = method,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    lookup_table = lookup_table,
    baseline_overrides = baseline_overrides_outer,
    create_new_col = create_new_col,
    verbose = verbose
  )

  # Process INNER sensor
  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("PROCESSING INNER SENSOR\n")
    cat(strrep("=", 72), "\n")
  }

  inner_correction <- apply_spacing_correction_per_segment(
    vh_data = vh_data,
    changepoints = changepoints,
    sensor_position = "inner",
    method = method,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    lookup_table = lookup_table,
    baseline_overrides = baseline_overrides_inner,
    create_new_col = create_new_col,
    verbose = verbose
  )

  # Combine corrected data
  # The corrected data from each call contains all rows, but only the relevant
  # sensor position was actually corrected
  vh_corrected <- outer_correction$vh_corrected
  
  # Update inner sensor rows with inner corrections
  inner_rows <- vh_corrected$sensor_position == "inner" & vh_corrected$method == method
  vh_corrected[inner_rows, ] <- inner_correction$vh_corrected[inner_rows, ]

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("BOTH SENSORS CORRECTED\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  # Return combined results
  result <- list(
    vh_corrected = vh_corrected,
    outer_results = outer_correction,
    inner_results = inner_correction,
    changepoints = changepoints,
    metadata = list(
      method = method,
      k_assumed = k_assumed,
      probe_spacing = probe_spacing,
      measurement_time = measurement_time,
      n_segments = length(changepoints) + 1,
      correction_date = Sys.time()
    )
  )

  class(result) <- c("both_sensors_correction_result", "list")

  return(result)
}


#' Smooth Segment Junctions to Prevent Discontinuities
#'
#' Applies a moving average across segment boundaries to smooth discontinuities
#' caused by baseline shifts. Only applied to HRM method (Burgess correction
#' is HRM-specific).
#'
#' @param vh_data Velocity data frame with spacing corrections already applied
#' @param changepoints Vector of changepoint dates defining segment boundaries
#' @param method Method to smooth (default: "HRM"). Only HRM should be smoothed
#'   as only HRM had Burgess correction applied.
#' @param window_size Number of measurements on each side of junction to include
#'   in moving average (default: 3). Total window = 2 * window_size + 1.
#' @param verbose Logical, whether to print progress (default: TRUE)
#'
#' @return The vh_data data frame with smoothed values at segment junctions
#'
#' @details
#' **Why Smooth Junctions:**
#'
#' When baseline offsets differ between segments (due to probe movement), the
#' Burgess correction can create artificial discontinuities at changepoints.
#' Real sap flow doesn't change instantaneously, so we apply a small moving
#' average window across junctions to blend the transition.
#'
#' **Window Size:**
#'
#' Default of 3 means:
#' \itemize{
#'   \item 3 measurements before changepoint
#'   \item The changepoint measurement itself
#'   \item 3 measurements after changepoint
#'   \item Total window: 7 measurements
#' }
#'
#' **HRM Only:**
#'
#' Only HRM is smoothed because:
#' \itemize{
#'   \item Burgess correction only validated for HRM
#'   \item Other methods (MHR, Tmax) have different correction factors
#'   \item Phase 5 (method calibration) handles other methods
#' }
#'
#' **Applied to Both Sensors:**
#'
#' Smoothing is applied independently to outer and inner sensors, each with
#' their own junction discontinuities.
#'
#' @examples
#' \dontrun{
#' # After spacing correction
#' vh_corrected <- apply_spacing_correction_both_sensors(vh_data, changepoints)
#'
#' # Smooth junctions (default 3-point window)
#' vh_smoothed <- smooth_segment_junctions(
#'   vh_corrected$vh_corrected,
#'   changepoints,
#'   window_size = 3
#' )
#'
#' # Larger window for more smoothing
#' vh_smoothed <- smooth_segment_junctions(
#'   vh_corrected$vh_corrected,
#'   changepoints,
#'   window_size = 5
#' )
#' }
#'
#' @family spacing correction functions
#' @export
smooth_segment_junctions <- function(vh_data,
                                      changepoints,
                                      method = "HRM",
                                      window_size = 3,
                                      verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  if (length(changepoints) == 0) {
    if (verbose) {
      cat("No changepoints - no junctions to smooth\n")
    }
    return(vh_data)
  }

  if (!"datetime" %in% names(vh_data)) {
    stop("vh_data must have a 'datetime' column")
  }

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Convert changepoints to POSIXct
  cpt_datetimes <- as.POSIXct(as.character(changepoints))

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SMOOTHING SEGMENT JUNCTIONS\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
    cat("Method:", method, "(HRM only)\n")
    cat("Changepoints to smooth:", length(cpt_datetimes), "\n")
    cat("Window size:", window_size, "measurements either side\n")
    cat("Total smoothing window:", 2 * window_size + 1, "measurements\n")
    cat("\n")
  }

  # Process each sensor position separately
  for (sensor_pos in c("outer", "inner")) {
    if (verbose) {
      cat("Smoothing", toupper(sensor_pos), "sensor...\n")
    }

    # Filter to method and sensor
    method_sensor_rows <- vh_data$method == method & 
                           vh_data$sensor_position == sensor_pos

    if (sum(method_sensor_rows) == 0) {
      if (verbose) {
        cat("  No data for", sensor_pos, "- skipping\n")
      }
      next
    }

    # Extract subset and order by datetime
    subset_data <- vh_data[method_sensor_rows, ]
    subset_data <- subset_data[order(subset_data$datetime), ]

    # Smooth at each changepoint
    n_smoothed <- 0
    for (cpt_date in cpt_datetimes) {
      # Find nearest measurement to changepoint
      time_diffs <- abs(as.numeric(subset_data$datetime - cpt_date))
      cpt_idx <- which.min(time_diffs)

      # Define smoothing window
      start_idx <- max(1, cpt_idx - window_size)
      end_idx <- min(nrow(subset_data), cpt_idx + window_size)

      # Check if we have enough points
      if (end_idx - start_idx < 2) {
        if (verbose) {
          cat("  Insufficient points at", format(cpt_date, "%Y-%m-%d"), "- skipping\n")
        }
        next
      }

      # Extract window values
      window_values <- subset_data$Vh_cm_hr[start_idx:end_idx]

      # Apply moving average (simple mean)
      smoothed_value <- mean(window_values, na.rm = TRUE)

      # Replace center point with smoothed value
      subset_data$Vh_cm_hr[cpt_idx] <- smoothed_value

      n_smoothed <- n_smoothed + 1
    }

    if (verbose) {
      cat("  Smoothed", n_smoothed, "of", length(cpt_datetimes), "junctions\n")
    }

    # Put smoothed values back into main data frame
    # Match by row order (since we ordered by datetime)
    vh_data$Vh_cm_hr[method_sensor_rows] <- subset_data$Vh_cm_hr[
      match(vh_data$datetime[method_sensor_rows], subset_data$datetime)
    ]
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("JUNCTION SMOOTHING COMPLETE\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  return(vh_data)
}
