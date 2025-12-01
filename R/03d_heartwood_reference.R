# R/03d_heartwood_reference.R
# Heartwood Reference Correction
# Functions for heartwood-based spacing correction

#' Heartwood Reference Correction Functions
#'
#' Functions for checking and applying heartwood reference correction.
#'
#' @name heartwood_reference
NULL


#' @export
check_heartwood_reference_available <- function(probe_config,
                                                 sapwood_depth,
                                                 bark_thickness = 0,
                                                 field_of_influence = 1.0) {

  # Helper for null-coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Extract probe parameters
  if (inherits(probe_config, "ProbeConfig")) {
    # R6 ProbeConfig object
    probe_length_mm <- probe_config$length %||% probe_config$probe_length %||% 35
    inner_sensor_mm <- probe_config$inner_sensor %||% 7.5
  } else if (is.list(probe_config)) {
    # Named list
    probe_length_mm <- probe_config$length %||% probe_config$probe_length %||% 35
    inner_sensor_mm <- probe_config$inner_sensor %||% 7.5
  } else {
    stop("probe_config must be a ProbeConfig object or a named list")
  }

  # Validate inputs
  if (!is.numeric(sapwood_depth) || sapwood_depth <= 0) {
    stop("sapwood_depth must be a positive number (in cm)")
  }
  if (!is.numeric(bark_thickness) || bark_thickness < 0) {
    stop("bark_thickness must be a non-negative number (in cm)")
  }
  if (!is.numeric(field_of_influence) || field_of_influence <= 0) {
    stop("field_of_influence must be a positive number (in cm)")
  }

  # Calculate inner sensor depth from cambium (cm)
  # Inner sensor position from probe handle = probe_length - inner_sensor_from_tip
  # Depth from cambium = that value (in mm) / 10 - bark_thickness
  inner_position_from_handle_mm <- probe_length_mm - inner_sensor_mm
  inner_depth_cm <- (inner_position_from_handle_mm / 10) - bark_thickness

  # Required margin: half the field of influence
  required_margin_cm <- field_of_influence / 2

  # Calculate margin (how far past sapwood boundary)
  margin_cm <- inner_depth_cm - sapwood_depth

  # Determine availability
  available <- margin_cm >= required_margin_cm

  # Build recommendation
  if (available) {
    recommendation <- sprintf(
      paste0("Heartwood reference correction IS available. ",
             "Inner sensor is %.2f cm past the sapwood boundary ",
             "(required: >= %.2f cm). The inner sensor can be used as a ",
             "continuous zero-flow reference."),
      margin_cm, required_margin_cm
    )
  } else if (margin_cm > 0) {
    recommendation <- sprintf(
      paste0("Heartwood reference correction NOT recommended. ",
             "Inner sensor is only %.2f cm past sapwood boundary ",
             "(required: >= %.2f cm for reliable reference). ",
             "Consider using changepoint-based correction instead, or ",
             "reduce field_of_influence if you are confident in sensor placement."),
      margin_cm, required_margin_cm
    )
  } else if (margin_cm == 0) {
    recommendation <- paste0(
      "Inner sensor is exactly at the sapwood/heartwood boundary. ",
      "Heartwood reference correction is NOT available. ",
      "Use changepoint-based correction methods instead."
    )
  } else {
    recommendation <- sprintf(
      paste0("Inner sensor is %.2f cm WITHIN sapwood (not in heartwood). ",
             "Heartwood reference correction is NOT available. ",
             "Use changepoint-based correction methods instead."),
      abs(margin_cm)
    )
  }

  result <- list(
    available = available,
    inner_depth_cm = inner_depth_cm,
    sapwood_depth_cm = sapwood_depth,
    margin_cm = margin_cm,
    required_margin_cm = required_margin_cm,
    probe_config_used = list(
      probe_length_mm = probe_length_mm,
      inner_sensor_from_tip_mm = inner_sensor_mm,
      bark_thickness_cm = bark_thickness
    ),
    field_of_influence_cm = field_of_influence,
    recommendation = recommendation
  )

  class(result) <- c("heartwood_reference_check", "list")

  return(result)
}

#' Print Method for Heartwood Reference Check
#'
#' @param x A heartwood_reference_check object
#' @param ... Additional arguments (ignored)
#' @export
print.heartwood_reference_check <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("HEARTWOOD REFERENCE AVAILABILITY CHECK\n")
  cat(strrep("=", 60), "\n\n")

  cat("Probe Configuration:\n")
  cat("  Probe length:        ", x$probe_config_used$probe_length_mm, " mm\n", sep = "")
  cat("  Inner sensor (from tip): ", x$probe_config_used$inner_sensor_from_tip_mm, " mm\n", sep = "")
  cat("  Bark thickness:      ", x$probe_config_used$bark_thickness_cm, " cm\n", sep = "")
  cat("\n")

  cat("Depth Analysis:\n")
  cat("  Inner sensor depth:  ", sprintf("%.2f", x$inner_depth_cm), " cm (from cambium)\n", sep = "")
  cat("  Sapwood depth:       ", sprintf("%.2f", x$sapwood_depth_cm), " cm\n", sep = "")
  cat("  Margin into heartwood: ", sprintf("%.2f", x$margin_cm), " cm\n", sep = "")
  cat("  Required margin:     ", sprintf("%.2f", x$required_margin_cm), " cm\n", sep = "")
  cat("\n")

  if (x$available) {
    cat("Status: AVAILABLE\n")
  } else {
    cat("Status: NOT AVAILABLE\n")
  }
  cat("\n")
  cat("Recommendation:\n")
  cat(strwrap(x$recommendation, width = 58, indent = 2, exdent = 2), sep = "\n")
  cat("\n")
  cat(strrep("=", 60), "\n")

  invisible(x)
}


#' Apply Heartwood Reference Correction
#'
#' Applies spacing correction using the inner sensor as a continuous zero-flow
#' reference. This method is applicable when the inner sensor is positioned
#' in the heartwood (verified by \code{check_heartwood_reference_available()}).
#'
#' For each measurement, the inner sensor Vh is used as the instantaneous
#' zero offset, and Burgess correction coefficients are applied to correct
#' the outer sensor reading.
#'
#' @param vh_data Data frame containing heat pulse velocity data with both
#'   inner and outer sensor readings. Must have columns: datetime, pulse_id,
#'   sensor_position, and a velocity column.
#' @param method HPV method to correct (default: "HRM").
#' @param method_col Name of method column (default: "method").
#' @param vh_col Name of velocity column (default: "Vh_cm_hr").
#' @param k_assumed Assumed thermal diffusivity (cm2/s, default: 0.0025).
#' @param probe_spacing Probe spacing x (cm, default: 0.5).
#' @param measurement_time Time after heat pulse for HRM measurement (seconds,
#'   default: 80).
#' @param lookup_table Pre-computed Burgess lookup table. If NULL, will be
#'   generated using \code{calculate_burgess_coefficients()}.
#' @param create_new_col If TRUE (default), creates new column with "_hrc"
#'   suffix. If FALSE, overwrites vh_col.
#' @param verbose Print progress messages (default: TRUE).
#'
#' @return A list with class \code{"heartwood_reference_correction_result"}:
#'   \item{vh_corrected}{Data frame with corrected velocities}
#'   \item{offset_summary}{Summary statistics of observed offsets}
#'   \item{metadata}{Correction metadata and parameters}
#'
#' @details
#' **Method Overview:**
#'
#' When the inner sensor is in heartwood, it experiences no convective heat
#' transfer from sap flow. Any Vh measured at the inner sensor represents
#' the instantaneous probe misalignment (zero offset). This offset can be
#' used to correct the outer sensor reading continuously.
#'
#' **Correction Process:**
#'
#' For each measurement:
#' \enumerate{
#'   \item Extract inner sensor Vh = zero_offset
#'   \item Look up Burgess correction coefficients (a, b) for that offset
#'   \item Apply to outer sensor: Vh_corrected = a * Vh_outer + b
#' }
#'
#' **Comparison with Changepoint Method:**
#'
#' The heartwood reference method provides continuous (per-measurement)
#' correction, whereas the changepoint method applies segment-based
#' correction. Use heartwood reference when:
#' \itemize{
#'   \item Inner sensor is confirmed to be in heartwood
#'   \item You want real-time offset tracking
#'   \item Probe alignment may drift within segments
#' }
#'
#' **Diagnostic Output:**
#'
#' The \code{offset_summary} in the result provides statistics on the
#' observed offsets:
#' \itemize{
#'   \item \code{mean_offset}: Average inner sensor Vh
#'   \item \code{sd_offset}: Standard deviation of offsets
#'   \item \code{range_offset}: Min/max observed offsets
#'   \item \code{quality}: Assessment of offset stability
#' }
#'
#' @examples
#' \dontrun{
#' # First check if heartwood reference is available
#' check <- check_heartwood_reference_available(
#'   probe_config = list(length = 35, inner_sensor = 7.5),
#'   sapwood_depth = 2.0
#' )
#'
#' if (check$available) {
#'   # Apply heartwood reference correction
#'   result <- apply_heartwood_reference_correction(
#'     vh_data = my_vh_data,
#'     method = "HRM"
#'   )
#'
#'   # View corrected data
#'   head(result$vh_corrected)
#'
#'   # Check offset statistics
#'   print(result$offset_summary)
#' }
#' }
#'
#' @seealso \code{\link{check_heartwood_reference_available}},
#'   \code{\link{apply_spacing_correction_per_segment}},
#'   \code{\link{calculate_burgess_coefficients}}
#'
#' @family spacing correction functions
#' @export
apply_heartwood_reference_correction <- function(vh_data,
                                                  method = "HRM",
                                                  method_col = "method",
                                                  vh_col = "Vh_cm_hr",
                                                  k_assumed = 0.0025,
                                                  probe_spacing = 0.5,
                                                  measurement_time = 80,
                                                  lookup_table = NULL,
                                                  create_new_col = TRUE,
                                                  verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "pulse_id", "sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for both inner and outer sensors
  sensor_positions <- unique(vh_data$sensor_position)
  if (!all(c("inner", "outer") %in% sensor_positions)) {
    stop("vh_data must contain both 'inner' and 'outer' sensor positions")
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("HEARTWOOD REFERENCE CORRECTION\n")
    cat(strrep("=", 72), "\n")
    cat("Method:", method, "\n")
    cat("Velocity column:", vh_col, "\n")
    cat(strrep("-", 72), "\n\n")
  }

  # Generate or validate lookup table
  if (is.null(lookup_table)) {
    if (verbose) cat("Generating Burgess lookup table...\n")
    lookup_table <- calculate_burgess_coefficients(
      zero_offset_range = c(-10, 10),
      resolution = 0.1,
      k = k_assumed,
      x = probe_spacing,
      t = measurement_time
    )
  }

  # Filter for specified method
  method_data <- vh_data[vh_data[[method_col]] == method, ]

  if (nrow(method_data) == 0) {
    stop("No data found for method '", method, "'")
  }

  # Separate inner and outer sensor data
  inner_data <- method_data[method_data$sensor_position == "inner", ]
  outer_data <- method_data[method_data$sensor_position == "outer", ]

  if (nrow(inner_data) == 0 || nrow(outer_data) == 0) {
    stop("Both inner and outer sensor data are required for heartwood reference correction")
  }

  if (verbose) {
    cat("Inner sensor observations:", nrow(inner_data), "\n")
    cat("Outer sensor observations:", nrow(outer_data), "\n\n")
  }

  # Match inner and outer by pulse_id
  # Create a lookup for inner sensor Vh by pulse_id
  inner_lookup <- stats::setNames(inner_data[[vh_col]], inner_data$pulse_id)

  # Get matching inner Vh for each outer observation
  outer_data$inner_vh_offset <- inner_lookup[as.character(outer_data$pulse_id)]

  # Check for unmatched pulses
  n_unmatched <- sum(is.na(outer_data$inner_vh_offset))
  if (n_unmatched > 0) {
    if (verbose) {
      cat("Warning:", n_unmatched, "outer observations have no matching inner sensor reading\n")
      cat("  These will not be corrected.\n\n")
    }
  }

  # Calculate offset summary statistics
  valid_offsets <- outer_data$inner_vh_offset[!is.na(outer_data$inner_vh_offset)]
  offset_summary <- list(
    n_observations = length(valid_offsets),
    mean_offset = mean(valid_offsets, na.rm = TRUE),
    median_offset = stats::median(valid_offsets, na.rm = TRUE),
    sd_offset = stats::sd(valid_offsets, na.rm = TRUE),
    min_offset = min(valid_offsets, na.rm = TRUE),
    max_offset = max(valid_offsets, na.rm = TRUE),
    range_offset = diff(range(valid_offsets, na.rm = TRUE))
  )

  # Assess offset quality
  if (abs(offset_summary$mean_offset) < 1 && offset_summary$sd_offset < 1) {
    offset_summary$quality <- "GOOD"
    offset_summary$interpretation <- "Probe well-aligned, stable offset"
  } else if (abs(offset_summary$mean_offset) < 3 && offset_summary$sd_offset < 2) {
    offset_summary$quality <- "ACCEPTABLE"
    offset_summary$interpretation <- "Moderate misalignment, correction recommended"
  } else if (abs(offset_summary$mean_offset) >= 3) {
    offset_summary$quality <- "WARNING"
    offset_summary$interpretation <- "Significant misalignment detected - check probe installation"
  } else {
    offset_summary$quality <- "VARIABLE"
    offset_summary$interpretation <- "High offset variability - may indicate installation issues"
  }

  if (verbose) {
    cat("Offset Statistics:\n")
    cat("  Mean offset: ", sprintf("%.2f", offset_summary$mean_offset), " cm/hr\n", sep = "")
    cat("  SD offset:   ", sprintf("%.2f", offset_summary$sd_offset), " cm/hr\n", sep = "")
    cat("  Range:       ", sprintf("%.2f to %.2f", offset_summary$min_offset, offset_summary$max_offset), " cm/hr\n", sep = "")
    cat("  Quality:     ", offset_summary$quality, "\n", sep = "")
    cat("  ", offset_summary$interpretation, "\n\n", sep = "")
  }

  # Apply correction for each observation
  corrected_vh <- numeric(nrow(outer_data))

  for (i in seq_len(nrow(outer_data))) {
    offset <- outer_data$inner_vh_offset[i]
    original_vh <- outer_data[[vh_col]][i]

    if (is.na(offset) || is.na(original_vh)) {
      corrected_vh[i] <- NA_real_
      next
    }

    # Get correction coefficients for this offset
    coef <- get_correction_coefficients(
      zero_offset = offset,
      lookup_table = lookup_table,
      verbose = FALSE
    )

    # Apply correction: Vh_corrected = a * Vh + b
    corrected_vh[i] <- coef$a * original_vh + coef$b
  }

  # Create output column name
  if (create_new_col) {
    corrected_col <- paste0(vh_col, "_hrc")
  } else {
    corrected_col <- vh_col
  }

  # Add corrected values to outer_data
  outer_data[[corrected_col]] <- corrected_vh
  outer_data$heartwood_ref_applied <- !is.na(corrected_vh)

  # Also mark inner data (no correction needed/applied)
  inner_data[[corrected_col]] <- inner_data[[vh_col]]  # Inner stays same
  inner_data$heartwood_ref_applied <- FALSE
  inner_data$inner_vh_offset <- NA_real_

  # Combine back together
  corrected_data <- rbind(inner_data, outer_data)

  # Sort by datetime and pulse_id
  corrected_data <- corrected_data[order(corrected_data$datetime, corrected_data$pulse_id), ]

  # Also include non-method data (uncorrected)
  other_data <- vh_data[vh_data[[method_col]] != method, ]
  if (nrow(other_data) > 0) {
    other_data[[corrected_col]] <- other_data[[vh_col]]
    other_data$heartwood_ref_applied <- FALSE
    other_data$inner_vh_offset <- NA_real_
    corrected_data <- rbind(corrected_data, other_data)
    corrected_data <- corrected_data[order(corrected_data$datetime, corrected_data$pulse_id), ]
  }

  # Create metadata
  metadata <- list(
    method = method,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_outer_corrected = sum(outer_data$heartwood_ref_applied, na.rm = TRUE),
    n_outer_uncorrected = sum(!outer_data$heartwood_ref_applied, na.rm = TRUE),
    date_applied = Sys.time(),
    approach = "Heartwood reference (continuous)",
    sapfluxr_version = utils::packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("-", 72), "\n")
    cat("Correction applied to", metadata$n_outer_corrected, "outer sensor observations\n")
    if (metadata$n_outer_uncorrected > 0) {
      cat("  (", metadata$n_outer_uncorrected, " could not be corrected due to missing inner data)\n", sep = "")
    }
    cat(strrep("=", 72), "\n\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    offset_summary = offset_summary,
    lookup_table = lookup_table,
    metadata = metadata
  )

  class(result) <- c("heartwood_reference_correction_result", "list")

  return(result)
}

#' Print Method for Heartwood Reference Correction Result
#'
#' @param x A heartwood_reference_correction_result object
#' @param ... Additional arguments (ignored)
#' @export
print.heartwood_reference_correction_result <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("HEARTWOOD REFERENCE CORRECTION RESULT\n")
  cat(strrep("=", 60), "\n\n")

  cat("Method:", x$metadata$method, "\n")
  cat("Approach:", x$metadata$approach, "\n\n")

  cat("Observations Corrected:\n")
  cat("  Outer sensor:", x$metadata$n_outer_corrected, "\n")
  if (x$metadata$n_outer_uncorrected > 0) {
    cat("  Uncorrected: ", x$metadata$n_outer_uncorrected, " (missing inner data)\n", sep = "")
  }
  cat("\n")

  cat("Offset Summary:\n")
  cat("  Mean:   ", sprintf("%.2f", x$offset_summary$mean_offset), " cm/hr\n", sep = "")
  cat("  SD:     ", sprintf("%.2f", x$offset_summary$sd_offset), " cm/hr\n", sep = "")
  cat("  Range:  ", sprintf("%.2f to %.2f", x$offset_summary$min_offset, x$offset_summary$max_offset), " cm/hr\n", sep = "")
  cat("  Quality:", x$offset_summary$quality, "\n")
  cat("  ", x$offset_summary$interpretation, "\n", sep = "")
  cat("\n")
  cat(strrep("=", 60), "\n")

  invisible(x)
}
