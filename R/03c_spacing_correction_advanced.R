# R/03c_spacing_correction_advanced.R
# Advanced Spacing Correction Methods
# Per-segment and manual spacing correction workflows

#' Advanced Spacing Correction Functions
#'
#' Functions for advanced spacing correction including per-segment and manual correction.
#'
#' @name spacing_correction_advanced
NULL


#' Apply Spacing Correction per Data Segment
#'
#' Applies Burgess spacing correction to each segment of data identified by
#' changepoints. For each segment, a zero flow reference (offset) is determined
#' and used to correct the readings.
#'
#' @param vh_data Data frame containing heat pulse velocity data.
#' @param changepoints A list of changepoints as returned by \code{detect_changepoints()}.
#' @param sensor_position Character, "inner" or "outer".
#' @param method Character, HPV method to correct (default: "HRM").
#' @param method_col Name of method column (default: "method").
#' @param vh_col Name of velocity column (default: "Vh_cm_hr").
#' @param k_assumed Assumed thermal diffusivity (cm^2/s, default: 0.0025).
#' @param probe_spacing Probe spacing x (cm, default: 0.5).
#' @param measurement_time Time after heat pulse (seconds, default: 80).
#' @param lookup_table Pre-computed Burgess lookup table.
#' @param baseline_overrides Optional list of manual offsets per segment.
#' @param create_new_col If TRUE (default), creates new column with "_sc" suffix.
#' @param correction_type Character, either "burgess" (default) or "linear".
#'   Burgess uses physics-based coefficients; Linear uses a simple 1:1 offset.
#' @param verbose Print messages (default: TRUE).
#'
#' @return A list containing corrected data frame and correction statistics.
#' @export
apply_spacing_correction_per_segment <- function(vh_data,
                                                  changepoints,
                                                  sensor_position,
                                                  method = "HRM",
                                                  method_col = "method",
                                                  vh_col = "Vh_cm_hr",
                                                  correction_type = c("burgess", "linear"),
                                                  k_assumed = 0.0025,
                                                  probe_spacing = 0.5,
                                                  measurement_time = 80,
                                                  lookup_table = NULL,
                                                  baseline_overrides = NULL,
                                                  create_new_col = TRUE,
                                                  verbose = TRUE) {

  # Match and validate correction type
  correction_type <- match.arg(correction_type)

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!sensor_position %in% c("outer", "inner")) {
    stop("sensor_position must be 'outer' or 'inner'")
  }

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Filter data for specified sensor and method
  sensor_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method,
  ]

  if (nrow(sensor_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
  }

  # Create or validate lookup table
  if (is.null(lookup_table)) {
    if (verbose) {
      cat("Generating Burgess coefficient lookup table...\n")
    }
    lookup_table <- calculate_burgess_coefficients(
      k = k_assumed,
      x = probe_spacing,
      t = measurement_time
    )
  }

  # Define segment boundaries from changepoints
  # Convert changepoints to POSIXct for comparison with datetime
  if (!is.null(changepoints) && length(changepoints) > 0) {
    cpt_datetimes <- as.POSIXct(as.character(changepoints))
    segment_starts <- c(min(sensor_data$datetime), cpt_datetimes)
    segment_ends <- c(cpt_datetimes, max(sensor_data$datetime))
  } else {
    # No changepoints - single segment
    segment_starts <- min(sensor_data$datetime)
    segment_ends <- max(sensor_data$datetime)
  }

  n_segments <- length(segment_starts)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("SEGMENT-BASED SPACING CORRECTION (k =", k_assumed, "cm^2/s)\n")
    cat(strrep("=", 72), "\n")
    cat("Sensor:", toupper(sensor_position), "| Method:", method, "\n")
    cat("Number of segments:", n_segments, "\n")
    cat("\n")
  }

  # Initialise storage
  segment_results <- list()
  corrected_vh <- numeric(nrow(sensor_data))

  # Initialise correction factor tracking vectors (for transparency)
  coef_a_vector <- rep(NA_real_, nrow(sensor_data))
  coef_b_vector <- rep(NA_real_, nrow(sensor_data))
  baseline_offset_vector <- rep(NA_real_, nrow(sensor_data))

  # Process each segment
  for (seg_id in seq_len(n_segments)) {

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing Segment", seg_id, "of", n_segments, "\n")
      cat(strrep("-", 72), "\n")
      cat("Period:", format(segment_starts[seg_id], "%Y-%m-%d %H:%M"), "to",
          format(segment_ends[seg_id], "%Y-%m-%d %H:%M"), "\n")
    }

    # Extract segment data
    seg_mask <- sensor_data$datetime >= segment_starts[seg_id] &
                sensor_data$datetime <= segment_ends[seg_id]
    seg_data <- sensor_data[seg_mask, ]

    if (nrow(seg_data) == 0) {
      if (verbose) {
        cat("  [FAIL] No data in this segment - skipping\n\n")
      }
      next
    }

    if (verbose) {
      cat("  Observations:", nrow(seg_data), "\n")
    }

    # Calculate zero offset as minimum Vh in this segment (baseline)
    vh_values <- seg_data[[vh_col]]
    vh_values_clean <- vh_values[!is.na(vh_values) & is.finite(vh_values)]

    if (length(vh_values_clean) == 0) {
      if (verbose) {
        cat("  [FAIL] No valid Vh values in segment - skipping\n\n")
      }
      next
    }

    # Check for manual baseline override
    if (!is.null(baseline_overrides) && seg_id %in% names(baseline_overrides)) {
      zero_vh <- baseline_overrides[[seg_id]]
      if (verbose) {
        cat("  Using MANUAL baseline override\n")
      }
    } else {
      zero_vh <- round(min(vh_values_clean, na.rm = TRUE), 1)
      if (verbose) {
        cat("  Using AUTO-DETECTED baseline (minimum)\n")
      }
    }
    mean_vh <- mean(vh_values_clean, na.rm = TRUE)
    sd_vh <- sd(vh_values_clean, na.rm = TRUE)
    cv <- sd_vh / abs(mean_vh)

    if (verbose) {
      cat("  Baseline (min Vh):", zero_vh, "cm/hr\n")
      cat("  Mean Vh:", round(mean_vh, 2), "cm/hr\n")
      cat("  SD:", round(sd_vh, 2), "| CV:", round(cv, 3), "\n")
    }

    # Get correction coefficients
    tryCatch({
      if (correction_type == "linear") {
        # Simple linear offset: a = 1, b = -zero_vh
        coef_result <- list(
          coef_a = 1.0,
          coef_b = -zero_vh,
          zero_vh = zero_vh,
          range_type = "linear",
          severity = "none",
          warning = NULL,
          correction_formula = paste0("Vh_corrected = 1.0 * Vh + (", -zero_vh, ")")
        )
      } else {
        # Physics-based Burgess coefficients
        coef_result <- get_correction_coefficients(
          zero_vh = zero_vh,
          lookup_table = lookup_table
        )
      }

      if (verbose) {
        cat("  Correction formula:", coef_result$correction_formula, "\n")
        if (correction_type == "burgess") {
          cat("  Severity:", toupper(coef_result$severity), "\n")
        }
      }

      # Apply correction to this segment
      corrected_vh[seg_mask] <- coef_result$coef_a * vh_values + coef_result$coef_b

      # Store correction factors for this segment (for transparency)
      coef_a_vector[seg_mask] <- coef_result$coef_a
      coef_b_vector[seg_mask] <- coef_result$coef_b
      baseline_offset_vector[seg_mask] <- zero_vh

      # Store segment results
      segment_results[[seg_id]] <- list(
        segment_id = seg_id,
        start_datetime = segment_starts[seg_id],
        end_datetime = segment_ends[seg_id],
        n_observations = nrow(seg_data),
        zero_vh = zero_vh,
        mean_vh = mean_vh,
        sd_vh = sd_vh,
        cv = cv,
        coef_a = coef_result$coef_a,
        coef_b = coef_result$coef_b,
        range_type = coef_result$range_type,
        severity = coef_result$severity,
        correction_formula = coef_result$correction_formula
      )

    }, error = function(e) {
      if (verbose) {
        cat("  [FAIL] Error getting correction coefficients:", e$message, "\n")
      }
      warning("Segment ", seg_id, ": ", e$message, call. = FALSE)
      corrected_vh[seg_mask] <- vh_values  # Use uncorrected
    })

    if (verbose) cat("\n")
  }

  # Apply corrections to full dataset
  corrected_data <- vh_data

  if (create_new_col) {
    corrected_col <- paste0(vh_col, "_sc")
    corrected_data[[corrected_col]] <- NA_real_
    corrected_data$spacing_correction_applied <- FALSE
  } else {
    corrected_col <- vh_col
    if (!"spacing_correction_applied" %in% names(corrected_data)) {
      corrected_data$spacing_correction_applied <- FALSE
    }
  }

  # Insert corrected values for this sensor/method
  # AGGRESSIVE FLATTENING: Force to plain numeric vector to prevent nested dataframe bug
  corrected_vh_flat <- as.numeric(unlist(corrected_vh))
  
  sensor_method_mask <- corrected_data$sensor_position == sensor_position &
                        corrected_data[[method_col]] == method

  corrected_data[[corrected_col]][sensor_method_mask] <- corrected_vh_flat
  corrected_data$spacing_correction_applied[sensor_method_mask] <- TRUE

  # Add correction factor columns (for transparency - matches wound correction pattern)
  if (!"spacing_correction_a" %in% names(corrected_data)) {
    corrected_data$spacing_correction_a <- NA_real_
    corrected_data$spacing_correction_b <- NA_real_
    corrected_data$baseline_offset_cm_hr <- NA_real_
  }
  corrected_data$spacing_correction_a[sensor_method_mask] <- coef_a_vector
  corrected_data$spacing_correction_b[sensor_method_mask] <- coef_b_vector
  corrected_data$baseline_offset_cm_hr[sensor_method_mask] <- baseline_offset_vector

  # Create metadata
  metadata <- list(
    method = method,
    sensor_position = sensor_position,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_segments = length(segment_results),
    changepoints = if (!is.null(changepoints)) as.character(changepoints) else character(0),
    date_applied = Sys.time(),
    approach = "Changepoint-based segmentation",
    sapfluxr_version = packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("[OK] Segment-based spacing correction complete!\n")
    cat("  ", length(segment_results), "segments processed\n")
    n_corrected <- sum(corrected_data$spacing_correction_applied, na.rm = TRUE)
    cat("  ", n_corrected, "observations corrected\n")
    cat("\nCorrection factor columns added:\n")
    cat("  spacing_correction_a - Slope coefficient (a)\n")
    cat("  spacing_correction_b - Intercept coefficient (b)\n")
    cat("  baseline_offset_cm_hr - Zero-flow baseline (cm/hr)\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    segment_results = segment_results,
    metadata = metadata
  )

  class(result) <- c("segment_spacing_correction_result", "list")

  return(result)
}

#' Apply Manual Spacing Correction with User-Specified Changepoints
#'
#' Applies spacing correction using manually specified changepoints and optional
#' baseline overrides. This gives users full control over segment boundaries and
#' zero offsets when they have domain knowledge about probe conditions.
#'
#' @param vh_data Data frame containing velocity data
#' @param manual_changepoints Vector of changepoint dates defining segment boundaries.
#'   Can be character strings ("2024-03-15") or Date objects. These divide the
#'   time series into n+1 segments.
#' @param baseline_overrides Optional named list specifying zero offset values for
#'   specific date ranges. Names must be in format "start_date/end_date" and values
#'   are the baseline Vh (cm/hr) to use for that segment.
#'   Example: \code{list("2024-01-01/2024-03-15" = 0.8, "2024-03-16/2024-06-10" = 1.2)}
#'   If a segment is not specified, its baseline will be auto-detected as the minimum.
#' @param sensor_position Sensor position to correct ("outer" or "inner")
#' @param method Method to correct (default: "HRM")
#' @param method_col Name of method column (default: "method")
#' @param vh_col Name of velocity column (default: "Vh_cm_hr")
#' @param k_assumed Assumed thermal diffusivity (cm^2/s) (default: 0.0025)
#' @param probe_spacing Probe spacing (cm) (default: 0.5)
#' @param measurement_time Measurement time (sec) (default: 80)
#' @param lookup_table Optional pre-calculated Burgess lookup table
#' @param create_new_col Logical, whether to create new corrected column (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{vh_corrected}{Data frame with spacing corrections applied, including:
#'     \itemize{
#'       \item \code{Vh_cm_hr_sc} - Spacing-corrected velocities (if create_new_col = TRUE)
#'       \item \code{spacing_correction_applied} - Logical flag indicating correction
#'       \item \code{spacing_correction_a} - Slope coefficient (a) applied per row
#'       \item \code{spacing_correction_b} - Intercept coefficient (b) applied per row
#'       \item \code{baseline_offset_cm_hr} - Zero-flow baseline used (cm/hr)
#'     }
#'   }
#'   \item{segment_results}{List of correction results per segment}
#'   \item{metadata}{List containing correction metadata}
#'
#' @details
#' **Manual Changepoint Specification:**
#'
#' This function allows complete user control over:
#' \itemize{
#'   \item Segment boundaries (via \code{manual_changepoints})
#'   \item Zero offset values (via \code{baseline_overrides})
#' }
#'
#' **Workflow:**
#' \enumerate{
#'   \item Parse manual changepoints to create segments
#'   \item For each segment:
#'     \itemize{
#'       \item Check if baseline_overrides specifies a value for this date range
#'       \item If yes, use the specified baseline
#'       \item If no, auto-detect minimum Vh as baseline
#'     }
#'   \item Look up Burgess coefficients for each baseline
#'   \item Apply segment-specific corrections
#' }
#'
#' **Date Range Format:**
#'
#' The \code{baseline_overrides} parameter uses "start_date/end_date" format:
#' \itemize{
#'   \item Dates should match the segment boundaries from changepoints
#'   \item Format: "YYYY-MM-DD/YYYY-MM-DD"
#'   \item Example: "2024-01-01/2024-03-15" = 0.8
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Manual changepoints, auto-detect all baselines
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15", "2024-06-10"),
#'   sensor_position = "outer",
#'   method = "HRM"
#' )
#'
#' # Example 2: Manual changepoints with some baseline overrides
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15", "2024-06-10"),
#'   baseline_overrides = list(
#'     "2024-01-01/2024-03-15" = 0.8,  # Segment 1: user-specified
#'     # Segment 2 will auto-detect (not specified)
#'     "2024-06-11/2024-12-31" = 1.5   # Segment 3: user-specified
#'   ),
#'   sensor_position = "outer"
#' )
#'
#' # Example 3: Specify all baselines manually
#' correction <- apply_manual_spacing_correction(
#'   vh_data = vh_results,
#'   manual_changepoints = c("2024-03-15"),
#'   baseline_overrides = list(
#'     "2024-01-01/2024-03-15" = 0.8,
#'     "2024-03-16/2024-12-31" = 1.2
#'   ),
#'   sensor_position = "outer"
#' )
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., Beverly, C.R., Ong, C.K.,
#'   Khan, A.A.H., & Bleby, T.M. (2001). An improved heat pulse method to
#'   measure low and reverse rates of sap flow in woody plants.
#'   *Tree Physiology*, 21(9), 589-598.
#'
#' @family spacing correction functions
#' @export
apply_manual_spacing_correction <- function(vh_data,
                                             manual_changepoints,
                                             baseline_overrides = NULL,
                                             sensor_position,
                                             method = "HRM",
                                             method_col = "method",
                                             vh_col = "Vh_cm_hr",
                                             correction_type = c("burgess", "linear"),
                                             k_assumed = 0.0025,
                                             probe_spacing = 0.5,
                                             measurement_time = 80,
                                             lookup_table = NULL,
                                             create_new_col = TRUE,
                                             verbose = TRUE) {

  # Match and validate correction type
  correction_type <- match.arg(correction_type)

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "sensor_position", method_col, vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!sensor_position %in% c("outer", "inner")) {
    stop("sensor_position must be 'outer' or 'inner'")
  }

  # Ensure datetime is POSIXct
  if (!inherits(vh_data$datetime, "POSIXct")) {
    vh_data$datetime <- as.POSIXct(vh_data$datetime)
  }

  # Filter data for specified sensor and method
  sensor_data <- vh_data[
    vh_data$sensor_position == sensor_position &
    vh_data[[method_col]] == method,
  ]

  if (nrow(sensor_data) == 0) {
    stop("No data found for sensor '", sensor_position, "' and method '", method, "'")
  }

  # Convert manual changepoints to Date or POSIXct
  if (is.null(manual_changepoints) || length(manual_changepoints) == 0) {
    manual_changepoints <- as.Date(character(0))
  } else if (is.character(manual_changepoints)) {
    # Try to parse as POSIXct first, fall back to Date if no time provided
    parsed_dt <- as.POSIXct(manual_changepoints, format = "%Y-%m-%d %H:%M:%S")
    if (any(is.na(parsed_dt))) {
      manual_changepoints <- as.Date(manual_changepoints)
    } else {
      manual_changepoints <- parsed_dt
    }
  } else if (!inherits(manual_changepoints, c("Date", "POSIXt"))) {
    stop("manual_changepoints must be character vector, Date, or POSIXt objects")
  }

  # Parse baseline overrides if provided
  segment_baselines <- list()
  if (!is.null(baseline_overrides)) {
    if (!is.list(baseline_overrides) || is.null(names(baseline_overrides))) {
      stop("baseline_overrides must be a named list with date range names like '2024-01-01/2024-03-15'")
    }

    for (range_name in names(baseline_overrides)) {
      # Parse "start_date/end_date" format
      date_parts <- strsplit(range_name, "/")[[1]]
      if (length(date_parts) != 2) {
        warning("Skipping invalid date range format: '", range_name, "' (expected 'start/end')")
        next
      }

      start_date <- as.Date(date_parts[1])
      end_date <- as.Date(date_parts[2])

      if (is.na(start_date) || is.na(end_date)) {
        warning("Skipping invalid dates in range: '", range_name, "'")
        next
      }

      if (start_date >= end_date) {
        warning("Skipping invalid date range (start >= end): '", range_name, "'")
        next
      }

      # Store baseline for this range
      segment_baselines[[range_name]] <- list(
        start = start_date,
        end = end_date,
        baseline = baseline_overrides[[range_name]]
      )
    }
  }

  # Create or validate lookup table
  if (is.null(lookup_table)) {
    if (verbose) {
      cat("Generating Burgess coefficient lookup table...\n")
    }
    lookup_table <- calculate_burgess_coefficients(
      k = k_assumed,
      x = probe_spacing,
      t = measurement_time
    )
  }

  # Identify sensor data mask relative to ORIGINAL vh_data
  # This ensures surgical write-back works correctly
  sensor_mask <- vh_data$sensor_position == sensor_position & 
                 vh_data[[method_col]] == method

  if (!any(sensor_mask)) {
    stop("No data found for sensor_position='", sensor_position, "' and method='", method, "'")
  }

  # Work with a subset but maintain mask relationship
  sensor_data <- vh_data[sensor_mask, ]

  # Initialise storage (same length as sensor_data)
  # Start with raw values, we will surgically update them
  corrected_vh <- sensor_data[[vh_col]]

  # Initialise correction factor tracking vectors (for transparency)
  coef_a_vector <- rep(NA_real_, nrow(sensor_data))
  coef_b_vector <- rep(NA_real_, nrow(sensor_data))
  baseline_offset_vector <- rep(NA_real_, nrow(sensor_data))
  applied_flag <- rep(FALSE, nrow(sensor_data))

  # Define segment boundaries from manual changepoints
  if (length(manual_changepoints) > 0) {
    cpt_datetimes <- as.POSIXct(as.character(manual_changepoints))
    # Add small buffer to ensure exact timestamp matches are included
    segment_starts <- c(min(sensor_data$datetime, na.rm = TRUE) - 1, cpt_datetimes)
    segment_ends <- c(cpt_datetimes, max(sensor_data$datetime, na.rm = TRUE) + 1)
  } else {
    # No changepoints - single segment covering the whole range
    segment_starts <- min(sensor_data$datetime, na.rm = TRUE) - 1
    segment_ends <- max(sensor_data$datetime, na.rm = TRUE) + 1
  }
  n_segments <- length(segment_starts)

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("MANUAL SPACING CORRECTION (k =", k_assumed, "cm^2/s)\n")
    cat(strrep("=", 72), "\n")
    cat("Sensor:", toupper(sensor_position), "| Method:", method, "\n")
    cat("Manual changepoints:", length(manual_changepoints), "\n")
    cat("Number of segments:", n_segments, "\n")
    if (length(segment_baselines) > 0) {
      cat("Baseline overrides:", length(segment_baselines), "segment(s)\n")
    }
    cat("\n")
  }

  # Process each segment
  segment_results <- list()

  for (seg_id in seq_len(n_segments)) {
    # Define segment mask relative to sensor_data
    seg_mask <- sensor_data$datetime > segment_starts[seg_id] & 
                sensor_data$datetime <= segment_ends[seg_id]
    
    if (!any(seg_mask)) next

    if (verbose) {
      cat(strrep("-", 72), "\n")
      cat("Processing Segment", seg_id, "of", n_segments, "\n")
      cat(strrep("-", 72), "\n")
      cat("Period:", format(segment_starts[seg_id] + 1, "%Y-%m-%d %H:%M"), "to",
          format(segment_ends[seg_id] - 1, "%Y-%m-%d %H:%M"), "\n")
    }

    # Extract segment data
    seg_data <- sensor_data[seg_mask, ]

    if (nrow(seg_data) == 0) {
      if (verbose) {
        cat("  [FAIL] No data in this segment - skipping\n\n")
      }
      next
    }

    if (verbose) {
      cat("  Observations:", nrow(seg_data), "\n")
    }

    # Get Vh values for this segment
    vh_values <- seg_data[[vh_col]]
    vh_values_clean <- vh_values[!is.na(vh_values) & is.finite(vh_values)]

    if (length(vh_values_clean) == 0) {
      if (verbose) {
        cat("  [FAIL] No valid Vh values in segment - skipping\n\n")
      }
      next
    }

    # Determine zero offset for this segment
    # Check if user specified a baseline for this segment
    seg_start_date <- as.Date(segment_starts[seg_id])
    seg_end_date <- as.Date(segment_ends[seg_id])

    user_baseline <- NULL
    baseline_source <- "auto-detected"

    # Search for matching baseline override
    for (range_name in names(segment_baselines)) {
      override <- segment_baselines[[range_name]]
      # Check if this segment falls within the specified range
      if (seg_start_date >= override$start && seg_end_date <= override$end) {
        user_baseline <- override$baseline
        baseline_source <- "user-specified"
        break
      }
    }

    # Use user baseline if specified, otherwise auto-detect
    if (!is.null(user_baseline)) {
      zero_vh <- round(user_baseline, 1)
      if (verbose) {
        cat("  Baseline (user-specified):", zero_vh, "cm/hr\n")
      }
    } else {
      zero_vh <- round(min(vh_values_clean, na.rm = TRUE), 1)
      if (verbose) {
        cat("  Baseline (auto-detected min):", zero_vh, "cm/hr\n")
      }
    }

    # Calculate segment statistics
    mean_vh <- mean(vh_values_clean, na.rm = TRUE)
    sd_vh <- sd(vh_values_clean, na.rm = TRUE)
    cv <- sd_vh / abs(mean_vh)

    if (verbose) {
      cat("  Mean Vh:", round(mean_vh, 2), "cm/hr\n")
      cat("  SD:", round(sd_vh, 2), "| CV:", round(cv, 3), "\n")
    }

    # Get correction coefficients
    tryCatch({
      if (correction_type == "linear") {
        # Simple linear offset: a = 1, b = -zero_vh
        coef_result <- list(
          coef_a = 1.0,
          coef_b = -zero_vh,
          zero_vh = zero_vh,
          range_type = "linear",
          severity = "none",
          warning = NULL,
          correction_formula = paste0("Vh_corrected = 1.0 * Vh + (", -zero_vh, ")")
        )
      } else {
        # Physics-based Burgess coefficients
        coef_result <- get_correction_coefficients(
          zero_vh = zero_vh,
          lookup_table = lookup_table
        )
      }

      if (verbose) {
        cat("  Correction formula:", coef_result$correction_formula, "\n")
        if (correction_type == "burgess") {
          cat("  Severity:", toupper(coef_result$severity), "\n")
        }
      }

      # Apply correction to this segment
      corrected_vh[seg_mask] <- coef_result$coef_a * vh_values + coef_result$coef_b

      # Store correction factors for this segment (for transparency)
      coef_a_vector[seg_mask] <- coef_result$coef_a
      coef_b_vector[seg_mask] <- coef_result$coef_b
      baseline_offset_vector[seg_mask] <- zero_vh

      # Store segment results
      segment_results[[seg_id]] <- list(
        segment_id = seg_id,
        start_datetime = segment_starts[seg_id],
        end_datetime = segment_ends[seg_id],
        n_observations = nrow(seg_data),
        zero_vh = zero_vh,
        baseline_source = baseline_source,
        mean_vh = mean_vh,
        sd_vh = sd_vh,
        cv = cv,
        coef_a = coef_result$coef_a,
        coef_b = coef_result$coef_b,
        range_type = coef_result$range_type,
        severity = coef_result$severity,
        correction_formula = coef_result$correction_formula
      )

    }, error = function(e) {
      if (verbose) {
        cat("  [FAIL] Error getting correction coefficients:", e$message, "\n")
      }
      warning("Segment ", seg_id, ": ", e$message, call. = FALSE)
      corrected_vh[seg_mask] <- vh_values  # Use uncorrected
    })

    if (verbose) cat("\n")
  }

  # Apply corrections to full dataset
  corrected_data <- vh_data

  if (create_new_col) {
    corrected_col <- paste0(vh_col, "_sc")
    corrected_data[[corrected_col]] <- NA_real_
    corrected_data$spacing_correction_applied <- FALSE
  } else {
    corrected_col <- vh_col
    if (!"spacing_correction_applied" %in% names(corrected_data)) {
      corrected_data$spacing_correction_applied <- FALSE
    }
  }

  # Insert corrected values for this sensor/method
  # AGGRESSIVE FLATTENING: Force to plain numeric vector to prevent nested dataframe bug
  corrected_vh_flat <- as.numeric(unlist(corrected_vh))
  
  sensor_method_mask <- corrected_data$sensor_position == sensor_position &
                        corrected_data[[method_col]] == method

  corrected_data[[corrected_col]][sensor_method_mask] <- corrected_vh_flat
  corrected_data$spacing_correction_applied[sensor_method_mask] <- TRUE

  # Add correction factor columns (for transparency - matches wound correction pattern)
  if (!"spacing_correction_a" %in% names(corrected_data)) {
    corrected_data$spacing_correction_a <- NA_real_
    corrected_data$spacing_correction_b <- NA_real_
    corrected_data$baseline_offset_cm_hr <- NA_real_
  }
  corrected_data$spacing_correction_a[sensor_method_mask] <- coef_a_vector
  corrected_data$spacing_correction_b[sensor_method_mask] <- coef_b_vector
  corrected_data$baseline_offset_cm_hr[sensor_method_mask] <- baseline_offset_vector

  # Create metadata
  metadata <- list(
    method = method,
    sensor_position = sensor_position,
    k_assumed = k_assumed,
    probe_spacing = probe_spacing,
    measurement_time = measurement_time,
    n_segments = length(segment_results),
    manual_changepoints = as.character(manual_changepoints),
    n_baseline_overrides = length(segment_baselines),
    date_applied = Sys.time(),
    approach = "Manual changepoint specification",
    sapfluxr_version = packageVersion("sapfluxr")
  )

  if (verbose) {
    cat(strrep("=", 72), "\n")
    cat("[OK] Manual spacing correction complete!\n")
    cat("  ", length(segment_results), "segments processed\n")
    n_user_specified <- sum(sapply(segment_results, function(x) x$baseline_source == "user-specified"))
    n_auto_detected <- sum(sapply(segment_results, function(x) x$baseline_source == "auto-detected"))
    cat("  ", n_user_specified, "user-specified baseline(s)\n")
    cat("  ", n_auto_detected, "auto-detected baseline(s)\n")
    n_corrected <- sum(corrected_data$spacing_correction_applied, na.rm = TRUE)
    cat("  ", n_corrected, "observations corrected\n")
    cat("\nCorrection factor columns added:\n")
    cat("  spacing_correction_a - Slope coefficient (a)\n")
    cat("  spacing_correction_b - Intercept coefficient (b)\n")
    cat("  baseline_offset_cm_hr - Zero-flow baseline (cm/hr)\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  result <- list(
    vh_corrected = corrected_data,
    segment_results = segment_results,
    metadata = metadata
  )

  class(result) <- c("manual_spacing_correction_result", "list")

  return(result)
}



