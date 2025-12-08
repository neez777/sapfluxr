# R/03a_zero_flow_offset.R
# Simple Linear Zero-Flow Offset Correction
#
# Provides empirical zero-flow offset correction suitable for all velocity
# calculation methods. This is an alternative to Burgess spacing correction
# that works universally but doesn't account for the physics of probe misalignment.

#' Apply Zero-Flow Linear Offset Correction
#'
#' Applies simple linear offset correction based on observed zero-flow periods.
#' This is an empirical correction suitable for all velocity calculation methods.
#'
#' @param vh_data Data frame with vh_results structure (output from
#'   \code{\link{calc_heat_pulse_velocity}})
#' @param zero_periods List of zero-flow periods. Each element should have:
#'   \itemize{
#'     \item \code{start}: Start datetime (POSIXct or character)
#'     \item \code{end}: End datetime (POSIXct or character)
#'   }
#' @param sensors Character vector of sensor positions to process.
#'   Default: c("outer", "inner")
#' @param methods Character vector of methods to correct. If NULL (default),
#'   applies to all methods present in the data.
#' @param vh_col Column containing current Vh values. Default: "Vh_cm_hr"
#' @param verbose Logical, print diagnostic messages. Default: TRUE
#'
#' @return Data frame with added columns:
#'   \item{Vh_cm_hr_zf}{Zero-flow offset corrected values}
#'   \item{Vh_cm_hr}{Updated to equal Vh_cm_hr_zf ("current" pointer)}
#'   \item{zero_flow_offset_applied}{Logical, TRUE for corrected rows}
#'
#'   Attributes updated:
#'   \item{current_vh_column}{"Vh_cm_hr_zf"}
#'   \item{corrections_applied}{Appends "zero_flow_offset"}
#'   \item{zero_flow_offset_results}{List of offset values per sensor/method}
#'
#' @details
#' **Correction Formula:**
#'
#' \code{Vh_corrected = Vh_raw - offset}
#'
#' Where \code{offset = mean(Vh)} during identified zero-flow periods
#'
#' **When to Use:**
#'
#' Use zero-flow offset correction when:
#' \itemize{
#'   \item You have identified genuine zero-flow periods
#'   \item You want simple empirical correction
#'   \item You're using non-HRM methods (MHR, Tmax)
#'   \item Offset is large (>5 cm/hr) and Burgess correction fails validation
#' }
#'
#' **Applicability:**
#'
#' Works for all methods: HRM, MHR, HRMXa, HRMXb, Tmax_Coh, Tmax_Klu
#'
#' **Alternative:**
#'
#' For HRM/HRMX methods, consider \code{\link{apply_spacing_correction_workflow}}
#' which uses Burgess et al. (2001) physics-based correction. Burgess correction
#' is more accurate but only validated for offsets ≤ ±5 cm/hr.
#'
#' @seealso
#' \code{\link{apply_spacing_correction_workflow}} for Burgess correction (HRM only)
#'
#' @family zero-flow corrections
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate velocities
#' vh_results <- calc_heat_pulse_velocity(data, methods = c("HRM", "MHR"))
#'
#' # Define zero-flow periods (e.g., nighttime with known zero flow)
#' zero_periods <- list(
#'   list(start = "2024-01-15 22:00:00", end = "2024-01-16 06:00:00"),
#'   list(start = "2024-01-16 22:00:00", end = "2024-01-17 06:00:00")
#' )
#'
#' # Apply zero-flow offset correction to all methods
#' vh_corrected <- apply_zero_flow_offset(vh_results, zero_periods)
#'
#' # Check results
#' attr(vh_corrected, "zero_flow_offset_results")
#'
#' # Apply to specific methods only
#' vh_corrected <- apply_zero_flow_offset(
#'   vh_results,
#'   zero_periods,
#'   methods = c("MHR", "Tmax_Coh")
#' )
#' }
apply_zero_flow_offset <- function(vh_data,
                                    zero_periods,
                                    sensors = c("outer", "inner"),
                                    methods = NULL,
                                    vh_col = "Vh_cm_hr",
                                    verbose = TRUE) {

  # Input validation
  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "sensor_position", "method", vh_col)
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.list(zero_periods) || length(zero_periods) == 0) {
    stop("zero_periods must be a non-empty list")
  }

  # Validate zero periods structure
  for (i in seq_along(zero_periods)) {
    period <- zero_periods[[i]]
    if (!all(c("start", "end") %in% names(period))) {
      stop("Each zero_period must have 'start' and 'end' elements")
    }
  }

  # If methods not specified, use all methods in data
  if (is.null(methods)) {
    methods <- unique(vh_data$method)
    methods <- methods[!is.na(methods)]
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 72), "\n")
    cat("ZERO-FLOW LINEAR OFFSET CORRECTION\n")
    cat(strrep("=", 72), "\n")
    cat("Sensors:", paste(sensors, collapse = ", "), "\n")
    cat("Methods:", paste(methods, collapse = ", "), "\n")
    cat("Zero-flow periods:", length(zero_periods), "\n")
    cat("\n")
  }

  # Create new columns
  vh_data$Vh_cm_hr_zf <- NA_real_
  vh_data$zero_flow_offset_applied <- FALSE

  offset_results <- list()

  # Process each sensor/method combination
  for (sensor in sensors) {
    for (method in methods) {

      # Filter data for this sensor/method
      sensor_method_data <- vh_data[
        vh_data$sensor_position == sensor &
        vh_data$method == method,
      ]

      if (nrow(sensor_method_data) == 0) {
        if (verbose) {
          cat(sprintf("  %s / %s: No data found - skipping\n", sensor, method))
        }
        next
      }

      # Extract data from zero-flow periods
      zero_flow_values <- c()
      for (period in zero_periods) {
        # Convert period times to POSIXct
        period_start <- if (inherits(period$start, "POSIXct")) {
          period$start
        } else {
          as.POSIXct(period$start, format = "%Y-%m-%d %H:%M:%S")
        }

        period_end <- if (inherits(period$end, "POSIXct")) {
          period$end
        } else {
          as.POSIXct(period$end, format = "%Y-%m-%d %H:%M:%S")
        }

        # Extract values in this period
        period_mask <- sensor_method_data$datetime >= period_start &
                       sensor_method_data$datetime <= period_end

        period_values <- sensor_method_data[[vh_col]][period_mask]
        period_values <- period_values[!is.na(period_values)]

        zero_flow_values <- c(zero_flow_values, period_values)
      }

      if (length(zero_flow_values) == 0) {
        warning("No data found in zero-flow periods for ",
                sensor, " / ", method, " - skipping")
        next
      }

      # Calculate mean offset
      offset <- mean(zero_flow_values)
      offset_sd <- sd(zero_flow_values)

      # Apply correction to all rows for this sensor/method
      mask <- vh_data$sensor_position == sensor & vh_data$method == method
      vh_data$Vh_cm_hr_zf[mask] <- vh_data[[vh_col]][mask] - offset
      vh_data$Vh_cm_hr[mask] <- vh_data$Vh_cm_hr_zf[mask]  # Update "current"
      vh_data$zero_flow_offset_applied[mask] <- TRUE

      # Store results
      result_key <- paste0(sensor, "_", method)
      offset_results[[result_key]] <- list(
        sensor = sensor,
        method = method,
        offset = offset,
        offset_sd = offset_sd,
        n_zero_flow_points = length(zero_flow_values),
        zero_flow_range = range(zero_flow_values)
      )

      if (verbose) {
        cat(sprintf("  %s / %s: offset = %.3f ± %.3f cm/hr (n = %d, range: [%.2f, %.2f])\n",
                    sensor, method, offset, offset_sd, length(zero_flow_values),
                    min(zero_flow_values), max(zero_flow_values)))
      }
    }
  }

  if (verbose) {
    cat("\n")
    cat("Zero-flow offset correction complete.\n")
    cat(strrep("=", 72), "\n")
    cat("\n")
  }

  # Update metadata attributes
  attr(vh_data, "current_vh_column") <- "Vh_cm_hr_zf"
  attr(vh_data, "corrections_applied") <- c(
    attr(vh_data, "corrections_applied"),
    "zero_flow_offset"
  )
  attr(vh_data, "zero_flow_offset_results") <- offset_results

  return(vh_data)
}
