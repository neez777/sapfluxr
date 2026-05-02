# R/utils_thermal_baseline.R
# Pre-pulse baseline temperature calculation methods

#' Calculate Pre-Pulse Baseline Temperature
#'
#' @description
#' Computes a single background (baseline) temperature scalar for each sensor
#' from the pre-pulse rows of a single heat pulse record. The scalar is later
#' subtracted from all post-pulse temperatures inside the C++ preprocessor to
#' produce delta-T values.
#'
#' Three methods are supported, selected via the \code{method} argument or the
#' \code{baseline.method} key in the analysis configuration
#' (see \code{\link{load_analysis_config}}):
#'
#' \describe{
#'   \item{\code{"mean_30s"}}{Mean of all pre-pulse rows. Numerically identical
#'     to the fixed 30-second average previously computed inside C++.}
#'   \item{\code{"mean_3s"}}{Mean of the last \emph{n} rows corresponding to
#'     \code{baseline.short_window_seconds} (default 3 s) immediately before
#'     the pulse. Reduces bias when a long-term background trend is present.}
#'   \item{\code{"slope_intercept"}}{Fits a simple OLS regression
#'     (\code{temperature ~ row_index}) per sensor over all pre-pulse rows,
#'     then evaluates the line at the pulse-onset row. Corrects for a linear
#'     pre-pulse drift.}
#' }
#'
#' @param pulse_data Data frame containing at minimum the columns \code{do},
#'   \code{di}, \code{uo}, \code{ui} (downstream-outer, downstream-inner,
#'   upstream-outer, upstream-inner temperatures). Only the first
#'   \code{pre_pulse_rows} rows are used.
#' @param pre_pulse_rows Integer. Number of rows that precede the heat pulse.
#' @param method Character. Baseline calculation method. One of
#'   \code{"mean_30s"}, \code{"mean_3s"}, or \code{"slope_intercept"}.
#'   Default: \code{NULL} (reads \code{baseline.method} from analysis
#'   configuration; see \code{\link{load_analysis_config}}).
#' @param sampling_interval Numeric. Seconds between consecutive measurements.
#'   Required when \code{method = "mean_3s"}; ignored otherwise.
#'
#' @return Named list with one scalar baseline value per sensor:
#'   \code{list(do = ..., di = ..., uo = ..., ui = ...)}.
#'
#' @keywords internal
calculate_baseline <- function(pulse_data,
                               pre_pulse_rows,
                               method            = NULL,
                               sampling_interval = NULL) {

  if (is.null(method))
    method <- get_analysis_param("baseline.method")

  allowed <- c("mean_30s", "mean_3s", "slope_intercept")
  if (!method %in% allowed) {
    stop(
      "baseline method '", method, "' is not recognised. ",
      "Must be one of: ", paste(allowed, collapse = ", "), "."
    )
  }

  sensors <- c("do", "di", "uo", "ui")
  end_row <- min(pre_pulse_rows, nrow(pulse_data))

  if (end_row < 1L) {
    stop("pre_pulse_rows must be >= 1 to compute a baseline.")
  }

  pre_data <- pulse_data[seq_len(end_row), sensors, drop = FALSE]

  if (method == "mean_30s") {
    baselines <- lapply(sensors, function(s) mean(pre_data[[s]], na.rm = TRUE))

  } else if (method == "mean_3s") {

    if (is.null(sampling_interval) || sampling_interval <= 0) {
      stop("sampling_interval must be a positive number when method = 'mean_3s'.")
    }
    short_secs   <- get_analysis_param("baseline.short_window_seconds")
    short_rows   <- max(1L, as.integer(round(short_secs / sampling_interval)))
    window_start <- max(1L, end_row - short_rows + 1L)
    window_data  <- pre_data[seq(window_start, end_row), , drop = FALSE]
    baselines    <- lapply(sensors, function(s) mean(window_data[[s]], na.rm = TRUE))

  } else {
    # slope_intercept: OLS per sensor evaluated at the pulse-onset row
    row_idx   <- seq_len(end_row)
    baselines <- lapply(sensors, function(s) {
      y     <- pre_data[[s]]
      valid <- !is.na(y)
      if (sum(valid) < 2L) {
        return(mean(y, na.rm = TRUE))
      }
      fit <- stats::lm.fit(
        cbind(1.0, as.numeric(row_idx[valid])),
        as.numeric(y[valid])
      )
      as.numeric(fit$coefficients[[1]] + fit$coefficients[[2]] * pre_pulse_rows)
    })
  }

  names(baselines) <- sensors
  baselines
}
