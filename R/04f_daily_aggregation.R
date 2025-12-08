# ============================================================================
# 04f_daily_aggregation.R
# ============================================================================
# Daily temporal aggregation of sap flux metrics
#
# This module implements Step 9 of the sap flow analysis workflow:
# - Daily total mean sap flux density (Jvm_daily, cm³/cm²/day)
# - Daily total sap flux (Qp_daily, L/day)
# - Optional daily normalisation
#
# Temporal aggregation enables:
# - Seasonal trend analysis
# - Comparison across longer time periods
# - Removal of diurnal variation
# - Integration with daily weather/soil data
# ============================================================================

#' Aggregate sap flux to daily totals
#'
#' Calculates daily total sap flux metrics by integrating (summing) all
#' measurements over each 24-hour period. This produces daily mean sap flux
#' density (Jvm_daily) and/or daily total sap flux (Qp_daily).
#'
#' For sub-daily measurements (e.g., hourly, half-hourly), the sum must be
#' adjusted by the measurement interval to get correct daily totals.
#'
#' @param data Data frame containing sap flux measurements with datetime stamps.
#'   Typically output from calc_sap_flux() or apply_sapwood_metrics().
#' @param datetime_col Character. Name of column containing POSIXct datetime
#'   values. Default: "datetime".
#' @param flux_density_col Character. Name of column containing sap flux density
#'   (Qps, cm/hr) for calculating daily mean flux density. If NULL, Jvm_daily
#'   will not be calculated. Default: "Qps_cm_hr".
#' @param total_flux_col Character. Name of column containing total sap flux
#'   (Qp, cm³/hr) for calculating daily total flux. If NULL, Qp_daily will not
#'   be calculated. Default: "Q_cm3_hr".
#' @param interval Character. Measurement interval: "auto" (detect from data),
#'   "hourly", "half-hourly", or a duration string like "30 min". Default: "auto".
#' @param require_complete_days Logical. If TRUE, only include days with complete
#'   24-hour records (no missing timestamps). Default: FALSE.
#' @param min_measurements_per_day Integer. Minimum number of measurements
#'   required per day. Days with fewer measurements will have NA values.
#'   Default: NULL (no minimum).
#'
#' @return Data frame with one row per day containing:
#'   - date: Date (Date class)
#'   - datetime: Start of day (POSIXct, for plotting)
#'   - Jvm_daily_cm3_cm2_day: Daily total mean flux density (if flux_density_col provided)
#'   - Jvm_daily_mm_day: Same as above but in mm/day units (1 cm³/cm² = 10 mm)
#'   - Qp_daily_L_day: Daily total sap flux in L/day (if total_flux_col provided)
#'   - Qp_daily_cm3_day: Daily total sap flux in cm³/day (if total_flux_col provided)
#'   - n_measurements: Number of measurements in this day
#'   - hours_covered: Total hours spanned by measurements
#'   - data_completeness: Fraction of expected measurements present (0-1)
#'
#' @details
#' The daily integration formula depends on measurement interval:
#'
#' For hourly measurements (n=24):
#'   Jvm_daily = sum(Qps_i) \[cm/day\]
#'   Qp_daily = sum(Q_i) / 1000 \[L/day\]
#'
#' For half-hourly measurements (n=48):
#'   Jvm_daily = sum(Qps_i) / 2 \[cm/day\]
#'   Qp_daily = sum(Q_i) / 2 / 1000 \[L/day\]
#'
#' General formula:
#'   Jvm_daily = sum(Qps_i) × (interval_hours / 1)
#'   Qp_daily = sum(Q_i) × (interval_hours / 1) / 1000
#'
#' Units conversion:
#'   - Qps in cm/hr → Jvm_daily in cm³/cm²/day (= cm/day)
#'   - Q in cm³/hr → Qp_daily in L/day (divide by 1000)
#'   - Jvm in cm/day → mm/day (multiply by 10)
#'
#' @examples
#' \dontrun{
#' # After calculating sap flux metrics
#' hourly_data <- calc_sap_flux(flux_data, sapwood_areas)
#'
#' # Aggregate to daily totals
#' daily_data <- aggregate_daily(hourly_data)
#'
#' # Require complete days only
#' daily_data <- aggregate_daily(
#'   hourly_data,
#'   require_complete_days = TRUE
#' )
#'
#' # Specify measurement interval
#' daily_data <- aggregate_daily(
#'   half_hourly_data,
#'   interval = "half-hourly"
#' )
#' }
#'
#' @family daily aggregation
#' @export
aggregate_daily <- function(data, datetime_col = "datetime",
                             flux_density_col = "Qps_cm_hr",
                             total_flux_col = "Q_cm3_hr",
                             interval = "auto",
                             require_complete_days = FALSE,
                             min_measurements_per_day = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!datetime_col %in% names(data)) {
    stop("Column '", datetime_col, "' not found in data.\n",
         "  Available columns: ", paste(names(data), collapse = ", "))
  }

  if (!inherits(data[[datetime_col]], "POSIXct")) {
    stop("Column '", datetime_col, "' must be POSIXct class")
  }

  # Check that at least one flux column is provided
  has_flux_density <- !is.null(flux_density_col) && flux_density_col %in% names(data)
  has_total_flux <- !is.null(total_flux_col) && total_flux_col %in% names(data)

  if (!has_flux_density && !has_total_flux) {
    stop("At least one of flux_density_col or total_flux_col must be provided and exist in data")
  }

  if (!is.null(flux_density_col) && !flux_density_col %in% names(data)) {
    warning("flux_density_col '", flux_density_col, "' not found. Jvm_daily will not be calculated.")
    has_flux_density <- FALSE
  }

  if (!is.null(total_flux_col) && !total_flux_col %in% names(data)) {
    warning("total_flux_col '", total_flux_col, "' not found. Qp_daily will not be calculated.")
    has_total_flux <- FALSE
  }

  # Detect measurement interval
  if (interval == "auto") {
    interval_hours <- detect_interval(data[[datetime_col]])
  } else if (interval == "hourly") {
    interval_hours <- 1.0
  } else if (interval == "half-hourly") {
    interval_hours <- 0.5
  } else {
    # Try to parse as duration string (e.g., "30 min")
    interval_hours <- parse_interval(interval)
  }

  # Create date column
  data$date <- as.Date(data[[datetime_col]], tz = attr(data[[datetime_col]], "tzone"))

  # Calculate expected measurements per day
  expected_per_day <- 24 / interval_hours

  # Group by date and aggregate
  daily_list <- list()

  for (d in unique(data$date)) {
    day_data <- data[data$date == d, ]
    n_obs <- nrow(day_data)

    # Check completeness requirements
    if (require_complete_days && n_obs < expected_per_day) {
      next  # Skip incomplete days
    }

    if (!is.null(min_measurements_per_day) && n_obs < min_measurements_per_day) {
      next  # Skip days with insufficient data
    }

    # Calculate daily totals
    # Get timezone from original data
    orig_tz <- attr(data[[datetime_col]], "tzone")
    if (is.null(orig_tz) || length(orig_tz) == 0) {
      orig_tz <- "UTC"
    } else if (length(orig_tz) > 1) {
      orig_tz <- orig_tz[1]  # Use first element if vector
    }

    result <- data.frame(
      date = d,
      datetime = as.POSIXct(d, tz = orig_tz),  # Convert Date to POSIXct at midnight
      n_measurements = n_obs,
      stringsAsFactors = FALSE
    )

    # Calculate hours covered
    if (n_obs > 1) {
      time_span <- as.numeric(difftime(max(day_data[[datetime_col]]),
                                        min(day_data[[datetime_col]]),
                                        units = "hours"))
      result$hours_covered <- time_span + interval_hours  # Include last measurement
    } else {
      result$hours_covered <- interval_hours
    }

    result$data_completeness <- n_obs / expected_per_day

    # Daily mean flux density (Jvm_daily)
    if (has_flux_density) {
      Qps_sum <- sum(day_data[[flux_density_col]], na.rm = TRUE)

      # Jvm_daily = sum(Qps) × interval_hours
      # For hourly (interval_hours = 1): sum directly
      # For half-hourly (interval_hours = 0.5): sum/2
      result$Jvm_daily_cm3_cm2_day <- Qps_sum * interval_hours

      # Convert to mm/day (1 cm = 10 mm)
      result$Jvm_daily_mm_day <- result$Jvm_daily_cm3_cm2_day * 10
    }

    # Daily total flux (Qp_daily)
    if (has_total_flux) {
      Q_sum <- sum(day_data[[total_flux_col]], na.rm = TRUE)

      # Qp_daily = sum(Q) × interval_hours
      result$Qp_daily_cm3_day <- Q_sum * interval_hours

      # Convert to L/day
      result$Qp_daily_L_day <- result$Qp_daily_cm3_day / 1000
    }

    daily_list[[as.character(d)]] <- result
  }

  # Combine into dataframe
  if (length(daily_list) == 0) {
    stop("No days met the completeness requirements.\n",
         "  require_complete_days = ", require_complete_days, "\n",
         "  min_measurements_per_day = ", ifelse(is.null(min_measurements_per_day),
                                                  "NULL", min_measurements_per_day))
  }

  daily_data <- do.call(rbind, daily_list)
  rownames(daily_data) <- NULL

  # Print diagnostic summary
  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("DAILY AGGREGATION SUMMARY\n")
  cat("=", rep("=", 70), "=\n", sep = "")

  cat(sprintf("\nMeasurement interval: %.2f hours (%.0f measurements/day expected)\n",
              interval_hours, expected_per_day))
  cat(sprintf("Days processed: %d\n", nrow(daily_data)))
  cat(sprintf("Date range: %s to %s\n",
              min(daily_data$date), max(daily_data$date)))

  # Completeness statistics
  cat(sprintf("\nData completeness:\n"))
  cat(sprintf("  Mean: %.1f%% of expected measurements\n",
              mean(daily_data$data_completeness) * 100))
  cat(sprintf("  Range: %.1f%% - %.1f%%\n",
              min(daily_data$data_completeness) * 100,
              max(daily_data$data_completeness) * 100))

  if (require_complete_days) {
    cat(sprintf("  (Only complete days included)\n"))
  }

  # Daily flux density summary
  if (has_flux_density) {
    cat(sprintf("\nDaily mean flux density (Jvm_daily):\n"))
    cat(sprintf("  Range: %.2f - %.2f cm³/cm²/day (= cm/day)\n",
                min(daily_data$Jvm_daily_cm3_cm2_day, na.rm = TRUE),
                max(daily_data$Jvm_daily_cm3_cm2_day, na.rm = TRUE)))
    cat(sprintf("         %.2f - %.2f mm/day\n",
                min(daily_data$Jvm_daily_mm_day, na.rm = TRUE),
                max(daily_data$Jvm_daily_mm_day, na.rm = TRUE)))
    cat(sprintf("  Mean:  %.2f mm/day\n",
                mean(daily_data$Jvm_daily_mm_day, na.rm = TRUE)))
  }

  # Daily total flux summary
  if (has_total_flux) {
    cat(sprintf("\nDaily total sap flux (Qp_daily):\n"))
    cat(sprintf("  Range: %.2f - %.2f L/day\n",
                min(daily_data$Qp_daily_L_day, na.rm = TRUE),
                max(daily_data$Qp_daily_L_day, na.rm = TRUE)))
    cat(sprintf("  Mean:  %.2f L/day\n",
                mean(daily_data$Qp_daily_L_day, na.rm = TRUE)))
    cat(sprintf("  Total: %.2f L over %d days\n",
                sum(daily_data$Qp_daily_L_day, na.rm = TRUE),
                nrow(daily_data)))
  }

  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\n")

  return(daily_data)
}


#' Normalise daily sap flux by maximum value
#'
#' Normalises daily sap flux metrics (Jvm_daily or Qp_daily) with respect to
#' the maximum value observed during a specified period. This produces
#' dimensionless relative flux metrics ranging from 0 to 1.
#'
#' @param data Data frame containing daily aggregated sap flux data (from
#'   aggregate_daily()).
#' @param normalise_col Character. Name of column to normalise. Typically
#'   "Jvm_daily_cm3_cm2_day" or "Qp_daily_L_day". Default: "Jvm_daily_cm3_cm2_day".
#' @param period Character. Period over which to calculate maximum:
#'   - "global": Maximum across entire dataset (default)
#'   - "monthly": Maximum within each month
#'   - "seasonal": Maximum within each season (3-month periods)
#'   - "custom": Use provided max_value
#' @param max_value Numeric. Custom maximum value for normalisation. Only used
#'   if period = "custom". If NULL and period = "custom", will use global max.
#' @param output_col Character. Name for output normalised column. If NULL,
#'   will be auto-generated as paste0(normalise_col, "_normalised").
#'   Default: NULL.
#' @param date_col Character. Name of column containing date values. Default: "date".
#'
#' @return Input dataframe with added normalised column (values 0-1).
#'
#' @details
#' The formula is:
#'   normalised_value = value / max_value
#'
#' Where max_value is determined by the period parameter.
#'
#' Normalisation is useful for:
#' - Comparing seasonal patterns across years
#' - Removing absolute magnitude differences
#' - Emphasising relative temporal dynamics
#'
#' @examples
#' \dontrun{
#' # Aggregate to daily
#' daily_data <- aggregate_daily(hourly_data)
#'
#' # Global normalisation
#' daily_data <- normalise_daily(daily_data)
#'
#' # Monthly normalisation
#' daily_data <- normalise_daily(
#'   daily_data,
#'   period = "monthly"
#' )
#'
#' # Normalise total flux instead of flux density
#' daily_data <- normalise_daily(
#'   daily_data,
#'   normalise_col = "Qp_daily_L_day"
#' )
#' }
#'
#' @family daily aggregation
#' @export
normalise_daily <- function(data, normalise_col = "Jvm_daily_cm3_cm2_day",
                             period = "global", max_value = NULL,
                             output_col = NULL, date_col = "date") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!normalise_col %in% names(data)) {
    stop("Column '", normalise_col, "' not found in data.\n",
         "  Available columns: ", paste(names(data), collapse = ", "))
  }

  if (!date_col %in% names(data)) {
    stop("Column '", date_col, "' not found in data")
  }

  if (!period %in% c("global", "monthly", "seasonal", "custom")) {
    stop("period must be one of: 'global', 'monthly', 'seasonal', 'custom'")
  }

  # Determine output column name
  if (is.null(output_col)) {
    output_col <- paste0(normalise_col, "_normalised")
  }

  # Extract values to normalise
  values <- data[[normalise_col]]

  # Calculate appropriate maximum
  if (period == "global") {
    max_val <- max(values, na.rm = TRUE)
    data[[output_col]] <- values / max_val

  } else if (period == "custom") {
    if (is.null(max_value)) {
      warning("period = 'custom' but max_value not provided. Using global maximum.")
      max_val <- max(values, na.rm = TRUE)
    } else {
      if (!is.numeric(max_value) || length(max_value) != 1) {
        stop("max_value must be a single numeric value")
      }
      if (max_value <= 0) {
        stop("max_value must be positive")
      }
      max_val <- max_value
    }
    data[[output_col]] <- values / max_val

  } else if (period == "monthly") {
    # Monthly normalisation
    dates <- data[[date_col]]
    year_month <- format(dates, "%Y-%m")
    normalised <- numeric(length(values))

    for (ym in unique(year_month)) {
      idx <- which(year_month == ym)
      month_max <- max(values[idx], na.rm = TRUE)

      if (is.finite(month_max) && month_max > 0) {
        normalised[idx] <- values[idx] / month_max
      } else {
        normalised[idx] <- NA_real_
      }
    }
    data[[output_col]] <- normalised

  } else if (period == "seasonal") {
    # Seasonal normalisation (by 3-month periods)
    dates <- data[[date_col]]
    months <- as.numeric(format(dates, "%m"))

    # Define seasons: Dec-Feb (1), Mar-May (2), Jun-Aug (3), Sep-Nov (4)
    seasons <- ceiling((months %% 12) / 3)
    seasons[seasons == 0] <- 4  # December
    year_season <- paste0(format(dates, "%Y"), "-S", seasons)

    normalised <- numeric(length(values))

    for (ys in unique(year_season)) {
      idx <- which(year_season == ys)
      season_max <- max(values[idx], na.rm = TRUE)

      if (is.finite(season_max) && season_max > 0) {
        normalised[idx] <- values[idx] / season_max
      } else {
        normalised[idx] <- NA_real_
      }
    }
    data[[output_col]] <- normalised
  }

  # Print diagnostic summary
  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("DAILY NORMALISATION SUMMARY\n")
  cat("=", rep("=", 70), "=\n", sep = "")

  cat(sprintf("\nNormalised column: %s\n", normalise_col))
  cat(sprintf("Output column: %s\n", output_col))
  cat(sprintf("Normalisation period: %s\n", period))

  if (period == "global" || period == "custom") {
    if (period == "custom" && !is.null(max_value)) {
      cat(sprintf("Maximum value: %.2f (custom)\n", max_value))
    } else {
      cat(sprintf("Maximum value: %.2f (global)\n", max_val))
    }
  }

  normalised_range <- range(data[[output_col]], na.rm = TRUE)
  cat(sprintf("\nNormalised range: %.4f - %.4f\n",
              normalised_range[1], normalised_range[2]))
  cat(sprintf("Mean normalised value: %.4f\n",
              mean(data[[output_col]], na.rm = TRUE)))

  cat("=", rep("=", 70), "=\n", sep = "")
  cat("\n")

  return(data)
}


# ============================================================================
# Internal helper functions
# ============================================================================

#' Detect measurement interval from datetime stamps
#'
#' @param datetime POSIXct vector
#' @return Numeric. Interval in hours
#' @keywords internal
detect_interval <- function(datetime) {
  if (length(datetime) < 2) {
    stop("Need at least 2 datetime values to detect interval")
  }

  # Sort datetime
  datetime <- sort(datetime)

  # Calculate differences
  diffs <- as.numeric(difftime(datetime[-1], datetime[-length(datetime)],
                               units = "hours"))

  # Remove outliers (gaps in data)
  # Use median instead of mode to be robust to irregular spacing
  interval_hours <- median(diffs, na.rm = TRUE)

  # Round to common intervals
  if (abs(interval_hours - 1.0) < 0.1) {
    interval_hours <- 1.0  # Hourly
  } else if (abs(interval_hours - 0.5) < 0.05) {
    interval_hours <- 0.5  # Half-hourly
  } else if (abs(interval_hours - 0.25) < 0.05) {
    interval_hours <- 0.25  # 15-minute
  }

  return(interval_hours)
}


#' Parse interval string to hours
#'
#' @param interval Character string like "30 min", "1 hour", etc.
#' @return Numeric. Interval in hours
#' @keywords internal
parse_interval <- function(interval) {
  # Try to parse common formats
  interval_lower <- tolower(interval)

  if (grepl("hour", interval_lower)) {
    # Extract number
    num <- as.numeric(sub("([0-9.]+).*", "\\1", interval_lower))
    return(num)
  } else if (grepl("min", interval_lower)) {
    # Extract number and convert to hours
    num <- as.numeric(sub("([0-9.]+).*", "\\1", interval_lower))
    return(num / 60)
  } else {
    stop("Could not parse interval string: '", interval, "'.\n",
         "  Use format like '30 min' or '1 hour', or specify 'hourly'/'half-hourly'")
  }
}
