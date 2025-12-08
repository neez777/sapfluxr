# ============================================================================
# 04e_sapwood_metrics.R
# ============================================================================
# Sapwood-area-weighted mean sap flux density and derived metrics
#
# This module implements Step 8 of the sap flow analysis workflow:
# - Sapwood-area-weighted mean sap flux density (Qps = Qp/As)
# - Optional temporal normalisation (Qpsn = Qps/Qps_max)
# - Optional leaf-area-specific flux (Qpl = Qp/Al)
#
# These metrics allow comparison across trees of different sizes and
# normalisation with respect to physiological drivers.
# ============================================================================

#' Calculate sapwood-area-weighted mean sap flux density
#'
#' Calculates the sapwood-area-weighted mean sap flux density (Qps) by dividing
#' total tree sap flux (Qp) by total sapwood cross-sectional area (As). This
#' metric represents the average sap flux density across the entire sapwood and
#' allows meaningful comparison between trees of different sizes.
#'
#' This is also referred to as sapwood-area-weighted mean sap velocity (Jvm).
#'
#' @param Qp Numeric vector. Total tree sap flux in cm³/hr (from calc_sap_flux()).
#' @param As Numeric. Total sapwood cross-sectional area in cm² (from
#'   calc_sapwood_areas()$total_sapwood_area_cm2). Should be a single value
#'   or a vector matching the length of Qp.
#'
#' @return Numeric vector of sapwood-area-weighted mean sap flux density (Qps)
#'   in cm³/hr/cm², which is equivalent to cm/hr (velocity units).
#'
#' @details
#' The formula is:
#'   Qps = Qp / As
#'
#' Where:
#' - Qp = total tree sap flux (cm³/hr)
#' - As = total sapwood area (cm²)
#' - Qps = sapwood-area-weighted mean flux density (cm³/hr/cm² = cm/hr)
#'
#' This metric is conceptually similar to a weighted average velocity across
#' the entire sapwood profile, accounting for the non-uniform distribution
#' of sap flux density with depth.
#'
#' @examples
#' \dontrun{
#' # After calculating sap flux
#' sapwood_areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)
#' flux_results <- calc_sap_flux(flux_data, sapwood_areas)
#'
#' # Calculate mean flux density
#' Qps <- calc_mean_flux_density(
#'   Qp = flux_results$Q_cm3_hr,
#'   As = sapwood_areas$total_sapwood_area_cm2
#' )
#' }
#'
#' @family sapwood metrics
#' @export
calc_mean_flux_density <- function(Qp, As) {
  # Input validation
  if (!is.numeric(Qp)) {
    stop("Qp must be numeric")
  }

  if (!is.numeric(As)) {
    stop("As must be numeric")
  }

  if (length(As) != 1 && length(As) != length(Qp)) {
    stop("As must be either a single value or match the length of Qp.\n",
         "  length(Qp) = ", length(Qp), "\n",
         "  length(As) = ", length(As))
  }

  if (any(As <= 0, na.rm = TRUE)) {
    stop("Sapwood area (As) must be positive.\n",
         "  Found As values: ", paste(unique(As), collapse = ", "))
  }

  # Calculate Qps = Qp / As
  Qps <- Qp / As

  return(Qps)
}


#' Normalise sap flux density by maximum value
#'
#' Normalises sapwood-area-weighted mean sap flux density (Qps) with respect to
#' the maximum value observed during a specified period. This produces a
#' dimensionless relative flux metric (Qpsn) ranging from 0 to 1.
#'
#' @param Qps Numeric vector. Sapwood-area-weighted mean sap flux density
#'   (cm³/hr/cm²) from calc_mean_flux_density().
#' @param period Character. Period over which to calculate maximum:
#'   - "global": Maximum across entire dataset (default)
#'   - "daily": Maximum within each day (requires datetime parameter)
#'   - "monthly": Maximum within each month (requires datetime parameter)
#'   - "custom": Use provided Qps_max value
#' @param datetime POSIXct vector. Datetime stamps corresponding to Qps values.
#'   Required if period is "daily" or "monthly". Optional otherwise.
#' @param Qps_max Numeric. Custom maximum value for normalisation. Only used
#'   if period = "custom". If NULL and period = "custom", will use global max.
#'
#' @return Numeric vector of normalised sap flux density (Qpsn), dimensionless
#'   values typically ranging from 0 to 1.
#'
#' @details
#' The formula is:
#'   Qpsn = Qps / Qps_max
#'
#' Where:
#' - Qps = sapwood-area-weighted mean flux density
#' - Qps_max = maximum Qps during specified period
#' - Qpsn = normalised flux density (0-1 range)
#'
#' Normalisation is useful for:
#' - Comparing temporal patterns across trees
#' - Removing size-dependent variation
#' - Emphasising relative changes in sap flux
#'
#' Note: NA values in Qps are preserved in the output. The maximum is
#' calculated ignoring NA values.
#'
#' @examples
#' \dontrun{
#' # Global normalisation
#' Qpsn <- normalise_flux_density(Qps)
#'
#' # Daily normalisation
#' Qpsn <- normalise_flux_density(
#'   Qps = flux_results$Qps,
#'   period = "daily",
#'   datetime = flux_results$datetime
#' )
#'
#' # Custom maximum (e.g., from calibration period)
#' Qpsn <- normalise_flux_density(
#'   Qps = flux_results$Qps,
#'   period = "custom",
#'   Qps_max = 5.2
#' )
#' }
#'
#' @family sapwood metrics
#' @export
normalise_flux_density <- function(Qps, period = "global", datetime = NULL,
                                     Qps_max = NULL) {
  # Input validation
  if (!is.numeric(Qps)) {
    stop("Qps must be numeric")
  }

  if (!period %in% c("global", "daily", "monthly", "custom")) {
    stop("period must be one of: 'global', 'daily', 'monthly', 'custom'")
  }

  # Check datetime requirement
  if (period %in% c("daily", "monthly") && is.null(datetime)) {
    stop("datetime is required when period = '", period, "'")
  }

  if (!is.null(datetime) && !inherits(datetime, "POSIXct")) {
    stop("datetime must be a POSIXct object")
  }

  if (!is.null(datetime) && length(datetime) != length(Qps)) {
    stop("datetime must match the length of Qps.\n",
         "  length(Qps) = ", length(Qps), "\n",
         "  length(datetime) = ", length(datetime))
  }

  # Calculate appropriate maximum
  if (period == "global") {
    Qps_max_val <- max(Qps, na.rm = TRUE)

  } else if (period == "custom") {
    if (is.null(Qps_max)) {
      warning("period = 'custom' but Qps_max not provided. Using global maximum.")
      Qps_max_val <- max(Qps, na.rm = TRUE)
    } else {
      if (!is.numeric(Qps_max) || length(Qps_max) != 1) {
        stop("Qps_max must be a single numeric value")
      }
      if (Qps_max <= 0) {
        stop("Qps_max must be positive")
      }
      Qps_max_val <- Qps_max
    }

  } else if (period == "daily") {
    # Daily maximum normalisation
    date <- as.Date(datetime)
    Qpsn <- numeric(length(Qps))

    for (d in unique(date)) {
      idx <- which(date == d)
      day_max <- max(Qps[idx], na.rm = TRUE)
      if (is.finite(day_max) && day_max > 0) {
        Qpsn[idx] <- Qps[idx] / day_max
      } else {
        Qpsn[idx] <- NA_real_
      }
    }
    return(Qpsn)

  } else if (period == "monthly") {
    # Monthly maximum normalisation
    year_month <- format(datetime, "%Y-%m")
    Qpsn <- numeric(length(Qps))

    for (ym in unique(year_month)) {
      idx <- which(year_month == ym)
      month_max <- max(Qps[idx], na.rm = TRUE)
      if (is.finite(month_max) && month_max > 0) {
        Qpsn[idx] <- Qps[idx] / month_max
      } else {
        Qpsn[idx] <- NA_real_
      }
    }
    return(Qpsn)
  }

  # Global or custom normalisation
  if (!is.finite(Qps_max_val) || Qps_max_val <= 0) {
    warning("Maximum Qps is not positive. Returning NA values.")
    return(rep(NA_real_, length(Qps)))
  }

  Qpsn <- Qps / Qps_max_val

  return(Qpsn)
}


#' Calculate leaf-area-specific sap flux
#'
#' Calculates sap flux per unit of supported leaf area (Qpl) by dividing total
#' tree sap flux (Qp) by total plant leaf area (Al). This metric relates water
#' uptake to transpiring leaf area and is useful for ecophysiological studies.
#'
#' @param Qp Numeric vector. Total tree sap flux in cm³/hr (from calc_sap_flux()).
#' @param Al Numeric. Total plant leaf area in m² (or cm²). Should be a single
#'   value or a vector matching the length of Qp.
#' @param Al_units Character. Units of leaf area: "m2" (default) or "cm2".
#'
#' @return Numeric vector of leaf-area-specific sap flux (Qpl) in cm³/hr/m²
#'   (if Al_units = "m2") or cm³/hr/cm² (if Al_units = "cm2").
#'
#' @details
#' The formula is:
#'   Qpl = Qp / Al
#'
#' Where:
#' - Qp = total tree sap flux (cm³/hr)
#' - Al = total plant leaf area (m² or cm²)
#' - Qpl = leaf-area-specific flux (cm³/hr/m² or cm³/hr/cm²)
#'
#' Leaf-area-specific flux is conceptually similar to transpiration rate and
#' can be compared to stomatal conductance measurements. It accounts for
#' differences in tree size through leaf area rather than sapwood area.
#'
#' Note: Leaf area measurements are challenging and often estimated from
#' allometric relationships. Measurement methods include:
#' - Direct harvest and scanning
#' - Allometric equations (from DBH, height, etc.)
#' - Leaf area index (LAI) measurements scaled to individual trees
#'
#' @examples
#' \dontrun{
#' # After calculating sap flux
#' flux_results <- calc_sap_flux(flux_data, sapwood_areas)
#'
#' # Calculate leaf-area-specific flux
#' Qpl <- calc_leaf_area_flux(
#'   Qp = flux_results$Q_cm3_hr,
#'   Al = 12.5,  # m²
#'   Al_units = "m2"
#' )
#' }
#'
#' @family sapwood metrics
#' @export
calc_leaf_area_flux <- function(Qp, Al, Al_units = "m2") {
  # Input validation
  if (!is.numeric(Qp)) {
    stop("Qp must be numeric")
  }

  if (!is.numeric(Al)) {
    stop("Al must be numeric")
  }

  if (!Al_units %in% c("m2", "cm2")) {
    stop("Al_units must be either 'm2' or 'cm2'")
  }

  if (length(Al) != 1 && length(Al) != length(Qp)) {
    stop("Al must be either a single value or match the length of Qp.\n",
         "  length(Qp) = ", length(Qp), "\n",
         "  length(Al) = ", length(Al))
  }

  if (any(Al <= 0, na.rm = TRUE)) {
    stop("Leaf area (Al) must be positive.\n",
         "  Found Al values: ", paste(unique(Al), collapse = ", "))
  }

  # Calculate Qpl = Qp / Al
  Qpl <- Qp / Al

  return(Qpl)
}


#' Apply sapwood metrics calculations to flux data
#'
#' Convenience wrapper function that applies sapwood-area-weighted mean flux
#' density calculation (and optional normalisation and leaf-area metrics) to a
#' dataframe containing sap flux results. This adds new columns to the input
#' dataframe.
#'
#' @param data Data frame containing sap flux results from calc_sap_flux() or
#'   apply_sap_flux_integration(). Must contain a column with total flux values.
#' @param flux_col Character. Name of column containing total sap flux (Qp) in
#'   cm³/hr. Default: "Q_cm3_hr".
#' @param sapwood_area Numeric. Total sapwood cross-sectional area (As) in cm².
#'   Can be a single value (assumes constant across all rows) or a vector
#'   matching the number of rows in data.
#' @param normalise Logical. Whether to calculate normalised flux density (Qpsn).
#'   Default: FALSE.
#' @param normalise_period Character. Period for normalisation: "global",
#'   "daily", "monthly", or "custom". Only used if normalise = TRUE.
#'   Default: "global".
#' @param Qps_max Numeric. Custom maximum for normalisation. Only used if
#'   normalise = TRUE and normalise_period = "custom". Default: NULL.
#' @param leaf_area Numeric. Total plant leaf area in m² (or cm²). If provided,
#'   calculates leaf-area-specific flux (Qpl). Default: NULL (not calculated).
#' @param leaf_area_units Character. Units of leaf area: "m2" or "cm2".
#'   Default: "m2".
#' @param datetime_col Character. Name of column containing datetime values.
#'   Required if normalise_period is "daily" or "monthly". Default: "datetime".
#'
#' @return Input dataframe with additional columns:
#'   - Qps_cm3_hr_cm2: Sapwood-area-weighted mean flux density (always added)
#'   - Qps_cm_hr: Same as Qps_cm3_hr_cm2 but emphasises velocity units (always added)
#'   - Qpsn: Normalised flux density (added if normalise = TRUE)
#'   - Qpl_cm3_hr_m2 or Qpl_cm3_hr_cm2: Leaf-area-specific flux (added if leaf_area provided)
#'
#' @details
#' This function streamlines the calculation of derived metrics from total sap
#' flux, allowing easy comparison across trees and normalisation for temporal
#' analysis.
#'
#' The function prints a diagnostic summary showing:
#' - Sapwood area used
#' - Range of Qps values
#' - Normalisation details (if applicable)
#' - Leaf area details (if applicable)
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- apply_sapwood_metrics(
#'   data = flux_results,
#'   sapwood_area = 245.5
#' )
#'
#' # With normalisation
#' results <- apply_sapwood_metrics(
#'   data = flux_results,
#'   sapwood_area = 245.5,
#'   normalise = TRUE,
#'   normalise_period = "daily"
#' )
#'
#' # With leaf area flux
#' results <- apply_sapwood_metrics(
#'   data = flux_results,
#'   sapwood_area = 245.5,
#'   leaf_area = 12.5,
#'   leaf_area_units = "m2"
#' )
#' }
#'
#' @family sapwood metrics
#' @export
apply_sapwood_metrics <- function(data, flux_col = "Q_cm3_hr",
                                    sapwood_area,
                                    normalise = FALSE,
                                    normalise_period = "global",
                                    Qps_max = NULL,
                                    leaf_area = NULL,
                                    leaf_area_units = "m2",
                                    datetime_col = "datetime") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!flux_col %in% names(data)) {
    stop("Column '", flux_col, "' not found in data.\n",
         "  Available columns: ", paste(names(data), collapse = ", "))
  }

  if (missing(sapwood_area)) {
    stop("sapwood_area is required")
  }

  if (normalise && normalise_period %in% c("daily", "monthly")) {
    if (!datetime_col %in% names(data)) {
      stop("Column '", datetime_col, "' not found in data.\n",
           "  Required for normalise_period = '", normalise_period, "'")
    }
  }

  # Calculate Qps
  data$Qps_cm3_hr_cm2 <- calc_mean_flux_density(
    Qp = data[[flux_col]],
    As = sapwood_area
  )

  # Add velocity-equivalent column (same values, different interpretation)
  data$Qps_cm_hr <- data$Qps_cm3_hr_cm2

  # Calculate normalised flux if requested
  if (normalise) {
    datetime_vec <- if (normalise_period %in% c("daily", "monthly")) {
      data[[datetime_col]]
    } else {
      NULL
    }

    data$Qpsn <- normalise_flux_density(
      Qps = data$Qps_cm3_hr_cm2,
      period = normalise_period,
      datetime = datetime_vec,
      Qps_max = Qps_max
    )
  }

  # Calculate leaf-area-specific flux if requested
  if (!is.null(leaf_area)) {
    Qpl <- calc_leaf_area_flux(
      Qp = data[[flux_col]],
      Al = leaf_area,
      Al_units = leaf_area_units
    )

    col_name <- paste0("Qpl_cm3_hr_", leaf_area_units)
    data[[col_name]] <- Qpl
  }

  # Print diagnostic summary
  cat("\n")
  cat("=", rep("=", 70), "=\n", sep = "")
  cat("SAPWOOD METRICS CALCULATION SUMMARY\n")
  cat("=" , rep("=", 70), "=\n", sep = "")

  # Sapwood area
  if (length(sapwood_area) == 1) {
    cat(sprintf("\nSapwood area (As): %.2f cm²\n", sapwood_area))
  } else {
    cat(sprintf("\nSapwood area (As): %.2f - %.2f cm² (variable)\n",
                min(sapwood_area, na.rm = TRUE),
                max(sapwood_area, na.rm = TRUE)))
  }

  # Qps range
  Qps_range <- range(data$Qps_cm3_hr_cm2, na.rm = TRUE)
  cat(sprintf("\nSapwood-area-weighted mean flux density (Qps):\n"))
  cat(sprintf("  Range: %.4f - %.4f cm³/hr/cm² (= cm/hr)\n",
              Qps_range[1], Qps_range[2]))
  cat(sprintf("  Mean:  %.4f cm/hr\n",
              mean(data$Qps_cm3_hr_cm2, na.rm = TRUE)))

  # Normalisation details
  if (normalise) {
    cat(sprintf("\nNormalisation:\n"))
    cat(sprintf("  Period: %s\n", normalise_period))

    if (normalise_period == "custom" && !is.null(Qps_max)) {
      cat(sprintf("  Qps_max: %.4f cm/hr\n", Qps_max))
    } else if (normalise_period == "global") {
      cat(sprintf("  Qps_max: %.4f cm/hr (global maximum)\n", Qps_range[2]))
    }

    Qpsn_range <- range(data$Qpsn, na.rm = TRUE)
    cat(sprintf("  Qpsn range: %.4f - %.4f (dimensionless)\n",
                Qpsn_range[1], Qpsn_range[2]))
  }

  # Leaf area details
  if (!is.null(leaf_area)) {
    cat(sprintf("\nLeaf-area-specific flux (Qpl):\n"))

    if (length(leaf_area) == 1) {
      cat(sprintf("  Leaf area (Al): %.2f %s\n", leaf_area, leaf_area_units))
    } else {
      cat(sprintf("  Leaf area (Al): %.2f - %.2f %s (variable)\n",
                  min(leaf_area, na.rm = TRUE),
                  max(leaf_area, na.rm = TRUE),
                  leaf_area_units))
    }

    col_name <- paste0("Qpl_cm3_hr_", leaf_area_units)
    Qpl_range <- range(data[[col_name]], na.rm = TRUE)
    cat(sprintf("  Qpl range: %.2f - %.2f cm³/hr/%s\n",
                Qpl_range[1], Qpl_range[2], leaf_area_units))
  }

  cat("\nColumns added:\n")
  cat("  - Qps_cm3_hr_cm2 (sapwood-area-weighted mean flux density)\n")
  cat("  - Qps_cm_hr (equivalent velocity representation)\n")
  if (normalise) {
    cat("  - Qpsn (normalised flux density)\n")
  }
  if (!is.null(leaf_area)) {
    cat(sprintf("  - Qpl_cm3_hr_%s (leaf-area-specific flux)\n", leaf_area_units))
  }

  cat("=" , rep("=", 70), "=\n", sep = "")
  cat("\n")

  return(data)
}
