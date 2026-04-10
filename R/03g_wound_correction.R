# R/03b_wound_correction.R
# Wound correction functions for heat pulse velocity measurements
# Based on ICT International SFM1 User Manual and Burgess et al. (2001)

#' @importFrom dplyr %>% filter select mutate left_join case_when
NULL

# =============================================================================
# WOUND CORRECTION COEFFICIENT LOOKUP TABLES
# Based on ICT Manual Appendix 23.1, derived from Swanson & Whitfield (1981)
# Modified by Burgess et al. (2001) for symmetric probe configurations
# =============================================================================

#' Wound Correction Coefficients for 5mm Probe Spacing
#'
#' Lookup table for wound correction coefficients at standard 5mm probe spacing.
#' Linear coefficient (B) provides excellent fit (r^2 > 0.998) for most applications.
#'
#' @format Data frame with columns:
#'   \describe{
#'     \item{wound_diameter_cm}{Wound diameter in cm (drill bit size)}
#'     \item{b}{Polynomial coefficient b}
#'     \item{c}{Polynomial coefficient c}
#'     \item{d}{Polynomial coefficient d}
#'     \item{B_linear}{Linear correction coefficient (recommended)}
#'     \item{r_squared}{R^2 for linear approximation}
#'   }
#'
#' @details
#' Coefficients apply correction: Vc = B \* Vh (linear) or Vc = b\*Vh + c\*Vh^2 + d\*Vh^3 (polynomial)
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., et al. (2001). Tree Physiology 21: 589-598.
#' ICT International (2020). SFM1 Sap Flow Meter User Manual Version 5.2, Appendix 23.1.
#'
#' @keywords internal
wound_coefficients_5mm <- data.frame(
  wound_diameter_cm = c(0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.26, 0.28, 0.30),
  b = c(1.6821, 1.7304, 1.7961, 1.8558, 1.9181, 1.9831, 2.0509, 2.0973, 2.2231, 2.3760, 2.5232),
  c = c(-0.0015, -0.0013, -0.0016, -0.0018, -0.0021, -0.0024, -0.0028, -0.0030, -0.0037, -0.0046, -0.0055),
  d = c(0.0002, 0.0002, 0.0002, 0.0003, 0.0003, 0.0004, 0.0004, 0.0005, 0.0006, 0.0008, 0.0010),
  B_linear = c(1.7283, 1.7853, 1.8568, 1.9216, 1.9891, 2.0594, 2.1326, 2.1825, 2.3176, 2.4813, 2.6383),
  r_squared = c(0.9993, 0.9992, 0.9991, 0.9990, 0.9989, 0.9988, 0.9987, 0.9987, 0.9985, 0.9983, 0.9982),
  stringsAsFactors = FALSE
)

#' Wound Correction Coefficients for 6mm Probe Spacing
#'
#' Lookup table for wound correction coefficients at legacy 6mm probe spacing.
#'
#' @format Same structure as wound_coefficients_5mm
#' @keywords internal
wound_coefficients_6mm <- data.frame(
  wound_diameter_cm = c(0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.26, 0.28, 0.30),
  b = c(1.6565, 1.7077, 1.7701, 1.8292, 1.8909, 1.9554, 2.0226, 2.0685, 2.1932, 2.3448, 2.4908),
  c = c(-0.0014, -0.0014, -0.0017, -0.0019, -0.0022, -0.0025, -0.0029, -0.0031, -0.0038, -0.0047, -0.0057),
  d = c(0.0002, 0.0002, 0.0002, 0.0003, 0.0003, 0.0004, 0.0004, 0.0005, 0.0006, 0.0008, 0.0010),
  B_linear = c(1.7023, 1.7585, 1.8265, 1.8905, 1.9572, 2.0267, 2.0991, 2.1482, 2.2817, 2.4467, 2.5985),
  r_squared = c(0.9993, 0.9992, 0.9991, 0.9990, 0.9989, 0.9988, 0.9987, 0.9987, 0.9985, 0.9984, 0.9983),
  stringsAsFactors = FALSE
)


# =============================================================================
# TEMPORAL WOUND DIAMETER CALCULATION
# =============================================================================

#' Calculate Time-Varying Wound Diameter
#'
#' Calculates wound diameter for given timestamps using linear interpolation
#' or segmented models. Accounts for wound expansion over time.
#'
#' @param timestamps POSIXct vector of measurement timestamps
#' @param wound_config List or WoodProperties object containing wound configuration.
#' @return Numeric vector of wound diameters (cm) for each timestamp
#' @export
calc_wound_diameter <- function(timestamps, wound_config) {

  if (!inherits(timestamps, "POSIXct")) stop("timestamps must be POSIXct")
  if (inherits(wound_config, "WoodProperties")) wound_config <- wound_config$wound_correction
  if (!is.list(wound_config)) stop("wound_config must be a list or WoodProperties object")
  if (is.null(wound_config$drill_bit_diameter_mm)) stop("wound_config must contain drill_bit_diameter_mm")

  wound_addition <- if (is.null(wound_config$wound_addition_mm)) 0.3 else wound_config$wound_addition_mm
  base_diameter_mm <- wound_config$drill_bit_diameter_mm + 2 * wound_addition

  # =========================================================================
  # MODEL B: Measured Wound Sizes Per Period (Step Function)
  # =========================================================================
  if (!is.null(wound_config$wound_at_reinstall_mm)) {
    wound_at_reinstall_mm <- wound_config$wound_at_reinstall_mm

    if (!is.null(wound_config$reinstall_dates)) {
      reinstall_dates <- as.POSIXct(wound_config$reinstall_dates)

      # VALIDATION: Model B MUST have one more diameter than reinstall dates
      if (length(wound_at_reinstall_mm) != length(reinstall_dates) + 1) {
        stop(sprintf("wound_at_reinstall_mm must have length = length(reinstall_dates) + 1. Expected %d, got %d.",
                    length(reinstall_dates) + 1, length(wound_at_reinstall_mm)))
      }
      if (is.unsorted(reinstall_dates)) stop("reinstall_dates must be in chronological order")

      # Using findInterval with rightmost.closed = FALSE:
      # x < vec[1] -> 0 (Period 1)
      # vec[1] <= x < vec[2] -> 1 (Period 2)
      # Map to wound_at_reinstall_mm indices (1-based)
      period_indices <- findInterval(timestamps, reinstall_dates, rightmost.closed = FALSE) + 1
      wound_diameter_mm <- wound_at_reinstall_mm[period_indices]
    } else {
      if (length(wound_at_reinstall_mm) != 1) stop("wound_at_reinstall_mm must have length 1 when no reinstall_dates provided")
      wound_diameter_mm <- rep(wound_at_reinstall_mm[1], length(timestamps))
    }
    return(wound_diameter_mm / 10)
  }

  # =========================================================================
  # MODEL A: Constant Healing Rate (Sawtooth Pattern)
  # =========================================================================

  # Determine installation starts
  if (!is.null(wound_config$reinstall_dates)) {
    reinstall_dates <- as.POSIXct(wound_config$reinstall_dates)
    if (is.unsorted(reinstall_dates)) stop("reinstall_dates must be in chronological order")

    if (!is.null(wound_config$initial_date)) {
      initial_date <- as.POSIXct(wound_config$initial_date)
      installation_starts <- c(initial_date, reinstall_dates)
    } else {
      installation_starts <- reinstall_dates
    }
  } else {
    if (!is.null(wound_config$initial_date)) {
      installation_starts <- as.POSIXct(wound_config$initial_date)
    } else {
      # Static default
      return(rep(base_diameter_mm / 10, length(timestamps)))
    }
  }

  # Determine growth rate
  growth_rate_mm_per_day <- 0
  if (!is.null(wound_config$final_diameter_mm) && !is.null(wound_config$initial_date) && !is.null(wound_config$final_date)) {
    initial_date <- as.POSIXct(wound_config$initial_date)
    final_date <- as.POSIXct(wound_config$final_date)
    if (final_date <= initial_date) stop("final_date must be after initial_date")
    days_between <- as.numeric(difftime(final_date, initial_date, units = "days"))
    growth_rate_mm_per_day <- (wound_config$final_diameter_mm - base_diameter_mm) / days_between
  }

  # Calculate diameter
  period_idx <- findInterval(timestamps, installation_starts, rightmost.closed = FALSE)
  period_idx[period_idx == 0] <- 1
  current_starts <- installation_starts[period_idx]

  # Growth rate is 0 before initial_date
  current_rates <- rep(growth_rate_mm_per_day, length(timestamps))
  before_initial <- timestamps < installation_starts[1]
  if (any(before_initial)) current_rates[before_initial] <- 0

  days_elapsed <- as.numeric(difftime(timestamps, current_starts, units = "days"))
  # Cap negative days (shouldn't happen with period_idx logic but for safety)
  days_elapsed <- pmax(0, days_elapsed)

  wound_diameter_mm <- base_diameter_mm + (days_elapsed * current_rates)

  # Cap at final diameter
  if (!is.null(wound_config$final_date) && !is.null(wound_config$final_diameter_mm)) {
    final_date <- as.POSIXct(wound_config$final_date)
    after_final <- timestamps > final_date
    if (any(after_final)) wound_diameter_mm[after_final] <- wound_config$final_diameter_mm
  }

  return(wound_diameter_mm / 10)
}

# =============================================================================
# MAIN WOUND CORRECTION FUNCTION
# =============================================================================

#' Apply Wound Correction to Heat Pulse Velocity Data
#'
#' @param vh_data Data frame containing velocity measurements.
#' @param wound_diameter Wound diameter in cm (optional).
#' @param probe_spacing Probe spacing: "5mm" or "6mm".
#' @param wood_properties Optional WoodProperties object.
#' @param use_spacing_corrected Logical. Default: TRUE
#' @param confirm_parameters Logical. Default: TRUE
#' @param verbose Logical. Default: TRUE
#' @return Corrected data frame.
#' @export
apply_wound_correction <- function(vh_data,
                                   wound_diameter = NULL,
                                   probe_spacing = "5mm",
                                   wood_properties = NULL,
                                   use_spacing_corrected = TRUE,
                                   confirm_parameters = TRUE,
                                   verbose = TRUE) {

  if (!is.data.frame(vh_data)) stop("vh_data must be a data frame")
  if (!all(c("datetime", "Vh_cm_hr") %in% names(vh_data))) stop("vh_data missing required columns")
  if (!probe_spacing %in% c("5mm", "6mm")) stop("probe_spacing must be '5mm' or '6mm'")

  wound_diameter_vector <- NULL
  use_temporal <- FALSE

  if (!is.null(wood_properties)) {
    if (is.character(wood_properties)) wood_properties <- load_wood_properties(wood_properties)

    if (inherits(wood_properties, "WoodProperties")) {
      wc <- wood_properties$wound_correction
      if (!is.null(wc$drill_bit_diameter_mm)) {
        wound_diameter_vector <- calc_wound_diameter(vh_data$datetime, wood_properties)
        use_temporal <- TRUE
      }
    }
  }

  if (!use_temporal) {
    if (is.null(wound_diameter) && !is.null(wood_properties)) {
      wound_diameter <- get_wound_diameter_from_config(wood_properties)
    }
    if (is.null(wound_diameter)) {
      if (interactive()) wound_diameter <- prompt_wound_diameter()
      else stop("wound_diameter must be specified")
    }
    wound_diameter_vector <- rep(wound_diameter, nrow(vh_data))
  }

  B_vector <- get_wound_correction_coefficient(wound_diameter_vector, probe_spacing)

  if (!"Vh_cm_hr_raw" %in% names(vh_data)) vh_data$Vh_cm_hr_raw <- vh_data$Vh_cm_hr
  has_sc <- "Vh_cm_hr_sc" %in% names(vh_data)
  input_col <- if (has_sc && use_spacing_corrected) "Vh_cm_hr_sc" else "Vh_cm_hr_raw"

  corrected_values <- vh_data[[input_col]] * B_vector
  vh_data$Vh_cm_hr_wc <- corrected_values
  vh_data$Vh_cm_hr <- ifelse(is.na(corrected_values), vh_data$Vh_cm_hr, corrected_values)
  vh_data$Vc_cm_hr <- vh_data$Vh_cm_hr # Legacy alias
  vh_data$wound_correction_applied <- TRUE
  vh_data$wound_correction_factor <- B_vector
  vh_data$wound_diameter_cm <- wound_diameter_vector

  attr(vh_data, "current_vh_column") <- "Vh_cm_hr_wc"
  attr(vh_data, "corrections_applied") <- c(attr(vh_data, "corrections_applied"), "wound")

  return(vh_data)
}

#' @keywords internal
get_wound_correction_coefficient <- function(wound_diameter, probe_spacing = "5mm") {
  coef_table <- if (probe_spacing == "5mm") wound_coefficients_5mm else wound_coefficients_6mm
  B_vector <- approx(x = coef_table$wound_diameter_cm, y = coef_table$B_linear, xout = wound_diameter, rule = 2)$y

  if (any(wound_diameter < min(coef_table$wound_diameter_cm), na.rm = TRUE)) {
    warning("Some wound diameters below table minimum. Using minimum coefficient.")
  }
  if (any(wound_diameter > max(coef_table$wound_diameter_cm), na.rm = TRUE)) {
    warning("Some wound diameters above table maximum. Using maximum coefficient.")
  }
  return(B_vector)
}

#' @keywords internal
get_wound_diameter_from_config <- function(wood_properties) {
  if (inherits(wood_properties, "WoodProperties")) {
    wc <- wood_properties$wound_correction
    if (!is.null(wc$wound_diameter_cm)) return(wc$wound_diameter_cm)
    if (!is.null(wc$drill_bit_diameter_mm)) return(wc$drill_bit_diameter_mm / 10)
    return(NULL)
  }
  if (is.character(wood_properties)) {
    wp <- tryCatch(load_wood_properties(wood_properties), error = function(e) NULL)
    if (!is.null(wp)) return(get_wound_diameter_from_config(wp))
  }
  return(NULL)
}

#' @keywords internal
prompt_wound_diameter <- function() {
  cat("\nEnter wound diameter in cm (e.g., 0.20): ")
  input <- as.numeric(trimws(readline()))
  if (is.na(input) || input <= 0) stop("Invalid wound diameter")
  return(input)
}

#' @export
list_wound_coefficients <- function(probe_spacing = "both") {
  result <- if (probe_spacing == "5mm") {
    wound_coefficients_5mm
  } else if (probe_spacing == "6mm") {
    wound_coefficients_6mm
  } else {
    list(
      "5mm" = wound_coefficients_5mm,
      "6mm" = wound_coefficients_6mm
    )
  }

  class(result) <- c("wound_coefficient_list", class(result))
  return(result)
}
