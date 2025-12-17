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
#' Linear coefficient (B) provides excellent fit (r² > 0.998) for most applications.
#'
#' @format Data frame with columns:
#'   \describe{
#'     \item{wound_diameter_cm}{Wound diameter in cm (drill bit size)}
#'     \item{b}{Polynomial coefficient b}
#'     \item{c}{Polynomial coefficient c}
#'     \item{d}{Polynomial coefficient d}
#'     \item{B_linear}{Linear correction coefficient (recommended)}
#'     \item{r_squared}{R² for linear approximation}
#'     \item{reinstall_dates}{POSIXct vector of reinstallation dates, optional}
#'     \item{reinstall_measured_diameters}{Numeric vector of measured wound diameters at each reinstallation (mm), optional. Enables Segmented Sawtooth Model with period-specific growth rates.}
#'   }
#'
#' @details
#' Coefficients apply correction: Vc = B × Vh (linear) or Vc = b×Vh + c×Vh² + d×Vh³ (polynomial)
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
#' between initial and final wound measurements. Accounts for wound expansion
#' over time.
#'
#' @param timestamps POSIXct vector of measurement timestamps
#' @param wound_config List or WoodProperties object containing wound configuration.
#'   If WoodProperties object, uses the wound_correction section.
#'   If list, must contain:
#'   \describe{
#'     \item{drill_bit_diameter_mm}{Drill bit diameter (mm)}
#'     \item{wound_addition_mm}{Wound tissue addition per side (mm), default 0.3}
#'     \item{initial_date}{Installation date (POSIXct or string)}
#'     \item{final_date}{Final measurement date (POSIXct or string), optional}
#'     \item{final_diameter_mm}{Final wound diameter (mm), optional}
#'   }
#'
#' @return Numeric vector of wound diameters (cm) for each timestamp
#'
#' @details
#' **Calculation Method:**
#'
#' Initial wound diameter = drill_bit_diameter + 2 × wound_addition
#'
#' If final measurements are provided, diameter is interpolated linearly:
#' \itemize{
#'   \item Before initial_date: Use initial diameter
#'   \item Between initial_date and final_date: Linear interpolation
#'   \item After final_date: Use final diameter
#' }
#'
#' If no final measurements, all timestamps use the initial diameter (static).
#'
#' **Units:**
#' Input in mm, output in cm (for compatibility with wound coefficient tables).
#'
#' @examples
#' \dontrun{
#' # Setup wound configuration
#' wound_cfg <- list(
#'   drill_bit_diameter_mm = 2.0,
#'   wound_addition_mm = 0.3,
#'   initial_date = as.POSIXct("2024-03-01"),
#'   final_date = as.POSIXct("2024-07-01"),
#'   final_diameter_mm = 2.6
#' )
#'
#' # Calculate for specific dates
#' dates <- as.POSIXct(c("2024-03-01", "2024-05-01", "2024-07-01"))
#' wound_diams <- calc_wound_diameter(dates, wound_cfg)
#' # Returns wound diameters in cm
#' }
#'
#' @family wound correction functions
#' @export
calc_wound_diameter <- function(timestamps, wound_config) {

  # -------------------------------------------------------------------------
  # Input validation and config extraction
  # -------------------------------------------------------------------------

  if (!inherits(timestamps, "POSIXct")) {
    stop("timestamps must be POSIXct")
  }

  # Extract wound config if WoodProperties object
  if (inherits(wound_config, "WoodProperties")) {
    wound_config <- wound_config$wound_correction
  }

  if (!is.list(wound_config)) {
    stop("wound_config must be a list or WoodProperties object")
  }

  # Check required fields
  if (is.null(wound_config$drill_bit_diameter_mm)) {
    stop("wound_config must contain drill_bit_diameter_mm")
  }

  # Set defaults
  wound_addition <- if (is.null(wound_config$wound_addition_mm)) 0.3 else wound_config$wound_addition_mm

  # -------------------------------------------------------------------------
  # STEP 1: Determine wound tracking model and validate parameters
  # -------------------------------------------------------------------------

  # Base diameter = drill bit + wound tissue addition on both sides
  base_diameter_mm <- wound_config$drill_bit_diameter_mm + 2 * wound_addition

  # Detect which model to use based on parameters provided
  has_model_a_params <- !is.null(wound_config$initial_date) &&
                        !is.null(wound_config$final_date) &&
                        !is.null(wound_config$final_diameter_mm)

  has_model_b_params <- !is.null(wound_config$wound_at_reinstall_mm)

  # Validate: cannot use both models simultaneously
  if (has_model_a_params && has_model_b_params) {
    stop("Cannot specify both Model A (initial_date/final_date/final_diameter_mm) and Model B (wound_at_reinstall_mm) parameters simultaneously. Choose one approach.")
  }

  # Initialize model flag and growth rate
  use_model_b <- has_model_b_params
  growth_rate_mm_per_day <- 0  # Default for Model A with no temporal tracking

  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # MODEL A: Healing Rate Calculation
  # -------------------------------------------------------------------------
  # Two sub-models:
  #   - Global Rate: Single growth rate from initial to final (original)
  #   - Segmented Rate: Period-specific rates using intermediate measurements

  period_growth_rates <- NULL  # Will hold period-specific rates if segmented model
  use_segmented_rates <- FALSE

  if (has_model_a_params) {
    initial_date <- as.POSIXct(wound_config$initial_date)
    final_date <- as.POSIXct(wound_config$final_date)
    final_diameter_mm <- wound_config$final_diameter_mm

    # Validate dates
    if (final_date <= initial_date) {
      stop("final_date must be after initial_date")
    }

    # Check if segmented model requested via reinstall_measured_diameters
    if (!is.null(wound_config$reinstall_measured_diameters)) {
      # ===================================================================
      # SEGMENTED SAWTOOTH MODEL: Period-specific growth rates
      # ===================================================================

      if (is.null(wound_config$reinstall_dates)) {
        stop("reinstall_measured_diameters requires reinstall_dates to be specified")
      }

      reinstall_dates <- as.POSIXct(wound_config$reinstall_dates)
      reinstall_measured_diameters <- wound_config$reinstall_measured_diameters

      # Validate: must have same length
      if (length(reinstall_measured_diameters) != length(reinstall_dates)) {
        stop(sprintf("reinstall_measured_diameters must have same length as reinstall_dates. Expected %d, got %d.",
                    length(reinstall_dates), length(reinstall_measured_diameters)))
      }

      # Validate chronological order
      if (is.unsorted(reinstall_dates)) {
        stop("reinstall_dates must be in chronological order")
      }

      # Calculate period-specific growth rates
      # Period 1: (Measured[1] - Base) / (ReinstallDate[1] - InitialDate)
      # Period i: (Measured[i] - Base) / (ReinstallDate[i] - ReinstallDate[i-1])
      # Final Period: (FinalDiam - Base) / (FinalDate - ReinstallDate[Last])

      n_periods <- length(reinstall_dates) + 1  # Initial period + periods after each reinstall
      period_growth_rates <- numeric(n_periods)

      # Period 1: Initial to first reinstall
      days_period1 <- as.numeric(difftime(reinstall_dates[1], initial_date, units = "days"))
      diameter_increase_period1 <- reinstall_measured_diameters[1] - base_diameter_mm
      period_growth_rates[1] <- diameter_increase_period1 / days_period1

      # Periods 2 to n-1: Between reinstalls
      if (length(reinstall_dates) > 1) {
        for (j in 2:length(reinstall_dates)) {
          days_period <- as.numeric(difftime(reinstall_dates[j], reinstall_dates[j-1], units = "days"))
          diameter_increase <- reinstall_measured_diameters[j] - base_diameter_mm
          period_growth_rates[j] <- diameter_increase / days_period
        }
      }

      # Final period: Last reinstall to final date
      days_final <- as.numeric(difftime(final_date, reinstall_dates[length(reinstall_dates)], units = "days"))
      diameter_increase_final <- final_diameter_mm - base_diameter_mm
      period_growth_rates[n_periods] <- diameter_increase_final / days_final

      use_segmented_rates <- TRUE

      message("Using Segmented Sawtooth Model with ", n_periods, " period-specific growth rates")

    } else {
      # ===================================================================
      # GLOBAL SAWTOOTH MODEL: Single growth rate (original)
      # ===================================================================

      # Calculate single global growth rate from initial to final
      days_between <- as.numeric(difftime(final_date, initial_date, units = "days"))
      diameter_increase <- final_diameter_mm - base_diameter_mm
      growth_rate_mm_per_day <- diameter_increase / days_between
    }
  }

  # STEP 2: Identify installation periods and wound diameters
  # -------------------------------------------------------------------------

  # =========================================================================
  # MODEL B: Measured Wound Sizes Per Period (Step Function)
  # =========================================================================
  if (use_model_b) {
    wound_at_reinstall_mm <- wound_config$wound_at_reinstall_mm

    if (!is.null(wound_config$reinstall_dates)) {
      reinstall_dates <- as.POSIXct(wound_config$reinstall_dates)

      # Validate: must have one more wound diameter than reinstall dates
      # (initial period + one for each period after reinstallation)
      expected_length <- length(reinstall_dates) + 1
      if (length(wound_at_reinstall_mm) != expected_length) {
        stop(sprintf("wound_at_reinstall_mm must have length = length(reinstall_dates) + 1. Expected %d, got %d.",
                    expected_length, length(wound_at_reinstall_mm)))
      }

      # Validate chronological order
      if (is.unsorted(reinstall_dates)) {
        stop("reinstall_dates must be in chronological order")
      }

      # Create period boundaries
      # Periods: (-Inf, reinstall1], (reinstall1, reinstall2], ..., (reinstallN, Inf)
      period_starts <- c(-Inf, reinstall_dates)
      period_diameters_mm <- wound_at_reinstall_mm

    } else {
      # No reinstallations - single period with single diameter
      if (length(wound_at_reinstall_mm) != 1) {
        stop("wound_at_reinstall_mm must have length 1 when no reinstall_dates provided")
      }
      period_starts <- c(-Inf)
      period_diameters_mm <- wound_at_reinstall_mm
    }

    # Assign wound diameters based on which period each timestamp falls into
    wound_diameter_mm <- numeric(length(timestamps))

    for (i in seq_along(timestamps)) {
      # Find which period this timestamp belongs to
      # Period j if: period_starts[j] < timestamp <= period_starts[j+1]
      # (or timestamp > period_starts[j] for last period)
      period_idx <- sum(timestamps[i] >= period_starts)
      wound_diameter_mm[i] <- period_diameters_mm[period_idx]
    }

    # Convert to cm and return
    wound_diameter_cm <- wound_diameter_mm / 10
    return(wound_diameter_cm)
  }

  # =========================================================================
  # MODEL A: Constant Healing Rate (Sawtooth Pattern)
  # =========================================================================

  # Create vector of all installation start dates
  # This includes initial installation + all reinstallations
  # Each reinstallation RESETS the wound diameter to base_diameter_mm
  if (!is.null(wound_config$reinstall_dates)) {
    # Ensure reinstall_dates are POSIXct
    reinstall_dates <- as.POSIXct(wound_config$reinstall_dates)

    # Validate chronological order
    if (is.unsorted(reinstall_dates)) {
      stop("reinstall_dates must be in chronological order")
    }

    # Combine initial date with reinstall dates if initial_date exists
    if (!is.null(wound_config$initial_date)) {
      initial_date <- as.POSIXct(wound_config$initial_date)
      installation_starts <- c(initial_date, reinstall_dates)
    } else {
      # No initial_date specified - use first reinstall as initial
      installation_starts <- reinstall_dates
    }
  } else {
    # No reinstallations - single installation period
    if (!is.null(wound_config$initial_date)) {
      installation_starts <- as.POSIXct(wound_config$initial_date)
    } else {
      # No dates at all - use static diameter for all timestamps
      return(rep(base_diameter_mm / 10, length(timestamps)))
    }
  }

  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # STEP 3: Vectorised calculation with "sawtooth" pattern
  # -------------------------------------------------------------------------

  # For each timestamp, find which installation period it belongs to
  # Then calculate wound diameter as: base + (days_since_install × growth_rate)
  # This creates a sawtooth pattern:
  #   - Linear growth from base_diameter after each installation
  #   - Reset to base_diameter at each reinstallation
  #   - Growth rate can be either constant (global) or period-specific (segmented)

  wound_diameter_mm <- numeric(length(timestamps))

  for (i in seq_along(timestamps)) {
    # Find the most recent installation start date before or equal to this timestamp
    current_installation_start <- max(installation_starts[installation_starts <= timestamps[i]])

    # Calculate days since this installation
    days_since_install <- as.numeric(difftime(timestamps[i], current_installation_start, units = "days"))

    # Determine which period this timestamp belongs to (for segmented model)
    if (use_segmented_rates) {
      # Find period index based on which installation start this is
      period_idx <- which(installation_starts == current_installation_start)

      # Use period-specific growth rate
      current_growth_rate <- period_growth_rates[period_idx]
    } else {
      # Use global growth rate
      current_growth_rate <- growth_rate_mm_per_day
    }

    # Calculate current diameter: base diameter + linear growth
    # Each reinstallation resets to base_diameter, then grows linearly
    wound_diameter_mm[i] <- base_diameter_mm + (days_since_install * current_growth_rate)
  }

  # STEP 4: Convert to cm and return
  # -------------------------------------------------------------------------

  wound_diameter_cm <- wound_diameter_mm / 10

  return(wound_diameter_cm)
}



# =============================================================================
# MAIN WOUND CORRECTION FUNCTION
# =============================================================================

#' Apply Wound Correction to Heat Pulse Velocity Data
#'
#' Corrects heat pulse velocity measurements for the systematic underestimation
#' caused by wound tissue around probe insertion holes. Uses linear correction
#' coefficients from Burgess et al. (2001) lookup tables.
#'
#' @param vh_data Data frame containing velocity measurements. Must have columns:
#'   \describe{
#'     \item{datetime}{POSIXct timestamp}
#'     \item{Vh_cm_hr}{Heat pulse velocity (cm/hr) - raw or after spacing correction}
#'     \item{method}{Calculation method used}
#'     \item{sensor_position}{"inner" or "outer"}
#'   }
#'   If spacing correction was applied, should also have \code{Vh_cm_hr_sc} column.
#'
#' @param wound_diameter Wound diameter in cm (typically 0.17-0.30 cm, i.e., 1.7-3.0 mm).
#'   This is usually the drill bit diameter used for probe installation.
#'   DEPRECATED: Use wood_properties parameter instead for temporal wound tracking.
#'   If NULL, will use wood_properties. Default: NULL
#'
#' @param probe_spacing Probe spacing: "5mm" (standard, default) or "6mm" (legacy).
#'   Determines which coefficient lookup table to use.
#'
#' @param wood_properties Optional WoodProperties object.
#'   If provided, uses wound configuration for temporal wound diameter calculation.
#'   Supports both static and time-varying wound diameters.
#'
#' @param use_spacing_corrected Logical. If TRUE and \code{Vh_cm_hr_sc} column exists,
#'   applies wound correction to spacing-corrected velocities. Default: TRUE
#'
#' @param confirm_parameters Logical. If TRUE and in interactive mode, prompts user
#'   to confirm parameters before applying correction. Default: TRUE
#'
#' @return Data frame with original columns plus:
#'   \describe{
#'     \item{Vc_cm_hr}{Wound-corrected velocity (cm/hr)}
#'     \item{wound_correction_factor}{Linear correction factor B applied}
#'     \item{wound_diameter_cm}{Wound diameter used for correction}
#'   }
#'
#' @details
#' **Why Wound Correction is Needed:**
#'
#' When probes are inserted into sapwood, they create wound tissue that:
#' - Has different thermal properties than healthy sapwood
#' - Causes systematic underestimation of sap velocity
#' - Effect increases with wound size and flow rate
#'
#' **Correction Method:**
#'
#' Uses linear approximation: \code{Vc = B × Vh}
#'
#' Where B is the correction factor from lookup tables (r² > 0.998).
#' B typically ranges from ~1.7 (small wounds) to ~2.6 (large wounds).
#'
#' **Wound Diameter:**
#'
#' The wound diameter depends on drill bit size:
#' - 1.7 mm drill → 0.17 cm wound diameter
#' - 2.0 mm drill → 0.20 cm wound diameter (common for ICT probes)
#' - 2.5 mm drill → 0.25 cm wound diameter
#' - 3.0 mm drill → 0.30 cm wound diameter
#'
#' **Workflow Position:**
#'
#' Wound correction should be applied AFTER spacing correction but BEFORE

#' flux density calculation:
#'
#' \code{Vh (raw) → Spacing Correction → Wound Correction → Vc → Flux Density}
#'
#' @examples
#' \dontrun{
#' # After spacing correction
#' vh_sc <- apply_spacing_correction(vh_results, ...)
#'
#' # Apply wound correction with 2.0 mm drill bit
#' vh_corrected <- apply_wound_correction(
#'   vh_sc,
#'   wound_diameter = 0.20
#' )
#'
#' # Or read wound diameter from wood properties
#' vh_corrected <- apply_wound_correction(
#'   vh_sc,
#'   wood_properties = "eucalyptus"
#' )
#'
#' # Check correction factor applied
#' unique(vh_corrected$wound_correction_factor)
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., Beverly, C.R., Ong, C.K.,
#' Khan, A.A.H., Bleby, T.M. (2001). An improved heat pulse method to measure
#' low and reverse rates of sap flow in woody plants. Tree Physiology 21: 589-598.
#'
#' Swanson, R.H., Whitfield, D.W.A. (1981). A numerical analysis of heat pulse
#' velocity theory and practice. Journal of Experimental Botany 32: 221-239.
#'
#' ICT International (2020). SFM1 Sap Flow Meter User Manual Version 5.2.
#'
#' @family correction functions
#' @export
apply_wound_correction <- function(vh_data,
                                   wound_diameter = NULL,
                                   probe_spacing = "5mm",
                                   wood_properties = NULL,
                                   use_spacing_corrected = TRUE,
                                   confirm_parameters = TRUE) {


  # -------------------------------------------------------------------------
  # Input validation
  # -------------------------------------------------------------------------

  if (!is.data.frame(vh_data)) {
    stop("vh_data must be a data frame")
  }

  required_cols <- c("datetime", "Vh_cm_hr")
  missing_cols <- setdiff(required_cols, names(vh_data))
  if (length(missing_cols) > 0) {
    stop("vh_data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!probe_spacing %in% c("5mm", "6mm"))
    stop("probe_spacing must be '5mm' or '6mm'")

  # -------------------------------------------------------------------------
  # Resolve wound diameter (static or temporal)
  # -------------------------------------------------------------------------

  wound_diameter_vector <- NULL  # Will hold diameter for each row (temporal) or single value (static)
  use_temporal <- FALSE

  # Try to use WoodProperties object for temporal wound tracking
  if (!is.null(wood_properties) && inherits(wood_properties, "WoodProperties")) {
    wound_cfg <- wood_properties$wound_correction

    # Check if temporal wound tracking is configured
    if (!is.null(wound_cfg$drill_bit_diameter_mm)) {
      # Calculate time-varying wound diameters
      wound_diameter_vector <- calc_wound_diameter(vh_data$datetime, wood_properties)
      use_temporal <- TRUE

      message("Using temporal wound diameter tracking")
    }
  }

  # Fall back to static wound diameter if temporal not available
  if (!use_temporal) {
    # Try old get_wound_diameter_from_config approach (backward compatibility)
    if (is.null(wound_diameter) && !is.null(wood_properties)) {
      wound_diameter <- get_wound_diameter_from_config(wood_properties)
    }

    # If still NULL, prompt user in interactive mode
    if (is.null(wound_diameter)) {
      if (interactive()) {
        wound_diameter <- prompt_wound_diameter()
      } else {
        stop("wound_diameter must be specified (in cm) or provide wood_properties with wound configuration")
      }
    }

    # Validate wound diameter range
    if (wound_diameter < 0.15 || wound_diameter > 0.35) {
      warning("wound_diameter ", wound_diameter, " cm is outside typical range (0.17-0.30 cm)")
    }

    # Create static diameter vector
    wound_diameter_vector <- rep(wound_diameter, nrow(vh_data))

    message("Using static wound diameter: ", wound_diameter, " cm")
  }

  # -------------------------------------------------------------------------
  # Get correction coefficients for each row
  # -------------------------------------------------------------------------

  # Get correction coefficient for each wound diameter
  B_vector <- sapply(wound_diameter_vector, function(wd) {
    get_wound_correction_coefficient(wd, probe_spacing)
  })

  # -------------------------------------------------------------------------
  # Interactive confirmation
  # -------------------------------------------------------------------------

  if (confirm_parameters && interactive()) {
    confirmed <- prompt_wound_correction_confirmation(
      wound_diameter = wound_diameter,
      probe_spacing = probe_spacing,
      correction_factor = B,
      n_rows = nrow(vh_data),
      use_spacing_corrected = use_spacing_corrected,
      has_spacing_corrected = "Vh_cm_hr_sc" %in% names(vh_data)
    )

    if (!confirmed) {
      message("\nWound correction cancelled by user.")
      return(invisible(NULL))
    }
  }

  # -------------------------------------------------------------------------
  # Determine which velocity column to correct (hybrid workflow)
  # -------------------------------------------------------------------------

  # Detect which correction was previously applied
  has_zf <- "Vh_cm_hr_zf" %in% names(vh_data) &&
            any(vh_data$zero_flow_offset_applied %||% FALSE, na.rm = TRUE)

  has_sc <- "Vh_cm_hr_sc" %in% names(vh_data) &&
            any(vh_data$spacing_correction_applied %||% FALSE, na.rm = TRUE)

  # Determine source and target columns
  if (has_sc && use_spacing_corrected) {
    input_col <- "Vh_cm_hr_sc"
    output_col <- "Vh_cm_hr_sc_wc"
    correction_base <- "spacing"
    message("Applying wound correction to spacing-corrected velocities (Vh_cm_hr_sc)")
  } else if (has_zf) {
    input_col <- "Vh_cm_hr_zf"
    output_col <- "Vh_cm_hr_zf_wc"
    correction_base <- "zero_flow_offset"
    message("Applying wound correction to zero-flow corrected velocities (Vh_cm_hr_zf)")
  } else if (has_sc) {
    # Has spacing but user set use_spacing_corrected = FALSE
    input_col <- "Vh_cm_hr_raw"
    output_col <- "Vh_cm_hr_wc"
    correction_base <- "none"
    message("Applying wound correction to raw velocities (Vh_cm_hr_raw)")
  } else {
    # No prior corrections
    input_col <- "Vh_cm_hr_raw"
    output_col <- "Vh_cm_hr_wc"
    correction_base <- "none"
    message("Applying wound correction to raw velocities (Vh_cm_hr_raw)")
  }

  # -------------------------------------------------------------------------
  # Apply correction
  # -------------------------------------------------------------------------

  vh_data[[output_col]] <- vh_data[[input_col]] * B_vector
  vh_data$Vh_cm_hr <- vh_data[[output_col]]  # Update "current" pointer
  vh_data$wound_correction_applied <- TRUE
  vh_data$wound_correction_factor <- B_vector
  vh_data$wound_diameter_cm <- wound_diameter_vector

  # Also keep legacy Vc_cm_hr for backward compatibility
  vh_data$Vc_cm_hr <- vh_data[[output_col]]

  # -------------------------------------------------------------------------
  # Update metadata attributes
  # -------------------------------------------------------------------------

  attr(vh_data, "current_vh_column") <- output_col
  attr(vh_data, "corrections_applied") <- c(
    attr(vh_data, "corrections_applied"),
    "wound"
  )

  # -------------------------------------------------------------------------
  # Print summary
  # -------------------------------------------------------------------------

  print_wound_correction_summary(vh_data, wound_diameter_vector, B_vector, input_col, use_temporal)

  return(vh_data)
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get Wound Correction Coefficient
#'
#' Looks up or interpolates the linear wound correction coefficient (B) from
#' lookup tables based on wound diameter and probe spacing.
#'
#' @param wound_diameter Wound diameter in cm
#' @param probe_spacing "5mm" or "6mm"
#' @return Numeric correction coefficient B
#' @keywords internal
get_wound_correction_coefficient <- function(wound_diameter, probe_spacing = "5mm") {

  # Select appropriate lookup table
  if (probe_spacing == "5mm") {
    coef_table <- wound_coefficients_5mm
  } else {
    coef_table <- wound_coefficients_6mm
  }

  # Check if exact match exists
  exact_match <- coef_table[coef_table$wound_diameter_cm == wound_diameter, ]
  if (nrow(exact_match) == 1) {
    return(exact_match$B_linear)
  }

  # Interpolate if between table values
  if (wound_diameter >= min(coef_table$wound_diameter_cm) &&
      wound_diameter <= max(coef_table$wound_diameter_cm)) {

    B <- approx(
      x = coef_table$wound_diameter_cm,
      y = coef_table$B_linear,
      xout = wound_diameter
    )$y

    return(B)
  }

  # Extrapolate with warning if outside range
  if (wound_diameter < min(coef_table$wound_diameter_cm)) {
    warning("wound_diameter ", wound_diameter, " cm is below table minimum (",
            min(coef_table$wound_diameter_cm), " cm). Using minimum coefficient.")
    return(min(coef_table$B_linear))
  }

  if (wound_diameter > max(coef_table$wound_diameter_cm)) {
    warning("wound_diameter ", wound_diameter, " cm is above table maximum (",
            max(coef_table$wound_diameter_cm), " cm). Using maximum coefficient.")
    return(max(coef_table$B_linear))
  }
}


#' Get Wound Diameter from Configuration
#'
#' Attempts to extract wound_diameter from a WoodProperties object or YAML file.
#'
#' @param wood_properties WoodProperties object, path to YAML, or config name
#' @return Wound diameter in cm, or NULL if not found
#' @keywords internal
get_wound_diameter_from_config <- function(wood_properties) {

  # If it's already a WoodProperties object
  if (inherits(wood_properties, "WoodProperties")) {
    # Check in yaml_data$probe_installation
    if (!is.null(wood_properties$yaml_data$probe_installation$wound_diameter)) {
      return(wood_properties$yaml_data$probe_installation$wound_diameter)
    }
    return(NULL)
  }

  # If it's a string (config name or path)
  if (is.character(wood_properties)) {
    tryCatch({
      wp <- load_wood_properties(wood_properties)
      if (!is.null(wp$yaml_data$probe_installation$wound_diameter)) {
        return(wp$yaml_data$probe_installation$wound_diameter)
      }
    }, error = function(e) {
      # Silently ignore if can't load
    })
  }

  return(NULL)
}


#' Prompt User for Wound Diameter
#'
#' Interactive prompt to get wound diameter from user.
#'
#' @return Wound diameter in cm
#' @keywords internal
prompt_wound_diameter <- function() {

  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("  WOUND DIAMETER REQUIRED\n")
  cat(strrep("=", 67), "\n\n")

  cat("Wound diameter is needed for correction. This is typically the\n")
  cat("drill bit diameter used for probe installation.\n\n")

  cat("Common values:\n")
  cat("  - 1.7 mm drill -> 0.17 cm\n")
  cat("
  - 2.0 mm drill -> 0.20 cm (common for ICT probes)\n")
  cat("  - 2.5 mm drill -> 0.25 cm\n")
  cat("  - 3.0 mm drill -> 0.30 cm\n\n")

  cat("Enter wound diameter in cm (e.g., 0.20): ")
  input <- readline()

  wound_diameter <- as.numeric(trimws(input))

  if (is.na(wound_diameter) || wound_diameter <= 0) {
    stop("Invalid wound diameter. Please enter a positive number (e.g., 0.20)")
  }

  return(wound_diameter)
}


#' Prompt for Wound Correction Confirmation
#'
#' Shows parameter summary and prompts user to confirm before applying correction.
#'
#' @param wound_diameter Wound diameter in cm
#' @param probe_spacing Probe spacing ("5mm" or "6mm")
#' @param correction_factor B coefficient
#' @param n_rows Number of data rows
#' @param use_spacing_corrected Whether using spacing-corrected data
#' @param has_spacing_corrected Whether spacing-corrected column exists
#' @return Logical indicating confirmation
#' @keywords internal
prompt_wound_correction_confirmation <- function(wound_diameter,
                                                  probe_spacing,
                                                  correction_factor,
                                                  n_rows,
                                                  use_spacing_corrected,
                                                  has_spacing_corrected) {

  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("  WOUND CORRECTION PARAMETERS\n")
  cat(strrep("=", 67), "\n\n")

  cat(sprintf("  Wound diameter:      %.2f cm (%.1f mm drill bit)\n",
              wound_diameter, wound_diameter * 10))
  cat(sprintf("  Probe spacing:       %s\n", probe_spacing))
  cat(sprintf("  Correction factor:   %.4f (B coefficient)\n", correction_factor))
  cat(sprintf("  Data rows:           %s\n", format(n_rows, big.mark = ",")))

  cat("\n  Input column:        ")
  if (use_spacing_corrected && has_spacing_corrected) {
    cat("Vh_cm_hr_sc (spacing-corrected)\n")
  } else {
    cat("Vh_cm_hr (raw)\n")
  }

  cat(sprintf("\n  Output column:       Vc_cm_hr\n"))
  cat(sprintf("  Correction formula:  Vc = %.4f × Vh\n", correction_factor))

  cat("\n")
  cat(strrep("=", 67), "\n\n")

  cat("Are these parameters correct? (yes/no): ")
  response <- tolower(trimws(readline()))

  if (response %in% c("y", "yes")) {
    return(TRUE)
  } else {
    cat("\nTo modify parameters:\n\n")
    cat("  wound_diameter = 0.20           # Change wound diameter (cm)\n")
    cat("  probe_spacing = \"6mm\"           # Use 6mm coefficient table\n")
    cat("  use_spacing_corrected = FALSE   # Use raw Vh instead of Vh_sc\n")
    cat("  confirm_parameters = FALSE      # Skip this prompt\n\n")
    cat("See ?apply_wound_correction for full documentation.\n")
    return(FALSE)
  }
}


#' Print Wound Correction Summary
#'
#' Prints summary of wound correction results.
#'
#' @param vh_data Corrected data frame
#' @param wound_diameter Wound diameter used
#' @param B Correction factor applied
#' @param input_col Input column name
#' @keywords internal
print_wound_correction_summary <- function(vh_data, wound_diameter_vector, B_vector, input_col, use_temporal = FALSE) {

  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("  WOUND CORRECTION APPLIED\n")
  cat(strrep("=", 67), "\n\n")

  # Calculate summary statistics
  input_mean <- mean(vh_data[[input_col]], na.rm = TRUE)
  output_mean <- mean(vh_data$Vc_cm_hr, na.rm = TRUE)
  input_range <- range(vh_data[[input_col]], na.rm = TRUE)
  output_range <- range(vh_data$Vc_cm_hr, na.rm = TRUE)

  if (use_temporal) {
    # Temporal wound diameter
    wound_range <- range(wound_diameter_vector, na.rm = TRUE)
    B_range <- range(B_vector, na.rm = TRUE)

    cat(sprintf("  Wound diameter:      %.2f - %.2f cm (%.1f - %.1f mm) [TEMPORAL]\n",
                wound_range[1], wound_range[2],
                wound_range[1] * 10, wound_range[2] * 10))
    cat(sprintf("  Correction factor:   %.4f - %.4f\n", B_range[1], B_range[2]))
  } else {
    # Static wound diameter
    wound_diameter <- wound_diameter_vector[1]
    B <- B_vector[1]

    cat(sprintf("  Wound diameter:      %.2f cm (%.1f mm)\n", wound_diameter, wound_diameter * 10))
    cat(sprintf("  Correction factor:   %.4f\n", B))
  }

  cat(sprintf("  Input column:        %s\n", input_col))
  cat(sprintf("  Output column:       Vc_cm_hr\n\n"))

  cat("  Velocity comparison:\n")
  cat(sprintf("    %-20s %10s %10s\n", "", "Input", "Corrected"))
  cat(sprintf("    %-20s %10.2f %10.2f\n", "Mean (cm/hr)", input_mean, output_mean))
  cat(sprintf("    %-20s %10.2f %10.2f\n", "Min (cm/hr)", input_range[1], output_range[1]))
  cat(sprintf("    %-20s %10.2f %10.2f\n", "Max (cm/hr)", input_range[2], output_range[2]))

  cat("\n")
  cat(strrep("=", 67), "\n\n")
}


#' List Available Wound Coefficients
#'
#' Shows the available wound diameter values in the lookup tables.
#'
#' @param probe_spacing "5mm", "6mm", or "both" (default)
#' @return Invisibly returns the coefficient table(s)
#'
#' @examples
#' list_wound_coefficients()
#' list_wound_coefficients("5mm")
#'
#' @export
list_wound_coefficients <- function(probe_spacing = "both") {

  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("  WOUND CORRECTION COEFFICIENTS\n")
  cat(strrep("=", 67), "\n\n")

  if (probe_spacing %in% c("5mm", "both")) {
    cat("5mm Probe Spacing (Standard):\n")
    cat(strrep("-", 50), "\n")
    cat(sprintf("%-12s %12s %10s\n", "Wound (cm)", "B (linear)", "r²"))
    cat(strrep("-", 50), "\n")
    for (i in seq_len(nrow(wound_coefficients_5mm))) {
      cat(sprintf("%-12.2f %12.4f %10.4f\n",
                  wound_coefficients_5mm$wound_diameter_cm[i],
                  wound_coefficients_5mm$B_linear[i],
                  wound_coefficients_5mm$r_squared[i]))
    }
    cat("\n")
  }

  if (probe_spacing %in% c("6mm", "both")) {
    cat("6mm Probe Spacing (Legacy):\n")
    cat(strrep("-", 50), "\n")
    cat(sprintf("%-12s %12s %10s\n", "Wound (cm)", "B (linear)", "r²"))
    cat(strrep("-", 50), "\n")
    for (i in seq_len(nrow(wound_coefficients_6mm))) {
      cat(sprintf("%-12.2f %12.4f %10.4f\n",
                  wound_coefficients_6mm$wound_diameter_cm[i],
                  wound_coefficients_6mm$B_linear[i],
                  wound_coefficients_6mm$r_squared[i]))
    }
    cat("\n")
  }

  cat("Formula: Vc = B × Vh\n")
  cat("Source: ICT Manual Appendix 23.1, Burgess et al. (2001)\n\n")

  if (probe_spacing == "5mm") {
    return(invisible(wound_coefficients_5mm))
  } else if (probe_spacing == "6mm") {
    return(invisible(wound_coefficients_6mm))
  } else {
    return(invisible(list(
      "5mm" = wound_coefficients_5mm,
      "6mm" = wound_coefficients_6mm
    )))
  }
}
