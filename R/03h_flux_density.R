# R/03c_flux_density.R
# Sap Flux Density Conversion Functions
# Converts heat pulse velocity (Vh) to sap flux density (Jv)

#' Becker & Edwards (1999) Temperature-Dependent Heat-Capacity Coefficient
#'
#' Computes the dimensionless heat-capacity coefficient \eqn{k} used in the
#' Edwards & Warwick (1984) conversion of wound-corrected heat pulse velocity to
#' sap velocity. Becker & Edwards (1999) give a quadratic in temperature that
#' corrects the systematic bias introduced when wood heat capacity is assumed
#' constant (most pronounced at night and in cold conditions):
#'
#' \deqn{k = a_0 + a_1 T + a_2 T^2}
#'
#' with default coefficients \eqn{a_0 = 0.400}, \eqn{a_1 = 0.00214},
#' \eqn{a_2 = -0.000006} (\eqn{T} in degrees Celsius). Coefficients are read from
#' the analysis configuration keys \code{becker_edwards.a0/a1/a2} so they can be
#' overridden via a custom YAML.
#'
#' @param temperature Numeric vector of wood temperatures (degrees Celsius).
#'   Intended for the physiologically relevant range 0-50 degC.
#'
#' @return Numeric vector of heat-capacity coefficients \eqn{k}, the same length
#'   as \code{temperature}.
#'
#' @references
#' Becker, P. & Edwards, W.R.N. (1999). Corrected heat capacity of wood for sap
#'   flow calculations. Tree Physiology 19: 767-768.
#' Edwards, W.R.N. & Warwick, N.W.M. (1984). Transpiration from a kiwifruit vine
#'   as estimated by the heat pulse technique. New Zealand Journal of
#'   Agricultural Research 27: 537-543.
#'
#' @examples
#' calc_becker_edwards_k(20)          # ~0.4404
#' calc_becker_edwards_k(c(0, 25, 50))
#'
#' @family flux density functions
#' @export
calc_becker_edwards_k <- function(temperature) {
  if (!is.numeric(temperature)) {
    stop("temperature must be numeric (degrees Celsius)")
  }
  a0 <- get_analysis_param("becker_edwards.a0")
  a1 <- get_analysis_param("becker_edwards.a1")
  a2 <- get_analysis_param("becker_edwards.a2")
  a0 + a1 * temperature + a2 * temperature^2
}


#' Temperature-Dependent Sap Flux Conversion Factor
#'
#' Recomputes the sap flux conversion factor \eqn{Z} with the Becker & Edwards
#' (1999) temperature-dependent heat-capacity coefficient \eqn{k(T)} in place of
#' the fixed dry-wood / sap specific-heat ratio used by
#' \code{\link{calculate_wood_properties}}.
#'
#' This underlies both temperature-dependent modes of
#' \code{\link{calc_sap_flux_density}}. Supplying a single temperature gives the
#' \code{"static"} mode; supplying one temperature per measurement gives the
#' \code{"dynamic"} mode, which the package and the companion Shiny application
#' refer to as the Becker & Edwards conversion because it is where the
#' temperature dependence is actually exercised.
#'
#' The constant-temperature factor is
#' \deqn{Z = (\rho_{dw}/\rho_s)\,((c_{dw} + mc\,c_s)/c_s)
#'         = (\rho_{dw}/\rho_s)\,(c_{dw}/c_s + mc).}
#' Becker & Edwards' coefficient \eqn{k(T)} is the dry-wood-to-water specific-heat
#' ratio \eqn{c_{dw}/c_s}, so substituting gives the temperature-dependent factor
#' \deqn{Z(T) = (\rho_{dw}/\rho_s)\,(k(T) + mc).}
#'
#' NOTE: this substitution is documented and flagged for verification against the
#' source article in
#' \code{knowledge_docs/function_reasoning/260702_becker_edwards_heat_capacity_reasoning.md}.
#'
#' @param wood_properties WoodProperties R6 object with derived properties
#'   (run \code{\link{calculate_wood_properties}} first).
#' @param temperature Numeric vector of wood temperatures (degrees Celsius).
#'
#' @return Numeric vector of conversion factors \eqn{Z(T)}, the same length as
#'   \code{temperature}.
#'
#' @family flux density functions
#' @export
sap_flux_conversion_factor_at_temp <- function(wood_properties, temperature) {
  if (!inherits(wood_properties, "WoodProperties")) {
    stop("wood_properties must be a WoodProperties R6 object")
  }
  deriv  <- wood_properties$derived_properties
  const  <- wood_properties$wood_constants
  rho_dw <- deriv$rho_dw_kg_m3
  mc     <- deriv$mc_kg_kg
  rho_s  <- const$rho_sap_kg_m3

  if (is.null(rho_dw) || is.na(rho_dw) || is.null(mc) || is.na(mc) ||
      is.null(rho_s) || is.na(rho_s)) {
    stop("wood_properties must have derived rho_dw_kg_m3 and mc_kg_kg and a sap ",
         "density constant. Run calculate_wood_properties() first.")
  }

  k_T <- calc_becker_edwards_k(temperature)
  (rho_dw / rho_s) * (k_T + mc)
}


#' Convert Heat Pulse Velocity to Sap Flux Density
#'
#' Converts corrected heat pulse velocity to sap flux density using the
#' wood-specific conversion factor (Z) from Burgess et al. (2001).
#'
#' @param Vh Heat pulse velocity (cm/h). Can be raw, spacing-corrected, or
#'   wound-corrected velocity. Use the most corrected version available.
#' @param vh_data Data frame containing velocity measurements.
#' @param wood_properties WoodProperties R6 object with calculated Z factor
#'   (sap_flux_conversion_factor). Must have run calculate_wood_properties() first.
#' @param velocity_col Name of velocity column to convert. Default: "Vc_cm_hr"
#'   (wound-corrected velocity). Use "Vh_cm_hr_sc" for spacing-corrected,
#'   or "Vh_cm_hr" for raw velocity.
#' @param temperature_mode How the heat-capacity term of \eqn{Z} is evaluated.
#'   \code{"constant"} (default) uses the fixed \eqn{Z} from
#'   \code{calculate_wood_properties()}, with no temperature dependence.
#'   \code{"static"} evaluates the temperature-dependent coefficient at a single
#'   fixed \code{temperature}. \code{"dynamic"} -- the Becker & Edwards
#'   conversion -- evaluates it per measurement from a temperature vector,
#'   typically each pulse's pre-pulse temperature (\code{prepulse_temp_c}).
#' @param temperature Wood temperature in degrees Celsius. A single value when
#'   \code{temperature_mode = "static"}; a vector the same length as \code{Vh}
#'   when \code{temperature_mode = "dynamic"}.
#' @param ... Additional arguments passed to methods.
#'
#' @return Sap flux density (cm^3/cm^2/h) = sap velocity (cm/h)
#'
#' @details
#' **Formula:**
#'
#' Jv = Z * Vh
#'
#' where Z = (rho_dw/rho_s) \* ((cdw + mc * cs) / cs)
#'
#' This is the formula from Burgess et al. (2001), after Barrett et al. (1995).
#'
#' **Workflow Position:**
#'
#' This conversion should be applied AFTER all velocity corrections:
#'
#' \code{Vh (raw) -> Spacing Correction -> Recalc with updated k -> Wound Correction -> Jv}
#'
#' **Physical Interpretation:**
#'
#' Heat pulse velocity (Vh) measures the speed at which heat moves through
#' sapwood. This includes heat carried by both the moving sap AND the wood matrix.
#' The Z factor accounts for the heat capacity of the wood matrix to extract
#' the true sap velocity.
#'
#' @examples
#' \dontrun{
#' # After calculating wood properties
#' wood_props <- load_wood_properties("eucalyptus")
#' wood_props <- calculate_wood_properties(wood_props)
#'
#' # Convert single velocity value
#' Vh <- 10.5  # cm/h
#' Jv <- calc_sap_flux_density(Vh, wood_props)
#'
#' # Convert velocity data frame
#' vh_data$Jv <- calc_sap_flux_density(vh_data$Vc_cm_hr, wood_props)
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., et al. (2001). Tree Physiology 21: 589-598.
#' Barrett, D.J., et al. (1995). Plant Cell Environ 18: 463-469.
#'
#' @family flux density functions
#' @export
calc_sap_flux_density <- function(Vh,
                                   wood_properties,
                                   vh_data = NULL,      # Alias for data frame input
                                   velocity_col = NULL, # Column name if vh_data provided
                                   temperature_mode = c("constant", "static", "dynamic"),
                                   temperature = NULL,
                                   ...) {

  temperature_mode <- match.arg(temperature_mode)

  # Handle data frame input (for user convenience)
  if (!is.null(vh_data)) {
    # User provided a data frame - delegate to apply_flux_conversion
    if (is.null(velocity_col)) {
      velocity_col <- if ("Vs_cm_hr" %in% names(vh_data)) "Vs_cm_hr" else "Vh_cm_hr"
    }
    return(apply_flux_conversion(
      data = vh_data,
      wood_properties = wood_properties,
      velocity_col = velocity_col,
      temperature_mode = temperature_mode,
      temperature = temperature,
      ...
    ))
  }

  # Input validation (vector form)
  if (!inherits(wood_properties, "WoodProperties")) {
    stop("wood_properties must be a WoodProperties R6 object")
  }

  # Temperature-dependent conversion. Both modes evaluate the same k(T)
  # coefficient; they differ only in whether T is a single fixed value (static)
  # or one value per measurement (dynamic, the Becker & Edwards conversion).
  # Dynamic in the vector form expects a temperature vector via `temperature`.
  if (temperature_mode %in% c("static", "dynamic")) {
    if (is.null(temperature) || !is.numeric(temperature)) {
      stop("temperature (numeric) is required when temperature_mode is 'static' or 'dynamic'.")
    }
    Z <- sap_flux_conversion_factor_at_temp(wood_properties, temperature)
    return(Vh * Z)
  }

  # Check that Z factor has been calculated (constant mode)
  Z <- wood_properties$derived_properties$sap_flux_conversion_factor

  if (is.null(Z) || is.na(Z)) {
    stop(paste(
      "Sap flux conversion factor (Z) not calculated.\n",
      "Run calculate_wood_properties() on the wood_properties object first."
    ))
  }

  # Apply conversion (vector form)
  Jv <- Vh * Z

  return(Jv)
}


#' Apply Sap Flux Density Conversion to Data Frame
#'
#' Adds a sap flux density column (Jv) to a data frame containing heat pulse
#' velocity measurements.
#'
#' @param data Data frame with velocity measurements
#' @param wood_properties WoodProperties R6 object with calculated Z factor
#' @param velocity_col Name of velocity column to convert. Default: \code{NULL}
#'   (auto-detects in priority order: \code{Vs_cm_hr}, \code{Vc_cm_hr},
#'   \code{Vh_cm_hr_sc}, \code{Vh_cm_hr}).
#' @param output_col Name for output column. Default: "Jv_cm3_cm2_hr"
#' @param temperature_mode Conversion factor mode. \code{"constant"} (default)
#'   uses the fixed \eqn{Z} from \code{calculate_wood_properties()}, with no
#'   temperature dependence; \code{"static"} evaluates the temperature-dependent
#'   \eqn{Z(T)} at a single \code{temperature}; \code{"dynamic"} -- the
#'   Becker & Edwards conversion -- evaluates \eqn{Z(T)} per row from
#'   \code{temperature_col}.
#' @param temperature Single numeric wood temperature (degrees Celsius) used when
#'   \code{temperature_mode = "static"}.
#' @param temperature_col Name of the per-row temperature column used when
#'   \code{temperature_mode = "dynamic"}. Default: \code{"prepulse_temp_c"}.
#'
#' @return Data frame with added Jv column
#'
#' @details
#' This is a convenience wrapper around \code{\link{calc_sap_flux_density}}
#' for data frame operations.
#'
#' **Column Selection (auto-detected in priority order):**
#' - \code{Vs_cm_hr} - Current best estimate (RECOMMENDED — updated at every correction step)
#' - \code{Vc_cm_hr} - Legacy alias for \code{Vs_cm_hr} after wound correction
#' - \code{Vh_cm_hr_sc} - Spacing-corrected velocity
#' - \code{Vh_cm_hr} - Raw heat pulse velocity
#'
#' @examples
#' \dontrun{
#' # After corrections — auto-detects Vs_cm_hr
#' vh_corrected <- apply_flux_conversion(
#'   vh_corrected,
#'   wood_properties = wood_props
#' )
#'
#' # Or explicitly
#' vh_corrected <- apply_flux_conversion(
#'   vh_corrected,
#'   wood_properties = wood_props,
#'   velocity_col = "Vs_cm_hr"
#' )
#'
#' head(vh_corrected[, c("datetime", "Vs_cm_hr", "Jv_cm3_cm2_hr")])
#' }
#'
#' @family flux density functions
#' @keywords internal
apply_flux_conversion <- function(data,
                                   wood_properties,
                                   velocity_col = NULL,
                                   output_col = "Jv_cm3_cm2_hr",
                                   temperature_mode = c("constant", "static", "dynamic"),
                                   temperature = NULL,
                                   temperature_col = "prepulse_temp_c") {

  temperature_mode <- match.arg(temperature_mode)

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (is.null(velocity_col)) {
    velocity_col <- if ("Vs_cm_hr" %in% names(data)) "Vs_cm_hr" else
                    if ("Vc_cm_hr" %in% names(data)) "Vc_cm_hr" else
                    if ("Vh_cm_hr_sc" %in% names(data)) "Vh_cm_hr_sc" else
                    "Vh_cm_hr"
  }

  if (!velocity_col %in% names(data)) {
    stop(paste(
      "Column", velocity_col, "not found in data.\n",
      "Available columns:", paste(names(data), collapse = ", ")
    ))
  }

  # Resolve the conversion factor Z (scalar for constant/static, per-row for dynamic)
  if (temperature_mode == "constant") {
    Z <- wood_properties$derived_properties$sap_flux_conversion_factor
    if (is.null(Z) || is.na(Z)) {
      stop(paste(
        "Sap flux conversion factor (Z) not calculated.\n",
        "Run calculate_wood_properties() on the wood_properties object first."
      ))
    }
  } else if (temperature_mode == "static") {
    if (is.null(temperature) || !is.numeric(temperature) || length(temperature) != 1) {
      stop("A single numeric 'temperature' is required when temperature_mode = 'static'.")
    }
    Z <- sap_flux_conversion_factor_at_temp(wood_properties, temperature)
  } else {  # dynamic
    if (!temperature_col %in% names(data)) {
      stop("Column '", temperature_col, "' not found for dynamic temperature mode.\n",
           "Ensure velocities were calculated with a calc_heat_pulse_velocity() ",
           "version that stores pre-pulse temperature.")
    }
    Z <- sap_flux_conversion_factor_at_temp(wood_properties, data[[temperature_col]])
  }

  # Apply conversion (Z is scalar or a per-row vector)
  data[[output_col]] <- data[[velocity_col]] * Z

  # Print summary
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SAP FLUX DENSITY CONVERSION APPLIED\n")
  cat(strrep("=", 70), "\n\n")

  if (length(Z) == 1L) {
    cat(sprintf("  Conversion factor (Z): %.4f\n", Z))
  } else {
    cat(sprintf("  Conversion factor (Z): %s (mean %.4f, range %.4f-%.4f)\n",
                temperature_mode, mean(Z, na.rm = TRUE),
                min(Z, na.rm = TRUE), max(Z, na.rm = TRUE)))
  }
  cat(sprintf("  Temperature mode: %s\n", temperature_mode))
  cat(sprintf("  Input column: %s\n", velocity_col))
  cat(sprintf("  Output column: %s\n", output_col))
  cat(sprintf("  Rows processed: %s\n", format(nrow(data), big.mark = ",")))
  cat("\n")

  # Calculate summary statistics
  input_mean <- mean(data[[velocity_col]], na.rm = TRUE)
  output_mean <- mean(data[[output_col]], na.rm = TRUE)
  input_range <- range(data[[velocity_col]], na.rm = TRUE)
  output_range <- range(data[[output_col]], na.rm = TRUE)

  cat("  Summary:\n")
  cat(sprintf("    %-25s %12s %12s\n", "", "Vh (input)", "Jv (output)"))
  cat(sprintf("    %-25s %12.2f %12.2f\n", "Mean (cm/hr or cm^3/cm^2/hr)", input_mean, output_mean))
  cat(sprintf("    %-25s %12.2f %12.2f\n", "Min", input_range[1], output_range[1]))
  cat(sprintf("    %-25s %12.2f %12.2f\n", "Max", input_range[2], output_range[2]))
  cat("\n")

  cat(strrep("=", 70), "\n")
  cat("\n")

  return(data)
}


#' Get Sap Flux Conversion Factor from Wood Properties
#'
#' Extracts the Z factor from a WoodProperties object for manual calculations.
#'
#' @param wood_properties WoodProperties R6 object
#'
#' @return Numeric Z factor value
#'
#' @examples
#' \dontrun{
#' wood_props <- load_wood_properties("eucalyptus")
#' wood_props <- calculate_wood_properties(wood_props)
#'
#' Z <- get_sap_flux_conversion_factor(wood_props)
#' print(Z)
#' }
#'
#' @family flux density functions
#' @keywords internal
get_sap_flux_conversion_factor <- function(wood_properties) {

  if (!inherits(wood_properties, "WoodProperties")) {
    stop("wood_properties must be a WoodProperties R6 object")
  }

  Z <- wood_properties$derived_properties$sap_flux_conversion_factor

  if (is.null(Z) || is.na(Z)) {
    stop(paste(
      "Sap flux conversion factor (Z) not calculated.\n",
      "Run calculate_wood_properties() on the wood_properties object first."
    ))
  }

  return(Z)
}
