
# ==============================================================================
# FILE: R/flux_calculations.R
# Flux Density and Tree-Level Scaling Functions
# ==============================================================================

#' Calculate Sap Flux Density from Heat Pulse Velocity
#'
#' Convert heat pulse velocity (Vh) to sap flux density (Jv) using wood
#' thermal properties and the correction factor approach of Burgess et al. (2001).
#'
#' @param vh_results Data frame with velocity results containing at minimum:
#'   datetime, Vh_cm_hr columns
#' @param wood_properties List containing wood thermal properties. If NULL,
#'   uses default values with a warning. Required elements:
#'   \describe{
#'     \item{fresh_density}{Fresh wood density (g/cm³)}
#'     \item{dry_density}{Dry wood density (g/cm³)}
#'     \item{moisture_content}{Gravimetric moisture content (decimal, 0-1)}
#'     \item{specific_heat_capacity}{Specific heat capacity of dry wood (J/g/°C)}
#'   }
#' @param thermal_diffusivity Thermal diffusivity of fresh sapwood (cm²/s).
#'   Default: 0.0025 (typical for many species)
#' @param add_metadata Logical whether to add wood properties metadata to results
#'   (default: TRUE)
#'
#' @return Data frame with original columns plus:
#'   \describe{
#'     \item{Jv_cm3_cm2_hr}{Sap flux density (cm³/cm²/hr)}
#'     \item{correction_factor_Z}{Correction factor used in conversion}
#'     \item{wood_properties}{List of wood properties used (if add_metadata = TRUE)}
#'   }
#'
#' @details
#' Uses the relationship from Burgess et al. (2001):
#' \deqn{J_v = V_s = Z \times V_h}
#'
#' Where the correction factor Z is:
#' \deqn{Z = \frac{\rho_{dw}}{\rho_s} \times \frac{c_{dw} + mc \times c_s}{c_s}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{\rho_{dw}} = dry wood density (g/cm³)
#'   \item \eqn{\rho_s} = fresh sapwood density (g/cm³)
#'   \item \eqn{c_{dw}} = specific heat capacity of dry wood (J/g/°C)
#'   \item \eqn{mc} = moisture content (decimal)
#'   \item \eqn{c_s} = specific heat capacity of water (4.18 J/g/°C)
#' }
#'
#' @examples
#' \dontrun{
#' # Define wood properties for eucalyptus
#' eucalyptus_props <- list(
#'   fresh_density = 0.8,
#'   dry_density = 0.55,
#'   moisture_content = 0.45,
#'   specific_heat_capacity = 1.5
#' )
#'
#' # Convert velocities to flux density
#' flux_data <- calc_sap_flux_density(vh_results, eucalyptus_props)
#' }
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., Beverly, C.R., Ong, C.K., Khan, A.A.H., Bleby, T.M. (2001).
#' An improved heat pulse method to measure low and reverse rates of sap flow in woody plants.
#' Tree Physiology, 21, 589-598.
#'
#' @seealso \code{\link{calc_tree_sap_flux}}, \code{\link{aggregate_velocity_temporal}}
#' @export
calc_sap_flux_density <- function(vh_results,
                                  wood_properties = NULL,
                                  thermal_diffusivity = 0.0025,
                                  add_metadata = TRUE) {

  # Input validation
  if (!is.data.frame(vh_results)) {
    stop("vh_results must be a data frame")
  }

  if (!"Vh_cm_hr" %in% names(vh_results)) {
    stop("vh_results must contain 'Vh_cm_hr' column")
  }

  # Set default wood properties if not provided
  if (is.null(wood_properties)) {
    wood_properties <- list(
      fresh_density = 0.8,           # g/cm³ (typical hardwood)
      dry_density = 0.55,            # g/cm³
      moisture_content = 0.45,       # 45% moisture content
      specific_heat_capacity = 1.5   # J/g/°C (typical for wood)
    )
    warning("Using default wood properties. Provide species-specific values for accurate results.\n",
            "Fresh density: ", wood_properties$fresh_density, " g/cm³\n",
            "Dry density: ", wood_properties$dry_density, " g/cm³\n",
            "Moisture content: ", wood_properties$moisture_content * 100, "%\n",
            "Specific heat capacity: ", wood_properties$specific_heat_capacity, " J/g/°C")
  }

  # Validate wood properties
  required_props <- c("fresh_density", "dry_density", "moisture_content", "specific_heat_capacity")
  missing_props <- setdiff(required_props, names(wood_properties))
  if (length(missing_props) > 0) {
    stop("Missing wood properties: ", paste(missing_props, collapse = ", "))
  }

  # Validate property values
  if (wood_properties$fresh_density <= 0 || wood_properties$dry_density <= 0) {
    stop("Wood densities must be positive")
  }

  if (wood_properties$moisture_content < 0 || wood_properties$moisture_content > 2) {
    warning("Moisture content seems unusual (", wood_properties$moisture_content, "). Expected range: 0-1 for typical sapwood")
  }

  if (thermal_diffusivity <= 0) {
    stop("thermal_diffusivity must be positive")
  }

  # Extract properties for calculation
  rho_dw <- wood_properties$dry_density      # Dry wood density (g/cm³)
  rho_s <- wood_properties$fresh_density     # Fresh sapwood density (g/cm³)
  mc <- wood_properties$moisture_content     # Moisture content (decimal)
  c_dw <- wood_properties$specific_heat_capacity  # Specific heat capacity dry wood (J/g/°C)
  c_s <- 4.18  # Specific heat capacity of water (J/g/°C)

  # Calculate correction factor Z (Burgess et al. 2001)
  Z <- (rho_dw / rho_s) * ((c_dw + mc * c_s) / c_s)

  # Create results dataframe
  flux_results <- vh_results

  # Convert velocity to flux density: Jv = Z × Vh
  flux_results$Jv_cm3_cm2_hr <- flux_results$Vh_cm_hr * Z
  flux_results$correction_factor_Z <- Z

  # Add metadata if requested
  if (add_metadata) {
    flux_results$thermal_diffusivity <- thermal_diffusivity

    # Store wood properties as attributes to avoid column expansion issues
    attr(flux_results, "wood_properties") <- wood_properties
    attr(flux_results, "conversion_method") <- "Burgess_et_al_2001"
    attr(flux_results, "conversion_date") <- Sys.Date()
  }

  return(flux_results)
}

# Print method for objects with wood properties
#' @export
print.sap_flux_density <- function(x, ...) {
  NextMethod("print")

  wood_props <- attr(x, "wood_properties")
  if (!is.null(wood_props)) {
    cat("\nWood properties used:\n")
    cat("Fresh density:", wood_props$fresh_density, "g/cm³\n")
    cat("Dry density:", wood_props$dry_density, "g/cm³\n")
    cat("Moisture content:", round(wood_props$moisture_content * 100, 1), "%\n")
    cat("Correction factor Z:", round(x$correction_factor_Z[1], 3), "\n")
  }
}