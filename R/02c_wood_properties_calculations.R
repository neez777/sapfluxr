# R/02c_wood_properties_calculations.R
# Wood Properties Calculation Functions
# Calculates derived thermal and physical properties from measurements

#' Calculate All Derived Wood Properties
#'
#' Calculates all derived wood properties from user measurements using the
#' formulas from Burgess et al. (2001) and Barrett et al. (1995).
#'
#' @param wood_properties WoodProperties R6 object with measurements and constants
#'
#' @return WoodProperties R6 object with populated derived_properties
#'
#' @details
#' This function implements the complete calculation chain:
#' 1. Unit conversions (g → kg, cm³ → m³)
#' 2. Base densities (dry, fresh, basic)
#' 3. Moisture content
#' 4. Fiber saturation point properties
#' 5. Thermal properties (conductivity, heat capacity, diffusivity)
#' 6. Conversion factors (Y and Z)
#'
#' **Input Methods:**
#' - Method 1: Provide fresh_weight_g, dry_weight_g, fresh_volume_cm3
#' - Method 2: Provide density_dry_kg_m3 AND density_fresh_kg_m3
#'
#' Must provide ONE method. Both methods calculate all derived properties
#' including Y and Z factors. Method 2 back-calculates moisture content from
#' the density ratio: mc = (ρfw/ρdw) - 1.
#'
#' @references
#' Burgess, S.S.O., Adams, M.A., Turner, N.C., et al. (2001). Tree Physiology 21: 589-598.
#' Barrett, D.J., et al. (1995). Plant Cell Environ 18: 463-469.
#'
#' @family wood property functions
#' @export
calculate_wood_properties <- function(wood_properties) {

  # -------------------------------------------------------------------------
  # Input validation
  # -------------------------------------------------------------------------

  if (!inherits(wood_properties, "WoodProperties")) {
    stop("wood_properties must be a WoodProperties R6 object")
  }

  meas <- wood_properties$wood_measurements
  const <- wood_properties$wood_constants

  # Check that user provided ONE input method
  method1_complete <- !is.null(meas$fresh_weight_g) &&
                      !is.null(meas$dry_weight_g) &&
                      !is.null(meas$fresh_volume_cm3)

  method2_complete <- !is.null(meas$density_dry_kg_m3) &&
                      !is.null(meas$density_fresh_kg_m3)

  if (!method1_complete && !method2_complete) {
    stop(paste(
      "Must provide wood measurements using one of two methods:\n",
      "Method 1: fresh_weight_g, dry_weight_g, fresh_volume_cm3\n",
      "Method 2: density_dry_kg_m3 AND density_fresh_kg_m3"
    ))
  }

  if (method1_complete && method2_complete) {
    warning(paste(
      "Both weight/volume data AND densities provided.",
      "Using weight/volume data (Method 1) and ignoring densities."
    ))
  }

  # -------------------------------------------------------------------------
  # STEP 1: Unit Conversions
  # -------------------------------------------------------------------------

  deriv <- list()  # Will store all derived properties

  if (method1_complete) {
    # Convert weights from grams to kg
    deriv$wf_kg <- meas$fresh_weight_g / 1000
    deriv$wd_kg <- meas$dry_weight_g / 1000

    # Convert volume from cm³ to m³
    deriv$vf_m3 <- meas$fresh_volume_cm3 / (100^3)

  } else {
    # Method 2: Have both densities
    # Use arbitrary volume (1 m³) for calculations that need weights/volumes
    # The actual values don't matter since they cancel out in ratios
    deriv$vf_m3 <- 1.0  # Arbitrary volume
    deriv$wf_kg <- meas$density_fresh_kg_m3 * deriv$vf_m3
    deriv$wd_kg <- meas$density_dry_kg_m3 * deriv$vf_m3
  }

  # -------------------------------------------------------------------------
  # STEP 2: Calculate Base Densities
  # -------------------------------------------------------------------------

  # Calculate densities (works for both methods since we have wf, wd, vf)
  # Dry wood density: ρdw = wd / vf
  deriv$rho_dw_kg_m3 <- deriv$wd_kg / deriv$vf_m3

  # Fresh wood density: ρfw = wf / vf
  deriv$rho_fw_kg_m3 <- deriv$wf_kg / deriv$vf_m3

  # Basic density (dry mass / fresh volume)
  deriv$basic_density_kg_m3 <- deriv$rho_dw_kg_m3

  # -------------------------------------------------------------------------
  # STEP 3: Calculate Moisture Content
  # -------------------------------------------------------------------------

  # Moisture content: mc = (wf - wd) / wd  [kg/kg]
  # Works for both methods since we have wf and wd (real or back-calculated)
  deriv$mc_kg_kg <- (deriv$wf_kg - deriv$wd_kg) / deriv$wd_kg

  # -------------------------------------------------------------------------
  # STEP 4: Calculate Fiber Saturation Point Properties
  # -------------------------------------------------------------------------

  # Moisture content at FSP: mc_FSP = 0.2 × (ρdw/ρs)^(-0.5)
  deriv$mc_FSP_kg_kg <- 0.2 * (deriv$rho_dw_kg_m3 / const$rho_sap_kg_m3)^(-0.5)

  # Specific gravity: G = ρdw / ρs
  deriv$specific_gravity <- deriv$rho_dw_kg_m3 / const$rho_sap_kg_m3

  # Void fraction at FSP: Fv_FSP = 1 - G × ((ρs/ρcw) + mc_FSP)
  deriv$Fv_FSP <- 1 - deriv$specific_gravity * (
    (const$rho_sap_kg_m3 / const$rho_cell_wall_kg_m3) + deriv$mc_FSP_kg_kg
  )

  # -------------------------------------------------------------------------
  # STEP 5: Calculate Thermal Properties
  # -------------------------------------------------------------------------

  # Thermal conductivity of dry wood at FSP: Kdw_FSP = 0.04186 × (21 - 20×Fv_FSP)
  deriv$Kdw_FSP_W_m_K <- 0.04186 * (21 - 20 * deriv$Fv_FSP)

  # Thermal conductivity of fresh sapwood
  # Kfw = Ks × (mc - mc_FSP) × (ρdw/ρs) + Kdw_FSP
  # Works for both methods since we now have mc
  deriv$Kfw_W_m_K <- const$K_sap_W_m_K *
                     (deriv$mc_kg_kg - deriv$mc_FSP_kg_kg) *
                     (deriv$rho_dw_kg_m3 / const$rho_sap_kg_m3) +
                     deriv$Kdw_FSP_W_m_K

  # Specific heat capacity of fresh sapwood
  # cfw = (wd×cdw + cs×(wf-wd)) / wf
  # Works for both methods since we have wf and wd (real or back-calculated)
  deriv$cfw_J_kg_K <- (deriv$wd_kg * const$c_dry_wood_J_kg_K +
                       const$c_sap_J_kg_K * (deriv$wf_kg - deriv$wd_kg)) /
                      deriv$wf_kg

  # Actual thermal diffusivity (axial)
  # kax = Kfw / (ρfw × cfw) × 10000  [convert m²/s to cm²/s]
  # Works for both methods - all required properties are calculated
  deriv$thermal_diffusivity_actual_cm2_s <- deriv$Kfw_W_m_K /
                                            (deriv$rho_fw_kg_m3 * deriv$cfw_J_kg_K) *
                                            100 * 100

  # -------------------------------------------------------------------------
  # STEP 6: Calculate Conversion Factors
  # -------------------------------------------------------------------------

  # Thermal diffusivity correction factor: Y = kax / k_default
  # Works for both methods
  deriv$thermal_diffusivity_correction_factor <- deriv$thermal_diffusivity_actual_cm2_s /
                                                  const$thermal_diffusivity_default_cm2_s

  # Sap flux conversion factor: Z = (ρdw/ρs) × ((cdw + mc×cs) / cs)
  # Works for both methods since we now have mc
  deriv$sap_flux_conversion_factor <- (deriv$rho_dw_kg_m3 / const$rho_sap_kg_m3) *
                                      ((const$c_dry_wood_J_kg_K + deriv$mc_kg_kg * const$c_sap_J_kg_K) /
                                       const$c_sap_J_kg_K)

  # -------------------------------------------------------------------------
  # Update wood_properties object
  # -------------------------------------------------------------------------

  wood_properties$derived_properties <- deriv

  # -------------------------------------------------------------------------
  # Print summary
  # -------------------------------------------------------------------------

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("WOOD PROPERTIES CALCULATED\n")
  cat(strrep("=", 70), "\n\n")

  if (method1_complete) {
    cat("Input Method: Weight and Volume Measurements\n")
    cat("  Fresh weight:", meas$fresh_weight_g, "g\n")
    cat("  Dry weight:", meas$dry_weight_g, "g\n")
    cat("  Fresh volume:", meas$fresh_volume_cm3, "cm³\n")
  } else {
    cat("Input Method: Direct Density\n")
    cat("  Dry density:", meas$density_dry_kg_m3, "kg/m³\n")
  }
  cat("\n")

  cat(strrep("-", 70), "\n")
  cat("CALCULATED PROPERTIES\n")
  cat(strrep("-", 70), "\n")

  if (!is.na(deriv$mc_kg_kg)) {
    cat(sprintf("  Moisture content: %.4f kg/kg (%.1f%%)\n",
                deriv$mc_kg_kg, deriv$mc_kg_kg * 100))
  }
  cat(sprintf("  Dry wood density: %.1f kg/m³\n", deriv$rho_dw_kg_m3))
  if (!is.na(deriv$rho_fw_kg_m3)) {
    cat(sprintf("  Fresh wood density: %.1f kg/m³\n", deriv$rho_fw_kg_m3))
  }
  cat(sprintf("  Basic density: %.1f kg/m³\n", deriv$basic_density_kg_m3))
  cat(sprintf("  Specific gravity: %.4f\n", deriv$specific_gravity))
  cat("\n")

  if (!is.na(deriv$thermal_diffusivity_actual_cm2_s)) {
    cat(sprintf("  Actual thermal diffusivity: %.6f cm²/s\n",
                deriv$thermal_diffusivity_actual_cm2_s))
    cat(sprintf("  Default thermal diffusivity: %.6f cm²/s\n",
                const$thermal_diffusivity_default_cm2_s))
    cat(sprintf("  Correction factor (Y): %.4f\n",
                deriv$thermal_diffusivity_correction_factor))
    cat("\n")
  }

  if (!is.na(deriv$sap_flux_conversion_factor)) {
    cat(sprintf("  Sap flux conversion factor (Z): %.4f\n",
                deriv$sap_flux_conversion_factor))
    cat("    → Use this to convert Vh to Jv: Jv = Z × Vh\n")
    cat("\n")
  }

  cat(strrep("=", 70), "\n")
  cat("\n")

  return(wood_properties)
}
