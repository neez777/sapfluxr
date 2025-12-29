# R/03c_flux_density.R
# Sap Flux Density Conversion Functions
# Converts heat pulse velocity (Vh) to sap flux density (Jv)

#' Convert Heat Pulse Velocity to Sap Flux Density
#'
#' Converts corrected heat pulse velocity to sap flux density using the
#' wood-specific conversion factor (Z) from Burgess et al. (2001).
#'
#' @param Vh Heat pulse velocity (cm/h). Can be raw, spacing-corrected, or
#'   wound-corrected velocity. Use the most corrected version available.
#' @param wood_properties WoodProperties R6 object with calculated Z factor
#'   (sap_flux_conversion_factor). Must have run calculate_wood_properties() first.
#'
#' @return Sap flux density (cm³/cm²/h) = sap velocity (cm/h)
#'
#' @details
#' **Formula:**
#'
#' Jv = Z × Vh
#'
#' where Z = (ρdw/ρs) × ((cdw + mc×cs) / cs)
#'
#' This is the formula from Burgess et al. (2001), after Barrett et al. (1995).
#'
#' **Workflow Position:**
#'
#' This conversion should be applied AFTER all velocity corrections:
#'
#' \code{Vh (raw) → Spacing Correction → Recalc with updated k → Wound Correction → Jv}
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
                                   ...) {

  # Handle data frame input (for user convenience)
  if (!is.null(vh_data)) {
    # User provided a data frame - delegate to apply_flux_conversion
    if (is.null(velocity_col)) {
      velocity_col <- "Vh_cm_hr"  # Default
    }
    return(apply_flux_conversion(
      data = vh_data,
      wood_properties = wood_properties,
      velocity_col = velocity_col,
      ...
    ))
  }

  # Input validation (vector form)
  if (!inherits(wood_properties, "WoodProperties")) {
    stop("wood_properties must be a WoodProperties R6 object")
  }

  # Check that Z factor has been calculated
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
#' @param velocity_col Name of velocity column to convert. Default: "Vc_cm_hr"
#'   (wound-corrected velocity). Use "Vh_cm_hr_sc" for spacing-corrected,
#'   or "Vh_cm_hr" for raw velocity.
#' @param output_col Name for output column. Default: "Jv_cm3_cm2_hr"
#'
#' @return Data frame with added Jv column
#'
#' @details
#' This is a convenience wrapper around \code{\link{calc_sap_flux_density}}
#' for data frame operations.
#'
#' **Column Selection:**
#' - "Vc_cm_hr" - Wound-corrected velocity (RECOMMENDED)
#' - "Vh_cm_hr_sc" - Spacing-corrected velocity
#' - "Vh_cm_hr" - Raw heat pulse velocity
#'
#' Use the most corrected velocity available.
#'
#' @examples
#' \dontrun{
#' # After wound correction
#' vh_corrected <- apply_wound_correction(vh_data, ...)
#'
#' # Convert to flux density
#' vh_corrected <- apply_flux_conversion(
#'   vh_corrected,
#'   wood_properties = wood_props,
#'   velocity_col = "Vc_cm_hr"
#' )
#'
#' # Check results
#' head(vh_corrected[, c("datetime", "Vh_cm_hr", "Vc_cm_hr", "Jv_cm3_cm2_hr")])
#' }
#'
#' @family flux density functions
#' @export
apply_flux_conversion <- function(data,
                                   wood_properties,
                                   velocity_col = "Vc_cm_hr",
                                   output_col = "Jv_cm3_cm2_hr") {

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!velocity_col %in% names(data)) {
    stop(paste(
      "Column", velocity_col, "not found in data.\n",
      "Available columns:", paste(names(data), collapse = ", ")
    ))
  }

  # Get Z factor
  Z <- wood_properties$derived_properties$sap_flux_conversion_factor

  if (is.null(Z) || is.na(Z)) {
    stop(paste(
      "Sap flux conversion factor (Z) not calculated.\n",
      "Run calculate_wood_properties() on the wood_properties object first."
    ))
  }

  # Apply conversion
  data[[output_col]] <- data[[velocity_col]] * Z

  # Print summary
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SAP FLUX DENSITY CONVERSION APPLIED\n")
  cat(strrep("=", 70), "\n\n")

  cat(sprintf("  Conversion factor (Z): %.4f\n", Z))
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
  cat(sprintf("    %-25s %12.2f %12.2f\n", "Mean (cm/hr or cm³/cm²/hr)", input_mean, output_mean))
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
#' @export
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
