# ==============================================================================
# FILE: R/sapwood_calculations.R
# Sapwood Area and Tree-Level Flux Scaling Functions
# ==============================================================================

#' Calculate Sapwood Area from Tree Measurements
#'
#' Calculate conducting sapwood area from tree diameter, bark thickness, and
#' heartwood dimensions using standard forestry measurements.
#'
#' @param diameter_breast_height Diameter at breast height (1.3m) in cm
#' @param bark_thickness Bark thickness in cm. If NULL, estimated from DBH using
#'   allometric relationships (default: NULL)
#' @param heartwood_radius Heartwood radius in cm. If NULL, estimated from DBH
#'   using allometric relationships (default: NULL)
#' @param heartwood_diameter Heartwood diameter in cm. Alternative to heartwood_radius.
#'   If both provided, radius takes precedence (default: NULL)
#' @param sapwood_thickness Sapwood thickness in cm. Alternative to heartwood
#'   specification (default: NULL)
#' @param estimation_method Method for estimating missing parameters.
#'   Options: "allometric", "conservative", "species_specific" (default: "allometric")
#' @param species_group Species group for parameter estimation. Options:
#'   "hardwood", "softwood", "eucalyptus", "pine" (default: "hardwood")
#'
#' @return List containing sapwood area calculations:
#'   \describe{
#'     \item{stem_measurements}{List with DBH, stem radius, bark thickness, cambium radius}
#'     \item{sapwood_measurements}{List with sapwood outer/inner radii, total sapwood area, heartwood radius}
#'     \item{probe_measurements}{List with probe depths and coverage areas for standard ICT configuration}
#'     \item{area_summary}{Summary of key area measurements}
#'     \item{estimation_notes}{Notes on which parameters were estimated}
#'   }
#'
#' @details
#' Standard calculations assume circular stem cross-section. The conducting sapwood
#' area is calculated as the area between the cambium (inner bark boundary) and
#' the heartwood boundary.
#'
#' For ICT SFM1x probes, standard sensor depths are:
#' \itemize{
#'   \item Outer sensor: 1.25 cm from cambium
#'   \item Inner sensor: 2.75 cm from cambium
#'   \item Detection limits: typically 0.5 cm beyond sensor positions
#' }
#'
#' Parameter estimation methods:
#' \itemize{
#'   \item \strong{allometric}: Uses published allometric relationships for species groups
#'   \item \strong{conservative}: Uses conservative estimates (thicker bark, larger heartwood)
#'   \item \strong{species_specific}: Uses species-specific equations where available
#' }
#'
#' @examples
#' \dontrun{
#' # Basic calculation with measured parameters
#' sapwood_area <- calc_sapwood_area(
#'   diameter_breast_height = 18.2,
#'   bark_thickness = 0.8,
#'   heartwood_diameter = 8.0
#' )
#'
#' # Calculation with automatic estimation
#' sapwood_area <- calc_sapwood_area(
#'   diameter_breast_height = 25.4,
#'   species_group = "eucalyptus"
#' )
#'
#' print(sapwood_area$area_summary)
#' }
#'
#' @references
#' Marshall, D.C. (1958). Measurement of sap flow in conifers by heat transport.
#' Plant Physiology, 33, 385-396.
#'
#' Swanson, R.H., Whitfield, D.W.A. (1981). A numerical analysis of heat pulse
#' velocity theory and practice. Journal of Experimental Botany, 32, 221-239.
#'
#' @seealso \code{\link{calc_tree_sap_flux}}, \code{\link{calc_sap_flux_density}}
#' @export
calc_sapwood_area <- function(diameter_breast_height,
                              bark_thickness = NULL,
                              heartwood_radius = NULL,
                              heartwood_diameter = NULL,
                              sapwood_thickness = NULL,
                              estimation_method = "allometric",
                              species_group = "hardwood") {

  # Input validation
  if (!is.numeric(diameter_breast_height) || diameter_breast_height <= 0) {
    stop("diameter_breast_height must be a positive number (cm)")
  }

  if (diameter_breast_height < 5 || diameter_breast_height > 200) {
    warning("DBH of ", diameter_breast_height, " cm seems unusual. Typical range: 5-200 cm")
  }

  if (!estimation_method %in% c("allometric", "conservative", "species_specific")) {
    stop("estimation_method must be one of: allometric, conservative, species_specific")
  }

  if (!species_group %in% c("hardwood", "softwood", "eucalyptus", "pine")) {
    warning("Unknown species_group: ", species_group, ". Using 'hardwood' defaults.")
    species_group <- "hardwood"
  }

  # Calculate basic stem dimensions
  stem_radius <- diameter_breast_height / 2

  # Track what was estimated
  estimation_notes <- character(0)

  # Estimate bark thickness if not provided
  if (is.null(bark_thickness)) {
    bark_thickness <- estimate_bark_thickness(diameter_breast_height, species_group, estimation_method)
    estimation_notes <- c(estimation_notes, paste("Bark thickness estimated:", round(bark_thickness, 2), "cm"))
  } else {
    if (bark_thickness < 0 || bark_thickness > stem_radius) {
      stop("bark_thickness must be between 0 and stem radius (", round(stem_radius, 2), " cm)")
    }
  }

  # Calculate cambium radius (radius to inner bark)
  cambium_radius <- stem_radius - bark_thickness

  if (cambium_radius <= 0) {
    stop("Bark thickness (", bark_thickness, " cm) is too large for DBH (", diameter_breast_height, " cm)")
  }

  # Determine heartwood radius using priority: heartwood_radius > heartwood_diameter > sapwood_thickness > estimation
  if (!is.null(heartwood_radius)) {
    if (heartwood_radius < 0 || heartwood_radius >= cambium_radius) {
      stop("heartwood_radius must be between 0 and cambium radius (", round(cambium_radius, 2), " cm)")
    }
    heartwood_radius_final <- heartwood_radius

  } else if (!is.null(heartwood_diameter)) {
    heartwood_radius_final <- heartwood_diameter / 2
    if (heartwood_radius_final < 0 || heartwood_radius_final >= cambium_radius) {
      stop("heartwood_diameter must be between 0 and cambium diameter (", round(cambium_radius * 2, 2), " cm)")
    }

  } else if (!is.null(sapwood_thickness)) {
    heartwood_radius_final <- cambium_radius - sapwood_thickness
    if (heartwood_radius_final < 0) {
      warning("Specified sapwood_thickness (", sapwood_thickness, " cm) is larger than available radius. Setting heartwood_radius to 0.")
      heartwood_radius_final <- 0
    }

  } else {
    # Estimate heartwood radius
    heartwood_radius_final <- estimate_heartwood_radius(cambium_radius, species_group, estimation_method)
    estimation_notes <- c(estimation_notes, paste("Heartwood radius estimated:", round(heartwood_radius_final, 2), "cm"))
  }

  # Calculate sapwood area
  sapwood_outer_radius <- cambium_radius
  sapwood_inner_radius <- heartwood_radius_final
  total_sapwood_area <- pi * (sapwood_outer_radius^2 - sapwood_inner_radius^2)

  # Calculate ICT probe coverage areas (standard configuration)
  probe_depths <- list(
    needle_length = 3.5,           # Total needle length (cm)
    tip_to_outer = 2.25,           # Needle tip to outer sensor (cm)
    tip_to_inner = 0.75,           # Needle tip to inner sensor (cm)
    hub_to_outer = 1.25,           # Needle hub to outer sensor (cm)
    hub_to_inner = 2.75,           # Needle hub to inner sensor (cm)
    outer_detection_limit = 1.75,  # Hub to outer detection limit (cm)
    inner_detection_limit = 3.25   # Hub to inner detection limit (cm)
  )

  # Calculate sensor positions relative to cambium
  outer_sensor_radius <- cambium_radius - probe_depths$hub_to_outer
  inner_sensor_radius <- cambium_radius - probe_depths$hub_to_inner
  outer_detection_radius <- cambium_radius - probe_depths$outer_detection_limit
  inner_detection_radius <- cambium_radius - probe_depths$inner_detection_limit

  # Ensure sensors are within sapwood
  outer_sensor_radius <- max(outer_sensor_radius, sapwood_inner_radius)
  inner_sensor_radius <- max(inner_sensor_radius, sapwood_inner_radius)
  outer_detection_radius <- max(outer_detection_radius, sapwood_inner_radius)
  inner_detection_radius <- max(inner_detection_radius, sapwood_inner_radius)

  # Calculate areas represented by each sensor
  # Outer sensor represents area from cambium to midpoint between sensors
  midpoint_radius <- (outer_sensor_radius + inner_sensor_radius) / 2
  outer_sensor_area <- pi * (sapwood_outer_radius^2 - midpoint_radius^2)

  # Inner sensor represents area from midpoint to heartwood
  inner_sensor_area <- pi * (midpoint_radius^2 - sapwood_inner_radius^2)

  # Detection areas (areas where flow can be reliably detected)
  outer_detection_area <- pi * (sapwood_outer_radius^2 - outer_detection_radius^2)
  inner_detection_area <- pi * (inner_detection_radius^2 - sapwood_inner_radius^2)

  # Coverage calculations
  total_probe_coverage <- (outer_sensor_area + inner_sensor_area) / total_sapwood_area
  detection_coverage <- (outer_detection_area + inner_detection_area) / total_sapwood_area

  # Prepare results
  stem_measurements <- list(
    diameter_breast_height = diameter_breast_height,
    stem_radius = stem_radius,
    bark_thickness = bark_thickness,
    cambium_radius = cambium_radius
  )

  sapwood_measurements <- list(
    sapwood_outer_radius = sapwood_outer_radius,
    sapwood_inner_radius = sapwood_inner_radius,
    sapwood_thickness = sapwood_outer_radius - sapwood_inner_radius,
    total_sapwood_area = total_sapwood_area,
    heartwood_radius = heartwood_radius_final,
    heartwood_area = pi * heartwood_radius_final^2
  )

  probe_measurements <- list(
    probe_depths = probe_depths,
    outer_sensor_radius = outer_sensor_radius,
    inner_sensor_radius = inner_sensor_radius,
    outer_sensor_area = outer_sensor_area,
    inner_sensor_area = inner_sensor_area,
    outer_detection_area = outer_detection_area,
    inner_detection_area = inner_detection_area,
    midpoint_radius = midpoint_radius
  )

  coverage_calculations <- list(
    outer_sensor_coverage = outer_sensor_area / total_sapwood_area,
    inner_sensor_coverage = inner_sensor_area / total_sapwood_area,
    total_probe_coverage = total_probe_coverage,
    detection_coverage = detection_coverage,
    unsampled_area = total_sapwood_area - (outer_sensor_area + inner_sensor_area),
    unsampled_fraction = 1 - total_probe_coverage
  )

  # Area summary for easy reference
  area_summary <- list(
    total_stem_area_cm2 = pi * stem_radius^2,
    total_sapwood_area_cm2 = total_sapwood_area,
    heartwood_area_cm2 = pi * heartwood_radius_final^2,
    probe_coverage_percent = round(total_probe_coverage * 100, 1),
    sapwood_thickness_cm = round(sapwood_outer_radius - sapwood_inner_radius, 2)
  )

  # Create result object
  result <- list(
    stem_measurements = stem_measurements,
    sapwood_measurements = sapwood_measurements,
    probe_measurements = probe_measurements,
    coverage_calculations = coverage_calculations,
    area_summary = area_summary,
    estimation_notes = estimation_notes,
    species_group = species_group,
    estimation_method = estimation_method
  )

  # Set class for method dispatch
  class(result) <- c("sapwood_area", "list")

  return(result)
}

#' Estimate Bark Thickness from DBH and Species
#' @param dbh Diameter at breast height (cm)
#' @param species_group Species group
#' @param method Estimation method
#' @return Estimated bark thickness (cm)
#' @keywords internal
estimate_bark_thickness <- function(dbh, species_group, method) {

  # Allometric relationships for bark thickness
  # Based on published forestry literature

  if (method == "conservative") {
    # Conservative estimates (thicker bark)
    multiplier <- switch(species_group,
                         "eucalyptus" = 0.08,
                         "pine" = 0.06,
                         "softwood" = 0.06,
                         "hardwood" = 0.07,
                         0.07  # default
    )
  } else {
    # Typical allometric estimates
    multiplier <- switch(species_group,
                         "eucalyptus" = 0.05,
                         "pine" = 0.04,
                         "softwood" = 0.04,
                         "hardwood" = 0.045,
                         0.045  # default
    )
  }

  # Add base thickness for small trees
  base_thickness <- 0.2

  estimated <- base_thickness + (dbh * multiplier)

  # Reasonable bounds
  max_thickness <- dbh * 0.15  # Max 15% of radius
  min_thickness <- 0.1

  return(max(min_thickness, min(estimated, max_thickness)))
}

#' Estimate Heartwood Radius from Cambium Radius and Species
#' @param cambium_radius Cambium radius (cm)
#' @param species_group Species group
#' @param method Estimation method
#' @return Estimated heartwood radius (cm)
#' @keywords internal
estimate_heartwood_radius <- function(cambium_radius, species_group, method) {

  if (method == "conservative") {
    # Conservative estimates (larger heartwood, less sapwood)
    heartwood_fraction <- switch(species_group,
                                 "eucalyptus" = 0.4,
                                 "pine" = 0.5,
                                 "softwood" = 0.5,
                                 "hardwood" = 0.35,
                                 0.4  # default
    )
  } else {
    # Typical estimates
    heartwood_fraction <- switch(species_group,
                                 "eucalyptus" = 0.25,
                                 "pine" = 0.35,
                                 "softwood" = 0.35,
                                 "hardwood" = 0.2,
                                 0.25  # default
    )
  }

  estimated <- cambium_radius * heartwood_fraction

  # Ensure minimum sapwood thickness of 1 cm
  min_sapwood <- 1.0
  max_heartwood <- cambium_radius - min_sapwood

  return(max(0, min(estimated, max_heartwood)))
}

#' Print method for sapwood_area objects
#' @param x A sapwood_area object
#' @param ... Additional arguments
#' @export
print.sapwood_area <- function(x, ...) {

  cat("Sapwood Area Calculations\n")
  cat("========================\n\n")

  cat("Tree Measurements:\n")
  cat("  DBH:", x$stem_measurements$diameter_breast_height, "cm\n")
  cat("  Bark thickness:", round(x$stem_measurements$bark_thickness, 2), "cm\n")
  cat("  Sapwood thickness:", x$area_summary$sapwood_thickness_cm, "cm\n\n")

  cat("Areas:\n")
  cat("  Total sapwood area:", round(x$area_summary$total_sapwood_area_cm2, 1), "cm²\n")
  cat("  Heartwood area:", round(x$area_summary$heartwood_area_cm2, 1), "cm²\n")
  cat("  Probe coverage:", x$area_summary$probe_coverage_percent, "%\n\n")

  if (length(x$estimation_notes) > 0) {
    cat("Estimation Notes:\n")
    for (note in x$estimation_notes) {
      cat("  -", note, "\n")
    }
  }

  invisible(x)
}

