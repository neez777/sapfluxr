#' Calculate Tree-Level Sap Flux from Flux Density
#'
#' Scale sap flux density measurements to whole-tree sap flux using sapwood area
#' and sensor coverage calculations.
#'
#' @param flux_results Data frame with flux density results from calc_sap_flux_density()
#'   Must contain: Jv_cm3_cm2_hr, sensor_position columns
#' @param sapwood_area_data Sapwood area calculations from calc_sapwood_area()
#' @param scaling_method Method for scaling flux density to tree level:
#'   \describe{
#'     \item{"outer_only"}{Uses only outer sensor measurements (default)}
#'     \item{"inner_only"}{Uses only inner sensor measurements}
#'     \item{"weighted_average"}{Weighted average based on area coverage}
#'     \item{"radial_profile"}{Assumes linear radial profile between sensors}
#'   }
#' @param flux_units Output units for tree flux. Options: "L_hr", "L_day", "kg_hr", "kg_day"
#'   (default: "L_hr")
#' @param water_density Water density for mass calculations (kg/L). Only used for kg units
#'   (default: 1.0)
#'
#' @return Data frame with tree-level flux calculations containing original columns plus:
#'   \describe{
#'     \item{tree_flux}{Tree-level flux in specified units}
#'     \item{flux_units}{Units of tree flux}
#'     \item{total_sapwood_area_cm2}{Total sapwood area used in scaling}
#'     \item{scaling_method}{Scaling method used}
#'     \item{sensor_coverage}{Fraction of sapwood area covered by sensors}
#'     \item{extrapolation_factor}{Factor used to extrapolate from measured to total area}
#'   }
#'
#' @details
#' Scaling methods:
#' \itemize{
#'   \item \strong{outer_only}: Applies outer sensor flux density to entire sapwood area
#'   \item \strong{inner_only}: Applies inner sensor flux density to entire sapwood area
#'   \item \strong{weighted_average}: Uses area-weighted average of both sensors
#'   \item \strong{radial_profile}: Assumes linear decline from outer to inner sensor
#' }
#'
#' The choice of scaling method depends on expected radial patterns:
#' \itemize{
#'   \item Use "outer_only" for species with flow concentrated near cambium
#'   \item Use "weighted_average" for relatively uniform radial flow
#'   \item Use "radial_profile" when expecting gradual radial decline
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate sapwood area
#' sapwood_area <- calc_sapwood_area(diameter_breast_height = 18.2)
#'
#' # Scale flux density to tree level
#' tree_flux <- calc_tree_sap_flux(
#'   flux_results,
#'   sapwood_area,
#'   scaling_method = "weighted_average",
#'   flux_units = "L_day"
#' )
#'
#' # Compare scaling methods
#' methods <- c("outer_only", "weighted_average", "radial_profile")
#' comparison <- lapply(methods, function(method) {
#'   calc_tree_sap_flux(flux_results, sapwood_area, scaling_method = method)
#' })
#' }
#'
#' @seealso \code{\link{calc_sapwood_area}}, \code{\link{calc_sap_flux_density}}
#' @export
calc_tree_sap_flux <- function(flux_results,
                               sapwood_area_data,
                               scaling_method = "outer_only",
                               flux_units = "L_hr",
                               water_density = 1.0) {

  # Input validation
  if (!is.data.frame(flux_results)) {
    stop("flux_results must be a data frame")
  }

  if (!"Jv_cm3_cm2_hr" %in% names(flux_results)) {
    stop("flux_results must contain 'Jv_cm3_cm2_hr' column from calc_sap_flux_density()")
  }

  if (!inherits(sapwood_area_data, "sapwood_area")) {
    stop("sapwood_area_data must be from calc_sapwood_area()")
  }

  if (!scaling_method %in% c("outer_only", "inner_only", "weighted_average", "radial_profile")) {
    stop("scaling_method must be one of: outer_only, inner_only, weighted_average, radial_profile")
  }

  if (!flux_units %in% c("L_hr", "L_day", "kg_hr", "kg_day")) {
    stop("flux_units must be one of: L_hr, L_day, kg_hr, kg_day")
  }

  # Extract sapwood area data
  total_sapwood_area <- sapwood_area_data$sapwood_measurements$total_sapwood_area
  outer_sensor_area <- sapwood_area_data$probe_measurements$outer_sensor_area
  inner_sensor_area <- sapwood_area_data$probe_measurements$inner_sensor_area

  # Check if required sensor positions are available
  available_sensors <- unique(flux_results$sensor_position)

  if (scaling_method %in% c("weighted_average", "radial_profile")) {
    if (!all(c("outer", "inner") %in% available_sensors)) {
      warning("Both outer and inner sensors required for '", scaling_method,
              "'. Falling back to 'outer_only' method.")
      scaling_method <- "outer_only"
    }
  }

  # Prepare results dataframe
  tree_flux_results <- flux_results

  # Apply scaling method
  if (scaling_method == "outer_only") {

    # Use outer sensor measurements
    outer_data <- flux_results[flux_results$sensor_position == "outer", ]
    if (nrow(outer_data) == 0) {
      stop("No outer sensor data available for outer_only scaling")
    }

    # Scale flux density to total sapwood area
    tree_flux_results <- outer_data
    tree_flux_results$tree_flux <- (outer_data$Jv_cm3_cm2_hr * total_sapwood_area) / 1000  # Convert to L/hr
    tree_flux_results$extrapolation_factor <- total_sapwood_area / total_sapwood_area  # 1.0
    tree_flux_results$sensor_coverage <- 1.0  # Assume outer represents entire area

  } else if (scaling_method == "inner_only") {

    # Use inner sensor measurements
    inner_data <- flux_results[flux_results$sensor_position == "inner", ]
    if (nrow(inner_data) == 0) {
      stop("No inner sensor data available for inner_only scaling")
    }

    # Scale flux density to total sapwood area
    tree_flux_results <- inner_data
    tree_flux_results$tree_flux <- (inner_data$Jv_cm3_cm2_hr * total_sapwood_area) / 1000  # Convert to L/hr
    tree_flux_results$extrapolation_factor <- total_sapwood_area / total_sapwood_area  # 1.0
    tree_flux_results$sensor_coverage <- 1.0  # Assume inner represents entire area

  } else if (scaling_method == "weighted_average") {

    # Separate outer and inner data
    outer_data <- flux_results[flux_results$sensor_position == "outer", ]
    inner_data <- flux_results[flux_results$sensor_position == "inner", ]

    # Match timestamps between sensors
    if ("datetime" %in% names(flux_results)) {
      merged_data <- merge(outer_data, inner_data,
                           by = intersect(names(outer_data), names(inner_data)),
                           suffixes = c("_outer", "_inner"))
    } else {
      # If no datetime, assume same length and order
      if (nrow(outer_data) != nrow(inner_data)) {
        stop("Cannot match outer and inner sensor data for weighted averaging")
      }
      merged_data <- cbind(outer_data, inner_data[, "Jv_cm3_cm2_hr", drop = FALSE])
      names(merged_data)[ncol(merged_data)] <- "Jv_cm3_cm2_hr_inner"
      names(merged_data)[which(names(merged_data) == "Jv_cm3_cm2_hr")] <- "Jv_cm3_cm2_hr_outer"
    }

    # Check if merge was successful
    if (nrow(merged_data) == 0) {
      stop("No matching data between outer and inner sensors for weighted average scaling")
    }

    # Calculate area-weighted flux density
    total_measured_area <- outer_sensor_area + inner_sensor_area
    outer_weight <- outer_sensor_area / total_measured_area
    inner_weight <- inner_sensor_area / total_measured_area

    merged_data$Jv_weighted <- (merged_data$Jv_cm3_cm2_hr_outer * outer_weight +
                                  merged_data$Jv_cm3_cm2_hr_inner * inner_weight)

    # Scale to tree level
    merged_data$tree_flux <- (merged_data$Jv_weighted * total_sapwood_area) / 1000
    merged_data$extrapolation_factor <- total_sapwood_area / total_measured_area
    merged_data$sensor_coverage <- total_measured_area / total_sapwood_area

    # Clean up merged data for return
    cols_to_keep <- setdiff(names(flux_results), "Jv_cm3_cm2_hr")
    tree_flux_results <- merged_data[, c(cols_to_keep, "Jv_weighted", "tree_flux",
                                         "extrapolation_factor", "sensor_coverage")]
    names(tree_flux_results)[names(tree_flux_results) == "Jv_weighted"] <- "Jv_cm3_cm2_hr"

  } else if (scaling_method == "radial_profile") {

    # Implement simplified radial profile (linear interpolation between sensors)
    outer_data <- flux_results[flux_results$sensor_position == "outer", ]
    inner_data <- flux_results[flux_results$sensor_position == "inner", ]

    # Match data as in weighted_average
    if ("datetime" %in% names(flux_results)) {
      merged_data <- merge(outer_data, inner_data,
                           by = intersect(names(outer_data), names(inner_data)),
                           suffixes = c("_outer", "_inner"))
    } else {
      if (nrow(outer_data) != nrow(inner_data)) {
        stop("Cannot match outer and inner sensor data for radial profile")
      }
      merged_data <- cbind(outer_data, inner_data[, "Jv_cm3_cm2_hr", drop = FALSE])
      names(merged_data)[ncol(merged_data)] <- "Jv_cm3_cm2_hr_inner"
      names(merged_data)[which(names(merged_data) == "Jv_cm3_cm2_hr")] <- "Jv_cm3_cm2_hr_outer"
    }

    # Check if merge was successful
    if (nrow(merged_data) == 0) {
      stop("No matching data between outer and inner sensors for radial profile scaling")
    }

    # Calculate average flux (simplified radial integration)
    merged_data$Jv_profile_mean <- (merged_data$Jv_cm3_cm2_hr_outer + merged_data$Jv_cm3_cm2_hr_inner) / 2

    # Scale to tree level
    merged_data$tree_flux <- (merged_data$Jv_profile_mean * total_sapwood_area) / 1000
    merged_data$extrapolation_factor <- total_sapwood_area / (outer_sensor_area + inner_sensor_area)
    merged_data$sensor_coverage <- (outer_sensor_area + inner_sensor_area) / total_sapwood_area

    # Clean up for return
    cols_to_keep <- setdiff(names(flux_results), "Jv_cm3_cm2_hr")
    tree_flux_results <- merged_data[, c(cols_to_keep, "Jv_profile_mean", "tree_flux",
                                         "extrapolation_factor", "sensor_coverage")]
    names(tree_flux_results)[names(tree_flux_results) == "Jv_profile_mean"] <- "Jv_cm3_cm2_hr"
  }

  # Convert units if needed
  if (flux_units == "L_day") {
    tree_flux_results$tree_flux <- tree_flux_results$tree_flux * 24
  } else if (flux_units == "kg_hr") {
    tree_flux_results$tree_flux <- tree_flux_results$tree_flux * water_density
  } else if (flux_units == "kg_day") {
    tree_flux_results$tree_flux <- tree_flux_results$tree_flux * 24 * water_density
  }

  # Add metadata
  tree_flux_results$flux_units <- flux_units
  tree_flux_results$total_sapwood_area_cm2 <- total_sapwood_area
  tree_flux_results$scaling_method <- scaling_method

  # Set class
  class(tree_flux_results) <- c("tree_sap_flux", "data.frame")

  return(tree_flux_results)
}