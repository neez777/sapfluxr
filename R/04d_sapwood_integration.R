# R/04d_sapwood_integration.R
# Sapwood Area Integration and Sap Flux Calculation
# Implements Hatton et al. (1990) weighted average method

#' Calculate Sapwood Area for Concentric Rings
#'
#' Calculates the cross-sectional area of sapwood rings based on tree
#' dimensions and sensor positions. Follows the ring allocation approach
#' for outer/inner sensor configurations.
#'
#' @param dbh Diameter at breast height (cm)
#' @param bark_thickness Bark thickness (cm). Default: 0
#' @param sapwood_depth Total sapwood depth from cambium (cm)
#' @param sensor_positions Character vector of sensor positions.
#'   Options: "outer" (5mm depth), "inner" (12.5mm depth).
#'   Default: c("outer", "inner")
#'
#' @return List containing:
#'   \describe{
#'     \item{total_sapwood_area_cm2}{Total conducting sapwood area (cm²)}
#'     \item{rings}{Data frame with columns: sensor, ring_name,
#'                  inner_radius_cm, outer_radius_cm, area_cm2,
#'                  depth_from_cambium_cm}
#'     \item{tree_dimensions}{List of tree dimensions used}
#'   }
#'
#' @details
#' **Ring Allocation Rules (based on sapwood depth):**
#'
#' - **Sapwood < 1.0 cm:** Only outer ring (Aso), inner sensor in heartwood
#' - **Sapwood 1.0-2.0 cm:** Outer ring (Aso) + partial inner ring (Asi)
#' - **Sapwood 2.0-2.5 cm:** Outer ring (Aso) + full inner ring (Asi)
#' - **Sapwood > 2.5 cm:** Outer (Aso) + inner (Asi) + innermost (Asim)
#'
#' **Ring boundaries:**
#' - Outer ring: 0-1.0 cm depth (outer sensor at 0.5 cm)
#' - Inner ring: 1.0-2.0 cm depth (inner sensor at 1.25 cm)
#' - Innermost ring: 2.0 cm to sapwood boundary
#'
#' @examples
#' \dontrun{
#' # Tree with 3.5 cm sapwood depth, 30 cm DBH
#' areas <- calc_sapwood_areas(
#'   dbh = 30,
#'   bark_thickness = 0.5,
#'   sapwood_depth = 3.5
#' )
#'
#' # View ring allocation
#' print(areas$rings)
#'
#' # Total sapwood area
#' print(areas$total_sapwood_area_cm2)
#' }
#'
#' @references
#' Hatton, T.J., Catchpole, E.A., & Vertessy, R.A. (1990). Integration of
#' sapflow velocity to estimate plant water use. Tree Physiology, 6, 201-209.
#'
#' @family sapwood integration functions
#' @export
calc_sapwood_areas <- function(dbh,
                                bark_thickness = 0,
                                sapwood_depth,
                                sensor_positions = c("outer", "inner")) {

  # Input validation
  if (!is.numeric(dbh) || dbh <= 0) {
    stop("dbh must be a positive number (cm)")
  }

  if (!is.numeric(bark_thickness) || bark_thickness < 0) {
    stop("bark_thickness must be a non-negative number (cm)")
  }

  if (!is.numeric(sapwood_depth) || sapwood_depth <= 0) {
    stop("sapwood_depth must be a positive number (cm)")
  }

  if (sapwood_depth + bark_thickness > dbh / 2) {
    warning(
      "Sapwood depth (", sapwood_depth, " cm) + bark thickness (",
      bark_thickness, " cm) exceeds stem radius (", dbh/2, " cm). ",
      "Check your measurements."
    )
  }

  # Calculate radii
  stem_radius_cm <- dbh / 2
  cambium_radius_cm <- stem_radius_cm - bark_thickness
  heartwood_radius_cm <- cambium_radius_cm - sapwood_depth

  if (heartwood_radius_cm < 0) {
    heartwood_radius_cm <- 0
    warning("Tree has no heartwood (sapwood extends to pith)")
  }

  # Total sapwood area
  total_sapwood_area_cm2 <- pi * (cambium_radius_cm^2 - heartwood_radius_cm^2)

  # Determine ring allocation based on sapwood depth
  rings <- data.frame(
    sensor = character(),
    ring_name = character(),
    inner_radius_cm = numeric(),
    outer_radius_cm = numeric(),
    area_cm2 = numeric(),
    depth_from_cambium_cm = character(),
    stringsAsFactors = FALSE
  )

  # Ring boundary definitions (fixed detection zones)
  outer_boundary_cm <- 1.0  # Outer ring: 0-10mm depth
  inner_boundary_cm <- 2.0  # Inner ring: 10-20mm depth

  # Case 1: Sapwood < 10mm (only outer ring)
  if (sapwood_depth < outer_boundary_cm) {
    # Outer ring: cambium to heartwood boundary
    r_outer <- cambium_radius_cm
    r_inner <- heartwood_radius_cm
    area <- pi * (r_outer^2 - r_inner^2)

    rings <- rbind(rings, data.frame(
      sensor = "outer",
      ring_name = "outer_ring",
      inner_radius_cm = r_inner,
      outer_radius_cm = r_outer,
      area_cm2 = area,
      depth_from_cambium_cm = sprintf("0.0-%.1f", sapwood_depth),
      stringsAsFactors = FALSE
    ))
  }

  # Case 2: Sapwood 10-20mm (outer + partial inner)
  else if (sapwood_depth >= outer_boundary_cm && sapwood_depth < inner_boundary_cm) {
    # Outer ring: cambium to 1.0cm depth
    r_outer_out <- cambium_radius_cm
    r_outer_in <- cambium_radius_cm - outer_boundary_cm
    area_outer <- pi * (r_outer_out^2 - r_outer_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "outer",
      ring_name = "outer_ring",
      inner_radius_cm = r_outer_in,
      outer_radius_cm = r_outer_out,
      area_cm2 = area_outer,
      depth_from_cambium_cm = "0.0-1.0",
      stringsAsFactors = FALSE
    ))

    # Inner ring: 1.0cm to heartwood boundary
    r_inner_out <- cambium_radius_cm - outer_boundary_cm
    r_inner_in <- heartwood_radius_cm
    area_inner <- pi * (r_inner_out^2 - r_inner_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "inner",
      ring_name = "inner_ring",
      inner_radius_cm = r_inner_in,
      outer_radius_cm = r_inner_out,
      area_cm2 = area_inner,
      depth_from_cambium_cm = sprintf("1.0-%.1f", sapwood_depth),
      stringsAsFactors = FALSE
    ))
  }

  # Case 3: Sapwood 20-25mm (outer + full inner)
  else if (sapwood_depth >= inner_boundary_cm && sapwood_depth < 2.5) {
    # Outer ring: cambium to 1.0cm
    r_outer_out <- cambium_radius_cm
    r_outer_in <- cambium_radius_cm - outer_boundary_cm
    area_outer <- pi * (r_outer_out^2 - r_outer_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "outer",
      ring_name = "outer_ring",
      inner_radius_cm = r_outer_in,
      outer_radius_cm = r_outer_out,
      area_cm2 = area_outer,
      depth_from_cambium_cm = "0.0-1.0",
      stringsAsFactors = FALSE
    ))

    # Inner ring: 1.0-2.0cm
    r_inner_out <- cambium_radius_cm - outer_boundary_cm
    r_inner_in <- cambium_radius_cm - inner_boundary_cm
    area_inner <- pi * (r_inner_out^2 - r_inner_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "inner",
      ring_name = "inner_ring",
      inner_radius_cm = r_inner_in,
      outer_radius_cm = r_inner_out,
      area_cm2 = area_inner,
      depth_from_cambium_cm = "1.0-2.0",
      stringsAsFactors = FALSE
    ))
  }

  # Case 4: Sapwood > 25mm (outer + inner + innermost)
  else {
    # Outer ring: cambium to 1.0cm
    r_outer_out <- cambium_radius_cm
    r_outer_in <- cambium_radius_cm - outer_boundary_cm
    area_outer <- pi * (r_outer_out^2 - r_outer_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "outer",
      ring_name = "outer_ring",
      inner_radius_cm = r_outer_in,
      outer_radius_cm = r_outer_out,
      area_cm2 = area_outer,
      depth_from_cambium_cm = "0.0-1.0",
      stringsAsFactors = FALSE
    ))

    # Inner ring: 1.0-2.0cm
    r_inner_out <- cambium_radius_cm - outer_boundary_cm
    r_inner_in <- cambium_radius_cm - inner_boundary_cm
    area_inner <- pi * (r_inner_out^2 - r_inner_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "inner",
      ring_name = "inner_ring",
      inner_radius_cm = r_inner_in,
      outer_radius_cm = r_inner_out,
      area_cm2 = area_inner,
      depth_from_cambium_cm = "1.0-2.0",
      stringsAsFactors = FALSE
    ))

    # Innermost ring: 2.0cm to heartwood boundary
    r_innermost_out <- cambium_radius_cm - inner_boundary_cm
    r_innermost_in <- heartwood_radius_cm
    area_innermost <- pi * (r_innermost_out^2 - r_innermost_in^2)

    rings <- rbind(rings, data.frame(
      sensor = "innermost",
      ring_name = "innermost_ring",
      inner_radius_cm = r_innermost_in,
      outer_radius_cm = r_innermost_out,
      area_cm2 = area_innermost,
      depth_from_cambium_cm = sprintf("2.0-%.1f", sapwood_depth),
      stringsAsFactors = FALSE
    ))
  }

  # Return results
  list(
    total_sapwood_area_cm2 = total_sapwood_area_cm2,
    rings = rings,
    tree_dimensions = list(
      dbh_cm = dbh,
      bark_thickness_cm = bark_thickness,
      stem_radius_cm = stem_radius_cm,
      cambium_radius_cm = cambium_radius_cm,
      sapwood_depth_cm = sapwood_depth,
      heartwood_radius_cm = heartwood_radius_cm
    )
  )
}


#' Calculate Sap Flux by Integrating Over Sapwood Rings
#'
#' Integrates sap flux density (Jv) measurements from multiple sensor depths
#' over the sapwood cross-sectional area to calculate total sap flux (Q).
#' Implements the weighted average method from Hatton et al. (1990).
#'
#' @param flux_data Data frame with sap flux density measurements. Must contain:
#'   \describe{
#'     \item{datetime}{Timestamp}
#'     \item{sensor_position}{Sensor position ("outer", "inner")}
#'     \item{Jv_cm3_cm2_hr}{Sap flux density (cm³/cm²/hr)}
#'   }
#' @param sapwood_areas Output from \code{\link{calc_sapwood_areas}}
#' @param method Integration method. Options: "weighted_average" (default,
#'   Hatton 1990) or "simple" (uniform weighting)
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Q_cm3_hr}{Total sap flux (cm³/hr) per timestamp}
#'     \item{Q_L_hr}{Total sap flux (L/hr) per timestamp}
#'     \item{Q_L_day}{Total sap flux (L/day) per timestamp}
#'   }
#'
#' @details
#' **Velocity Assumptions for Unmeasured Regions:**
#'
#' Based on your spreadsheet specifications:
#'
#' 1. **Sapwood 10-20mm with inner sensor in heartwood:**
#'    - Inner ring velocity = Outer velocity / 2
#'    - (Linear decrease from outer boundary to heartwood)
#'
#' 2. **Sapwood > 25mm with innermost region beyond sensors:**
#'    - Innermost ring velocity = Inner velocity / 2
#'    - (Linear decrease from inner boundary to heartwood)
#'
#' **Integration Formula (Hatton et al. 1990):**
#'
#' Q = Σ (Aₖ × Jvₖ)
#'
#' Where:
#' - Aₖ = area of ring k (cm²)
#' - Jvₖ = flux density for ring k (cm³/cm²/hr)
#' - Sum over all sapwood rings
#'
#' @examples
#' \dontrun{
#' # Calculate sapwood areas
#' areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 3.5)
#'
#' # Integrate flux density measurements
#' flux_data <- calc_sap_flux(flux_data, areas)
#'
#' # View total flux
#' head(flux_data[, c("datetime", "Q_L_hr", "Q_L_day")])
#' }
#'
#' @references
#' Hatton, T.J., Catchpole, E.A., & Vertessy, R.A. (1990). Integration of
#' sapflow velocity to estimate plant water use. Tree Physiology, 6, 201-209.
#'
#' @family sapwood integration functions
#' @export
calc_sap_flux <- function(flux_data,
                          sapwood_areas,
                          method = "weighted_average") {

  # Input validation
  if (!is.data.frame(flux_data)) {
    stop("flux_data must be a data frame")
  }

  required_cols <- c("datetime", "sensor_position", "Jv_cm3_cm2_hr")
  missing_cols <- setdiff(required_cols, names(flux_data))
  if (length(missing_cols) > 0) {
    stop(
      "flux_data missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!is.list(sapwood_areas) || is.null(sapwood_areas$rings)) {
    stop("sapwood_areas must be output from calc_sapwood_areas()")
  }

  # Get sapwood depth for determining velocity assumptions
  sapwood_depth <- sapwood_areas$tree_dimensions$sapwood_depth_cm
  rings <- sapwood_areas$rings

  # Process each timestamp
  results <- flux_data %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(
      Q_cm3_hr = calc_flux_single_timestamp(
        sensor_positions = sensor_position,
        Jv_values = Jv_cm3_cm2_hr,
        rings = rings,
        sapwood_depth = sapwood_depth,
        method = method
      ),
      .groups = "drop"
    )

  # Add L/hr and L/day
  results$Q_L_hr <- results$Q_cm3_hr / 1000
  results$Q_L_day <- results$Q_L_hr * 24

  # Merge back with original data
  flux_data <- dplyr::left_join(flux_data, results, by = "datetime")

  return(flux_data)
}


#' Calculate Flux for a Single Timestamp (Internal Helper)
#'
#' @param sensor_positions Vector of sensor positions at this timestamp
#' @param Jv_values Vector of Jv values corresponding to sensors
#' @param rings Sapwood rings data frame
#' @param sapwood_depth Total sapwood depth (cm)
#' @param method Integration method
#'
#' @return Total flux (cm³/hr) for this timestamp
#' @keywords internal
calc_flux_single_timestamp <- function(sensor_positions,
                                        Jv_values,
                                        rings,
                                        sapwood_depth,
                                        method) {

  # Get Jv for each sensor (handle missing values)
  Jv_outer <- Jv_values[sensor_positions == "outer"][1]
  Jv_inner <- Jv_values[sensor_positions == "inner"][1]

  if (is.na(Jv_outer)) Jv_outer <- 0
  if (is.na(Jv_inner)) Jv_inner <- 0

  # Initialize flux components
  Q_total <- 0

  # Integrate over each ring
  for (i in seq_len(nrow(rings))) {
    ring <- rings[i, ]
    area <- ring$area_cm2

    if (ring$sensor == "outer") {
      # Outer ring always uses outer sensor measurement
      Q_total <- Q_total + area * Jv_outer

    } else if (ring$sensor == "inner") {
      # Inner ring: depends on sapwood depth
      # Use first value if vector (should be constant per tree)
      sw_depth <- sapwood_depth[1]
      if (sw_depth >= 1.0 && sw_depth < 2.0) {
        # Case 2: Inner sensor in heartwood, assume Jv = outer/2
        Q_total <- Q_total + area * (Jv_outer / 2)
      } else {
        # Case 3+: Inner sensor measures this ring directly
        Q_total <- Q_total + area * Jv_inner
      }

    } else if (ring$sensor == "innermost") {
      # Innermost ring (sapwood > 25mm): assume Jv = inner/2
      Q_total <- Q_total + area * (Jv_inner / 2)
    }
  }

  return(Q_total)
}


#' Apply Sap Flux Integration to Data Frame
#'
#' Convenience wrapper that calculates sapwood areas and integrates flux
#' in a single call.
#'
#' @param flux_data Data frame with Jv measurements and tree dimensions
#' @param dbh_col Name of DBH column (cm). Default: "dbh"
#' @param sapwood_depth_col Name of sapwood depth column (cm).
#'   Default: "sapwood_depth"
#' @param bark_thickness_col Name of bark thickness column (cm).
#'   Default: "bark_thickness". If NULL, assumes 0.
#' @param method Integration method. Default: "weighted_average"
#'
#' @return Data frame with added Q columns
#'
#' @examples
#' \dontrun{
#' # If your data has tree dimensions
#' flux_data$dbh <- 30
#' flux_data$sapwood_depth <- 3.5
#' flux_data$bark_thickness <- 0.5
#'
#' result <- apply_sap_flux_integration(flux_data)
#' }
#'
#' @family sapwood integration functions
#' @export
apply_sap_flux_integration <- function(flux_data,
                                        dbh_col = "dbh",
                                        sapwood_depth_col = "sapwood_depth",
                                        bark_thickness_col = "bark_thickness",
                                        method = "weighted_average") {

  # Validate columns exist
  if (!dbh_col %in% names(flux_data)) {
    stop("Column '", dbh_col, "' not found in flux_data")
  }

  if (!sapwood_depth_col %in% names(flux_data)) {
    stop("Column '", sapwood_depth_col, "' not found in flux_data")
  }

  # Get tree dimensions (assume constant per tree)
  dbh <- unique(flux_data[[dbh_col]])[1]
  sapwood_depth <- unique(flux_data[[sapwood_depth_col]])[1]

  if (is.null(bark_thickness_col) || !bark_thickness_col %in% names(flux_data)) {
    bark_thickness <- 0
  } else {
    bark_thickness <- unique(flux_data[[bark_thickness_col]])[1]
  }

  # Calculate sapwood areas
  sapwood_areas <- calc_sapwood_areas(
    dbh = dbh,
    bark_thickness = bark_thickness,
    sapwood_depth = sapwood_depth
  )

  # Integrate flux
  result <- calc_sap_flux(
    flux_data = flux_data,
    sapwood_areas = sapwood_areas,
    method = method
  )

  # Add sapwood area info as attribute
  attr(result, "sapwood_areas") <- sapwood_areas

  # Print summary
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SAP FLUX INTEGRATION APPLIED\n")
  cat(strrep("=", 70), "\n\n")

  cat("Tree Dimensions:\n")
  cat(sprintf("  DBH: %.1f cm\n", dbh))
  cat(sprintf("  Bark thickness: %.1f cm\n", bark_thickness))
  cat(sprintf("  Sapwood depth: %.1f cm\n", sapwood_depth))
  cat(sprintf("  Total sapwood area: %.1f cm²\n",
              sapwood_areas$total_sapwood_area_cm2))
  cat("\n")

  cat("Sapwood Rings:\n")
  print(sapwood_areas$rings[, c("ring_name", "area_cm2", "depth_from_cambium_cm")])
  cat("\n")

  cat("Flux Summary:\n")
  cat(sprintf("  Mean flux: %.2f L/hr (%.2f L/day)\n",
              mean(result$Q_L_hr, na.rm = TRUE),
              mean(result$Q_L_day, na.rm = TRUE)))
  cat(sprintf("  Max flux: %.2f L/hr (%.2f L/day)\n",
              max(result$Q_L_hr, na.rm = TRUE),
              max(result$Q_L_day, na.rm = TRUE)))
  cat("\n")

  cat(strrep("=", 70), "\n\n")

  return(result)
}
