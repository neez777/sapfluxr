# R/04d_sapwood_integration.R
# Sapwood Area Integration and Sap Flux Calculation
# Implements Hatton et al. (1990) weighted average method

#' Calculate Sapwood Area for Concentric Rings
#'
#' Calculates the cross-sectional area of sapwood rings (annuli) based on tree
#' dimensions, probe geometry, and sensor positions. Implements the Hatton et al.
#' (1990) weighted average method with probe-derived annulus boundaries.
#'
#' @param dbh Diameter at breast height (cm).
#' @param bark_thickness_dbh Bark thickness at the DBH measurement site (cm).
#'   This is the **full, unshaved** bark and is used to derive the inner-bark
#'   (cambium) radius: \code{cambium_radius = dbh/2 - bark_thickness_dbh}.
#' @param bark_thickness_probe Bark thickness at the probe installation site (cm),
#'   after shaving. Must be \eqn{\le} \code{bark_thickness_dbh}. Used together
#'   with \code{spacer_thickness} (from the probe config) to calculate sensor
#'   depths from the cambium:
#'   \deqn{d_{sensor} = \frac{(\text{probe length} - \text{sensor from tip}) -
#'     (\text{bark\_probe} \times 10 + \text{spacer\_mm})}{10}}
#' @param sapwood_thickness Radial thickness of the conducting sapwood (cm),
#'   measured from the **cambium (inner bark) surface** to the sapwood-heartwood
#'   boundary. This is the sapwood layer width only — do not include bark.
#'   Equivalent to what Tim's spreadsheet calls "Sapwood width".
#' @param sensor_positions Character vector of sensor positions to include.
#'   Options: \code{"outer"}, \code{"inner"}. Default: \code{c("outer", "inner")}.
#' @param probe_config Optional probe configuration. Accepts:
#'   \describe{
#'     \item{NULL}{Uses standard ICT SFM1 defaults: probe length 35 mm,
#'       outer sensor 22.5 mm from tip, inner sensor 7.5 mm from tip.}
#'     \item{ProbeConfiguration}{Object from \code{\link{load_probe_config}()}.
#'       Probe dimensions are read from the stored YAML data.}
#'     \item{named list}{With fields \code{length}, \code{outer_sensor},
#'       \code{inner_sensor} (all in mm, distances from probe tip).}
#'   }
#'
#' @return List containing:
#'   \describe{
#'     \item{total_sapwood_area_cm2}{Total conducting sapwood area (cm^2).}
#'     \item{rings}{Data frame with one row per annulus and columns:
#'       \code{sensor} (which sensor or "sensorless"),
#'       \code{sensor_source} ("outer" or "inner" — which sensor's Jv to use),
#'       \code{measured} (logical — TRUE if a sensor directly measures this ring),
#'       \code{ring_name}, \code{inner_radius_cm}, \code{outer_radius_cm},
#'       \code{area_cm2}, \code{depth_from_cambium_cm}.}
#'     \item{tree_dimensions}{List of tree geometry values, including
#'       \code{actual_sapwood_cm} (sapwood thickness from cambium).}
#'     \item{probe_landmarks}{List of probe geometry depths from cambium:
#'       outer and inner sensor positions, their midpoint, and the probe tip.}
#'   }
#'
#' @details
#' ## Measurement convention
#'
#' \code{sapwood_thickness} is the radial width of the conducting sapwood only —
#' measured from the cambium to the heartwood boundary (IB→HW). Do not add bark.
#' This matches the "Sapwood width" column in Tim's reference spreadsheet.
#'
#' Two bark parameters are required because the bark at the DBH measurement site
#' (intact, full thickness) differs from the bark at the probe installation site
#' (typically shaved down). Using only the probe-site (shaved) value for the
#' cambium-radius formula causes systematic overestimation of sapwood area.
#'
#' ## Probe geometry and annulus boundaries
#'
#' Sensor depths from the cambium are derived from the probe configuration,
#' the probe-site bark thickness, and the spacer thickness (from the probe config):
#'
#' \deqn{d_{sensor} = \frac{(\text{probe length} - \text{sensor from tip}) -
#'   (\text{bark\_probe} \times 10 + \text{spacer\_mm})}{10}}
#'
#' Three fixed probe landmarks (all from the cambium) determine the annulus
#' boundaries:
#' \describe{
#'   \item{Midpoint}{Mean depth of outer and inner sensors. Always the boundary
#'     between the outer annulus and the inner annulus (or sensorless zone).}
#'   \item{Inner sensor depth}{Determines whether the inner annulus is directly
#'     measured or estimated.}
#'   \item{Probe tip depth}{If sapwood extends beyond the probe tip, a sensorless
#'     annulus is added from the probe tip to the heartwood boundary.}
#' }
#'
#' \strong{Annulus allocation} (example: standard ICT probe, bark_probe = 0.5 cm, spacer = 0)
#'
#' Probe landmarks from cambium: outer sensor 0.75 cm, midpoint 1.50 cm,
#' inner sensor 2.25 cm, probe tip 3.00 cm.
#'
#' \describe{
#'   \item{Actual sapwood < 0.75 cm}{Error — outer sensor is not within the sapwood.}
#'   \item{Actual sapwood 0.75-1.50 cm}{1 annulus: outer sensor measures the full
#'     sapwood depth (0 to sapwood boundary).}
#'   \item{Actual sapwood 1.50-2.25 cm}{2 annuli: outer sensor (0 to 1.50 cm);
#'     sensorless zone (1.50 cm to sapwood boundary, estimated as Jv_outer / 2).}
#'   \item{Actual sapwood 2.25-3.00 cm}{2 annuli: outer sensor (0 to 1.50 cm);
#'     inner sensor (1.50 cm to sapwood boundary).}
#'   \item{Actual sapwood over 3.00 cm}{3 annuli: outer sensor (0 to 1.50 cm);
#'     inner sensor (1.50 to 3.00 cm); sensorless zone (3.00 cm to sapwood
#'     boundary, estimated as Jv_inner / 2).}
#' }
#'
#' ## Sensorless annuli
#'
#' When an annulus has no direct sensor measurement (\code{measured = FALSE}),
#' \code{\link{calc_sap_flux}} estimates its flux density as half that of the
#' nearest measured sensor (\code{sensor_source}), following a linear decrease
#' assumption.
#'
#' @examples
#' \dontrun{
#' # Standard case: full bark 0.5 cm (no shaving), sapwood thickness 2.0 cm
#' areas <- calc_sapwood_areas(
#'   dbh                  = 30,
#'   bark_thickness_dbh   = 0.5,
#'   bark_thickness_probe = 0.5,
#'   sapwood_thickness    = 2.0
#' )
#' print(areas$rings)
#' print(areas$total_sapwood_area_cm2)
#'
#' # Probe installed through shaved bark (Tim's reference geometry)
#' probe <- load_probe_config("symmetrical")
#' probe$yaml_data$probe$spacer_thickness <- 2.5  # 2.5 mm hub-to-bark gap
#' areas2 <- calc_sapwood_areas(
#'   dbh                  = 18.2,
#'   bark_thickness_dbh   = 1.7,
#'   bark_thickness_probe = 0.5,
#'   sapwood_thickness    = 3.0,
#'   probe_config         = probe
#' )
#' print(areas2$total_sapwood_area_cm2)  # ~111.21 cm^2
#' }
#'
#' @references
#' Hatton, T.J., Catchpole, E.A., & Vertessy, R.A. (1990). Integration of
#' sapflow velocity to estimate plant water use. Tree Physiology, 6, 201-209.
#'
#' @seealso \code{\link{calc_sap_flux}}, \code{\link{plot_radial_velocity_profile}}
#'
#' @family sapwood integration functions
#' @export
calc_sapwood_areas <- function(dbh,
                                bark_thickness_dbh,
                                bark_thickness_probe,
                                sapwood_thickness,
                                sensor_positions = c("outer", "inner"),
                                probe_config = NULL) {

  # ── Input validation ────────────────────────────────────────────────────────
  if (!is.numeric(dbh) || dbh <= 0) {
    stop("dbh must be a positive number (cm)")
  }
  if (!is.numeric(bark_thickness_dbh) || bark_thickness_dbh < 0) {
    stop("bark_thickness_dbh must be a non-negative number (cm)")
  }
  if (!is.numeric(bark_thickness_probe) || bark_thickness_probe < 0) {
    stop("bark_thickness_probe must be a non-negative number (cm)")
  }
  if (bark_thickness_probe > bark_thickness_dbh) {
    stop(sprintf(
      "bark_thickness_probe (%.2f cm) cannot exceed bark_thickness_dbh (%.2f cm). ",
      bark_thickness_probe, bark_thickness_dbh
    ),
    "Shaving can only remove bark, not add it.")
  }
  if (!is.numeric(sapwood_thickness) || sapwood_thickness <= 0) {
    stop("sapwood_thickness must be a positive number (cm)")
  }

  cambium_radius_cm <- dbh / 2 - bark_thickness_dbh
  if (sapwood_thickness > cambium_radius_cm) {
    warning(
      "sapwood_thickness (", sapwood_thickness, " cm) exceeds cambium radius (",
      round(cambium_radius_cm, 3), " cm). Check your measurements."
    )
  }

  # ── Extract probe parameters ────────────────────────────────────────────────
  if (is.null(probe_config)) {
    probe_length_mm     <- 35
    outer_from_tip_mm   <- 22.5
    inner_from_tip_mm   <- 7.5
    spacer_thickness_mm <- 0
  } else if (inherits(probe_config, "ProbeConfiguration")) {
    # R6 ProbeConfiguration — pull dimensions from stored yaml_data
    pc                  <- probe_config$yaml_data$probe
    probe_length_mm     <- pc$length            %||% 35
    outer_from_tip_mm   <- pc$outer_sensor      %||% 22.5
    inner_from_tip_mm   <- pc$inner_sensor      %||% 7.5
    spacer_thickness_mm <- pc$spacer_thickness  %||% 0
  } else if (is.list(probe_config)) {
    # Plain list: accept either top-level fields or nested under $probe
    pc                  <- probe_config$probe %||% probe_config
    probe_length_mm     <- pc$length            %||% probe_config$probe_length %||% 35
    outer_from_tip_mm   <- pc$outer_sensor      %||% 22.5
    inner_from_tip_mm   <- pc$inner_sensor      %||% 7.5
    spacer_thickness_mm <- pc$spacer_thickness  %||% 0
  } else {
    stop("probe_config must be NULL, a named list, or a ProbeConfiguration object")
  }

  # ── Tree geometry ────────────────────────────────────────────────────────────
  actual_sapwood_cm   <- sapwood_thickness
  stem_radius_cm      <- dbh / 2
  heartwood_radius_cm <- cambium_radius_cm - actual_sapwood_cm

  if (heartwood_radius_cm < 0) {
    heartwood_radius_cm <- 0
    warning("Tree has no heartwood (sapwood extends to pith)")
  }

  total_sapwood_area_cm2 <- pi * (cambium_radius_cm^2 - heartwood_radius_cm^2)

  # ── Probe landmark depths from cambium (cm) ──────────────────────────────────
  # The probe needle hub sits on the shaved bark surface. The install offset
  # combines the remaining (probe-site) bark and any spacer between hub and bark.
  install_offset_mm <- bark_thickness_probe * 10 + spacer_thickness_mm
  outer_sensor_cm   <- ((probe_length_mm - outer_from_tip_mm) - install_offset_mm) / 10
  inner_sensor_cm   <- ((probe_length_mm - inner_from_tip_mm) - install_offset_mm) / 10
  midpoint_cm       <- (outer_sensor_cm + inner_sensor_cm) / 2
  probe_tip_cm      <- (probe_length_mm - install_offset_mm) / 10

  # Active sensors = those in sensor_positions whose depth is within sapwood
  all_sensor_depths <- c(outer = outer_sensor_cm, inner = inner_sensor_cm)
  requested_depths  <- all_sensor_depths[names(all_sensor_depths) %in% sensor_positions]
  active_sensors    <- names(requested_depths)[requested_depths < actual_sapwood_cm]

  if (outer_sensor_cm >= actual_sapwood_cm) {
    stop(sprintf(
      "Outer sensor depth (%.2f cm from cambium) is at or beyond the sapwood (%.2f cm). ",
      outer_sensor_cm, actual_sapwood_cm
    ),
    "Check sapwood_thickness and bark_thickness.")
  }

  # ── Build annuli from probe landmarks ────────────────────────────────────────
  # Three zones are defined by the midpoint and inner detection limit.
  # HRM detection radius = 0.5 cm: the inner sensor reliably measures flux
  # within ±0.5 cm of its position, so the measured zone ends at inner + 0.5 cm.
  # The probe tip overrides this only when it falls closer than 0.5 cm past the
  # inner sensor (i.e. the needle ends before the detection limit is reached).
  inner_det_lim_cm <- min(inner_sensor_cm + 0.5, probe_tip_cm)

  zones <- list()

  # Zone 1 — outer sensor zone: cambium to min(midpoint, heartwood)
  zone1_end <- min(midpoint_cm, actual_sapwood_cm)
  zones[[1]] <- list(
    d_start       = 0,
    d_end         = zone1_end,
    sensor        = "outer",
    sensor_source = "outer",
    measured      = TRUE,
    ring_name     = "outer_ring"
  )

  # Zone 2 — inner sensor zone: midpoint to min(inner_det_lim, heartwood)
  if (actual_sapwood_cm > midpoint_cm) {
    zone2_end      <- min(inner_det_lim_cm, actual_sapwood_cm)
    inner_measured <- "inner" %in% active_sensors
    zones[[length(zones) + 1]] <- list(
      d_start       = midpoint_cm,
      d_end         = zone2_end,
      sensor        = if (inner_measured) "inner" else "sensorless",
      sensor_source = if (inner_measured) "inner" else "outer",
      measured      = inner_measured,
      ring_name     = if (inner_measured) "inner_ring" else "inner_ring_estimated"
    )
  }

  # Zone 3 — beyond detection limit: inner_det_lim to heartwood
  if (actual_sapwood_cm > inner_det_lim_cm) {
    zones[[length(zones) + 1]] <- list(
      d_start       = inner_det_lim_cm,
      d_end         = actual_sapwood_cm,
      sensor        = "sensorless",
      sensor_source = "inner",
      measured      = FALSE,
      ring_name     = "beyond_probe_ring"
    )
  }

  # ── Convert zones to data frame with radii and areas ────────────────────────
  n <- length(zones)
  rings <- data.frame(
    sensor                = character(n),
    sensor_source         = character(n),
    measured              = logical(n),
    ring_name             = character(n),
    inner_radius_cm       = numeric(n),
    outer_radius_cm       = numeric(n),
    area_cm2              = numeric(n),
    depth_from_cambium_cm = character(n),
    stringsAsFactors      = FALSE
  )

  for (k in seq_len(n)) {
    z     <- zones[[k]]
    r_out <- cambium_radius_cm - z$d_start
    r_in  <- cambium_radius_cm - z$d_end
    rings[k, ] <- list(
      sensor                = z$sensor,
      sensor_source         = z$sensor_source,
      measured              = z$measured,
      ring_name             = z$ring_name,
      inner_radius_cm       = r_in,
      outer_radius_cm       = r_out,
      area_cm2              = pi * (r_out^2 - r_in^2),
      depth_from_cambium_cm = sprintf("%.3f-%.3f", z$d_start, z$d_end)
    )
  }

  # ── Return ───────────────────────────────────────────────────────────────────
  list(
    total_sapwood_area_cm2 = total_sapwood_area_cm2,
    rings                  = rings,
    tree_dimensions        = list(
      dbh_cm                  = dbh,
      bark_thickness_dbh_cm   = bark_thickness_dbh,
      bark_thickness_probe_cm = bark_thickness_probe,
      spacer_thickness_cm     = spacer_thickness_mm / 10,
      stem_radius_cm          = stem_radius_cm,
      cambium_radius_cm       = cambium_radius_cm,
      sapwood_thickness_cm    = sapwood_thickness,
      actual_sapwood_cm       = actual_sapwood_cm,
      heartwood_radius_cm     = heartwood_radius_cm
    ),
    probe_landmarks        = list(
      outer_sensor_depth_cm      = outer_sensor_cm,
      inner_sensor_depth_cm      = inner_sensor_cm,
      midpoint_depth_cm          = midpoint_cm,
      inner_det_lim_depth_cm     = inner_det_lim_cm,
      probe_tip_depth_cm         = probe_tip_cm,
      active_sensors             = active_sensors
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
#'     \item{Jv_cm3_cm2_hr}{Sap flux density (cm^3/cm^2/hr)}
#'   }
#' @param sapwood_areas Output from \code{\link{calc_sapwood_areas}}
#' @param method Integration method. Options: "weighted_average" (default,
#'   Hatton 1990) or "simple" (uniform weighting)
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Q_cm3_hr}{Total sap flux (cm^3/hr) per timestamp}
#'     \item{Q_L_hr}{Total sap flux (L/hr) per timestamp}
#'     \item{Q_L_day}{Total sap flux (L/day) per timestamp}
#'   }
#'
#' @details
#' **Integration Formula (Hatton et al. 1990):**
#'
#' \deqn{Q = \sum_{k} A_k \cdot J_{v,k}}
#'
#' Where \eqn{A_k} is the cross-sectional area of ring \eqn{k} (cm^2) and
#' \eqn{J_{v,k}} is the sap flux density for that ring (cm^3/cm^2/hr).
#'
#' **Flux assignment per annulus:**
#'
#' Annulus boundaries and sensor assignments are determined entirely by
#' \code{\link{calc_sapwood_areas}}. Each annulus in the \code{rings} data frame
#' carries a \code{sensor_source} (which sensor's Jv to use) and a \code{measured}
#' flag. For sensorless annuli (\code{measured = FALSE}), Jv is estimated as half
#' the adjacent sensor's value, assuming a linear velocity decrease with depth.
#'
#' @examples
#' \dontrun{
#' # Calculate sapwood areas (sapwood_depth is from bark surface)
#' areas <- calc_sapwood_areas(dbh = 30, bark_thickness = 0.5, sapwood_depth = 3.0)
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
#' @seealso \code{\link{calc_sapwood_areas}}
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

  rings <- sapwood_areas$rings

  # Detect additional grouping columns (method, method_label, pulse_id)
  # These need to be preserved so each method gets its own integration
  potential_group_cols <- c("method", "method_label", "pulse_id")
  group_cols <- c("datetime", intersect(potential_group_cols, names(flux_data)))

  # Process each unique combination of grouping columns
  results <- flux_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      Q_cm3_hr = calc_flux_single_timestamp(
        sensor_positions = sensor_position,
        Jv_values        = Jv_cm3_cm2_hr,
        rings            = rings,
        method           = method
      ),
      .groups = "drop"
    )

  # Add L/hr and L/day
  results$Q_L_hr <- results$Q_cm3_hr / 1000
  results$Q_L_day <- results$Q_L_hr * 24

  # Merge back with original data using all grouping columns
  flux_data <- dplyr::left_join(flux_data, results, by = group_cols)

  return(flux_data)
}


#' Calculate Flux for a Single Timestamp (Internal Helper)
#'
#' @param sensor_positions Vector of sensor positions at this timestamp.
#' @param Jv_values Vector of Jv values corresponding to each sensor.
#' @param rings Sapwood rings data frame (output from \code{calc_sapwood_areas}).
#'   Must contain \code{sensor_source} (character) and \code{measured} (logical)
#'   columns.
#' @param method Integration method (currently unused; reserved for future methods).
#'
#' @return Total flux (cm^3/hr) for this timestamp.
#'
#' @details
#' For each annulus, the flux contribution is:
#' \itemize{
#'   \item \code{area * Jv_source} when \code{measured = TRUE}
#'   \item \code{area * (Jv_source / 2)} when \code{measured = FALSE} (sensorless
#'     annulus — assumes a linear velocity decrease to half the nearest sensor value)
#' }
#' The source sensor for each annulus is encoded in the \code{sensor_source} column
#' of the rings data frame, set by \code{\link{calc_sapwood_areas}}.
#'
#' @keywords internal
calc_flux_single_timestamp <- function(sensor_positions,
                                        Jv_values,
                                        rings,
                                        method) {

  # Build a named Jv lookup from the sensor data at this timestamp
  Jv_outer <- Jv_values[sensor_positions == "outer"][1]
  Jv_inner <- Jv_values[sensor_positions == "inner"][1]
  if (is.na(Jv_outer)) Jv_outer <- 0
  if (is.na(Jv_inner)) Jv_inner <- 0

  Jv_lookup <- c(outer = Jv_outer, inner = Jv_inner)

  # Integrate over each annulus
  Q_total <- 0
  for (i in seq_len(nrow(rings))) {
    ring      <- rings[i, ]
    Jv_source <- Jv_lookup[ring$sensor_source]
    # Sensorless rings use half the adjacent sensor's value (linear decrease)
    Jv_ring   <- if (isTRUE(ring$measured)) Jv_source else Jv_source / 2
    Q_total   <- Q_total + ring$area_cm2 * Jv_ring
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
#' @param sapwood_thickness_col Name of sapwood thickness column (cm, cambium to
#'   heartwood). Default: "sapwood_thickness"
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
#' flux_data$sapwood_thickness <- 2.5
#' flux_data$bark_thickness_dbh <- 0.5
#' flux_data$bark_thickness_probe <- 0.5
#'
#' result <- apply_sap_flux_integration(flux_data)
#' }
#'
#' @family sapwood integration functions
#' @export
apply_sap_flux_integration <- function(flux_data,
                                        dbh_col = "dbh",
                                        sapwood_thickness_col = "sapwood_thickness",
                                        bark_thickness_dbh_col = "bark_thickness_dbh",
                                        bark_thickness_probe_col = "bark_thickness_probe",
                                        method = "weighted_average") {

  # Validate columns exist
  if (!dbh_col %in% names(flux_data)) {
    stop("Column '", dbh_col, "' not found in flux_data")
  }

  if (!sapwood_thickness_col %in% names(flux_data)) {
    stop("Column '", sapwood_thickness_col, "' not found in flux_data")
  }

  if (!bark_thickness_dbh_col %in% names(flux_data)) {
    stop("Column '", bark_thickness_dbh_col, "' not found in flux_data. ",
         "Provide 'bark_thickness_dbh' (full bark at DBH site) and ",
         "'bark_thickness_probe' (probe-site bark after shaving) as separate columns.")
  }

  if (!bark_thickness_probe_col %in% names(flux_data)) {
    stop("Column '", bark_thickness_probe_col, "' not found in flux_data. ",
         "Provide 'bark_thickness_probe' (probe-site bark after shaving).")
  }

  # Get tree dimensions (assume constant per tree)
  dbh                  <- unique(flux_data[[dbh_col]])[1]
  sapwood_thickness    <- unique(flux_data[[sapwood_thickness_col]])[1]
  bark_thickness_dbh   <- unique(flux_data[[bark_thickness_dbh_col]])[1]
  bark_thickness_probe <- unique(flux_data[[bark_thickness_probe_col]])[1]

  # Calculate sapwood areas
  sapwood_areas <- calc_sapwood_areas(
    dbh                  = dbh,
    bark_thickness_dbh   = bark_thickness_dbh,
    bark_thickness_probe = bark_thickness_probe,
    sapwood_thickness    = sapwood_thickness
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
  cat(sprintf("  Bark thickness (DBH site): %.1f cm\n", bark_thickness_dbh))
  cat(sprintf("  Bark thickness (probe site): %.1f cm\n", bark_thickness_probe))
  cat(sprintf("  Sapwood thickness: %.1f cm\n", sapwood_thickness))
  cat(sprintf("  Total sapwood area: %.1f cm^2\n",
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
