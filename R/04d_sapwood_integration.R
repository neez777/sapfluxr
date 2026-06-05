# R/04d_sapwood_integration.R
# Sapwood Area Integration and Sap Flux Calculation
# Area-weighted summation after Hatton et al. (1990); radial assumption for
# sensorless annuli selectable as linear decay (Pausch et al. 2000) or
# constant velocity (nearest-neighbour).

#' Calculate Sapwood Area for Concentric Rings
#'
#' Calculates the cross-sectional area of sapwood rings (annuli) based on tree
#' dimensions, probe geometry, and sensor positions, following the area-weighted
#' integration of Hatton et al. (1990) with probe-derived annulus boundaries.
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
#'       outer and inner sensor positions, their midpoint, the outer and inner
#'       detection limits (sensor + 0.5 cm), the probe tip, and which sensors are
#'       active.}
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
#' Each HRM needle represents flux reliably only within its \strong{detection
#' radius} (\eqn{\pm}0.5 cm of the sensor). This gives an \emph{outer} and an
#' \emph{inner} detection limit, each 0.5 cm radially inward of its sensor. How
#' the annuli are bounded depends on whether the inner sensor falls within the
#' sapwood:
#' \describe{
#'   \item{Inner sensor within sapwood (both sensors live)}{The midpoint between
#'     the sensors divides their zones of influence. Outer annulus = cambium to
#'     midpoint; inner annulus = midpoint to the inner detection limit; any
#'     sapwood beyond the inner detection limit is a sensorless annulus estimated
#'     by decay from the inner sensor.}
#'   \item{Inner sensor in heartwood (only the outer sensor is live)}{The
#'     midpoint is meaningless — there is no second measurement to share with.
#'     The outer annulus is bounded by the \emph{outer detection limit}
#'     (outer sensor + 0.5 cm); the remaining sapwood from there to the heartwood
#'     is a sensorless annulus estimated by decay from the outer sensor.
#'     Bounding the measured ring at the midpoint here would over-credit it and
#'     inflate whole-tree water use.}
#' }
#'
#' \strong{Annulus allocation} (example: standard ICT probe, bark_probe = 0.5 cm, spacer = 0)
#'
#' Probe landmarks from cambium: outer sensor 0.75 cm, outer detection limit
#' 1.25 cm, midpoint 1.50 cm, inner sensor 2.25 cm, inner detection limit
#' 2.75 cm.
#'
#' \describe{
#'   \item{Actual sapwood < 0.75 cm}{Error — outer sensor is not within the sapwood.}
#'   \item{Actual sapwood 0.75-1.25 cm}{1 annulus: outer sensor measures the full
#'     sapwood depth (0 to sapwood boundary).}
#'   \item{Actual sapwood 1.25 cm to just under 2.25 cm}{2 annuli: outer sensor
#'     (0 to 1.25 cm); sensorless zone (1.25 cm to sapwood boundary, estimated as
#'     Jv_outer / 2). The inner sensor is in heartwood, so the outer detection
#'     limit bounds the measured ring.}
#'   \item{Actual sapwood 2.25-2.75 cm}{2 annuli: outer sensor (0 to 1.50 cm);
#'     inner sensor (1.50 cm to sapwood boundary). The inner sensor at exactly
#'     2.25 cm (sapwood = inner-sensor depth) counts as measuring (inclusive
#'     boundary), so the midpoint divides the annuli.}
#'   \item{Actual sapwood over 2.75 cm}{3 annuli: outer sensor (0 to 1.50 cm);
#'     inner sensor (1.50 to 2.75 cm); sensorless zone (2.75 cm to sapwood
#'     boundary, estimated as Jv_inner / 2).}
#' }
#'
#' ## Sensorless annuli
#'
#' When an annulus has no direct sensor measurement (\code{measured = FALSE}),
#' \code{\link{calc_sap_flux}} estimates its flux density from the nearest
#' measured sensor (\code{sensor_source}) using one of two integration methods:
#' \describe{
#'   \item{\code{"linear_decay"}}{Mean flux over the sensorless annulus is
#'     half the adjacent sensor value, assuming a linear decline from the
#'     sensor depth to zero at the heartwood (Pausch et al. 2000).}
#'   \item{\code{"constant_velocity"}}{Flux over the sensorless annulus equals
#'     the adjacent sensor value (nearest-neighbour assumption).}
#' }
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

  # Active sensors = those in sensor_positions whose depth is within sapwood.
  # A sensor sitting exactly on the sapwood/heartwood boundary counts as within
  # the sapwood (inclusive, <=): per Tim's convention it still represents a
  # measured ring, so the midpoint (not the outer detection limit) divides the
  # annuli. Only a sensor strictly beyond the boundary is "in heartwood".
  all_sensor_depths <- c(outer = outer_sensor_cm, inner = inner_sensor_cm)
  requested_depths  <- all_sensor_depths[names(all_sensor_depths) %in% sensor_positions]
  active_sensors    <- names(requested_depths)[requested_depths <= actual_sapwood_cm]

  if (outer_sensor_cm >= actual_sapwood_cm) {
    stop(sprintf(
      "Outer sensor depth (%.2f cm from cambium) is at or beyond the sapwood (%.2f cm). ",
      outer_sensor_cm, actual_sapwood_cm
    ),
    "Check sapwood_thickness and bark_thickness.")
  }

  # ── Build annuli from probe landmarks ────────────────────────────────────────
  # Each HRM needle reliably represents flux only within its detection radius
  # (±0.5 cm of the sensor position). This defines an OUTER and an INNER
  # detection limit, each 0.5 cm radially inward of its sensor. The inner limit
  # is capped at the probe tip — the needle cannot sense past its own end.
  detection_radius_cm <- 0.5
  outer_det_lim_cm <- outer_sensor_cm + detection_radius_cm
  inner_det_lim_cm <- min(inner_sensor_cm + detection_radius_cm, probe_tip_cm)

  inner_active <- "inner" %in% active_sensors
  zones <- list()

  if (inner_active) {
    # ── Two live sensors ──────────────────────────────────────────────────────
    # The midpoint between the sensors divides their zones of influence: the
    # outer sensor represents cambium → midpoint, the inner sensor represents
    # midpoint → its detection limit, and any sapwood beyond the inner detection
    # limit is estimated by radial decay from the inner measurement.

    # Zone 1 — outer sensor: cambium to midpoint
    zones[[1]] <- list(
      d_start       = 0,
      d_end         = midpoint_cm,
      sensor        = "outer",
      sensor_source = "outer",
      measured      = TRUE,
      ring_name     = "outer_ring"
    )

    # Zone 2 — inner sensor: midpoint to min(inner detection limit, heartwood)
    zones[[length(zones) + 1]] <- list(
      d_start       = midpoint_cm,
      d_end         = min(inner_det_lim_cm, actual_sapwood_cm),
      sensor        = "inner",
      sensor_source = "inner",
      measured      = TRUE,
      ring_name     = "inner_ring"
    )

    # Zone 3 — beyond inner detection limit: estimated from the inner sensor
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
  } else {
    # ── Only the outer sensor is live (inner sensor embedded in heartwood) ─────
    # With no valid inner measurement the midpoint has no meaning: there is no
    # second sensor to share influence with. The outer sensor reliably
    # represents flux only out to its own detection limit; the remaining sapwood
    # from there to the heartwood is unmeasured and is estimated by radial decay
    # from the outer measurement. Bounding the measured ring at the midpoint
    # instead would over-credit it and inflate whole-tree water use.

    # Zone 1 — outer sensor: cambium to min(outer detection limit, heartwood)
    zones[[1]] <- list(
      d_start       = 0,
      d_end         = min(outer_det_lim_cm, actual_sapwood_cm),
      sensor        = "outer",
      sensor_source = "outer",
      measured      = TRUE,
      ring_name     = "outer_ring"
    )

    # Zone 2 — outer detection limit to heartwood: estimated from outer sensor
    if (actual_sapwood_cm > outer_det_lim_cm) {
      zones[[length(zones) + 1]] <- list(
        d_start       = outer_det_lim_cm,
        d_end         = actual_sapwood_cm,
        sensor        = "sensorless",
        sensor_source = "outer",
        measured      = FALSE,
        ring_name     = "inner_ring_estimated"
      )
    }
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
      outer_det_lim_depth_cm     = outer_det_lim_cm,
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
#' Follows the area-weighted summation of Hatton et al. (1990); the radial
#' assumption applied to unmeasured (sensorless) annuli is controlled by
#' \code{method}.
#'
#' @param flux_data Data frame with sap flux density measurements. Must contain:
#'   \describe{
#'     \item{datetime}{Timestamp}
#'     \item{sensor_position}{Sensor position ("outer", "inner")}
#'     \item{Jv_cm3_cm2_hr}{Sap flux density (cm^3/cm^2/hr)}
#'   }
#' @param sapwood_areas Output from \code{\link{calc_sapwood_areas}}
#' @param method Radial integration method for sensorless annuli. Options:
#'   \describe{
#'     \item{\code{"linear_decay"}}{(default) Pausch et al. (2000) — assumes
#'       sap flux declines linearly from the adjacent sensor value to zero
#'       across the unmeasured annulus, giving a mean of \eqn{J_v / 2}.}
#'     \item{\code{"constant_velocity"}}{Nearest-neighbour — the adjacent
#'       sensor value is applied unchanged across the unmeasured annulus.}
#'   }
#'
#' @return Data frame with \strong{one row per unique timestamp} (and per
#'   \code{method}/\code{method_label}/\code{pulse_id} group if present).
#'   The input \code{sensor_position} and \code{Jv_cm3_cm2_hr} columns are
#'   collapsed into the following radial component columns:
#'   \describe{
#'     \item{Q_outer_cm3_hr}{Flux from directly-measured outer-sensor annuli (cm^3/hr)}
#'     \item{Q_inner_cm3_hr}{Flux from directly-measured inner-sensor annuli (cm^3/hr)}
#'     \item{Q_unmeasured_cm3_hr}{Flux from sensorless annuli, estimated via \code{method} (cm^3/hr)}
#'     \item{Q_total_cm3_hr}{Sum of all three components (cm^3/hr)}
#'     \item{Q_total_L_hr}{Total sap flux (L/hr)}
#'     \item{Q_total_L_day}{Instantaneous total expressed as L/day (= Q_total_L_hr × 24).
#'       This is \emph{not} a true daily integral; use \code{\link{aggregate_daily_flux}}
#'       to integrate correctly over a measurement period.}
#'   }
#'
#'   \strong{Note:} because the output is one row per timestamp, individual
#'   sensor Jv values are no longer present. To retain tree-level metadata
#'   columns (e.g. \code{tree_id}, \code{dbh}), join the result back via
#'   \code{dplyr::left_join(q_result, flux_data \%>\% dplyr::distinct(datetime, tree_id, dbh))}.
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
#' flag. For sensorless annuli (\code{measured = FALSE}), the adjacent sensor's
#' Jv is applied according to \code{method}: halved under \code{"linear_decay"}
#' (Pausch et al. 2000), or used unchanged under \code{"constant_velocity"}.
#'
#' @examples
#' \dontrun{
#' areas <- calc_sapwood_areas(
#'   dbh = 30, bark_thickness_dbh = 0.5,
#'   bark_thickness_probe = 0.5, sapwood_thickness = 3.5
#' )
#'
#' flux_data <- data.frame(
#'   datetime        = rep(as.POSIXct("2024-01-01 12:00", tz = "UTC"), 2),
#'   sensor_position = c("outer", "inner"),
#'   Jv_cm3_cm2_hr   = c(10, 7)
#' )
#'
#' q <- calc_sap_flux(flux_data, areas)
#' # One row per timestamp; Q broken into radial components
#' q[, c("datetime", "Q_outer_cm3_hr", "Q_inner_cm3_hr",
#'        "Q_unmeasured_cm3_hr", "Q_total_L_hr")]
#'
#' # True daily totals (accounts for measurement interval)
#' daily <- aggregate_daily_flux(q)
#' }
#'
#' @references
#' Hatton, T.J., Catchpole, E.A., & Vertessy, R.A. (1990). Integration of
#' sapflow velocity to estimate plant water use. Tree Physiology, 6, 201-209.
#'
#' Pausch, R.C., Grote, E.E., & Dawson, T.E. (2000). Estimating water use by
#' sugar maple trees: considerations when using heat-pulse methods in trees
#' with deep functional sapwood. Tree Physiology, 20, 217-227.
#'
#' @seealso \code{\link{calc_sapwood_areas}}, \code{\link{aggregate_daily_flux}}
#'
#' @family sapwood integration functions
#' @export
calc_sap_flux <- function(flux_data,
                          sapwood_areas,
                          method = c("linear_decay", "constant_velocity")) {

  # Rename to avoid dplyr data-mask collision: flux_data may have a "method"
  # column (HRM/MHR label) that would shadow this argument inside summarise().
  .radial_method <- match.arg(method)

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

  # Grouping cols: datetime + any of method/method_label/pulse_id present
  potential_group_cols <- c("method", "method_label", "pulse_id")
  group_cols <- c("datetime", intersect(potential_group_cols, names(flux_data)))

  # One row per timestamp (× group_cols): call helper and unpack components
  results <- flux_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::reframe(
      calc_flux_single_timestamp(
        sensor_positions = sensor_position,
        Jv_values        = Jv_cm3_cm2_hr,
        rings            = rings,
        method           = .radial_method
      )
    )

  results$Q_total_L_hr  <- results$Q_total_cm3_hr / 1000
  results$Q_total_L_day <- results$Q_total_L_hr * 24

  return(results)
}


#' Calculate Flux Components for a Single Timestamp (Internal Helper)
#'
#' @param sensor_positions Vector of sensor positions at this timestamp.
#' @param Jv_values Vector of Jv values corresponding to each sensor.
#' @param rings Sapwood rings data frame (output from \code{calc_sapwood_areas}).
#'   Must contain \code{sensor_source} (character) and \code{measured} (logical)
#'   columns.
#' @param method Radial integration method for sensorless annuli:
#'   \code{"linear_decay"} (Pausch et al. 2000) halves the adjacent sensor's
#'   Jv; \code{"constant_velocity"} uses it unchanged.
#'
#' @return Single-row tibble with columns \code{Q_outer_cm3_hr},
#'   \code{Q_inner_cm3_hr}, \code{Q_unmeasured_cm3_hr}, \code{Q_total_cm3_hr}.
#'
#' @keywords internal
calc_flux_single_timestamp <- function(sensor_positions,
                                        Jv_values,
                                        rings,
                                        method) {

  Jv_outer <- Jv_values[sensor_positions == "outer"][1]
  Jv_inner <- Jv_values[sensor_positions == "inner"][1]
  if (is.na(Jv_outer)) Jv_outer <- 0
  if (is.na(Jv_inner)) Jv_inner <- 0

  Jv_lookup <- c(outer = Jv_outer, inner = Jv_inner)

  sensorless_factor <- switch(method,
                              linear_decay      = 0.5,
                              constant_velocity = 1.0,
                              stop("Unknown integration method: ", method))

  Q_outer      <- 0
  Q_inner      <- 0
  Q_unmeasured <- 0

  for (i in seq_len(nrow(rings))) {
    ring      <- rings[i, ]
    Jv_source <- Jv_lookup[ring$sensor_source]
    contrib   <- ring$area_cm2 * if (isTRUE(ring$measured)) {
      Jv_source
    } else {
      Jv_source * sensorless_factor
    }

    if (!isTRUE(ring$measured)) {
      Q_unmeasured <- Q_unmeasured + contrib
    } else if (ring$sensor_source == "outer") {
      Q_outer <- Q_outer + contrib
    } else {
      Q_inner <- Q_inner + contrib
    }
  }

  # unname() strips the "outer"/"inner" attribute inherited from Jv_lookup subsetting
  tibble::tibble(
    Q_outer_cm3_hr      = unname(Q_outer),
    Q_inner_cm3_hr      = unname(Q_inner),
    Q_unmeasured_cm3_hr = unname(Q_unmeasured),
    Q_total_cm3_hr      = unname(Q_outer + Q_inner + Q_unmeasured)
  )
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
#' @param method Radial integration method for sensorless annuli. One of
#'   \code{"linear_decay"} (default, Pausch et al. 2000) or
#'   \code{"constant_velocity"}. See \code{\link{calc_sap_flux}}.
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
                                        method = c("linear_decay", "constant_velocity")) {

  method <- match.arg(method)

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
              mean(result$Q_total_L_hr, na.rm = TRUE),
              mean(result$Q_total_L_day, na.rm = TRUE)))
  cat(sprintf("  Max flux: %.2f L/hr (%.2f L/day)\n",
              max(result$Q_total_L_hr, na.rm = TRUE),
              max(result$Q_total_L_day, na.rm = TRUE)))
  cat("\n")

  cat(strrep("=", 70), "\n\n")

  return(result)
}


#' Aggregate Per-Timestamp Sap Flux to Daily Totals
#'
#' Converts the per-timestamp output of \code{\link{calc_sap_flux}} to true
#' daily integrals by multiplying each instantaneous rate by the measurement
#' interval. This is the correct way to obtain daily water use in L/day;
#' the \code{Q_total_L_day} column in \code{calc_sap_flux} output is
#' merely the instantaneous rate scaled to 24 h and should not be summed.
#'
#' @param q_data Data frame — output from \code{\link{calc_sap_flux}} or
#'   \code{\link{apply_sap_flux_integration}}. Must contain a POSIXct
#'   \code{datetime} column and at least \code{Q_total_L_hr}.
#' @param datetime_col Character. Name of the datetime column. Default: \code{"datetime"}.
#' @param interval_hours Numeric or \code{NULL}. Measurement interval in hours.
#'   If \code{NULL} (default), the interval is auto-detected from the data.
#' @param group_cols Character vector or \code{NULL}. Additional columns to
#'   group by (e.g. \code{"method_label"}, \code{"method"}). If \code{NULL},
#'   all of \code{"method"}, \code{"method_label"}, \code{"pulse_id"} present
#'   in the data are used automatically.
#' @param min_completeness Numeric in \code{[0, 1]} or \code{NULL}. If set, days with
#'   fewer than this fraction of expected measurements will have their flux
#'   columns replaced with \code{NA}. Default: \code{NULL} (no filter).
#'
#' @return Data frame with one row per date (× group_cols) containing:
#'   \describe{
#'     \item{date}{Date (Date class)}
#'     \item{Q_outer_L_day}{Daily total from outer-sensor annuli (L/day)}
#'     \item{Q_inner_L_day}{Daily total from inner-sensor annuli (L/day)}
#'     \item{Q_unmeasured_L_day}{Daily total from sensorless annuli (L/day)}
#'     \item{Q_total_L_day}{True daily total water use (L/day)}
#'     \item{n_measurements}{Number of sub-daily records in this day}
#'     \item{data_completeness}{Fraction of expected measurements present (0-1)}
#'   }
#'
#' @examples
#' \dontrun{
#' areas <- calc_sapwood_areas(
#'   dbh = 30, bark_thickness_dbh = 0.5,
#'   bark_thickness_probe = 0.5, sapwood_thickness = 3.5
#' )
#' q <- calc_sap_flux(flux_data, areas)
#' daily <- aggregate_daily_flux(q)
#' }
#'
#' @seealso \code{\link{calc_sap_flux}}, \code{\link{aggregate_daily}}
#' @family sapwood integration functions
#' @export
aggregate_daily_flux <- function(q_data,
                                  datetime_col    = "datetime",
                                  interval_hours  = NULL,
                                  group_cols      = NULL,
                                  min_completeness = NULL) {

  if (!is.data.frame(q_data)) stop("q_data must be a data frame")
  if (!datetime_col %in% names(q_data)) {
    stop("Column '", datetime_col, "' not found in q_data")
  }
  if (!inherits(q_data[[datetime_col]], "POSIXct")) {
    stop("Column '", datetime_col, "' must be POSIXct")
  }

  required_q_cols <- c("Q_total_L_hr")
  missing_q <- setdiff(required_q_cols, names(q_data))
  if (length(missing_q) > 0) {
    stop("q_data missing required columns: ", paste(missing_q, collapse = ", "),
         "\nProvide output from calc_sap_flux().")
  }

  # Auto-detect grouping columns
  if (is.null(group_cols)) {
    potential <- c("method", "method_label", "pulse_id")
    group_cols <- intersect(potential, names(q_data))
  }

  # Detect interval
  if (is.null(interval_hours)) {
    interval_hours <- detect_interval(q_data[[datetime_col]])
  }
  expected_per_day <- round(24 / interval_hours)

  q_data$date <- as.Date(q_data[[datetime_col]],
                          tz = attr(q_data[[datetime_col]], "tzone") %||% "UTC")

  all_group_cols <- c("date", group_cols)

  component_cols <- intersect(
    c("Q_outer_L_hr", "Q_inner_L_hr", "Q_unmeasured_L_hr", "Q_total_L_hr"),
    names(q_data)
  )

  # If cm3 components present but L components absent, derive them
  for (suffix in c("outer", "inner", "unmeasured", "total")) {
    col_L   <- paste0("Q_", suffix, "_L_hr")
    col_cm3 <- paste0("Q_", suffix, "_cm3_hr")
    if (!col_L %in% names(q_data) && col_cm3 %in% names(q_data)) {
      q_data[[col_L]] <- q_data[[col_cm3]] / 1000
      component_cols <- c(component_cols, col_L)
    }
  }
  component_cols <- unique(component_cols)

  result <- q_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_group_cols))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(component_cols),
        ~ sum(.x, na.rm = TRUE) * interval_hours,
        .names = "{.col}"
      ),
      n_measurements   = dplyr::n(),
      data_completeness = dplyr::n() / expected_per_day,
      .groups = "drop"
    )

  # Rename *_L_hr → *_L_day
  for (col in component_cols) {
    new_col <- sub("_L_hr$", "_L_day", col)
    if (new_col != col) {
      names(result)[names(result) == col] <- new_col
    }
  }

  # Apply completeness filter
  if (!is.null(min_completeness)) {
    day_cols <- grep("_L_day$", names(result), value = TRUE)
    mask <- result$data_completeness < min_completeness
    result[mask, day_cols] <- NA_real_
  }

  return(result)
}
