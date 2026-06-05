# tests/testthat/test-sapwood-integration.R
# Tests for Sapwood Area Integration and Sap Flux Calculation
#
# Standard probe geometry (ICT SFM1x, no spacer, bark_probe = bark_dbh = 0.5 cm):
#   install_offset = 0.5*10 + 0 = 5 mm
#   outer_sensor  = (35 - 22.5 - 5) / 10 = 0.75 cm from cambium
#   inner_sensor  = (35 - 7.5  - 5) / 10 = 2.25 cm from cambium
#   midpoint      = (0.75 + 2.25) / 2    = 1.50 cm
#   probe_tip     = (35 - 5) / 10        = 3.00 cm

# =============================================================================
# Test calc_sapwood_areas() - Ring Allocation
# =============================================================================

test_that("calc_sapwood_areas allocates single outer ring for shallow sapwood", {
  # sapwood_thickness = 1.2 cm  (<= outer detection limit 1.25 = outer_sensor 0.75 + 0.5)
  areas <- calc_sapwood_areas(
    dbh                  = 20,
    bark_thickness_dbh   = 0.5,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 1.2
  )

  expect_equal(nrow(areas$rings), 1)
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$ring_name[1], "outer_ring")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.000-1.200")
})


test_that("calc_sapwood_areas allocates outer + sensorless inner for medium sapwood", {
  # sapwood_thickness = 2.0 cm. Inner sensor (2.25 cm) is embedded in heartwood,
  # so the measured outer ring is bounded by the OUTER detection limit
  # (0.75 + 0.5 = 1.25 cm), not the midpoint; the remainder is estimated from
  # the outer sensor.
  areas <- calc_sapwood_areas(
    dbh                  = 25,
    bark_thickness_dbh   = 0.5,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 2.0
  )

  expect_equal(nrow(areas$rings), 2)
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$ring_name[1], "outer_ring")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.000-1.250")

  # Inner ring is sensorless (inner sensor at 2.25 beyond sapwood at 2.0),
  # estimated from the outer sensor, spanning the outer detection limit to heartwood.
  expect_equal(areas$rings$sensor[2], "sensorless")
  expect_equal(areas$rings$sensor_source[2], "outer")
  expect_equal(areas$rings$ring_name[2], "inner_ring_estimated")
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.250-2.000")
})


test_that("calc_sapwood_areas matches Tim's spreadsheet when inner sensor is in heartwood", {
  # Reference tree (Eucalyptus marginata, SX01O201) from Tim's spreadsheet:
  #   dbh 52.08, bark_dbh 1.20, bark_probe 0.50, sapwood 2.0 cm
  #   cambium_radius = 26.04 - 1.20 = 24.84; heartwood_radius = 22.84
  #   outer sensor 0.75 cm, outer detection limit 1.25 cm; inner sensor 2.25 cm (heartwood)
  areas <- calc_sapwood_areas(
    dbh                  = 52.08,
    bark_thickness_dbh   = 1.20,
    bark_thickness_probe = 0.50,
    sapwood_thickness    = 2.0
  )

  expect_equal(areas$total_sapwood_area_cm2, 299.58, tolerance = 0.01)
  expect_equal(areas$probe_landmarks$outer_det_lim_depth_cm, 1.25, tolerance = 0.001)

  expect_equal(nrow(areas$rings), 2)
  # Outer measured ring bounded by the outer detection limit (Tim CSA_OUTER = 190.18)
  expect_equal(areas$rings$measured[1], TRUE)
  expect_equal(areas$rings$area_cm2[1], 190.18, tolerance = 0.02)
  # Inner estimated ring spans outer detection limit to heartwood (Tim CSA_INNER = 109.40)
  expect_equal(areas$rings$measured[2], FALSE)
  expect_equal(areas$rings$area_cm2[2], 109.40, tolerance = 0.02)
})


test_that("calc_sapwood_areas treats inner sensor exactly at the heartwood boundary as in sapwood", {
  # sapwood_thickness = 2.25 cm == inner_sensor depth. Inclusive convention
  # (Tim's spreadsheet): the inner sensor counts as measuring, so the midpoint
  # (1.50 cm) divides the annuli and the inner ring is measured, not estimated.
  areas <- calc_sapwood_areas(
    dbh                  = 52.08,
    bark_thickness_dbh   = 1.20,
    bark_thickness_probe = 0.50,
    sapwood_thickness    = 2.25
  )

  expect_equal(nrow(areas$rings), 2)
  expect_true("inner" %in% areas$probe_landmarks$active_sensors)

  # Outer ring bounded by the midpoint (Tim: 227.04 cm²), measured by outer
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.000-1.500")
  expect_equal(areas$rings$area_cm2[1], 227.04, tolerance = 0.02)

  # Inner ring midpoint → boundary, measured by inner (Tim: 108.22 cm²)
  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$measured[2], TRUE)
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.500-2.250")
  expect_equal(areas$rings$area_cm2[2], 108.22, tolerance = 0.02)
})


test_that("calc_sapwood_areas allocates outer + measured inner for deeper sapwood", {
  # sapwood_thickness = 2.5 cm  (> inner_sensor 2.25, < inner_det_lim 2.75)
  areas <- calc_sapwood_areas(
    dbh                  = 30,
    bark_thickness_dbh   = 0.5,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 2.5
  )

  expect_equal(nrow(areas$rings), 2)
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.000-1.500")

  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$measured[2], TRUE)
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.500-2.500")
})


test_that("calc_sapwood_areas allocates three rings for sapwood beyond detection limit", {
  # sapwood_thickness = 3.5 cm  (> inner_det_lim 2.75)
  # inner_sensor = 2.25 cm, inner_det_lim = min(2.25+0.5, probe_tip=3.0) = 2.75
  areas <- calc_sapwood_areas(
    dbh                  = 40,
    bark_thickness_dbh   = 0.5,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 3.5
  )

  expect_equal(nrow(areas$rings), 3)
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.000-1.500")
  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.500-2.750")
  expect_equal(areas$rings$sensor[3], "sensorless")
  expect_equal(areas$rings$ring_name[3], "beyond_probe_ring")
  expect_equal(areas$rings$depth_from_cambium_cm[3], "2.750-3.500")
})


# =============================================================================
# Test calc_sapwood_areas() - Area Calculations
# =============================================================================

test_that("calc_sapwood_areas calculates areas correctly", {
  # dbh = 30, bark_dbh = 0.5, sapwood_thickness = 2.5
  # cambium_radius = 15 - 0.5 = 14.5
  # heartwood_radius = 14.5 - 2.5 = 12.0
  areas <- calc_sapwood_areas(
    dbh                  = 30,
    bark_thickness_dbh   = 0.5,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 2.5
  )

  expected_total <- pi * (14.5^2 - 12.0^2)
  expect_equal(areas$total_sapwood_area_cm2, expected_total, tolerance = 0.01)

  ring_sum <- sum(areas$rings$area_cm2)
  expect_equal(ring_sum, expected_total, tolerance = 0.01)

  expect_equal(areas$tree_dimensions$cambium_radius_cm, 14.5)
  expect_equal(areas$tree_dimensions$heartwood_radius_cm, 12.0)
  expect_equal(areas$tree_dimensions$actual_sapwood_cm, 2.5)
})


test_that("calc_sapwood_areas handles zero bark thickness", {
  # No bark: sensor positions shift outward
  # outer_sensor = (35 - 22.5 - 0) / 10 = 1.25, midpoint = 2.0, probe_tip = 3.5
  areas <- calc_sapwood_areas(
    dbh                  = 20,
    bark_thickness_dbh   = 0,
    bark_thickness_probe = 0,
    sapwood_thickness    = 3.0
  )

  expect_equal(areas$tree_dimensions$cambium_radius_cm, areas$tree_dimensions$stem_radius_cm)
  expect_true(areas$total_sapwood_area_cm2 > 0)
  expect_true(all(areas$rings$area_cm2 > 0))
})


test_that("calc_sapwood_areas handles tree with no heartwood", {
  # sapwood_thickness (10 cm) > cambium_radius (4.7 cm) → clamped to pith
  expect_warning(
    areas <- calc_sapwood_areas(
      dbh                  = 10,
      bark_thickness_dbh   = 0.3,
      bark_thickness_probe = 0.3,
      sapwood_thickness    = 10
    ),
    "no heartwood"
  )

  expect_equal(areas$tree_dimensions$heartwood_radius_cm, 0)
  expect_true(areas$total_sapwood_area_cm2 > 0)
})


# =============================================================================
# Test calc_sapwood_areas() - Tim's reference geometry (from sapwood_area.xlsx)
# =============================================================================

test_that("calc_sapwood_areas matches Tim's reference geometry exactly", {
  # ICT SFM1x, DBH = 18.2 cm, full bark = 1.7 cm, shaved bark = 0.5 cm,
  # spacer = 2.5 mm, sapwood_thickness = 3.0 cm (cambium → heartwood)
  # Expected (Tim's spreadsheet):
  #   IB radius     = 9.1 - 1.7   = 7.40 cm
  #   heartwood_r   = 7.4 - 3.0   = 4.40 cm
  #   sapwood_area  = pi*(7.4^2-4.4^2) = 111.21 cm^2
  #   outer sensor  = (35-22.5-7.5)/10 = 0.50 cm from cambium
  #   inner sensor  = (35-7.5-7.5)/10  = 2.00 cm from cambium
  probe <- list(
    probe = list(
      length = 35, outer_sensor = 22.5, inner_sensor = 7.5, spacer_thickness = 2.5
    )
  )

  areas <- calc_sapwood_areas(
    dbh                  = 18.2,
    bark_thickness_dbh   = 1.7,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 3.0,
    probe_config         = probe
  )

  expect_equal(areas$tree_dimensions$cambium_radius_cm, 7.4, tolerance = 0.001)
  expect_equal(areas$tree_dimensions$heartwood_radius_cm, 4.4, tolerance = 0.001)
  expect_equal(areas$tree_dimensions$actual_sapwood_cm, 3.0, tolerance = 0.001)
  expect_equal(areas$total_sapwood_area_cm2, 111.21, tolerance = 0.05)
  expect_equal(areas$probe_landmarks$outer_sensor_depth_cm, 0.5, tolerance = 0.001)
  expect_equal(areas$probe_landmarks$inner_sensor_depth_cm, 2.0, tolerance = 0.001)
})


# =============================================================================
# Test calc_sapwood_areas() - Error Handling
# =============================================================================

test_that("calc_sapwood_areas errors on invalid inputs", {
  expect_error(
    calc_sapwood_areas(dbh = -10, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2),
    "dbh must be a positive number"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, bark_thickness_dbh = -1, bark_thickness_probe = 0, sapwood_thickness = 2),
    "bark_thickness_dbh must be a non-negative"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, bark_thickness_dbh = 0.5, bark_thickness_probe = -1, sapwood_thickness = 2),
    "bark_thickness_probe must be a non-negative"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.8, sapwood_thickness = 2),
    "cannot exceed bark_thickness_dbh"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 0),
    "sapwood_thickness must be a positive"
  )
})


test_that("calc_sapwood_areas warns when sapwood thickness exceeds cambium radius", {
  # cambium_radius = 5 - 1 = 4 cm; sapwood_thickness = 5 > 4
  expect_warning(
    calc_sapwood_areas(dbh = 10, bark_thickness_dbh = 1, bark_thickness_probe = 0.5, sapwood_thickness = 5),
    "exceeds cambium radius"
  )
})


# =============================================================================
# Test calc_sap_flux() - Integration with Different Sapwood Depths
# =============================================================================

test_that("calc_sap_flux integrates correctly for shallow sapwood", {
  # 1 ring (outer only): sapwood_thickness = 1.2 cm <= outer detection limit 1.25 cm
  areas <- calc_sapwood_areas(
    dbh = 20, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 1.2
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0
  )

  result <- calc_sap_flux(flux_data, areas)

  expected_Q <- areas$rings$area_cm2[1] * 10.0
  expect_equal(unname(result$Q_total_cm3_hr[1]), expected_Q, tolerance = 0.01)
  expect_equal(unname(result$Q_total_L_hr[1]), expected_Q / 1000, tolerance = 0.001)
  expect_equal(unname(result$Q_total_L_day[1]), unname(result$Q_total_L_hr[1]) * 24, tolerance = 0.001)
})


test_that("calc_sap_flux applies velocity assumption for sensorless inner ring", {
  # 2 rings, inner sensorless. sapwood_thickness = 2.0 cm, inner_sensor at 2.25 (in heartwood)
  areas <- calc_sapwood_areas(
    dbh = 25, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.0
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 0.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * (10.0 / 2)

  expect_equal(unname(result$Q_total_cm3_hr[1]), expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux integrates directly when inner sensor is active", {
  # 2 rings, inner sensor measured. sapwood_thickness = 2.5 cm
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 8.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * 8.0

  expect_equal(unname(result$Q_total_cm3_hr[1]), expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux handles three rings with beyond-probe sensorless zone", {
  # 3 rings. sapwood_thickness = 3.5 cm > inner_det_lim 2.75
  areas <- calc_sapwood_areas(
    dbh = 40, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 3.5
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 7.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  A_outer     <- areas$rings$area_cm2[1]
  A_inner     <- areas$rings$area_cm2[2]
  A_beyond    <- areas$rings$area_cm2[3]
  expected_Q  <- A_outer * 10.0 + A_inner * 7.0 + A_beyond * (7.0 / 2)

  expect_equal(unname(result$Q_total_cm3_hr[1]), expected_Q, tolerance = 0.01)
})


# =============================================================================
# Test calc_sap_flux() - Multiple Timestamps
# =============================================================================

test_that("calc_sap_flux handles multiple timestamps — one row per timestamp", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  flux_data <- data.frame(
    datetime = rep(as.POSIXct(c("2024-01-01 10:00:00",
                                 "2024-01-01 11:00:00",
                                 "2024-01-01 12:00:00"), tz = "UTC"), each = 2),
    sensor_position = rep(c("outer", "inner"), 3),
    Jv_cm3_cm2_hr = c(5.0, 4.0, 10.0, 8.0, 15.0, 12.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  # One row per timestamp — prevents double-counting when summing Q
  expect_equal(nrow(result), 3)

  # Q increases with Jv across timestamps
  expect_true(all(diff(result$Q_total_cm3_hr) > 0))
})


# =============================================================================
# Test calc_sap_flux() - Edge Cases
# =============================================================================

test_that("calc_sap_flux handles NA values in Jv", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, NA)
  )

  result <- calc_sap_flux(flux_data, areas)

  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * 0.0

  expect_equal(unname(result$Q_total_cm3_hr[1]), expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux handles missing sensor data", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0
  )

  result <- calc_sap_flux(flux_data, areas)

  expect_true(!is.na(result$Q_total_cm3_hr[1]))
  expect_true(result$Q_total_cm3_hr[1] > 0)
})


test_that("calc_sap_flux handles zero flux", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(0.0, 0.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  expect_equal(unname(result$Q_total_cm3_hr[1]), 0.0)
  expect_equal(unname(result$Q_total_L_hr[1]), 0.0)
  expect_equal(unname(result$Q_total_L_day[1]), 0.0)
})


# =============================================================================
# Test calc_sap_flux() - Error Handling
# =============================================================================

test_that("calc_sap_flux errors on invalid inputs", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )

  expect_error(
    calc_sap_flux(c(1, 2, 3), areas),
    "flux_data must be a data frame"
  )

  bad_data <- data.frame(
    datetime = as.POSIXct("2024-01-01", tz = "UTC"),
    wrong_col = "outer"
  )

  expect_error(
    calc_sap_flux(bad_data, areas),
    "missing required columns"
  )

  expect_error(
    calc_sap_flux(
      data.frame(datetime = Sys.time(), sensor_position = "outer", Jv_cm3_cm2_hr = 10),
      list(wrong_structure = TRUE)
    ),
    "sapwood_areas must be output from calc_sapwood_areas"
  )
})


# =============================================================================
# Test apply_sap_flux_integration() - Convenience Wrapper
# =============================================================================

test_that("apply_sap_flux_integration works end-to-end", {
  flux_data <- data.frame(
    datetime = rep(as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 12:00:00"),
                               tz = "UTC"), each = 2),
    sensor_position = rep(c("outer", "inner"), 2),
    Jv_cm3_cm2_hr = c(5.0, 4.0, 10.0, 8.0),
    dbh = 30,
    sapwood_thickness = 2.5,
    bark_thickness_dbh = 0.5,
    bark_thickness_probe = 0.5
  )

  result <- suppressMessages(apply_sap_flux_integration(flux_data))

  expect_true("Q_total_cm3_hr" %in% names(result))
  expect_true("Q_total_L_hr" %in% names(result))
  expect_true("Q_total_L_day" %in% names(result))
  expect_true(all(result$Q_total_cm3_hr > 0))
  expect_true(!is.null(attr(result, "sapwood_areas")))
})


test_that("apply_sap_flux_integration handles custom column names", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0,
    tree_dbh = 30,
    sw_thick = 2.5,
    bark_dbh = 0.5,
    bark_probe = 0.5
  )

  result <- suppressMessages(
    apply_sap_flux_integration(
      flux_data,
      dbh_col                  = "tree_dbh",
      sapwood_thickness_col    = "sw_thick",
      bark_thickness_dbh_col   = "bark_dbh",
      bark_thickness_probe_col = "bark_probe"
    )
  )

  expect_true("Q_total_L_hr" %in% names(result))
  expect_true(result$Q_total_L_hr[1] > 0)
})


test_that("apply_sap_flux_integration errors when bark columns are missing", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0,
    dbh = 30,
    sapwood_thickness = 2.5
  )

  expect_error(
    apply_sap_flux_integration(flux_data),
    "bark_thickness_dbh"
  )
})


# =============================================================================
# Test Integration Accuracy (return value completeness)
# =============================================================================

test_that("calc_sapwood_areas return value includes all required fields", {
  areas <- calc_sapwood_areas(
    dbh                  = 18.2,
    bark_thickness_dbh   = 1.7,
    bark_thickness_probe = 0.5,
    sapwood_thickness    = 3.0,
    probe_config         = list(
      probe = list(length = 35, outer_sensor = 22.5, inner_sensor = 7.5, spacer_thickness = 2.5)
    )
  )

  # tree_dimensions must expose both bark thicknesses and spacer
  expect_true("bark_thickness_dbh_cm"   %in% names(areas$tree_dimensions))
  expect_true("bark_thickness_probe_cm" %in% names(areas$tree_dimensions))
  expect_true("spacer_thickness_cm"     %in% names(areas$tree_dimensions))
  expect_equal(areas$tree_dimensions$bark_thickness_dbh_cm,   1.7, tolerance = 0.001)
  expect_equal(areas$tree_dimensions$bark_thickness_probe_cm, 0.5, tolerance = 0.001)
  expect_equal(areas$tree_dimensions$spacer_thickness_cm,     0.25, tolerance = 0.001)

  # probe_landmarks present
  expect_true("outer_sensor_depth_cm"   %in% names(areas$probe_landmarks))
  expect_true("inner_sensor_depth_cm"   %in% names(areas$probe_landmarks))
  expect_true("midpoint_depth_cm"       %in% names(areas$probe_landmarks))
  expect_true("probe_tip_depth_cm"      %in% names(areas$probe_landmarks))
  expect_true("outer_det_lim_depth_cm"  %in% names(areas$probe_landmarks))
  expect_true("inner_det_lim_depth_cm"  %in% names(areas$probe_landmarks))

  # outer_det_lim = outer_sensor + 0.5; outer_sensor_depth = 0.5 cm → 1.0 cm
  expect_equal(areas$probe_landmarks$outer_det_lim_depth_cm, 1.0, tolerance = 0.001)
  # inner_det_lim = min(inner_sensor + 0.5, probe_tip)
  # inner_sensor_depth = 2.0 cm, probe_tip = 2.75 cm → min(2.5, 2.75) = 2.5
  expect_equal(areas$probe_landmarks$inner_det_lim_depth_cm, 2.5, tolerance = 0.001)
})


# =============================================================================
# Regression test against fixture reference values
# =============================================================================

test_that("calc_sapwood_areas matches reference fixture values", {
  fixture_path <- testthat::test_path("fixtures", "sapwood_reference_cases.csv")
  skip_if_not(file.exists(fixture_path), "fixture file not found")

  ref <- read.csv(fixture_path, stringsAsFactors = FALSE)

  probe_fn <- function(spacer_mm) {
    list(probe = list(length = 35, outer_sensor = 22.5, inner_sensor = 7.5,
                      spacer_thickness = spacer_mm))
  }

  for (i in seq_len(nrow(ref))) {
    r   <- ref[i, ]
    lbl <- r$case_id

    a <- calc_sapwood_areas(
      dbh                  = r$dbh,
      bark_thickness_dbh   = r$bark_thickness_dbh,
      bark_thickness_probe = r$bark_thickness_probe,
      sapwood_thickness    = r$sapwood_thickness,
      probe_config         = probe_fn(r$spacer_mm)
    )

    expect_equal(a$tree_dimensions$cambium_radius_cm,   r$cambium_radius_cm,   tolerance = 0.001, label = paste(lbl, "cambium_r"))
    expect_equal(a$tree_dimensions$actual_sapwood_cm,   r$actual_sapwood_cm,   tolerance = 0.001, label = paste(lbl, "actual_sapwood"))
    expect_equal(a$total_sapwood_area_cm2,              r$total_sapwood_area,  tolerance = 0.01,  label = paste(lbl, "total_area"))
    expect_equal(a$probe_landmarks$outer_sensor_depth_cm,  r$outer_sensor_depth, tolerance = 0.001, label = paste(lbl, "outer_depth"))
    expect_equal(a$probe_landmarks$inner_sensor_depth_cm,  r$inner_sensor_depth, tolerance = 0.001, label = paste(lbl, "inner_depth"))
    expect_equal(a$probe_landmarks$inner_det_lim_depth_cm, r$inner_det_lim_depth, tolerance = 0.001, label = paste(lbl, "det_lim"))
    expect_equal(nrow(a$rings), r$n_zones, label = paste(lbl, "n_zones"))

    if (r$n_zones >= 1) expect_equal(a$rings$area_cm2[1], r$zone1_area, tolerance = 0.01, label = paste(lbl, "zone1"))
    if (r$n_zones >= 2) expect_equal(a$rings$area_cm2[2], r$zone2_area, tolerance = 0.01, label = paste(lbl, "zone2"))
    if (r$n_zones >= 3) expect_equal(a$rings$area_cm2[3], r$zone3_area, tolerance = 0.01, label = paste(lbl, "zone3"))
  }
})


# =============================================================================
# Test calc_sap_flux() — radial components and no-duplication properties
# =============================================================================

test_that("calc_sap_flux Q components sum to Q_total", {
  areas <- calc_sapwood_areas(
    dbh = 40, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 3.5
  )
  flux_data <- data.frame(
    datetime        = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr   = c(10.0, 7.0)
  )
  result <- calc_sap_flux(flux_data, areas)

  expect_equal(
    result$Q_outer_cm3_hr + result$Q_inner_cm3_hr + result$Q_unmeasured_cm3_hr,
    result$Q_total_cm3_hr,
    tolerance = 1e-9
  )
})


test_that("calc_sap_flux Q_outer is zero when only inner sensor measured (direct zone)", {
  # 2 rings, inner measured (sapwood 2.5 cm)
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )
  flux_data <- data.frame(
    datetime        = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr   = c(10.0, 8.0)
  )
  result <- calc_sap_flux(flux_data, areas)

  expect_true(result$Q_outer_cm3_hr > 0)
  expect_true(result$Q_inner_cm3_hr > 0)
  expect_equal(result$Q_unmeasured_cm3_hr, 0, tolerance = 1e-9)
})


test_that("calc_sap_flux linear_decay halves unmeasured relative to constant_velocity", {
  # 3 rings (sapwood 3.5 cm → beyond-probe sensorless zone)
  areas <- calc_sapwood_areas(
    dbh = 40, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 3.5
  )
  flux_data <- data.frame(
    datetime        = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr   = c(10.0, 7.0)
  )
  q_decay    <- calc_sap_flux(flux_data, areas, method = "linear_decay")
  q_constant <- calc_sap_flux(flux_data, areas, method = "constant_velocity")

  # Unmeasured zone contribution should be exactly doubled under constant_velocity
  expect_equal(
    q_constant$Q_unmeasured_cm3_hr,
    2 * q_decay$Q_unmeasured_cm3_hr,
    tolerance = 1e-9
  )
  # Measured components are identical under both methods
  expect_equal(q_decay$Q_outer_cm3_hr, q_constant$Q_outer_cm3_hr, tolerance = 1e-9)
  expect_equal(q_decay$Q_inner_cm3_hr, q_constant$Q_inner_cm3_hr, tolerance = 1e-9)
})


test_that("calc_sap_flux no double-counting: summing Q over rows equals one tree-period", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )
  # 3 timestamps × 2 sensors — 6 input rows; expect 3 output rows
  flux_data <- data.frame(
    datetime = rep(as.POSIXct(c("2024-01-01 10:00:00",
                                 "2024-01-01 11:00:00",
                                 "2024-01-01 12:00:00"), tz = "UTC"), each = 2),
    sensor_position = rep(c("outer", "inner"), 3),
    Jv_cm3_cm2_hr = c(5.0, 4.0, 10.0, 8.0, 15.0, 12.0)
  )
  result <- calc_sap_flux(flux_data, areas)

  expect_equal(nrow(result), 3)
  # Sum of Q across rows equals sum of per-timestamp unique totals (no inflation)
  expect_equal(sum(result$Q_total_L_hr), sum(result$Q_total_L_hr), tolerance = 1e-9)
  # All Q values positive
  expect_true(all(result$Q_total_L_hr > 0))
})


# =============================================================================
# Test aggregate_daily_flux()
# =============================================================================

test_that("aggregate_daily_flux produces correct daily totals for hourly data", {
  areas <- calc_sapwood_areas(
    dbh = 30, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 2.5
  )
  # 48 half-hourly records per day, constant Jv (00:00–23:30 → all one day)
  n <- 48
  flux_data <- data.frame(
    datetime        = seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                          by = "30 min", length.out = n),
    sensor_position = rep("outer", n),
    Jv_cm3_cm2_hr   = rep(10.0, n)
  )
  q <- calc_sap_flux(flux_data, areas)
  daily <- aggregate_daily_flux(q)

  expect_equal(nrow(daily), 1)
  # Expected: Q_total_L_hr constant × 0.5 h × 48 records = 24 h of flow
  expected_L_day <- q$Q_total_L_hr[1] * 0.5 * n
  expect_equal(daily$Q_total_L_day, expected_L_day, tolerance = 0.001)
})


test_that("aggregate_daily_flux components sum to total", {
  areas <- calc_sapwood_areas(
    dbh = 40, bark_thickness_dbh = 0.5, bark_thickness_probe = 0.5, sapwood_thickness = 3.5
  )
  n_ts <- 24  # unique timestamps
  flux_data <- data.frame(
    datetime        = rep(seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                              by = "1 hour", length.out = n_ts), each = 2),
    sensor_position = rep(c("outer", "inner"), n_ts),
    Jv_cm3_cm2_hr   = rep(c(10.0, 7.0), n_ts)
  )
  q <- calc_sap_flux(flux_data, areas)
  daily <- aggregate_daily_flux(q)

  expect_equal(
    daily$Q_outer_L_day + daily$Q_inner_L_day + daily$Q_unmeasured_L_day,
    daily$Q_total_L_day,
    tolerance = 1e-9
  )
})
