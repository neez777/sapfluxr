# tests/testthat/test-sapwood-integration.R
# Tests for Sapwood Area Integration and Sap Flux Calculation

# =============================================================================
# Test calc_sapwood_areas() - Ring Allocation
# =============================================================================

test_that("calc_sapwood_areas allocates single outer ring for shallow sapwood", {
  # Case 1: Sapwood < 10mm
  areas <- calc_sapwood_areas(
    dbh = 20,
    bark_thickness = 0.5,
    sapwood_depth = 0.8  # 8mm sapwood
  )

  # Should have only 1 ring (outer)
  expect_equal(nrow(areas$rings), 1)
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$ring_name[1], "outer_ring")

  # Ring should span full sapwood depth
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.0-0.8")
})


test_that("calc_sapwood_areas allocates outer + partial inner for medium sapwood", {
  # Case 2: Sapwood 10-20mm (inner sensor in heartwood)
  areas <- calc_sapwood_areas(
    dbh = 25,
    bark_thickness = 0.5,
    sapwood_depth = 1.5  # 15mm sapwood
  )

  # Should have 2 rings
  expect_equal(nrow(areas$rings), 2)

  # Outer ring: 0-1.0cm
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.0-1.0")

  # Inner ring: 1.0cm to sapwood boundary
  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.0-1.5")
})


test_that("calc_sapwood_areas allocates outer + full inner for 20-25mm sapwood", {
  # Case 3: Sapwood 20-25mm
  areas <- calc_sapwood_areas(
    dbh = 30,
    bark_thickness = 0.5,
    sapwood_depth = 2.2  # 22mm sapwood
  )

  # Should have 2 rings
  expect_equal(nrow(areas$rings), 2)

  # Outer ring: 0-1.0cm
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.0-1.0")

  # Inner ring: 1.0-2.0cm
  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.0-2.0")
})


test_that("calc_sapwood_areas allocates three rings for deep sapwood", {
  # Case 4: Sapwood > 25mm
  areas <- calc_sapwood_areas(
    dbh = 40,
    bark_thickness = 0.5,
    sapwood_depth = 3.5  # 35mm sapwood
  )

  # Should have 3 rings
  expect_equal(nrow(areas$rings), 3)

  # Outer ring: 0-1.0cm
  expect_equal(areas$rings$sensor[1], "outer")
  expect_equal(areas$rings$depth_from_cambium_cm[1], "0.0-1.0")

  # Inner ring: 1.0-2.0cm
  expect_equal(areas$rings$sensor[2], "inner")
  expect_equal(areas$rings$depth_from_cambium_cm[2], "1.0-2.0")

  # Innermost ring: 2.0cm to sapwood boundary
  expect_equal(areas$rings$sensor[3], "innermost")
  expect_equal(areas$rings$depth_from_cambium_cm[3], "2.0-3.5")
})


# =============================================================================
# Test calc_sapwood_areas() - Area Calculations
# =============================================================================

test_that("calc_sapwood_areas calculates areas correctly", {
  areas <- calc_sapwood_areas(
    dbh = 30,
    bark_thickness = 0.5,
    sapwood_depth = 2.5
  )

  # Stem radius = 15 cm
  # Cambium radius = 15 - 0.5 = 14.5 cm
  # Heartwood radius = 14.5 - 2.5 = 12.0 cm

  # Total sapwood area
  expected_total <- pi * (14.5^2 - 12.0^2)
  expect_equal(areas$total_sapwood_area_cm2, expected_total, tolerance = 0.01)

  # Sum of ring areas should equal total
  ring_sum <- sum(areas$rings$area_cm2)
  expect_equal(ring_sum, expected_total, tolerance = 0.01)
})


test_that("calc_sapwood_areas handles zero bark thickness", {
  areas <- calc_sapwood_areas(
    dbh = 20,
    bark_thickness = 0,
    sapwood_depth = 1.5
  )

  # Cambium radius should equal stem radius
  expect_equal(
    areas$tree_dimensions$cambium_radius_cm,
    areas$tree_dimensions$stem_radius_cm
  )

  # Should still calculate valid areas
  expect_true(areas$total_sapwood_area_cm2 > 0)
  expect_true(all(areas$rings$area_cm2 > 0))
})


test_that("calc_sapwood_areas handles tree with no heartwood", {
  # Extreme case: sapwood extends to pith
  expect_warning(
    areas <- calc_sapwood_areas(
      dbh = 10,
      bark_thickness = 0.3,
      sapwood_depth = 10  # Sapwood deeper than possible
    ),
    "no heartwood"
  )

  # Heartwood radius should be set to 0
  expect_equal(areas$tree_dimensions$heartwood_radius_cm, 0)

  # Total area should still be positive
  expect_true(areas$total_sapwood_area_cm2 > 0)
})


# =============================================================================
# Test calc_sapwood_areas() - Error Handling
# =============================================================================

test_that("calc_sapwood_areas errors on invalid inputs", {
  expect_error(
    calc_sapwood_areas(dbh = -10, sapwood_depth = 2),
    "dbh must be a positive number"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, bark_thickness = -1, sapwood_depth = 2),
    "bark_thickness must be a non-negative"
  )

  expect_error(
    calc_sapwood_areas(dbh = 20, sapwood_depth = 0),
    "sapwood_depth must be a positive"
  )
})


test_that("calc_sapwood_areas warns when sapwood exceeds radius", {
  expect_warning(
    calc_sapwood_areas(dbh = 10, bark_thickness = 1, sapwood_depth = 10),
    "exceeds stem radius"
  )
})


# =============================================================================
# Test calc_sap_flux() - Integration with Different Sapwood Depths
# =============================================================================

test_that("calc_sap_flux integrates correctly for shallow sapwood", {
  # Case 1: Sapwood < 10mm (single outer ring)
  areas <- calc_sapwood_areas(dbh = 20, sapwood_depth = 0.8)

  # Create test flux data
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0
  )

  result <- calc_sap_flux(flux_data, areas)

  # Q = Area_outer × Jv_outer
  expected_Q <- areas$rings$area_cm2[1] * 10.0
  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.01)

  # Check L/hr and L/day conversions
  expect_equal(result$Q_L_hr[1], expected_Q / 1000, tolerance = 0.001)
  expect_equal(result$Q_L_day[1], result$Q_L_hr[1] * 24, tolerance = 0.001)
})


test_that("calc_sap_flux applies velocity assumptions for medium sapwood", {
  # Case 2: Sapwood 10-20mm (inner sensor in heartwood)
  areas <- calc_sapwood_areas(dbh = 25, sapwood_depth = 1.5)

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 0.0)  # Inner is in heartwood (Jv=0)
  )

  result <- calc_sap_flux(flux_data, areas)

  # Q = Area_outer × Jv_outer + Area_inner × (Jv_outer / 2)
  # Inner ring velocity assumed to be half of outer
  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * (10.0 / 2)

  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux integrates directly for full inner sensor coverage", {
  # Case 3: Sapwood 20-25mm (inner sensor measures its ring)
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.2)

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 8.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  # Q = Area_outer × Jv_outer + Area_inner × Jv_inner
  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * 8.0

  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux handles deep sapwood with innermost ring", {
  # Case 4: Sapwood > 25mm (three rings)
  areas <- calc_sapwood_areas(dbh = 40, sapwood_depth = 3.5)

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 7.0)
  )

  result <- calc_sap_flux(flux_data, areas)

  # Q = Area_outer × Jv_outer + Area_inner × Jv_inner + Area_innermost × (Jv_inner / 2)
  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  A_innermost <- areas$rings$area_cm2[3]
  expected_Q <- A_outer * 10.0 + A_inner * 7.0 + A_innermost * (7.0 / 2)

  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.01)
})


# =============================================================================
# Test calc_sap_flux() - Multiple Timestamps
# =============================================================================

test_that("calc_sap_flux handles multiple timestamps", {
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)

  flux_data <- data.frame(
    datetime = rep(as.POSIXct(c("2024-01-01 10:00:00",
                                 "2024-01-01 11:00:00",
                                 "2024-01-01 12:00:00"), tz = "UTC"), each = 2),
    sensor_position = rep(c("outer", "inner"), 3),
    Jv_cm3_cm2_hr = c(5.0, 4.0,  # 10:00
                       10.0, 8.0,  # 11:00
                       15.0, 12.0)  # 12:00
  )

  result <- calc_sap_flux(flux_data, areas)

  # Should have 6 rows (2 sensors × 3 timestamps)
  expect_equal(nrow(result), 6)

  # Each timestamp should have the same Q value for both sensors
  q_values <- result %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(unique_q = length(unique(Q_cm3_hr)))

  expect_true(all(q_values$unique_q == 1))

  # Q should increase over time (flux increasing)
  q_by_time <- result %>%
    dplyr::group_by(datetime) %>%
    dplyr::slice(1) %>%
    dplyr::pull(Q_cm3_hr)

  expect_true(all(diff(q_by_time) > 0))
})


# =============================================================================
# Test calc_sap_flux() - Edge Cases
# =============================================================================

test_that("calc_sap_flux handles NA values in Jv", {
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, NA)  # Inner sensor has NA
  )

  result <- calc_sap_flux(flux_data, areas)

  # Should treat NA as 0
  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  expected_Q <- A_outer * 10.0 + A_inner * 0.0

  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.01)
})


test_that("calc_sap_flux handles missing sensor data", {
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)

  # Only outer sensor data
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0
  )

  result <- calc_sap_flux(flux_data, areas)

  # Should handle missing inner sensor (treat as 0)
  expect_true(!is.na(result$Q_cm3_hr[1]))
  expect_true(result$Q_cm3_hr[1] > 0)
})


test_that("calc_sap_flux handles zero flux", {
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)

  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(0.0, 0.0)  # Nighttime, no flow
  )

  result <- calc_sap_flux(flux_data, areas)

  # Q should be 0
  expect_equal(result$Q_cm3_hr[1], 0.0)
  expect_equal(result$Q_L_hr[1], 0.0)
  expect_equal(result$Q_L_day[1], 0.0)
})


# =============================================================================
# Test calc_sap_flux() - Error Handling
# =============================================================================

test_that("calc_sap_flux errors on invalid inputs", {
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 2.5)

  # Not a dataframe
  expect_error(
    calc_sap_flux(c(1, 2, 3), areas),
    "flux_data must be a data frame"
  )

  # Missing required columns
  bad_data <- data.frame(
    datetime = as.POSIXct("2024-01-01", tz = "UTC"),
    wrong_col = "outer"
  )

  expect_error(
    calc_sap_flux(bad_data, areas),
    "missing required columns"
  )

  # Invalid sapwood_areas object
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
  # Create test data with tree dimensions
  flux_data <- data.frame(
    datetime = rep(as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 12:00:00"),
                               tz = "UTC"), each = 2),
    sensor_position = rep(c("outer", "inner"), 2),
    Jv_cm3_cm2_hr = c(5.0, 4.0, 10.0, 8.0),
    dbh = 30,
    sapwood_depth = 2.5,
    bark_thickness = 0.5
  )

  result <- suppressMessages(apply_sap_flux_integration(flux_data))

  # Should have Q columns
  expect_true("Q_cm3_hr" %in% names(result))
  expect_true("Q_L_hr" %in% names(result))
  expect_true("Q_L_day" %in% names(result))

  # All Q values should be positive
  expect_true(all(result$Q_cm3_hr > 0))
  expect_true(all(result$Q_L_hr > 0))
  expect_true(all(result$Q_L_day > 0))

  # Sapwood areas should be attached as attribute
  expect_true(!is.null(attr(result, "sapwood_areas")))
})


test_that("apply_sap_flux_integration handles custom column names", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0,
    tree_dbh = 30,
    sw_depth = 2.5,
    bark = 0.5
  )

  result <- suppressMessages(
    apply_sap_flux_integration(
      flux_data,
      dbh_col = "tree_dbh",
      sapwood_depth_col = "sw_depth",
      bark_thickness_col = "bark"
    )
  )

  expect_true("Q_L_hr" %in% names(result))
  expect_true(result$Q_L_hr[1] > 0)
})


test_that("apply_sap_flux_integration handles missing bark thickness column", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = "outer",
    Jv_cm3_cm2_hr = 10.0,
    dbh = 30,
    sapwood_depth = 2.5
    # No bark_thickness column
  )

  result <- suppressMessages(
    apply_sap_flux_integration(flux_data)
  )

  # Should default to 0 bark thickness
  areas <- attr(result, "sapwood_areas")
  expect_equal(areas$tree_dimensions$bark_thickness_cm, 0)
})


# =============================================================================
# Test Integration Accuracy (Hatton 1990 Examples)
# =============================================================================

test_that("integration matches expected values for known scenarios", {
  # Scenario: 30cm DBH, 3cm sapwood, with velocity gradient
  areas <- calc_sapwood_areas(dbh = 30, sapwood_depth = 3.0)

  # Flux density measurements
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    sensor_position = c("outer", "inner"),
    Jv_cm3_cm2_hr = c(10.0, 8.0)  # Velocity decreases with depth
  )

  result <- calc_sap_flux(flux_data, areas)

  # Calculate expected Q:
  # Q = A_outer × Jv_outer + A_inner × Jv_inner + A_innermost × (Jv_inner/2)
  A_outer <- areas$rings$area_cm2[1]
  A_inner <- areas$rings$area_cm2[2]
  A_innermost <- areas$rings$area_cm2[3]
  expected_Q <- A_outer * 10.0 + A_inner * 8.0 + A_innermost * (8.0 / 2)

  expect_equal(result$Q_cm3_hr[1], expected_Q, tolerance = 0.1)
})
