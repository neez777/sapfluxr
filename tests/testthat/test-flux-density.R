# tests/testthat/test-flux-density.R
# Tests for Sap Flux Density Conversion Functions

# Helper function to create wood properties for tests
create_test_wood_props <- function() {
  wood_props <- WoodProperties$new(
    wood_measurements = list(
      fresh_weight_g = 80,
      dry_weight_g = 55,
      fresh_volume_cm3 = 100
    ),
    wood_constants = list(
      rho_sap_kg_m3 = 1000,
      rho_cell_wall_kg_m3 = 1540,
      K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186,
      c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  )

  wood_props <- calculate_wood_properties(wood_props)
  return(wood_props)
}

# =============================================================================
# Test calc_sap_flux_density() - Basic Functionality
# =============================================================================

test_that("calc_sap_flux_density converts single velocity value correctly", {
  wood_props <- create_test_wood_props()
  Z <- wood_props$derived_properties$sap_flux_conversion_factor

  Vh <- 10.5
  Jv <- calc_sap_flux_density(Vh, wood_props)

  # Expected: Jv = Z × Vh
  expected <- Z * Vh
  expect_equal(Jv, expected, tolerance = 0.001)
})


test_that("calc_sap_flux_density converts vector of velocities correctly", {
  wood_props <- create_test_wood_props()
  Z <- wood_props$derived_properties$sap_flux_conversion_factor

  Vh_vec <- c(5.2, 10.5, 15.8, 20.1, -2.3)
  Jv_vec <- calc_sap_flux_density(Vh_vec, wood_props)

  # Expected: Jv = Z × Vh (vectorized)
  expected <- Z * Vh_vec
  expect_equal(Jv_vec, expected, tolerance = 0.001)
  expect_equal(length(Jv_vec), length(Vh_vec))
})


test_that("calc_sap_flux_density handles NA values correctly", {
  wood_props <- create_test_wood_props()

  Vh_with_na <- c(10, NA, 15, NA, 20)
  Jv <- calc_sap_flux_density(Vh_with_na, wood_props)

  # Check that NA positions are preserved
  expect_true(is.na(Jv[2]))
  expect_true(is.na(Jv[4]))
  expect_false(is.na(Jv[1]))
  expect_false(is.na(Jv[3]))
  expect_false(is.na(Jv[5]))
})


test_that("calc_sap_flux_density handles negative velocities (reverse flow)", {
  wood_props <- create_test_wood_props()
  Z <- wood_props$derived_properties$sap_flux_conversion_factor

  Vh_negative <- c(-5.2, -10.1, -2.3)
  Jv <- calc_sap_flux_density(Vh_negative, wood_props)

  # All should be negative and correctly scaled
  expect_true(all(Jv < 0))
  expect_equal(Jv, Z * Vh_negative, tolerance = 0.001)
})


# =============================================================================
# Test calc_sap_flux_density() - Error Handling
# =============================================================================

test_that("calc_sap_flux_density errors if wood_properties not provided", {
  expect_error(
    calc_sap_flux_density(10.5, NULL),
    "wood_properties must be a WoodProperties R6 object"
  )
})


test_that("calc_sap_flux_density errors if Z factor not calculated", {
  # Create wood properties but DON'T calculate derived properties
  wood_props <- WoodProperties$new(
    wood_measurements = list(
      fresh_weight_g = 80,
      dry_weight_g = 55,
      fresh_volume_cm3 = 100
    ),
    wood_constants = list(
      rho_sap_kg_m3 = 1000,
      rho_cell_wall_kg_m3 = 1540,
      K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186,
      c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  )

  # Try to convert without calculating Z factor
  expect_error(
    calc_sap_flux_density(10.5, wood_props),
    "Sap flux conversion factor.*not calculated"
  )
})


# =============================================================================
# Test apply_flux_conversion() - Data Frame Operations
# =============================================================================

test_that("apply_flux_conversion adds Jv column to data frame", {
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                   as.POSIXct("2024-01-01 05:00:00", tz = "UTC"),
                   by = "hour"),
    pulse_id = 1:6,
    Vh_cm_hr = c(8.2, 10.5, 12.3, 15.1, 18.4, 20.2),
    Vc_cm_hr = c(10.4, 13.2, 15.5, 19.0, 23.1, 25.4)
  )

  wood_props <- create_test_wood_props()

  # Apply flux conversion (suppress printed output)
  result <- suppressMessages(
    apply_flux_conversion(test_data, wood_props, velocity_col = "Vc_cm_hr")
  )

  # Check that Jv column was added
  expect_true("Jv_cm3_cm2_hr" %in% names(result))

  # Check that original columns are preserved
  expect_true(all(c("datetime", "pulse_id", "Vh_cm_hr", "Vc_cm_hr") %in% names(result)))

  # Check row count unchanged
  expect_equal(nrow(result), nrow(test_data))
})


test_that("apply_flux_conversion calculates correct values", {
  test_data <- data.frame(
    Vc_cm_hr = c(10.0, 20.0, 30.0)
  )

  wood_props <- create_test_wood_props()
  Z <- wood_props$derived_properties$sap_flux_conversion_factor

  result <- suppressMessages(
    apply_flux_conversion(test_data, wood_props)
  )

  # Expected values: Jv = Z × Vc
  expected <- test_data$Vc_cm_hr * Z
  expect_equal(result$Jv_cm3_cm2_hr, expected, tolerance = 0.001)
})


test_that("apply_flux_conversion allows custom column names", {
  test_data <- data.frame(
    my_velocity = c(10, 15, 20)
  )

  wood_props <- create_test_wood_props()

  result <- suppressMessages(
    apply_flux_conversion(
      test_data,
      wood_props,
      velocity_col = "my_velocity",
      output_col = "my_flux"
    )
  )

  expect_true("my_flux" %in% names(result))
  expect_false("Jv_cm3_cm2_hr" %in% names(result))
})


test_that("apply_flux_conversion errors if velocity column missing", {
  test_data <- data.frame(
    wrong_column = c(10, 15, 20)
  )

  wood_props <- create_test_wood_props()

  expect_error(
    apply_flux_conversion(test_data, wood_props),
    "Column Vc_cm_hr not found"
  )
})


test_that("apply_flux_conversion errors if data is not a data frame", {
  wood_props <- create_test_wood_props()

  expect_error(
    apply_flux_conversion(c(10, 15, 20), wood_props),
    "data must be a data frame"
  )
})


# =============================================================================
# Test get_sap_flux_conversion_factor()
# =============================================================================

test_that("get_sap_flux_conversion_factor extracts Z correctly", {
  wood_props <- create_test_wood_props()

  Z <- get_sap_flux_conversion_factor(wood_props)

  # Should match the derived property
  expect_equal(Z, wood_props$derived_properties$sap_flux_conversion_factor)
  expect_true(Z > 0)
})


test_that("get_sap_flux_conversion_factor errors if Z not calculated", {
  wood_props <- WoodProperties$new(
    wood_measurements = list(
      fresh_weight_g = 80,
      dry_weight_g = 55,
      fresh_volume_cm3 = 100
    ),
    wood_constants = list(
      rho_sap_kg_m3 = 1000,
      rho_cell_wall_kg_m3 = 1540,
      K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186,
      c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  )

  # Don't calculate derived properties
  expect_error(
    get_sap_flux_conversion_factor(wood_props),
    "not calculated"
  )
})


# =============================================================================
# Test Integration with Real Workflow
# =============================================================================

test_that("flux density integrates correctly with wound correction workflow", {
  # Simulate wound-corrected data
  test_data <- data.frame(
    datetime = seq(as.POSIXct("2024-01-01", tz = "UTC"),
                   as.POSIXct("2024-01-10", tz = "UTC"),
                   by = "day"),
    sensor_position = rep("outer", 10),
    Vh_cm_hr = seq(10, 19, by = 1),
    Vc_cm_hr = seq(12, 21, by = 1),
    wound_correction_applied = TRUE
  )

  wood_props <- create_test_wood_props()

  # Apply flux conversion
  result <- suppressMessages(
    apply_flux_conversion(test_data, wood_props, velocity_col = "Vc_cm_hr")
  )

  # Check workflow columns exist
  expect_true(all(c("Vh_cm_hr", "Vc_cm_hr", "Jv_cm3_cm2_hr") %in% names(result)))

  # Z factor should be calculated and positive
  Z <- wood_props$derived_properties$sap_flux_conversion_factor
  expect_true(Z > 0)

  # Jv should be correctly scaled by Z
  expect_equal(result$Jv_cm3_cm2_hr, result$Vc_cm_hr * Z, tolerance = 0.001)
})


# =============================================================================
# Test Different Wood Species
# =============================================================================

test_that("flux conversion works with different wood densities", {
  # Test with low-density wood (e.g., balsa)
  wood_low <- WoodProperties$new(
    wood_measurements = list(
      fresh_weight_g = 40,
      dry_weight_g = 20,
      fresh_volume_cm3 = 100
    ),
    wood_constants = list(
      rho_sap_kg_m3 = 1000,
      rho_cell_wall_kg_m3 = 1540,
      K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186,
      c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  )
  wood_low <- calculate_wood_properties(wood_low)

  # Test with high-density wood (e.g., ironwood)
  wood_high <- WoodProperties$new(
    wood_measurements = list(
      fresh_weight_g = 120,
      dry_weight_g = 90,
      fresh_volume_cm3 = 100
    ),
    wood_constants = list(
      rho_sap_kg_m3 = 1000,
      rho_cell_wall_kg_m3 = 1540,
      K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186,
      c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  )
  wood_high <- calculate_wood_properties(wood_high)

  Vh <- 15.0
  Jv_low <- calc_sap_flux_density(Vh, wood_low)
  Jv_high <- calc_sap_flux_density(Vh, wood_high)

  # Different wood densities should give different flux densities
  expect_false(isTRUE(all.equal(Jv_low, Jv_high)))

  # Both should be positive for positive velocity
  expect_true(Jv_low > 0)
  expect_true(Jv_high > 0)
})
