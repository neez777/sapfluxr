# Tests for Wood Property Configuration System

# =============================================================================
# Configuration Loading Tests
# =============================================================================

test_that("list_available_wood_properties() returns expected configurations", {
  configs <- list_available_wood_properties()

  # Should return a data frame
  expect_s3_class(configs, "data.frame")

  # Should have required columns
  expect_true(all(c("name", "species", "description", "file", "default") %in% names(configs)))

  # Should have at least the three built-in configs
  expect_true(nrow(configs) >= 3)
  expect_true("generic_sw" %in% configs$name)
  expect_true("eucalyptus" %in% configs$name)
  expect_true("pine" %in% configs$name)

  # generic_sw should be marked as default
  default_config <- configs[configs$default, ]
  expect_equal(nrow(default_config), 1)
  expect_equal(default_config$name, "generic_sw")
})


test_that("load_wood_properties() loads default configuration", {
  wood <- load_wood_properties()

  # Should return WoodProperties object
  expect_s3_class(wood, "WoodProperties")
  expect_s3_class(wood, "R6")

  # Should load generic_sw by default
  expect_match(wood$config_name, "Softwood", ignore.case = TRUE)
  expect_equal(wood$wood_property$wood_type, "softwood")
  expect_equal(wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.0025)
})


test_that("load_wood_properties() loads specific configurations", {
  # Load eucalyptus
  euc_wood <- load_wood_properties("eucalyptus")
  expect_s3_class(euc_wood, "WoodProperties")
  expect_match(euc_wood$config_name, "Eucalyptus", ignore.case = TRUE)
  expect_equal(euc_wood$wood_property$species, "Eucalyptus spp.")
  expect_equal(euc_wood$wood_property$wood_type, "hardwood")
  expect_equal(euc_wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.00143)

  # Load pine
  pine_wood <- load_wood_properties("pine")
  expect_s3_class(pine_wood, "WoodProperties")
  expect_match(pine_wood$config_name, "Pine", ignore.case = TRUE)
  expect_equal(pine_wood$wood_property$species, "Pinus spp.")
  expect_equal(pine_wood$wood_property$wood_type, "softwood")
  expect_equal(pine_wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.00145)
})


test_that("get_default_wood_properties() works", {
  wood <- get_default_wood_properties()

  expect_s3_class(wood, "WoodProperties")
  expect_match(wood$config_name, "Softwood", ignore.case = TRUE)
  expect_equal(wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.0025)
})


test_that("load_wood_properties() respects overrides for wood constants", {
  # Override thermal_diffusivity
  wood <- load_wood_properties("eucalyptus",
                               overrides = list(thermal_diffusivity_default_cm2_s = 0.003))

  expect_equal(wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.003)
  # Other properties should remain unchanged
  expect_equal(wood$wood_property$species, "Eucalyptus spp.")
  expect_equal(wood$wood_property$wood_type, "hardwood")

  # Override multiple constants
  wood2 <- load_wood_properties("generic_sw",
                                overrides = list(
                                  thermal_diffusivity_default_cm2_s = 0.0028,
                                  rho_sap_kg_m3 = 1010,
                                  c_sap_J_kg_K = 4200
                                ))

  expect_equal(wood2$wood_constants$thermal_diffusivity_default_cm2_s, 0.0028)
  expect_equal(wood2$wood_constants$rho_sap_kg_m3, 1010)
  expect_equal(wood2$wood_constants$c_sap_J_kg_K, 4200)
})


test_that("load_wood_properties() respects tree_overrides", {
  # Override tree measurements
  wood <- load_wood_properties("eucalyptus",
                               tree_overrides = list(
                                 dbh = 45.2,
                                 sapwood_depth = 3.5
                               ))

  expect_equal(wood$tree_measurements$dbh, 45.2)
  expect_equal(wood$tree_measurements$sapwood_depth, 3.5)
  # Unspecified measurements should still be NULL
  expect_null(wood$tree_measurements$bark_thickness)
})


test_that("load_wood_properties() handles both overrides and tree_overrides", {
  wood <- load_wood_properties("pine",
                               overrides = list(thermal_diffusivity_default_cm2_s = 0.003),
                               tree_overrides = list(dbh = 50, sapwood_area = 200))

  expect_equal(wood$wood_constants$thermal_diffusivity_default_cm2_s, 0.003)
  expect_equal(wood$tree_measurements$dbh, 50)
  expect_equal(wood$tree_measurements$sapwood_area, 200)
})


test_that("WoodProperties object has new structure with required sections", {
  wood <- load_wood_properties("eucalyptus")

  # Check new structure exists
  expect_type(wood$wood_measurements, "list")
  expect_type(wood$wood_constants, "list")
  expect_type(wood$wood_property, "list")
  expect_type(wood$tree_measurements, "list")
  expect_type(wood$wound_correction, "list")
  expect_type(wood$quality_thresholds, "list")
  expect_type(wood$derived_properties, "list")

  # Check wood_constants has required fields
  expect_true("thermal_diffusivity_default_cm2_s" %in% names(wood$wood_constants))
  expect_true("rho_sap_kg_m3" %in% names(wood$wood_constants))
  expect_true("c_sap_J_kg_K" %in% names(wood$wood_constants))
  expect_true("K_sap_W_m_K" %in% names(wood$wood_constants))
  expect_true("rho_cell_wall_kg_m3" %in% names(wood$wood_constants))
  expect_true("c_dry_wood_J_kg_K" %in% names(wood$wood_constants))

  # Check wood_measurements has required fields
  expect_true("fresh_weight_g" %in% names(wood$wood_measurements))
  expect_true("dry_weight_g" %in% names(wood$wood_measurements))
  expect_true("fresh_volume_cm3" %in% names(wood$wood_measurements))
  expect_true("density_dry_kg_m3" %in% names(wood$wood_measurements))

  # Check wound_correction has required fields
  expect_true("drill_bit_diameter_mm" %in% names(wood$wound_correction))
  expect_true("wound_addition_mm" %in% names(wood$wound_correction))

  # Check tree measurements structure
  expect_true("dbh" %in% names(wood$tree_measurements))
  expect_true("sapwood_depth" %in% names(wood$tree_measurements))

  # Check quality thresholds structure
  expect_true("max_velocity_cm_hr" %in% names(wood$quality_thresholds))
  expect_true("min_velocity_cm_hr" %in% names(wood$quality_thresholds))
  expect_true("temperature_range" %in% names(wood$quality_thresholds))
})


test_that("Quality thresholds are species-specific", {
  # Generic softwood
  generic <- load_wood_properties("generic_sw")
  expect_equal(generic$quality_thresholds$max_velocity_cm_hr, 200)

  # Eucalyptus (lower max velocity)
  euc <- load_wood_properties("eucalyptus")
  expect_equal(euc$quality_thresholds$max_velocity_cm_hr, 150)

  # Pine
  pine <- load_wood_properties("pine")
  expect_equal(pine$quality_thresholds$max_velocity_cm_hr, 180)

  # Temperature ranges should differ
  expect_equal(generic$quality_thresholds$temperature_range, c(-10, 60))
  expect_equal(euc$quality_thresholds$temperature_range, c(0, 50))
  expect_equal(pine$quality_thresholds$temperature_range, c(-15, 45))
})


test_that("load_wood_properties() falls back to hardcoded defaults for invalid names", {
  # Invalid config names fall back to hardcoded generic_sw defaults
  expect_message(
    wood1 <- load_wood_properties("nonexistent_wood_type"),
    "Using hardcoded wood properties defaults"
  )
  expect_s3_class(wood1, "WoodProperties")

  expect_message(
    wood2 <- load_wood_properties("invalid_config"),
    "Using hardcoded wood properties defaults"
  )
  expect_s3_class(wood2, "WoodProperties")
})


test_that("load_wood_properties() falls back to hardcoded defaults for invalid paths", {
  # Invalid file paths fall back to hardcoded defaults
  expect_message(
    wood <- load_wood_properties("path/to/nonexistent/file.yaml"),
    "Using hardcoded wood properties defaults"
  )
  expect_s3_class(wood, "WoodProperties")
})


test_that("WoodProperties print method works with new structure", {
  wood <- load_wood_properties("eucalyptus")

  # Should not error when printing
  expect_output(print(wood), "WOOD PROPERTIES CONFIGURATION")
  expect_output(print(wood), "Eucalyptus")
  expect_output(print(wood), "CONSTANTS")
  expect_output(print(wood), "Default thermal diffusivity")
})


test_that("YAML source is stored", {
  wood <- load_wood_properties("eucalyptus")

  # Should have yaml_source field
  expect_false(is.null(wood$yaml_source))
  expect_match(wood$yaml_source, "wood_eucalyptus\\.yaml$")

  # Should have yaml_data field
  expect_type(wood$yaml_data, "list")
  expect_true("metadata" %in% names(wood$yaml_data))
  expect_true("wood_property" %in% names(wood$yaml_data))
})


test_that("Tree measurements can be NULL (optional)", {
  wood <- load_wood_properties("generic_sw")

  # Tree measurements should exist as list but values should be NULL
  expect_type(wood$tree_measurements, "list")
  expect_null(wood$tree_measurements$dbh)
  expect_null(wood$tree_measurements$sapwood_depth)
  expect_null(wood$tree_measurements$sapwood_area)
})


test_that("Quality thresholds have sensible defaults", {
  wood <- load_wood_properties("generic_sw")

  # Should have default quality thresholds
  expect_equal(wood$quality_thresholds$max_velocity_cm_hr, 200)
  expect_equal(wood$quality_thresholds$min_velocity_cm_hr, -50)
  expect_equal(wood$quality_thresholds$temperature_range, c(-10, 60))
})


test_that("Wound correction section exists with defaults", {
  wood <- load_wood_properties("generic_sw")

  # Should have wound_correction section
  expect_type(wood$wound_correction, "list")
  expect_equal(wood$wound_correction$drill_bit_diameter_mm, 2.0)
  expect_equal(wood$wound_correction$wound_addition_mm, 0.3)
  expect_null(wood$wound_correction$initial_date)
  expect_null(wood$wound_correction$final_date)
  expect_null(wood$wound_correction$final_diameter_mm)
})


# =============================================================================
# Wood Properties Calculation Tests (Method 1: Weight & Volume)
# =============================================================================

test_that("calculate_wood_properties() works with Method 1 (weight/volume)", {
  # Create wood properties with measurements (from implementation docs test data)
  wood <- load_wood_properties("generic_sw")

  # Set measurements (example from implementation documents)
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506

  # Calculate derived properties
  wood <- calculate_wood_properties(wood)

  # Check that derived properties were calculated
  expect_false(is.null(wood$derived_properties))
  expect_type(wood$derived_properties, "list")

  # Check moisture content (expected ~1.0799 kg/kg)
  expect_equal(wood$derived_properties$mc_kg_kg, (0.5493 - 0.2641) / 0.2641, tolerance = 0.0001)
  expect_equal(wood$derived_properties$mc_kg_kg, 1.0799, tolerance = 0.001)

  # Check dry wood density
  wd_kg <- 0.2641 / 1000
  vf_m3 <- 0.4506 / (100^3)
  expected_rho_dw <- wd_kg / vf_m3
  expect_equal(wood$derived_properties$rho_dw_kg_m3, expected_rho_dw, tolerance = 0.1)

  # Check fresh wood density
  wf_kg <- 0.5493 / 1000
  expected_rho_fw <- wf_kg / vf_m3
  expect_equal(wood$derived_properties$rho_fw_kg_m3, expected_rho_fw, tolerance = 0.1)

  # Check specific gravity
  expect_false(is.null(wood$derived_properties$specific_gravity))
  expect_gt(wood$derived_properties$specific_gravity, 0)
  expect_lt(wood$derived_properties$specific_gravity, 1)

  # Check conversion factors
  expect_false(is.null(wood$derived_properties$thermal_diffusivity_correction_factor))
  expect_false(is.null(wood$derived_properties$sap_flux_conversion_factor))

  # Check Z factor (expected ~0.801)
  expect_equal(wood$derived_properties$sap_flux_conversion_factor, 0.801, tolerance = 0.01)

  # Check that all key derived properties are non-NA
  expect_false(is.na(wood$derived_properties$mc_kg_kg))
  expect_false(is.na(wood$derived_properties$rho_dw_kg_m3))
  expect_false(is.na(wood$derived_properties$rho_fw_kg_m3))
  expect_false(is.na(wood$derived_properties$specific_gravity))
  expect_false(is.na(wood$derived_properties$mc_FSP_kg_kg))
  expect_false(is.na(wood$derived_properties$Fv_FSP))
})


test_that("calculate_wood_properties() calculates all derived properties correctly", {
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506

  wood <- calculate_wood_properties(wood)

  # Check all expected derived properties exist
  expected_properties <- c(
    "wf_kg", "wd_kg", "vf_m3",
    "rho_dw_kg_m3", "rho_fw_kg_m3", "basic_density_kg_m3",
    "mc_kg_kg", "mc_FSP_kg_kg", "specific_gravity", "Fv_FSP",
    "Kdw_FSP_W_m_K", "Kfw_W_m_K", "cfw_J_kg_K",
    "thermal_diffusivity_actual_cm2_s",
    "thermal_diffusivity_correction_factor",
    "sap_flux_conversion_factor"
  )

  for (prop in expected_properties) {
    expect_true(prop %in% names(wood$derived_properties),
                info = paste("Missing derived property:", prop))
  }

  # Check thermal diffusivity correction factor (Y)
  Y <- wood$derived_properties$thermal_diffusivity_correction_factor
  expect_gt(Y, 0)
  expect_lt(Y, 10)  # Should be reasonable

  # Check sap flux conversion factor (Z)
  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_gt(Z, 0)
  expect_lt(Z, 2)  # Typically between 0.4 and 1.2
})


# =============================================================================
# Wood Properties Calculation Tests (Method 2: Dual Density)
# =============================================================================

test_that("calculate_wood_properties() works with Method 2 (dual density)", {
  wood <- load_wood_properties("generic_sw")

  # Method 2: Provide both dry and fresh density
  # Using realistic values: dry = 450 kg/m³, fresh = 900 kg/m³
  # This gives mc = (900/450) - 1 = 1.0 kg/kg
  wood$wood_measurements$density_dry_kg_m3 <- 450
  wood$wood_measurements$density_fresh_kg_m3 <- 900
  wood$wood_measurements$fresh_weight_g <- NULL
  wood$wood_measurements$dry_weight_g <- NULL
  wood$wood_measurements$fresh_volume_cm3 <- NULL

  # Calculate derived properties
  wood <- calculate_wood_properties(wood)

  # Check that densities are correct
  expect_equal(wood$derived_properties$rho_dw_kg_m3, 450)
  expect_equal(wood$derived_properties$rho_fw_kg_m3, 900)
  expect_false(is.null(wood$derived_properties$specific_gravity))

  # Moisture content should be calculated from density ratio
  # mc = (ρfw/ρdw) - 1 = (900/450) - 1 = 1.0
  expect_equal(wood$derived_properties$mc_kg_kg, 1.0, tolerance = 0.001)

  # ALL thermal properties should be calculated (not NA)
  expect_false(is.na(wood$derived_properties$Kfw_W_m_K))
  expect_false(is.na(wood$derived_properties$cfw_J_kg_K))
  expect_false(is.na(wood$derived_properties$thermal_diffusivity_actual_cm2_s))

  # Sap flux conversion factor (Z) should be calculated!
  expect_false(is.na(wood$derived_properties$sap_flux_conversion_factor))
  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_gt(Z, 0)
  expect_lt(Z, 2)  # Typically between 0.4 and 1.2

  # Thermal diffusivity correction factor (Y) should be calculated
  Y <- wood$derived_properties$thermal_diffusivity_correction_factor
  expect_false(is.na(Y))
  expect_gt(Y, 0)
  expect_lt(Y, 10)
})


test_that("calculate_wood_properties() validates input methods", {
  wood <- load_wood_properties("generic_sw")

  # Remove all measurements - should error
  wood$wood_measurements$fresh_weight_g <- NULL
  wood$wood_measurements$dry_weight_g <- NULL
  wood$wood_measurements$fresh_volume_cm3 <- NULL
  wood$wood_measurements$density_dry_kg_m3 <- NULL
  wood$wood_measurements$density_fresh_kg_m3 <- NULL

  expect_error(
    calculate_wood_properties(wood),
    "Must provide wood measurements"
  )
})


test_that("calculate_wood_properties() requires BOTH densities for Method 2", {
  wood <- load_wood_properties("generic_sw")

  # Only provide dry density (missing fresh density) - should error
  wood$wood_measurements$density_dry_kg_m3 <- 450
  wood$wood_measurements$density_fresh_kg_m3 <- NULL
  wood$wood_measurements$fresh_weight_g <- NULL
  wood$wood_measurements$dry_weight_g <- NULL
  wood$wood_measurements$fresh_volume_cm3 <- NULL

  expect_error(
    calculate_wood_properties(wood),
    "Must provide wood measurements"
  )

  # Only provide fresh density (missing dry density) - should error
  wood$wood_measurements$density_dry_kg_m3 <- NULL
  wood$wood_measurements$density_fresh_kg_m3 <- 900

  expect_error(
    calculate_wood_properties(wood),
    "Must provide wood measurements"
  )
})


test_that("Method 2 (dual density) produces equivalent results to Method 1", {
  # Use Method 1 to get baseline results
  wood1 <- load_wood_properties("generic_sw")
  wood1$wood_measurements$fresh_weight_g <- 0.5493
  wood1$wood_measurements$dry_weight_g <- 0.2641
  wood1$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood1 <- calculate_wood_properties(wood1)

  # Calculate densities from Method 1
  rho_dw <- wood1$derived_properties$rho_dw_kg_m3
  rho_fw <- wood1$derived_properties$rho_fw_kg_m3

  # Use Method 2 with the same densities
  wood2 <- load_wood_properties("generic_sw")
  wood2$wood_measurements$density_dry_kg_m3 <- rho_dw
  wood2$wood_measurements$density_fresh_kg_m3 <- rho_fw
  wood2 <- calculate_wood_properties(wood2)

  # Both methods should give identical results
  expect_equal(wood2$derived_properties$mc_kg_kg,
               wood1$derived_properties$mc_kg_kg,
               tolerance = 0.001)
  expect_equal(wood2$derived_properties$rho_dw_kg_m3,
               wood1$derived_properties$rho_dw_kg_m3,
               tolerance = 0.1)
  expect_equal(wood2$derived_properties$rho_fw_kg_m3,
               wood1$derived_properties$rho_fw_kg_m3,
               tolerance = 0.1)
  expect_equal(wood2$derived_properties$specific_gravity,
               wood1$derived_properties$specific_gravity,
               tolerance = 0.001)
  expect_equal(wood2$derived_properties$sap_flux_conversion_factor,
               wood1$derived_properties$sap_flux_conversion_factor,
               tolerance = 0.001)
  expect_equal(wood2$derived_properties$thermal_diffusivity_correction_factor,
               wood1$derived_properties$thermal_diffusivity_correction_factor,
               tolerance = 0.001)
})


test_that("calculate_wood_properties() warns when both methods provided", {
  wood <- load_wood_properties("generic_sw")

  # Provide both methods
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood$wood_measurements$density_dry_kg_m3 <- 450
  wood$wood_measurements$density_fresh_kg_m3 <- 900

  expect_warning(
    wood <- calculate_wood_properties(wood),
    "Both weight/volume data AND densities provided"
  )

  # Should use Method 1 (weight/volume) and ignore densities
  expect_false(is.na(wood$derived_properties$mc_kg_kg))
  # Calculated from actual measurements, not from density ratio
  expect_equal(wood$derived_properties$mc_kg_kg,
               (0.5493 - 0.2641) / 0.2641,
               tolerance = 0.001)
})


test_that("calculate_wood_properties() requires WoodProperties object", {
  expect_error(
    calculate_wood_properties(list(foo = "bar")),
    "must be a WoodProperties R6 object"
  )
})


# =============================================================================
# Sap Flux Density Conversion Tests
# =============================================================================

test_that("calc_sap_flux_density() converts velocity to flux density", {
  # Setup wood properties with calculated Z factor
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood <- calculate_wood_properties(wood)

  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_equal(Z, 0.801, tolerance = 0.01)

  # Test conversion
  Vh <- 10.5  # cm/h
  Jv <- calc_sap_flux_density(Vh, wood)

  expect_equal(Jv, Vh * Z, tolerance = 0.001)
  expect_equal(Jv, 10.5 * 0.801, tolerance = 0.01)
})


test_that("calc_sap_flux_density() works with vectors", {
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood <- calculate_wood_properties(wood)

  Vh_vector <- c(5.2, 10.5, 15.8, 20.1)
  Jv_vector <- calc_sap_flux_density(Vh_vector, wood)

  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_equal(length(Jv_vector), 4)
  expect_equal(Jv_vector, Vh_vector * Z, tolerance = 0.001)
})


test_that("calc_sap_flux_density() errors if Z not calculated", {
  wood <- load_wood_properties("generic_sw")
  # Don't run calculate_wood_properties()

  expect_error(
    calc_sap_flux_density(10, wood),
    "Run calculate_wood_properties"
  )
})


test_that("apply_flux_conversion() adds Jv column to data frame", {
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood <- calculate_wood_properties(wood)

  # Create test data
  vh_data <- data.frame(
    datetime = seq(as.POSIXct("2025-01-01 00:00:00"),
                   by = "30 min", length.out = 10),
    Vc_cm_hr = c(5.2, 10.5, 15.8, 20.1, 18.5, 12.3, 8.7, 6.1, 4.5, 3.2)
  )

  # Apply conversion
  result <- apply_flux_conversion(vh_data, wood, velocity_col = "Vc_cm_hr")

  # Check Jv column added
  expect_true("Jv_cm3_cm2_hr" %in% names(result))
  expect_equal(nrow(result), 10)

  # Check values
  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_equal(result$Jv_cm3_cm2_hr, vh_data$Vc_cm_hr * Z, tolerance = 0.001)
})


test_that("apply_flux_conversion() accepts custom column names", {
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood <- calculate_wood_properties(wood)

  vh_data <- data.frame(
    datetime = seq(as.POSIXct("2025-01-01 00:00:00"), by = "hour", length.out = 5),
    my_velocity = c(5.2, 10.5, 15.8, 20.1, 18.5)
  )

  result <- apply_flux_conversion(
    vh_data,
    wood,
    velocity_col = "my_velocity",
    output_col = "my_flux"
  )

  expect_true("my_flux" %in% names(result))
  expect_false("Jv_cm3_cm2_hr" %in% names(result))

  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_equal(result$my_flux, vh_data$my_velocity * Z, tolerance = 0.001)
})


test_that("get_sap_flux_conversion_factor() extracts Z factor", {
  wood <- load_wood_properties("generic_sw")
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506
  wood <- calculate_wood_properties(wood)

  Z <- get_sap_flux_conversion_factor(wood)

  expect_type(Z, "double")
  expect_equal(Z, 0.801, tolerance = 0.01)
  expect_equal(Z, wood$derived_properties$sap_flux_conversion_factor)
})


test_that("get_sap_flux_conversion_factor() errors if Z not calculated", {
  wood <- load_wood_properties("generic_sw")

  expect_error(
    get_sap_flux_conversion_factor(wood),
    "Run calculate_wood_properties"
  )
})


# =============================================================================
# Wound Diameter Calculation Tests
# =============================================================================

test_that("calc_wound_diameter() calculates initial wound diameter correctly", {
  wood <- load_wood_properties("generic_sw")

  # Default: drill_bit = 2.0mm, wound_addition = 0.3mm
  # Initial wound = 2.0 + 2*0.3 = 2.6mm = 0.26cm

  timestamps <- seq(as.POSIXct("2025-01-01"), by = "day", length.out = 10)
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  # Without temporal tracking, should return constant diameter
  expect_equal(length(wound_diameters), 10)
  expect_equal(wound_diameters[1], 0.26, tolerance = 0.001)
  expect_equal(wound_diameters[10], 0.26, tolerance = 0.001)
  expect_true(all(wound_diameters == 0.26))
})


test_that("calc_wound_diameter() respects custom drill bit and wound addition", {
  wood <- load_wood_properties("generic_sw")

  # Custom: drill_bit = 2.5mm, wound_addition = 0.5mm
  # Initial wound = 2.5 + 2*0.5 = 3.5mm = 0.35cm
  wood$wound_correction$drill_bit_diameter_mm <- 2.5
  wood$wound_correction$wound_addition_mm <- 0.5

  timestamps <- seq(as.POSIXct("2025-01-01"), by = "day", length.out = 5)
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  expect_equal(wound_diameters[1], 0.35, tolerance = 0.001)
  expect_true(all(wound_diameters == 0.35))
})


test_that("calc_wound_diameter() performs linear interpolation with temporal tracking", {
  wood <- load_wood_properties("generic_sw")

  # Set temporal tracking parameters
  wood$wound_correction$drill_bit_diameter_mm <- 2.0
  wood$wound_correction$wound_addition_mm <- 0.3
  wood$wound_correction$initial_date <- "2025-01-01"
  wood$wound_correction$final_date <- "2025-01-11"
  wood$wound_correction$final_diameter_mm <- 3.6  # Grew from 2.6mm to 3.6mm over 10 days

  # Create timestamps at days 0, 5, 10
  timestamps <- as.POSIXct(c("2025-01-01", "2025-01-06", "2025-01-11"))
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  # Expected: 2.6mm at day 0, 3.1mm at day 5, 3.6mm at day 10
  expect_equal(wound_diameters[1], 0.26, tolerance = 0.01)  # Initial
  expect_equal(wound_diameters[2], 0.31, tolerance = 0.01)  # Midpoint
  expect_equal(wound_diameters[3], 0.36, tolerance = 0.01)  # Final
})


test_that("calc_wound_diameter() handles dates before initial date", {
  wood <- load_wood_properties("generic_sw")

  wood$wound_correction$initial_date <- "2025-01-10"
  wood$wound_correction$final_date <- "2025-01-20"
  wood$wound_correction$final_diameter_mm <- 3.6

  # Timestamps before initial date
  timestamps <- as.POSIXct(c("2025-01-05", "2025-01-08", "2025-01-15"))
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  # Should use initial diameter for dates before installation
  expect_equal(wound_diameters[1], 0.26, tolerance = 0.01)  # Before initial
  expect_equal(wound_diameters[2], 0.26, tolerance = 0.01)  # Before initial
  expect_gt(wound_diameters[3], 0.26)  # After initial, should be growing
})


test_that("calc_wound_diameter() handles dates after final date", {
  wood <- load_wood_properties("generic_sw")

  wood$wound_correction$initial_date <- "2025-01-01"
  wood$wound_correction$final_date <- "2025-01-10"
  wood$wound_correction$final_diameter_mm <- 3.6

  # Timestamps after final date
  timestamps <- as.POSIXct(c("2025-01-05", "2025-01-12", "2025-01-15"))
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  # Should use final diameter for dates after final measurement
  expect_gt(wound_diameters[1], 0.26)  # During wound period
  expect_equal(wound_diameters[2], 0.36, tolerance = 0.01)  # After final
  expect_equal(wound_diameters[3], 0.36, tolerance = 0.01)  # After final
})


test_that("calc_wound_diameter() accepts wound_config list directly", {
  wound_config <- list(
    drill_bit_diameter_mm = 2.5,
    wound_addition_mm = 0.4,
    initial_date = NULL,
    final_date = NULL,
    final_diameter_mm = NULL
  )

  timestamps <- seq(as.POSIXct("2025-01-01"), by = "day", length.out = 5)
  wound_diameters <- calc_wound_diameter(timestamps, wound_config)

  # Expected: 2.5 + 2*0.4 = 3.3mm = 0.33cm
  expect_equal(wound_diameters[1], 0.33, tolerance = 0.01)
  expect_equal(wound_diameters, rep(0.33, 5), tolerance = 0.01)
})


# =============================================================================
# Integration Tests
# =============================================================================

test_that("Complete workflow: load → calculate → convert to Jv works", {
  # Load wood properties
  wood <- load_wood_properties("generic_sw")

  # Set measurements
  wood$wood_measurements$fresh_weight_g <- 0.5493
  wood$wood_measurements$dry_weight_g <- 0.2641
  wood$wood_measurements$fresh_volume_cm3 <- 0.4506

  # Calculate derived properties
  wood <- calculate_wood_properties(wood)

  # Create test velocity data
  vh_data <- data.frame(
    datetime = seq(as.POSIXct("2025-01-01"), by = "hour", length.out = 24),
    Vc_cm_hr = rnorm(24, mean = 10, sd = 3)
  )

  # Convert to flux density
  result <- apply_flux_conversion(vh_data, wood)

  # Verify complete workflow
  expect_s3_class(result, "data.frame")
  expect_true("Jv_cm3_cm2_hr" %in% names(result))
  expect_equal(nrow(result), 24)

  # Verify conversion is correct
  Z <- wood$derived_properties$sap_flux_conversion_factor
  expect_equal(result$Jv_cm3_cm2_hr, vh_data$Vc_cm_hr * Z, tolerance = 0.001)
})


test_that("Wound correction with temporal tracking integration works", {
  wood <- load_wood_properties("generic_sw")

  # Set temporal wound tracking
  wood$wound_correction$drill_bit_diameter_mm <- 2.0
  wood$wound_correction$wound_addition_mm <- 0.3
  wood$wound_correction$initial_date <- "2025-01-01"
  wood$wound_correction$final_date <- "2025-01-10"
  wood$wound_correction$final_diameter_mm <- 3.6

  # Create timestamps
  timestamps <- seq(as.POSIXct("2025-01-01"),
                   as.POSIXct("2025-01-10"),
                   by = "day")

  # Calculate wound diameters
  wound_diameters <- calc_wound_diameter(timestamps, wood)

  # Verify interpolation
  expect_equal(length(wound_diameters), 10)
  expect_equal(wound_diameters[1], 0.26, tolerance = 0.01)  # Initial
  expect_equal(wound_diameters[10], 0.36, tolerance = 0.01)  # Final
  expect_true(all(diff(wound_diameters) >= 0))  # Should be increasing
})
