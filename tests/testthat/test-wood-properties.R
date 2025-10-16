# Tests for Wood Property Configuration System

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
  expect_equal(wood$wood_type, "softwood")
  expect_equal(wood$thermal_diffusivity, 0.0025)
})


test_that("load_wood_properties() loads specific configurations", {
  # Load eucalyptus
  euc_wood <- load_wood_properties("eucalyptus")
  expect_s3_class(euc_wood, "WoodProperties")
  expect_match(euc_wood$config_name, "Eucalyptus", ignore.case = TRUE)
  expect_equal(euc_wood$species, "Eucalyptus spp.")
  expect_equal(euc_wood$wood_type, "hardwood")
  expect_equal(euc_wood$thermal_diffusivity, 0.00143)

  # Load pine
  pine_wood <- load_wood_properties("pine")
  expect_s3_class(pine_wood, "WoodProperties")
  expect_match(pine_wood$config_name, "Pine", ignore.case = TRUE)
  expect_equal(pine_wood$species, "Pinus spp.")
  expect_equal(pine_wood$wood_type, "softwood")
  expect_equal(pine_wood$thermal_diffusivity, 0.00145)
})


test_that("get_default_wood_properties() works", {
  wood <- get_default_wood_properties()

  expect_s3_class(wood, "WoodProperties")
  expect_match(wood$config_name, "Softwood", ignore.case = TRUE)
  expect_equal(wood$thermal_diffusivity, 0.0025)
})


test_that("load_wood_properties() respects overrides", {
  # Override thermal_diffusivity
  wood <- load_wood_properties("eucalyptus",
                               overrides = list(thermal_diffusivity = 0.003))

  expect_equal(wood$thermal_diffusivity, 0.003)
  # Other properties should remain unchanged
  expect_equal(wood$species, "Eucalyptus spp.")
  expect_equal(wood$wood_type, "hardwood")

  # Override multiple properties
  wood2 <- load_wood_properties("generic_sw",
                                overrides = list(
                                  thermal_diffusivity = 0.0028,
                                  moisture_content = 35,
                                  dry_density = 500
                                ))

  expect_equal(wood2$thermal_diffusivity, 0.0028)
  expect_equal(wood2$moisture_content, 35)
  expect_equal(wood2$dry_density, 500)
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
                               overrides = list(thermal_diffusivity = 0.003),
                               tree_overrides = list(dbh = 50, sapwood_area = 200))

  expect_equal(wood$thermal_diffusivity, 0.003)
  expect_equal(wood$tree_measurements$dbh, 50)
  expect_equal(wood$tree_measurements$sapwood_area, 200)
})


test_that("create_custom_wood_properties() creates valid configuration", {
  custom <- create_custom_wood_properties(
    config_name = "My Custom Wood",
    species = "Pinus radiata",
    thermal_diffusivity = 0.0028,
    dry_density = 450,
    moisture_content = 35,
    dbh = 45.2,
    sapwood_depth = 3.5
  )

  expect_s3_class(custom, "WoodProperties")
  expect_equal(custom$config_name, "My Custom Wood")
  expect_equal(custom$species, "Pinus radiata")
  expect_equal(custom$thermal_diffusivity, 0.0028)
  expect_equal(custom$dry_density, 450)
  expect_equal(custom$moisture_content, 35)
  expect_equal(custom$tree_measurements$dbh, 45.2)
  expect_equal(custom$tree_measurements$sapwood_depth, 3.5)
})


test_that("WoodProperties object has required fields", {
  wood <- load_wood_properties("eucalyptus")

  # Check thermal properties exist
  expect_false(is.null(wood$thermal_diffusivity))
  expect_false(is.null(wood$thermal_conductivity))
  expect_false(is.null(wood$volumetric_heat_capacity))

  # Check physical properties
  expect_false(is.null(wood$dry_density))
  expect_false(is.null(wood$moisture_content))

  # Check classification
  expect_false(is.null(wood$species))
  expect_false(is.null(wood$wood_type))

  # Check tree measurements structure
  expect_type(wood$tree_measurements, "list")
  expect_true("dbh" %in% names(wood$tree_measurements))
  expect_true("sapwood_depth" %in% names(wood$tree_measurements))

  # Check quality thresholds structure
  expect_type(wood$quality_thresholds, "list")
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


test_that("load_wood_properties() errors on invalid configuration name", {
  expect_error(
    load_wood_properties("nonexistent_wood_type"),
    "not found"
  )

  expect_error(
    load_wood_properties("invalid_config"),
    "Available configurations"
  )
})


test_that("load_wood_properties() errors on invalid file path", {
  expect_error(
    load_wood_properties("path/to/nonexistent/file.yaml"),
    "not found"
  )
})


test_that("WoodProperties validation catches issues", {
  # Should warn about unusual diffusivity value
  expect_warning(
    create_custom_wood_properties(thermal_diffusivity = 0.1),  # Way too high
    "outside typical range"
  )

  # Should error if diffusivity is NULL
  expect_error(
    WoodProperties$new(thermal_diffusivity = NULL),
    "thermal_diffusivity is required"
  )

  # Should error if diffusivity is not numeric
  expect_error(
    WoodProperties$new(thermal_diffusivity = "not_a_number"),
    "must be numeric"
  )
})


test_that("WoodProperties print method works", {
  wood <- load_wood_properties("eucalyptus")

  # Should not error when printing
  expect_output(print(wood), "Wood Properties Configuration")
  expect_output(print(wood), "Eucalyptus")
  expect_output(print(wood), "Thermal Properties")
  expect_output(print(wood), "thermal diffusivity", ignore.case = TRUE)
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


test_that("Custom wood properties without tree measurements works", {
  custom <- create_custom_wood_properties(
    species = "Test species",
    thermal_diffusivity = 0.0025
    # Don't specify any tree measurements
  )

  expect_s3_class(custom, "WoodProperties")
  expect_null(custom$tree_measurements$dbh)
  expect_null(custom$tree_measurements$sapwood_depth)
})


test_that("Quality thresholds have sensible defaults", {
  custom <- create_custom_wood_properties(
    thermal_diffusivity = 0.0025
  )

  # Should have default quality thresholds
  expect_equal(custom$quality_thresholds$max_velocity_cm_hr, 200)
  expect_equal(custom$quality_thresholds$min_velocity_cm_hr, -50)
  expect_equal(custom$quality_thresholds$temperature_range, c(-10, 60))
})


test_that("Quality thresholds can be customized", {
  custom <- create_custom_wood_properties(
    thermal_diffusivity = 0.0025,
    max_velocity_cm_hr = 150,
    min_velocity_cm_hr = -30,
    temperature_range = c(0, 40)
  )

  expect_equal(custom$quality_thresholds$max_velocity_cm_hr, 150)
  expect_equal(custom$quality_thresholds$min_velocity_cm_hr, -30)
  expect_equal(custom$quality_thresholds$temperature_range, c(0, 40))
})
