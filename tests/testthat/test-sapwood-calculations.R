# ==============================================================================
# Test file for sapwood calculations functions
# Tests for R/12_sapwood_area.R and R/13_tree_water_use.R
# ==============================================================================

library(testthat)

# Test calc_sapwood_area function =============================================

test_that("calc_sapwood_area works with complete manual measurements", {
  result <- calc_sapwood_area(
    diameter_breast_height = 20.0,
    bark_thickness = 1.0,
    heartwood_diameter = 8.0
  )

  expect_s3_class(result, "sapwood_area")
  expect_type(result, "list")

  # Check structure
  expected_names <- c("stem_measurements", "sapwood_measurements",
                      "probe_measurements", "area_summary", "estimation_notes")
  expect_true(all(expected_names %in% names(result)))

  # Check calculated values
  expect_equal(result$stem_measurements$diameter_breast_height, 20.0)
  expect_equal(result$stem_measurements$bark_thickness, 1.0)
  expect_equal(result$sapwood_measurements$heartwood_radius, 4.0)
  expect_equal(result$stem_measurements$cambium_radius, 9.0) # (20/2) - 1.0

  # Sapwood area should be positive
  expect_true(result$area_summary$total_sapwood_area_cm2 > 0)
  expect_true(result$area_summary$heartwood_area_cm2 > 0)
})

test_that("calc_sapwood_area works with automatic bark thickness estimation", {
  result <- calc_sapwood_area(
    diameter_breast_height = 25.0,
    heartwood_diameter = 10.0,
    species_group = "eucalyptus"
  )

  expect_s3_class(result, "sapwood_area")

  # Should have estimated bark thickness - check for full phrase in notes
  expect_true(any(grepl("Bark thickness estimated", result$estimation_notes)))
  expect_true(result$stem_measurements$bark_thickness > 0)
  expect_equal(result$stem_measurements$diameter_breast_height, 25.0)
})

test_that("calc_sapwood_area works with automatic heartwood estimation", {
  result <- calc_sapwood_area(
    diameter_breast_height = 18.0,
    bark_thickness = 0.8,
    species_group = "pine",
    estimation_method = "conservative"
  )

  expect_s3_class(result, "sapwood_area")

  # Should have estimated heartwood - check for full phrase in notes
  expect_true(any(grepl("Heartwood radius estimated", result$estimation_notes)))
  expect_true(result$sapwood_measurements$heartwood_radius > 0)
  expect_equal(result$stem_measurements$bark_thickness, 0.8)
})

test_that("calc_sapwood_area works with sapwood thickness input", {
  result <- calc_sapwood_area(
    diameter_breast_height = 22.0,
    sapwood_thickness = 4.5
  )

  expect_s3_class(result, "sapwood_area")
  expect_equal(result$area_summary$sapwood_thickness_cm, 4.5)

  # Check that heartwood radius was calculated correctly
  # cambium_radius - sapwood_thickness should equal heartwood_radius
  expected_heartwood_radius <- result$stem_measurements$cambium_radius - 4.5
  expect_equal(result$sapwood_measurements$heartwood_radius, expected_heartwood_radius)
})

test_that("calc_sapwood_area handles different species groups", {
  species_groups <- c("eucalyptus", "pine", "softwood", "hardwood")

  for (species in species_groups) {
    result <- calc_sapwood_area(
      diameter_breast_height = 20.0,
      species_group = species
    )

    expect_s3_class(result, "sapwood_area")
    expect_true(result$area_summary$total_sapwood_area_cm2 > 0)
  }
})

test_that("calc_sapwood_area validates input parameters", {
  # Test invalid DBH
  expect_error(
    calc_sapwood_area(diameter_breast_height = -5),
    "must be a positive number"
  )

  expect_error(
    calc_sapwood_area(diameter_breast_height = "invalid"),
    "must be a positive number"
  )

  # Test invalid estimation method
  expect_error(
    calc_sapwood_area(diameter_breast_height = 20, estimation_method = "invalid"),
    "must be one of: allometric, conservative, species_specific"
  )

  # Test warning for unusual DBH
  expect_warning(
    calc_sapwood_area(diameter_breast_height = 250),
    "seems unusual"
  )
})

test_that("calc_sapwood_area handles conflicting parameters appropriately", {
  # When both heartwood_radius and heartwood_diameter provided, radius should take precedence
  result <- calc_sapwood_area(
    diameter_breast_height = 20.0,
    bark_thickness = 1.0,
    heartwood_radius = 3.0,
    heartwood_diameter = 8.0  # This should be ignored
  )

  expect_equal(result$sapwood_measurements$heartwood_radius, 3.0)
})

# Test helper functions ======================================================

test_that("estimate_bark_thickness produces reasonable values", {
  # Test different species and methods
  species_groups <- c("eucalyptus", "pine", "softwood", "hardwood")
  methods <- c("allometric", "conservative")

  for (species in species_groups) {
    for (method in methods) {
      thickness <- sapFluxR:::estimate_bark_thickness(20.0, species, method)

      expect_type(thickness, "double")
      expect_true(thickness > 0)
      expect_true(thickness < 10) # Should be reasonable for 20cm DBH
    }
  }
})

test_that("estimate_heartwood_radius produces reasonable values", {
  species_groups <- c("eucalyptus", "pine", "softwood", "hardwood")
  methods <- c("allometric", "conservative")

  for (species in species_groups) {
    for (method in methods) {
      heartwood_r <- sapFluxR:::estimate_heartwood_radius(9.0, species, method) # 9cm cambium radius

      expect_type(heartwood_r, "double")
      expect_true(heartwood_r >= 0)
      expect_true(heartwood_r < 8.0) # Should leave at least 1cm sapwood
    }
  }
})

test_that("estimate_heartwood_radius ensures minimum sapwood thickness", {
  # Test with small cambium radius
  heartwood_r <- sapFluxR:::estimate_heartwood_radius(2.0, "hardwood", "allometric")

  # Should ensure at least 1cm sapwood thickness
  expect_true((2.0 - heartwood_r) >= 1.0)
})

# Test print.sapwood_area method ==============================================

test_that("print method for sapwood_area works correctly", {
  result <- calc_sapwood_area(
    diameter_breast_height = 20.0,
    bark_thickness = 1.0,
    heartwood_diameter = 8.0
  )

  # Test that print method runs without error and produces output
  expect_output(print(result), "Sapwood Area Calculations")
  expect_output(print(result), "Tree Measurements:")
  expect_output(print(result), "Areas:")
  expect_output(print(result), "DBH:")
  expect_output(print(result), "Total sapwood area:")
})

# Test calc_tree_sap_flux function ===========================================

# Create better sample data for flux tests (matching datetime between sensors)
create_sample_flux_data <- function() {
  # Create matching timestamps for both sensors
  timestamps <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + 0:11 * 3600

  data.frame(
    datetime = rep(timestamps, 2),
    sensor_position = rep(c("outer", "inner"), each = 12),
    Jv_cm3_cm2_hr = c(
      runif(12, 5, 15),  # outer sensor values
      runif(12, 3, 8)    # inner sensor values
    ),
    tree_id = rep("T001", 24)
  )
}

create_sample_sapwood_data <- function() {
  calc_sapwood_area(
    diameter_breast_height = 20.0,
    bark_thickness = 1.0,
    heartwood_diameter = 8.0
  )
}

test_that("calc_tree_sap_flux works with outer_only scaling", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  result <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "outer_only",
    flux_units = "L_hr"
  )

  expect_s3_class(result, "tree_sap_flux")
  expect_s3_class(result, "data.frame")

  # Check required columns
  expect_true("tree_flux" %in% names(result))
  expect_true("flux_units" %in% names(result))
  expect_true("scaling_method" %in% names(result))
  expect_true("sensor_coverage" %in% names(result))
  expect_true("extrapolation_factor" %in% names(result))

  # Check values
  expect_equal(unique(result$flux_units), "L_hr")
  expect_equal(unique(result$scaling_method), "outer_only")
  expect_true(all(result$tree_flux > 0))

  # Should only contain outer sensor data
  expect_true(all(result$sensor_position == "outer"))
})

test_that("calc_tree_sap_flux works with inner_only scaling", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  result <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "inner_only",
    flux_units = "L_hr"
  )

  expect_s3_class(result, "tree_sap_flux")
  expect_equal(unique(result$scaling_method), "inner_only")
  expect_true(all(result$tree_flux > 0))

  # Should only contain inner sensor data
  expect_true(all(result$sensor_position == "inner"))
})

test_that("calc_tree_sap_flux works with weighted_average scaling", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  result <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "weighted_average",
    flux_units = "L_hr"
  )

  expect_s3_class(result, "tree_sap_flux")
  expect_equal(unique(result$scaling_method), "weighted_average")
  expect_true(all(result$tree_flux > 0))

  # Should have merged data from both sensors
  expect_true("Jv_cm3_cm2_hr" %in% names(result))
})

test_that("calc_tree_sap_flux works with radial_profile scaling", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  result <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "radial_profile",
    flux_units = "L_hr"
  )

  expect_s3_class(result, "tree_sap_flux")
  expect_equal(unique(result$scaling_method), "radial_profile")
  expect_true(all(result$tree_flux > 0))
})

test_that("calc_tree_sap_flux handles different units correctly", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  # Test all unit types
  units <- c("L_hr", "L_day", "kg_hr", "kg_day")

  for (unit in units) {
    result <- calc_tree_sap_flux(
      flux_data,
      sapwood_data,
      scaling_method = "outer_only",
      flux_units = unit
    )

    expect_equal(unique(result$flux_units), unit)
    expect_true(all(result$tree_flux > 0))

    # L_day should be 24x larger than L_hr
    if (unit == "L_day") {
      result_hr <- calc_tree_sap_flux(
        flux_data, sapwood_data, scaling_method = "outer_only", flux_units = "L_hr"
      )
      expect_equal(result$tree_flux[1], result_hr$tree_flux[1] * 24, tolerance = 1e-6)
    }
  }
})

test_that("calc_tree_sap_flux validates input parameters", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  # Test invalid flux_results
  expect_error(
    calc_tree_sap_flux("invalid", sapwood_data),
    "must be a data frame"
  )

  # Test missing Jv_cm3_cm2_hr column
  bad_flux_data <- flux_data[, !names(flux_data) %in% "Jv_cm3_cm2_hr"]
  expect_error(
    calc_tree_sap_flux(bad_flux_data, sapwood_data),
    "must contain 'Jv_cm3_cm2_hr' column"
  )

  # Test invalid sapwood_area_data
  expect_error(
    calc_tree_sap_flux(flux_data, "invalid"),
    "must be from calc_sapwood_area"
  )

  # Test invalid scaling method
  expect_error(
    calc_tree_sap_flux(flux_data, sapwood_data, scaling_method = "invalid"),
    "must be one of"
  )

  # Test invalid flux units
  expect_error(
    calc_tree_sap_flux(flux_data, sapwood_data, flux_units = "invalid"),
    "must be one of"
  )
})

test_that("calc_tree_sap_flux handles missing sensor data gracefully", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  # Test with only outer sensor data
  outer_only_data <- flux_data[flux_data$sensor_position == "outer", ]

  expect_warning(
    result <- calc_tree_sap_flux(
      outer_only_data,
      sapwood_data,
      scaling_method = "weighted_average"
    ),
    "Falling back to 'outer_only'"
  )

  expect_equal(unique(result$scaling_method), "outer_only")
})

test_that("calc_tree_sap_flux fails appropriately when required data missing", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  # Test with no outer sensor data for outer_only method
  inner_only_data <- flux_data[flux_data$sensor_position == "inner", ]

  expect_error(
    calc_tree_sap_flux(inner_only_data, sapwood_data, scaling_method = "outer_only"),
    "No outer sensor data available"
  )

  # Test with no inner sensor data for inner_only method
  outer_only_data <- flux_data[flux_data$sensor_position == "outer", ]

  expect_error(
    calc_tree_sap_flux(outer_only_data, sapwood_data, scaling_method = "inner_only"),
    "No inner sensor data available"
  )
})

test_that("calc_tree_sap_flux handles water density parameter", {
  flux_data <- create_sample_flux_data()
  sapwood_data <- create_sample_sapwood_data()

  # Test with custom water density
  result <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "outer_only",
    flux_units = "kg_hr",
    water_density = 0.95
  )

  expect_true(all(result$tree_flux > 0))

  # Compare with default density
  result_default <- calc_tree_sap_flux(
    flux_data,
    sapwood_data,
    scaling_method = "outer_only",
    flux_units = "kg_hr",
    water_density = 1.0
  )

  # Should be proportional to density ratio
  expect_equal(result$tree_flux[1], result_default$tree_flux[1] * 0.95, tolerance = 1e-6)
})

# Integration tests ===========================================================

test_that("sapwood calculation workflow integrates correctly", {
  # Test complete workflow from tree measurements to flux scaling

  # Step 1: Calculate sapwood area
  sapwood_data <- calc_sapwood_area(
    diameter_breast_height = 22.5,
    bark_thickness = 1.2,
    heartwood_diameter = 9.0
  )

  expect_s3_class(sapwood_data, "sapwood_area")

  # Step 2: Create flux data (simulated) with matching timestamps
  timestamps <- as.POSIXct("2024-01-01 08:00:00", tz = "UTC") + 0:23 * 1800  # 30min intervals

  flux_data <- data.frame(
    datetime = rep(timestamps, 2),
    sensor_position = rep(c("outer", "inner"), each = 24),
    Jv_cm3_cm2_hr = c(
      10 * sin(seq(0, 2*pi, length.out = 24)) + 12,  # Diurnal pattern outer
      6 * sin(seq(0, 2*pi, length.out = 24)) + 8     # Diurnal pattern inner
    ),
    tree_id = rep("Test_Tree", 48),
    stringsAsFactors = FALSE
  )

  # Step 3: Scale to tree level with different methods
  scaling_methods <- c("outer_only", "weighted_average", "radial_profile")

  results <- list()
  for (method in scaling_methods) {
    results[[method]] <- calc_tree_sap_flux(
      flux_data,
      sapwood_data,
      scaling_method = method,
      flux_units = "L_day"
    )

    expect_s3_class(results[[method]], "tree_sap_flux")
    expect_equal(unique(results[[method]]$scaling_method), method)
    expect_true(all(results[[method]]$tree_flux > 0))
  }

  # Different methods should give different results
  expect_false(identical(results$outer_only$tree_flux, results$weighted_average$tree_flux))
  expect_false(identical(results$weighted_average$tree_flux, results$radial_profile$tree_flux))
})

# Edge cases and robustness tests ============================================

test_that("sapwood calculations handle edge cases", {
  # Very small tree
  result_small <- calc_sapwood_area(diameter_breast_height = 8.0)
  expect_s3_class(result_small, "sapwood_area")
  expect_true(result_small$area_summary$total_sapwood_area_cm2 > 0)

  # Large tree
  result_large <- calc_sapwood_area(diameter_breast_height = 80.0)
  expect_s3_class(result_large, "sapwood_area")
  expect_true(result_large$area_summary$total_sapwood_area_cm2 > result_small$area_summary$total_sapwood_area_cm2)
})

test_that("flux calculations handle single sensor scenarios", {
  sapwood_data <- create_sample_sapwood_data()

  # Only outer sensor
  flux_outer <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + 0:11 * 3600,
    sensor_position = rep("outer", 12),
    Jv_cm3_cm2_hr = runif(12, 5, 15),
    tree_id = rep("T001", 12)
  )

  result <- calc_tree_sap_flux(flux_outer, sapwood_data, scaling_method = "outer_only")
  expect_s3_class(result, "tree_sap_flux")
  expect_true(all(result$tree_flux > 0))
})