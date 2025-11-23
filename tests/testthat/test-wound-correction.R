# tests/testthat/test-wound-correction.R
# Tests for wound correction functionality

test_that("wound coefficient lookup tables are valid", {

 # Check 5mm table structure
 expect_true(exists("wound_coefficients_5mm", envir = asNamespace("sapfluxr")))

 coef_5mm <- sapfluxr:::wound_coefficients_5mm
 expect_s3_class(coef_5mm, "data.frame")
 expect_true(all(c("wound_diameter_cm", "b", "c", "d", "B_linear", "r_squared") %in% names(coef_5mm)))
 expect_equal(nrow(coef_5mm), 11)

 # Check values are reasonable
 expect_true(all(coef_5mm$wound_diameter_cm >= 0.15 & coef_5mm$wound_diameter_cm <= 0.35))
 expect_true(all(coef_5mm$B_linear >= 1.5 & coef_5mm$B_linear <= 3.0))
 expect_true(all(coef_5mm$r_squared >= 0.99))

 # Check 6mm table
 coef_6mm <- sapfluxr:::wound_coefficients_6mm
 expect_s3_class(coef_6mm, "data.frame")
 expect_equal(nrow(coef_6mm), 11)
})


test_that("get_wound_correction_coefficient returns correct values", {

 # Exact match from table (5mm, 0.20 cm wound)
 B <- sapfluxr:::get_wound_correction_coefficient(0.20, "5mm")
 expect_equal(B, 1.9216)

 # Exact match from 6mm table
 B_6mm <- sapfluxr:::get_wound_correction_coefficient(0.20, "6mm")
 expect_equal(B_6mm, 1.8905)

 # Interpolation between table values
 B_interp <- sapfluxr:::get_wound_correction_coefficient(0.195, "5mm")
 expect_true(B_interp > 1.8568 && B_interp < 1.9216)  # Between 0.19 and 0.20

 # Edge cases - should warn and return boundary values
 expect_warning(
   B_low <- sapfluxr:::get_wound_correction_coefficient(0.10, "5mm"),
   "below table minimum"
 )
 expect_equal(B_low, min(sapfluxr:::wound_coefficients_5mm$B_linear))

 expect_warning(
   B_high <- sapfluxr:::get_wound_correction_coefficient(0.40, "5mm"),
   "above table maximum"
 )
 expect_equal(B_high, max(sapfluxr:::wound_coefficients_5mm$B_linear))
})


test_that("apply_wound_correction validates inputs", {

 # Create minimal test data
 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 10),
   Vh_cm_hr = c(5, 10, 15, 20, 25, 20, 15, 10, 5, 0),
   method = "HRM",
   sensor_position = "outer"
 )

 # Should error without wound_diameter in non-interactive mode
 expect_error(
   apply_wound_correction(test_data, confirm_parameters = FALSE),
   "wound_diameter must be specified"
 )

 # Should error with invalid data type
 expect_error(
   apply_wound_correction("not a data frame", wound_diameter = 0.20, confirm_parameters = FALSE),
   "must be a data frame"
 )

 # Should error with missing required columns
 bad_data <- data.frame(x = 1:10)
 expect_error(
   apply_wound_correction(bad_data, wound_diameter = 0.20, confirm_parameters = FALSE),
   "missing required columns"
 )

 # Should error with invalid probe_spacing
 expect_error(
   apply_wound_correction(test_data, wound_diameter = 0.20, probe_spacing = "7mm",
                          confirm_parameters = FALSE),
   "must be '5mm' or '6mm'"
 )
})


test_that("apply_wound_correction applies linear correction correctly", {

 # Create test data
 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5),
   Vh_cm_hr = c(0, 10, 20, 30, 40),
   method = "HRM",
   sensor_position = "outer"
 )

 # Apply correction with known coefficient
 # B for 0.20 cm wound at 5mm spacing = 1.9216
 result <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   probe_spacing = "5mm",
   confirm_parameters = FALSE
 )

 # Check output columns exist
 expect_true("Vc_cm_hr" %in% names(result))
 expect_true("wound_correction_factor" %in% names(result))
 expect_true("wound_diameter_cm" %in% names(result))

 # Check correction was applied correctly: Vc = B * Vh
 expected_B <- 1.9216
 expect_equal(result$wound_correction_factor[1], expected_B)
 expect_equal(result$Vc_cm_hr, test_data$Vh_cm_hr * expected_B)

 # Check wound diameter is stored
 expect_equal(unique(result$wound_diameter_cm), 0.20)
})


test_that("apply_wound_correction uses spacing-corrected velocities when available", {

 # Create test data with both raw and spacing-corrected columns
 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5),
   Vh_cm_hr = c(10, 20, 30, 40, 50),
   Vh_cm_hr_sc = c(12, 24, 36, 48, 60),  # Spacing-corrected values
   method = "HRM",
   sensor_position = "outer"
 )

 # Default should use spacing-corrected
 result <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   confirm_parameters = FALSE
 )

 expected_B <- 1.9216
 expect_equal(result$Vc_cm_hr, test_data$Vh_cm_hr_sc * expected_B)

 # Can force use of raw values
 result_raw <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   use_spacing_corrected = FALSE,
   confirm_parameters = FALSE
 )

 expect_equal(result_raw$Vc_cm_hr, test_data$Vh_cm_hr * expected_B)
})


test_that("apply_wound_correction handles different probe spacings", {

 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 3),
   Vh_cm_hr = c(10, 20, 30),
   method = "HRM",
   sensor_position = "outer"
 )

 # 5mm spacing
 result_5mm <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   probe_spacing = "5mm",
   confirm_parameters = FALSE
 )

 # 6mm spacing
 result_6mm <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   probe_spacing = "6mm",
   confirm_parameters = FALSE
 )

 # 5mm should have slightly higher correction factor than 6mm
 expect_true(result_5mm$wound_correction_factor[1] > result_6mm$wound_correction_factor[1])

 # Both should increase velocities
 expect_true(all(result_5mm$Vc_cm_hr > test_data$Vh_cm_hr))
 expect_true(all(result_6mm$Vc_cm_hr > test_data$Vh_cm_hr))
})


test_that("apply_wound_correction warns for unusual wound diameters", {

 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 3),
   Vh_cm_hr = c(10, 20, 30),
   method = "HRM",
   sensor_position = "outer"
 )

 # Very small wound diameter
 expect_warning(
   apply_wound_correction(test_data, wound_diameter = 0.10, confirm_parameters = FALSE),
   "outside typical range"
 )

 # Very large wound diameter
 expect_warning(
   apply_wound_correction(test_data, wound_diameter = 0.40, confirm_parameters = FALSE),
   "outside typical range"
 )
})


test_that("list_wound_coefficients returns expected output", {

 # Should run without error
 expect_silent(capture.output(result <- list_wound_coefficients("5mm")))
 expect_s3_class(result, "data.frame")

 # Both tables
 expect_silent(capture.output(result_both <- list_wound_coefficients("both")))
 expect_type(result_both, "list")
 expect_true(all(c("5mm", "6mm") %in% names(result_both)))
})


test_that("get_wound_diameter_from_config extracts from wood_properties", {

 # Skip if load_wood_properties not available
 skip_if_not(exists("load_wood_properties", envir = asNamespace("sapfluxr")))

 # Load a wood properties config that should have wound_diameter
 wp <- load_wood_properties("generic_sw")

 # Extract wound diameter
 wd <- sapfluxr:::get_wound_diameter_from_config(wp)

 # Should return the value from YAML (0.20 as we set it)
 expect_equal(wd, 0.20)

 # Should also work with config name string
 wd_string <- sapfluxr:::get_wound_diameter_from_config("eucalyptus")
 expect_equal(wd_string, 0.20)
})


test_that("wound correction preserves all original columns", {

 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5),
   Vh_cm_hr = c(10, 20, 30, 40, 50),
   method = "HRM",
   sensor_position = "outer",
   pulse_id = 1:5,
   quality_flag = "OK",
   custom_column = letters[1:5]
 )

 result <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   confirm_parameters = FALSE
 )

 # All original columns should be preserved
 expect_true(all(names(test_data) %in% names(result)))

 # Plus the new columns
 expect_true(all(c("Vc_cm_hr", "wound_correction_factor", "wound_diameter_cm") %in% names(result)))
})


test_that("wound correction handles NA values correctly", {

 test_data <- data.frame(
   datetime = seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5),
   Vh_cm_hr = c(10, NA, 30, NA, 50),
   method = "HRM",
   sensor_position = "outer"
 )

 result <- apply_wound_correction(
   test_data,
   wound_diameter = 0.20,
   confirm_parameters = FALSE
 )

 # NAs should propagate
 expect_true(is.na(result$Vc_cm_hr[2]))
 expect_true(is.na(result$Vc_cm_hr[4]))

 # Non-NA values should be corrected
 expect_false(is.na(result$Vc_cm_hr[1]))
 expect_false(is.na(result$Vc_cm_hr[3]))
 expect_false(is.na(result$Vc_cm_hr[5]))
})
