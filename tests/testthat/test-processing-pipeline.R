# Tests for Processing Pipeline Functions
# Source file: R/14_processing_pipeline.R
# Testing functions: process_sap_data, assess_data_quality, recommend_methods

library(testthat)

# Test helper function to create mock data
create_mock_sap_data <- function() {
  measurements <- data.frame(
    pulse_id = rep(1:3, each = 120),
    datetime = rep(seq(as.POSIXct("2024-01-01 10:00:00"), by = "sec", length.out = 120), 3),
    do = rep(c(18.8, 19.5, 18.9), each = 120) + rnorm(360, 0, 0.01),
    di = rep(c(18.7, 19.2, 18.8), each = 120) + rnorm(360, 0, 0.01),
    uo = rep(c(18.8, 19.1, 18.8), each = 120) + rnorm(360, 0, 0.01),
    ui = rep(c(18.7, 19.0, 18.7), each = 120) + rnorm(360, 0, 0.01)
  )

  diagnostics <- data.frame(
    pulse_id = 1:3,
    datetime = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:30:00", "2024-01-01 11:00:00")),
    batt_volt = c(4.1, 4.0, 4.1),
    batt_current = c(15, 15, 15),
    batt_temp = c(25, 26, 25),
    external_volt = c(23, 23, 23),
    external_current = c(15, 15, 15)
  )

  sap_data <- list(
    measurements = measurements,
    diagnostics = diagnostics,
    metadata = list(
      format = "test_format",
      file_path = "test_data.txt",
      import_time = Sys.time(),
      n_pulses = 3
    )
  )

  class(sap_data) <- "sap_data"
  return(sap_data)
}

# Mock probe config helper
create_mock_probe_config <- function() {
  list(
    name = "three_probe_symmetric",
    compatible_methods = c("HRM", "MHR", "DMA", "Tmax_Klu", "Tmax_Coh", "HRMXa", "HRMXb")
  )
}

# Tests for assess_data_quality ----
test_that("assess_data_quality basic functionality", {
  mock_sap_data <- create_mock_sap_data()
  mock_probe_config <- create_mock_probe_config()

  result <- assess_data_quality(mock_sap_data, mock_probe_config)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("overall_score", "component_scores", "issues", "recommendations", "diagnostics"))

  # Check that overall score is numeric and within range
  expect_type(result$overall_score, "double")
  expect_true(result$overall_score >= 0 && result$overall_score <= 100)

  # Check component scores
  expect_type(result$component_scores, "list")
  expect_true(all(sapply(result$component_scores, function(x) is.numeric(x) && x >= 0 && x <= 100)))

  # Check that we have issues and recommendations as character vectors
  expect_type(result$issues, "character")
  expect_type(result$recommendations, "character")
})

test_that("assess_data_quality handles missing sensors", {
  mock_sap_data <- create_mock_sap_data()
  # Remove one sensor
  mock_sap_data$measurements$do <- NULL
  mock_probe_config <- create_mock_probe_config()

  result <- assess_data_quality(mock_sap_data, mock_probe_config)

  # Should still return valid structure
  expect_type(result, "list")
  expect_true(result$overall_score >= 0 && result$overall_score <= 100)

  # Should identify missing sensor
  expect_true(any(grepl("Missing sensors", result$issues)))
})

test_that("assess_data_quality handles poor quality data", {
  mock_sap_data <- create_mock_sap_data()
  # Add high noise to make poor quality
  mock_sap_data$measurements$do <- mock_sap_data$measurements$do + rnorm(nrow(mock_sap_data$measurements), 0, 2.0)
  mock_probe_config <- create_mock_probe_config()

  result <- assess_data_quality(mock_sap_data, mock_probe_config)

  expect_type(result, "list")
  # High noise should result in lower quality score
  expect_true(result$overall_score < 90)
})

# Tests for recommend_methods ----
test_that("recommend_methods functionality", {
  mock_sap_data <- create_mock_sap_data()
  mock_probe_config <- create_mock_probe_config()

  # Test with different quality scores
  mock_quality_high <- list(overall_score = 90)
  mock_quality_medium <- list(overall_score = 75)
  mock_quality_low <- list(overall_score = 40)

  result_high <- recommend_methods(mock_sap_data, mock_probe_config, mock_quality_high)
  result_medium <- recommend_methods(mock_sap_data, mock_probe_config, mock_quality_medium)
  result_low <- recommend_methods(mock_sap_data, mock_probe_config, mock_quality_low)

  # Check structure
  expect_named(result_high, c("recommended_methods", "recommended_parameters", "rationale", "quality_based", "configuration_compatible"))

  # Check that different quality scores give recommendations
  expect_true(length(result_high$recommended_methods) > 0)
  expect_true(length(result_medium$recommended_methods) > 0)
  expect_true(length(result_low$recommended_methods) > 0)

  # All should be configuration compatible
  expect_true(result_high$configuration_compatible)
  expect_true(result_medium$configuration_compatible)
  expect_true(result_low$configuration_compatible)

  # Parameters should be a list with expected elements
  expect_type(result_high$recommended_parameters, "list")
  expect_true("diffusivity" %in% names(result_high$recommended_parameters))
  expect_true("x" %in% names(result_high$recommended_parameters))
})

test_that("recommend_methods handles edge cases", {
  mock_sap_data <- create_mock_sap_data()

  # Test with limited compatible methods
  limited_config <- list(
    name = "limited_config",
    compatible_methods = c("MHR")  # Only one method
  )
  mock_quality <- list(overall_score = 80)

  result <- recommend_methods(mock_sap_data, limited_config, mock_quality)

  # Should still return valid recommendations
  expect_true(length(result$recommended_methods) > 0)
  expect_true("MHR" %in% result$recommended_methods)
})

# Tests for process_sap_data ----
test_that("process_sap_data integration test basic structure", {
  # This test checks the structure without full integration
  # since we don't have all the existing functions in the test environment

  expect_error(process_sap_data("not_sap_data"), "Input must be a sap_data object")
})

test_that("process_sap_data works with valid data", {
  mock_sap_data <- create_mock_sap_data()

  # Test that process_sap_data works with valid mock data
  # It should complete without error
  result <- process_sap_data(mock_sap_data, methods = "HRM", verbose = FALSE)

  # Check that result has expected structure
  expect_true(inherits(result, "sap_flow_results"))
  expect_true("vh_results" %in% names(result))
  expect_true("metadata" %in% names(result))
})

# Tests for print.sap_flow_results ----
test_that("print.sap_flow_results works", {
  # Create mock results object
  mock_results <- structure(
    list(
      velocity_results = data.frame(pulse_id = 1, method = "HRM", Vh_cm_hr = 10),
      quality_assessment = list(overall_score = 85, issues = character(0), recommendations = "Good quality"),
      probe_config = list(name = "test_config"),
      processing_summary = list(
        methods_used = c("HRM", "MHR"),
        total_pulses = 3,
        total_measurements = 6
      )
    ),
    class = "sap_flow_results"
  )

  expect_output(print(mock_results), "Sap Flow Processing Results")
  expect_output(print(mock_results), "Configuration: test_config")
  expect_output(print(mock_results), "Data Quality Score: 85")
})

# Edge cases and error handling ----
test_that("assess_data_quality handles minimal data", {
  # Test with very minimal data
  minimal_sap_data <- list(
    measurements = data.frame(
      pulse_id = 1,
      datetime = as.POSIXct("2024-01-01 10:00:00"),
      do = 18.8
    ),
    diagnostics = data.frame(pulse_id = 1)
  )
  class(minimal_sap_data) <- "sap_data"

  minimal_config <- list(name = "minimal", compatible_methods = c("HRM"))

  result <- assess_data_quality(minimal_sap_data, minimal_config)

  # Should handle gracefully and return valid structure
  expect_type(result, "list")
  expect_true(is.numeric(result$overall_score))
  expect_true(result$overall_score >= 0 && result$overall_score <= 100)
})

test_that("recommend_methods fallback behavior", {
  mock_sap_data <- create_mock_sap_data()

  # Test with config that has no compatible methods
  empty_config <- list(
    name = "empty_config",
    compatible_methods = character(0)
  )
  mock_quality <- list(overall_score = 50)

  result <- recommend_methods(mock_sap_data, empty_config, mock_quality)

  # Should fallback to basic method
  expect_true(length(result$recommended_methods) > 0)
  expect_true("HRM" %in% result$recommended_methods)  # Fallback method
})