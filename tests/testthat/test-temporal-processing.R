# Tests for temporal processing functions
# Source file: R/10_temporal_processing.R

library(testthat)

test_that("aggregate_velocity_temporal works with sample data", {
  # Test with minimal data structure
  sample_data <- data.frame(
    datetime = as.POSIXct(c("2024-11-15 10:00:00", "2024-11-15 10:30:00")),
    method = c("HRM", "HRM"),
    sensor_position = c("outer", "outer"),
    Vh_cm_hr = c(15.0, 16.0),
    quality_flag = c("OK", "OK")
  )

  result <- aggregate_velocity_temporal(sample_data, interval = "hourly")
  expect_s3_class(result, "data.frame")
  expect_true("Vh_cm_hr" %in% names(result))
})