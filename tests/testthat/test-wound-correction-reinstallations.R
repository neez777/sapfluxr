# Tests for Multiple Probe Reinstallations (Segmented Wound Correction)
# Testing the updated calc_wound_diameter() function with reinstall_dates

library(testthat)
library(lubridate)

# ============================================================================
# Test 1: Single Reinstallation
# ============================================================================

test_that("calc_wound_diameter handles single reinstallation correctly", {
  # Setup: 3 months of data with 1 reinstallation
  timestamps <- seq(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-03-31 23:00:00", tz = "UTC"),
    by = "hour"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-02-01"),
    wound_at_reinstall_mm = c(2.6, 2.4)  # initial, after reinstall
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Check results
  expect_type(result, "double")
  expect_equal(length(result), length(timestamps))

  # January data should have 2.6mm = 0.26cm
  jan_data <- result[timestamps < as.POSIXct("2024-02-01", tz = "UTC")]
  expect_true(all(jan_data == 0.26))

  # Feb-Mar data should have 2.4mm = 0.24cm
  feb_mar_data <- result[timestamps >= as.POSIXct("2024-02-01", tz = "UTC")]
  expect_true(all(feb_mar_data == 0.24))

  # Should have exactly 2 unique values
  expect_equal(length(unique(result)), 2)
})

# ============================================================================
# Test 2: Multiple Reinstallations (3 periods)
# ============================================================================

test_that("calc_wound_diameter handles multiple reinstallations", {
  # Setup: 1 year with 3 reinstallations (4 periods)
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date(c("2024-03-15", "2024-07-20", "2024-11-05")),
    wound_at_reinstall_mm = c(2.6, 2.4, 2.7, 2.5)
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Check structure
  expect_type(result, "double")
  expect_equal(length(result), length(timestamps))

  # Check each period
  period1 <- result[timestamps < as.POSIXct("2024-03-15", tz = "UTC")]
  period2 <- result[timestamps >= as.POSIXct("2024-03-15", tz = "UTC") &
                    timestamps < as.POSIXct("2024-07-20", tz = "UTC")]
  period3 <- result[timestamps >= as.POSIXct("2024-07-20", tz = "UTC") &
                    timestamps < as.POSIXct("2024-11-05", tz = "UTC")]
  period4 <- result[timestamps >= as.POSIXct("2024-11-05", tz = "UTC")]

  expect_true(all(period1 == 0.26))
  expect_true(all(period2 == 0.24))
  expect_true(all(period3 == 0.27))
  expect_true(all(period4 == 0.25))

  # Should have exactly 4 unique values
  expect_equal(length(unique(result)), 4)
})

# ============================================================================
# Test 3: Edge Case - Reinstallation at Data Start
# ============================================================================

test_that("calc_wound_diameter handles reinstallation at data start", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-02-28", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-01-01"),  # At start
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # All data should be in second period (after reinstall)
  expect_true(all(result == 0.24))
})

# ============================================================================
# Test 4: Edge Case - Reinstallation at Data End
# ============================================================================

test_that("calc_wound_diameter handles reinstallation at data end", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-02-28", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-02-28"),  # At end
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Most data should be in first period
  period1 <- result[timestamps < as.POSIXct("2024-02-28", tz = "UTC")]
  period2 <- result[timestamps >= as.POSIXct("2024-02-28", tz = "UTC")]

  expect_true(all(period1 == 0.26))
  expect_true(all(period2 == 0.24))
})

# ============================================================================
# Test 5: Validation - Wrong Number of Wound Diameters
# ============================================================================

test_that("calc_wound_diameter errors on wrong number of wound diameters", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31", tz = "UTC"),
    by = "day"
  )

  # 2 reinstalls but only 2 wound values (should be 3)
  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date(c("2024-03-01", "2024-06-01")),
    wound_at_reinstall_mm = c(2.6, 2.4)  # WRONG - should be 3!
  )

  expect_error(
    calc_wound_diameter(timestamps, wound_config),
    regexp = "wound_at_reinstall_mm must have length = length\\(reinstall_dates\\) \\+ 1"
  )

  # 1 reinstall but 3 wound values (should be 2)
  wound_config2 <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-03-01"),
    wound_at_reinstall_mm = c(2.6, 2.4, 2.8)  # WRONG - too many!
  )

  expect_error(
    calc_wound_diameter(timestamps, wound_config2),
    regexp = "wound_at_reinstall_mm must have length = length\\(reinstall_dates\\) \\+ 1"
  )
})

# ============================================================================
# Test 6: Validation - Unsorted Reinstallation Dates
# ============================================================================

test_that("calc_wound_diameter errors on unsorted reinstall dates", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date(c("2024-06-01", "2024-03-01")),  # Wrong order!
    wound_at_reinstall_mm = c(2.6, 2.4, 2.8)
  )

  expect_error(
    calc_wound_diameter(timestamps, wound_config),
    regexp = "reinstall_dates must be in chronological order"
  )
})

# ============================================================================
# Test 7: Date Format Flexibility
# ============================================================================

test_that("calc_wound_diameter accepts multiple date formats", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-03-31", tz = "UTC"),
    by = "day"
  )

  # Test with character dates
  wound_config_char <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = "2024-02-01",
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result_char <- calc_wound_diameter(timestamps, wound_config_char)

  # Test with Date objects
  wound_config_date <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-02-01"),
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result_date <- calc_wound_diameter(timestamps, wound_config_date)

  # Test with POSIXct
  wound_config_posix <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.POSIXct("2024-02-01", tz = "UTC"),
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result_posix <- calc_wound_diameter(timestamps, wound_config_posix)

  # All should give same results
  expect_equal(result_char, result_date)
  expect_equal(result_date, result_posix)
})

# ============================================================================
# Test 8: Segment Assignment Boundaries
# ============================================================================

test_that("calc_wound_diameter assigns segments correctly at boundaries", {
  # Create timestamps at exact boundary times
  timestamps <- as.POSIXct(c(
    "2024-01-31 23:59:59",  # Just before reinstall
    "2024-02-01 00:00:00",  # Exact reinstall time
    "2024-02-01 00:00:01"   # Just after reinstall
  ), tz = "UTC")

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.POSIXct("2024-02-01 00:00:00", tz = "UTC"),
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Before reinstall should be 0.26
  expect_equal(result[1], 0.26)

  # At and after reinstall should be 0.24
  expect_equal(result[2], 0.24)
  expect_equal(result[3], 0.24)
})

# ============================================================================
# Test 9: Backward Compatibility - Static Mode
# ============================================================================

test_that("calc_wound_diameter still supports static mode (no reinstallations)", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # All values should be the same (2.0 + 2*0.3 = 2.6mm = 0.26cm)
  expect_true(all(result == 0.26))
  expect_equal(length(unique(result)), 1)
})

# ============================================================================
# Test 10: Backward Compatibility - Legacy Temporal Mode
# ============================================================================

test_that("calc_wound_diameter still supports legacy temporal mode", {
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31", tz = "UTC"),
    by = "month"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    initial_date = as.POSIXct("2024-01-01", tz = "UTC"),
    final_date = as.POSIXct("2024-12-31", tz = "UTC"),
    final_diameter_mm = 2.8
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Should have interpolated values
  expect_type(result, "double")
  expect_equal(length(result), length(timestamps))

  # First value should be initial (0.26cm)
  expect_equal(result[1], 0.26, tolerance = 0.001)

  # Last value should be final (approximately 0.28cm due to interpolation)
  expect_equal(result[length(result)], 0.28, tolerance = 0.01)

  # Middle values should be interpolated
  expect_true(all(result >= 0.26 & result <= 0.28))
  expect_true(any(result > 0.26 & result < 0.28))  # Some intermediate values
})

# ============================================================================
# Test 11: Integration with apply_wound_correction()
# ============================================================================

test_that("apply_wound_correction works with segmented wound diameters", {
  # Test direct use of calc_wound_diameter with wood properties
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-03-31", tz = "UTC"),
    by = "day"
  )

  # Create wood properties with reinstallation
  wood_props <- list(
    wound_correction = list(
      drill_bit_diameter_mm = 2.0,
      wound_addition_mm = 0.3,
      reinstall_dates = as.Date("2024-02-01"),
      wound_at_reinstall_mm = c(2.6, 2.4)
    )
  )
  class(wood_props) <- "WoodProperties"

  # Calculate wound diameters
  wound_diams <- calc_wound_diameter(timestamps, wood_props)

  # Check wound diameters assigned correctly
  jan_wounds <- wound_diams[timestamps < as.POSIXct("2024-02-01", tz = "UTC")]
  feb_mar_wounds <- wound_diams[timestamps >= as.POSIXct("2024-02-01", tz = "UTC")]

  expect_true(all(jan_wounds == 0.26))
  expect_true(all(feb_mar_wounds == 0.24))
  expect_equal(length(wound_diams), length(timestamps))
})

# ============================================================================
# Test 12: Many Reinstallations (Stress Test)
# ============================================================================

test_that("calc_wound_diameter handles many reinstallations efficiently", {
  # 2 years of hourly data
  timestamps <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2024-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )

  # Monthly reinstallations (24 reinstalls, 25 periods)
  reinstall_dates <- seq(
    as.Date("2023-02-01"),
    as.Date("2024-12-01"),
    by = "month"
  )

  # Generate wound diameters (varying between 2.3 and 2.8mm)
  wound_diams <- seq(2.6, 2.3, length.out = length(reinstall_dates) + 1)

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = reinstall_dates,
    wound_at_reinstall_mm = wound_diams
  )

  # Should complete without error
  result <- calc_wound_diameter(timestamps, wound_config)

  expect_type(result, "double")
  expect_equal(length(result), length(timestamps))

  # Should have correct number of unique values
  expect_equal(length(unique(result)), length(reinstall_dates) + 1)
})

# ============================================================================
# Test 13: Empty/Minimal Edge Cases
# ============================================================================

test_that("calc_wound_diameter handles edge cases gracefully", {
  # Single timestamp
  single_ts <- as.POSIXct("2024-06-15", tz = "UTC")

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date(c("2024-03-01", "2024-09-01")),
    wound_at_reinstall_mm = c(2.6, 2.4, 2.8)
  )

  result_single <- calc_wound_diameter(single_ts, wound_config)
  expect_equal(length(result_single), 1)
  expect_equal(result_single, 0.24)  # In middle period

  # Timestamp before all reinstallations
  early_ts <- as.POSIXct("2024-01-01", tz = "UTC")
  result_early <- calc_wound_diameter(early_ts, wound_config)
  expect_equal(result_early, 0.26)  # Initial period

  # Timestamp after all reinstallations
  late_ts <- as.POSIXct("2024-12-31", tz = "UTC")
  result_late <- calc_wound_diameter(late_ts, wound_config)
  expect_equal(result_late, 0.28)  # Final period
})

# ============================================================================
# Test 14: YAML Configuration Loading
# ============================================================================

test_that("segmented wound correction works from YAML config", {
  # This test verifies that reinstall_dates can be loaded from YAML
  # and used correctly (character format dates)

  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-06-30", tz = "UTC"),
    by = "day"
  )

  # Simulate config loaded from YAML (character dates)
  yaml_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = c("2024-03-01", "2024-05-01"),
    wound_at_reinstall_mm = c(2.6, 2.3, 2.7)
  )

  result <- calc_wound_diameter(timestamps, yaml_config)

  # Should have 3 unique values
  unique_vals <- unique(result)
  expect_equal(length(unique_vals), 3)

  # Values should be close to 0.26, 0.23, 0.27 (allowing for rounding)
  expect_true(any(abs(unique_vals - 0.26) < 0.001))
  expect_true(any(abs(unique_vals - 0.23) < 0.001))
  expect_true(any(abs(unique_vals - 0.27) < 0.001))
})

# ============================================================================
# Test 15: Segment Transition Counts
# ============================================================================

test_that("calc_wound_diameter transitions correctly between segments", {
  # Daily data across 3 months with 1 reinstallation
  timestamps <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    as.POSIXct("2024-03-31", tz = "UTC"),
    by = "day"
  )

  wound_config <- list(
    drill_bit_diameter_mm = 2.0,
    wound_addition_mm = 0.3,
    reinstall_dates = as.Date("2024-02-15"),
    wound_at_reinstall_mm = c(2.6, 2.4)
  )

  result <- calc_wound_diameter(timestamps, wound_config)

  # Count transitions (where wound diameter changes)
  transitions <- which(diff(result) != 0)

  # Should have exactly 1 transition (at reinstallation)
  expect_equal(length(transitions), 1)

  # Transition should occur around Feb 15
  transition_date <- timestamps[transitions[1]]
  expect_true(transition_date >= as.POSIXct("2024-02-14", tz = "UTC"))
  expect_true(transition_date <= as.POSIXct("2024-02-16", tz = "UTC"))
})
