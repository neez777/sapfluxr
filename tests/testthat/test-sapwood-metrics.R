# ============================================================================
# test-sapwood-metrics.R
# ============================================================================
# Tests for sapwood-area-weighted metrics (Step 8)
#
# Tests cover:
# - calc_mean_flux_density() - Qps calculation
# - normalise_flux_density() - temporal normalisation
# - calc_leaf_area_flux() - leaf-area-specific flux
# - apply_sapwood_metrics() - convenience wrapper
# ============================================================================

library(testthat)
library(sapfluxr)

# ============================================================================
# Tests for calc_mean_flux_density()
# ============================================================================

test_that("calc_mean_flux_density calculates Qps correctly for single value", {
  Qp <- 1000  # cm³/hr
  As <- 200   # cm²

  Qps <- calc_mean_flux_density(Qp, As)

  expect_equal(Qps, 5.0)  # 1000/200 = 5 cm/hr
})

test_that("calc_mean_flux_density works with vector Qp and scalar As", {
  Qp <- c(500, 1000, 1500, 2000)  # cm³/hr
  As <- 250                        # cm²

  Qps <- calc_mean_flux_density(Qp, As)

  expected <- c(2.0, 4.0, 6.0, 8.0)
  expect_equal(Qps, expected)
})

test_that("calc_mean_flux_density works with vector As matching Qp length", {
  Qp <- c(1000, 1200, 1400)
  As <- c(200, 240, 280)

  Qps <- calc_mean_flux_density(Qp, As)

  expected <- c(5.0, 5.0, 5.0)  # Each Qp/As pair
  expect_equal(Qps, expected)
})

test_that("calc_mean_flux_density handles NA values correctly", {
  Qp <- c(1000, NA, 1500, 2000)
  As <- 250

  Qps <- calc_mean_flux_density(Qp, As)

  expect_equal(Qps[1], 4.0)
  expect_true(is.na(Qps[2]))
  expect_equal(Qps[3], 6.0)
  expect_equal(Qps[4], 8.0)
})

test_that("calc_mean_flux_density rejects negative sapwood area", {
  expect_error(
    calc_mean_flux_density(Qp = 1000, As = -200),
    "Sapwood area.*must be positive"
  )
})

test_that("calc_mean_flux_density rejects zero sapwood area", {
  expect_error(
    calc_mean_flux_density(Qp = 1000, As = 0),
    "Sapwood area.*must be positive"
  )
})

test_that("calc_mean_flux_density rejects mismatched vector lengths", {
  expect_error(
    calc_mean_flux_density(Qp = c(1000, 1500), As = c(200, 250, 300)),
    "As must be either a single value or match the length of Qp"
  )
})

test_that("calc_mean_flux_density rejects non-numeric inputs", {
  expect_error(
    calc_mean_flux_density(Qp = "1000", As = 200),
    "Qp must be numeric"
  )

  expect_error(
    calc_mean_flux_density(Qp = 1000, As = "200"),
    "As must be numeric"
  )
})

test_that("calc_mean_flux_density units are correct (cm³/hr/cm² = cm/hr)", {
  # Physical check: Qps should have velocity units
  Qp <- 1000  # cm³/hr
  As <- 100   # cm²

  Qps <- calc_mean_flux_density(Qp, As)

  # Qps = 1000 cm³/hr / 100 cm² = 10 cm³/hr/cm² = 10 cm/hr
  expect_equal(Qps, 10.0)
})


# ============================================================================
# Tests for normalise_flux_density()
# ============================================================================

test_that("normalise_flux_density performs global normalisation correctly", {
  Qps <- c(2.0, 4.0, 6.0, 8.0, 10.0)

  Qpsn <- normalise_flux_density(Qps, period = "global")

  expected <- c(0.2, 0.4, 0.6, 0.8, 1.0)
  expect_equal(Qpsn, expected)
})

test_that("normalise_flux_density handles NA values in global normalisation", {
  Qps <- c(2.0, NA, 6.0, 8.0, 10.0)

  Qpsn <- normalise_flux_density(Qps, period = "global")

  expect_equal(Qpsn[1], 0.2)
  expect_true(is.na(Qpsn[2]))
  expect_equal(Qpsn[3], 0.6)
  expect_equal(Qpsn[5], 1.0)
})

test_that("normalise_flux_density performs custom normalisation correctly", {
  Qps <- c(2.0, 4.0, 6.0, 8.0)

  # Normalise by custom max = 20
  Qpsn <- normalise_flux_density(Qps, period = "custom", Qps_max = 20.0)

  expected <- c(0.1, 0.2, 0.3, 0.4)
  expect_equal(Qpsn, expected)
})

test_that("normalise_flux_density warns when custom max not provided", {
  Qps <- c(2.0, 4.0, 6.0, 8.0)

  expect_warning(
    Qpsn <- normalise_flux_density(Qps, period = "custom"),
    "period = 'custom' but Qps_max not provided"
  )

  # Should fall back to global max
  expect_equal(Qpsn[4], 1.0)
})

test_that("normalise_flux_density performs daily normalisation correctly", {
  datetime <- as.POSIXct(c(
    "2024-01-01 06:00:00",
    "2024-01-01 12:00:00",  # Max for day 1
    "2024-01-01 18:00:00",
    "2024-01-02 06:00:00",
    "2024-01-02 12:00:00",
    "2024-01-02 18:00:00"   # Max for day 2
  ), tz = "UTC")

  Qps <- c(2.0, 8.0, 4.0, 3.0, 6.0, 9.0)

  Qpsn <- normalise_flux_density(Qps, period = "daily", datetime = datetime)

  # Day 1: normalise by 8.0
  expect_equal(Qpsn[1], 2.0/8.0)
  expect_equal(Qpsn[2], 1.0)
  expect_equal(Qpsn[3], 4.0/8.0)

  # Day 2: normalise by 9.0
  expect_equal(Qpsn[4], 3.0/9.0)
  expect_equal(Qpsn[5], 6.0/9.0)
  expect_equal(Qpsn[6], 1.0)
})

test_that("normalise_flux_density performs monthly normalisation correctly", {
  datetime <- as.POSIXct(c(
    "2024-01-05 12:00:00",
    "2024-01-15 12:00:00",  # Max for Jan
    "2024-01-25 12:00:00",
    "2024-02-05 12:00:00",
    "2024-02-15 12:00:00",  # Max for Feb
    "2024-02-25 12:00:00"
  ), tz = "UTC")

  Qps <- c(3.0, 9.0, 6.0, 4.0, 12.0, 8.0)

  Qpsn <- normalise_flux_density(Qps, period = "monthly", datetime = datetime)

  # January: normalise by 9.0
  expect_equal(Qpsn[1], 3.0/9.0, tolerance = 0.001)
  expect_equal(Qpsn[2], 1.0)
  expect_equal(Qpsn[3], 6.0/9.0, tolerance = 0.001)

  # February: normalise by 12.0
  expect_equal(Qpsn[4], 4.0/12.0, tolerance = 0.001)
  expect_equal(Qpsn[5], 1.0)
  expect_equal(Qpsn[6], 8.0/12.0, tolerance = 0.001)
})

test_that("normalise_flux_density requires datetime for daily period", {
  Qps <- c(2.0, 4.0, 6.0)

  expect_error(
    normalise_flux_density(Qps, period = "daily"),
    "datetime is required when period = 'daily'"
  )
})

test_that("normalise_flux_density requires datetime for monthly period", {
  Qps <- c(2.0, 4.0, 6.0)

  expect_error(
    normalise_flux_density(Qps, period = "monthly"),
    "datetime is required when period = 'monthly'"
  )
})

test_that("normalise_flux_density rejects invalid period", {
  Qps <- c(2.0, 4.0, 6.0)

  expect_error(
    normalise_flux_density(Qps, period = "yearly"),
    "period must be one of: 'global', 'daily', 'monthly', 'custom'"
  )
})

test_that("normalise_flux_density rejects mismatched datetime length", {
  Qps <- c(2.0, 4.0, 6.0, 8.0)
  datetime <- as.POSIXct(c("2024-01-01 12:00:00", "2024-01-02 12:00:00"), tz = "UTC")

  expect_error(
    normalise_flux_density(Qps, period = "daily", datetime = datetime),
    "datetime must match the length of Qps"
  )
})

test_that("normalise_flux_density rejects non-POSIXct datetime", {
  Qps <- c(2.0, 4.0, 6.0)
  datetime <- c("2024-01-01", "2024-01-02", "2024-01-03")

  expect_error(
    normalise_flux_density(Qps, period = "daily", datetime = datetime),
    "datetime must be a POSIXct object"
  )
})

test_that("normalise_flux_density handles all-NA day correctly", {
  datetime <- as.POSIXct(c(
    "2024-01-01 12:00:00",
    "2024-01-02 12:00:00"
  ), tz = "UTC")

  Qps <- c(5.0, NA)

  Qpsn <- normalise_flux_density(Qps, period = "daily", datetime = datetime)

  expect_equal(Qpsn[1], 1.0)  # Day 1: only value, so = 1.0
  expect_true(is.na(Qpsn[2]))  # Day 2: NA value
})

test_that("normalise_flux_density rejects negative Qps_max", {
  Qps <- c(2.0, 4.0, 6.0)

  expect_error(
    normalise_flux_density(Qps, period = "custom", Qps_max = -10),
    "Qps_max must be positive"
  )
})


# ============================================================================
# Tests for calc_leaf_area_flux()
# ============================================================================

test_that("calc_leaf_area_flux calculates Qpl correctly with m² units", {
  Qp <- 2400   # cm³/hr
  Al <- 12.0   # m²

  Qpl <- calc_leaf_area_flux(Qp, Al, Al_units = "m2")

  expect_equal(Qpl, 200.0)  # 2400 / 12 = 200 cm³/hr/m²
})

test_that("calc_leaf_area_flux calculates Qpl correctly with cm² units", {
  Qp <- 2400     # cm³/hr
  Al <- 120000   # cm²

  Qpl <- calc_leaf_area_flux(Qp, Al, Al_units = "cm2")

  expect_equal(Qpl, 0.02)  # 2400 / 120000 = 0.02 cm³/hr/cm²
})

test_that("calc_leaf_area_flux works with vector Qp and scalar Al", {
  Qp <- c(1200, 2400, 3600)
  Al <- 12.0  # m²

  Qpl <- calc_leaf_area_flux(Qp, Al, Al_units = "m2")

  expected <- c(100.0, 200.0, 300.0)
  expect_equal(Qpl, expected)
})

test_that("calc_leaf_area_flux works with vector Al matching Qp length", {
  Qp <- c(2400, 3000, 3600)
  Al <- c(12.0, 15.0, 18.0)

  Qpl <- calc_leaf_area_flux(Qp, Al, Al_units = "m2")

  expected <- c(200.0, 200.0, 200.0)
  expect_equal(Qpl, expected)
})

test_that("calc_leaf_area_flux handles NA values correctly", {
  Qp <- c(2400, NA, 3600)
  Al <- 12.0

  Qpl <- calc_leaf_area_flux(Qp, Al, Al_units = "m2")

  expect_equal(Qpl[1], 200.0)
  expect_true(is.na(Qpl[2]))
  expect_equal(Qpl[3], 300.0)
})

test_that("calc_leaf_area_flux rejects negative leaf area", {
  expect_error(
    calc_leaf_area_flux(Qp = 2400, Al = -12, Al_units = "m2"),
    "Leaf area.*must be positive"
  )
})

test_that("calc_leaf_area_flux rejects zero leaf area", {
  expect_error(
    calc_leaf_area_flux(Qp = 2400, Al = 0, Al_units = "m2"),
    "Leaf area.*must be positive"
  )
})

test_that("calc_leaf_area_flux rejects invalid Al_units", {
  expect_error(
    calc_leaf_area_flux(Qp = 2400, Al = 12, Al_units = "km2"),
    "Al_units must be either 'm2' or 'cm2'"
  )
})

test_that("calc_leaf_area_flux rejects mismatched vector lengths", {
  expect_error(
    calc_leaf_area_flux(Qp = c(2400, 3000), Al = c(12, 15, 18), Al_units = "m2"),
    "Al must be either a single value or match the length of Qp"
  )
})

test_that("calc_leaf_area_flux rejects non-numeric inputs", {
  expect_error(
    calc_leaf_area_flux(Qp = "2400", Al = 12, Al_units = "m2"),
    "Qp must be numeric"
  )

  expect_error(
    calc_leaf_area_flux(Qp = 2400, Al = "12", Al_units = "m2"),
    "Al must be numeric"
  )
})


# ============================================================================
# Tests for apply_sapwood_metrics()
# ============================================================================

test_that("apply_sapwood_metrics adds Qps columns correctly", {
  flux_data <- data.frame(
    datetime = as.POSIXct(c(
      "2024-01-01 12:00:00",
      "2024-01-01 13:00:00",
      "2024-01-01 14:00:00"
    ), tz = "UTC"),
    Q_cm3_hr = c(1000, 1500, 2000)
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 250
  )

  expect_true("Qps_cm3_hr_cm2" %in% names(result))
  expect_true("Qps_cm_hr" %in% names(result))

  expect_equal(result$Qps_cm3_hr_cm2, c(4.0, 6.0, 8.0))
  expect_equal(result$Qps_cm_hr, c(4.0, 6.0, 8.0))
})

test_that("apply_sapwood_metrics adds normalised column when requested", {
  flux_data <- data.frame(
    datetime = as.POSIXct(c(
      "2024-01-01 12:00:00",
      "2024-01-01 13:00:00",
      "2024-01-01 14:00:00"
    ), tz = "UTC"),
    Q_cm3_hr = c(500, 750, 1000)
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 250,
    normalise = TRUE,
    normalise_period = "global"
  )

  expect_true("Qpsn" %in% names(result))
  expect_equal(result$Qpsn, c(0.5, 0.75, 1.0))
})

test_that("apply_sapwood_metrics adds leaf area column when requested", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    Q_cm3_hr = 2400
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 250,
    leaf_area = 12.0,
    leaf_area_units = "m2"
  )

  expect_true("Qpl_cm3_hr_m2" %in% names(result))
  expect_equal(result$Qpl_cm3_hr_m2, 200.0)
})

test_that("apply_sapwood_metrics handles custom flux column name", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    total_flux = 1000
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    flux_col = "total_flux",
    sapwood_area = 200
  )

  expect_equal(result$Qps_cm3_hr_cm2, 5.0)
})

test_that("apply_sapwood_metrics rejects missing flux column", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    flow = 1000
  )

  expect_error(
    apply_sapwood_metrics(data = flux_data, sapwood_area = 200),
    "Column 'Q_cm3_hr' not found in data"
  )
})

test_that("apply_sapwood_metrics rejects missing datetime for daily normalisation", {
  flux_data <- data.frame(
    timestamp = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    Q_cm3_hr = 1000
  )

  expect_error(
    apply_sapwood_metrics(
      data = flux_data,
      sapwood_area = 200,
      normalise = TRUE,
      normalise_period = "daily"
    ),
    "Column 'datetime' not found in data"
  )
})

test_that("apply_sapwood_metrics works with custom datetime column", {
  flux_data <- data.frame(
    timestamp = as.POSIXct(c(
      "2024-01-01 12:00:00",
      "2024-01-01 13:00:00"
    ), tz = "UTC"),
    Q_cm3_hr = c(500, 1000)
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 250,
    normalise = TRUE,
    normalise_period = "daily",
    datetime_col = "timestamp"
  )

  expect_true("Qpsn" %in% names(result))
  expect_equal(result$Qpsn, c(0.5, 1.0))
})

test_that("apply_sapwood_metrics handles all optional features together", {
  flux_data <- data.frame(
    datetime = as.POSIXct(c(
      "2024-01-01 12:00:00",
      "2024-01-01 13:00:00"
    ), tz = "UTC"),
    Q_cm3_hr = c(1200, 2400)
  )

  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 300,
    normalise = TRUE,
    normalise_period = "global",
    leaf_area = 15.0,
    leaf_area_units = "m2"
  )

  # Check all columns added
  expect_true("Qps_cm3_hr_cm2" %in% names(result))
  expect_true("Qps_cm_hr" %in% names(result))
  expect_true("Qpsn" %in% names(result))
  expect_true("Qpl_cm3_hr_m2" %in% names(result))

  # Check values
  expect_equal(result$Qps_cm3_hr_cm2, c(4.0, 8.0))
  expect_equal(result$Qpsn, c(0.5, 1.0))
  expect_equal(result$Qpl_cm3_hr_m2, c(80.0, 160.0))
})

test_that("apply_sapwood_metrics rejects non-dataframe input", {
  expect_error(
    apply_sapwood_metrics(data = c(1000, 1500), sapwood_area = 250),
    "data must be a data frame"
  )
})

test_that("apply_sapwood_metrics requires sapwood_area parameter", {
  flux_data <- data.frame(
    datetime = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    Q_cm3_hr = 1000
  )

  expect_error(
    apply_sapwood_metrics(data = flux_data),
    "sapwood_area is required"
  )
})


# ============================================================================
# Integration tests with realistic sap flux data
# ============================================================================

test_that("Integration: metrics pipeline works with realistic data", {
  # Simulate realistic sap flux data
  flux_data <- data.frame(
    datetime = seq(
      as.POSIXct("2024-01-15 00:00:00", tz = "UTC"),
      as.POSIXct("2024-01-15 23:00:00", tz = "UTC"),
      by = "hour"
    ),
    Q_cm3_hr = c(
      # Night (low flux)
      50, 45, 40, 35, 30, 25,
      # Morning rise
      100, 300, 600, 1000,
      # Midday peak
      1500, 1800, 2000, 1900, 1700,
      # Afternoon decline
      1400, 1100, 800, 500,
      # Evening
      200, 100, 70, 60, 55
    )
  )

  # Apply all metrics
  result <- apply_sapwood_metrics(
    data = flux_data,
    sapwood_area = 400,  # cm²
    normalise = TRUE,
    normalise_period = "daily",
    leaf_area = 20.0,  # m²
    leaf_area_units = "m2"
  )

  # Check structure
  expect_equal(nrow(result), 24)
  expect_true(all(c("Qps_cm3_hr_cm2", "Qpsn", "Qpl_cm3_hr_m2") %in% names(result)))

  # Check Qps range (should be Qp/400)
  expect_equal(range(result$Qps_cm3_hr_cm2),
               c(0.0625, 5.0),  # 25/400 to 2000/400
               tolerance = 0.001)

  # Check normalisation (max should be 1.0)
  expect_equal(max(result$Qpsn), 1.0)
  expect_equal(min(result$Qpsn), 0.0125, tolerance = 0.001)  # 25/2000

  # Check leaf area flux
  expect_equal(range(result$Qpl_cm3_hr_m2),
               c(1.25, 100.0),  # 25/20 to 2000/20
               tolerance = 0.01)
})
