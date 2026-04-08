# Tests for weather data import and VPD calculation functions

test_that("read_weather_data imports CSV with auto-detection", {
  # Create a temporary CSV file
  temp_file <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    DateTime = c("1/01/2024 10:00", "1/01/2024 10:10", "1/01/2024 10:20"),
    `Air Temperature_°C` = c(20.5, 21.0, 20.8),
    `Relative Humidity_%` = c(55.0, 53.5, 54.2),
    `Atmospheric Pressure_kPa` = c(101.3, 101.2, 101.3),
    check.names = FALSE
  )

  write.csv(test_data, temp_file, row.names = FALSE)

  # Test auto-detection
  weather <- read_weather_data(temp_file)

  expect_s3_class(weather, "weather_data")
  expect_s3_class(weather, "tbl_df")
  expect_equal(nrow(weather), 3)
  expect_true("datetime" %in% names(weather))
  expect_true("air_temp_c" %in% names(weather))
  expect_true("relative_humidity" %in% names(weather))
  expect_true("pressure_kpa" %in% names(weather))

  # Clean up
  unlink(temp_file)
})


test_that("read_weather_data handles manual column specification", {
  temp_file <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    timestamp = c("2024-01-01 10:00", "2024-01-01 10:10", "2024-01-01 10:20"),
    Ta = c(20.5, 21.0, 20.8),
    RH = c(55.0, 53.5, 54.2)
  )

  write.csv(test_data, temp_file, row.names = FALSE)

  weather <- read_weather_data(
    temp_file,
    datetime_col = "timestamp",
    temp_col = "Ta",
    rh_col = "RH"
  )

  expect_equal(nrow(weather), 3)
  expect_equal(weather$air_temp_c, test_data$Ta)
  expect_equal(weather$relative_humidity, test_data$RH)

  unlink(temp_file)
})


test_that("read_weather_data detects datetime formats correctly", {
  temp_file <- tempfile(fileext = ".csv")

  # Test dmy format (common in Australia)
  test_data <- data.frame(
    Date_Time = c("2/08/2024 13:40", "2/08/2024 14:00", "2/08/2024 14:20"),
    Temperature = c(16.9, 15.1, 15.0),
    Humidity = c(50.6, 55.1, 59.8)
  )

  write.csv(test_data, temp_file, row.names = FALSE)

  weather <- read_weather_data(temp_file)

  expect_s3_class(weather$datetime, "POSIXct")
  expect_equal(lubridate::day(weather$datetime[1]), 2)
  expect_equal(lubridate::month(weather$datetime[1]), 8)
  expect_equal(lubridate::year(weather$datetime[1]), 2024)

  unlink(temp_file)
})


test_that("detect_temperature_column identifies correct column", {
  test_data <- data.frame(
    timestamp = c("2024-01-01 10:00", "2024-01-01 10:10"),
    air_temp = c(20.5, 21.0),
    humidity = c(55.0, 53.5),
    pressure = c(101.3, 101.2)
  )

  result <- sapfluxr:::detect_temperature_column(test_data)
  expect_equal(result, "air_temp")
})


test_that("detect_temperature_column uses range heuristics", {
  # Test with no clear column name but correct range
  test_data <- data.frame(
    col1 = c("2024-01-01 10:00", "2024-01-01 10:10"),
    col2 = c(20.5, 21.0),  # Temperature range
    col3 = c(55.0, 53.5),   # Could be RH
    col4 = c(101.3, 101.2)  # Pressure range
  )

  result <- sapfluxr:::detect_temperature_column(test_data)
  expect_equal(result, "col2")
})


test_that("detect_humidity_column identifies correct column", {
  test_data <- data.frame(
    timestamp = c("2024-01-01 10:00", "2024-01-01 10:10"),
    temperature = c(20.5, 21.0),
    rel_humidity = c(55.0, 53.5),
    pressure = c(101.3, 101.2)
  )

  result <- sapfluxr:::detect_humidity_column(test_data)
  expect_equal(result, "rel_humidity")
})


test_that("detect_pressure_column identifies correct column", {
  test_data <- data.frame(
    timestamp = c("2024-01-01 10:00", "2024-01-01 10:10"),
    temperature = c(20.5, 21.0),
    humidity = c(55.0, 53.5),
    atm_pressure = c(101.3, 101.2)
  )

  result <- sapfluxr:::detect_pressure_column(test_data)
  expect_equal(result, "atm_pressure")
})


test_that("validate_weather_data detects out-of-range values", {
  test_data <- tibble::tibble(
    datetime = lubridate::ymd_hm(c("2024-01-01 10:00", "2024-01-01 10:10")),
    air_temp_c = c(60, 21.0),  # 60 is too high
    relative_humidity = c(55.0, 110)  # 110 is invalid
  )

  expect_warning(
    result <- sapfluxr:::validate_weather_data(test_data),
    "validation issues"
  )

  issues <- attr(result, "validation_issues")
  expect_true(any(grepl("Temperature outside expected range", issues)))
  expect_true(any(grepl("Relative humidity outside valid range", issues)))
})


test_that("validate_weather_data detects missing values", {
  test_data <- tibble::tibble(
    datetime = lubridate::ymd_hm(c("2024-01-01 10:00", "2024-01-01 10:10", "2024-01-01 10:20")),
    air_temp_c = c(20, NA, 21),
    relative_humidity = c(55, 56, NA)
  )

  expect_warning(
    result <- sapfluxr:::validate_weather_data(test_data),
    "validation issues"
  )

  issues <- attr(result, "validation_issues")
  expect_true(any(grepl("missing temperature values", issues)))
  expect_true(any(grepl("missing relative humidity values", issues)))
})


test_that("calc_vpd calculates VPD correctly", {
  weather_data <- tibble::tibble(
    datetime = lubridate::ymd_hm("2024-01-01 10:00"),
    air_temp_c = 20,
    relative_humidity = 50
  )

  result <- calc_vpd(weather_data)

  expect_true("vpd_kpa" %in% names(result))
  expect_true(is.numeric(result$vpd_kpa))
  expect_true(result$vpd_kpa > 0)

  # VPD should be positive when RH < 100%
  expect_gt(result$vpd_kpa, 0)
})


test_that("calc_vpd handles known VPD values", {
  # At 20°C and 50% RH, VPD should be approximately 1.17 kPa
  weather_data <- tibble::tibble(
    datetime = lubridate::ymd_hm("2024-01-01 10:00"),
    air_temp_c = 20,
    relative_humidity = 50
  )

  result <- calc_vpd(weather_data)

  # Check within reasonable tolerance
  expect_equal(result$vpd_kpa, 1.17, tolerance = 0.05)
})


test_that("calc_vpd returns components when requested", {
  weather_data <- tibble::tibble(
    datetime = lubridate::ymd_hm("2024-01-01 10:00"),
    air_temp_c = 25,
    relative_humidity = 60
  )

  result <- calc_vpd(weather_data, return_components = TRUE)

  expect_true("vpd_kpa" %in% names(result))
  expect_true("svp_kpa" %in% names(result))
  expect_true("avp_kpa" %in% names(result))

  # Check relationship: VPD = SVP - AVP
  expect_equal(result$vpd_kpa, result$svp_kpa - result$avp_kpa, tolerance = 1e-10)
})


test_that("calc_vpd works with custom column names", {
  my_data <- data.frame(
    temp = c(20, 25, 30),
    rh = c(50, 60, 70)
  )

  result <- calc_vpd(my_data, temp_col = "temp", rh_col = "rh")

  expect_true("vpd_kpa" %in% names(result))
  expect_equal(length(result$vpd_kpa), 3)
  expect_true(all(result$vpd_kpa > 0))
})


test_that("calc_vpd accepts custom Magnus parameters", {
  weather_data <- tibble::tibble(
    datetime = lubridate::ymd_hm("2024-01-01 10:00"),
    air_temp_c = 20,
    relative_humidity = 50
  )

  # Default parameters
  result1 <- calc_vpd(weather_data)

  # Custom parameters (should give different result)
  result2 <- calc_vpd(weather_data, magnus_coef = 17.5, magnus_base_temp = 240.0)

  expect_false(isTRUE(all.equal(result1$vpd_kpa, result2$vpd_kpa)))
})


test_that("calc_vpd handles edge cases", {
  # RH = 100% should give VPD = 0
  weather_100rh <- tibble::tibble(
    air_temp_c = 20,
    relative_humidity = 100
  )

  result_100 <- calc_vpd(weather_100rh)
  expect_equal(result_100$vpd_kpa, 0, tolerance = 1e-10)

  # RH = 0% should give VPD = SVP
  weather_0rh <- tibble::tibble(
    air_temp_c = 20,
    relative_humidity = 0
  )

  result_0 <- calc_vpd(weather_0rh, return_components = TRUE)
  expect_equal(result_0$vpd_kpa, result_0$svp_kpa, tolerance = 1e-10)
})


test_that("calc_vpd handles missing values appropriately", {
  weather_data <- tibble::tibble(
    air_temp_c = c(20, NA, 25),
    relative_humidity = c(50, 60, NA)
  )

  result <- calc_vpd(weather_data)

  expect_true(is.na(result$vpd_kpa[2]))
  expect_true(is.na(result$vpd_kpa[3]))
  expect_false(is.na(result$vpd_kpa[1]))
})


test_that("calc_vpd warns about invalid RH values", {
  weather_data <- tibble::tibble(
    air_temp_c = c(20, 25),
    relative_humidity = c(50, 110)  # 110 is invalid
  )

  expect_warning(
    calc_vpd(weather_data),
    "outside the valid range"
  )
})


test_that("VPD increases with temperature at constant RH", {
  weather_data <- tibble::tibble(
    air_temp_c = c(10, 20, 30),
    relative_humidity = c(50, 50, 50)
  )

  result <- calc_vpd(weather_data)

  # VPD should increase with temperature
  expect_true(result$vpd_kpa[2] > result$vpd_kpa[1])
  expect_true(result$vpd_kpa[3] > result$vpd_kpa[2])
})


test_that("VPD decreases with RH at constant temperature", {
  weather_data <- tibble::tibble(
    air_temp_c = c(20, 20, 20),
    relative_humidity = c(30, 50, 70)
  )

  result <- calc_vpd(weather_data)

  # VPD should decrease with increasing RH
  expect_true(result$vpd_kpa[1] > result$vpd_kpa[2])
  expect_true(result$vpd_kpa[2] > result$vpd_kpa[3])
})


test_that("print.weather_data displays correctly", {
  temp_file <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    DateTime = c("1/01/2024 10:00", "1/01/2024 10:10"),
    Temperature = c(20.5, 21.0),
    Humidity = c(55.0, 53.5)
  )

  write.csv(test_data, temp_file, row.names = FALSE)

  weather <- read_weather_data(temp_file)

  # Capture print output
  output <- capture.output(print(weather))

  expect_true(any(grepl("Weather Data", output)))
  expect_true(any(grepl("Records:", output)))
  expect_true(any(grepl("Time range:", output)))

  unlink(temp_file)
})


test_that("read_weather_data preserves metadata", {
  temp_file <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    DateTime = c("1/01/2024 10:00", "1/01/2024 10:10"),
    Temperature = c(20.5, 21.0),
    Humidity = c(55.0, 53.5)
  )

  write.csv(test_data, temp_file, row.names = FALSE)

  weather <- read_weather_data(temp_file)

  expect_true(!is.null(attr(weather, "source_file")))
  expect_true(!is.null(attr(weather, "import_time")))
  expect_true(!is.null(attr(weather, "column_mapping")))

  unlink(temp_file)
})


test_that("read_weather_data errors on missing file", {
  expect_error(
    read_weather_data("nonexistent_file.csv"),
    "File not found"
  )
})


test_that("read_weather_data errors on empty file", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(), temp_file, row.names = FALSE)

  expect_error(
    read_weather_data(temp_file),
    "empty"
  )

  unlink(temp_file)
})


test_that("calc_vpd errors on missing columns", {
  weather_data <- tibble::tibble(
    wrong_col = c(20, 25)
  )

  expect_error(
    calc_vpd(weather_data),
    "not found in data"
  )
})


# ============================================================================
# SILO Data Tests
# ============================================================================

# Helper to create a sample SILO CSV file
create_sample_silo_csv <- function(file_path, n_days = 30,
                                    start_date = "2013-03-26",
                                    include_extra_cols = FALSE) {
  dates <- seq(as.Date(start_date), by = "day", length.out = n_days)

  # Generate rainfall: mostly 0, some rain events
  set.seed(42)
  rainfall <- ifelse(runif(n_days) > 0.7, round(runif(n_days, 0.1, 30), 1), 0)

  # Build metadata column: first 5 rows have metadata, rest empty
  metadata <- rep('""', n_days)
  metadata[1] <- '"elevation=  24.2 m"'
  metadata[2] <- '"reference=r"'
  metadata[3] <- '"extracted=20260312"'
  metadata[4] <- '"dataset=BoM Only"'
  metadata[5] <- '"Please read our web site for information"'

  lines <- character(n_days + 1)

  if (include_extra_cols) {
    lines[1] <- "latitude,longitude,YYYY-MM-DD,daily_rain,daily_rain_source,max_temp,max_temp_source,min_temp,min_temp_source,metadata"
    for (i in seq_len(n_days)) {
      max_t <- round(runif(1, 15, 35), 1)
      min_t <- round(max_t - runif(1, 5, 15), 1)
      lines[i + 1] <- sprintf(" -32.0500, 115.8500,%s,%8.1f,25,%8.1f,25,%8.1f,25,%s",
                               format(dates[i]), rainfall[i], max_t, min_t, metadata[i])
    }
  } else {
    lines[1] <- "latitude,longitude,YYYY-MM-DD,daily_rain,daily_rain_source,metadata"
    for (i in seq_len(n_days)) {
      lines[i + 1] <- sprintf(" -32.0500, 115.8500,%s,%8.1f,25,%s",
                               format(dates[i]), rainfall[i], metadata[i])
    }
  }

  writeLines(lines, file_path)
}


test_that("read_silo_data reads standard SILO CSV correctly", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_silo_csv(temp_file, n_days = 30)

  silo <- read_silo_data(temp_file)

  expect_s3_class(silo, "silo_data")
  expect_equal(nrow(silo), 30)
  expect_true(all(c("date", "daily_rain", "rain_3d", "rain_7d") %in% names(silo)))
  expect_s3_class(silo$date, "Date")
  expect_true(is.numeric(silo$daily_rain))
})


test_that("read_silo_data extracts site metadata", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_silo_csv(temp_file)

  silo <- read_silo_data(temp_file)

  expect_equal(attr(silo, "latitude"), -32.05)
  expect_equal(attr(silo, "longitude"), 115.85)
  expect_equal(attr(silo, "silo_elevation"), "24.2 m")
  expect_equal(attr(silo, "silo_dataset"), "BoM Only")
  expect_equal(attr(silo, "silo_extracted"), "20260312")
})


test_that("read_silo_data calculates rolling accumulations correctly", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  # Create minimal data with known rainfall
  dates <- seq(as.Date("2013-01-01"), by = "day", length.out = 7)
  lines <- c(
    "latitude,longitude,YYYY-MM-DD,daily_rain,daily_rain_source,metadata",
    sprintf(' -32.0500, 115.8500,%s,%8.1f,25,""',
            format(dates), c(0, 0, 10, 5, 0, 20, 0))
  )
  writeLines(lines, temp_file)

  silo <- read_silo_data(temp_file)

  # rain_3d: sum of current + 2 preceding days
  # Day 1: 0
  # Day 2: 0+0 = 0
  # Day 3: 0+0+10 = 10
  # Day 4: 0+10+5 = 15
  # Day 5: 10+5+0 = 15
  # Day 6: 5+0+20 = 25
  # Day 7: 0+20+0 = 20
  expect_equal(silo$rain_3d, c(0, 0, 10, 15, 15, 25, 20))

  # rain_7d: sum of current + 6 preceding days
  # Day 7: 0+0+10+5+0+20+0 = 35
  expect_equal(silo$rain_7d[7], 35)
})


test_that("read_silo_data subsets by date range", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_silo_csv(temp_file, n_days = 30, start_date = "2013-03-26")

  silo <- read_silo_data(temp_file,
                          start_date = "2013-04-01",
                          end_date = "2013-04-10")

  expect_equal(nrow(silo), 10)
  expect_equal(min(silo$date), as.Date("2013-04-01"))
  expect_equal(max(silo$date), as.Date("2013-04-10"))
})


test_that("read_silo_data handles extra SILO columns", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_silo_csv(temp_file, n_days = 10, include_extra_cols = TRUE)

  silo <- read_silo_data(temp_file)

  # Should have the extra columns without the source columns
  expect_true("max_temp" %in% names(silo))
  expect_true("min_temp" %in% names(silo))
  expect_false("daily_rain_source" %in% names(silo))
  expect_false("max_temp_source" %in% names(silo))
})


test_that("read_silo_data errors on non-SILO files", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  writeLines("col1,col2,col3\n1,2,3", temp_file)

  expect_error(read_silo_data(temp_file), "does not appear to be a SILO CSV")
})


test_that("read_silo_data errors on missing file", {
  expect_error(read_silo_data("nonexistent.csv"), "File not found")
})


test_that("compute_rolling_sum handles edge cases", {
  # Simple case
  expect_equal(compute_rolling_sum(c(1, 2, 3, 4, 5), window = 3),
               c(1, 3, 6, 9, 12))

  # Window larger than data
  expect_equal(compute_rolling_sum(c(1, 2), window = 5),
               c(1, 3))

  # Single value
  expect_equal(compute_rolling_sum(5, window = 3), 5)
})


test_that("print.silo_data produces output", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  create_sample_silo_csv(temp_file, n_days = 10)
  silo <- read_silo_data(temp_file)

  output <- capture.output(print(silo))
  expect_true(any(grepl("SILO Daily Weather Data", output)))
  expect_true(any(grepl("Location:", output)))
  expect_true(any(grepl("Rainfall:", output)))
})
