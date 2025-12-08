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
