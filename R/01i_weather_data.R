# Weather Data Import and Processing Functions
#
# Functions for importing environmental/weather data and calculating
# vapor pressure deficit (VPD) for sapfluxr analyses.

#' Read and process weather data from CSV files
#'
#' Intelligently imports weather data from CSV files, automatically detecting
#' columns for datetime, air temperature, relative humidity, and atmospheric pressure.
#' The function uses heuristics based on column names and data ranges to identify
#' the appropriate columns.
#'
#' @param file_path Character string. Path to the CSV file containing weather data.
#' @param datetime_col Character string or NULL. Name or index of datetime column.
#'   If NULL (default), function will attempt to auto-detect.
#' @param temp_col Character string or NULL. Name or index of temperature column.
#'   If NULL (default), function will attempt to auto-detect.
#' @param rh_col Character string or NULL. Name or index of relative humidity column.
#'   If NULL (default), function will attempt to auto-detect.
#' @param pressure_col Character string or NULL. Name or index of atmospheric pressure column.
#'   If NULL (default), function will attempt to auto-detect.
#' @param confirm Logical. If TRUE and auto-detection is ambiguous, prompts user
#'   for confirmation. Default is TRUE.
#'
#' @return A tibble with standardised column names:
#'   \item{datetime}{POSIXct datetime}
#'   \item{air_temp_c}{Air temperature in degrees Celsius}
#'   \item{relative_humidity}{Relative humidity as percentage (0-100)}
#'   \item{pressure_kpa}{Atmospheric pressure in kPa (optional)}
#'
#' @details
#' The function uses the following heuristics for auto-detection:
#' \itemize{
#'   \item Temperature: Values typically < 50°C, column names containing "temp", "air", "ta"
#'   \item Relative Humidity: Values 0-100, column names containing "rh", "humidity", "humid"
#'   \item Pressure: Values around 80-110 kPa, column names containing "pressure", "press", "pa", "kpa"
#'   \item Datetime: Column names containing "date", "time", "datetime", or parseable datetime values
#' }
#'
#' @examples
#' \dontrun{
#' # Automatic column detection
#' weather <- read_weather_data("weather_station.csv")
#'
#' # Manual column specification
#' weather <- read_weather_data(
#'   "weather_station.csv",
#'   datetime_col = "timestamp",
#'   temp_col = "AirTemp",
#'   rh_col = "RH"
#' )
#' }
#'
#' @family weather data functions
#' @export
read_weather_data <- function(file_path,
                              datetime_col = NULL,
                              temp_col = NULL,
                              rh_col = NULL,
                              pressure_col = NULL,
                              confirm = TRUE) {

  # Check file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Read the CSV file
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)

  if (nrow(raw_data) == 0) {
    stop("The weather data file is empty")
  }

  # Auto-detect columns if not specified
  if (is.null(datetime_col)) {
    datetime_col <- detect_datetime_column(raw_data)
  }

  if (is.null(temp_col)) {
    temp_col <- detect_temperature_column(raw_data)
  }

  if (is.null(rh_col)) {
    rh_col <- detect_humidity_column(raw_data)
  }

  if (is.null(pressure_col)) {
    pressure_col <- detect_pressure_column(raw_data)
  }

  # Validate detections
  if (is.null(datetime_col)) {
    stop("Could not auto-detect datetime column. Please specify datetime_col parameter.")
  }

  if (is.null(temp_col)) {
    stop("Could not auto-detect temperature column. Please specify temp_col parameter.")
  }

  if (is.null(rh_col)) {
    stop("Could not auto-detect relative humidity column. Please specify rh_col parameter.")
  }

  # Extract and rename columns
  result <- tibble::tibble(
    datetime = parse_datetime_column(raw_data[[datetime_col]]),
    air_temp_c = as.numeric(raw_data[[temp_col]]),
    relative_humidity = as.numeric(raw_data[[rh_col]])
  )

  # Add pressure if detected
  if (!is.null(pressure_col)) {
    result$pressure_kpa <- as.numeric(raw_data[[pressure_col]])
  }

  # Validate data ranges
  result <- validate_weather_data(result)

  # Add metadata
  attr(result, "source_file") <- file_path
  attr(result, "import_time") <- Sys.time()
  attr(result, "column_mapping") <- list(
    datetime = datetime_col,
    temperature = temp_col,
    humidity = rh_col,
    pressure = pressure_col
  )

  class(result) <- c("weather_data", class(result))

  return(result)
}


#' Detect datetime column in weather data
#'
#' @param data A data frame
#' @return Character string of column name, or NULL if not found
#' @keywords internal
detect_datetime_column <- function(data) {
  col_names <- tolower(names(data))

  # Look for common datetime column names
  datetime_patterns <- c("datetime", "date_time", "timestamp", "time", "date")

  for (pattern in datetime_patterns) {
    matches <- grep(pattern, col_names, value = TRUE)
    if (length(matches) > 0) {
      return(names(data)[which(col_names == matches[1])])
    }
  }

  # Try to parse first column as datetime
  first_col <- data[[1]]
  if (is.character(first_col) || is.factor(first_col)) {
    test_parse <- try(lubridate::parse_date_time(first_col[1:min(10, length(first_col))],
                                                  orders = c("dmy HM", "mdy HM", "ymd HM",
                                                            "dmy HMS", "mdy HMS", "ymd HMS")),
                     silent = TRUE)
    if (!inherits(test_parse, "try-error")) {
      return(names(data)[1])
    }
  }

  return(NULL)
}


#' Detect temperature column in weather data
#'
#' @param data A data frame
#' @return Character string of column name, or NULL if not found
#' @keywords internal
detect_temperature_column <- function(data) {
  col_names <- tolower(names(data))

  # Look for temperature keywords
  temp_patterns <- c("temp", "air", "ta", "t_air", "air_temp")

  candidates <- c()
  for (pattern in temp_patterns) {
    matches <- grep(pattern, col_names)
    if (length(matches) > 0) {
      candidates <- c(candidates, matches)
    }
  }

  # If we have candidates, check their data ranges
  if (length(candidates) > 0) {
    for (idx in unique(candidates)) {
      col_data <- as.numeric(data[[idx]])
      col_data <- col_data[!is.na(col_data)]

      # Temperature heuristic: typically -40 to 50°C for environmental data
      if (length(col_data) > 0 &&
          min(col_data, na.rm = TRUE) >= -40 &&
          max(col_data, na.rm = TRUE) <= 50 &&
          mean(col_data, na.rm = TRUE) < 50) {
        return(names(data)[idx])
      }
    }
  }

  # Fallback: look for numeric columns in typical temperature range
  for (i in seq_along(data)) {
    if (is.numeric(data[[i]])) {
      col_data <- data[[i]][!is.na(data[[i]])]
      if (length(col_data) > 0 &&
          min(col_data) >= -40 &&
          max(col_data) <= 50 &&
          mean(col_data) < 50) {
        return(names(data)[i])
      }
    }
  }

  return(NULL)
}


#' Detect relative humidity column in weather data
#'
#' @param data A data frame
#' @return Character string of column name, or NULL if not found
#' @keywords internal
detect_humidity_column <- function(data) {
  col_names <- tolower(names(data))

  # Look for humidity keywords
  rh_patterns <- c("rh", "humidity", "humid", "relative")

  candidates <- c()
  for (pattern in rh_patterns) {
    matches <- grep(pattern, col_names)
    if (length(matches) > 0) {
      candidates <- c(candidates, matches)
    }
  }

  # Check candidates for typical RH range (0-100)
  if (length(candidates) > 0) {
    for (idx in unique(candidates)) {
      col_data <- as.numeric(data[[idx]])
      col_data <- col_data[!is.na(col_data)]

      # RH heuristic: 0-100%
      if (length(col_data) > 0 &&
          min(col_data, na.rm = TRUE) >= 0 &&
          max(col_data, na.rm = TRUE) <= 100) {
        return(names(data)[idx])
      }
    }
  }

  # Fallback: look for numeric columns in 0-100 range
  for (i in seq_along(data)) {
    if (is.numeric(data[[i]])) {
      col_data <- data[[i]][!is.na(data[[i]])]
      if (length(col_data) > 0 &&
          min(col_data) >= 0 &&
          max(col_data) <= 100 &&
          mean(col_data) > 10 &&  # RH rarely stays below 10%
          mean(col_data) < 100) {  # RH rarely at 100% constantly
        # Make sure it's not temperature (would fail the <50 check earlier)
        if (max(col_data) > 50) {  # Likely humidity, not temperature
          return(names(data)[i])
        }
      }
    }
  }

  return(NULL)
}


#' Detect atmospheric pressure column in weather data
#'
#' @param data A data frame
#' @return Character string of column name, or NULL if not found
#' @keywords internal
detect_pressure_column <- function(data) {
  col_names <- tolower(names(data))

  # Look for pressure keywords
  pressure_patterns <- c("pressure", "press", "atm", "kpa", "pa", "bar")

  candidates <- c()
  for (pattern in pressure_patterns) {
    matches <- grep(pattern, col_names)
    if (length(matches) > 0) {
      candidates <- c(candidates, matches)
    }
  }

  # Check candidates for typical pressure range (80-110 kPa)
  if (length(candidates) > 0) {
    for (idx in unique(candidates)) {
      col_data <- as.numeric(data[[idx]])
      col_data <- col_data[!is.na(col_data)]

      # Pressure heuristic: 80-110 kPa (typical atmospheric pressure range)
      if (length(col_data) > 0 &&
          min(col_data, na.rm = TRUE) >= 80 &&
          max(col_data, na.rm = TRUE) <= 110) {
        return(names(data)[idx])
      }
    }
  }

  # Fallback: look for numeric columns in pressure range
  for (i in seq_along(data)) {
    if (is.numeric(data[[i]])) {
      col_data <- data[[i]][!is.na(data[[i]])]
      if (length(col_data) > 0 &&
          min(col_data) >= 80 &&
          max(col_data) <= 110 &&
          sd(col_data, na.rm = TRUE) < 10) {  # Pressure varies less than temp/RH
        return(names(data)[i])
      }
    }
  }

  return(NULL)
}


#' Parse datetime column with multiple format attempts
#'
#' @param datetime_vec Character or factor vector
#' @return POSIXct vector
#' @keywords internal
parse_datetime_column <- function(datetime_vec) {
  # Try multiple common datetime formats
  formats <- c("dmy HM", "mdy HM", "ymd HM",
               "dmy HMS", "mdy HMS", "ymd HMS",
               "dmy", "mdy", "ymd")

  for (fmt in formats) {
    result <- try(lubridate::parse_date_time(datetime_vec, orders = fmt, quiet = TRUE),
                 silent = TRUE)

    if (!inherits(result, "try-error") && sum(!is.na(result)) > 0.9 * length(result)) {
      return(result)
    }
  }

  stop("Could not parse datetime column. Please check the datetime format.")
}


#' Validate weather data
#'
#' @param data A tibble with weather data
#' @return The input tibble with validation warnings/errors
#' @keywords internal
validate_weather_data <- function(data) {
  issues <- character()

  # Check for NAs
  na_temp <- sum(is.na(data$air_temp_c))
  na_rh <- sum(is.na(data$relative_humidity))

  if (na_temp > 0) {
    issues <- c(issues, sprintf("%d missing temperature values", na_temp))
  }

  if (na_rh > 0) {
    issues <- c(issues, sprintf("%d missing relative humidity values", na_rh))
  }

  # Check temperature range
  temp_range <- range(data$air_temp_c, na.rm = TRUE)
  if (temp_range[1] < -40 || temp_range[2] > 50) {
    issues <- c(issues, sprintf("Temperature outside expected range: %.1f to %.1f°C",
                               temp_range[1], temp_range[2]))
  }

  # Check RH range
  rh_range <- range(data$relative_humidity, na.rm = TRUE)
  if (rh_range[1] < 0 || rh_range[2] > 100) {
    issues <- c(issues, sprintf("Relative humidity outside valid range: %.1f to %.1f%%",
                               rh_range[1], rh_range[2]))
  }

  # Check for duplicate timestamps
  if (any(duplicated(data$datetime))) {
    issues <- c(issues, "Duplicate timestamps detected")
  }

  # Check temporal ordering
  if (is.unsorted(data$datetime)) {
    issues <- c(issues, "Data is not in chronological order")
  }

  if (length(issues) > 0) {
    warning("Weather data validation issues:\n  ", paste(issues, collapse = "\n  "))
  }

  attr(data, "validation_issues") <- issues

  return(data)
}


#' Calculate vapor pressure deficit (VPD)
#'
#' Calculates vapor pressure deficit from air temperature and relative humidity
#' using the Magnus equation. VPD is the difference between the amount of moisture
#' in the air and how much moisture the air can hold when saturated.
#'
#' @param weather_data A weather_data object from \code{read_weather_data()}, or
#'   a data frame with columns \code{air_temp_c} and \code{relative_humidity}.
#' @param temp_col Character string. Name of temperature column if not using
#'   weather_data object. Default is "air_temp_c".
#' @param rh_col Character string. Name of relative humidity column if not using
#'   weather_data object. Default is "relative_humidity".
#' @param magnus_coef Numeric. Magnus equation coefficient (dimensionless).
#'   Default is 17.27 (Alduchov & Eskridge 1996).
#' @param magnus_base_temp Numeric. Magnus equation base temperature (°C).
#'   Default is 237.7°C (Alduchov & Eskridge 1996).
#' @param return_components Logical. If TRUE, returns saturated vapor pressure
#'   and actual vapor pressure in addition to VPD. Default is FALSE.
#'
#' @return If \code{return_components = FALSE}, returns the input data frame with
#'   an additional column \code{vpd_kpa} (vapor pressure deficit in kPa).
#'   If \code{return_components = TRUE}, also adds columns \code{svp_kpa}
#'   (saturated vapor pressure) and \code{avp_kpa} (actual vapor pressure).
#'
#' @details
#' The Magnus-Tetens equation calculates saturated vapor pressure (SVP) as:
#' \deqn{SVP = e_0 \times \exp\left(\frac{\alpha \times T}{\beta + T}\right)}
#'
#' Where:
#' \itemize{
#'   \item \eqn{e_0} = 0.61078 kPa (reference saturation vapor pressure at 0°C)
#'   \item \eqn{T} = air temperature (°C)
#'   \item \eqn{\alpha} = 17.27 (dimensionless coefficient, \code{magnus_coef})
#'   \item \eqn{\beta} = 237.7°C (base temperature, \code{magnus_base_temp})
#' }
#'
#' Actual vapor pressure (AVP) is calculated from relative humidity:
#' \deqn{AVP = \frac{RH}{100} \times SVP}
#'
#' Vapor pressure deficit is then:
#' \deqn{VPD = SVP - AVP}
#'
#' Default parameters (α=17.27, β=237.7) are appropriate for temperatures
#' between 0-60°C. For sub-zero temperatures, consider adjusting parameters.
#'
#' @references
#' Alduchov, O. A., & Eskridge, R. E. (1996). Improved Magnus form approximation
#' of saturation vapor pressure. Journal of Applied Meteorology, 35(4), 601-609.
#'
#' @examples
#' \dontrun{
#' # Using weather_data object
#' weather <- read_weather_data("weather_station.csv")
#' weather_vpd <- calc_vpd(weather)
#'
#' # Using custom data frame
#' my_data <- data.frame(
#'   temp = c(20, 25, 30),
#'   rh = c(50, 60, 70)
#' )
#' my_data_vpd <- calc_vpd(my_data, temp_col = "temp", rh_col = "rh")
#'
#' # Return all components
#' weather_full <- calc_vpd(weather, return_components = TRUE)
#'
#' # Custom Magnus parameters (for sub-zero temperatures)
#' weather_vpd <- calc_vpd(weather, magnus_coef = 17.5, magnus_base_temp = 240.0)
#' }
#'
#' @family weather data functions
#' @export
calc_vpd <- function(weather_data,
                     temp_col = "air_temp_c",
                     rh_col = "relative_humidity",
                     magnus_coef = 17.27,
                     magnus_base_temp = 237.7,
                     return_components = FALSE) {

  # Validate inputs
  if (!is.data.frame(weather_data)) {
    stop("weather_data must be a data frame or weather_data object")
  }

  if (!temp_col %in% names(weather_data)) {
    stop("Temperature column '", temp_col, "' not found in data")
  }

  if (!rh_col %in% names(weather_data)) {
    stop("Relative humidity column '", rh_col, "' not found in data")
  }

  # Extract temperature and humidity
  temp <- weather_data[[temp_col]]
  rh <- weather_data[[rh_col]]

  # Check for valid ranges
  if (any(rh < 0 | rh > 100, na.rm = TRUE)) {
    warning("Some relative humidity values are outside the valid range (0-100%)")
  }

  # Calculate saturated vapor pressure (SVP) using Magnus-Tetens equation
  # SVP = e_0 * exp((alpha * T) / (beta + T))
  # where e_0 = 0.61078 kPa, alpha = magnus_coef, beta = magnus_base_temp
  svp_kpa <- 0.61078 * exp((magnus_coef * temp) / (magnus_base_temp + temp))

  # Calculate actual vapor pressure (AVP)
  avp_kpa <- (rh / 100) * svp_kpa

  # Calculate vapor pressure deficit (VPD)
  vpd_kpa <- svp_kpa - avp_kpa

  # Add to data frame
  result <- weather_data
  result$vpd_kpa <- vpd_kpa

  if (return_components) {
    result$svp_kpa <- svp_kpa
    result$avp_kpa <- avp_kpa
  }

  return(result)
}


#' Print method for weather_data objects
#'
#' @param x A weather_data object
#' @param ... Additional arguments (not used)
#' @export
print.weather_data <- function(x, ...) {
  cat("Weather Data\n")
  cat("============\n\n")

  source_file <- attr(x, "source_file")
  if (!is.null(source_file)) {
    cat("Source file:", basename(source_file), "\n")
  }

  import_time <- attr(x, "import_time")
  if (!is.null(import_time)) {
    cat("Imported:", format(import_time), "\n")
  }

  cat("Records:", nrow(x), "\n")

  if (nrow(x) > 0) {
    cat("Time range:", format(min(x$datetime)), "to", format(max(x$datetime)), "\n")
  }

  cat("\nColumns:", paste(names(x), collapse = ", "), "\n")

  validation_issues <- attr(x, "validation_issues")
  if (!is.null(validation_issues) && length(validation_issues) > 0) {
    cat("\nValidation issues:\n")
    cat(paste("  -", validation_issues, collapse = "\n"), "\n")
  }

  cat("\n")
  NextMethod()
}
