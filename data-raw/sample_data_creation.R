# Sample Data Creation Script for sapFluxR Package
# This script creates sample datasets for testing and examples

library(tibble)
library(lubridate)

# Create sample ICT current format data
create_sample_ict_current <- function() {

  # Create realistic sap flow measurement data
  n_pulses <- 5
  n_measurements_per_pulse <- 120  # 2 minutes at 1-second intervals

  sample_data <- character(n_pulses)

  for (i in 1:n_pulses) {
    # Create timestamp (30 minutes apart)
    timestamp <- format(as.POSIXct("2024-11-15 10:00:00") + (i-1) * 1800,
                        format = "%Y-%m-%dT%H:%M:%SZ")

    # Create diagnostic data
    diagnostics <- sprintf(
      '{"date":"%s","bv":%.2f,"bc":%.2f,"bt":%.2f,"ep":%d,"ev":%.2f,"ec":%.2f',
      timestamp,
      runif(1, 3.8, 4.2),    # Battery voltage
      sample(c(15, 200), 1),  # Battery current (15 for measurement, 200 for heating)
      runif(1, 20, 35),       # Battery temperature
      1,                      # External power flag
      runif(1, 20, 25),       # External voltage
      runif(1, 15, 90)        # External current
    )

    # Create temperature measurements with realistic heat pulse pattern
    temp_measurements <- character(n_measurements_per_pulse)

    # Baseline temperatures
    baseline_do <- 18.8
    baseline_di <- 18.6
    baseline_uo <- 18.9
    baseline_ui <- 18.7

    for (j in 1:n_measurements_per_pulse) {
      # Add heat pulse effect after 30 seconds (pre-pulse period)
      if (j > 30) {
        time_after_pulse <- j - 30
        # Downstream sensors get stronger, earlier signal
        temp_do <- baseline_do + 1.2 * exp(-time_after_pulse / 20) + rnorm(1, 0, 0.001)
        temp_di <- baseline_di + 1.0 * exp(-time_after_pulse / 18) + rnorm(1, 0, 0.001)
        # Upstream sensors get weaker, later signal
        temp_uo <- baseline_uo + 0.8 * exp(-(time_after_pulse-5) / 25) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
        temp_ui <- baseline_ui + 0.6 * exp(-(time_after_pulse-5) / 22) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
      } else {
        # Pre-pulse temperatures with small random variations
        temp_do <- baseline_do + rnorm(1, 0, 0.001)
        temp_di <- baseline_di + rnorm(1, 0, 0.001)
        temp_uo <- baseline_uo + rnorm(1, 0, 0.001)
        temp_ui <- baseline_ui + rnorm(1, 0, 0.001)
      }

      temp_measurements[j] <- sprintf(
        ',{"do":%.3f,"di":%.3f,"uo":%.3f,"ui":%.3f}',
        temp_do, temp_di, temp_uo, temp_ui
      )
    }

    # Combine diagnostics and measurements
    sample_data[i] <- paste0("[", diagnostics, paste(temp_measurements, collapse = ""), "]")
  }

  return(paste(sample_data, collapse = "\n"))
}

# Create sample CSV format data
create_sample_csv <- function() {

  n_rows <- 600  # 5 pulses × 120 measurements each

  # Create timestamps
  base_time <- as.POSIXct("2024-11-15 10:00:00")
  timestamps <- character(n_rows)
  pulse_ids <- integer(n_rows)

  idx <- 1
  for (pulse in 1:5) {
    pulse_start_time <- base_time + (pulse - 1) * 1800  # 30 min apart
    for (measurement in 1:120) {
      timestamps[idx] <- format(pulse_start_time + (measurement - 1), "%Y-%m-%d %H:%M:%S")
      pulse_ids[idx] <- pulse
      idx <- idx + 1
    }
  }

  # Create temperature data similar to ICT format
  temperatures <- data.frame(
    datetime = timestamps,
    pulse_id = pulse_ids,
    do = numeric(n_rows),
    di = numeric(n_rows),
    uo = numeric(n_rows),
    ui = numeric(n_rows),
    batt_volt = numeric(n_rows),
    batt_current = numeric(n_rows),
    stringsAsFactors = FALSE
  )

  # Fill with realistic data
  for (i in 1:n_rows) {
    pulse_id <- pulse_ids[i]
    measurement_in_pulse <- ((i - 1) %% 120) + 1

    # Baseline temperatures
    baseline_do <- 18.8
    baseline_di <- 18.6
    baseline_uo <- 18.9
    baseline_ui <- 18.7

    # Add heat pulse effect
    if (measurement_in_pulse > 30) {
      time_after_pulse <- measurement_in_pulse - 30
      temperatures$do[i] <- baseline_do + 1.2 * exp(-time_after_pulse / 20) + rnorm(1, 0, 0.001)
      temperatures$di[i] <- baseline_di + 1.0 * exp(-time_after_pulse / 18) + rnorm(1, 0, 0.001)
      temperatures$uo[i] <- baseline_uo + 0.8 * exp(-(time_after_pulse-5) / 25) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
      temperatures$ui[i] <- baseline_ui + 0.6 * exp(-(time_after_pulse-5) / 22) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
    } else {
      temperatures$do[i] <- baseline_do + rnorm(1, 0, 0.001)
      temperatures$di[i] <- baseline_di + rnorm(1, 0, 0.001)
      temperatures$uo[i] <- baseline_uo + rnorm(1, 0, 0.001)
      temperatures$ui[i] <- baseline_ui + rnorm(1, 0, 0.001)
    }

    # Add diagnostic data
    temperatures$batt_volt[i] <- runif(1, 3.8, 4.2)
    temperatures$batt_current[i] <- ifelse(measurement_in_pulse <= 2, 200, 15)  # High during heating
  }

  return(temperatures)
}

# Create sample legacy format data (tab-delimited)
create_sample_legacy <- function() {

  # Create header
  header <- "Date\tTime\tPulse_ID\tDO\tDI\tUO\tUI\tBatt_V\tExt_V"

  # Create data rows
  n_rows <- 600
  data_rows <- character(n_rows)

  base_time <- as.POSIXct("2024-11-15 10:00:00")

  idx <- 1
  for (pulse in 1:5) {
    pulse_start_time <- base_time + (pulse - 1) * 1800
    for (measurement in 1:120) {
      timestamp <- pulse_start_time + (measurement - 1)
      date_str <- format(timestamp, "%Y-%m-%d")
      time_str <- format(timestamp, "%H:%M:%S")

      # Generate temperatures similar to other formats
      baseline_do <- 18.8
      baseline_di <- 18.6
      baseline_uo <- 18.9
      baseline_ui <- 18.7

      if (measurement > 30) {
        time_after_pulse <- measurement - 30
        temp_do <- baseline_do + 1.2 * exp(-time_after_pulse / 20) + rnorm(1, 0, 0.001)
        temp_di <- baseline_di + 1.0 * exp(-time_after_pulse / 18) + rnorm(1, 0, 0.001)
        temp_uo <- baseline_uo + 0.8 * exp(-(time_after_pulse-5) / 25) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
        temp_ui <- baseline_ui + 0.6 * exp(-(time_after_pulse-5) / 22) * (time_after_pulse > 5) + rnorm(1, 0, 0.001)
      } else {
        temp_do <- baseline_do + rnorm(1, 0, 0.001)
        temp_di <- baseline_di + rnorm(1, 0, 0.001)
        temp_uo <- baseline_uo + rnorm(1, 0, 0.001)
        temp_ui <- baseline_ui + rnorm(1, 0, 0.001)
      }

      data_rows[idx] <- sprintf(
        "%s\t%s\t%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.2f\t%.2f",
        date_str, time_str, pulse,
        temp_do, temp_di, temp_uo, temp_ui,
        runif(1, 3.8, 4.2), runif(1, 20, 25)
      )
      idx <- idx + 1
    }
  }

  return(paste(c(header, data_rows), collapse = "\n"))
}

# Create the sample data files
if (!dir.exists("inst/extdata")) {
  dir.create("inst/extdata", recursive = TRUE)
}

# Create ICT current format sample
cat("Creating sample ICT current format data...\n")
ict_current_data <- create_sample_ict_current()
writeLines(ict_current_data, "inst/extdata/sample_ict_current.txt")

# Create CSV format sample
cat("Creating sample CSV format data...\n")
csv_data <- create_sample_csv()
write.csv(csv_data, "inst/extdata/sample_ict_data.csv", row.names = FALSE)

# Create legacy format sample
cat("Creating sample legacy format data...\n")
legacy_data <- create_sample_legacy()
writeLines(legacy_data, "inst/extdata/sample_ict_legacy.txt")

# Create a smaller dataset for quick examples
cat("Creating quick example data...\n")
quick_example_data <- create_sample_ict_current()
# Take only first 2 pulses for quick examples
quick_lines <- strsplit(quick_example_data, "\n")[[1]][1:2]
writeLines(quick_lines, "inst/extdata/quick_example.txt")

cat("Sample data creation complete!\n")
cat("Files created:\n")
cat("- inst/extdata/sample_ict_current.txt (ICT current format)\n")
cat("- inst/extdata/sample_ict_data.csv (CSV format)\n")
cat("- inst/extdata/sample_ict_legacy.txt (Legacy format)\n")
cat("- inst/extdata/quick_example.txt (Small example)\n")

# Test that the sample data can be imported
cat("\nTesting sample data import...\n")
tryCatch({
  # This would work once the package is built
  # sample_file <- system.file("extdata", "sample_ict_current.txt", package = "sapFluxR")
  # test_data <- read_sap_data(sample_file)
  # cat("Sample data import test: PASSED\n")
  cat("Sample data import test: SKIPPED (run after package installation)\n")
}, error = function(e) {
  cat("Sample data import test: FAILED -", e$message, "\n")
})