## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("neez777/sapfluxr")

## -----------------------------------------------------------------------------
library(sapfluxr)
library(ggplot2)  # For plotting

## ----eval=FALSE---------------------------------------------------------------
# # Import raw heat pulse data
# heat_pulse_data <- read_heat_pulse_data("path/to/your/data.txt")

## ----include=FALSE------------------------------------------------------------
# Create example data for this vignette
set.seed(42)
n_pulses <- 10

# Simulate realistic temperature data
measurements <- data.frame(
  pulse_id = rep(1:n_pulses, each = 150),
  datetime = rep(seq(as.POSIXct("2024-03-15 08:00:00"),
                     by = "30 min", length.out = n_pulses), each = 150)
)

# Add time within each pulse
measurements$datetime <- measurements$datetime + rep(0:149, times = n_pulses)

# Simulate temperature data with realistic heat pulse response
for (i in 1:n_pulses) {
  pulse_rows <- (i-1)*150 + 1:150
  time_sec <- 0:149

  # Pre-pulse baseline temperatures
  baseline_do <- 18.5 + rnorm(1, 0, 0.1)
  baseline_di <- 18.4 + rnorm(1, 0, 0.1)
  baseline_uo <- 18.6 + rnorm(1, 0, 0.1)
  baseline_ui <- 18.3 + rnorm(1, 0, 0.1)

  # Heat pulse response (rises after 30 sec, peaks around 60-80 sec, decays)
  heat_response <- ifelse(time_sec < 30, 0,
                          exp(-(time_sec - 60)^2 / 800) * runif(1, 0.8, 1.2))

  measurements$do[pulse_rows] <- baseline_do + heat_response * 1.2 + rnorm(150, 0, 0.02)
  measurements$di[pulse_rows] <- baseline_di + heat_response * 1.0 + rnorm(150, 0, 0.02)
  measurements$uo[pulse_rows] <- baseline_uo + heat_response * 0.8 + rnorm(150, 0, 0.01)
  measurements$ui[pulse_rows] <- baseline_ui + heat_response * 0.7 + rnorm(150, 0, 0.01)
}

# Create diagnostic data
diagnostics <- data.frame(
  pulse_id = 1:n_pulses,
  datetime = seq(as.POSIXct("2024-03-15 08:00:00"), by = "30 min", length.out = n_pulses),
  batt_volt = runif(n_pulses, 3.9, 4.1),
  batt_current = c(rnorm(n_pulses-1, 15, 2), rnorm(1, 180, 10)),  # One high heating pulse
  batt_temp = rnorm(n_pulses, 25, 2),
  external_volt = rnorm(n_pulses, 21, 1),
  external_current = c(rnorm(n_pulses-1, 18, 2), rnorm(1, 75, 5))
)

# Create heat_pulse_data object
heat_pulse_data <- list(
  measurements = measurements,
  diagnostics = diagnostics,
  metadata = list(
    file_path = "example_data.txt",
    file_name = "example_data.txt",
    format = "ict_current",
    import_time = Sys.time(),
    file_size = 125000,
    file_size_mb = 0.12,
    n_pulses = n_pulses,
    n_measurements = nrow(measurements),
    chunk_size = 100000,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    package_version = "0.2.0"
  )
)
class(heat_pulse_data) <- c("heat_pulse_data", "list")

## -----------------------------------------------------------------------------
# Summary of imported data
str(heat_pulse_data, max.level = 2)

# View first few temperature measurements
head(heat_pulse_data$measurements)

# View diagnostic information
head(heat_pulse_data$diagnostics)

## -----------------------------------------------------------------------------
# Calculate using multiple methods
vh_results <- calc_heat_pulse_velocity(
  heat_pulse_data,
  methods = c("HRM", "MHR", "HRMXa", "Tmax_Klu"),
  show_progress = FALSE
)

# View results
head(vh_results)

## -----------------------------------------------------------------------------
# Results structure
colnames(vh_results)

# Quality flags
table(vh_results$quality_flag)

# Summary by method
aggregate(Vh_cm_hr ~ method, data = vh_results, FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = sum(!is.na(x)))
})

## ----fig.width=8, fig.height=5------------------------------------------------
# Plot velocity time series
plot_vh_timeseries(
  vh_results,
  methods = c("HRM", "MHR", "HRMXa"),
  sensor_position = "outer"
)

## ----fig.width=8, fig.height=6------------------------------------------------
# Plot trace for a single pulse
plot_heat_pulse_trace(
  heat_pulse_data,
  vh_results,
  pulse_id = 5,
  show_methods = c("HRM", "HRMXa", "Tmax_Klu")
)

