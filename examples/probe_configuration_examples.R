#' Probe Configuration System Examples
#'
#' This file demonstrates the probe configuration detection and validation
#' system developed for the sapFluxR package.

library(sapFluxR)

# Example 1: Basic Configuration Detection ----

# Load sample sap flow data
data_file <- system.file("extdata", "sample_ict_data.csv", package = "sapFluxR")
sap_data <- read_sap_data(data_file)

# Automatically detect probe configuration
config <- detect_probe_config(sap_data)
print(config)

# Expected output:
# Auto-detected configuration: three_probe_symmetric
# Probe Configuration: three_probe_symmetric
# Type: HRM Standard
# Sensors: upstream = -6 mm, downstream = 6 mm
# Compatible methods: HRM, MHR, Tmax_Coh, Tmax_Klu, HRMXa, HRMXb, DMA
# Recommended priority: HRM > DMA > MHR > Tmax_Coh

# Example 2: Manual Configuration Specification ----

# Specify configuration explicitly using metadata
metadata <- list(probe_config = "three_probe_asymmetric")
config_manual <- detect_probe_config(sap_data, metadata)
print(config_manual)

# Example 3: Working with Standard Configurations ----

# Get all available standard configurations
standard_configs <- get_standard_configs()
names(standard_configs)

# Examine specific configurations
symmetric_config <- standard_configs$three_probe_symmetric
asymmetric_config <- standard_configs$three_probe_asymmetric
extended_config <- standard_configs$four_probe_extended
advanced_config <- standard_configs$four_probe_advanced

# Check method compatibility
symmetric_config$is_method_compatible("HRM")     # TRUE
symmetric_config$is_method_compatible("CHPM")    # FALSE (limited)
asymmetric_config$is_method_compatible("CHPM")   # TRUE
extended_config$is_method_compatible("DRM")      # TRUE

# Example 4: Configuration Validation ----

# Validate configuration for specific methods
validation <- validate_probe_config(config, methods = c("HRM", "MHR", "DMA"))
cat("Configuration valid:", validation$valid, "\n")

if (!validation$valid) {
  cat("Issues:", paste(validation$issues, collapse = "; "), "\n")
}

if (length(validation$warnings) > 0) {
  cat("Warnings:", paste(validation$warnings, collapse = "; "), "\n")
}

# Example 5: Data Validation Against Configuration ----

# Validate actual data against the detected configuration
data_validation <- validate_data_for_config(sap_data, config, methods = c("HRM", "DMA"))

cat("Data validation results:\n")
cat("Valid:", data_validation$valid, "\n")
cat("Configuration:", data_validation$config_name, "\n")
cat("Methods validated:", paste(data_validation$methods_validated, collapse = ", "), "\n")

if (length(data_validation$issues) > 0) {
  cat("Issues found:\n")
  for (issue in data_validation$issues) {
    cat("- ", issue, "\n")
  }
}

if (length(data_validation$warnings) > 0) {
  cat("Warnings:\n")
  for (warning in data_validation$warnings) {
    cat("- ", warning, "\n")
  }
}

# Example 6: Method Compatibility Matrix ----

# View the method compatibility matrix
print_compatibility_matrix()

# Get compatibility matrix as data frame
compatibility_df <- get_method_compatibility_matrix()
print(compatibility_df)

# Example 7: Creating Custom Probe Configuration ----

# Create a custom probe configuration for special research setup
custom_config <- ProbeConfiguration$new(
  config_name = "custom_research_setup",
  config_type = "Custom Research Configuration",
  heater_position = 0,
  sensor_positions = list(
    upstream = -8,      # Further upstream
    downstream = 12,    # Further downstream
    radial = 8         # Additional radial sensor
  ),
  probe_diameter = 2.0,  # Larger probe
  thermal_diffusivity = 0.0030,  # Species-specific value
  compatible_methods = c("HRM", "Tmax_Coh", "MHR"),
  method_priorities = c("HRM", "MHR", "Tmax_Coh"),
  required_parameters = list(
    HRM = list(thermal_diffusivity = "specified", extended_spacing = TRUE),
    MHR = list(thermal_diffusivity = "specified"),
    Tmax_Coh = list(extended_downstream_distance = TRUE)
  )
)

print(custom_config)

# Validate the custom configuration
custom_validation <- validate_probe_config(custom_config, methods = c("HRM", "MHR"))
cat("Custom config valid:", custom_validation$valid, "\n")

# Example 8: Method Parameter Requirements ----

# Get required parameters for different methods
hrm_params <- config$get_method_parameters("HRM")
print("HRM parameters required:")
print(hrm_params)

dma_params <- config$get_method_parameters("DMA")
print("DMA parameters required:")
print(dma_params)

# Check which parameters are needed for incompatible methods
if (config$is_method_compatible("CHPM")) {
  chpm_params <- config$get_method_parameters("CHPM")
  print("CHPM parameters:")
  print(chmp_params)
} else {
  cat("CHMP not compatible with this configuration\n")
}

# Example 9: Configuration Recommendations ----

# Get recommended methods in priority order
recommended_methods <- config$get_recommended_methods()
cat("Recommended methods in priority order:\n")
for (i in seq_along(recommended_methods)) {
  method <- recommended_methods[i]
  priority <- i
  cat(sprintf("%d. %s\n", priority, method))
}

# Example 10: Handling Configuration Detection Failures ----

# Simulate data with insufficient sensors
minimal_data <- sap_data
minimal_data$measurements$di <- NULL
minimal_data$measurements$ui <- NULL

# This should fail gracefully
tryCatch({
  failed_config <- detect_probe_config(minimal_data)
}, error = function(e) {
  cat("Configuration detection failed as expected:", e$message, "\n")

  # Provide user guidance
  available_sensors <- names(minimal_data$measurements)[
    names(minimal_data$measurements) %in% c("do", "di", "uo", "ui", "dt", "ur")
  ]
  cat("Available sensors:", paste(available_sensors, collapse = ", "), "\n")
  cat("Minimum requirements:\n")
  cat("- HRM: do, di, uo, ui\n")
  cat("- CHPM: do, uo (di, ui recommended)\n")
  cat("- T-max: at least do or uo\n")
})

# Example 11: Advanced Validation Scenarios ----

# Test with realistic heating data simulation
# Create mock data with proper heating signatures
realistic_data <- sap_data

# Add realistic heating pattern to demonstrate validation
if ("pulse_id" %in% names(realistic_data$measurements)) {

  # Select first pulse for modification
  pulse_1_indices <- which(realistic_data$measurements$pulse_id == 1)

  if (length(pulse_1_indices) >= 100) {
    # Simulate heating starting after 30 seconds (pre-pulse period)
    heat_start <- 31
    n_points <- length(pulse_1_indices)

    # Create heating effect
    time_since_heat <- pmax(0, seq_len(n_points) - heat_start)
    heating_curve <- 0.8 * exp(-time_since_heat / 40) * (time_since_heat > 0)

    # Apply differential heating (downstream > upstream)
    realistic_data$measurements$do[pulse_1_indices] <-
      realistic_data$measurements$do[pulse_1_indices] + heating_curve * 1.3
    realistic_data$measurements$di[pulse_1_indices] <-
      realistic_data$measurements$di[pulse_1_indices] + heating_curve * 1.2
    realistic_data$measurements$uo[pulse_1_indices] <-
      realistic_data$measurements$uo[pulse_1_indices] + heating_curve * 0.9
    realistic_data$measurements$ui[pulse_1_indices] <-
      realistic_data$measurements$ui[pulse_1_indices] + heating_curve * 0.8
  }
}

# Validate the enhanced data
enhanced_validation <- validate_data_for_config(realistic_data, config, methods = c("HRM"))
cat("Enhanced data validation:\n")
cat("Valid:", enhanced_validation$valid, "\n")

# Example 12: Configuration Comparison ----

# Compare different configurations for the same data
configs_to_test <- list(
  symmetric = standard_configs$three_probe_symmetric,
  asymmetric = standard_configs$three_probe_asymmetric
)

cat("Configuration comparison:\n")
for (config_name in names(configs_to_test)) {
  test_config <- configs_to_test[[config_name]]

  # Test data compatibility
  compat_test <- validate_data_for_config(sap_data, test_config)

  cat(sprintf("  %s: Valid = %s, Issues = %d, Warnings = %d\n",
              config_name, compat_test$valid,
              length(compat_test$issues), length(compat_test$warnings)))

  # Show compatible methods
  methods <- test_config$get_recommended_methods()
  cat(sprintf("    Recommended methods: %s\n", paste(methods[1:3], collapse = ", ")))
}

# Example 13: Quality Control Integration ----

# The validation system integrates quality control checks
# This demonstrates how the system catches data quality issues

cat("\nQuality control demonstration:\n")

# Create data with quality issues for demonstration
quality_test_data <- sap_data

# Issue 1: Constant sensor (sensor failure)
quality_test_data$measurements$do[1:50] <- 20.000

# Issue 2: Large baseline differences (alignment problem)
quality_test_data$measurements$uo <- quality_test_data$measurements$uo + 2.5

# Issue 3: Excessive missing values
quality_test_data$measurements$di[1:80] <- NA

# Validate problematic data
quality_validation <- validate_data_for_config(quality_test_data, config)

cat("Quality issues detected:\n")
for (issue in quality_validation$issues) {
  cat("- ISSUE:", issue, "\n")
}
for (warning in quality_validation$warnings) {
  cat("- WARNING:", warning, "\n")
}

cat("\n=== Probe Configuration System Examples Complete ===\n")
cat("The system successfully:\n")
cat("1. Detects probe configurations automatically\n")
cat("2. Validates configurations against method requirements\n")
cat("3. Checks data quality against configuration specs\n")
cat("4. Provides method compatibility guidance\n")
cat("5. Handles custom configurations\n")
cat("6. Integrates quality control checks\n")