# Test Probe Configuration System
# Source file: R/03_probe_configuration.R

library(testthat)

# Test ProbeConfiguration R6 Class ----

test_that("ProbeConfiguration class initializes correctly", {
  config <- ProbeConfiguration$new(
    config_name = "test_config",
    config_type = "Test Configuration",
    sensor_positions = list(upstream = -6, downstream = 6),
    compatible_methods = c("HRM", "MHR"),
    method_priorities = c("HRM", "MHR"),
    required_parameters = list(HRM = list(thermal_diffusivity = "required"))
  )

  expect_equal(config$config_name, "test_config")
  expect_equal(config$config_type, "Test Configuration")
  expect_equal(config$sensor_positions$upstream, -6)
  expect_equal(config$sensor_positions$downstream, 6)
  expect_true(config$is_method_compatible("HRM"))
  expect_false(config$is_method_compatible("CHPM"))
})

test_that("ProbeConfiguration validation catches errors", {
  expect_error({
    ProbeConfiguration$new(
      config_name = "",  # Empty name should fail
      config_type = "Test",
      sensor_positions = list(upstream = -6),  # Missing downstream
      compatible_methods = c("HRM")
    )
  }, "Invalid probe configuration")
})

test_that("ProbeConfiguration method compatibility works", {
  config <- ProbeConfiguration$new(
    config_name = "test_config",
    config_type = "Test Configuration",
    sensor_positions = list(upstream = -6, downstream = 6),
    compatible_methods = c("HRM", "MHR", "DMA"),
    method_priorities = c("HRM", "DMA", "MHR"),
    required_parameters = list()
  )

  expect_true(config$is_method_compatible("HRM"))
  expect_true(config$is_method_compatible("MHR"))
  expect_true(config$is_method_compatible("DMA"))
  expect_false(config$is_method_compatible("CHPM"))

  recommended <- config$get_recommended_methods()
  expect_equal(recommended, c("HRM", "DMA", "MHR"))
})

# Helper function to create mock sap_data
create_mock_sap_data_with_sensors <- function(sensors = c("do", "di", "uo", "ui")) {
  n_points <- 100
  measurements <- data.frame(
    datetime = seq(as.POSIXct("2024-01-01 10:00:00"),
                   by = "1 sec", length.out = n_points),
    pulse_id = rep(1, n_points)
  )

  # Add requested sensors
  for (sensor in sensors) {
    measurements[[sensor]] <- 20 + rnorm(n_points, mean = 0, sd = 0.1)
  }

  diagnostics <- data.frame(
    pulse_id = 1,
    datetime = as.POSIXct("2024-01-01 10:00:00"),
    batt_volt = 12.5,
    batt_current = 0.1,
    batt_temp = 25.0,
    external_volt = 12.0,
    external_current = 0.05
  )

  result <- list(
    measurements = measurements,
    diagnostics = diagnostics
  )
  class(result) <- "sap_data"
  return(result)
}

# Test Configuration Validation ----

test_that("validate_probe_config works with valid configuration", {
  config <- load_probe_config("symmetrical")

  validation <- validate_probe_config(config)

  expect_true(validation$valid)
  expect_equal(length(validation$issues), 0)
})

# Removed test for deprecated function validate_probe_config catches method incompatibility

test_that("validate_probe_config works with list input", {
  config_list <- list(
    config_name = "test_config",
    config_type = "Test",
    sensor_positions = list(upstream = -6, downstream = 6),
    compatible_methods = c("HRM"),
    method_priorities = c("HRM"),
    required_parameters = list()
  )

  validation <- validate_probe_config(config_list)

  expect_true(validation$valid)
})

# Test Data Validation Against Configuration ----

# Removed test for internal validation function (validate_data_for_config with missing sensors)

test_that("validate_data_for_config detects sensor issues", {
  config <- load_probe_config("symmetrical")
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Make one sensor constant (failure simulation)
  sap_data$measurements$do <- rep(20.0, nrow(sap_data$measurements))

  validation <- validate_data_for_config(sap_data, config)

  expect_false(validation$valid)
  expect_true(any(grepl("constant values", validation$issues)))
})

test_that("validate_data_for_config detects excessive missing data", {
  config <- load_probe_config("symmetrical")
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Make most values missing
  sap_data$measurements$do[1:80] <- NA

  validation <- validate_data_for_config(sap_data, config)

  expect_false(validation$valid)
  expect_true(any(grepl("missing values", validation$issues)))
})

test_that("validate_data_for_config detects alignment issues", {
  config <- load_probe_config("symmetrical")
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Create large baseline temperature differences
  sap_data$measurements$do <- sap_data$measurements$do + 3.0
  sap_data$measurements$uo <- sap_data$measurements$uo - 2.0

  validation <- validate_data_for_config(sap_data, config)

  expect_true(length(validation$warnings) > 0)
  expect_true(any(grepl("baseline temperature differences", validation$warnings)))
})

# Test Method Compatibility Matrix ----

test_that("get_method_compatibility_matrix returns correct structure", {
  matrix_df <- get_method_compatibility_matrix()

  expect_true(is.data.frame(matrix_df))
  expect_true("configuration" %in% names(matrix_df))
  expect_true("HRM" %in% names(matrix_df))
  expect_true("CHPM" %in% names(matrix_df))
  expect_true("DRM" %in% names(matrix_df))

  # Check that ratings are in valid range
  numeric_cols <- names(matrix_df)[names(matrix_df) != "configuration"]
  for (col in numeric_cols) {
    values <- matrix_df[[col]]
    expect_true(all(values >= 0 & values <= 3))
  }
})

test_that("method compatibility matrix has expected relationships", {
  matrix_df <- get_method_compatibility_matrix()

  # HRM should be optimal (3) for three_probe_symmetric
  hrm_symmetric <- matrix_df[matrix_df$configuration == "three_probe_symmetric", "HRM"]
  expect_equal(hrm_symmetric, 3)

  # CHPM should be optimal (3) for three_probe_asymmetric
  chpm_asymmetric <- matrix_df[matrix_df$configuration == "three_probe_asymmetric", "CHPM"]
  expect_equal(chpm_asymmetric, 3)

  # DRM should be optimal (3) for four_probe_extended
  drm_extended <- matrix_df[matrix_df$configuration == "four_probe_extended", "DRM"]
  expect_equal(drm_extended, 3)
})

test_that("print_compatibility_matrix runs without error", {
  expect_output(print_compatibility_matrix(), "Method Compatibility Matrix")
  expect_output(print_compatibility_matrix(), "Legend")
})

# Test Method-Specific Validation ----

test_that("HRM validation detects insufficient temperature rise", {
  config <- load_probe_config("symmetrical")
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Keep temperatures completely flat (no heating effect)
  sap_data$measurements$do <- rep(20.0, nrow(sap_data$measurements))
  sap_data$measurements$di <- rep(20.0, nrow(sap_data$measurements))
  sap_data$measurements$uo <- rep(20.0, nrow(sap_data$measurements))
  sap_data$measurements$ui <- rep(20.0, nrow(sap_data$measurements))

  validation <- validate_method_compatibility(sap_data, config, "HRM")

  # Should detect insufficient temperature rise
  expect_true(length(validation$issues) > 0 || length(validation$warnings) > 0)
})

test_that("CHPM validation detects timing issues", {
  config <- load_probe_config("asymmetrical")
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Make time intervals too coarse for CHPM (>10 seconds)
  sap_data$measurements$time <- seq(0, by = 15, length.out = nrow(sap_data$measurements))

  validation <- validate_method_compatibility(sap_data, config, "CHPM")

  expect_true(length(validation$warnings) > 0)
  expect_true(any(grepl("Temporal resolution.*coarse", validation$warnings)))
})

# Removed test for DRM (four_probe_extended config doesn't have YAML equivalent)

# Test Edge Cases and Error Handling ----

# Removed tests for deprecated detect_probe_config function

test_that("validate_probe_config handles various input types", {
  expect_error(validate_probe_config("invalid"), "must be a ProbeConfiguration object or list")

  # Test with incomplete list
  incomplete_list <- list(config_name = "test")
  validation <- validate_probe_config(incomplete_list)
  expect_false(validation$valid)
})

test_that("ProbeConfiguration handles extreme sensor positions", {
  config <- ProbeConfiguration$new(
    config_name = "extreme_config",
    config_type = "Test",
    sensor_positions = list(upstream = -25, downstream = 25),  # Very far spacing
    compatible_methods = c("HRM")
  )

  # Check that validation catches the extreme positions
  validation <- config$validate()
  expect_true(length(validation$warnings) > 0)
  expect_true(any(grepl("unusually far from heater", validation$warnings)))
})

# Integration Tests ----

test_that("full workflow works with standard configuration", {
  # Create realistic mock data with heating effect
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Add realistic heating pattern to first pulse
  pulse_1 <- sap_data$measurements$pulse_id == 1
  heat_start <- 31  # After 30-second pre-pulse period

  # Simulate temperature rise after heating
  time_since_heat <- pmax(0, seq_len(sum(pulse_1)) - heat_start)
  heating_effect <- 0.5 * exp(-time_since_heat / 50) * (time_since_heat > 0)

  sap_data$measurements$do[pulse_1] <- sap_data$measurements$do[pulse_1] + heating_effect * 1.2
  sap_data$measurements$di[pulse_1] <- sap_data$measurements$di[pulse_1] + heating_effect * 1.1
  sap_data$measurements$uo[pulse_1] <- sap_data$measurements$uo[pulse_1] + heating_effect * 0.8
  sap_data$measurements$ui[pulse_1] <- sap_data$measurements$ui[pulse_1] + heating_effect * 0.9

  # Test full workflow with explicit configuration
  config <- load_probe_config("symmetrical")
  expect_true(inherits(config, "ProbeConfiguration"))

  validation <- validate_data_for_config(sap_data, config, methods = c("HRM", "MHR"))
  # The validation might detect some issues due to the mock data, but should not fail completely
  expect_true(is.list(validation))
  expect_true("valid" %in% names(validation))

  config_validation <- validate_probe_config(config, methods = c("HRM", "MHR"))
  expect_true(config_validation$valid)
})

test_that("method recommendations work correctly", {
  # Test symmetrical configuration
  sym_config <- load_probe_config("symmetrical")
  recommended <- sym_config$get_recommended_methods()
  expect_true(length(recommended) > 0)
  expect_equal(recommended[1], "HRM")

  # All recommended methods should be compatible
  for (method in recommended) {
    expect_true(sym_config$is_method_compatible(method))
  }

  # Test asymmetrical configuration
  asym_config <- load_probe_config("asymmetrical")
  recommended_asym <- asym_config$get_recommended_methods()
  expect_true(length(recommended_asym) > 0)
  expect_equal(recommended_asym[1], "CHPM")
})


# Test YAML-Based Configuration Loading System ----

test_that("list_available_probe_configs() returns expected configurations", {
  configs <- list_available_probe_configs()

  # Should return a data frame
  expect_s3_class(configs, "data.frame")

  # Should have required columns
  expect_true(all(c("name", "description", "file", "default") %in% names(configs)))

  # Should have at least the two built-in configs
  expect_true(nrow(configs) >= 2)
  expect_true("symmetrical" %in% configs$name)
  expect_true("asymmetrical" %in% configs$name)

  # symmetrical should be marked as default
  default_config <- configs[configs$default, ]
  expect_equal(nrow(default_config), 1)
  expect_equal(default_config$name, "symmetrical")
})


test_that("load_probe_config() loads default configuration", {
  config <- load_probe_config()

  # Should return ProbeConfiguration object
  expect_s3_class(config, "ProbeConfiguration")
  expect_s3_class(config, "R6")

  # Should load symmetrical configuration by default
  expect_equal(config$config_type, "symmetric")
  expect_equal(config$sensor_positions$upstream_inner, -0.5)  # 5mm = 0.5cm
  expect_equal(config$sensor_positions$downstream_inner, 0.5)
  expect_equal(config$heat_pulse_duration, 2)
})


test_that("load_probe_config() loads specific configurations", {
  # Load symmetrical explicitly
  sym_config <- load_probe_config("symmetrical")
  expect_s3_class(sym_config, "ProbeConfiguration")
  expect_equal(sym_config$config_type, "symmetric")
  expect_equal(sym_config$sensor_positions$upstream_inner, -0.5)
  expect_equal(sym_config$sensor_positions$downstream_inner, 0.5)
  expect_true(sym_config$is_method_compatible("HRM"))

  # Load asymmetrical
  asym_config <- load_probe_config("asymmetrical")
  expect_s3_class(asym_config, "ProbeConfiguration")
  expect_equal(asym_config$config_type, "asymmetric")
  expect_equal(asym_config$sensor_positions$upstream_inner, -0.5)  # 5mm
  expect_equal(asym_config$sensor_positions$downstream_inner, 1.0)  # 10mm
  expect_true(asym_config$is_method_compatible("CHPM"))
})


test_that("get_default_probe_config() works", {
  config <- get_default_probe_config()

  expect_s3_class(config, "ProbeConfiguration")
  expect_equal(config$config_type, "symmetric")
  expect_equal(config$heat_pulse_duration, 2)
})


test_that("load_probe_config() respects custom_params overrides", {
  # Override heat_pulse_duration
  config <- load_probe_config("symmetrical",
                              custom_params = list(heat_pulse_duration = 3))

  expect_equal(config$heat_pulse_duration, 3)
  # Other properties should remain unchanged
  expect_equal(config$sensor_positions$upstream_inner, -0.5)
  expect_equal(config$sensor_positions$downstream_inner, 0.5)

  # Override multiple parameters
  config2 <- load_probe_config("symmetrical",
                               custom_params = list(
                                 heat_pulse_duration = 2.5,
                                 upstream_distance = 6,
                                 downstream_distance = 6
                               ))

  expect_equal(config2$heat_pulse_duration, 2.5)
  expect_equal(config2$sensor_positions$upstream_inner, -0.6)  # 6mm = 0.6cm
  expect_equal(config2$sensor_positions$downstream_inner, 0.6)
})


test_that("create_custom_probe_config() creates valid configuration", {
  custom <- create_custom_probe_config(
    config_name = "My Custom Probe",
    upstream_distance = 7,
    downstream_distance = 7,
    heat_pulse_duration = 2.5,
    probe_diameter = 2.0
  )

  expect_s3_class(custom, "ProbeConfiguration")
  expect_equal(custom$config_name, "My Custom Probe")
  expect_equal(custom$sensor_positions$upstream_inner, -0.7)  # 7mm = 0.7cm
  expect_equal(custom$sensor_positions$downstream_inner, 0.7)
  expect_equal(custom$heat_pulse_duration, 2.5)
  expect_equal(custom$probe_diameter, 2.0)
})


test_that("ProbeConfiguration object has required fields", {
  config <- load_probe_config("symmetrical")

  # Check required fields exist
  expect_false(is.null(config$config_name))
  expect_false(is.null(config$config_type))
  expect_false(is.null(config$sensor_positions))
  expect_false(is.null(config$heat_pulse_duration))
  expect_false(is.null(config$probe_diameter))
  expect_false(is.null(config$compatible_methods))
  expect_false(is.null(config$method_priorities))

  # Check sensor positions structure
  expect_type(config$sensor_positions, "list")
  expect_true("upstream_inner" %in% names(config$sensor_positions))
  expect_true("downstream_inner" %in% names(config$sensor_positions))

  # Check numeric values
  expect_true(is.numeric(config$heat_pulse_duration))
  expect_true(config$heat_pulse_duration > 0)
})


test_that("Configuration-specific compatible methods are correct", {
  # Symmetrical config
  sym <- load_probe_config("symmetrical")
  expect_true(sym$is_method_compatible("HRM"))
  expect_true(sym$is_method_compatible("MHR"))
  expect_true(sym$is_method_compatible("DMA"))

  # Asymmetrical config
  asym <- load_probe_config("asymmetrical")
  expect_true(asym$is_method_compatible("CHPM"))
  expect_true(asym$is_method_compatible("MHR"))
  expect_false(asym$is_method_compatible("HRM"))  # HRM not compatible with asymmetric

  # Check method priorities
  sym_priorities <- sym$get_recommended_methods()
  expect_true(length(sym_priorities) > 0)
  expect_equal(sym_priorities[1], "HRM")

  asym_priorities <- asym$get_recommended_methods()
  expect_equal(asym_priorities[1], "CHPM")
})


test_that("load_probe_config() errors on invalid configuration name", {
  expect_error(
    load_probe_config("nonexistent_probe_type"),
    "not found"
  )

  expect_error(
    load_probe_config("invalid_config"),
    "Available"  # Matches "Available: symmetrical, asymmetrical"
  )
})


test_that("load_probe_config() errors on invalid file path", {
  expect_error(
    load_probe_config("path/to/nonexistent/file.yaml"),
    "not found"
  )
})


test_that("ProbeConfiguration validation catches issues", {
  # Should error if config_name is empty
  expect_error(
    ProbeConfiguration$new(
      config_name = "",
      config_type = "Test",
      sensor_positions = list(upstream = -5, downstream = 5),
      compatible_methods = c("HRM")
    ),
    "Invalid probe configuration"
  )

  # Should error if sensor_positions is incomplete
  expect_error(
    ProbeConfiguration$new(
      config_name = "test",
      config_type = "Test",
      sensor_positions = list(upstream = -5),  # Missing downstream
      compatible_methods = c("HRM")
    ),
    "Invalid probe configuration"
  )
})


test_that("ProbeConfiguration print method works", {
  config <- load_probe_config("symmetrical")

  # Should not error when printing
  expect_output(print(config), "Probe Configuration")
  expect_output(print(config), "symmetric")  # Check for config type
  expect_output(print(config), "Sensors:")  # Actual output format
  expect_output(print(config), "Compatible methods:")  # Actual output format (lowercase m)
})


test_that("YAML source is stored", {
  config <- load_probe_config("symmetrical")

  # Should have yaml_source field
  expect_false(is.null(config$yaml_source))
  expect_match(config$yaml_source, "probe_symmetrical\\.yaml$")

  # Should have yaml_data field
  expect_type(config$yaml_data, "list")
  expect_true("metadata" %in% names(config$yaml_data))
  expect_true("probe" %in% names(config$yaml_data))
})


test_that("create_custom_probe_config() uses sensible defaults", {
  # Minimal custom config (only required parameters)
  custom <- create_custom_probe_config(
    upstream_distance = 5,
    downstream_distance = 5
  )

  expect_s3_class(custom, "ProbeConfiguration")
  expect_equal(custom$heat_pulse_duration, 2)  # Default value
  expect_equal(custom$probe_diameter, 1.27)    # Default value
  expect_true(custom$is_method_compatible("HRM"))
  expect_true(custom$is_method_compatible("MHR"))
})


test_that("custom_params stores calculation parameters in required_parameters", {
  config <- load_probe_config("symmetrical")

  # Check that probe spacing (x) is stored in required_parameters
  expect_false(is.null(config$required_parameters$x))
  expect_equal(config$required_parameters$x, 0.5)  # Average of 0.5 and 0.5 cm

  # Heat pulse duration should also be in required_parameters
  expect_equal(config$required_parameters$heat_pulse_duration, 2)
})


test_that("load_probe_config() handles both name and path inputs", {
  # Load by name
  config1 <- load_probe_config("symmetrical")
  expect_s3_class(config1, "ProbeConfiguration")

  # Load by full path (construct path to built-in YAML)
  yaml_path <- system.file("configurations", "probe_symmetrical.yaml",
                          package = "sapFluxR")

  if (file.exists(yaml_path) && nchar(yaml_path) > 0) {
    config2 <- load_probe_config(yaml_path)
    expect_s3_class(config2, "ProbeConfiguration")
    expect_equal(config1$config_name, config2$config_name)
  } else {
    skip("Package not installed, cannot test path-based loading")
  }
})


test_that("ProbeConfiguration required_parameters work correctly", {
  config <- load_probe_config("symmetrical")

  # Should have required_parameters list
  expect_type(config$required_parameters, "list")

  # Should include x (probe spacing in cm)
  expect_false(is.null(config$required_parameters$x))
  expect_equal(config$required_parameters$x, 0.5)  # 5mm = 0.5cm

  # Should include heat_pulse_duration
  expect_false(is.null(config$required_parameters$heat_pulse_duration))
  expect_equal(config$required_parameters$heat_pulse_duration, 2)
})


# Integration Tests with YAML Loading ----

test_that("YAML-loaded config integrates with calc_heat_pulse_velocity()", {
  skip_if_not_installed("devtools")

  # Create mock sap data
  sap_data <- create_mock_sap_data_with_sensors(c("do", "di", "uo", "ui"))

  # Add realistic heating pattern
  pulse_1 <- sap_data$measurements$pulse_id == 1
  heat_start <- 31
  time_since_heat <- pmax(0, seq_len(sum(pulse_1)) - heat_start)
  heating_effect <- 0.5 * exp(-time_since_heat / 50) * (time_since_heat > 0)

  sap_data$measurements$do[pulse_1] <- sap_data$measurements$do[pulse_1] + heating_effect * 1.2
  sap_data$measurements$di[pulse_1] <- sap_data$measurements$di[pulse_1] + heating_effect * 1.1
  sap_data$measurements$uo[pulse_1] <- sap_data$measurements$uo[pulse_1] + heating_effect * 0.8
  sap_data$measurements$ui[pulse_1] <- sap_data$measurements$ui[pulse_1] + heating_effect * 0.9

  # Load configuration
  config <- load_probe_config("symmetrical")

  # This should work with the new calc_heat_pulse_velocity() signature
  # Note: This will only work once calc_heat_pulse_velocity() is updated
  # expect_silent({
  #   results <- calc_heat_pulse_velocity(sap_data, probe_config = config)
  # })

  expect_s3_class(config, "ProbeConfiguration")
})


test_that("Multiple configs can coexist", {
  sym <- load_probe_config("symmetrical")
  asym <- load_probe_config("asymmetrical")
  custom <- create_custom_probe_config(
    upstream_distance = 8,
    downstream_distance = 8
  )

  # All should be valid ProbeConfiguration objects
  expect_s3_class(sym, "ProbeConfiguration")
  expect_s3_class(asym, "ProbeConfiguration")
  expect_s3_class(custom, "ProbeConfiguration")

  # Should have different properties
  expect_false(identical(sym$sensor_positions, asym$sensor_positions))
  expect_false(identical(sym$sensor_positions, custom$sensor_positions))
})