# R/02a_probe_configuration.R
# Probe Properties Configuration System

# Hardcoded fallback defaults ----

#' Get Hardcoded Probe Configuration Defaults
#'
#' Returns hardcoded probe configuration data as a list matching YAML structure.
#' This serves as a fallback if YAML files cannot be found.
#'
#' @param config_name Name of configuration ("symmetrical" or "asymmetrical")
#' @return List with probe configuration data
#' @keywords internal
get_hardcoded_probe_defaults <- function(config_name = "symmetrical") {

  if (config_name == "symmetrical") {
    # ICT SFM1x Standard Symmetric Configuration
    list(
      metadata = list(
        config_name = "ICT Complete Standard",
        description = "Complete ICT standard configuration",
        version = "1.0",
        default = TRUE
      ),
      probe = list(
        heater_position = 0,
        upstream_distance = 5,        # mm
        downstream_distance = 5,      # mm
        diameter = 1.27,              # mm
        length = 35,                  # mm
        needle_diameter = 1.27,       # mm
        inner_sensor = 7.5,           # mm from tip
        outer_sensor = 22.5,          # mm from tip
        manufacturer = "ICT",
        model = "SFM1",
        heat_pulse_duration = 2       # seconds
      ),
      methods = list(
        compatible = c("HRM", "MHR", "DMA", "Tmax_Coh", "Tmax_Klu", "HRMx"),
        recommended = c("HRM", "Tmax_Coh", "MHR", "DMA"),
        priority_order = c("HRM", "Tmax_Coh", "MHR", "DMA")
      )
    )

  } else if (config_name == "asymmetrical") {
    # CHPM-Optimised Asymmetric Configuration
    list(
      metadata = list(
        config_name = "CHPM Optimised Configuration",
        description = "Asymmetric probe configuration optimised for CHPM method",
        version = "1.0",
        default = FALSE
      ),
      probe = list(
        heater_position = 0,
        upstream_distance = 7.5,      # mm - greater spacing for CHPM
        downstream_distance = 2.5,    # mm - closer for faster response
        diameter = 1.27,
        length = 35,
        needle_diameter = 1.27,
        inner_sensor = 7.5,
        outer_sensor = 22.5,
        manufacturer = "ICT",
        model = "SFM1",
        heat_pulse_duration = 2
      ),
      methods = list(
        compatible = c("CHPM", "HRM", "MHR", "DMA"),
        recommended = c("CHPM", "HRM"),
        priority_order = c("CHPM", "HRM", "MHR", "DMA")
      )
    )

  } else {
    # Default to symmetrical if unknown
    get_hardcoded_probe_defaults("symmetrical")
  }
}

#' ProbeConfiguration R6 Class
#'
#' @description
#' R6 class for representing probe configurations with validation
#' and method compatibility checking.
#'
#' @field config_name Configuration name
#' @field config_type Type of configuration
#' @field heater_position Position of heater element (typically 0)
#' @field sensor_positions Named list of sensor positions relative to heater
#' @field probe_diameter Probe diameter in mm
#' @field heat_pulse_duration Heat pulse duration in seconds
#' @field thermal_diffusivity Thermal diffusivity (cm²/s), NULL if estimated
#' @field compatible_methods List of compatible calculation methods
#' @field method_priorities Priority ranking of methods for this configuration
#' @field required_parameters List of required parameters for each method
#' @field yaml_source Path to source YAML file (if loaded from file)
#' @field yaml_data Raw YAML data (if loaded from file)
#'
#' @export
ProbeConfiguration <- R6::R6Class(
  "ProbeConfiguration",
  public = list(
    config_name = NULL,
    config_type = NULL,
    heater_position = NULL,
    sensor_positions = NULL,
    probe_diameter = NULL,
    heat_pulse_duration = NULL,
    thermal_diffusivity = NULL,
    compatible_methods = NULL,
    method_priorities = NULL,
    required_parameters = NULL,
    yaml_source = NULL,
    yaml_data = NULL,

    #' @description
    #' Initialize a new ProbeConfiguration
    #'
    #' @param config_name Name of the configuration
    #' @param config_type Type of configuration
    #' @param heater_position Position of heater (typically 0)
    #' @param sensor_positions Named list of sensor positions
    #' @param probe_diameter Probe diameter in mm
    #' @param heat_pulse_duration Heat pulse duration in seconds
    #' @param thermal_diffusivity Thermal diffusivity (cm²/s)
    #' @param compatible_methods Compatible calculation methods
    #' @param method_priorities Priority ranking of methods (optional)
    #' @param required_parameters Required parameters for each method (optional)
    #' @param yaml_source Path to source YAML file (optional)
    #' @param yaml_data Raw YAML data (optional)
    initialize = function(config_name, config_type, heater_position = 0,
                          sensor_positions, probe_diameter = 1.27,
                          heat_pulse_duration = 2, thermal_diffusivity = NULL,
                          compatible_methods, method_priorities = NULL,
                          required_parameters = list(), yaml_source = NULL,
                          yaml_data = NULL) {

      self$config_name <- config_name
      self$config_type <- config_type
      self$heater_position <- heater_position
      self$sensor_positions <- sensor_positions
      self$probe_diameter <- probe_diameter
      self$heat_pulse_duration <- heat_pulse_duration
      self$thermal_diffusivity <- thermal_diffusivity
      self$compatible_methods <- compatible_methods
      self$yaml_source <- yaml_source
      self$yaml_data <- yaml_data

      # Set default method priorities if not provided
      if (is.null(method_priorities) && !is.null(compatible_methods)) {
        self$method_priorities <- compatible_methods
      } else {
        self$method_priorities <- method_priorities
      }

      self$required_parameters <- required_parameters

      # Validate configuration on creation
      validation <- self$validate()
      if (!validation$valid) {
        stop("Invalid probe configuration: ", paste(validation$issues, collapse = "; "))
      }
    },

    #' @description
    #' Validate the probe configuration
    #'
    #' @return List with validation results
    validate = function() {
      issues <- character(0)
      warnings <- character(0)

      # Check required fields
      if (is.null(self$config_name) || nchar(self$config_name) == 0) {
        issues <- c(issues, "Configuration name is required")
      }

      if (is.null(self$sensor_positions) || length(self$sensor_positions) == 0) {
        issues <- c(issues, "Sensor positions are required")
      }

      # Check sensor positions
      if (!is.null(self$sensor_positions)) {
        # Basic requirement: at least one upstream and one downstream sensor
        has_upstream <- any(grepl("upstream", names(self$sensor_positions), ignore.case = TRUE))
        has_downstream <- any(grepl("downstream", names(self$sensor_positions), ignore.case = TRUE))

        if (!has_upstream) {
          issues <- c(issues, "Missing upstream sensor position")
        }
        if (!has_downstream) {
          issues <- c(issues, "Missing downstream sensor position")
        }

        # Check for reasonable distances
        positions <- unlist(self$sensor_positions)
        if (any(abs(positions) > 20)) {
          warnings <- c(warnings, "Some sensor positions are unusually far from heater (>20mm)")
        }
      }

      # Check compatible methods
      if (is.null(self$compatible_methods) || length(self$compatible_methods) == 0) {
        issues <- c(issues, "At least one compatible method is required")
      }

      # Check probe diameter
      if (!is.null(self$probe_diameter) && (self$probe_diameter < 0.5 || self$probe_diameter > 5)) {
        warnings <- c(warnings, "Unusual probe diameter - check specifications")
      }

      return(list(
        valid = length(issues) == 0,
        issues = issues,
        warnings = warnings
      ))
    },

    #' @description
    #' Check if a method is compatible with this configuration
    #'
    #' @param method_name Name of the calculation method
    #' @return Logical indicating compatibility
    is_method_compatible = function(method_name) {
      return(method_name %in% self$compatible_methods)
    },

    #' @description
    #' Get recommended methods in priority order
    #'
    #' @param n Maximum number of methods to return
    #' @return Character vector of method names
    get_recommended_methods = function(n = 3) {
      if (!is.null(self$method_priorities)) {
        return(head(self$method_priorities, n))
      } else {
        return(head(self$compatible_methods, n))
      }
    },

    #' @description
    #' Get required parameters for a specific method
    #'
    #' @param method_name Name of the calculation method
    #' @return List of required parameters
    get_method_parameters = function(method_name) {
      if (!is.null(self$required_parameters) && method_name %in% names(self$required_parameters)) {
        return(self$required_parameters[[method_name]])
      } else {
        return(list())
      }
    },

    #' @description
    #' Print configuration summary
    print = function() {
      cat("Probe Configuration:", self$config_name, "\n")
      cat("Type:", self$config_type, "\n")
      cat("Sensors:", paste(names(self$sensor_positions), "=",
                            unlist(self$sensor_positions), "mm", collapse = ", "), "\n")
      if (!is.null(self$heat_pulse_duration)) {
        cat("Heat pulse duration:", self$heat_pulse_duration, "seconds\n")
      }
      cat("Compatible methods:", paste(self$compatible_methods, collapse = ", "), "\n")
      if (!is.null(self$method_priorities)) {
        cat("Recommended priority:", paste(self$method_priorities, collapse = " > "), "\n")
      }
      if (!is.null(self$yaml_source)) {
        cat("Source:", basename(self$yaml_source), "\n")
      }
    }
  )
)

#' Validate Probe Configuration
#'
#' @description
#' Validate that a probe configuration is complete and sensible
#' for the intended calculations.
#'
#' @param config ProbeConfiguration object or list
#' @param methods Character vector of methods to validate for
#' @return List with validation results
#' @export
validate_probe_config <- function(config, methods = NULL) {

  if (!inherits(config, "ProbeConfiguration") && !is.list(config)) {
    stop("Config must be a ProbeConfiguration object or list")
  }

  # Convert list to ProbeConfiguration if needed
  if (is.list(config) && !inherits(config, "ProbeConfiguration")) {
    config_result <- tryCatch({
      do.call(ProbeConfiguration$new, config)
    }, error = function(e) {
      return(list(
        valid = FALSE,
        issues = paste("Cannot create ProbeConfiguration from list:", e$message),
        warnings = character(0)
      ))
    })

    # If conversion failed, return the error result
    if (is.list(config_result) && !inherits(config_result, "ProbeConfiguration")) {
      return(config_result)
    }

    config <- config_result
  }

  # Basic validation (only if we have a valid ProbeConfiguration object)
  if (!inherits(config, "ProbeConfiguration")) {
    return(list(
      valid = FALSE,
      issues = "Could not create valid ProbeConfiguration object",
      warnings = character(0)
    ))
  }

  basic_validation <- config$validate()
  issues <- basic_validation$issues
  warnings <- basic_validation$warnings

  # Method-specific validation
  if (!is.null(methods)) {
    for (method in methods) {
      if (!config$is_method_compatible(method)) {
        issues <- c(issues, paste("Method", method, "is not compatible with configuration",
                                  config$config_name))
      } else {
        # Check method-specific requirements
        method_params <- config$get_method_parameters(method)
        validation_result <- validate_method_requirements(method, method_params, config)
        issues <- c(issues, validation_result$issues)
        warnings <- c(warnings, validation_result$warnings)
      }
    }
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    config_name = config$config_name
  ))
}

#' Validate Method Requirements
#'
#' @param method_name Name of calculation method
#' @param method_params Required parameters for the method
#' @param config ProbeConfiguration object
#' @return List with validation results
#' @keywords internal
validate_method_requirements <- function(method_name, method_params, config) {

  issues <- character(0)
  warnings <- character(0)

  # Method-specific validation
  if (method_name == "HRM") {
    if (is.null(config$thermal_diffusivity)) {
      warnings <- c(warnings, "HRM requires thermal diffusivity - will need to be estimated")
    }

    # Check for reasonable sensor spacing for HRM
    positions <- unlist(config$sensor_positions)
    if (length(positions) >= 2) {
      spacing <- max(positions) - min(positions)
      if (spacing < 5 || spacing > 15) {
        warnings <- c(warnings, paste("HRM sensor spacing (", round(spacing, 1),
                                      "mm) may not be optimal (5-12mm recommended)"))
      }
    }
  }

  if (method_name == "CHMP") {
    # CHMP only works with asymmetric spacing
    if ("upstream" %in% names(config$sensor_positions) &&
        "downstream" %in% names(config$sensor_positions)) {
      up_dist <- abs(config$sensor_positions$upstream)
      down_dist <- abs(config$sensor_positions$downstream)

      if (abs(up_dist - down_dist) < 2) {
        warnings <- c(warnings, "CHMP required asymmetric sensor spacing")
      }
    }
  }

  if (method_name %in% c("Tmax_Coh", "Tmax_Klu")) {
    # T-max methods need sufficient downstream distance
    if ("downstream" %in% names(config$sensor_positions)) {
      down_dist <- abs(config$sensor_positions$downstream)
      if (down_dist < 5) {
        warnings <- c(warnings, paste("T-max methods may have limited range with downstream sensor at",
                                      down_dist, "mm (>5mm recommended)"))
      }
    }
  }

  if (method_name == "DRM") {
    # DRM needs multiple downstream sensors
    downstream_sensors <- config$sensor_positions[grepl("downstream", names(config$sensor_positions))]
    if (length(downstream_sensors) < 2) {
      issues <- c(issues, "DRM requires at least 2 downstream sensors")
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Data for Configuration
#'
#' @description
#' Validate that the measurement data is suitable for the probe configuration
#' and specified calculation methods.
#'
#' @param sap_data A sap_data object
#' @param config ProbeConfiguration object
#' @param methods Character vector of methods to validate for
#' @return List with validation results
#' @export
validate_data_for_config <- function(sap_data, config, methods = NULL) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  if (!inherits(config, "ProbeConfiguration")) {
    stop("config must be a ProbeConfiguration object")
  }

  issues <- character(0)
  warnings <- character(0)
  measurements <- sap_data$measurements

  # Check for required sensors based on configuration
  required_sensors <- get_required_sensors_for_config(config)
  available_sensors <- intersect(required_sensors, names(measurements))
  missing_sensors <- setdiff(required_sensors, names(measurements))

  if (length(missing_sensors) > 0) {
    issues <- c(issues, paste("Missing required sensors:", paste(missing_sensors, collapse = ", ")))
  }

  # Validate available sensor data
  for (sensor in available_sensors) {
    sensor_data <- measurements[[sensor]]

    # Check for excessive missing values
    missing_pct <- sum(is.na(sensor_data)) / length(sensor_data) * 100
    if (missing_pct > 50) {
      issues <- c(issues, paste("Sensor", sensor, "has", round(missing_pct, 1), "% missing values"))
    } else if (missing_pct > 20) {
      warnings <- c(warnings, paste("Sensor", sensor, "has", round(missing_pct, 1), "% missing values"))
    }

    # Check for sensor failure (constant values)
    non_na_data <- sensor_data[!is.na(sensor_data)]
    if (length(non_na_data) > 10) {
      if (sd(non_na_data) < 0.001) {
        issues <- c(issues, paste("Sensor", sensor, "shows constant values - possible sensor failure"))
      }
    }
  }

  # Check data structure validity
  structure_validation <- validate_data_structure(sap_data)
  issues <- c(issues, structure_validation$issues)
  warnings <- c(warnings, structure_validation$warnings)

  # Check sensor alignment
  if (length(available_sensors) >= 2) {
    alignment_validation <- validate_sensor_alignment(sap_data, config)
    issues <- c(issues, alignment_validation$issues)
    warnings <- c(warnings, alignment_validation$warnings)
  }

  # Method-specific data validation
  if (!is.null(methods)) {
    for (method in methods) {
      if (config$is_method_compatible(method)) {
        method_validation <- validate_method_compatibility(sap_data, config, method)
        issues <- c(issues, method_validation$issues)
        warnings <- c(warnings, method_validation$warnings)
      }
    }
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    config_name = config$config_name,
    methods_validated = methods
  ))
}

# Include helper functions from probe_validation.R that are needed
# [The rest of the helper functions would go here - validate_data_structure,
#  validate_sensor_alignment, validate_method_compatibility, etc.]

#' Validate Data Structure
#' @param sap_data A sap_data object
#' @return List with validation results
#' @keywords internal
validate_data_structure <- function(sap_data) {

  issues <- character(0)
  warnings <- character(0)

  measurements <- sap_data$measurements

  # Check basic data requirements
  if (nrow(measurements) < 100) {
    issues <- c(issues, "Insufficient data - need at least 100 measurements")
  }

  # Check for pulse data structure
  if ("pulse_id" %in% names(measurements) || "id" %in% names(measurements)) {
    pulse_col <- if ("pulse_id" %in% names(measurements)) "pulse_id" else "id"
    n_pulses <- length(unique(measurements[[pulse_col]]))

    if (n_pulses < 2) {
      issues <- c(issues, "Need at least 2 measurement pulses for analysis")
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Sensor Alignment
#' @param sap_data A sap_data object
#' @param config ProbeConfiguration object
#' @return List with validation results
#' @keywords internal
validate_sensor_alignment <- function(sap_data, config) {

  issues <- character(0)
  warnings <- character(0)

  measurements <- sap_data$measurements

  # Check baseline temperature differences between sensors
  # Get first few measurements as baseline
  baseline_window <- min(50, nrow(measurements))
  if (baseline_window > 10) {
    # Check temperature sensors if available
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(measurements))

    if (length(temp_cols) >= 2) {
      baseline_temps <- list()
      for (col in temp_cols) {
        baseline_temps[[col]] <- mean(measurements[[col]][1:baseline_window], na.rm = TRUE)
      }

      # Check for large baseline differences (> 2°C)
      temp_diffs <- abs(diff(unlist(baseline_temps)))
      if (any(temp_diffs > 2.0, na.rm = TRUE)) {
        warnings <- c(warnings, sprintf(
          "Large baseline temperature differences detected (max: %.2f°C) - may indicate sensor misalignment",
          max(temp_diffs, na.rm = TRUE)
        ))
      }
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Validate Method Compatibility
#' @param sap_data A sap_data object
#' @param config ProbeConfiguration object
#' @param method_name Name of the method
#' @return List with validation results
#' @keywords internal
validate_method_compatibility <- function(sap_data, config, method_name) {

  issues <- character(0)
  warnings <- character(0)

  measurements <- sap_data$measurements

  # HRM validation
  if (method_name == "HRM") {
    # Check for sufficient temperature rise
    temp_cols <- intersect(c("do", "di", "uo", "ui"), names(measurements))
    if (length(temp_cols) > 0) {
      max_rise <- 0
      for (col in temp_cols) {
        temp_data <- measurements[[col]]
        if (length(temp_data) > 0) {
          baseline <- mean(temp_data[1:min(10, length(temp_data))], na.rm = TRUE)
          max_temp <- max(temp_data, na.rm = TRUE)
          max_rise <- max(max_rise, max_temp - baseline, na.rm = TRUE)
        }
      }
      if (max_rise < 0.1) {
        issues <- c(issues, "Insufficient temperature rise for HRM method - check heater function")
      }
    }
  }

  # CHPM validation
  if (method_name == "CHPM") {
    # Check temporal resolution
    if ("time" %in% names(measurements)) {
      time_diffs <- diff(measurements$time)
      if (length(time_diffs) > 0) {
        median_interval <- median(time_diffs, na.rm = TRUE)
        if (median_interval > 10) {
          warnings <- c(warnings, sprintf(
            "Temporal resolution (%.1f s) may be too coarse for CHPM crossover detection",
            median_interval
          ))
        }
      }
    }
  }

  # DRM validation
  if (method_name == "DRM") {
    # Check for multiple downstream sensors in configuration
    sensor_positions <- config$sensor_positions
    config_downstream_sensors <- sum(grepl("downstream", names(sensor_positions)))

    if (config_downstream_sensors < 2) {
      issues <- c(issues, "DRM requires at least 2 downstream sensors for proper implementation")
    }

    # Check for downstream sensors in actual data
    available_sensors <- names(measurements)
    # Map standard sensor names (di, do) to downstream
    data_downstream_count <- sum(c("di", "do") %in% available_sensors)
    # Also check for explicit downstream named sensors
    data_downstream_count <- data_downstream_count + sum(grepl("downstream", available_sensors))

    if (data_downstream_count < 2) {
      issues <- c(issues, "DRM requires at least 2 downstream sensors in the data")
    }
  }

  return(list(issues = issues, warnings = warnings))
}

#' Get Required Sensors for Configuration
#' @param config ProbeConfiguration object
#' @return Character vector of required sensor names
#' @keywords internal
get_required_sensors_for_config <- function(config) {

  # Basic requirements for each configuration type
  if (config$config_name == "three_probe_symmetric") {
    return(c("do", "di", "uo", "ui"))
  }

  if (config$config_name == "three_probe_asymmetric") {
    return(c("do", "uo"))  # Minimum for CHMP, inner sensors recommended
  }

  if (config$config_name == "four_probe_extended") {
    return(c("do", "di", "uo", "ui"))  # Additional sensors like dt may be present
  }

  if (config$config_name == "four_probe_advanced") {
    return(c("do", "di", "uo", "ui"))  # Additional sensors for comprehensive analysis
  }

  # Default fallback
  return(c("do", "uo"))
}

#' Get Method Compatibility Matrix
#'
#' @description
#' Returns the method compatibility matrix showing which calculation
#' methods work with which probe configurations.
#'
#' @return Data frame with compatibility ratings (0=incompatible, 1=limited, 2=good, 3=optimal)
#' @export
get_method_compatibility_matrix <- function() {

  configs <- c("three_probe_symmetric", "three_probe_asymmetric",
               "four_probe_extended", "four_probe_advanced")
  methods <- c("HRM", "MHR", "CHPM", "Tmax_Coh", "Tmax_Klu", "DMA", "DRM", "Sapflow+")

  # Create compatibility matrix (simplified version)
  compatibility <- data.frame(
    configuration = configs,
    HRM = c(3, 0, 3, 2),      # HRM works best with symmetric configs
    MHR = c(3, 0, 3, 2),      # Similar to HRM
    CHPM = c(0, 3, 3, 2),     # CHPM works best with asymmetric
    Tmax_Coh = c(2, 3, 3, 2), # T-max methods work reasonably with most
    Tmax_Klu = c(2, 3, 3, 2),
    DMA = c(3, 2, 3, 2),      # DMA adapts to different configs
    DRM = c(0, 0, 3, 1),      # DRM needs multiple downstream sensors
    "Sapflow+" = c(0, 0, 1, 3) # Sapflow+ needs advanced config
  )

  return(compatibility)
}

#' Print Method Compatibility Matrix
#'
#' @description
#' Pretty print the method compatibility matrix with symbols
#'
#' @return Invisible NULL (prints to console)
#' @export
print_compatibility_matrix <- function() {

  matrix_df <- get_method_compatibility_matrix()
  configs <- matrix_df$configuration
  methods <- names(matrix_df)[-1]  # exclude 'configuration' column

  cat("Method Compatibility Matrix\n")
  cat("Legend: ✓✓✓ = Optimal, ✓✓ = Good, ✓ = Possible with limitations, ✗ = Not possible\n\n")

  # Print header
  cat(sprintf("%-25s", "Configuration"))
  for (method in methods) {
    cat(sprintf("%8s", method))
  }
  cat("\n")
  cat(paste(rep("-", 25 + length(methods) * 8), collapse = ""), "\n")

  # Print rows
  for (i in seq_along(configs)) {
    cat(sprintf("%-25s", configs[i]))
    for (method in methods) {
      rating <- matrix_df[i, method]
      symbol <- switch(as.character(rating),
                       "3" = "✓✓✓",
                       "2" = "✓✓ ",
                       "1" = "✓  ",
                       "0" = "✗  ")
      cat(sprintf("%8s", symbol))
    }
    cat("\n")
  }

  invisible(NULL)
}

#' Validate Probe Alignment Advanced
#'
#' @description
#' Advanced probe alignment validation using multiple statistical and
#' physical criteria specific to heat pulse probe configurations.
#'
#' @param sap_data A sap_data object
#' @param config ProbeConfiguration object (optional)
#' @param alignment_tolerance Numeric, maximum acceptable temperature difference for alignment (default: 1.0)
#' @param symmetry_check Logical, whether to check upstream/downstream symmetry (default: TRUE)
#' @param depth_check Logical, whether to check inner/outer sensor depth consistency (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{alignment_status}{Overall probe alignment status}
#'     \item{baseline_analysis}{Baseline temperature analysis}
#'     \item{symmetry_analysis}{Upstream/downstream symmetry analysis}
#'     \item{depth_analysis}{Inner/outer sensor depth analysis}
#'     \item{recommendations}{Specific alignment recommendations}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic alignment check
#' alignment <- validate_probe_alignment_advanced(sap_data)
#' print(alignment$alignment_status)
#'
#' # With probe configuration
#' config <- detect_probe_config(sap_data)
#' alignment <- validate_probe_alignment_advanced(sap_data, config)
#' }
#'
#' @family probe_configuration
#' @export
validate_probe_alignment_advanced <- function(sap_data, config = NULL,
                                              alignment_tolerance = 1.0,
                                              symmetry_check = TRUE,
                                              depth_check = TRUE) {

  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  measurements <- sap_data$measurements
  required_sensors <- c("do", "di", "uo", "ui")
  available_sensors <- intersect(required_sensors, names(measurements))

  if (length(available_sensors) < 2) {
    stop("Need at least 2 temperature sensors for alignment analysis")
  }

  # Initialize results
  alignment_status <- "UNKNOWN"
  baseline_analysis <- list()
  symmetry_analysis <- list()
  depth_analysis <- list()
  recommendations <- character(0)

  # Get baseline temperatures from multiple pulses
  pulse_col <- if ("pulse_id" %in% names(measurements)) "pulse_id" else "id"
  pulses <- unique(measurements[[pulse_col]])

  if (length(pulses) < 2) {
    warning("Need at least 2 pulses for comprehensive alignment analysis")
    return(list(
      alignment_status = "INSUFFICIENT_DATA",
      baseline_analysis = list(),
      symmetry_analysis = list(),
      depth_analysis = list(),
      recommendations = "Need more measurement pulses for analysis"
    ))
  }

  # Calculate baseline temperatures for each sensor and pulse
  baseline_matrix <- matrix(NA, length(pulses), length(available_sensors))
  colnames(baseline_matrix) <- available_sensors
  rownames(baseline_matrix) <- as.character(pulses)

  for (i in seq_along(pulses)) {
    pulse_data <- measurements[measurements[[pulse_col]] == pulses[i], ]

    if (nrow(pulse_data) >= 30) {
      for (sensor in available_sensors) {
        baseline_matrix[i, sensor] <- mean(pulse_data[[sensor]][1:30], na.rm = TRUE)
      }
    }
  }

  # Remove pulses with insufficient data
  valid_pulses <- apply(baseline_matrix, 1, function(x) sum(!is.na(x)) >= 2)
  baseline_matrix <- baseline_matrix[valid_pulses, , drop = FALSE]

  if (nrow(baseline_matrix) < 2) {
    return(list(
      alignment_status = "INSUFFICIENT_DATA",
      baseline_analysis = list(),
      symmetry_analysis = list(),
      depth_analysis = list(),
      recommendations = "Insufficient valid baseline data"
    ))
  }

  # 1. Baseline Analysis
  baseline_means <- colMeans(baseline_matrix, na.rm = TRUE)
  baseline_stds <- apply(baseline_matrix, 2, sd, na.rm = TRUE)
  baseline_range <- max(baseline_means, na.rm = TRUE) - min(baseline_means, na.rm = TRUE)

  baseline_analysis$mean_temperatures <- baseline_means
  baseline_analysis$temperature_range <- baseline_range
  baseline_analysis$temporal_stability <- baseline_stds
  baseline_analysis$alignment_quality <- ifelse(baseline_range < alignment_tolerance,
                                                "GOOD", ifelse(baseline_range < 2 * alignment_tolerance, "FAIR", "POOR"))

  # 2. Symmetry Analysis (if all four sensors available)
  if (symmetry_check && all(c("do", "di", "uo", "ui") %in% available_sensors)) {

    # Upstream vs Downstream comparison
    downstream_temps <- rowMeans(baseline_matrix[, c("do", "di"), drop = FALSE], na.rm = TRUE)
    upstream_temps <- rowMeans(baseline_matrix[, c("uo", "ui"), drop = FALSE], na.rm = TRUE)
    ud_differences <- downstream_temps - upstream_temps

    symmetry_analysis$upstream_downstream <- list(
      mean_difference = mean(ud_differences, na.rm = TRUE),
      difference_std = sd(ud_differences, na.rm = TRUE),
      symmetry_quality = ifelse(abs(mean(ud_differences, na.rm = TRUE)) < 0.5, "GOOD",
                                ifelse(abs(mean(ud_differences, na.rm = TRUE)) < 1.0, "FAIR", "POOR"))
    )

    if (abs(mean(ud_differences, na.rm = TRUE)) > 1.0) {
      recommendations <- c(recommendations,
                           "Large upstream-downstream temperature difference - check probe insertion depth")
    }
  }

  # 3. Depth Analysis (inner vs outer sensors)
  if (depth_check && all(c("do", "uo") %in% available_sensors) &&
      all(c("di", "ui") %in% available_sensors)) {

    # Outer vs Inner comparison
    outer_temps <- rowMeans(baseline_matrix[, c("do", "uo"), drop = FALSE], na.rm = TRUE)
    inner_temps <- rowMeans(baseline_matrix[, c("di", "ui"), drop = FALSE], na.rm = TRUE)
    radial_differences <- outer_temps - inner_temps

    depth_analysis$inner_outer <- list(
      mean_difference = mean(radial_differences, na.rm = TRUE),
      difference_std = sd(radial_differences, na.rm = TRUE),
      depth_quality = ifelse(abs(mean(radial_differences, na.rm = TRUE)) < 0.3, "GOOD",
                             ifelse(abs(mean(radial_differences, na.rm = TRUE)) < 0.7, "FAIR", "POOR"))
    )

    if (abs(mean(radial_differences, na.rm = TRUE)) > 0.7) {
      recommendations <- c(recommendations,
                           "Large inner-outer temperature difference - check probe installation depth")
    }
  }

  # 4. Overall Alignment Status
  quality_scores <- c()

  if (baseline_analysis$alignment_quality == "GOOD") quality_scores <- c(quality_scores, 3)
  else if (baseline_analysis$alignment_quality == "FAIR") quality_scores <- c(quality_scores, 2)
  else quality_scores <- c(quality_scores, 1)

  if (length(symmetry_analysis) > 0) {
    if (symmetry_analysis$upstream_downstream$symmetry_quality == "GOOD") quality_scores <- c(quality_scores, 3)
    else if (symmetry_analysis$upstream_downstream$symmetry_quality == "FAIR") quality_scores <- c(quality_scores, 2)
    else quality_scores <- c(quality_scores, 1)
  }

  if (length(depth_analysis) > 0) {
    if (depth_analysis$inner_outer$depth_quality == "GOOD") quality_scores <- c(quality_scores, 3)
    else if (depth_analysis$inner_outer$depth_quality == "FAIR") quality_scores <- c(quality_scores, 2)
    else quality_scores <- c(quality_scores, 1)
  }

  overall_score <- mean(quality_scores)

  if (overall_score >= 2.5) {
    alignment_status <- "GOOD"
  } else if (overall_score >= 2.0) {
    alignment_status <- "FAIR"
    recommendations <- c(recommendations, "Consider minor probe alignment adjustments")
  } else {
    alignment_status <- "POOR"
    recommendations <- c(recommendations, "Probe alignment needs significant improvement")
  }

  return(list(
    alignment_status = alignment_status,
    baseline_analysis = baseline_analysis,
    symmetry_analysis = symmetry_analysis,
    depth_analysis = depth_analysis,
    recommendations = recommendations
  ))
}


# ============================================================================
# YAML-Based Configuration Loading System
# ============================================================================
#' Load Probe Configuration from YAML
#'
#' Loads probe configuration from a YAML configuration file and returns a
#' ProbeConfiguration object. Can load built-in configurations by name or
#' custom YAML files by path.
#'
#' @param config_name Character string specifying configuration name
#'   ("symmetrical", "asymmetrical") or path to custom YAML file.
#'   If NULL, uses default ("symmetrical").
#' @param custom_params Named list of parameters to override from YAML.
#'   For example: \code{list(heat_pulse_duration = 3, upstream_distance = 6)}
#'
#' @return ProbeConfiguration R6 object with fields:
#'   \item{config_name}{Name of the configuration}
#'   \item{heat_pulse_duration}{Heat pulse duration in seconds}
#'   \item{sensor_positions}{Named list of sensor positions}
#'   \item{compatible_methods}{Vector of compatible HPV methods}
#'
#' @details
#' Built-in configurations include:
#' \describe{
#'   \item{symmetrical}{ICT SFM1x standard (5mm upstream, 5mm downstream)}
#'   \item{asymmetrical}{CHPM optimized (5mm upstream, 10mm downstream)}
#' }
#'
#' Custom parameters can override any probe specification from the YAML file.
#' The function automatically converts distances from mm to cm for calculations.
#'
#' @examples
#' \dontrun{
#' # Load default symmetric configuration
#' config <- load_probe_config()
#'
#' # Load asymmetric configuration
#' config <- load_probe_config("asymmetrical")
#'
#' # Load with parameter overrides
#' config <- load_probe_config("symmetrical",
#'                            custom_params = list(heat_pulse_duration = 3))
#'
#' # Load custom YAML file
#' config <- load_probe_config("path/to/my_probe.yaml")
#'
#' # Use in calculations
#' results <- calc_heat_pulse_velocity(sap_data, probe_config = config)
#' }
#'
#' @seealso \code{\link{get_default_probe_config}}, \code{\link{list_available_probe_configs}},
#'   \code{\link{create_custom_probe_config}}
#'
#' @family probe configuration functions
#' @export
load_probe_config <- function(config_name = NULL, custom_params = NULL) {
  if (is.null(config_name)) config_name <- "symmetrical"

  # Try to load from YAML file
  config_data <- NULL
  yaml_source <- NULL

  if (file.exists(config_name)) {
    # User provided a file path
    yaml_file <- config_name
    yaml_source <- yaml_file
    config_data <- tryCatch(yaml::read_yaml(yaml_file),
                           error = function(e) {
                             warning("Failed to parse YAML file: ", e$message,
                                    "\nFalling back to hardcoded defaults.")
                             NULL
                           })

  } else {
    # Try to find built-in YAML
    yaml_file <- system.file("configurations", paste0("probe_", config_name, ".yaml"), package = "sapfluxr")

    if (file.exists(yaml_file) && yaml_file != "") {
      yaml_source <- yaml_file
      config_data <- tryCatch(yaml::read_yaml(yaml_file),
                             error = function(e) {
                               warning("Failed to parse YAML file: ", e$message,
                                      "\nFalling back to hardcoded defaults.")
                               NULL
                             })
    }
  }

  # Fall back to hardcoded defaults if YAML not found or failed to parse
  if (is.null(config_data)) {
    message("Using hardcoded probe configuration defaults for '", config_name, "'")
    config_data <- get_hardcoded_probe_defaults(config_name)
    yaml_source <- "hardcoded_defaults"
  }

  probe <- config_data$probe
  x_upstream <- probe$upstream_distance / 10
  x_downstream <- probe$downstream_distance / 10

  sensor_positions <- list(upstream_inner = -x_upstream, downstream_inner = x_downstream,
                          upstream_outer = -x_upstream, downstream_outer = x_downstream)

  heat_pulse_duration <- if (!is.null(probe$heat_pulse_duration)) probe$heat_pulse_duration else 2

  if (!is.null(custom_params)) {
    if (!is.null(custom_params$heat_pulse_duration)) heat_pulse_duration <- custom_params$heat_pulse_duration
    if (!is.null(custom_params$upstream_distance)) {
      x_upstream <- custom_params$upstream_distance / 10
      sensor_positions$upstream_inner <- -x_upstream
      sensor_positions$upstream_outer <- -x_upstream
    }
    if (!is.null(custom_params$downstream_distance)) {
      x_downstream <- custom_params$downstream_distance / 10
      sensor_positions$downstream_inner <- x_downstream
      sensor_positions$downstream_outer <- x_downstream
    }
  }

  methods <- config_data$methods
  compatible_methods <- if (!is.null(methods$compatible)) methods$compatible else c("HRM", "MHR", "DMA")
  priority_order <- if (!is.null(methods$priority_order)) methods$priority_order else
                    if (!is.null(methods$recommended)) methods$recommended else compatible_methods

  ProbeConfiguration$new(
    config_name = config_data$metadata$config_name,
    config_type = ifelse(x_upstream == x_downstream, "symmetric", "asymmetric"),
    heater_position = probe$heater_position,
    sensor_positions = sensor_positions,
    probe_diameter = probe$diameter,
    heat_pulse_duration = heat_pulse_duration,
    thermal_diffusivity = NULL,
    compatible_methods = compatible_methods,
    method_priorities = priority_order,
    required_parameters = list(x = mean(c(x_upstream, x_downstream)), heat_pulse_duration = heat_pulse_duration),
    yaml_source = yaml_source,
    yaml_data = config_data
  )
}

#' Get Default Probe Configuration
#'
#' Returns the default probe configuration (symmetric SFM1x).
#' This is a convenience wrapper around \code{load_probe_config("symmetrical")}.
#'
#' @param custom_params List of parameters to override (optional).
#'   For example: \code{list(heat_pulse_duration = 3)}
#'
#' @return ProbeConfiguration R6 object
#'
#' @examples
#' \dontrun{
#' # Get default configuration
#' config <- get_default_probe_config()
#' print(config)
#'
#' # Get default with overrides
#' config <- get_default_probe_config(
#'   custom_params = list(heat_pulse_duration = 3)
#' )
#' }
#'
#' @seealso \code{\link{load_probe_config}}
#'
#' @family probe configuration functions
#' @export
get_default_probe_config <- function(custom_params = NULL) {
  load_probe_config("symmetrical", custom_params = custom_params)
}

#' List Available Probe Configurations
#'
#' Lists all probe configurations available in the package,
#' including configuration names, descriptions, and file paths.
#'
#' @return Data frame with columns:
#'   \item{name}{Configuration name (use with \code{load_probe_config()})}
#'   \item{description}{Configuration description}
#'   \item{file}{Full path to YAML file}
#'   \item{default}{Logical indicating if this is the default configuration}
#'
#' @examples
#' \dontrun{
#' # List all available configurations
#' configs <- list_available_probe_configs()
#' print(configs)
#'
#' # Load a specific configuration by name
#' config <- load_probe_config(configs$name[2])
#' }
#'
#' @seealso \code{\link{load_probe_config}}
#'
#' @family probe configuration functions
#' @export
list_available_probe_configs <- function() {
  config_dir <- system.file("configurations", package = "sapfluxr")
  if (config_dir == "" || !dir.exists(config_dir)) {
    return(data.frame(name = character(0), description = character(0),
                     file = character(0), default = logical(0), stringsAsFactors = FALSE))
  }

  yaml_files <- list.files(config_dir, pattern = "^probe_.*\\.yaml$", full.names = TRUE)
  if (length(yaml_files) == 0) {
    return(data.frame(name = character(0), description = character(0),
                     file = character(0), default = logical(0), stringsAsFactors = FALSE))
  }

  configs <- lapply(yaml_files, function(f) {
    config_data <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(config_data)) return(NULL)
    data.frame(name = gsub("^probe_(.*)\\.yaml$", "\\1", basename(f)),
              description = ifelse(is.null(config_data$metadata$description), "", config_data$metadata$description),
              file = f, default = isTRUE(config_data$metadata$default), stringsAsFactors = FALSE)
  })

  configs <- configs[!sapply(configs, is.null)]
  if (length(configs) == 0) {
    return(data.frame(name = character(0), description = character(0),
                     file = character(0), default = logical(0), stringsAsFactors = FALSE))
  }

  result <- do.call(rbind, configs)
  result[order(-result$default, result$name), ]
}

#' Create Custom Probe Configuration
#'
#' Helper function to create a custom probe configuration in R
#' without needing a YAML file. Useful for quick testing or one-off configurations.
#'
#' @param config_name Name for the configuration. Default: "Custom Configuration"
#' @param upstream_distance Distance upstream of heater (mm). Default: 5
#' @param downstream_distance Distance downstream of heater (mm). Default: 5
#' @param probe_diameter Probe diameter (mm). Default: 1.27
#' @param heat_pulse_duration Heat pulse duration (seconds). Default: 2
#' @param compatible_methods Vector of compatible method names.
#'   Default: \code{c("HRM", "MHR", "DMA")}
#'
#' @return ProbeConfiguration R6 object
#'
#' @details
#' This function creates a ProbeConfiguration object programmatically without
#' requiring a YAML file. The configuration type (symmetric/asymmetric) is
#' automatically determined based on the upstream and downstream distances.
#'
#' Distances are specified in millimeters and automatically converted to
#' centimeters for internal calculations.
#'
#' @examples
#' \dontrun{
#' # Create custom symmetric configuration
#' custom_config <- create_custom_probe_config(
#'   config_name = "My Custom Probe",
#'   upstream_distance = 6,
#'   downstream_distance = 6,
#'   heat_pulse_duration = 3
#' )
#'
#' # Create asymmetric configuration
#' asymmetric <- create_custom_probe_config(
#'   config_name = "CHPM Optimized",
#'   upstream_distance = 5,
#'   downstream_distance = 10,
#'   compatible_methods = c("CHPM", "HRM", "DMA")
#' )
#'
#' # Use in calculations
#' results <- calc_heat_pulse_velocity(sap_data, probe_config = custom_config)
#' }
#'
#' @seealso \code{\link{load_probe_config}}, \code{\link{ProbeConfiguration}}
#'
#' @family probe configuration functions
#' @export
create_custom_probe_config <- function(config_name = "Custom Configuration",
                                      upstream_distance = 5, downstream_distance = 5,
                                      probe_diameter = 1.27, heat_pulse_duration = 2,
                                      compatible_methods = c("HRM", "MHR", "DMA")) {
  x_upstream <- upstream_distance / 10
  x_downstream <- downstream_distance / 10

  sensor_positions <- list(upstream_inner = -x_upstream, downstream_inner = x_downstream,
                          upstream_outer = -x_upstream, downstream_outer = x_downstream)

  config_type <- ifelse(upstream_distance == downstream_distance, "symmetric", "asymmetric")

  ProbeConfiguration$new(
    config_name = config_name, config_type = config_type, heater_position = 0,
    sensor_positions = sensor_positions, probe_diameter = probe_diameter,
    heat_pulse_duration = heat_pulse_duration, thermal_diffusivity = NULL,
    compatible_methods = compatible_methods, method_priorities = compatible_methods,
    required_parameters = list(x = mean(c(x_upstream, x_downstream)), heat_pulse_duration = heat_pulse_duration)
  )
}
