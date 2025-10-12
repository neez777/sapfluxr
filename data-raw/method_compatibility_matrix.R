#' Create Method Compatibility Matrix Data
#'
#' Creates the method compatibility matrix as package data

# Method compatibility matrix
# Ratings: 3 = Optimal, 2 = Good, 1 = Possible with limitations, 0 = Not possible

method_compatibility_matrix <- data.frame(
  configuration = c("three_probe_symmetric", "three_probe_asymmetric",
                    "four_probe_extended", "four_probe_advanced"),

  HRM = c(3, 0, 2, 2),           # Heat Ratio Method
  CHPM = c(0, 3, 2, 1),          # Compensation Heat Pulse Method
  Tmax_Coh = c(2, 2, 2, 2),      # T-max Cohen
  Tmax_Klu = c(1, 1, 2, 1),      # T-max Kluitenberg
  DRM = c(0, 0, 3, 1),           # Double Ratio Method
  MHR = c(2, 0, 2, 2),           # Maximum Heat Ratio
  Sapflow_plus = c(0, 0, 0, 3),  # Sapflow+ comprehensive method
  DMA = c(3, 2, 3, 1),           # Dual Method Approach

  HRMXa = c(2, 1, 2, 2),         # Heat Ratio Method variant a
  HRMXb = c(2, 1, 2, 2),         # Heat Ratio Method variant b

  stringsAsFactors = FALSE
)

# Method descriptions and characteristics
method_descriptions <- data.frame(
  method = c("HRM", "CHPM", "Tmax_Coh", "Tmax_Klu", "DRM", "MHR",
             "Sapflow_plus", "DMA", "HRMXa", "HRMXb"),

  full_name = c(
    "Heat Ratio Method",
    "Compensation Heat Pulse Method",
    "T-max Cohen Method",
    "T-max Kluitenberg Method",
    "Double Ratio Method",
    "Maximum Heat Ratio Method",
    "Sapflow+ Comprehensive Method",
    "Dual Method Approach",
    "Heat Ratio Method variant A",
    "Heat Ratio Method variant B"
  ),

  typical_range_min = c(-10, 3, 5, 5, -28.8, -10, -15, -10, -10, -10),
  typical_range_max = c(45, 200, 100, 100, 85, 45, 110, 100, 45, 45),

  requires_thermal_diffusivity = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  requires_timing_precision = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),

  best_for = c(
    "Low flows, reverse flows, nocturnal measurements",
    "Moderate to high flows, absolute transpiration",
    "Moderate to high flows",
    "High flows with finite pulse correction",
    "Full range, real-time thermal diffusivity",
    "Extended HRM range using peak ratios",
    "Multi-parameter analysis including water content",
    "Automatic method switching based on flow conditions",
    "Modified HRM with improved signal processing",
    "Alternative HRM calculation approach"
  ),

  stringsAsFactors = FALSE
)

# Configuration specifications
probe_configuration_specs <- data.frame(
  configuration = c("three_probe_symmetric", "three_probe_asymmetric",
                    "four_probe_extended", "four_probe_advanced"),

  full_name = c(
    "Three-Probe Symmetric (HRM Standard)",
    "Three-Probe Asymmetric (CHPM Standard)",
    "Four-Probe Extended (DRM/Research)",
    "Four-Probe Advanced (Sapflow+ Style)"
  ),

  heater_position = c(0, 0, 0, 0),
  upstream_distance = c(-6, -5, -5, -6),
  downstream_distance_1 = c(6, 10, 5, 6),
  downstream_distance_2 = c(NA, NA, 10, NA),

  total_span_mm = c(12, 15, 15, 12),
  symmetric = c(TRUE, FALSE, FALSE, TRUE),

  primary_method = c("HRM", "CHPM", "DRM", "Sapflow_plus"),

  typical_applications = c(
    "Standard sap flow monitoring, low to moderate flows",
    "High flow measurements, transpiration studies",
    "Research applications, full flow range",
    "Comprehensive thermal analysis, multi-parameter studies"
  ),

  stringsAsFactors = FALSE
)

# Parameter requirements by method and configuration
method_parameters <- list(
  HRM = list(
    thermal_diffusivity = "required",
    measurement_window = c(60, 100),
    pre_pulse_period = 30,
    spacing_symmetric = "preferred"
  ),

  CHPM = list(
    heat_pulse_duration = "required",
    crossover_detection = "critical",
    temporal_resolution = "high",
    asymmetric_spacing = "required"
  ),

  Tmax_Coh = list(
    thermal_diffusivity = "required",
    peak_detection = "critical",
    sufficient_downstream_distance = "required"
  ),

  Tmax_Klu = list(
    thermal_diffusivity = "required",
    heat_pulse_duration = "required",
    finite_pulse_correction = "applied",
    peak_detection = "critical"
  ),

  DRM = list(
    multiple_downstream_sensors = "required",
    real_time_diffusivity = "calculated",
    extended_measurement_window = c(50, 120)
  ),

  MHR = list(
    thermal_diffusivity = "required",
    peak_ratio_calculation = "critical",
    all_sensor_functionality = "required"
  ),

  Sapflow_plus = list(
    comprehensive_sensor_array = "required",
    multi_parameter_analysis = "enabled",
    advanced_processing = "required"
  ),

  DMA = list(
    peclet_number_calculation = "automatic",
    method_switching_threshold = 1.0,
    both_hrm_and_tmax_capable = "required"
  )
)

# Quality control thresholds
quality_control_thresholds <- list(
  temperature = list(
    reasonable_range = c(-10, 60),
    baseline_alignment_max = 2.0,
    minimum_rise_hrm = 0.1,
    minimum_rise_chpm = 0.05,
    minimum_rise_tmax = 0.3
  ),

  timing = list(
    maximum_interval_chpm = 2.0,
    minimum_measurement_duration = 100,
    minimum_pre_pulse_period = 20
  ),

  data_completeness = list(
    maximum_missing_percent = 20,
    minimum_pulses = 2,
    minimum_points_per_pulse = 60
  ),

  sensor_alignment = list(
    maximum_baseline_difference = 1.0,
    maximum_radial_difference = 0.5,
    minimum_signal_to_noise = 2.0
  )
)

# Save as package data
usethis::use_data(method_compatibility_matrix, overwrite = TRUE)
usethis::use_data(method_descriptions, overwrite = TRUE)
usethis::use_data(probe_configuration_specs, overwrite = TRUE)
usethis::use_data(method_parameters, overwrite = TRUE)
usethis::use_data(quality_control_thresholds, overwrite = TRUE)