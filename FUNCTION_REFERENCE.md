# sapFluxR Function Reference

**Version:** 0.1.0
**Last Updated:** 2025-10-10

This document provides a comprehensive reference to all functions in the sapFluxR package, organized by workflow stage.

## Table of Contents

1. [Data Import (Stage 2)](#1-data-import-stage-2)
2. [Data Validation (Stage 2)](#2-data-validation-stage-2)
3. [Probe Configuration (Stage 3)](#3-probe-configuration-stage-3)
4. [Core HPV Methods (Stage 4)](#4-core-hpv-methods-stage-4)
5. [Advanced HPV Methods (Stage 4)](#5-advanced-hpv-methods-stage-4)
6. [YAML Methods (Stage 4)](#6-yaml-methods-stage-4)
7. [Method Comparison (Stage 5 & 10)](#7-method-comparison-stage-5--10)
8. [Quality Control (Stage 5 & 10)](#8-quality-control-stage-5--10)
9. [Diagnostic Reporting (Stage 5 & 10)](#9-diagnostic-reporting-stage-5--10)
10. [Temporal Processing (Stage 6)](#10-temporal-processing-stage-6)
11. [Flux Density (Stage 7)](#11-flux-density-stage-7)
12. [Sapwood Area (Stage 8)](#12-sapwood-area-stage-8)
13. [Tree Water Use (Stage 9)](#13-tree-water-use-stage-9)
14. [Processing Pipeline (Unified)](#14-processing-pipeline-unified)
15. [Export & Reporting](#15-export--reporting)
16. [Multi-Tree Utilities](#16-multi-tree-utilities)
17. [Visualisation](#17-visualisation)
18. [Utilities](#18-utilities)

---

## 1. Data Import (Stage 2)

**File:** `R/01_data_import.R`

### read_sap_data()
**Exported:** Yes

Imports and parses sap flow data from ICT SFM1x sensors.

**Inputs:**
- `file_path` (character): Path to the data file
- `format` (character, optional): Data format - "ict_current", "ict_legacy", or "csv". Auto-detected if NULL.
- `validate_data` (logical, default: TRUE): Whether to validate imported data
- `chunk_size` (integer, optional): Characters per chunk for large files
- `show_progress` (logical, optional): Show progress updates (auto TRUE for files >1MB)
- `...`: Additional arguments passed to specific import functions

**Outputs:**
A `sap_data` object (list) containing:
- `diagnostics`: Data frame with sensor diagnostics (battery voltage, current, temperature)
- `measurements`: Data frame with temperature measurements (do, di, uo, ui) and timestamps
- `metadata`: List with file information, format, import time, file size, pulse count
- `validation`: Validation results (if validate_data = TRUE)

**Description:**
Main data import function with automatic format detection. Handles three ICT sensor formats: current JSON-like format, legacy format, and CSV. Uses character-based chunking for large files.

---

### detect_format()
**Exported:** No (internal)

Automatically detects the data format by reading the first 3000 characters.

**Inputs:**
- `file_path` (character): Path to data file

**Outputs:**
- (character): Format string - "ict_current", "ict_legacy", or "csv"

---

### read_ict_current()
**Exported:** No (internal)

Parses ICT current format (JSON-like) using character-based chunking.

**Inputs:**
- `file_path` (character): Path to data file
- `chunk_size` (integer): Characters per chunk
- `show_progress` (logical): Show progress updates
- `...`: Additional arguments

**Outputs:**
- (list): Parsed data with diagnostics and measurements

---

### read_ict_legacy()
**Exported:** No (internal)

Parses ICT legacy format (nested JSON with "Rdg" and "T" arrays).

**Inputs:**
- `file_path` (character): Path to data file
- `chunk_size` (integer): Characters per chunk
- `show_progress` (logical): Show progress updates
- `...`: Additional arguments

**Outputs:**
- (list): Parsed data with diagnostics and measurements

---

### read_csv_format()
**Exported:** No (internal)

Parses CSV or tab-delimited format.

**Inputs:**
- `file_path` (character): Path to data file
- `show_progress` (logical): Show progress updates
- `...`: Additional arguments

**Outputs:**
- (list): Parsed data with diagnostics and measurements

---

### read_multiple_sap_data()
**Exported:** Yes

Reads multiple sap flow data files and combines them.

**Inputs:**
- `file_paths` (character vector): Paths to multiple data files
- `tree_ids` (character vector, optional): Tree identifiers for each file
- `format` (character, optional): Data format (auto-detected if NULL)
- `validate_data` (logical, default: TRUE): Whether to validate data
- `...`: Additional arguments

**Outputs:**
A `multi_tree_data` object (list of sap_data objects) with class attribute.

**Description:**
Batch import function for processing multiple trees or time periods. Automatically generates tree IDs if not provided.

---

### combine_multiple_sap_data()
**Exported:** No (internal)

Combines multiple sap_data objects into a single dataset.

**Inputs:**
- `sap_data_list` (list): List of sap_data objects
- `tree_ids` (character vector, optional): Tree identifiers

**Outputs:**
- (sap_data): Combined sap_data object with tree_id column

---

## 2. Data Validation (Stage 2)

**File:** `R/02_data_validation.R`

### validate_sap_data()
**Exported:** Yes

Performs comprehensive validation of imported sap flow data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `temperature_range` (numeric vector, default: c(-10, 60)): Min/max reasonable temperature values (°C)
- `voltage_range` (numeric vector, default: c(0, 30)): Min/max reasonable voltage values
- `strict_validation` (logical, default: FALSE): Apply strict validation rules

**Outputs:**
A list containing:
- `valid` (logical): Overall validation result
- `issues` (character vector): Critical validation issues found
- `warnings` (character vector): Non-critical warnings
- `summary` (list): Validation statistics

**Description:**
Checks data integrity, value ranges, temporal consistency, and sensor consistency. Returns detailed validation report.

---

### validate_structure()
**Exported:** No (internal)

Validates the structure of sap_data object components.

**Inputs:**
- `sap_data` (sap_data object)

**Outputs:**
- (list): Issues and warnings about structure

---

### validate_ranges()
**Exported:** No (internal)

Validates that temperature and voltage values are within reasonable ranges.

**Inputs:**
- `sap_data` (sap_data object)
- `temperature_range` (numeric vector): Acceptable temperature range
- `voltage_range` (numeric vector): Acceptable voltage range

**Outputs:**
- (list): Issues and warnings about out-of-range values

---

### validate_temporal_consistency()
**Exported:** No (internal)

Validates temporal consistency (timestamps, gaps, ordering).

**Inputs:**
- `sap_data` (sap_data object)

**Outputs:**
- (list): Issues and warnings about temporal problems

---

### validate_sensor_consistency()
**Exported:** No (internal)

Validates sensor consistency (missing sensors, flat lines, outliers).

**Inputs:**
- `sap_data` (sap_data object)

**Outputs:**
- (list): Issues and warnings about sensor problems

---

## 3. Probe Configuration (Stage 3)

**File:** `R/03_probe_configuration.R`

### ProbeConfiguration (R6 Class)
**Exported:** Yes

R6 class representing probe configurations with validation and method compatibility checking.

**Fields:**
- `config_name` (character): Configuration name
- `config_type` (character): Type of configuration
- `heater_position` (numeric): Position of heater element (typically 0)
- `sensor_positions` (named list): Sensor positions relative to heater
- `probe_diameter` (numeric): Probe diameter in mm
- `thermal_diffusivity` (numeric): Thermal diffusivity (cm²/s)
- `compatible_methods` (character vector): Compatible calculation methods
- `method_priorities` (character vector): Priority ranking of methods
- `required_parameters` (list): Required parameters for each method

**Methods:**
- `$initialize()`: Create new configuration
- `$validate()`: Validate configuration
- `$is_method_compatible(method_name)`: Check method compatibility
- `$get_recommended_methods(n)`: Get top n recommended methods
- `$print()`: Print configuration summary

---

### detect_probe_config()
**Exported:** Yes

Automatically detects probe configuration from sap_data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `tolerance` (numeric, default: 0.1): Tolerance for sensor position matching

**Outputs:**
- (ProbeConfiguration): Detected configuration object

**Description:**
Analyzes available temperature sensors and determines the probe configuration type (symmetric, asymmetric, single-sided, etc.).

---

### get_standard_configs()
**Exported:** Yes

Returns all standard probe configurations available in the package.

**Inputs:**
None

**Outputs:**
- (list): Named list of ProbeConfiguration objects

**Description:**
Provides pre-defined configurations for common ICT sensor setups.

---

### validate_probe_config()
**Exported:** Yes

Validates a probe configuration object.

**Inputs:**
- `probe_config` (ProbeConfiguration): Configuration to validate

**Outputs:**
- (list): Validation results with valid/issues/warnings

---

### get_method_compatibility_matrix()
**Exported:** Yes

Returns a matrix showing which methods are compatible with which configurations.

**Inputs:**
None

**Outputs:**
- (matrix): Configuration × Method compatibility matrix (logical values)

---

### validate_method_compatibility()
**Exported:** Yes

Checks if a method is compatible with a given configuration.

**Inputs:**
- `method_name` (character): Name of calculation method
- `probe_config` (ProbeConfiguration): Probe configuration
- `strict` (logical, default: FALSE): Strict compatibility checking

**Outputs:**
- (logical): TRUE if compatible

---

## 4. Core HPV Methods (Stage 4)

**File:** `R/04_heat_pulse_velocity_core.R`

### calc_heat_pulse_velocity()
**Exported:** Yes

Main function for calculating heat pulse velocity from sap flow data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `pulse_ids` (integer vector, optional): Specific pulse IDs to process (NULL = all)
- `methods` (character vector, default: c("HRM", "MHR", "DMA")): Calculation methods to use
- `parameters` (list, optional): Calculation parameters (uses defaults if NULL)
- `plot_results` (logical, default: FALSE): Generate diagnostic plots

**Outputs:**
A `vh_results` tibble with columns:
- `datetime`: Timestamp of measurement
- `pulse_id`: Pulse identification number
- `method`: Calculation method used
- `sensor_position`: "inner" or "outer" sensor position
- `Vh_cm_hr`: Heat pulse velocity in cm/hr
- `quality_flag`: Data quality indicator ("OK", "WARNING", "ERROR", "SUSPECT")

**Description:**
Calculates sap velocity using multiple established methods. Automatically handles preprocessing, method execution, and quality flagging.

**Available Methods:**
- "HRM" - Heat Ratio Method (Burgess et al. 2001)
- "MHR" - Maximum Heat Ratio (Lopez et al. 2021)
- "HRMXa" - Modified HRM variant a
- "HRMXb" - Modified HRM variant b
- "Tmax_Coh" - T-max Cohen method (Cohen et al. 1981)
- "Tmax_Klu" - T-max Kluitenberg method (Kluitenberg & Ham 2004)
- "DMA" - Dual Method Approach (automatic method switching)

**Parameters:**
- `diffusivity` (default: 0.0025): Thermal diffusivity of sapwood (cm²/s)
- `x` (default: 0.5): Distance from heat source (cm)
- `L` (default: 0.5): Lower proportion of deltaTmax for HRMX
- `H` (default: 0.8): Higher proportion of deltaTmax for HRMX
- `tp_1` (default: 2): Heat pulse duration (sec) for Tmax_Klu
- `HRM_start` (default: 60): Start of sampling window for HRM (sec after pulse)
- `HRM_end` (default: 100): End of sampling window for HRM (sec after pulse)
- `pre_pulse` (default: 30): Pre-pulse period (sec)

---

### calc_hrm()
**Exported:** No (internal)

Calculates heat pulse velocity using the Heat Ratio Method.

**Inputs:**
- `dTratio_douo` (numeric vector): Temperature ratio downstream/upstream (outer)
- `dTratio_diui` (numeric vector): Temperature ratio downstream/upstream (inner)
- `HRM_period` (logical vector): Sampling window
- `diffusivity` (numeric): Thermal diffusivity
- `x` (numeric): Probe spacing

**Outputs:**
- (list): Vh_outer and Vh_inner velocities

---

### calc_mhr()
**Exported:** No (internal)

Calculates heat pulse velocity using the Maximum Heat Ratio Method.

**Inputs:**
- `dTratio_douo` (numeric vector): Temperature ratio (outer)
- `dTratio_diui` (numeric vector): Temperature ratio (inner)
- `diffusivity` (numeric): Thermal diffusivity
- `x` (numeric): Probe spacing

**Outputs:**
- (list): Vh_outer and Vh_inner velocities using maximum ratios

---

### calc_hrmx()
**Exported:** No (internal)

Calculates heat pulse velocity using modified HRM variants (HRMXa/HRMXb).

**Inputs:**
- `delatT_do`, `delatT_di`, `delatT_uo`, `delatT_ui` (numeric vectors): Temperature differences
- `L` (numeric): Lower window bound proportion
- `H` (numeric): Upper window bound proportion
- `diffusivity` (numeric): Thermal diffusivity
- `x` (numeric): Probe spacing
- `variant` (character): "HRMXa" or "HRMXb"

**Outputs:**
- (list): Vh_outer and Vh_inner velocities

---

### calc_tmax_coh()
**Exported:** No (internal)

Calculates heat pulse velocity using T-max Cohen method.

**Inputs:**
- `tp` (numeric vector): Time after heat pulse
- `delatT_do`, `delatT_uo` (numeric vectors): Temperature differences
- `diffusivity` (numeric): Thermal diffusivity
- `x` (numeric): Probe spacing

**Outputs:**
- (list): Vh_outer velocity using time-to-peak approach

---

### calc_tmax_klu()
**Exported:** No (internal)

Calculates heat pulse velocity using T-max Kluitenberg method (corrected for heat pulse duration).

**Inputs:**
- `tp` (numeric vector): Time after heat pulse
- `delatT_do`, `delatT_uo` (numeric vectors): Temperature differences
- `diffusivity` (numeric): Thermal diffusivity
- `x` (numeric): Probe spacing
- `tp_1` (numeric): Heat pulse duration

**Outputs:**
- (list): Vh_outer velocity with duration correction

---

### calc_dma()
**Exported:** No (internal)

Calculates heat pulse velocity using Dual Method Approach (automatically switches between HRM and Tmax based on velocity).

**Inputs:**
- Multiple temperature parameters
- `diffusivity`, `x`, `tp_1`: Physical parameters

**Outputs:**
- (list): Vh_outer and Vh_inner using optimal method for velocity range

---

### get_default_parameters()
**Exported:** Yes

Returns default calculation parameters.

**Inputs:**
None

**Outputs:**
- (list): Default parameter values for all methods

---

### create_test_sap_data()
**Exported:** Yes

Creates synthetic test data for package development and testing.

**Inputs:**
- `n_pulses` (integer, default: 5): Number of pulses to generate
- `n_measurements_per_pulse` (integer, default: 120): Measurements per pulse

**Outputs:**
- (sap_data): Synthetic sap_data object with known properties

---

## 5. Advanced HPV Methods (Stage 4)

**File:** `R/05_heat_pulse_velocity_advanced.R`

### calc_unified_hpv()
**Exported:** Yes

Advanced unified HPV calculator with automatic method selection and quality assessment.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `pulse_ids` (integer vector, optional): Pulses to process
- `methods` (character vector, default: c("DRM", "CHPM", "HRM", "MHR")): Methods to try
- `parameters` (list, optional): Calculation parameters
- `select_best` (logical, default: TRUE): Automatically select best method per pulse
- `return_all` (logical, default: FALSE): Return results from all methods

**Outputs:**
- (vh_results tibble): With additional columns for method reasoning and quality scores

**Description:**
Advanced calculator that tries multiple methods and selects the best result based on data quality, method applicability, and consistency checks.

---

### calc_chpm()
**Exported:** Yes

Calculates velocity using Calibrated Half-Maximum Point Method.

**Inputs:**
- `temp_data` (data frame): Temperature data for single pulse
- `parameters` (list): Calculation parameters

**Outputs:**
- (list): Velocity, applicability flag, quality metrics

**Description:**
Uses the half-maximum point of temperature rise for velocity calculation. Best for moderate to high velocities with clear temperature peaks.

---

### calc_drm()
**Exported:** Yes

Calculates velocity using Double Ratio Method.

**Inputs:**
- `temp_data` (data frame): Temperature data for single pulse
- `parameters` (list): Calculation parameters

**Outputs:**
- (list): Velocity, quality metrics, window optimization results

**Description:**
Uses ratios from both upstream and downstream sensors with adaptive window selection. Robust for noisy data.

---

### detect_optimal_method()
**Exported:** Yes

Automatically detects the optimal calculation method based on data characteristics and probe configuration.

**Inputs:**
- `temp_data` (data frame): Temperature data for single pulse
- `probe_config` (ProbeConfiguration): Probe configuration
- `parameters` (list, optional): Calculation parameters

**Outputs:**
- (character): Recommended method name

**Description:**
Analyzes signal quality, flow velocity range, sensor configuration, and data quality to recommend the best method.

---

### assess_temp_data_quality()
**Exported:** Yes

Evaluates signal-to-noise ratio of temperature measurements.

**Inputs:**
- `temp_data` (data frame): Temperature data
- `min_signal_ratio` (numeric, default: 3): Minimum acceptable signal-to-noise ratio

**Outputs:**
A list containing:
- `individual_sensors`: SNR for each temperature sensor
- `overall_adequate`: Boolean if all sensors meet threshold
- `min_snr`: Minimum signal-to-noise ratio found
- `quality_flag`: "good" or "poor"

---

### estimate_realtime_diffusivity()
**Exported:** Yes

Estimates thermal diffusivity in real-time from temperature data.

**Inputs:**
- `temp_data` (data frame): Temperature data
- `probe_spacing` (numeric): Distance between sensors (cm)

**Outputs:**
- (numeric): Estimated thermal diffusivity (cm²/s)

**Description:**
Uses temperature decay rates to estimate wood thermal properties in real-time. Useful for variable moisture conditions.

---

## 6. YAML Methods (Stage 4)

**File:** `R/06_heat_pulse_velocity_yaml.R`

### calc_heat_pulse_velocity_yaml()
**Exported:** Yes

Calculates heat pulse velocity using YAML-defined methods.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `methods` (character vector): YAML method names to use
- `parameters` (list, optional): Calculation parameters

**Outputs:**
- (vh_results tibble): Calculated velocities

**Description:**
Executes methods defined in YAML configuration files, allowing researchers to define new methods without writing R code.

---

### list_available_methods()
**Exported:** Yes

Lists all available YAML-defined methods.

**Inputs:**
None

**Outputs:**
- (data frame): Method names, descriptions, and file paths

---

### load_method_config()
**Exported:** Yes

Loads a YAML method configuration file.

**Inputs:**
- `method_name` (character): Name of method to load

**Outputs:**
- (list): Parsed YAML configuration

---

### execute_yaml_method()
**Exported:** No (internal)

Executes a single YAML-defined method on temperature data.

**Inputs:**
- `temp_data` (data frame): Temperature data
- `method_config` (list): Parsed YAML configuration
- `parameters` (list): Calculation parameters

**Outputs:**
- (list): Calculated velocity and quality metrics

---

## 7. Method Comparison (Stage 5 & 10)

**File:** `R/07_method_comparison.R`

### compare_hpv_methods()
**Exported:** Yes

Compares results from multiple HPV calculation methods.

**Inputs:**
- `vh_results` (vh_results tibble): Results from calc_heat_pulse_velocity()
- `methods` (character vector, optional): Methods to compare (NULL = all)
- `group_by` (character, default: "pulse_id"): Grouping variable

**Outputs:**
A list containing:
- `summary_stats`: Summary statistics by method
- `correlations`: Correlation matrix between methods
- `agreements`: Bland-Altman agreement statistics
- `bias`: Mean bias between method pairs

**Description:**
Comprehensive comparison including statistical tests, correlation analysis, and agreement measures.

---

### plot_method_comparison()
**Exported:** Yes

Creates diagnostic plots comparing multiple methods.

**Inputs:**
- `vh_results` (vh_results tibble): Results from calc_heat_pulse_velocity()
- `methods` (character vector, optional): Methods to compare
- `plot_type` (character, default: "scatter"): Type of plot ("scatter", "timeseries", "bland_altman", "correlation")

**Outputs:**
- (ggplot): Comparison plot

---

## 8. Quality Control (Stage 5 & 10)

**File:** `R/08_quality_control.R`

### diagnose_sensor_performance()
**Exported:** Yes

Diagnoses sensor performance issues from sap_data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `check_battery` (logical, default: TRUE): Check battery voltage issues
- `check_sensors` (logical, default: TRUE): Check sensor issues
- `check_alignment` (logical, default: TRUE): Check probe alignment

**Outputs:**
A list containing:
- `overall_health`: "good", "fair", or "poor"
- `battery_diagnostics`: Battery performance metrics
- `sensor_diagnostics`: Sensor-specific diagnostics
- `alignment_diagnostics`: Probe alignment assessment
- `recommendations`: List of recommended actions

---

### detect_sensor_outliers()
**Exported:** Yes

Detects outliers in temperature sensor data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `method` (character, default: "IQR"): Outlier detection method ("IQR", "zscore", "mad")
- `threshold` (numeric): Threshold for outlier detection

**Outputs:**
- (data frame): Outlier flags and scores for each measurement

---

### validate_probe_alignment_advanced()
**Exported:** Yes

Advanced probe alignment validation using temperature symmetry analysis.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `tolerance` (numeric, default: 0.1): Alignment tolerance

**Outputs:**
A list containing:
- `aligned`: Boolean indicating if probes are properly aligned
- `symmetry_scores`: Symmetry metrics
- `issues`: List of alignment issues found

---

## 9. Diagnostic Reporting (Stage 5 & 10)

**File:** `R/09_diagnostic_reporting.R`

### generate_diagnostic_report()
**Exported:** Yes

Generates a comprehensive diagnostic report for sap flow data analysis.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `vh_results` (vh_results tibble, optional): Velocity calculation results
- `output_file` (character): Output file path (.html, .md, or .pdf)
- `template` (character, default: "standard"): Report template ("standard", "research", "technical", "monitoring")
- `include_plots` (logical, default: TRUE): Include diagnostic plots
- `include_raw_data` (logical, default: FALSE): Include raw data tables

**Outputs:**
- Creates report file and returns path

**Description:**
Creates publication-ready diagnostic reports with data overview, quality assessment, method comparison, statistical summaries, and recommendations.

---

### create_diagnostic_summary()
**Exported:** No (internal)

Creates a summary of diagnostic information.

**Inputs:**
- `sap_data` (sap_data object)
- `vh_results` (vh_results tibble, optional)

**Outputs:**
- (list): Structured diagnostic summary

---

## 10. Temporal Processing (Stage 6)

**File:** `R/10_temporal_processing.R`

### aggregate_velocity_temporal()
**Exported:** Yes

Aggregates heat pulse velocity data to different time scales.

**Inputs:**
- `vh_results` (vh_results tibble): Results from calc_heat_pulse_velocity()
- `time_scale` (character, default: "hourly"): Aggregation scale ("hourly", "daily", "weekly", "monthly")
- `agg_function` (function, default: mean): Aggregation function
- `min_observations` (integer, default: 1): Minimum observations required per period

**Outputs:**
- (vh_results tibble): Aggregated velocity data with additional temporal columns

---

### interpolate_missing_velocity()
**Exported:** Yes

Interpolates missing velocity data using various methods.

**Inputs:**
- `vh_results` (vh_results tibble): Results with potential gaps
- `method` (character, default: "linear"): Interpolation method ("linear", "spline", "locf", "nocb")
- `max_gap` (integer, default: NULL): Maximum gap size to interpolate (NULL = no limit)

**Outputs:**
- (vh_results tibble): Data with interpolated values

---

## 11. Flux Density (Stage 7)

**File:** `R/11_flux_density.R`

### calc_sap_flux_density()
**Exported:** Yes

Converts heat pulse velocity (Vh) to sap flux density (Js).

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results from calc_heat_pulse_velocity()
- `sapwood_density` (numeric, default: 1000): Sapwood density (kg/m³)
- `wood_water_content` (numeric, default: 0.5): Volumetric water content (0-1)
- `correction_factor` (numeric, default: 1): Species-specific correction factor

**Outputs:**
A `sap_flux_density` object (tibble) with columns:
- Original vh_results columns
- `Js_g_m2_s`: Sap flux density in g m⁻² s⁻¹
- `Js_kg_m2_hr`: Sap flux density in kg m⁻² hr⁻¹
- `Js_L_m2_day`: Sap flux density in L m⁻² day⁻¹

**Description:**
Applies Cermak et al. (2004) correction to convert velocity to mass flux. Accounts for wood density and water content.

---

## 12. Sapwood Area (Stage 8)

**File:** `R/12_sapwood_area.R`

### calc_sapwood_area()
**Exported:** Yes

Calculates sapwood area from tree measurements.

**Inputs:**
- `dbh` (numeric): Diameter at breast height (cm)
- `bark_thickness` (numeric, optional): Bark thickness (cm). Estimated if NULL.
- `heartwood_radius` (numeric, optional): Heartwood radius (cm). Estimated if NULL.
- `species` (character, optional): Tree species for improved estimates
- `method` (character, default: "direct"): Calculation method ("direct", "allometric")

**Outputs:**
A `sapwood_area` object (list) containing:
- `sapwood_area_cm2`: Sapwood area in cm²
- `sapwood_area_m2`: Sapwood area in m²
- `dbh`: Input DBH
- `bark_thickness`: Bark thickness used
- `heartwood_radius`: Heartwood radius used
- `method`: Calculation method
- `species`: Species (if provided)

**Description:**
Calculates conducting sapwood area accounting for bark and heartwood. Uses allometric equations when direct measurements unavailable.

---

### estimate_bark_thickness()
**Exported:** Yes

Estimates bark thickness from DBH and optional species information.

**Inputs:**
- `dbh` (numeric): Diameter at breast height (cm)
- `species` (character, optional): Tree species

**Outputs:**
- (numeric): Estimated bark thickness (cm)

---

### estimate_heartwood_radius()
**Exported:** Yes

Estimates heartwood radius from DBH and optional species information.

**Inputs:**
- `dbh` (numeric): Diameter at breast height (cm)
- `species` (character, optional): Tree species

**Outputs:**
- (numeric): Estimated heartwood radius (cm)

---

## 13. Tree Water Use (Stage 9)

**File:** `R/13_tree_water_use.R`

### calc_tree_sap_flux()
**Exported:** Yes

Scales sap flux density to whole-tree water use.

**Inputs:**
- `flux_density` (sap_flux_density object): From calc_sap_flux_density()
- `sapwood_area` (sapwood_area object or numeric): Sapwood area (cm² or object from calc_sapwood_area())
- `radial_variation` (character, default: "none"): Account for radial variation ("none", "linear", "exponential")
- `azimuthal_variation` (character, default: "none"): Account for azimuthal variation ("none", "cardinal")

**Outputs:**
A `tree_sap_flux` object (tibble) with columns:
- Original flux_density columns
- `Q_L_hr`: Whole-tree sap flux in L hr⁻¹
- `Q_L_day`: Whole-tree sap flux in L day⁻¹
- `Q_kg_day`: Whole-tree water use in kg day⁻¹

**Description:**
Scales point measurements to whole-tree water use. Can account for spatial variation in sap flux within the sapwood.

---

## 14. Processing Pipeline (Unified)

**File:** `R/14_processing_pipeline.R`

### process_sap_data()
**Exported:** Yes

Unified processing pipeline from raw data to tree-level water use.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `methods` (character vector, optional): HPV methods to use
- `parameters` (list, optional): Calculation parameters
- `dbh` (numeric, optional): Tree DBH for scaling (cm)
- `sapwood_area` (numeric, optional): Sapwood area (cm²)
- `assess_quality` (logical, default: TRUE): Perform quality assessment
- `recommend_methods` (logical, default: TRUE): Automatically recommend methods
- `aggregate_temporal` (character, optional): Time scale for aggregation

**Outputs:**
A `sap_flow_results` object (list) containing:
- `vh_results`: Heat pulse velocity results
- `flux_density`: Sap flux density (if sapwood parameters provided)
- `tree_flux`: Whole-tree flux (if DBH provided)
- `quality_assessment`: Quality diagnostics
- `method_recommendations`: Recommended methods
- `processing_metadata`: Processing parameters and timestamps

**Description:**
One-stop processing function that handles the complete workflow. Automatically performs quality checks and method selection.

---

### assess_data_quality()
**Exported:** Yes

Assesses overall data quality for the processing pipeline.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()

**Outputs:**
A list containing:
- `overall_quality`: "excellent", "good", "fair", or "poor"
- `quality_scores`: Numeric scores for different aspects
- `issues`: List of quality issues identified
- `recommendations`: Recommended actions

---

### recommend_methods()
**Exported:** Yes

Recommends optimal HPV calculation methods based on data characteristics.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `probe_config` (ProbeConfiguration, optional): Probe configuration
- `n_methods` (integer, default: 3): Number of methods to recommend

**Outputs:**
A list containing:
- `recommended`: Character vector of method names (in priority order)
- `reasoning`: Explanations for each recommendation
- `compatibility`: Compatibility scores for all methods

---

## 15. Export & Reporting

**File:** `R/export_reporting.R`

### export_sap_data()
**Exported:** Yes

Exports sap flow data and results to various formats.

**Inputs:**
- `data` (sap_data, vh_results, or sap_flow_results): Data to export
- `output_file` (character): Output file path
- `format` (character, default: "csv"): Export format ("csv", "xlsx", "json", "rds", "txt")
- `include_metadata` (logical, default: TRUE): Include metadata
- `include_quality_summary` (logical, default: FALSE): Include quality summary
- `filter_quality` (character, optional): Filter by quality flags

**Outputs:**
- Creates output file and returns path

**Description:**
Flexible export function supporting multiple formats with optional metadata and quality filtering.

---

### batch_process_sap_flow()
**Exported:** Yes

Batch processes multiple sap flow data files.

**Inputs:**
- `file_paths` (character vector): Paths to data files
- `tree_ids` (character vector, optional): Tree identifiers
- `methods` (character vector): HPV methods to use
- `parameters` (list, optional): Calculation parameters
- `output_dir` (character): Directory for output files
- `parallel` (logical, default: FALSE): Use parallel processing

**Outputs:**
- (list): Processing results for each file
- Creates individual output files

---

### calc_enhanced_statistics()
**Exported:** Yes

Calculates enhanced statistical summaries for velocity results.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `group_by` (character, optional): Grouping variable(s)
- `include_distribution_tests` (logical, default: FALSE): Include normality/distribution tests

**Outputs:**
- (data frame): Enhanced statistics including mean, median, SD, CV, skewness, kurtosis, percentiles

---

### generate_analysis_report()
**Exported:** Yes

Generates a comprehensive analysis report with statistical tests and visualizations.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `output_file` (character): Output file path (.html or .pdf)
- `template` (character, default: "standard"): Report template
- `include_statistical_tests` (logical, default: TRUE): Include statistical tests
- `significance_level` (numeric, default: 0.05): Significance level for tests

**Outputs:**
- Creates report file and returns path

---

## 16. Multi-Tree Utilities

**File:** `R/multi_tree_utilities.R`

### compare_trees()
**Exported:** Yes

Compares sap flux between multiple trees.

**Inputs:**
- `multi_tree_data` (multi_tree_data object): Multiple sap_data objects
- `comparison_type` (character, default: "all"): Type of comparison ("summary", "statistical", "temporal", "methods", "all")
- `methods` (character vector, optional): HPV methods to use

**Outputs:**
A `tree_comparison` object (list) containing:
- `summary`: Summary statistics by tree
- `statistical_tests`: ANOVA, Kruskal-Wallis, etc.
- `temporal_patterns`: Time series comparison
- `method_performance`: Method performance by tree

---

### compare_trees_summary()
**Exported:** No (internal)

Creates summary comparison between trees.

**Inputs:**
- `multi_tree_data` (multi_tree_data object)

**Outputs:**
- (data frame): Summary statistics by tree

---

### compare_trees_statistical()
**Exported:** No (internal)

Performs statistical tests comparing trees.

**Inputs:**
- `vh_results_list` (list): Velocity results for each tree

**Outputs:**
- (list): Statistical test results

---

## 17. Visualisation

**Files:** `R/visualisation_core.R`, `R/visualisation_utilities.R`

### plot_hpv_timeseries()
**Exported:** Yes

Creates time series plot of heat pulse velocity data.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `colour_by` (character, default: "method"): Variable for coloring ("method", "sensor_position", "quality_flag")
- `facet_by` (character, optional): Variable for faceting
- `smooth` (logical, default: FALSE): Add smoothing line
- `quality_filter` (character vector, optional): Filter by quality flags

**Outputs:**
- (ggplot): Time series plot

---

### plot_temperature_data()
**Exported:** Yes

Creates diagnostic plots of raw temperature data.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `pulse_ids` (integer vector, optional): Specific pulses to plot
- `plot_type` (character, default: "timeseries"): Plot type ("timeseries", "heatmap", "differences")

**Outputs:**
- (ggplot): Temperature diagnostic plot

---

### plot_diagnostics()
**Exported:** Yes

Creates comprehensive diagnostic plot panel.

**Inputs:**
- `sap_data` (sap_data object): Data from read_sap_data()
- `vh_results` (vh_results tibble, optional): Velocity results
- `plot_types` (character vector, default: c("temperature", "velocity", "quality", "methods")): Types of plots to include

**Outputs:**
- (ggplot or list of ggplots): Diagnostic panel

---

### theme_sapfluxr()
**Exported:** Yes

Custom ggplot2 theme for sapFluxR plots.

**Inputs:**
- `base_size` (numeric, default: 12): Base font size
- `base_family` (character, default: ""): Base font family

**Outputs:**
- (theme): ggplot2 theme object

---

### get_method_colours()
**Exported:** Yes

Returns consistent color palette for HPV methods.

**Inputs:**
- `methods` (character vector, optional): Method names
- `palette` (character, default: "default"): Color palette ("default", "colourblind", "viridis")

**Outputs:**
- (named vector): Colors for each method

---

### save_publication_plot()
**Exported:** Yes

Saves plot in publication-ready format.

**Inputs:**
- `plot` (ggplot): Plot to save
- `filename` (character): Output filename
- `width` (numeric, default: 7): Width in inches
- `height` (numeric, default: 5): Height in inches
- `dpi` (numeric, default: 300): Resolution
- `format` (character, default: "png"): Output format

**Outputs:**
- Creates plot file

---

## 18. Utilities

**File:** `R/utilities.R`

### calc_velocity_stats()
**Exported:** Yes

Calculates summary statistics for velocity results.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `group_by` (character, optional): Grouping variable(s)
- `stats` (character vector, default: c("mean", "median", "sd", "min", "max")): Statistics to calculate

**Outputs:**
- (data frame): Summary statistics

---

### filter_velocity_results()
**Exported:** Yes

Filters velocity results based on quality flags and value ranges.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `quality_flags` (character vector, optional): Quality flags to keep ("OK", "WARNING", etc.)
- `velocity_range` (numeric vector, optional): Min/max velocity range
- `methods` (character vector, optional): Methods to keep
- `remove_na` (logical, default: TRUE): Remove NA values

**Outputs:**
- (vh_results tibble): Filtered results

---

### convert_velocity_units()
**Exported:** Yes

Converts heat pulse velocity between different units.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `from_units` (character, default: "cm_hr"): Current units
- `to_units` (character): Target units ("cm_hr", "mm_hr", "m_day", "cm_s", "mm_s")

**Outputs:**
- (vh_results tibble): Results with converted units

---

### export_velocity_results()
**Exported:** Yes

Exports velocity results to CSV with optional quality summary.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `output_file` (character): Output file path
- `include_quality_summary` (logical, default: FALSE): Include quality summary

**Outputs:**
- Creates CSV file and returns path

---

### plot_velocity_diagnostics()
**Exported:** Yes

Creates diagnostic plots for velocity results.

**Inputs:**
- `vh_results` (vh_results tibble): Velocity results
- `plot_type` (character): Type of plot ("histogram", "boxplot", "timeseries", "methods")

**Outputs:**
- (ggplot): Diagnostic plot

---

### check_package_status()
**Exported:** Yes

Checks sapFluxR package installation and dependencies.

**Inputs:**
None

**Outputs:**
- (list): Package status information including version, dependencies, and optional packages

---

## Quality Flags

All velocity calculation functions assign quality flags to results:

- **"OK"**: Data passes all quality checks
- **"WARNING"**: Data has minor issues but is usable (e.g., slightly high velocity)
- **"ERROR"**: Data fails critical quality checks (e.g., infinite values, missing sensors)
- **"SUSPECT"**: Data is questionable (e.g., outliers, unusual patterns)

---

## References

- **Burgess, S.S.O., et al. (2001).** An improved heat pulse method to measure low and reverse rates of sap flow in woody plants. *Tree Physiology* 21:589-598.
- **Lopez, B.C., et al. (2021).** Maximum heat ratio method for sap flow measurement. *Plant and Soil* 469:503-523.
- **Cohen, Y., et al. (1981).** Improvement of the heat pulse method for determining sap flow in trees. *Plant, Cell and Environment* 4:391-397.
- **Kluitenberg, G.J., & Ham, J.M. (2004).** Improved theory for calculating sap flow with the heat pulse method. *Agricultural and Forest Meteorology* 126:169-173.
- **Cermak, J., et al. (2004).** Sapwood as the scaling parameter - defining according to xylem water content or radial pattern of sap flow? *Annals of Forest Science* 61:491-497.

---

## Version History

See [NEWS.md](NEWS.md) for version history and changes.

---

**Last updated:** 2025-10-10
**Package version:** 0.1.0
