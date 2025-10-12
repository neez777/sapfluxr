# sapFluxR User Inputs Guide

This document provides a comprehensive guide to all user inputs required for the sapFluxR package, organized by category and function.

## Table of Contents

1. [Data Input](#data-input)
2. [Wood Properties](#wood-properties)
3. [Probe Properties](#probe-properties)
4. [Tree Measurements](#tree-measurements)
5. [Calculation Parameters](#calculation-parameters)
6. [Method Selection](#method-selection)
7. [Quality Control](#quality-control)
8. [Export Options](#export-options)
9. [Processing Options](#processing-options)

---

## Data Input

### Primary Data File
- **Variable**: `file_path`
- **Type**: Character string
- **Description**: Path to sap flow data file
- **Required**: Yes
- **Supported Formats**:
  - ICT Current format (JSON-like)
  - ICT Legacy format
  - CSV/Tab-delimited format
- **Example**: `"data/sap_flow_2024.txt"`

### Data Import Options
- **Variable**: `format`
- **Type**: Character string
- **Description**: Data format specification
- **Required**: No (auto-detected if NULL)
- **Options**: `"ict_current"`, `"ict_legacy"`, `"csv"`
- **Default**: Auto-detected

- **Variable**: `validate_data`
- **Type**: Logical
- **Description**: Whether to validate imported data
- **Required**: No
- **Default**: `TRUE`

- **Variable**: `chunk_size`
- **Type**: Integer
- **Description**: Characters per chunk for large files
- **Required**: No
- **Default**: Auto-sized based on file size

- **Variable**: `show_progress`
- **Type**: Logical
- **Description**: Show progress updates
- **Required**: No
- **Default**: `TRUE` for files >1MB

---

## Wood Properties

### Required Wood Properties
These properties are required for converting heat pulse velocity to sap flux density:

#### Fresh Wood Density
- **Variable**: `fresh_density`
- **Type**: Numeric
- **Description**: Fresh sapwood density
- **Units**: g/cm³
- **Range**: 0.3 - 1.2 g/cm³
- **Default**: 0.8 g/cm³ (typical hardwood)
- **Species Examples**:
  - Eucalyptus: 0.7-0.9 g/cm³
  - Pine: 0.4-0.6 g/cm³
  - Oak: 0.6-0.8 g/cm³

#### Dry Wood Density
- **Variable**: `dry_density`
- **Type**: Numeric
- **Description**: Dry wood density
- **Units**: g/cm³
- **Range**: 0.2 - 0.8 g/cm³
- **Default**: 0.55 g/cm³
- **Species Examples**:
  - Eucalyptus: 0.5-0.7 g/cm³
  - Pine: 0.3-0.5 g/cm³
  - Oak: 0.4-0.6 g/cm³

#### Moisture Content
- **Variable**: `moisture_content`
- **Type**: Numeric
- **Description**: Gravimetric moisture content
- **Units**: Decimal (0-1)
- **Range**: 0.0 - 1.0 (0-100%)
- **Default**: 0.45 (45%)
- **Typical Range**: 0.3-0.6 (30-60%)

#### Specific Heat Capacity
- **Variable**: `specific_heat_capacity`
- **Type**: Numeric
- **Description**: Specific heat capacity of dry wood
- **Units**: J/g/°C
- **Range**: 1.0 - 2.0 J/g/°C
- **Default**: 1.5 J/g/°C
- **Species Examples**:
  - Most hardwoods: 1.3-1.7 J/g/°C
  - Most softwoods: 1.4-1.8 J/g/°C

### Thermal Diffusivity
- **Variable**: `thermal_diffusivity`
- **Type**: Numeric
- **Description**: Thermal diffusivity of fresh sapwood
- **Units**: cm²/s
- **Range**: 0.001 - 0.01 cm²/s
- **Default**: 0.0025 cm²/s
- **Species Examples**:
  - Eucalyptus: 0.002-0.003 cm²/s
  - Pine: 0.0025-0.0035 cm²/s
  - Oak: 0.002-0.0025 cm²/s

---

## Probe Properties

### Probe Configuration
- **Variable**: `config_name`
- **Type**: Character string
- **Description**: Name of probe configuration
- **Options**: `"three_probe_symmetric"`, `"three_probe_asymmetric"`, `"four_probe_extended"`
- **Default**: Auto-detected

### Sensor Positions
- **Variable**: `sensor_positions`
- **Type**: Named list
- **Description**: Sensor positions relative to heater
- **Format**: `list(upstream = -6, downstream = 6)`
- **Units**: mm
- **Standard Configurations**:
  - Three-probe symmetric: upstream = -6, downstream = 6
  - Three-probe asymmetric: upstream = -5, downstream = 10
  - Four-probe extended: upstream = -6, downstream = 6, additional sensors

### Probe Diameter
- **Variable**: `probe_diameter`
- **Type**: Numeric
- **Description**: Probe diameter
- **Units**: mm
- **Range**: 1.0 - 2.0 mm
- **Default**: 1.27 mm (standard ICT probe)

### Heater Position
- **Variable**: `heater_position`
- **Type**: Numeric
- **Description**: Position of heater element
- **Units**: mm
- **Default**: 0 (center)

---

## Tree Measurements

### Required Measurements

#### Diameter at Breast Height (DBH)
- **Variable**: `diameter_breast_height`
- **Type**: Numeric
- **Description**: Diameter at breast height (1.3m)
- **Units**: cm
- **Range**: 5 - 200 cm
- **Required**: Yes
- **Typical Range**: 10-50 cm

### Optional Measurements

#### Bark Thickness
- **Variable**: `bark_thickness`
- **Type**: Numeric
- **Description**: Bark thickness
- **Units**: cm
- **Range**: 0.1 - 5.0 cm
- **Default**: Estimated from DBH
- **Estimation Methods**: Allometric relationships

#### Heartwood Dimensions
Choose one of the following:

##### Heartwood Radius
- **Variable**: `heartwood_radius`
- **Type**: Numeric
- **Description**: Heartwood radius
- **Units**: cm
- **Range**: 0 - (stem_radius - bark_thickness)
- **Default**: Estimated from DBH

##### Heartwood Diameter
- **Variable**: `heartwood_diameter`
- **Type**: Numeric
- **Description**: Heartwood diameter
- **Units**: cm
- **Range**: 0 - (stem_diameter - 2*bark_thickness)
- **Default**: Estimated from DBH

##### Sapwood Thickness
- **Variable**: `sapwood_thickness`
- **Type**: Numeric
- **Description**: Sapwood thickness
- **Units**: cm
- **Range**: 1.0 - 10.0 cm
- **Default**: Calculated from other measurements

### Estimation Parameters

#### Estimation Method
- **Variable**: `estimation_method`
- **Type**: Character string
- **Description**: Method for estimating missing parameters
- **Options**: `"allometric"`, `"conservative"`, `"species_specific"`
- **Default**: `"allometric"`

#### Species Group
- **Variable**: `species_group`
- **Type**: Character string
- **Description**: Species group for parameter estimation
- **Options**: `"hardwood"`, `"softwood"`, `"eucalyptus"`, `"pine"`
- **Default**: `"hardwood"`

---

## Calculation Parameters

### Core Parameters

#### Thermal Diffusivity
- **Variable**: `diffusivity`
- **Type**: Numeric
- **Description**: Thermal diffusivity of sapwood
- **Units**: cm²/s
- **Range**: 0.001 - 0.01 cm²/s
- **Default**: 0.0025 cm²/s
- **Validation**: Must be > 0 and ≤ 0.01

#### Probe Spacing
- **Variable**: `x`
- **Type**: Numeric
- **Description**: Distance from heat source
- **Units**: cm
- **Range**: 0.1 - 2.0 cm
- **Default**: 0.5 cm
- **Validation**: Must be > 0 and ≤ 2

### Timing Parameters

#### Pre-pulse Period
- **Variable**: `pre_pulse`
- **Type**: Numeric
- **Description**: Pre-pulse baseline period
- **Units**: seconds
- **Range**: 10 - 60 seconds
- **Default**: 30 seconds
- **Validation**: Must be > 0 and ≤ 60

#### HRM Sampling Window
- **Variable**: `HRM_start`
- **Type**: Numeric
- **Description**: Start of HRM sampling window
- **Units**: seconds after pulse
- **Range**: 30 - 120 seconds
- **Default**: 60 seconds
- **Validation**: Must be > pre_pulse

- **Variable**: `HRM_end`
- **Type**: Numeric
- **Description**: End of HRM sampling window
- **Units**: seconds after pulse
- **Range**: 60 - 200 seconds
- **Default**: 100 seconds
- **Validation**: Must be > HRM_start

#### Heat Pulse Duration
- **Variable**: `tp_1`
- **Type**: Numeric
- **Description**: Heat pulse duration for Tmax_Klu method
- **Units**: seconds
- **Range**: 1 - 10 seconds
- **Default**: 2 seconds
- **Validation**: Must be > 0 and ≤ 10

### HRMX Parameters

#### HRMX Window Bounds
- **Variable**: `L`
- **Type**: Numeric
- **Description**: Lower proportion of deltaTmax for HRMX sampling window
- **Units**: Proportion (0-1)
- **Range**: 0.3 - 0.7
- **Default**: 0.5

- **Variable**: `H`
- **Type**: Numeric
- **Description**: Higher proportion of deltaTmax for HRMX sampling window
- **Units**: Proportion (0-1)
- **Range**: 0.6 - 0.9
- **Default**: 0.8
- **Validation**: Must be > L

---

## Method Selection

### Calculation Methods
- **Variable**: `methods`
- **Type**: Character vector
- **Description**: Heat pulse velocity calculation methods
- **Options**:
  - `"HRM"`: Heat Ratio Method (best for low flows)
  - `"MHR"`: Maximum Heat Ratio (good for moderate flows)
  - `"HRMXa"`: Modified HRM variant A
  - `"HRMXb"`: Modified HRM variant B
  - `"Tmax_Coh"`: T-max Cohen method (high flows)
  - `"Tmax_Klu"`: T-max Kluitenberg method (high flows)
  - `"DMA"`: Dual Method Approach (automatic switching)
- **Default**: `c("HRM", "MHR", "DMA")`

### Method Compatibility
Methods are automatically filtered based on probe configuration:
- **Three-probe symmetric**: All methods available
- **Three-probe asymmetric**: Limited to CHPM, Tmax methods
- **Four-probe extended**: All methods available

---

## Quality Control

### Data Validation Ranges

#### Temperature Range
- **Variable**: `temperature_range`
- **Type**: Numeric vector (length 2)
- **Description**: Valid temperature range for sensor data
- **Units**: °C
- **Default**: `c(-10, 60)`
- **Range**: `c(-20, 80)` (extended range)

#### Voltage Range
- **Variable**: `voltage_range`
- **Type**: Numeric vector (length 2)
- **Description**: Valid voltage range for battery/external power
- **Units**: Volts
- **Default**: `c(0, 30)`
- **Range**: `c(0, 50)` (extended range)

#### Validation Strictness
- **Variable**: `strict_validation`
- **Type**: Logical
- **Description**: Apply strict validation rules
- **Default**: `FALSE`
- **Options**: `TRUE` (strict), `FALSE` (lenient)

### Quality Filtering

#### Quality Flags
- **Variable**: `quality_flags`
- **Type**: Character vector
- **Description**: Quality flags to include in results
- **Options**:
  - `"OK"`: Normal results
  - `"HIGH_VELOCITY"`: Extremely high velocities (>200 cm/hr)
  - `"NEGATIVE_FLOW"`: Strong reverse flow (<-50 cm/hr)
  - `"MISSING"`: Missing or invalid data
  - `"INFINITE"`: Infinite values
- **Default**: `"OK"`

#### Velocity Range
- **Variable**: `velocity_range`
- **Type**: Numeric vector (length 2)
- **Description**: Valid velocity range for filtering
- **Units**: cm/hr
- **Default**: `c(-50, 200)`
- **Range**: `c(-100, 500)` (extended range)

---

## Export Options

### File Format
- **Variable**: `format`
- **Type**: Character string
- **Description**: Export file format
- **Options**:
  - `"csv"`: Comma-separated values
  - `"xlsx"`: Excel workbook
  - `"json"`: JSON format
  - `"txt"`: Tab-delimited text
  - `"rds"`: R data format
  - `"research_standard"`: Standardized research format
- **Default**: Determined by file extension

### File Path
- **Variable**: `file_path`
- **Type**: Character string
- **Description**: Output file path
- **Required**: Yes
- **Example**: `"results/sap_flow_analysis.xlsx"`

### Export Content Options

#### Include Metadata
- **Variable**: `include_metadata`
- **Type**: Logical
- **Description**: Include metadata in export
- **Default**: `TRUE`

#### Include Diagnostics
- **Variable**: `include_diagnostics`
- **Type**: Logical
- **Description**: Include diagnostic data
- **Default**: `TRUE`

#### Include Quality Flags
- **Variable**: `include_quality_flags`
- **Type**: Logical
- **Description**: Include quality flags
- **Default**: `TRUE`

### File Options

#### Compression
- **Variable**: `compression`
- **Type**: Logical
- **Description**: Compress output files
- **Default**: `FALSE`

#### Overwrite
- **Variable**: `overwrite`
- **Type**: Logical
- **Description**: Overwrite existing files
- **Default**: `FALSE`

### Filtering Options
- **Variable**: `filter_options`
- **Type**: List
- **Description**: Filtering options for export
- **Options**:
  - `methods`: Vector of methods to include
  - `quality_flags`: Vector of quality flags to include
  - `date_range`: Vector of start/end dates
  - `sensor_positions`: Vector of sensor positions
- **Example**: `list(methods = c("HRM", "MHR"), quality_flags = "OK")`

---

## Processing Options

### Scaling Methods
- **Variable**: `scaling_method`
- **Type**: Character string
- **Description**: Method for scaling flux density to tree level
- **Options**:
  - `"outer_only"`: Use only outer sensor measurements
  - `"inner_only"`: Use only inner sensor measurements
  - `"weighted_average"`: Weighted average based on area coverage
  - `"radial_profile"`: Linear radial profile between sensors
- **Default**: `"outer_only"`

### Flux Units
- **Variable**: `flux_units`
- **Type**: Character string
- **Description**: Output units for tree flux
- **Options**:
  - `"L_hr"`: Liters per hour
  - `"L_day"`: Liters per day
  - `"kg_hr"`: Kilograms per hour
  - `"kg_day"`: Kilograms per day
- **Default**: `"L_hr"`

### Water Density
- **Variable**: `water_density`
- **Type**: Numeric
- **Description**: Water density for mass calculations
- **Units**: kg/L
- **Default**: 1.0 kg/L
- **Range**: 0.95 - 1.05 kg/L

### Processing Control

#### Quality Control
- **Variable**: `quality_control`
- **Type**: Logical
- **Description**: Apply quality control
- **Default**: `TRUE`

#### Verbose Output
- **Variable**: `verbose`
- **Type**: Logical
- **Description**: Print processing information
- **Default**: `TRUE`

#### Plot Results
- **Variable**: `plot_results`
- **Type**: Logical
- **Description**: Generate diagnostic plots
- **Default**: `FALSE`

---

## Example Input Configurations

### Basic Configuration
```r
# Data input
file_path <- "data/sap_flow_2024.txt"

# Wood properties (Eucalyptus)
wood_properties <- list(
  fresh_density = 0.8,
  dry_density = 0.55,
  moisture_content = 0.45,
  specific_heat_capacity = 1.5
)

# Calculation parameters
parameters <- list(
  diffusivity = 0.0025,
  x = 0.5,
  pre_pulse = 30,
  HRM_start = 60,
  HRM_end = 100
)

# Tree measurements
tree_measurements <- list(
  diameter_breast_height = 18.2,
  bark_thickness = 0.8,
  heartwood_diameter = 8.0
)
```

### Advanced Configuration
```r
# Multiple methods with custom parameters
methods <- c("HRM", "MHR", "DMA", "Tmax_Klu")

# Extended parameters
parameters <- list(
  diffusivity = 0.003,  # Species-specific
  x = 0.6,             # Custom probe spacing
  L = 0.4,             # HRMX lower bound
  H = 0.9,             # HRMX upper bound
  tp_1 = 3,            # Extended pulse duration
  pre_pulse = 25,      # Shorter pre-pulse
  HRM_start = 70,      # Later HRM start
  HRM_end = 120        # Extended HRM window
)

# Quality control
quality_control <- list(
  temperature_range = c(-5, 50),
  voltage_range = c(5, 25),
  strict_validation = TRUE,
  velocity_range = c(-20, 150)
)

# Export options
export_options <- list(
  format = "xlsx",
  include_metadata = TRUE,
  include_diagnostics = TRUE,
  compression = FALSE,
  filter_options = list(
    methods = c("HRM", "DMA"),
    quality_flags = "OK"
  )
)
```

---

## Input Validation Summary

### Required Inputs
1. **Data file path** - Primary sap flow data
2. **DBH** - Tree diameter at breast height
3. **Wood properties** - For flux density conversion (or use defaults)

### Recommended Inputs
1. **Species-specific wood properties** - For accurate results
2. **Probe configuration** - For method compatibility
3. **Custom parameters** - For specific sensor setups

### Optional Inputs
1. **Tree measurements** - Bark thickness, heartwood dimensions
2. **Quality control ranges** - Custom validation thresholds
3. **Export options** - Format and content preferences

### Default Values
The package provides sensible defaults for most parameters, but users should provide species-specific values for wood properties and thermal diffusivity for best results.

