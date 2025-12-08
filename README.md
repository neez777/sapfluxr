# sapfluxr  <img src="man/figures/sapfluxr_logo.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analysing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

## Key Features

### Data Import & Preprocessing
- **Multi-format Import**: Supports current JSON-like format and legacy formats from ICT SFM1x sensors
- **Automatic Format Detection**: Intelligently detects data format without user specification
- **Clock Drift Correction**: Fix device clock drift with linear interpolation from a single calibration point
- **Comprehensive Validation**: Built-in data quality checks and validation

### Heat Pulse Velocity Calculations
- **Multiple Calculation Methods**: Implements 6 core heat pulse velocity calculation methods:
  - Heat Ratio Method (HRM) - includes Peclet number calculation
  - Maximum Heat Ratio (MHR)
  - Modified HRM variants (HRMXa, HRMXb)
  - T-max methods (Cohen, Kluitenberg)

### Zero-Flow Correction
- **Simple Linear Offset** (Universal)
  - Empirical correction using identified zero-flow periods
  - Works for all methods (HRM, MHR, Tmax, etc.)
  - Subtracts mean offset observed during zero-flow
- **Burgess Spacing Correction** (HRM/HRMX Only)
  - Physics-based correction using Burgess et al. (2001)
  - **Novel Heartwood Reference Method** ✨ (NEW in v0.4.0)
    - Uses inner sensor in heartwood as continuous zero reference
    - Per-measurement correction, real-time drift tracking
    - Automatic geometry checking
  - **Changepoint-Based Correction**
    - Detects baseline shifts using PELT algorithm
    - Segment-specific corrections for long deployments
  - **Manual Specification**
    - User-defined changepoints with optional overrides
    - Integrates field knowledge and cut-stem calibration

### Post-Processing & Quality Control
- **Selectable DMA (sDMA)**: Apply automatic method switching after calculation
  - Choose your own secondary method for high-flow conditions
  - Efficient: calculate base methods once, apply multiple sDMA variants
- **Two-Tier Quality Flagging**: Comprehensive quality control system
  - **CALC_ flags**: Calculation quality (FAILED, INFINITE, EXTREME)
  - **DATA_ flags**: Data quality (MISSING, ILLOGICAL, OUTLIER, SUSPECT)
  - Detects missing pulses, outliers, rate-of-change issues, cross-sensor anomalies
- **Flexible Analysis**: Extensive utilities for filtering, summarising, and exporting results
- **Advanced Visualisation**: Specialised plotting functions including dual-axis sDMA plots

## Installation

Install the development version from GitHub:

```r
# Install from GitHub
if (!require(devtools)) install.packages("devtools")
devtools::install_github("neez777/sapfluxr")
```

Once available on CRAN:

```r
install.packages("sapfluxr")
```

## Supported Data Formats

### ICT Current Format (JSON-like)
```
[{"date":"2024-11-15T10:00:00Z","bv":4.11,"bc":200.00,"bt":30.68,"ep":1,"ev":22.76,"ec":85.80,
{"do":18.781,"di":18.588,"uo":18.818,"ui":18.652},
{"do":18.781,"di":18.588,"uo":18.817,"ui":18.652}}]
```

### CSV Format
```
datetime,pulse_id,do,di,uo,ui,batt_volt,batt_current
2024-11-15 10:00:00,1,18.781,18.588,18.818,18.652,4.11,200.0
2024-11-15 10:00:01,1,18.781,18.588,18.817,18.652,4.11,200.0
```

### Legacy Format
```
{"Rdg":[{"k":"date","v":"2024-03-15T12:30:01Z"},{"k":"bv","v":    4.03 },
{"k":"bc","v":   15.00 },{"k":"bt","v":   27.06},{"k":"ep","v":1},{"k":"ev","v":   20.69},
{"k":"ec","v":    2.10}],{"N":01,"T":[{"k":"do","v":  20.671},{"k":"di","v":  20.341}
```

## Heat Pulse Velocity Methods

### Calculation Methods

| Method | Description | Best For | Reference |
|--------|-------------|----------|-----------|
| **HRM** | Heat Ratio Method | Low flows, reverse flows | Burgess et al. (2001) |
| **MHR** | Maximum Heat Ratio | Moderate flows | Lopez et al. (2021) |
| **HRMXa/b** | Modified HRM variants | Enhanced HRM accuracy | Burgess & Bleby (unpublished) |
| **Tmax_Coh** | T-max Cohen method | High flows | Cohen et al. (1981) |
| **Tmax_Klu** | T-max Kluitenberg method | High flows (adjusted) | Kluitenberg & Ham (2004) |

### Post-Processing: Dual Method Approach

DMA (Dual Method Approach) is now applied as a post-processing step using `apply_sdma_processing()`, allowing flexible method selection after calculation.

### Selectable DMA (sDMA)

**New efficient workflow:** Calculate base methods once, then apply sDMA as post-processing.

```r
# Step 1: Calculate base methods (each calculated once)
vh <- calc_heat_pulse_velocity(data, methods = c("HRM", "MHR", "Tmax_Klu"))

# Step 2: Apply sDMA switching with your chosen secondary method
vh_sdma <- apply_sdma_processing(vh, secondary_method = "MHR")

# Now have: HRM, MHR, Tmax_Klu, sDMA:MHR (all in one tibble)

# Or test multiple secondary methods at once
vh_multi <- apply_sdma_processing(vh, secondary_method = c("MHR", "Tmax_Klu"))

# Visualise with Peclet number
plot_sdma_timeseries(vh_sdma, sdma_method = "sDMA:MHR")
```

**Available secondary methods**: MHR, Tmax_Coh, Tmax_Klu, HRMXa, HRMXb

**Key features**:
- Automatic switching at Pe = 1.0 threshold
- Returns Peclet number for each measurement
- Shows which method was actually used via `selected_method` column
- Can be applied after corrections: `apply_hpv_corrections() %>% apply_sdma_processing()`
- Dedicated dual-axis plotting function

## Zero-Flow Correction Methods

Probe spacing errors due to misalignment, tree swelling, or wood movement cause systematic bias in velocity measurements. sapfluxr provides three correction approaches:

### 1. Heartwood Reference (Preferred when available) ✨

**Novel approach using probe geometry:** When the inner temperature sensor is positioned in the heartwood (non-conducting wood), it provides a **continuous zero-flow reference** because heartwood conducts no sap.

```r
# Check if heartwood reference is available
hw_check <- check_heartwood_reference_available(
  probe_config = list(length = 35, inner_sensor = 7.5),  # Standard ICT probe
  sapwood_depth = 2.0,        # YOUR tree's sapwood depth (cm)
  bark_thickness = 0.3        # Bark thickness (cm)
)

print(hw_check)  # Shows if AVAILABLE or NOT AVAILABLE

# If available, apply continuous correction
if (hw_check$available) {
  correction <- apply_heartwood_reference_correction(
    vh_data = vh_flagged,
    method = "HRM"
  )
  vh_corrected <- correction$vh_corrected

  # Check offset statistics
  print(correction$offset_summary)
}
```

**Advantages:**
- **Continuous correction**: Per-measurement adjustment (not segment-based)
- **Real-time drift tracking**: Captures probe alignment changes continuously
- **Physics-based**: No assumption about zero-flow periods needed

**Requirements:**
- Inner sensor must be positioned in heartwood (checked automatically)
- Sapwood depth must be known

### 2. Changepoint Detection

**Segment-based correction:** Detects baseline shifts and applies correction per segment.

```r
# Calculate daily minima
daily_min <- calculate_daily_minima(vh_flagged, sensor_position = "outer")

# Detect changepoints
cpt_result <- detect_changepoints(daily_min, penalty = "MBIC")

# Apply correction to both sensors
correction <- apply_spacing_correction_both_sensors(
  vh_data = vh_flagged,
  changepoints = cpt_result$changepoints,
  method = "HRM"
)
```

### 3. Manual Specification

**User-controlled:** Specify changepoints based on field knowledge.

```r
correction <- apply_spacing_correction_both_sensors(
  vh_data = vh_flagged,
  changepoints = c("2024-03-15", "2024-06-10"),
  method = "HRM"
)
```

**See the quickstart vignette for detailed workflows:** `vignette("quickstart", package = "sapfluxr")`

## Wood Properties Configuration

sapfluxr uses a flexible wood properties system that supports two input methods for calculating derived thermal properties and conversion factors.

### Built-in Species Configurations

Three configurations are included:
- **generic_sw** (default): Generic temperate softwood
- **eucalyptus**: Eucalyptus species properties
- **pine**: Pinus species properties

### Two Input Methods

Both methods calculate **all derived properties** including thermal diffusivity correction factor (Y) and sap flux conversion factor (Z).

**Method 1: Weight & Volume Measurements**

Direct laboratory measurements of fresh weight, dry weight, and fresh volume.

```r
# Load configuration
wood <- load_wood_properties("generic_sw")

# Set measurements from laboratory analysis
wood$wood_measurements$fresh_weight_g <- 0.5493
wood$wood_measurements$dry_weight_g <- 0.2641
wood$wood_measurements$fresh_volume_cm3 <- 0.4506

# Calculate all derived properties
wood <- calculate_wood_properties(wood)

# View results
print(wood)
```

**Method 2: Dual Density (RECOMMENDED - Easier than Method 1)**

Measure both dry and fresh density on the same sample. Moisture content is back-calculated from the density ratio: `mc = (ρfw/ρdw) - 1`, allowing calculation of all thermal properties including the Z factor.

```r
# Load and configure
wood <- load_wood_properties("eucalyptus")

# Provide BOTH densities measured on the same sample
wood$wood_measurements$density_dry_kg_m3 <- 550     # Oven-dry density
wood$wood_measurements$density_fresh_kg_m3 <- 1100  # Fresh (field-moist) density

# Calculate ALL derived properties (including Z factor!)
wood <- calculate_wood_properties(wood)

# Check calculated moisture content and Z factor
print(wood$derived_properties$mc_kg_kg)                    # 1.0 kg/kg (100%)
print(wood$derived_properties$sap_flux_conversion_factor)  # Z factor
```

**Why Method 2 is recommended:**
- Easier than Method 1 (no fresh weight timing issues, simpler volume measurement)
- Still calculates Z factor (critical for Vh → Jv conversion)
- Only slightly more work than measuring single density
- Volume cancels out in calculations, so actual volume doesn't need to be known precisely

### Wood Properties Workflow Order

The complete workflow progresses through these stages:

1. **Spacing Correction** → Corrects probe spacing errors
2. **Recalculate Vh** → Re-run HPV calculations with updated thermal diffusivity (Y factor)
3. **Wound Correction** → Applies temporal wound expansion correction
4. **Flux Density Conversion** → Converts Vh to Jv using Z factor

```r
# 1. Calculate heat pulse velocity
vh_data <- calc_heat_pulse_velocity(heat_pulse_data, wood_properties = wood)

# 2. Apply spacing correction
vh_corrected <- apply_spacing_correction_both_sensors(vh_data, ...)

# 3. Apply wound correction with temporal tracking
wood$wound_correction$drill_bit_diameter_mm <- 2.0
wood$wound_correction$wound_addition_mm <- 0.3
wood$wound_correction$initial_date <- "2025-01-01"
wood$wound_correction$final_date <- "2025-12-31"
wood$wound_correction$final_diameter_mm <- 4.5

vh_wound_corrected <- apply_wound_correction(
  vh_corrected,
  wood_properties = wood,
  probe_spacing = "5mm"
)

# 4. Convert to sap flux density (Jv)
flux_data <- apply_flux_conversion(
  vh_wound_corrected,
  wood_properties = wood,
  velocity_col = "Vc_cm_hr"
)
```

### Wound Correction Configuration

Wound correction accounts for wound tissue formation around the heater probe. The initial wound size is calculated as:

```
initial_wound = drill_bit_diameter + (2 × wound_addition)
```

Default values:
- `drill_bit_diameter_mm`: 2.0 mm
- `wound_addition_mm`: 0.3 mm per side

This gives an initial wound of 2.6 mm (0.26 cm).

**Temporal wound tracking** (optional) models linear wound expansion over time:

```r
# Configure temporal wound tracking
wood$wound_correction$initial_date <- "2025-01-01"      # Installation date
wood$wound_correction$final_date <- "2025-12-31"        # Final measurement date
wood$wound_correction$final_diameter_mm <- 4.5          # Measured final wound size
```

### Sap Flux Conversion (Z Factor)

The Z factor converts heat pulse velocity (Vh) to sap flux density (Jv):

```
Jv = Z × Vh
```

Where Z accounts for the heat capacity of the wood matrix using the formula from Burgess et al. (2001):

```
Z = (ρdw/ρs) × ((cdw + mc × cs) / cs)
```

Both input methods (weight/volume and dual density) calculate the Z factor. Method 2 back-calculates moisture content from the density ratio, allowing complete property derivation.

### Custom Wood Properties

Create custom configurations for your specific tree:

```r
# Option 1: Override existing configuration
wood <- load_wood_properties(
  "eucalyptus",
  overrides = list(
    thermal_diffusivity_default_cm2_s = 0.003,
    rho_sap_kg_m3 = 1010
  ),
  tree_overrides = list(
    dbh = 45.2,
    sapwood_depth = 3.5,
    sapwood_area = 125.6
  )
)

# Option 2: Create from scratch
custom_wood <- create_custom_wood_properties(
  config_name = "My Custom Tree",
  species = "Pinus radiata",
  thermal_diffusivity = 0.0028,
  dry_density = 450,
  moisture_content = 35,
  dbh = 45.2
)
```

## Complete Workflow

```r
library(sapfluxr)

# 1. Import data (auto-detects format)
heat_pulse_data <- read_heat_pulse_data("mydata.txt")

# 1a. (Optional) Fix clock drift if device clock was inaccurate
heat_pulse_data <- fix_clock_drift(
  data = heat_pulse_data,
  observed_device_time = as.POSIXct("2025-01-16 08:05:00"),
  observed_actual_time = as.POSIXct("2025-01-16 08:00:00")
)

# 1b. Configure wood properties
wood <- load_wood_properties("eucalyptus")

# Add wood measurements (Method 2: Dual Density - RECOMMENDED)
wood$wood_measurements$density_dry_kg_m3 <- 550
wood$wood_measurements$density_fresh_kg_m3 <- 1100

# Calculate derived properties (thermal diffusivity, Z factor, etc.)
wood <- calculate_wood_properties(wood)

# Alternative: Method 1 (Weight & Volume)
# wood$wood_measurements$fresh_weight_g <- 0.5493
# wood$wood_measurements$dry_weight_g <- 0.2641
# wood$wood_measurements$fresh_volume_cm3 <- 0.4506
# wood <- calculate_wood_properties(wood)

# 2. Calculate heat pulse velocities
vh_results <- calc_heat_pulse_velocity(
  heat_pulse_data,
  methods = c("HRM", "MHR", "HRMXa", "Tmax_Klu"),
  wood_properties = wood
)

# 3. Apply comprehensive quality control
qc_results <- flag_vh_quality(
  vh_results,
  wood_properties = "eucalyptus",
  detect_missing_pulses = TRUE,
  detect_outliers = TRUE,
  detect_rate_of_change = TRUE
)
vh_flagged <- qc_results$vh_flagged

# Check quality flags
table(vh_flagged$quality_flag)

# 4. Zero-flow correction (NEW in v0.4.0: heartwood reference method)

# First, check if heartwood reference is available
hw_check <- check_heartwood_reference_available(
  probe_config = list(length = 35, inner_sensor = 7.5),
  sapwood_depth = 2.0,     # Your tree's sapwood depth (cm)
  bark_thickness = 0.3     # Bark thickness (cm)
)

if (hw_check$available) {
  # Preferred: Heartwood reference (continuous correction)
  correction <- apply_heartwood_reference_correction(
    vh_data = vh_flagged,
    method = "HRM"
  )
  vh_corrected <- correction$vh_corrected
  print(correction$offset_summary)  # View offset statistics

} else {
  # Alternative: Changepoint-based correction
  daily_min <- calculate_daily_minima(vh_flagged, sensor_position = "outer")
  cpt_result <- detect_changepoints(daily_min, penalty = "MBIC")

  correction <- apply_spacing_correction_both_sensors(
    vh_data = vh_flagged,
    changepoints = cpt_result$changepoints,
    method = "HRM"
  )
  vh_corrected <- correction$vh_corrected
}

# 5. (Optional) Apply wound correction with temporal tracking
wood$wound_correction$drill_bit_diameter_mm <- 2.0
wood$wound_correction$wound_addition_mm <- 0.3
wood$wound_correction$initial_date <- "2025-01-01"
wood$wound_correction$final_date <- "2025-12-31"
wood$wound_correction$final_diameter_mm <- 4.5

vh_wound_corrected <- apply_wound_correction(
  vh_corrected,
  wood_properties = wood,
  probe_spacing = "5mm"
)

# 6. Convert to sap flux density (Jv)
flux_data <- apply_flux_conversion(
  vh_wound_corrected,
  wood_properties = wood,
  velocity_col = "Vc_cm_hr"  # Use wound-corrected velocity
)

# 7. (Optional) Apply sDMA post-processing
vh_sdma <- apply_sdma_processing(flux_data, secondary_method = "MHR")

# 8. Visualise results
plot_vh_timeseries(flux_data, methods = c("HRM", "MHR", "HRMXa"))
plot_heat_pulse_trace(heat_pulse_data, flux_data, pulse_id = 1)
plot_sdma_timeseries(vh_sdma, sdma_method = "sDMA:MHR")

# 9. Export clean data only
clean_data <- flux_data[flux_data$quality_flag == "OK", ]
write.csv(clean_data, "sap_flux_density_clean.csv", row.names = FALSE)
```

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Based on original R code by Gavan McGrath and Tim Bleby
- ICT International for SFM1x sensor technology
- Contributors to heat pulse velocity calculation methods

---

**Authors**: Grant Joyce
**Maintainer**: Grant Joyce, neez1977@gmail.com
**License**: GPL-3
**Version**: 0.4.0
