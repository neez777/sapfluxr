# sapfluxr  <img src="man/figures/sapfluxr_logo.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analyzing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

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

## Complete Workflow

```r
library(sapfluxr)

# 1. Import data (auto-detects format)
heat_pulse_data <- read_heat_pulse_data("mydata.txt")

# 1a. (Optional) Fix clock drift if device clock was inaccurate
heat_pulse_data <- fix_clock_drift(
  data = heat_pulse_data,
  observed_device_time = as.POSIXct("2025-01-16 08:05:00"),  # What device showed
  observed_actual_time = as.POSIXct("2025-01-16 08:00:00")   # Actual correct time
)

# 2. Calculate heat pulse velocities
vh_results <- calc_heat_pulse_velocity(
  heat_pulse_data,
  methods = c("HRM", "MHR", "HRMXa", "Tmax_Klu"),
  wood_properties = "eucalyptus"
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

# Check quality flags (two-tier system: CALC_ and DATA_ prefixes)
table(vh_flagged$quality_flag)

# View gap report
print(qc_results$gap_report)

# 4. (Optional) Apply sDMA post-processing
vh_sdma <- apply_sdma_processing(vh_flagged, secondary_method = "MHR")

# 5. Visualise results
plot_vh_timeseries(vh_flagged, methods = c("HRM", "MHR", "HRMXa"))
plot_heat_pulse_trace(heat_pulse_data, vh_flagged, pulse_id = 1)
plot_sdma_timeseries(vh_sdma, sdma_method = "sDMA:MHR")

# 6. Export clean data only
clean_data <- vh_flagged[vh_flagged$quality_flag == "OK", ]
write.csv(clean_data, "sap_velocity_clean.csv", row.names = FALSE)
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
**Version**: 0.2.0
