# sapfluxr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analyzing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

## Key Features

- **Multi-format Import**: Supports current JSON-like format and legacy formats from ICT SFM1x sensors
- **Automatic Format Detection**: Intelligently detects data format without user specification
- **Comprehensive Validation**: Built-in data quality checks and validation
- **Multiple Calculation Methods**: Implements 6 core heat pulse velocity calculation methods:
  - Heat Ratio Method (HRM) - includes Peclet number calculation
  - Maximum Heat Ratio (MHR)
  - Modified HRM variants (HRMXa, HRMXb)
  - T-max methods (Cohen, Kluitenberg)
- **Post-Processing Features**:
  - Selectable DMA (sDMA) - Apply automatic method switching after calculation
  - Choose your own secondary method for high-flow conditions
  - Efficient: calculate base methods once, apply multiple sDMA variants
- **Quality Control**: Automatic quality flagging and diagnostic tools
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
