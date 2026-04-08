# sapfluxr  <img src="man/figures/sapfluxr_logo.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analysing sap flow data from ICT SFM1x sensors. It provides a robust, non-destructive, and transparent 10-step pipeline from raw sensor measurements to tree-level water use estimates.

## The 10-Step Pipeline

`sapfluxr` implements a physically grounded processing pipeline designed for scientific rigour and reproducibility:

1. **Data Import**: Automated multi-format detection (JSON, CSV, Legacy).
2. **Wood Property Estimation**: Fresh weight/volume or Dual-Density characterisation.
3. **HPV Calculation**: Multi-method engine (HRM, MHR, T-max Cohen & Kluitenberg).
4. **Zero-Flow Identification**: Manual, PELT (statistical), or Dual-Stable (physiological) detection.
5. **Baseline Correction**: Segmented Flat or Burgess Spacing models.
6. **Wound Correction**: Linear or polynomial scaling with temporal tracking.
7. **Method Calibration**: Aligning secondary methods to the corrected HRM scale.
8. **Dual Method Switching (sDMA)**: PÃ©clet-based switching for full diurnal coverage.
9. **Flux Density & Radial Integration**: Hatton (1990) two-annulus scaling.
10. **Daily Aggregation**: Integration to daily L/day with completeness tracking.

## Quick Start Example

```r
library(sapfluxr)

# 1. Import raw data
hp_data <- read_heat_pulse_data("data.txt")

# 2. Load wood properties (e.g., Eucalyptus)
wood <- load_wood_properties("eucalyptus")

# 3. Calculate velocities
vh <- calc_heat_pulse_velocity(hp_data, methods = c("HRM", "MHR"), wood_properties = wood)

# 4. Detect baseline shifts
vh <- detect_changepoints(vh, method = "pelt")

# 5. Apply zero-flow correction
vh <- apply_zero_flow_offset(vh, correction_model = "flat")

# 6. Apply wound correction
vh <- apply_wound_correction(vh, wound_diameter = 2.0)

# 7 & 8. Calibrate and switch (sDMA)
vh <- calibrate_secondary_method(vh, primary = "HRM", secondary = "MHR")
vh <- apply_sdma_switching(vh, primary = "HRM", secondary = "MHR", mode = "peclet")

# 9. Scale to tree-level flux (Q)
sap_flux <- calc_sap_flux(vh, wood_properties = wood, dbh = 35, sapwood_depth_mm = 40)

# 10. Aggregate to daily totals
daily <- aggregate_daily(sap_flux)
```

## Documentation

For detailed guides on each step of the pipeline, see the package vignettes:

* [**Quick Start Guide**](vignettes/sapfluxr-quickstart.Rmd): A high-level overview of the 10-step pipeline.
* [**01. Data Import & Configuration**](vignettes/vignette-01-import-and-config.Rmd): Getting data in and setting up physical properties.
* [**02. HPV Calculation Methods**](vignettes/vignette-02-hpv-calculation.Rmd): The physics of HRM, MHR, and T-max.
* [**03. Baseline & Zero-Flow Correction**](vignettes/vignette-03-baseline-and-zero-flow.Rmd): Addressing probe drift and misalignment.
* [**04. Advanced Corrections & Switching**](vignettes/vignette-04-wound-and-method-correction.Rmd): Wound correction, calibration, and sDMA.
* [**05. Flux Scaling & Daily Totals**](vignettes/vignette-05-flux-scaling-and-aggregation.Rmd): From velocity to L/day.

## Key Features

* **Non-Destructive Architecture**: Original measurements are preserved; each correction creates a new tracked column.
* **Physics-Led Switching**: Automatically selects the most accurate method based on the **PÃ©clet Number (Pe)**.
* **Optimised Performance**: Core calculation engines implemented in C++ for handling large, multi-year datasets.
* **Reproducible Design**: All parameters and correction histories are tracked in metadata attributes.

## Installation

Install the development version from GitHub:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("neez777/sapfluxr")
```

## License

This project is licensed under the GPL-3 License.

---

**Authors**: Grant Joyce, Gavan McGrath, Tim Bleby
**Maintainer**: Grant Joyce, <neez1977@gmail.com>
**Version**: 0.5.0 (Experimental)
