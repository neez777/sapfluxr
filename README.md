# sapfluxr  <img src="man/figures/sapfluxr_logo.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analysing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

## Modern sDMA Workflow ✨

`sapfluxr` now implements a **Selectable Dual Method Approach (sDMA)** that ensures data continuity and physical accuracy across all flow ranges.

**Key features**:
- **Correction Transfer**: Calibrates high-flow methods (MHR) against *corrected* low-flow data (HRM), transferring spacing and wound corrections mathematically.
- **Physics-Led Switching**: Automatically switches between methods based on the **Peclet Number (Pe)** threshold (typically Pe = 1.0).
- **Smooth Transitions**: Prevents "volcano" discontinuities (velocity drops at peak flow) by aligning method scales before switching.

```r
# 1. Calculate raw velocities
vh <- calc_heat_pulse_velocity(data, methods = c("HRM", "MHR"), wood_properties = "eucalyptus")

# 2. Apply corrections to HRM (Spacing + Wound)
vh_corrected <- vh %>% 
  apply_spacing_correction(method = "vpd", weather_data = weather) %>%
  apply_wound_correction(wood_properties = wood)

# 3. Apply sDMA: Calibrate MHR to corrected HRM and switch at Pe > 1.0
calib <- calibrate_method_to_primary(vh_corrected, primary="HRM", secondary="MHR")
vh_sdma <- apply_sdma_switching(vh_corrected, secondary="MHR", mode="peclet", calibration=calib)

# 4. Visualise results
plot_sdma_timeseries(vh_sdma, sdma_method = "sDMA:MHR")
```

## Key Features

### Data Import & Preprocessing
- **Multi-format Import**: Supports current JSON-like format and legacy formats from ICT SFM1x sensors.
- **Automatic Format Detection**: Intelligently detects data format without user specification.
- **Clock Drift Correction**: Fix device clock drift with linear interpolation.

### Heat Pulse Velocity Calculations
- **Multiple Methods**: HRM, MHR, HRMXa/b, T-max (Cohen, Kluitenberg).
- **Optimised Performance**: Core calculations implemented in C++ for 50-100x speedup.

### Zero-Flow Correction (Spacing)
- **Stable VPD Method**: ✨ (NEW) Uses pre-dawn environmental stability to detect baseline shifts automatically.
- **Heartwood Reference**: Uses inner sensor in heartwood as continuous zero reference.
- **Changepoint-Based**: Detects alignment shifts using the PELT algorithm.

### Wood Properties & Scaling
- **Dual Density Method**: Back-calculate moisture content from dry/fresh density (Recommended).
- **Temporal Wound Tracking**: Models linear wound expansion over the duration of deployment.
- **Radial Integration**: Weighted average integration (Hatton et al., 1990) for whole-tree scaling.

## Installation

Install the development version from GitHub:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("neez777/sapfluxr")
```

## License

This project is licensed under the GPL-3 License.

---

**Authors**: Grant Joyce  
**Maintainer**: Grant Joyce, neez1977@gmail.com  
**Version**: 0.5.0 (Experimental)