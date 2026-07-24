# sapfluxr  <img src="man/figures/sapfluxr_logo.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analysing sap flow data from ICT SFM1x sensors. It provides a robust, non-destructive, and transparent pipeline from raw sensor measurements to tree-level water use estimates.

## The Processing Pipeline

`sapfluxr` implements a physically grounded pipeline, mirrored by the companion `shiny-sapfluxr` application, designed for scientific rigour and reproducibility:

1. **Data Import**: Automated multi-format detection (JSON, CSV, legacy), validation, optional clock-drift and weather/VPD import.
2. **Configuration**: Probe geometry and wood thermal properties (built-in, custom YAML, or derived from core measurements).
3. **HPV Calculation & QC**: Multi-method engine (HRM, MHR, T-max Cohen & Kluitenberg) with quality flagging.
4. **Spacing Correction**: Zero-flow identification (PELT, dual-stable, VPD) with segment/gradient × Burgess/linear correction.
5. **Wound Correction**: Linear or polynomial scaling with temporal wound tracking.
6. **Calibration & sDMA**: Aligning secondary methods to the corrected HRM scale, then Péclet-based switching (Pe computed at this stage from corrected velocities).
7. **Flux Density & Integration**: $J_v = Z \cdot V_h$ and two-annulus radial integration to tree water use.
8. **Aggregation & Visualisation**: Daily totals, normalised metrics, and diagnostic plots.

## Quick Start Example

```r
library(sapfluxr)

# 1. Import raw data
hp_data <- read_heat_pulse_data("data.txt")

# 2. Configure probe and wood properties
probe <- load_probe_config("symmetrical")
wood  <- load_wood_properties("eucalyptus")

# 3. Calculate velocities and flag quality
vh <- calc_heat_pulse_velocity(hp_data, methods = c("HRM", "MHR"),
                               probe_config = probe, wood_properties = wood)
vh <- flag_vh_quality(vh)

# 4. Spacing correction (PELT anchors + Burgess)
anchors <- detect_changepoints(vh, sensor_position = "outer")
vh <- apply_spacing_correction(vh, changepoints = anchors$changepoints,
                               offset_model = "segment", correction_math = "burgess",
                               sensor_position = "both", wood_properties = wood)

# 5. Wound correction
vh <- apply_wound_correction(vh, probe_spacing = "5mm", method = "linear",
                             wood_properties = wood)

# 6. Calibrate secondary methods and switch (sDMA — Pe auto-computed)
calibs <- calibrate_multiple_methods(vh, primary_method = "HRM", secondary_methods = "MHR")
vh <- transform_multiple_methods(vh, calibs)
vh <- apply_sdma_processing(vh, secondary_method = "MHR",
                            probe_config = probe, wood_properties = wood,
                            peclet_threshold = 1.0)

# 7. Flux density and tree water use
flux <- vh
flux$Jv_cm3_cm2_hr <- calc_sap_flux_density(Vh = flux$Vs_cm_hr, wood_properties = wood)
flux$dbh <- 35; flux$sapwood_thickness <- 2; flux$bark_thickness_dbh <- 1.2; flux$bark_thickness_probe <- 0.5
q <- apply_sap_flux_integration(flux, method = "linear_decay")

# 8. Aggregate to daily totals
daily <- aggregate_daily(q)
```

## Documentation

Start with the **Get Started** guide for an end-to-end tour, then dive into the per-stage vignettes:

* [**Get Started with sapfluxr**](vignettes/sapfluxr.Rmd): The full pipeline from start to finish.
* [**1. Data Import**](vignettes/vignette-01-data-import.Rmd): Formats, validation, clock drift, weather/VPD.
* [**2. Probe & Wood Configuration**](vignettes/vignette-02-configuration.Rmd): Geometry and thermal properties.
* [**3. HPV Calculation & QC**](vignettes/vignette-03-hpv-calculation.Rmd): HRM, MHR, T-max, baseline methods, quality flags.
* [**4. Baseline & Spacing Correction**](vignettes/vignette-04-spacing-correction.Rmd): Zero-flow anchors and correction models.
* [**5. Wound Correction**](vignettes/vignette-05-wound-correction.Rmd): Linear/polynomial scaling and temporal tracking.
* [**6. Calibration & sDMA**](vignettes/vignette-06-calibration-and-sdma.Rmd): Method alignment and Péclet switching.
* [**7. Flux Density & Integration**](vignettes/vignette-07-flux-and-integration.Rmd): From velocity to tree water use.
* [**8. Aggregation & Visualisation**](vignettes/vignette-08-aggregation-and-visualisation.Rmd): Daily totals, metrics, plots.

## Key Features

* **Non-Destructive Architecture**: Original measurements are preserved; each correction creates a new tracked column.
* **Physics-Led Switching**: Automatically selects the most accurate method based on the **Péclet Number (Pe)**.
* **Optimised Performance**: Core calculation engines implemented in C++ for handling large, multi-year datasets.
* **Reproducible Design**: All parameters and correction histories are tracked in metadata attributes.

## Installation

### Prerequisites

`sapfluxr` uses compiled C++ code (Rcpp). Before installing, make sure you have a working C++ toolchain:

| Platform | Requirement |
|---|---|
| **Windows** | [Rtools](https://cran.r-project.org/bin/windows/Rtools/) — install the version matching your R version |
| **macOS** | Xcode Command Line Tools: `xcode-select --install` |
| **Linux** | `gcc` / `g++` — usually already present; install via your package manager if not |

### Install sapfluxr

Once on CRAN:

```r
install.packages("sapfluxr")
```

Or install the development version from GitHub:

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("neez777/sapfluxr")
```

### Companion Shiny application

The interactive `shiny-sapfluxr` app mirrors the full package pipeline. To run it locally:

```r
# 1. Install sapfluxr (above), then install the additional Shiny dependencies
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "fresh", "plotly", "DT", "waiter", "leaflet", "webshot2"
))

# 2. Clone the companion repository and run
# git clone https://github.com/neez777/sapfluxr
shiny::runApp("path/to/shiny-sapfluxr")
```

## License

This project is licensed under the GPL-3 License.

---

**Authors**: Grant Joyce, Gavan McGrath, Tim Bleby
**Maintainer**: Grant Joyce, <neez1977@gmail.com>
**Version**: 0.9.0
