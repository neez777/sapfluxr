# sapfluxr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

**sapfluxr** is a comprehensive R package for importing, processing, and analyzing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

## Key Features

- **Multi-format Import**: Supports current JSON-like format and legacy formats from ICT SFM1x sensors
- **Automatic Format Detection**: Intelligently detects data format without user specification
- **Comprehensive Validation**: Built-in data quality checks and validation
- **Multiple Calculation Methods**: Implements 7+ heat pulse velocity calculation methods:
  - Heat Ratio Method (HRM)
  - Maximum Heat Ratio (MHR)
  - Modified HRM variants (HRMXa, HRMXb)
  - T-max methods (Cohen, Kluitenberg)
  - Dual Method Approach (DMA)
- **Quality Control**: Automatic quality flagging and diagnostic tools
- **Flexible Analysis**: Extensive utilities for filtering, summarizing, and exporting results

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

| Method | Description | Best For | Reference |
|--------|-------------|----------|-----------|
| **HRM** | Heat Ratio Method | Low flows, reverse flows | Burgess et al. (2001) |
| **MHR** | Maximum Heat Ratio | Moderate flows | Lopez et al. (2021) |
| **HRMXa/b** | Modified HRM variants | Enhanced HRM accuracy | Burgess & Bleby (unpublished) |
| **Tmax_Coh** | T-max Cohen method | High flows | Cohen et al. (1981) |
| **Tmax_Klu** | T-max Kluitenberg method | High flows (adjusted) | Kluitenberg & Ham (2004) |
| **DMA** | Dual Method Approach | Full range (automatic switching) | Forster (2020) |

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
