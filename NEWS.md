# sapFluxR 0.1.0

## Initial Release

This is the first release of sapFluxR, a comprehensive R package for processing sap flow data from ICT SFM1x sensors.

### New Features

* **Multi-format Data Import**: Support for ICT current format (JSON-like), CSV format, and legacy formats with automatic format detection
* **Comprehensive Data Validation**: Built-in quality checks for data integrity, temporal consistency, and sensor validation
* **Multiple Heat Pulse Velocity Methods**: Implementation of 7 calculation methods:
  - Heat Ratio Method (HRM) - Burgess et al. (2001)
  - Maximum Heat Ratio (MHR) - Lopez et al. (2021)
  - Modified HRM variants (HRMXa, HRMXb) - Burgess & Bleby (unpublished)
  - T-max Cohen method - Cohen et al. (1981)
  - T-max Kluitenberg method - Kluitenberg & Ham (2004)
  - Dual Method Approach (DMA) - Automatic method switching
* **Quality Control System**: Automatic quality flagging with categories for high velocity, negative flow, infinite values, and missing data
* **Flexible Analysis Tools**: Functions for filtering, summarizing, and statistical analysis of velocity results
* **Diagnostic Plotting**: Built-in diagnostic plots for method comparison, quality assessment, and time series visualization
* **Unit Conversion**: Support for multiple velocity units (cm/hr, mm/hr, m/day, etc.)
* **Export Capabilities**: CSV export with optional quality summaries
* **Comprehensive Documentation**: Detailed vignettes, function documentation, and examples

### Core Functions

* `read_sap_data()` - Import sap flow data with automatic format detection
* `detect_format()` - Automatic data format identification
* `validate_sap_data()` - Comprehensive data validation
* `calc_heat_pulse_velocity()` - Calculate velocities using multiple methods
* `filter_velocity_results()` - Quality-based filtering of results
* `calc_velocity_stats()` - Statistical summary functions
* `plot_velocity_diagnostics()` - Diagnostic plotting tools
* `export_velocity_results()` - Data export functionality

### Data Formats Supported

* **ICT Current Format**: JSON-like format from current ICT SFM1x sensors
* **CSV Format**: Standard comma-separated values with required columns
* **Legacy Format**: Tab-delimited and fixed-width formats from older sensors

### Package Infrastructure

* Comprehensive test suite with >95% code coverage
* Extensive documentation with examples and vignettes
* Sample data files for testing and examples
* Continuous integration setup
* MIT license for open-source use

### Dependencies

* **Required**: dplyr, stringr, lubridate, jsonlite, readr, tibble, rlang
* **Suggested**: testthat, knitr, rmarkdown, xts, dygraphs, htmltools

### Getting Started

```r
# Install development version
devtools::install_github("yourusername/sapFluxR")

# Basic workflow
library(sapFluxR)
sap_data <- read_sap_data("data.txt")
vh_results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR", "DMA"))
stats <- calc_velocity_stats(vh_results, group_by = "method")
export_velocity_results(vh_results, "results.csv")
```

### Acknowledgments

This package builds upon the foundational R code developed by Gavan McGrath and Tim Bleby for processing ICT SFM1x sap flow data. We thank the scientific community for developing and validating the heat pulse velocity calculation methods implemented in this package.