# sapFluxR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

**sapFluxR** is a comprehensive R package for importing, processing, and analyzing sap flow data from ICT SFM1x sensors. It provides robust tools for handling multiple data formats, automatic format detection, data validation, and calculating heat pulse velocities using various established methods.

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
devtools::install_github("neez777/sapFluxR")
```

Once available on CRAN:

```r
install.packages("sapFluxR")
```

## Quick Start

```r
library(sapFluxR)

# Import sap flow data (automatic format detection)
sap_data <- read_sap_data("path/to/your/data.txt")

# Calculate heat pulse velocities using multiple methods
vh_results <- calc_heat_pulse_velocity(
  sap_data,
  methods = c("HRM", "MHR", "DMA")
)

# View results
print(vh_results)

# Calculate summary statistics
stats <- calc_velocity_stats(vh_results, group_by = "method")
print(stats)

# Export results
export_velocity_results(vh_results, "sap_flow_velocities.csv")
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
Tab-delimited or fixed-width formats from older ICT sensors (format-specific handling available).

## Heat Pulse Velocity Methods

| Method | Description | Best For | Reference |
|--------|-------------|----------|-----------|
| **HRM** | Heat Ratio Method | Low flows, reverse flows | Burgess et al. (2001) |
| **MHR** | Maximum Heat Ratio | Moderate flows | Lopez et al. (2021) |
| **HRMXa/b** | Modified HRM variants | Enhanced HRM accuracy | Burgess & Bleby (unpublished) |
| **Tmax_Coh** | T-max Cohen method | High flows | Cohen et al. (1981) |
| **Tmax_Klu** | T-max Kluitenberg method | High flows (adjusted) | Kluitenberg & Ham (2004) |
| **DMA** | Dual Method Approach | Full range (automatic switching) | Forster (2020) |

## Configuration System

sapFluxR uses a flexible YAML-based configuration system to manage:

1. **Probe Configuration** - Hardware specifications (sensor spacing, geometry, heat pulse duration)
2. **Wood Properties** - Species-specific thermal and physical properties
3. **Calculation Parameters** - Analysis settings and time windows

### Quick Configuration Examples

```r
# Use defaults (symmetric probe + generic softwood)
results <- calc_heat_pulse_velocity(sap_data)

# Specify species
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus")

# Use asymmetric probe for CHPM method
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "asymmetrical",
                                    methods = "CHPM")

# Create custom configuration
custom_wood <- create_custom_wood_properties(
  species = "Quercus robur",
  thermal_diffusivity = 0.0018,
  dbh = 42.5,
  sapwood_depth = 4.2
)

results <- calc_heat_pulse_velocity(sap_data, wood_properties = custom_wood)
```

### Built-in Configurations

**Probe Configurations:**
- `symmetrical` (default) - ICT SFM1x standard (5mm spacing)
- `asymmetrical` - CHPM optimized (5mm/10mm spacing)

**Wood Properties:**
- `generic_sw` (default) - Generic softwood
- `eucalyptus` - Eucalyptus spp.
- `pine` - Pinus spp.

### Learn More

- **📖 [Configuration Guide](CONFIGURATION_GUIDE.md)** - Complete guide to the configuration system
- **🔄 [Migration Guide](MIGRATION_GUIDE.md)** - Upgrading from previous versions

## Workflow Example

```r
library(sapFluxR)

# 1. Import and validate data
sap_data <- read_sap_data("SFM1x_data.txt", validate_data = TRUE)

# Check validation results
if (!sap_data$validation$valid) {
  print(sap_data$validation$issues)
}

# 2. Set custom calculation parameters
params <- list(
  diffusivity = 0.0025,  # Thermal diffusivity (cm²/s)
  x = 0.5,               # Probe spacing (cm)
  pre_pulse = 30,        # Pre-pulse period (sec)
  HRM_start = 60,        # HRM analysis window start (sec)
  HRM_end = 100          # HRM analysis window end (sec)
)

# 3. Calculate velocities
vh_results <- calc_heat_pulse_velocity(
  sap_data,
  methods = c("HRM", "MHR", "DMA"),
  parameters = params
)

# 4. Quality control and filtering
clean_results <- filter_velocity_results(
  vh_results,
  quality_flags = "OK",
  velocity_range = c(-10, 200)
)

# 5. Analysis and visualization
stats_by_method <- calc_velocity_stats(clean_results, group_by = "method")
plot_velocity_diagnostics(clean_results, "methods_comparison")

# 6. Export results
export_velocity_results(
  clean_results,
  "processed_velocities.csv",
  include_quality_summary = TRUE
)
```

## Advanced Features

### Custom Parameters

```r
# Species-specific thermal diffusivity values
eucalyptus_params <- list(diffusivity = 0.0025, x = 0.5)
pine_params <- list(diffusivity = 0.0030, x = 0.6)

# Method-specific parameters
custom_params <- list(
  diffusivity = 0.0025,
  x = 0.5,
  L = 0.4,        # Lower HRMX window bound
  H = 0.9,        # Upper HRMX window bound
  tp_1 = 3,       # Heat pulse duration for T-max
  pre_pulse = 25  # Pre-pulse period
)
```

### Batch Processing

```r
# Process multiple files
file_list <- list.files("data/", pattern = "\\.txt$", full.names = TRUE)

results_list <- lapply(file_list, function(file) {
  sap_data <- read_sap_data(file)
  calc_heat_pulse_velocity(sap_data, methods = c("HRM", "DMA"))
})

# Combine all results
all_results <- do.call(rbind, results_list)
```

### Quality Assessment

```r
# Comprehensive data validation
validation_result <- validate_sap_data(
  sap_data,
  temperature_range = c(-10, 60),
  voltage_range = c(0, 30),
  strict_validation = TRUE
)

# Diagnostic plots
plot_velocity_diagnostics(vh_results, "quality_flags")
plot_velocity_diagnostics(vh_results, "time_series")
plot_velocity_diagnostics(vh_results, "histogram")
```

## Data Structure

The package uses a standardized data structure:

```r
# sap_data object structure
str(sap_data)
# List of 4
#  $ diagnostics: tibble [n × 7] (pulse_id, datetime, batt_volt, batt_current,
#                                 batt_temp, external_volt, external_current)
#  $ measurements: tibble [m × 6] (pulse_id, datetime, do, di, uo, ui)
#  $ metadata    : List of 5 (file_path, format, import_time, file_size, n_pulses)
#  $ validation  : List of 4 (valid, issues, warnings, summary)

# vh_results object structure
str(vh_results)
# tibble [p × 6] (datetime, pulse_id, method, sensor_position, Vh_cm_hr, quality_flag)
```

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Setup

```bash
# Clone repository
git clone https://github.com/neez777/sapFluxR.git
cd sapFluxR

# Install development dependencies
R -e "install.packages(c('devtools', 'testthat', 'roxygen2'))"

# Install package in development mode
R -e "devtools::install()"

# Run tests
R -e "devtools::test()"

# Check package
R -e "devtools::check()"
```

## Getting Help

- **Documentation**: Run `?function_name` in R for function help
- **Vignettes**: `vignette("getting-started", package = "sapFluxR")`
- **Issues**: [GitHub Issues](https://github.com/neez777/sapFluxR/issues)
- **Questions**: [GitHub Discussions](https://github.com/neez777/sapFluxR/discussions)

## Citation

If you use sapFluxR in your research, please cite:

```
Joyce, G. W. (2025). sapFluxR: An R Package for Processing ICT SFM1x Sap Flow Data.
R package version 0.2.0. https://github.com/neez777/sapFluxR
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Based on original R code by Gavan McGrath and Tim Bleby
- ICT International for SFM1x sensor technology
- Contributors to heat pulse velocity calculation methods

---

**Authors**: Grant Joyce
**Maintainer**: Grant Joyce, neez1977@gmail.com
**License**: GPL-3
**Version**: 0.2.0