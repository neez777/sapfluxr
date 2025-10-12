# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## ⚠️ CRITICAL: Pre-Development Protocol

### BEFORE Creating or Modifying ANY Function:

1. **MANDATORY SEARCH**: Search project for existing similar functions
2. **CHECK NAMING**: Verify function name follows established conventions
3. **VERIFY PLACEMENT**: Confirm function belongs in correct file
4. **AVOID DUPLICATION**: Check if functionality already exists

**See "Function Development Protocol" section below for complete guidelines.**

## Package Overview

**sapFluxR** is an R package for processing and analysing sap flow data from ICT SFM1x heat pulse velocity sensors. It provides a complete pipeline from raw temperature measurements to tree-level sap flux calculations, implementing 7+ heat pulse velocity (HPV) calculation methods.

## Development Commands

### Package Development
```r
# Install package in development mode
devtools::install()

# Run all tests
devtools::test()

# Run specific test file
devtools::test_active_file("tests/testthat/test-heat-pulse-velocity.R")

# Check package (comprehensive validation)
devtools::check()

# Build package documentation
devtools::document()

# Load package for interactive development
devtools::load_all()
```

### Testing
```r
# Run tests with coverage
covr::package_coverage()

# Run specific test
testthat::test_file("tests/testthat/test-data-import.R")

# Run tests matching pattern
testthat::test_local(filter = "hpv")
```

### Documentation
```r
# Preview vignettes
devtools::build_vignettes()

# View specific vignette
vignette("getting-started", package = "sapFluxR")
```

## Code Architecture

### Modular File Structure

The R source files follow a numbered organization system (01-14) that directly corresponds to the sap flow processing workflow stages:

**Stage 2: Data Import & Validation (01-02):**
- `01_data_import.R` - Multi-format data import with automatic format detection
- `02_data_validation.R` - Data quality checks and validation logic

**Stage 3: Probe Configuration (03):**
- `03_probe_configuration.R` - Probe setup, configuration detection, validation, and alignment analysis

**Stage 4: Heat Pulse Velocity Calculations (04-06):**
- `04_heat_pulse_velocity_core.R` - Core HPV methods (HRM, MHR) with preprocessing utilities
- `05_heat_pulse_velocity_advanced.R` - Advanced methods (HRMXa/b, Tmax variants, DMA, CHPM, DRM)
- `06_heat_pulse_velocity_yaml.R` - YAML-based method definition system

**Stage 5 & 10: Quality Control & Validation (07-09):**
- `07_method_comparison.R` - Cross-method comparison and validation
- `08_quality_control.R` - Sensor diagnostics and quality control
- `09_diagnostic_reporting.R` - Comprehensive diagnostic reports and templates

**Stage 6: Temporal Processing & Aggregation (10):**
- `10_temporal_processing.R` - Temporal aggregation and gap filling

**Stage 7-9: Flux Scaling & Tree-Level Calculations (11-13):**
- `11_flux_density.R` - Convert velocity to flux density (Vh → Js)
- `12_sapwood_area.R` - Sapwood area calculations
- `13_tree_water_use.R` - Tree-level water use scaling

**Unified Pipeline (14):**
- `14_processing_pipeline.R` - Main processing pipeline orchestrating all stages

**Supporting Files (not numbered - alphabetical):**
- `export_reporting.R` - Data export and reporting functions
- `multi_tree_utilities.R` - Multi-tree processing utilities
- `utilities.R` - General utility functions used across modules
- `visualisation_core.R` - Core plotting functions
- `visualisation_utilities.R` - Plotting helper functions
- `zzz.R` - Package initialization

### Data Flow Architecture

```
Raw Data → Import → Validation → Probe Config Detection → Quality Assessment
                                                                 ↓
Export ← Reporting ← Tree Scaling ← Flux Density ← HPV Calculation
```

**Key Data Structures:**

1. **sap_data object** (from `read_sap_data()`):
   - `diagnostics`: Battery voltage, current, temperature, external sensors
   - `measurements`: Temperature readings (do, di, uo, ui) per pulse
   - `metadata`: File info, format, import time, pulse count
   - `validation`: Quality checks, issues, warnings

2. **vh_results** (from `calc_heat_pulse_velocity()`):
   - Tibble with columns: datetime, pulse_id, method, sensor_position, Vh_cm_hr, quality_flag

3. **Multi-tree data**: List of sap_data objects from `read_multiple_sap_data()`

### HPV Calculation Methods

The package implements 7 distinct heat pulse velocity methods, each suited for different flow conditions:

- **HRM** (Heat Ratio Method) - Best for low/reverse flows, uses temperature ratios
- **MHR** (Maximum Heat Ratio) - Moderate flows, uses peak temperatures
- **HRMXa/HRMXb** - Modified HRM variants with different sampling windows
- **Tmax_Coh** (Cohen) - High flows, time-to-peak approach
- **Tmax_Klu** (Kluitenberg) - High flows with heat pulse duration correction
- **DMA** (Dual Method Approach) - Automatically switches between methods based on flow velocity

**Method Selection Logic:**
- Low velocity (<15 cm/hr): Use HRM or DMA
- Medium velocity (15-50 cm/hr): Use MHR or DMA
- High velocity (>50 cm/hr): Use Tmax methods or DMA
- Unknown/mixed conditions: Use DMA (recommended default)

See `R/14_cross_method_comparison.R` for method compatibility matrix and `user_inputs.md` for detailed parameter specifications.

### YAML-Based Method Development

Researchers can define new HPV methods using YAML configuration files instead of writing R code. This makes method development accessible to non-programmers who understand the underlying physics.

**Key files:**
- Method definitions: `inst/methods/*.yaml`
- Parser/executor: `R/yaml_method_parser.R`
- Documentation: `README_YAML_METHODS.md`

**Example workflow:**
```r
# List available YAML methods
list_available_methods()

# Calculate using YAML methods
results <- calc_heat_pulse_velocity_yaml(sap_data, methods = c("HRM", "MHR"))
```

## Important Development Notes

### Parameter Defaults

All calculation functions use consistent default parameters defined in `get_default_parameters()`:
- `diffusivity = 0.0025` cm²/s (thermal diffusivity of sapwood)
- `x = 0.5` cm (probe spacing)
- `pre_pulse = 30` sec (pre-pulse baseline period)
- `HRM_start = 60`, `HRM_end = 100` sec (HRM analysis window)

When modifying calculations, preserve parameter consistency across methods.

### Multi-Tree Processing

The package supports both single-tree and multi-tree data objects. Many functions handle both:
- Check object class with `inherits(data, "multi_tree_data")`
- Single-tree functions often work on individual elements
- Use `batch_process_sap_flow()` for parallel multi-tree processing

### Quality Flags

The package uses standardised quality flags throughout:
- `"OK"` - Data passes all quality checks
- `"WARNING"` - Data has minor issues but is usable
- `"ERROR"` - Data fails critical quality checks
- `"SUSPECT"` - Data is questionable (outliers, unusual patterns)

Maintain consistency when adding new quality checks.

### Testing Strategy

Tests are organized by module, matching the numbered file structure:
- Unit tests for individual functions
- Integration tests for pipelines (`test-processing-pipeline.R`)
- Method validation tests (`test-heat-pulse-velocity.R`, `test-advanced-hpv-methods.R`)

When adding features, add corresponding tests in the matching test file.

## Function Development Protocol (CRITICAL)

### MANDATORY Pre-Development Checks

Before creating or modifying ANY function, you MUST:

1. **Search for existing functions**:
   ```r
   # Search by name
   grep -r "function_name" R/

   # Search by functionality
   grep -r "similar_task" R/
   ```

2. **Check naming conventions**:
   - Look at existing functions in target file
   - Verify pattern matches established conventions
   - Avoid non-standard prefixes (e.g., `temp_`, `tmp_`)

3. **Verify no duplication**:
   - Can existing function be extended?
   - Does similar functionality already exist?
   - Is this the right approach?

### Function Naming Conventions

**ALWAYS follow these patterns:**

| Function Type | Pattern | Examples |
|---------------|---------|----------|
| **Core calculations** | `calc_[method]_[output]` | `calc_hrm()`, `calc_sap_flux_density()` |
| **Data operations** | `[verb]_[object]` | `read_sap_data()`, `validate_sap_data()` |
| **Utilities** | `[action]_[target]` | `convert_velocity_units()`, `filter_velocity_results()` |
| **Plotting** | `plot_[what]_[type]` | `plot_hpv_timeseries()`, `plot_temperature_data()` |
| **Validation** | `validate_[what]` | `validate_probe_config()`, `validate_sap_data()` |
| **Detection** | `detect_[what]` | `detect_probe_config()`, `detect_optimal_method()` |
| **Assessment** | `assess_[what]` | `assess_data_quality()`, `assess_signal_quality()` |

### Function Placement Rules

| File | Function Types | Prefixes |
|------|----------------|----------|
| `01_data_import.R` | Data reading | `read_`, `parse_`, `detect_format` |
| `02_data_validation.R` | Validation | `validate_` |
| `03_probe_configuration.R` | Probe config | `detect_probe_`, `validate_probe_` |
| `04_heat_pulse_velocity_core.R` | Core HPV | `calc_hrm()`, `calc_mhr()`, `calc_heat_pulse_velocity()` |
| `05_heat_pulse_velocity_advanced.R` | Advanced HPV | `calc_drm()`, `calc_chpm()`, `calc_unified_hpv()` |
| `07_method_comparison.R` | Method comparison | `compare_` |
| `08_quality_control.R` | QC functions | `diagnose_`, `detect_outliers` |
| `utilities.R` | General utilities | `convert_`, `filter_`, `export_` |
| `visualisation_core.R` | Plotting | `plot_`, `create_`, `theme_` |

### Red Flags to AVOID

❌ Creating functions without searching first
❌ Using non-standard prefixes (e.g., `temp_`, `tmp_`, `my_`)
❌ Suggesting similar names (`calc_velocity` when `calc_heat_pulse_velocity` exists)
❌ Placing functions in wrong files
❌ Duplicating existing functionality
❌ Ignoring established naming patterns

### Before Committing Code - Checklist

- [ ] Searched project for similar functions
- [ ] Verified naming follows conventions
- [ ] Confirmed function placement is correct
- [ ] No functionality duplication
- [ ] Created corresponding tests
- [ ] Updated documentation
- [ ] Verified integration with workflow

### Documentation Standards

The package uses roxygen2 for documentation. When documenting functions:
- Include `@family` tags to group related functions
- Provide working `@examples` (use `\dontrun{}` for file I/O)
- Document all parameters with units and valid ranges
- Reference scientific papers in `@references` for methods

## Common Workflows

### Adding a New HPV Method

1. **As R code:** Add to `R/advanced_hpv_methods.R` following existing patterns
2. **As YAML:** Create `inst/methods/method_name.yaml` following template in `README_YAML_METHODS.md`
3. Add tests in `tests/testthat/test-advanced-hpv-methods.R`
4. Update method compatibility matrix in `R/14_cross_method_comparison.R`
5. Document in method selection guide

### Extending Data Import

1. Add format detection logic in `detect_format()` (`R/01_data_import.R`)
2. Create parser function (e.g., `read_new_format()`)
3. Add validation for format-specific quirks
4. Add tests with sample data in `tests/testthat/test-data-import.R`

### Adding New Diagnostic Checks

1. Add check function to `R/13_sensor_diagnostics.R`
2. Integrate into `diagnose_sensor_performance()`
3. Update quality flag logic if needed
4. Add to diagnostic report template in `R/22_report_templates.R`

## Key Dependencies

**Core tidyverse:**
- dplyr, tibble, tidyr - Data manipulation
- readr, stringr - Data import and text processing
- lubridate - Datetime handling

**Suggested packages:**
- ggplot2, plotly - Visualisation
- testthat - Testing framework
- knitr, rmarkdown - Documentation

## Project Context Files

Additional documentation in repository:
- `.dev/workflow_analysis.md` - Complete scientific workflow breakdown
- `.dev/visual_summary.md` - Data flow diagrams and architecture
- `pseudo.md` - Pseudocode and function specifications
- `user_inputs.md` - Comprehensive parameter guide
- `NEWS.md` - Version history and changes

## File Format Support

The package auto-detects and imports three data formats from ICT sensors:
1. **ICT Current** - JSON-like format with nested arrays
2. **ICT Legacy** - Tab-delimited or fixed-width legacy format
3. **CSV** - Standard CSV with datetime, pulse_id, do, di, uo, ui columns

Format detection is automatic in `read_sap_data()` but can be specified explicitly if needed.
