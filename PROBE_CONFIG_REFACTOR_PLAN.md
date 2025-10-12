# Probe Configuration Refactor Plan

**Date:** 2025-10-11
**Status:** Planning
**Priority:** High

---

## Executive Summary

Refactor the probe configuration system from auto-detection to YAML-based configuration with user-friendly R overrides. Remove auto-detection logic (which cannot reliably determine probe spacing from data alone) and replace with explicit YAML configuration loading.

---

## Current State Analysis

### Existing Files & Functions

**File:** `R/03_probe_configuration.R`

**Functions to REMOVE:**
- `detect_probe_config()` - Lines 188-236: Auto-detects config from data (doesn't work reliably)
- `detect_config_from_sensors()` - Lines 238-309: Helper for auto-detection
- `get_standard_configs()` - Lines 311-398: Returns hardcoded R6 objects (replace with YAML loading)

**Functions to KEEP/MODIFY:**
- `ProbeConfiguration` (R6 class) - Keep but simplify initialization from YAML
- `validate_probe_config()` - Keep for validation
- `validate_method_requirements()` - Keep
- `validate_data_for_config()` - Keep
- `validate_sensor_alignment()` - Keep
- `get_method_compatibility_matrix()` - Keep or derive from YAML configs

**Functions to ADD:**
- `load_probe_config()` - Load configuration from YAML file
- `get_default_probe_config()` - Return default symmetric SFM1x config
- `list_available_probe_configs()` - List all available configs
- `create_custom_probe_config()` - Helper to create custom configs in R

### Existing YAML Files

**Location:** `inst/configurations/`

1. **`probe_symmetrical.yaml`** - SFM1x standard (DEFAULT)
   - Symmetric: 5mm upstream, 5mm downstream
   - Compatible methods: HRM, MHR, DMA, Tmax_Coh, Tmax_Klu, HRMx
   - Manufacturer: ICT, Model: SFM1

2. **`probe_asymmetrical.yaml`** - CHPM optimized
   - Asymmetric: 5mm upstream, 10mm downstream
   - Compatible methods: CHPM, Tmax_Coh, Tmax_Klu, MHR

### Current Usage Locations

**`R/14_processing_pipeline.R`:**
- Line 87: `probe_config <- detect_probe_config(sap_data)`
- Used in `process_sap_data()` function

**`R/05_heat_pulse_velocity_advanced.R`:**
- `detect_optimal_method()` uses probe_config
- `calc_unified_hpv()` uses probe_config

**Tests:**
- Multiple test files use probe configuration

---

## Proposed New Architecture

### 1. YAML Configuration Structure

**Enhanced YAML Schema:**

```yaml
# inst/configurations/probe_symmetrical.yaml
metadata:
  config_name: "ICT SFM1x Symmetric"
  description: "Standard symmetric probe configuration for ICT SFM1x sensors"
  created_date: "2025-10-11"
  version: "1.0"
  manufacturer: "ICT International"
  model: "SFM1"
  default: true  # Mark as default configuration

probe:
  # Physical specifications
  heater_position: 0           # mm, heater at origin
  upstream_distance: 5         # mm, sensor distance upstream (converted to cm internally)
  downstream_distance: 5       # mm, sensor distance downstream
  diameter: 1.27              # mm, standard probe diameter
  length: 35                  # mm, standard probe length

  # Sensor positions along probe
  inner_sensor: 7.5           # mm, distance from probe tip
  outer_sensor: 22.5          # mm, distance from probe tip

  # Derived spacing (calculated automatically)
  # x_inner: 0.5  # cm, probe spacing for inner sensors
  # x_outer: 0.5  # cm, probe spacing for outer sensors

# Thermal properties (optional - can be overridden in R)
thermal:
  diffusivity: 0.0025         # cm²/s, default thermal diffusivity
  diffusivity_range: [0.002, 0.004]  # Reasonable range
  estimate_from_data: false   # Whether to estimate in real-time

# Method compatibility
methods:
  compatible:
    - HRM
    - MHR
    - DMA
    - Tmax_Coh
    - Tmax_Klu
    - HRMx

  recommended:
    - HRM      # Best for low/reverse flows
    - DMA      # Best for mixed conditions
    - MHR      # Best for moderate flows

  priority_order:
    - HRM
    - DMA
    - MHR
    - Tmax_Coh

# Quality thresholds specific to this configuration
quality:
  min_signal_ratio: 3.0
  max_velocity_cm_hr: 200
  min_velocity_cm_hr: -50
```

### 2. New R Functions

**File:** `R/03_probe_configuration.R` (refactored)

```r
#' Load Probe Configuration from YAML
#'
#' Loads probe configuration from YAML file and converts to ProbeConfiguration object.
#'
#' @param config_name Character string of configuration name ("symmetrical", "asymmetrical")
#'   or path to custom YAML file
#' @param custom_params List of parameters to override from YAML (optional)
#'
#' @return ProbeConfiguration object
#'
#' @examples
#' # Load default (symmetric)
#' config <- load_probe_config()
#'
#' # Load specific config
#' config <- load_probe_config("asymmetrical")
#'
#' # Load with overrides
#' config <- load_probe_config("symmetrical",
#'                            custom_params = list(diffusivity = 0.003))
#'
#' # Load custom YAML
#' config <- load_probe_config("path/to/my_probe.yaml")
#'
#' @export
load_probe_config <- function(config_name = "symmetrical", custom_params = NULL) {

  # Determine YAML file path
  if (file.exists(config_name)) {
    # User provided full path to custom YAML
    yaml_file <- config_name
  } else {
    # Look in package configurations
    yaml_file <- system.file(
      "configurations",
      paste0("probe_", config_name, ".yaml"),
      package = "sapFluxR"
    )

    if (!file.exists(yaml_file) || yaml_file == "") {
      stop("Probe configuration '", config_name, "' not found. ",
           "Available configs: ",
           paste(list_available_probe_configs()$name, collapse = ", "))
    }
  }

  # Parse YAML
  config_data <- yaml::read_yaml(yaml_file)

  # Extract and convert probe specifications
  probe <- config_data$probe

  # Convert mm to cm for calculations
  x_upstream <- probe$upstream_distance / 10    # mm to cm
  x_downstream <- probe$downstream_distance / 10

  # Calculate sensor positions
  sensor_positions <- list(
    upstream_inner = -x_upstream,
    downstream_inner = x_downstream,
    upstream_outer = -x_upstream,
    downstream_outer = x_downstream
  )

  # Get thermal properties
  thermal <- config_data$thermal
  diffusivity <- if (!is.null(thermal$diffusivity)) {
    thermal$diffusivity
  } else {
    0.0025  # Default
  }

  # Apply user overrides
  if (!is.null(custom_params)) {
    if (!is.null(custom_params$diffusivity)) {
      diffusivity <- custom_params$diffusivity
    }
    if (!is.null(custom_params$upstream_distance)) {
      x_upstream <- custom_params$upstream_distance / 10
      sensor_positions$upstream_inner <- -x_upstream
      sensor_positions$upstream_outer <- -x_upstream
    }
    if (!is.null(custom_params$downstream_distance)) {
      x_downstream <- custom_params$downstream_distance / 10
      sensor_positions$downstream_inner <- x_downstream
      sensor_positions$downstream_outer <- x_downstream
    }
  }

  # Create ProbeConfiguration R6 object
  config <- ProbeConfiguration$new(
    config_name = config_data$metadata$config_name,
    config_type = ifelse(x_upstream == x_downstream, "symmetric", "asymmetric"),
    heater_position = probe$heater_position,
    sensor_positions = sensor_positions,
    probe_diameter = probe$diameter,
    thermal_diffusivity = diffusivity,
    compatible_methods = config_data$methods$compatible,
    method_priorities = config_data$methods$priority_order,
    required_parameters = list(
      x = mean(c(x_upstream, x_downstream)),
      diffusivity = diffusivity
    )
  )

  # Store original YAML data for reference
  config$yaml_source <- yaml_file
  config$yaml_data <- config_data

  return(config)
}


#' Get Default Probe Configuration
#'
#' Returns the default symmetric SFM1x probe configuration.
#' This is a convenience wrapper around load_probe_config("symmetrical").
#'
#' @param custom_params List of parameters to override (optional)
#'
#' @return ProbeConfiguration object
#'
#' @examples
#' config <- get_default_probe_config()
#' config <- get_default_probe_config(custom_params = list(diffusivity = 0.003))
#'
#' @export
get_default_probe_config <- function(custom_params = NULL) {
  load_probe_config("symmetrical", custom_params = custom_params)
}


#' List Available Probe Configurations
#'
#' Lists all probe configurations available in the package.
#'
#' @return Data frame with config names, descriptions, and file paths
#'
#' @examples
#' configs <- list_available_probe_configs()
#' print(configs)
#'
#' @export
list_available_probe_configs <- function() {

  # Find all probe_*.yaml files in package configurations
  config_dir <- system.file("configurations", package = "sapFluxR")

  if (config_dir == "" || !dir.exists(config_dir)) {
    warning("Package configurations directory not found")
    return(data.frame(
      name = character(0),
      description = character(0),
      file = character(0),
      default = logical(0)
    ))
  }

  yaml_files <- list.files(
    config_dir,
    pattern = "^probe_.*\\.yaml$",
    full.names = TRUE
  )

  if (length(yaml_files) == 0) {
    return(data.frame(
      name = character(0),
      description = character(0),
      file = character(0),
      default = logical(0)
    ))
  }

  # Parse each YAML to get metadata
  configs <- lapply(yaml_files, function(f) {
    config_data <- yaml::read_yaml(f)
    data.frame(
      name = gsub("^probe_(.*)\\.yaml$", "\\1", basename(f)),
      description = config_data$metadata$description,
      file = f,
      default = isTRUE(config_data$metadata$default),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, configs)

  # Sort with default first
  result <- result[order(-result$default, result$name), ]
  rownames(result) <- NULL

  return(result)
}


#' Create Custom Probe Configuration
#'
#' Helper function to create a custom probe configuration in R
#' without needing a YAML file. Useful for quick testing or one-off configs.
#'
#' @param config_name Name for the configuration
#' @param upstream_distance Distance upstream of heater (mm)
#' @param downstream_distance Distance downstream of heater (mm)
#' @param probe_diameter Probe diameter (mm)
#' @param diffusivity Thermal diffusivity (cm²/s)
#' @param compatible_methods Vector of compatible method names
#'
#' @return ProbeConfiguration object
#'
#' @examples
#' # Create custom asymmetric configuration
#' custom_config <- create_custom_probe_config(
#'   config_name = "My Custom Config",
#'   upstream_distance = 6,
#'   downstream_distance = 8,
#'   diffusivity = 0.0028
#' )
#'
#' @export
create_custom_probe_config <- function(config_name = "Custom Configuration",
                                       upstream_distance = 5,
                                       downstream_distance = 5,
                                       probe_diameter = 1.27,
                                       diffusivity = 0.0025,
                                       compatible_methods = c("HRM", "MHR", "DMA")) {

  # Convert mm to cm
  x_upstream <- upstream_distance / 10
  x_downstream <- downstream_distance / 10

  # Create sensor positions
  sensor_positions <- list(
    upstream_inner = -x_upstream,
    downstream_inner = x_downstream,
    upstream_outer = -x_upstream,
    downstream_outer = x_downstream
  )

  # Determine config type
  config_type <- ifelse(upstream_distance == downstream_distance,
                       "symmetric",
                       "asymmetric")

  # Create configuration
  config <- ProbeConfiguration$new(
    config_name = config_name,
    config_type = config_type,
    heater_position = 0,
    sensor_positions = sensor_positions,
    probe_diameter = probe_diameter,
    thermal_diffusivity = diffusivity,
    compatible_methods = compatible_methods,
    method_priorities = compatible_methods,
    required_parameters = list(
      x = mean(c(x_upstream, x_downstream)),
      diffusivity = diffusivity
    )
  )

  return(config)
}
```

### 3. Modified Processing Pipeline

**File:** `R/14_processing_pipeline.R`

**OLD (Lines 85-87):**
```r
# Auto-detect probe configuration
probe_config <- detect_probe_config(sap_data)
```

**NEW:**
```r
# Load probe configuration (default or user-specified)
if (is.null(probe_config)) {
  probe_config <- get_default_probe_config()
  message("Using default symmetric SFM1x probe configuration")
} else if (is.character(probe_config)) {
  # User provided config name or file path
  probe_config <- load_probe_config(probe_config, custom_params = probe_params)
}
# Otherwise user provided ProbeConfiguration object directly
```

**Updated function signature:**
```r
process_sap_data <- function(sap_data,
                            methods = NULL,
                            parameters = NULL,
                            probe_config = NULL,      # NEW: explicit config
                            probe_params = NULL,      # NEW: config overrides
                            dbh = NULL,
                            sapwood_area = NULL,
                            assess_quality = TRUE,
                            recommend_methods = TRUE,
                            aggregate_temporal = NULL) {
  # ... function body
}
```

### 4. User-Facing Workflows

#### Workflow 1: Use Default (Most Common)
```r
# Simple - uses default symmetric SFM1x config
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)

# Or with pipeline
results <- process_sap_data(sap_data)
```

#### Workflow 2: Override Parameters
```r
# Use default but change diffusivity
sap_data <- read_sap_data("data.txt")
results <- process_sap_data(
  sap_data,
  probe_params = list(diffusivity = 0.0028)
)
```

#### Workflow 3: Use Different Built-in Config
```r
# Use asymmetric configuration
results <- process_sap_data(
  sap_data,
  probe_config = "asymmetrical"
)
```

#### Workflow 4: Load Custom YAML
```r
# User has their own probe configuration
results <- process_sap_data(
  sap_data,
  probe_config = "path/to/my_custom_probe.yaml"
)
```

#### Workflow 5: Create Config in R
```r
# Create custom config programmatically
my_config <- create_custom_probe_config(
  config_name = "Custom 6mm spacing",
  upstream_distance = 6,
  downstream_distance = 6,
  diffusivity = 0.0028
)

results <- process_sap_data(sap_data, probe_config = my_config)
```

#### Workflow 6: View Available Configs
```r
# List all available configurations
configs <- list_available_probe_configs()
print(configs)

# Load a specific one
config <- load_probe_config("symmetrical")
print(config)
```

---

## Implementation Steps

### Phase 1: Preparation (1-2 hours)

1. **Update YAML files** with enhanced schema
   - Add `default: true` to `probe_symmetrical.yaml`
   - Add thermal properties section
   - Add quality thresholds
   - Ensure consistent structure

2. **Create backup** of `03_probe_configuration.R`
   - Copy to `03_probe_configuration_OLD.R` (temporary)

### Phase 2: Code Implementation (3-4 hours)

3. **Add new functions** to `03_probe_configuration.R`
   - `load_probe_config()`
   - `get_default_probe_config()`
   - `list_available_probe_configs()`
   - `create_custom_probe_config()`

4. **Modify ProbeConfiguration R6 class**
   - Add `yaml_source` and `yaml_data` fields
   - Keep all existing methods

5. **Comment out (don't delete yet) old functions**
   - `detect_probe_config()` - Mark as deprecated
   - `detect_config_from_sensors()` - Mark as deprecated
   - `get_standard_configs()` - Mark as deprecated

6. **Update `14_processing_pipeline.R`**
   - Modify `process_sap_data()` signature
   - Replace `detect_probe_config()` call with new logic
   - Add parameter documentation

7. **Update `04_heat_pulse_velocity_core.R`**
   - Modify `calc_heat_pulse_velocity()` signature if needed
   - Accept optional `probe_config` parameter

8. **Update `05_heat_pulse_velocity_advanced.R`**
   - Update functions that use probe_config
   - Ensure they accept probe_config as parameter

### Phase 3: Testing (2-3 hours)

9. **Update test files**
   - Modify tests that use `detect_probe_config()`
   - Add tests for new functions
   - Test default behavior
   - Test custom YAML loading
   - Test parameter overrides

10. **Create test custom YAML** in `tests/testthat/`
    - `test_probe_custom.yaml` with non-standard spacing
    - Use in tests

11. **Run full test suite**
    ```r
    devtools::test()
    ```

12. **Test real workflows**
    - Test all 6 user workflows above
    - Check error messages are helpful

### Phase 4: Documentation (1-2 hours)

13. **Update roxygen documentation**
    - Document all new functions
    - Add examples
    - Update @param descriptions
    - Run `devtools::document()`

14. **Update README.md**
    - Add section on probe configuration
    - Show examples of workflows

15. **Update user_inputs.md**
    - Document probe_config parameter
    - Document probe_params parameter
    - Explain YAML structure

16. **Update FUNCTION_REFERENCE.md**
    - Add new functions
    - Mark old functions as deprecated

17. **Create migration guide**
    - `PROBE_CONFIG_MIGRATION.md`
    - Show old vs new code
    - Help users transition

### Phase 5: Cleanup (30 min)

18. **Remove deprecated functions**
    - Delete `detect_probe_config()`
    - Delete `detect_config_from_sensors()`
    - Delete `get_standard_configs()`
    - Remove from NAMESPACE

19. **Run final checks**
    ```r
    devtools::check()
    ```

20. **Delete backup file**
    - Remove `03_probe_configuration_OLD.R`

---

## Files to Modify

### R Source Files
- ✏️ `R/03_probe_configuration.R` - Major refactor
- ✏️ `R/14_processing_pipeline.R` - Update process_sap_data()
- ✏️ `R/04_heat_pulse_velocity_core.R` - Add probe_config parameter
- ✏️ `R/05_heat_pulse_velocity_advanced.R` - Update functions using probe_config
- ✏️ `R/08_quality_control.R` - Update if uses probe detection

### Configuration Files
- ✏️ `inst/configurations/probe_symmetrical.yaml` - Enhance schema
- ✏️ `inst/configurations/probe_asymmetrical.yaml` - Enhance schema
- ➕ `inst/configurations/probe_custom_example.yaml` - Add example for users

### Documentation Files
- ✏️ `README.md` - Add probe config section
- ✏️ `user_inputs.md` - Document new parameters
- ✏️ `FUNCTION_REFERENCE.md` - Update function list
- ✏️ `CLAUDE.md` - Update development notes
- ➕ `PROBE_CONFIG_MIGRATION.md` - New migration guide

### Test Files
- ✏️ `tests/testthat/test-probe-config.R` - Major updates
- ✏️ `tests/testthat/test-processing-pipeline.R` - Update pipeline tests
- ✏️ `tests/testthat/test-heat-pulse-velocity.R` - Update if needed
- ➕ `tests/testthat/test_probe_custom.yaml` - Test YAML file

---

## Breaking Changes & Migration

### Breaking Changes

1. **`detect_probe_config()` removed**
   - Was never reliable anyway
   - Users should explicitly specify config

2. **`get_standard_configs()` removed**
   - Replaced by `load_probe_config()` and `list_available_probe_configs()`

3. **`process_sap_data()` signature changed**
   - Added `probe_config` parameter
   - Added `probe_params` parameter
   - Old code without these will use defaults (backward compatible)

### Migration Path

**Old Code:**
```r
# This will break
config <- detect_probe_config(sap_data)
results <- process_sap_data(sap_data)
```

**New Code (Option 1 - Simplest):**
```r
# Use default - works automatically
results <- process_sap_data(sap_data)
```

**New Code (Option 2 - Explicit):**
```r
# Explicitly load config
config <- load_probe_config("symmetrical")
results <- process_sap_data(sap_data, probe_config = config)
```

---

## Validation & Quality Checks

### Pre-Implementation Checks
- ✅ YAML files exist and are valid
- ✅ Current tests pass
- ✅ Understand all usages of probe detection

### Post-Implementation Checks
- ⬜ All new functions have tests
- ⬜ All new functions have documentation
- ⬜ `devtools::test()` passes
- ⬜ `devtools::check()` passes with 0 errors, 0 warnings
- ⬜ All 6 user workflows work correctly
- ⬜ Error messages are clear and helpful
- ⬜ YAML validation catches malformed configs

---

## Risks & Mitigation

### Risk 1: Breaking User Code
**Mitigation:**
- Keep default behavior working (uses symmetric config automatically)
- Provide clear migration guide
- Add helpful warning messages

### Risk 2: YAML Parsing Errors
**Mitigation:**
- Validate YAML structure on load
- Provide clear error messages
- Include example YAMLs in package

### Risk 3: Parameter Confusion
**Mitigation:**
- Clear documentation
- Helpful examples
- `list_available_probe_configs()` for discovery

### Risk 4: Test Failures
**Mitigation:**
- Update tests incrementally
- Keep old code commented until tests pass
- Run tests frequently during development

---

## Success Criteria

✅ **Core Functionality:**
- Default workflow works without specifying probe_config
- Users can load built-in configs by name
- Users can load custom YAML files
- Users can override parameters in R
- Users can create configs programmatically

✅ **Code Quality:**
- All tests pass
- No warnings from `devtools::check()`
- Documentation complete
- Examples work

✅ **User Experience:**
- Simpler than before (no auto-detection confusion)
- Flexible (multiple ways to specify config)
- Clear error messages
- Good documentation

---

## Timeline Estimate

| Phase | Tasks | Time Estimate |
|-------|-------|---------------|
| **Phase 1: Preparation** | Update YAMLs, backup | 1-2 hours |
| **Phase 2: Implementation** | Code changes | 3-4 hours |
| **Phase 3: Testing** | Update tests, run checks | 2-3 hours |
| **Phase 4: Documentation** | Update docs | 1-2 hours |
| **Phase 5: Cleanup** | Remove old code | 30 min |
| **Total** | | **8-12 hours** |

---

## Questions for User

Before implementing, please confirm:

1. ✅ **Default behavior:** Should `calc_heat_pulse_velocity()` and `process_sap_data()` use symmetric config if not specified?

2. ✅ **Parameter names:** Are `probe_config` and `probe_params` good names?

3. **Custom YAML location:** Should users put custom YAMLs in their project directory or install them into the package?

4. **Validation strictness:** Should invalid YAML files cause errors or warnings?

5. **Backwards compatibility:** Should we keep `detect_probe_config()` as deprecated with warning, or remove completely?

---

## Next Steps

1. **Review this plan** - Confirm approach and timeline
2. **Answer questions above**
3. **Begin Phase 1** - Update YAML files
4. **Implement incrementally** - Test after each phase
5. **Create migration guide** - Help users transition

---

**Status:** Ready for review and approval
