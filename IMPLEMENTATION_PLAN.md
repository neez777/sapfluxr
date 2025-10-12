# Configuration System Refactor - Implementation Plan

**Date Created:** 2025-10-11
**Status:** Ready for Implementation
**Estimated Time:** 8-12 hours across multiple sessions

---

## Executive Summary

This plan refactors the sapFluxR configuration system to:
1. **Remove unreliable auto-detection** of probe configuration
2. **Implement YAML-based configuration loading** for both probe and wood properties
3. **Provide sensible defaults** that work out-of-the-box
4. **Allow flexible overrides** at multiple levels (YAML files, function parameters, individual values)

---

## Configuration Architecture

### Three-Tier Parameter System

```
┌─────────────────────────────────────────────────────────────┐
│ 1. PROBE CONFIGURATION (Hardware - Fixed per probe model)  │
│    - probe_symmetrical.yaml (DEFAULT)                       │
│    - probe_asymmetrical.yaml                                │
│    - User custom probe YAMLs                                │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 2. WOOD PROPERTIES (Species/Tree - Variable)               │
│    - wood_generic_sw.yaml (DEFAULT)                         │
│    - wood_eucalyptus.yaml                                   │
│    - wood_pine.yaml                                         │
│    - User custom wood YAMLs (per species or per tree)       │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 3. CALCULATION PARAMETERS (Analysis settings)               │
│    - Defined in R code as function parameters               │
│    - User provides as list to override defaults             │
└─────────────────────────────────────────────────────────────┘
```

---

## Variable Categorization (FINAL)

### Category 1: Probe Configuration Variables

**Location:** `inst/configurations/probe_*.yaml` under `probe:` section

| Variable | Units | Example | Source |
|----------|-------|---------|--------|
| heater_position | mm | 0 | Probe specs |
| upstream_distance | mm | 5 | Probe specs |
| downstream_distance | mm | 5 | Probe specs |
| diameter | mm | 1.27 | Probe specs |
| length | mm | 35 | Probe specs |
| needle_diameter | mm | 1.27 | Probe specs |
| inner_sensor | mm | 7.5 | Probe specs |
| outer_sensor | mm | 22.5 | Probe specs |
| manufacturer | - | "ICT" | Probe specs |
| model | - | "SFM1" | Probe specs |
| **heat_pulse_duration** | **seconds** | **2** | **Probe specs** (NEW!) |

**Also in probe config:**
- `methods.compatible` - List of compatible HPV methods
- `methods.recommended` - Recommended methods for this probe
- `methods.priority_order` - Priority ranking of methods

---

### Category 2: Wood Properties Variables

**Location:** `inst/configurations/wood_*.yaml` under `wood_property:` section

| Variable | Units | Typical Range | Required? |
|----------|-------|---------------|-----------|
| thermal_diffusivity | cm²/s | 0.002-0.004 | Yes (key variable) |
| thermal_conductivity | W/(m·K) | 0.1-0.5 | Optional (can estimate) |
| volumetric_heat_capacity | J/(m³·K) | 1.5e6-2.5e6 | Optional (can estimate) |
| dry_density | kg/m³ | 300-800 | Recommended |
| basic_density | kg/m³ | 250-650 | Optional |
| moisture_content | % | 20-50 | Recommended |
| species | - | "Eucalyptus spp." | Optional |
| wood_type | - | "softwood"/"hardwood" | Optional |
| temperature | °C | 15-30 | Optional |

**Tree-specific measurements (optional in YAML, overridable via function params):**

| Variable | Units | Purpose | Required? |
|----------|-------|---------|-----------|
| dbh | cm | Diameter at breast height | For scaling |
| bark_thickness | cm | Bark thickness | For sapwood calcs |
| sapwood_depth | cm | Sapwood depth | For flux scaling |
| sapwood_area | cm² | Conducting sapwood area | For tree-level water use |
| heartwood_radius | cm | Heartwood radius | Optional |

**Quality thresholds (species defaults, overridable):**

| Variable | Units | Default | Purpose |
|----------|-------|---------|---------|
| max_velocity_cm_hr | cm/hr | 200 | QC upper bound |
| min_velocity_cm_hr | cm/hr | -50 | QC lower bound (allows reverse flow) |
| temperature_range | °C | [-10, 60] | Acceptable temp range |

---

### Category 3: Calculation Parameters

**Location:** Function parameters in R code (NOT in YAML)

| Variable | Units | Default | Used By |
|----------|-------|---------|---------|
| pre_pulse | seconds | 30 | All methods |
| HRM_start | seconds | 60 | HRM, MHR, DMA |
| HRM_end | seconds | 100 | HRM, MHR, DMA |
| L | proportion | 0.5 | HRMXa, HRMXb |
| H | proportion | 0.8 | HRMXa, HRMXb |
| min_signal_ratio | - | 3.0 | Quality control |

**Note:** `tp_1` (heat pulse duration) moved to probe config YAML, but can still be overridden via calculation parameters if needed.

---

## Current vs. Proposed Workflow

### Current Workflow (BROKEN)
```r
sap_data <- read_sap_data("data.txt")
probe_config <- detect_probe_config(sap_data)  # ❌ Can't reliably detect from data!
results <- calc_heat_pulse_velocity(sap_data)
```

### Proposed Workflow (SIMPLE DEFAULT)
```r
# Just works - uses defaults
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)
# Defaults used:
# - probe: symmetrical (probe_symmetrical.yaml)
# - wood: generic softwood (wood_generic_sw.yaml)
# - params: standard calculation settings
```

### Proposed Workflow (SPECIES-SPECIFIC)
```r
# Use species-specific wood properties
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(
  sap_data,
  wood_properties = "eucalyptus"  # Loads wood_eucalyptus.yaml
)
```

### Proposed Workflow (CUSTOM YAML FILES)
```r
# User has custom configurations
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = "path/to/my_probe.yaml",
  wood_properties = "path/to/my_tree.yaml"
)
```

### Proposed Workflow (MULTI-TREE WITH YAML FILES)
```r
# Import multiple trees with tree-specific wood property files
tree_files <- list.files("data/", pattern = "*.txt", full.names = TRUE)
wood_files <- list.files("data/", pattern = "*_tree.yaml", full.names = TRUE)

results <- batch_process_sap_flow(
  data_files = tree_files,
  wood_property_files = wood_files,  # Matched by name or order
  probe_config = "symmetrical"
)
```

### Proposed Workflow (PARAMETER OVERRIDES)
```r
# Override specific values without creating YAML
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(
  sap_data,
  wood_properties = "eucalyptus",
  wood_overrides = list(
    thermal_diffusivity = 0.0028,
    dbh = 45.2,
    sapwood_depth = 3.2
  ),
  parameters = list(
    HRM_start = 70,
    HRM_end = 110
  )
)
```

---

## Implementation Phases

### Phase 1: Update YAML Files (1 hour)
**Goal:** Add missing variables to existing YAML files

#### Phase 1.1: Update Probe YAML Files
Add `heat_pulse_duration` to probe configurations.

**Files to modify:**
- `inst/configurations/probe_symmetrical.yaml`
- `inst/configurations/probe_asymmetrical.yaml`

**Changes:**
```yaml
probe:
  heater_position: 0
  upstream_distance: 5
  downstream_distance: 5
  diameter: 1.27
  length: 35
  needle_diameter: 1.27
  inner_sensor: 7.5
  outer_sensor: 22.5
  manufacturer: ICT
  model: SFM1
  heat_pulse_duration: 2        # NEW: seconds, typical ICT pulse duration
```

#### Phase 1.2: Update Wood Property YAML Files
Add quality thresholds and optional tree measurements sections.

**Files to modify:**
- `inst/configurations/wood_generic_sw.yaml`
- `inst/configurations/wood_eucalyptus.yaml`
- `inst/configurations/wood_pine.yaml`

**Changes:**
```yaml
wood_property:
  # ... existing thermal/physical properties ...

# NEW: Tree measurements (optional - can override per tree)
tree_measurements:
  dbh: null                   # cm, diameter at breast height
  bark_thickness: null        # cm
  sapwood_depth: null         # cm
  sapwood_area: null          # cm²
  heartwood_radius: null      # cm

# NEW: Quality thresholds (species-specific defaults)
quality_thresholds:
  max_velocity_cm_hr: 200     # cm/hr, upper bound for this species
  min_velocity_cm_hr: -50     # cm/hr, lower bound (allows reverse flow)
  temperature_range: [-10, 60]  # °C, acceptable range
```

#### Phase 1.3: Set Default Flags
Ensure default configurations are marked.

**Changes:**
- `probe_symmetrical.yaml`: Add `default: true` to metadata
- `wood_generic_sw.yaml`: Add `default: true` to metadata

---

### Phase 2: Create Wood Property Loading System (2-3 hours)
**Goal:** Implement R functions to load and manage wood properties (parallel to probe config system)

#### Phase 2.1: Create New R File
**New file:** `R/03b_wood_properties.R`

**Functions to implement:**

1. **`WoodProperties` R6 Class**
   - Similar structure to ProbeConfiguration
   - Fields: thermal properties, tree measurements, quality thresholds
   - Methods: validate, print, summary

2. **`load_wood_properties()`**
   - Load wood properties from YAML file
   - Accept: name ("eucalyptus"), path, or NULL (use default)
   - Return: WoodProperties object
   - Allow overrides via `overrides` parameter

3. **`get_default_wood_properties()`**
   - Return default generic softwood properties
   - Convenience wrapper around load_wood_properties("generic_sw")

4. **`list_available_wood_properties()`**
   - List all wood_*.yaml files in package
   - Return data frame with name, species, description

5. **`create_custom_wood_properties()`**
   - Create WoodProperties object in R without YAML
   - For quick testing and one-off configurations

**Pseudocode for key function:**
```r
load_wood_properties <- function(config_name = "generic_sw",
                                  overrides = NULL,
                                  tree_overrides = NULL) {

  # Determine YAML file path
  if (is.null(config_name)) {
    config_name <- "generic_sw"  # Default
  }

  if (file.exists(config_name)) {
    yaml_file <- config_name  # User provided path
  } else {
    yaml_file <- system.file(
      "configurations",
      paste0("wood_", config_name, ".yaml"),
      package = "sapFluxR"
    )

    if (!file.exists(yaml_file) || yaml_file == "") {
      stop("Wood properties '", config_name, "' not found. ",
           "Available: ", paste(list_available_wood_properties()$name, collapse = ", "))
    }
  }

  # Parse YAML
  config_data <- yaml::read_yaml(yaml_file)

  # Extract properties
  wood_props <- config_data$wood_property
  tree_meas <- config_data$tree_measurements
  quality <- config_data$quality_thresholds

  # Apply wood property overrides
  if (!is.null(overrides)) {
    wood_props <- modifyList(wood_props, overrides)
  }

  # Apply tree measurement overrides
  if (!is.null(tree_overrides)) {
    if (is.null(tree_meas)) tree_meas <- list()
    tree_meas <- modifyList(tree_meas, tree_overrides)
  }

  # Create WoodProperties object
  wood_config <- WoodProperties$new(
    config_name = config_data$metadata$config_name,
    thermal_diffusivity = wood_props$thermal_diffusivity,
    thermal_conductivity = wood_props$thermal_conductivity,
    volumetric_heat_capacity = wood_props$volumetric_heat_capacity,
    dry_density = wood_props$dry_density,
    basic_density = wood_props$basic_density,
    moisture_content = wood_props$moisture_content,
    species = wood_props$species,
    wood_type = wood_props$wood_type,
    temperature = wood_props$temperature,
    tree_measurements = tree_meas,
    quality_thresholds = quality,
    yaml_source = yaml_file,
    yaml_data = config_data
  )

  return(wood_config)
}
```

---

### Phase 3: Refactor Probe Configuration System (2-3 hours)
**Goal:** Remove auto-detection, implement YAML-based loading (already mostly designed in PROBE_CONFIG_REFACTOR_PLAN.md)

#### Phase 3.1: Add New Functions to `R/03_probe_configuration.R`

**Functions to implement:**

1. **`load_probe_config()`**
   - Load probe configuration from YAML
   - Accept: name ("symmetrical"), path, or NULL (use default)
   - Return: ProbeConfiguration object
   - Support parameter overrides

2. **`get_default_probe_config()`**
   - Return default symmetric probe configuration
   - Convenience wrapper

3. **`list_available_probe_configs()`**
   - List all probe_*.yaml files
   - Return data frame with names and descriptions

4. **`create_custom_probe_config()`**
   - Create ProbeConfiguration object in R without YAML

**Read heat_pulse_duration from YAML:**
```r
load_probe_config <- function(config_name = "symmetrical",
                              custom_params = NULL) {
  # ... load YAML ...

  probe <- config_data$probe

  # Get heat pulse duration (NEW!)
  heat_pulse_duration <- if (!is.null(probe$heat_pulse_duration)) {
    probe$heat_pulse_duration
  } else {
    2  # Default fallback
  }

  # ... rest of function ...

  config <- ProbeConfiguration$new(
    # ... existing fields ...
    heat_pulse_duration = heat_pulse_duration,  # NEW field
    # ...
  )
}
```

#### Phase 3.2: Modify ProbeConfiguration R6 Class

**Add field:**
- `heat_pulse_duration` - stored and accessible

#### Phase 3.3: Remove/Deprecate Old Functions

**Functions to REMOVE from `R/03_probe_configuration.R`:**
- `detect_probe_config()` - Lines 188-236 (unreliable, cannot work)
- `detect_config_from_sensors()` - Lines 238-309 (helper for auto-detection)
- `get_standard_configs()` - Lines 311-398 (replaced by load_probe_config)

**Strategy:**
1. Comment out functions first (don't delete)
2. Add deprecation warnings
3. Test everything works
4. Delete in final cleanup phase

---

### Phase 4: Update Core Calculation Functions (2-3 hours)
**Goal:** Modify calculation functions to accept and use new configuration system

#### Phase 4.1: Update `calc_heat_pulse_velocity()` in `R/04_heat_pulse_velocity_core.R`

**Current signature:**
```r
calc_heat_pulse_velocity <- function(sap_data,
                                     pulse_ids = NULL,
                                     methods = c("HRM", "MHR", "DMA"),
                                     parameters = NULL,
                                     plot_results = FALSE)
```

**New signature:**
```r
calc_heat_pulse_velocity <- function(sap_data,
                                     pulse_ids = NULL,
                                     methods = c("HRM", "MHR", "DMA"),

                                     # NEW: Configuration system
                                     probe_config = NULL,
                                     wood_properties = NULL,

                                     # Keep for backward compatibility & overrides
                                     parameters = NULL,

                                     # NEW: Convenience overrides
                                     diffusivity = NULL,
                                     x = NULL,

                                     plot_results = FALSE)
```

**Implementation logic:**
```r
calc_heat_pulse_velocity <- function(sap_data,
                                     pulse_ids = NULL,
                                     methods = c("HRM", "MHR", "DMA"),
                                     probe_config = NULL,
                                     wood_properties = NULL,
                                     parameters = NULL,
                                     diffusivity = NULL,
                                     x = NULL,
                                     plot_results = FALSE) {

  # Step 1: Load probe configuration
  if (is.null(probe_config)) {
    probe_config <- get_default_probe_config()
  } else if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
  }
  # Otherwise assume probe_config is already ProbeConfiguration object

  # Step 2: Load wood properties
  if (is.null(wood_properties)) {
    wood_properties <- get_default_wood_properties()
  } else if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
  }
  # Otherwise assume wood_properties is already WoodProperties object

  # Step 3: Build parameters list with priority hierarchy:
  # 1. User-provided individual overrides (diffusivity, x)
  # 2. User-provided parameters list
  # 3. Configuration objects (probe_config, wood_properties)
  # 4. Hardcoded defaults

  default_params <- list(
    diffusivity = wood_properties$thermal_diffusivity,  # From wood config
    x = probe_config$required_parameters$x,             # From probe config
    tp_1 = probe_config$heat_pulse_duration,            # From probe config (NEW!)
    L = 0.5,
    H = 0.8,
    HRM_start = 60,
    HRM_end = 100,
    pre_pulse = 30
  )

  # Merge with user parameters
  if (!is.null(parameters)) {
    params <- modifyList(default_params, parameters)
  } else {
    params <- default_params
  }

  # Apply individual overrides (highest priority)
  if (!is.null(diffusivity)) {
    params$diffusivity <- diffusivity
  }
  if (!is.null(x)) {
    params$x <- x
  }

  # ... rest of calculation logic ...
}
```

#### Phase 4.2: Update `process_sap_data()` in `R/14_processing_pipeline.R`

**Current line 87:**
```r
probe_config <- detect_probe_config(sap_data)  # ❌ REMOVE THIS
```

**Replace with:**
```r
# Load probe configuration (default or user-specified)
if (is.null(probe_config)) {
  probe_config <- get_default_probe_config()
  message("Using default symmetric probe configuration (ICT SFM1x)")
} else if (is.character(probe_config)) {
  probe_config <- load_probe_config(probe_config)
  message("Loaded probe configuration: ", probe_config$config_name)
}

# Load wood properties (default or user-specified)
if (is.null(wood_properties)) {
  wood_properties <- get_default_wood_properties()
  message("Using default wood properties (generic softwood)")
} else if (is.character(wood_properties)) {
  wood_properties <- load_wood_properties(wood_properties)
  message("Loaded wood properties: ", wood_properties$config_name)
}
```

**Update function signature:**
```r
process_sap_data <- function(sap_data,
                            methods = NULL,
                            parameters = NULL,

                            # NEW configuration system
                            probe_config = NULL,
                            wood_properties = NULL,

                            # Tree-specific measurements (override wood properties)
                            dbh = NULL,
                            sapwood_area = NULL,
                            sapwood_depth = NULL,
                            bark_thickness = NULL,

                            # ... rest of parameters ...
                            assess_quality = TRUE,
                            recommend_methods = TRUE,
                            aggregate_temporal = NULL)
```

#### Phase 4.3: Update Other Functions Using Configurations

**Check and update these files:**
- `R/05_heat_pulse_velocity_advanced.R` - Functions using probe_config
- `R/08_quality_control.R` - Use quality thresholds from wood_properties
- `R/11_flux_density.R` - May need wood properties for scaling
- `R/13_tree_water_use.R` - Needs tree measurements from wood_properties

---

### Phase 5: Testing (2-3 hours)
**Goal:** Ensure all workflows work correctly

#### Phase 5.1: Update Test Files

**Files to update:**
- `tests/testthat/test-probe-config.R` - Update to test new loading functions
- `tests/testthat/test-processing-pipeline.R` - Update pipeline tests
- `tests/testthat/test-heat-pulse-velocity.R` - Update calculation tests

**New tests to add:**
- Test `load_probe_config()` with name and path
- Test `load_wood_properties()` with name and path
- Test default behavior (no configs specified)
- Test parameter override hierarchy
- Test multi-tree workflow with per-tree YAML files
- Test error handling (missing files, malformed YAML)

#### Phase 5.2: Create Test Data

**New test files needed:**
- `tests/testthat/test_probe_custom.yaml` - Custom probe for testing
- `tests/testthat/test_wood_custom.yaml` - Custom wood properties for testing

#### Phase 5.3: Test All Workflows

Test each workflow from "Current vs. Proposed Workflow" section:
1. ✅ Simple default workflow
2. ✅ Species-specific workflow
3. ✅ Custom YAML files workflow
4. ✅ Multi-tree workflow
5. ✅ Parameter overrides workflow

#### Phase 5.4: Run Package Checks

```r
# Run all tests
devtools::test()

# Run full package check
devtools::check()

# Test coverage
covr::package_coverage()
```

---

### Phase 6: Documentation (1-2 hours)
**Goal:** Document new system for users

#### Phase 6.1: Update Function Documentation

**Files to update with roxygen2 docs:**
- `R/03_probe_configuration.R` - Document new functions
- `R/03b_wood_properties.R` - Document all wood property functions
- `R/04_heat_pulse_velocity_core.R` - Update calc_heat_pulse_velocity docs
- `R/14_processing_pipeline.R` - Update process_sap_data docs

**Run:**
```r
devtools::document()
```

#### Phase 6.2: Update User Guides

**Files to update:**
- `README.md` - Add configuration system section
- `user_inputs.md` - Document new parameters and YAML structure
- `FUNCTION_REFERENCE.md` - Add new functions, mark deprecated ones

#### Phase 6.3: Create New Documentation

**New files to create:**

1. **`CONFIGURATION_GUIDE.md`** - Comprehensive guide to configuration system
   - How to use defaults
   - How to create custom YAML files
   - Parameter override hierarchy
   - Multi-tree workflows
   - Troubleshooting

2. **`MIGRATION_GUIDE.md`** - Help users transition from old system
   - What changed
   - Old code → New code examples
   - Breaking changes list
   - FAQ

#### Phase 6.4: Create Example YAML Files for Users

**New files in `inst/configurations/`:**
- `probe_custom_example.yaml` - Template for users to create custom probes
- `wood_custom_example.yaml` - Template for users to create custom wood properties

---

### Phase 7: Cleanup & Finalization (30 minutes)
**Goal:** Remove old code, final checks

#### Phase 7.1: Remove Deprecated Functions

**From `R/03_probe_configuration.R`, delete:**
- `detect_probe_config()`
- `detect_config_from_sensors()`
- `get_standard_configs()`

**Update NAMESPACE:**
```r
devtools::document()  # Will update NAMESPACE automatically
```

#### Phase 7.2: Final Checks

```r
# Run all tests one more time
devtools::test()

# Run full package check
devtools::check()

# Build documentation
devtools::document()

# Build and check package
devtools::build()
devtools::check_built()
```

#### Phase 7.3: Update NEWS.md

Add entry for this major refactor:
```markdown
# sapFluxR 0.X.0

## Major Changes

### Configuration System Refactor
- **Breaking change:** Removed unreliable `detect_probe_config()` function
- **New:** YAML-based configuration system for probe and wood properties
- **New:** `load_probe_config()` and `load_wood_properties()` functions
- **New:** Sensible defaults - package works out-of-the-box
- **New:** Flexible parameter override system
- **New:** Support for per-tree YAML configuration files

### New Features
- Heat pulse duration now configurable in probe YAML files
- Quality thresholds now species-specific in wood property YAMLs
- Tree measurements (DBH, sapwood depth) can be stored in wood property YAMLs
- Multi-tree workflows with per-tree configuration files

### Migration
- See MIGRATION_GUIDE.md for help transitioning from old code
- Old code without explicit configs will use sensible defaults (backward compatible)
```

---

## Parameter Override Hierarchy (CRITICAL)

Users can specify parameters at multiple levels. Priority (highest to lowest):

1. **Individual function parameters** (e.g., `diffusivity = 0.003`)
2. **`parameters` list** (e.g., `parameters = list(diffusivity = 0.003)`)
3. **Configuration objects** (probe_config, wood_properties)
4. **Hardcoded defaults** in function

**Example showing all levels:**
```r
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = "symmetrical",              # Provides x = 0.5, tp_1 = 2
  wood_properties = "eucalyptus",            # Provides diffusivity = 0.00143
  parameters = list(diffusivity = 0.0025),   # Overrides wood property
  diffusivity = 0.0028                       # Overrides everything - WINS
)
# Final diffusivity used: 0.0028
```

---

## Files Modified/Created Summary

### Files to MODIFY

| File | Changes | Estimated Time |
|------|---------|----------------|
| `inst/configurations/probe_symmetrical.yaml` | Add heat_pulse_duration | 5 min |
| `inst/configurations/probe_asymmetrical.yaml` | Add heat_pulse_duration | 5 min |
| `inst/configurations/wood_generic_sw.yaml` | Add tree_measurements, quality_thresholds, default flag | 10 min |
| `inst/configurations/wood_eucalyptus.yaml` | Add tree_measurements, quality_thresholds | 10 min |
| `inst/configurations/wood_pine.yaml` | Add tree_measurements, quality_thresholds | 10 min |
| `R/03_probe_configuration.R` | Add new functions, remove old ones | 2 hours |
| `R/04_heat_pulse_velocity_core.R` | Update calc_heat_pulse_velocity signature and logic | 1 hour |
| `R/14_processing_pipeline.R` | Update process_sap_data signature and logic | 1 hour |
| `R/05_heat_pulse_velocity_advanced.R` | Update functions using configs | 30 min |
| `R/08_quality_control.R` | Use quality thresholds from wood_properties | 30 min |
| `tests/testthat/test-probe-config.R` | Update tests for new system | 1 hour |
| `tests/testthat/test-processing-pipeline.R` | Update pipeline tests | 30 min |
| `README.md` | Add configuration section | 30 min |
| `user_inputs.md` | Document new parameters | 30 min |
| `NEWS.md` | Add release notes | 10 min |

### Files to CREATE

| File | Purpose | Estimated Time |
|------|---------|----------------|
| `R/03b_wood_properties.R` | Wood property loading system | 2-3 hours |
| `inst/configurations/probe_custom_example.yaml` | Template for users | 15 min |
| `inst/configurations/wood_custom_example.yaml` | Template for users | 15 min |
| `tests/testthat/test_probe_custom.yaml` | Test data | 10 min |
| `tests/testthat/test_wood_custom.yaml` | Test data | 10 min |
| `tests/testthat/test-wood-properties.R` | Tests for wood property system | 1 hour |
| `CONFIGURATION_GUIDE.md` | User guide | 1 hour |
| `MIGRATION_GUIDE.md` | Migration help | 30 min |

---

## Breaking Changes

### Functions Removed
- `detect_probe_config()` - Cannot reliably detect probe configuration from data
- `detect_config_from_sensors()` - Helper for broken auto-detection
- `get_standard_configs()` - Replaced by `load_probe_config()` and `list_available_probe_configs()`

### Function Signatures Changed
- `calc_heat_pulse_velocity()` - Added probe_config, wood_properties parameters
- `process_sap_data()` - Added probe_config, wood_properties parameters

### Behavior Changes
- **Default behavior:** Package now uses explicit defaults (symmetric probe, generic softwood)
- **No auto-detection:** Users must specify probe config if not using default

### Backward Compatibility
✅ **Old code will still work** if not using removed functions:
```r
# This still works - uses new defaults
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)
```

❌ **This code will break:**
```r
# This will fail - detect_probe_config removed
config <- detect_probe_config(sap_data)
```

**Migration:**
```r
# NEW: Just omit it, use default
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)

# OR: Explicitly load config
config <- load_probe_config("symmetrical")
results <- calc_heat_pulse_velocity(sap_data, probe_config = config)
```

---

## Success Criteria

### Functional Requirements
- ✅ Default workflow works without any configuration specified
- ✅ Users can load built-in configs by name
- ✅ Users can load custom YAML files by path
- ✅ Users can override individual parameters
- ✅ Users can create configs programmatically in R
- ✅ Multi-tree workflow with per-tree YAML files works
- ✅ Parameter override hierarchy works correctly

### Code Quality
- ✅ All tests pass (`devtools::test()`)
- ✅ No warnings from `devtools::check()`
- ✅ Code coverage >80% for new functions
- ✅ All functions documented with roxygen2
- ✅ Examples work and are tested

### User Experience
- ✅ Clear, helpful error messages
- ✅ Comprehensive documentation
- ✅ Migration guide available
- ✅ Example YAML templates provided
- ✅ `list_available_*()` functions work for discovery

---

## Session-by-Session Workflow

### Session 1: YAML Updates & Wood Property System (3-4 hours)
1. Update all YAML files (Phase 1)
2. Create `R/03b_wood_properties.R` with all functions (Phase 2)
3. Write tests for wood property functions
4. Test: `devtools::test_active_file()`

### Session 2: Probe Config Refactor (2-3 hours)
1. Add new functions to `R/03_probe_configuration.R` (Phase 3)
2. Comment out old functions (don't delete yet)
3. Update ProbeConfiguration R6 class
4. Test: probe config loading works

### Session 3: Update Core Functions (2-3 hours)
1. Update `calc_heat_pulse_velocity()` (Phase 4.1)
2. Update `process_sap_data()` (Phase 4.2)
3. Update other functions as needed (Phase 4.3)
4. Test: full workflow works

### Session 4: Testing & Documentation (2-3 hours)
1. Update all test files (Phase 5)
2. Run full test suite
3. Update documentation (Phase 6)
4. Create user guides

### Session 5: Cleanup & Finalization (1 hour)
1. Remove deprecated functions (Phase 7.1)
2. Final checks (Phase 7.2)
3. Update NEWS.md (Phase 7.3)
4. Commit changes

---

## Quick Reference: Key Functions to Implement

### Probe Configuration Functions (in `R/03_probe_configuration.R`)
```r
load_probe_config(config_name = "symmetrical", custom_params = NULL)
get_default_probe_config(custom_params = NULL)
list_available_probe_configs()
create_custom_probe_config(config_name, upstream_distance, downstream_distance, ...)
```

### Wood Property Functions (NEW in `R/03b_wood_properties.R`)
```r
load_wood_properties(config_name = "generic_sw", overrides = NULL, tree_overrides = NULL)
get_default_wood_properties(overrides = NULL)
list_available_wood_properties()
create_custom_wood_properties(species, thermal_diffusivity, ...)
```

---

## Risk Mitigation

### Risk: Breaking User Code
**Mitigation:**
- Keep default behavior working (uses sensible defaults automatically)
- Provide detailed migration guide
- Add helpful error messages pointing to new functions
- Test backward compatibility

### Risk: YAML Parsing Errors
**Mitigation:**
- Validate YAML structure on load
- Provide clear error messages with file path
- Include example YAMLs in package
- Test with malformed YAML files

### Risk: Parameter Confusion
**Mitigation:**
- Document parameter override hierarchy clearly
- Provide examples of each workflow
- `list_available_*()` functions for discovery
- Clear function documentation

### Risk: Test Failures
**Mitigation:**
- Update tests incrementally
- Keep old code commented until tests pass
- Run tests frequently during development
- Test each phase before moving to next

---

## Next Steps to Start Implementation

1. **Confirm this plan** - Review and approve
2. **Start with Session 1** - YAML updates and wood property system
3. **Test after each phase** - Don't move forward until tests pass
4. **Ask questions** - If anything is unclear during implementation

---

**Status:** Ready for implementation
**Start with:** Session 1 (YAML updates & wood property system)
**Estimated total time:** 10-14 hours across 5 sessions
