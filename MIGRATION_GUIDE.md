# Migration Guide: Configuration System Update

## Overview

This guide helps you migrate from the old automatic probe detection system to the new YAML-based configuration system in sapFluxR.

**Key Change**: Probe configuration is now **explicitly specified** rather than automatically detected, providing more control and reliability.

---

## Summary of Changes

### What's Changed

| Aspect | Old System (< v2.0) | New System (>= v2.0) |
|--------|---------------------|----------------------|
| **Probe detection** | Automatic from sensor data | Explicit specification or defaults |
| **Configuration source** | Hard-coded in R | YAML files + programmatic creation |
| **Default behavior** | Attempted auto-detection | Uses sensible defaults (symmetric probe) |
| **Wood properties** | Mixed with probe config | Separate configuration layer |
| **Customization** | Limited | Full flexibility with overrides |
| **Reproducibility** | Uncertain (detection could vary) | Guaranteed (explicit config) |

### What's New

✅ **YAML-based configurations** - Easy to read, edit, and version control
✅ **Separate probe and wood properties** - Mix and match configurations
✅ **Built-in species libraries** - Eucalyptus, Pine, generic softwood
✅ **Parameter override hierarchy** - Multiple levels of customization
✅ **Programmatic configuration** - Create configs in R code
✅ **Better documentation** - Each config file is self-documenting

### What Still Works

✅ **All calculation methods** - HRM, MHR, CHPM, Tmax, DMA, etc.
✅ **Data import** - No changes to `read_sap_data()`
✅ **Processing pipeline** - `process_sap_data()` updated but compatible
✅ **Visualization** - All plotting functions unchanged

---

## Quick Migration Checklist

### For Most Users (Using Defaults)

If you were using `calc_heat_pulse_velocity()` or `process_sap_data()` without explicit configuration:

**Old Code:**
```r
results <- calc_heat_pulse_velocity(sap_data)
```

**New Code (same!):**
```r
results <- calc_heat_pulse_velocity(sap_data)
# Now uses explicit defaults: symmetrical probe + generic softwood
```

✅ **No action required** - Your code will continue to work with sensible defaults.

### For Advanced Users (Custom Configurations)

If you were:
- Manually specifying probe configurations
- Using custom parameter values
- Working with specific species

📋 **Action required** - Follow the detailed migration steps below.

---

## Detailed Migration Steps

### Step 1: Update Function Calls

#### calc_heat_pulse_velocity()

**Old:**
```r
# Old: relied on automatic detection
results <- calc_heat_pulse_velocity(sap_data, methods = "HRM")

# Old: manually created config object
config <- ProbeConfiguration$new(
  config_name = "custom",
  config_type = "symmetric",
  sensor_positions = list(upstream = -5, downstream = 5),
  compatible_methods = c("HRM", "MHR")
)
results <- calc_heat_pulse_velocity(sap_data, config = config)
```

**New:**
```r
# New: use built-in configuration by name
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "symmetrical",
                                    methods = "HRM")

# New: or create programmatically with helper function
probe <- create_custom_probe_config(
  upstream_distance = 5,
  downstream_distance = 5
)
results <- calc_heat_pulse_velocity(sap_data, probe_config = probe)

# New: specify wood properties separately
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "symmetrical",
                                    wood_properties = "eucalyptus",
                                    methods = "HRM")
```

#### process_sap_data()

**Old:**
```r
# Old: automatic detection
processed <- process_sap_data(sap_data)

# Old: with parameters
processed <- process_sap_data(sap_data,
                              parameters = list(diffusivity = 0.0025))
```

**New:**
```r
# New: explicit configuration (or use defaults)
processed <- process_sap_data(sap_data,
                              probe_config = "symmetrical",      # explicit
                              wood_properties = "generic_sw")    # explicit

# New: with parameters still works
processed <- process_sap_data(sap_data,
                              probe_config = "symmetrical",
                              wood_properties = "eucalyptus",
                              parameters = list(HRM_start = 70))

# New: individual parameter override
processed <- process_sap_data(sap_data,
                              wood_properties = "eucalyptus",
                              diffusivity = 0.0015)  # override species value
```

### Step 2: Replace Custom Configurations

If you created custom `ProbeConfiguration` objects manually:

**Old:**
```r
custom_config <- ProbeConfiguration$new(
  config_name = "My Custom Probe",
  config_type = "asymmetric",
  sensor_positions = list(
    upstream_inner = -6,
    downstream_inner = 8,
    upstream_outer = -6,
    downstream_outer = 8
  ),
  probe_diameter = 1.5,
  thermal_diffusivity = 0.0025,
  compatible_methods = c("HRM", "MHR", "CHPM")
)
```

**New Option 1: Use Helper Function**
```r
custom_probe <- create_custom_probe_config(
  config_name = "My Custom Probe",
  upstream_distance = 6,      # mm
  downstream_distance = 8,    # mm
  probe_diameter = 1.5,       # mm
  heat_pulse_duration = 2,
  compatible_methods = c("HRM", "MHR", "CHPM")
)

# thermal_diffusivity now in wood properties
custom_wood <- load_wood_properties("generic_sw",
                                   overrides = list(thermal_diffusivity = 0.0025))

results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = custom_probe,
                                    wood_properties = custom_wood)
```

**New Option 2: Create YAML File**
```yaml
# inst/configurations/probe_custom.yaml
metadata:
  config_name: My Custom Probe

probe:
  upstream_distance: 6
  downstream_distance: 8
  diameter: 1.5
  heat_pulse_duration: 2

methods:
  compatible: [HRM, MHR, CHPM]
```

```r
# Load and use
custom_probe <- load_probe_config("custom")
custom_wood <- load_wood_properties("generic_sw",
                                   overrides = list(thermal_diffusivity = 0.0025))

results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = custom_probe,
                                    wood_properties = custom_wood)
```

### Step 3: Separate Probe and Wood Properties

The new system separates probe hardware from wood properties.

**Old (mixed together):**
```r
config <- list(
  probe_spacing = 0.5,         # cm
  diffusivity = 0.00143,       # cm²/s
  density = 700,               # kg/m³
  moisture_content = 45        # %
)
results <- calc_heat_pulse_velocity(sap_data, parameters = config)
```

**New (separated):**
```r
# Probe hardware
probe <- load_probe_config("symmetrical")  # 0.5 cm spacing

# Wood properties
wood <- load_wood_properties("eucalyptus")  # diffusivity, density, moisture

# Use together
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = probe,
                                    wood_properties = wood)

# Or override individual properties
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "symmetrical",
                                    wood_properties = "eucalyptus",
                                    diffusivity = 0.0015)  # measured value
```

---

## Migration Examples

### Example 1: Basic Analysis (Softwood)

**Old Code:**
```r
library(sapFluxR)

sap_data <- read_sap_data("tree01.csv")
results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR"))
processed <- process_sap_data(sap_data)
```

**New Code (no changes needed!):**
```r
library(sapFluxR)

sap_data <- read_sap_data("tree01.csv")
results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR"))
processed <- process_sap_data(sap_data)

# Behind the scenes: uses symmetrical probe + generic_sw wood
```

**Action**: ✅ None - code works as-is with improved reliability

---

### Example 2: Custom Diffusivity Value

**Old Code:**
```r
results <- calc_heat_pulse_velocity(sap_data,
                                    parameters = list(diffusivity = 0.00143))
```

**New Code:**
```r
# Option 1: Use eucalyptus preset (diffusivity = 0.00143)
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus")

# Option 2: Override generic_sw default
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "generic_sw",
                                    diffusivity = 0.00143)

# Option 3: Create custom wood properties
wood <- create_custom_wood_properties(
  species = "My Species",
  thermal_diffusivity = 0.00143
)
results <- calc_heat_pulse_velocity(sap_data, wood_properties = wood)
```

**Action**: 📋 Replace with wood properties configuration or individual override

---

### Example 3: Custom Probe Spacing

**Old Code:**
```r
# Manual configuration with custom spacing
config <- ProbeConfiguration$new(
  config_name = "custom",
  config_type = "symmetric",
  sensor_positions = list(upstream_inner = -7, downstream_inner = 7,
                         upstream_outer = -7, downstream_outer = 7),
  compatible_methods = c("HRM", "MHR")
)

results <- calc_heat_pulse_velocity(sap_data, config = config)
```

**New Code:**
```r
# Use helper function (much simpler!)
probe <- create_custom_probe_config(
  config_name = "Custom 7mm Probe",
  upstream_distance = 7,    # mm
  downstream_distance = 7,   # mm
  compatible_methods = c("HRM", "MHR")
)

results <- calc_heat_pulse_velocity(sap_data, probe_config = probe)
```

**Action**: 📋 Replace manual `ProbeConfiguration$new()` with `create_custom_probe_config()`

---

### Example 4: Species-Specific Analysis (Eucalyptus)

**Old Code:**
```r
# Had to manually specify all eucalyptus parameters
params <- list(
  diffusivity = 0.00143,
  density = 700,
  moisture_content = 45,
  HRM_start = 60,
  HRM_end = 100
)

results <- calc_heat_pulse_velocity(sap_data, parameters = params)
```

**New Code:**
```r
# Use built-in eucalyptus configuration
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus",
                                    parameters = list(
                                      HRM_start = 60,
                                      HRM_end = 100
                                    ))

# Or with tree-specific measurements
wood <- load_wood_properties("eucalyptus",
                             tree_overrides = list(
                               dbh = 42.5,
                               sapwood_depth = 4.2
                             ))

results <- calc_heat_pulse_velocity(sap_data, wood_properties = wood)
```

**Action**: 📋 Replace manual parameters with wood properties configuration

---

### Example 5: CHPM Method (Asymmetric Probe)

**Old Code:**
```r
# Had to create asymmetric configuration manually
config <- ProbeConfiguration$new(
  config_name = "asymmetric",
  config_type = "asymmetric",
  sensor_positions = list(
    upstream_inner = -5,
    downstream_inner = 10,
    upstream_outer = -5,
    downstream_outer = 10
  ),
  compatible_methods = "CHPM"
)

results <- calc_heat_pulse_velocity(sap_data, config = config, methods = "CHPM")
```

**New Code:**
```r
# Use built-in asymmetric configuration (much simpler!)
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "asymmetrical",
                                    methods = "CHPM")

# With custom wood properties
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "asymmetrical",
                                    wood_properties = "eucalyptus",
                                    methods = "CHPM")
```

**Action**: 📋 Replace manual asymmetric config with `"asymmetrical"` preset

---

## Common Migration Scenarios

### Scenario 1: "I never specified any configuration"

**Before:**
```r
results <- calc_heat_pulse_velocity(sap_data)
```

**After:**
```r
results <- calc_heat_pulse_velocity(sap_data)
# No change needed! Now uses explicit defaults.
```

**Impact**: ✅ No code changes, improved reliability

---

### Scenario 2: "I only specified diffusivity"

**Before:**
```r
results <- calc_heat_pulse_velocity(sap_data,
                                    parameters = list(diffusivity = 0.00143))
```

**After:**
```r
# Option A: Use species preset if it matches
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus")  # has 0.00143

# Option B: Override default
results <- calc_heat_pulse_velocity(sap_data,
                                    diffusivity = 0.00143)
```

**Impact**: 📋 Minor change, clearer intent

---

### Scenario 3: "I created custom ProbeConfiguration objects"

**Before:**
```r
my_probe <- ProbeConfiguration$new(...)  # Long manual setup
results <- calc_heat_pulse_velocity(sap_data, config = my_probe)
```

**After:**
```r
# Much simpler with helper
my_probe <- create_custom_probe_config(
  upstream_distance = 6,
  downstream_distance = 6
)
results <- calc_heat_pulse_velocity(sap_data, probe_config = my_probe)
```

**Impact**: 📋 Moderate change, much simpler

---

### Scenario 4: "I used species-specific parameters"

**Before:**
```r
euc_params <- list(
  diffusivity = 0.00143,
  density = 700,
  moisture_content = 45,
  max_velocity = 150
)
results <- calc_heat_pulse_velocity(sap_data, parameters = euc_params)
```

**After:**
```r
# All eucalyptus properties are pre-configured!
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus")

# Or customize further
wood <- load_wood_properties("eucalyptus",
                             overrides = list(moisture_content = 50))
results <- calc_heat_pulse_velocity(sap_data, wood_properties = wood)
```

**Impact**: 📋 Major improvement, cleaner code

---

## Removed Functions

**IMPORTANT**: The following functions have been completely removed (not deprecated):

| Function | Status | Replacement |
|----------|--------|-------------|
| `detect_probe_config()` | ❌ Removed | `load_probe_config()` or `get_default_probe_config()` |
| `detect_config_from_sensors()` | ❌ Removed | `load_probe_config()` |
| `get_standard_configs()` | ❌ Removed | `list_available_probe_configs()` |

**Old approach (detection) - NO LONGER WORKS:**
```r
config <- detect_probe_config(sap_data)  # ERROR: function not found
```

**New approach (specification) - REQUIRED:**
```r
# Specify what you're using
config <- load_probe_config("symmetrical")

# Or use default
config <- get_default_probe_config()
```

**Why removed instead of deprecated**: Since this package is pre-release with no current users, deprecated functions were removed entirely to avoid technical debt.

---

## Troubleshooting Migration Issues

### Issue 1: "My custom configuration doesn't work"

**Error:**
```r
Error: Probe configuration 'my_old_config' not found
```

**Solution:**
Create a new configuration using the helper function or YAML file:

```r
# Convert old manual config to new helper
new_config <- create_custom_probe_config(
  config_name = "my_config",
  upstream_distance = 5,      # Check your old values
  downstream_distance = 5,
  heat_pulse_duration = 2
)
```

### Issue 2: "diffusivity parameter not working"

**Error:**
```r
Warning: thermal_diffusivity not found in parameters
```

**Solution:**
Use `diffusivity` as individual parameter or specify via `wood_properties`:

```r
# Correct: individual parameter
results <- calc_heat_pulse_velocity(sap_data, diffusivity = 0.00143)

# Or: via wood properties
results <- calc_heat_pulse_velocity(sap_data, wood_properties = "eucalyptus")
```

### Issue 3: "Missing thermal_diffusivity"

**Error:**
```r
Error: thermal_diffusivity is required but not provided
```

**Solution:**
Always specify wood properties (or it uses default):

```r
# Explicit specification
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "generic_sw")

# Or override individually
results <- calc_heat_pulse_velocity(sap_data,
                                    diffusivity = 0.0025)
```

### Issue 4: "Probe detection warning"

**Warning:**
```r
Warning: probe detection is deprecated, using defaults
```

**Solution:**
Explicitly specify probe configuration:

```r
# Replace automatic detection
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "symmetrical")
```

---

## Benefits of the New System

### 1. Reproducibility

**Old:** Detection could vary based on data quality
```r
# Results might differ due to detection uncertainty
config1 <- detect_probe_config(sap_data_clean)
config2 <- detect_probe_config(sap_data_noisy)  # Might detect differently!
```

**New:** Explicit configuration guarantees consistency
```r
# Always uses the same configuration
results1 <- calc_heat_pulse_velocity(sap_data_clean,
                                     probe_config = "symmetrical")
results2 <- calc_heat_pulse_velocity(sap_data_noisy,
                                     probe_config = "symmetrical")  # Same config!
```

### 2. Clarity

**Old:** Unclear what configuration was used
```r
results <- calc_heat_pulse_velocity(sap_data)  # What probe? What diffusivity?
```

**New:** Explicit and self-documenting
```r
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "symmetrical",    # ICT standard
                                    wood_properties = "eucalyptus")  # E. globulus
```

### 3. Version Control

**Old:** Configuration buried in R code
```r
config <- ProbeConfiguration$new(
  config_name = "custom",
  sensor_positions = list(...),  # Hard to track changes
  ...
)
```

**New:** YAML files easy to track and diff
```yaml
# probe_custom.yaml - trackable in git!
metadata:
  version: '1.2'
  modified: '2025-01-15'
probe:
  upstream_distance: 6  # Changed from 5 on 2025-01-15
```

### 4. Species Libraries

**Old:** Manual parameter lookup and entry
```r
# Had to find and manually enter eucalyptus values
params <- list(diffusivity = 0.00143, density = 700, moisture = 45, ...)
```

**New:** Pre-configured species libraries
```r
# One line gets all eucalyptus properties!
results <- calc_heat_pulse_velocity(sap_data, wood_properties = "eucalyptus")
```

---

## Getting Help

If you encounter issues during migration:

1. **Check this guide** - Common scenarios covered above
2. **Review Configuration Guide** - See `CONFIGURATION_GUIDE.md` for detailed examples
3. **Check function documentation**: `?calc_heat_pulse_velocity`, `?load_probe_config`
4. **Open an issue** - Provide reproducible example on GitHub

---

## Summary

✅ **Most users**: No code changes needed - defaults now explicit and reliable
📋 **Advanced users**: Replace manual configs with helper functions or YAML files
🎯 **Benefits**: Better reproducibility, clarity, and maintainability

The new system provides more control while maintaining backward compatibility for simple use cases.
