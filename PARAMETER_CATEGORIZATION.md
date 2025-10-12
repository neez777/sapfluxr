# Parameter Categorization Guide

**Date:** 2025-10-11
**Purpose:** Clarify what parameters belong in which configuration category

---

## Overview

The sapFluxR package uses three distinct types of parameters that should be kept separate:

1. **Probe Configuration** - Physical hardware specifications (fixed per probe type)
2. **Wood Properties** - Species/tree-specific thermal and physical properties
3. **Calculation Parameters** - Method-specific analysis settings

---

## 1. Probe Configuration Parameters

**What:** Physical specifications of the heat pulse probe hardware
**Source:** Manufacturer specifications, probe datasheets
**Frequency of change:** Never (fixed per probe model)
**YAML file:** `inst/configurations/probe_*.yaml`

### Parameters:

| Parameter | Description | Units | Example |
|-----------|-------------|-------|---------|
| `heater_position` | Position of heater element | mm | 0 (origin) |
| `upstream_distance` | Distance from heater to upstream sensor | mm | 5 |
| `downstream_distance` | Distance from heater to downstream sensor | mm | 5 |
| `probe_diameter` | Physical diameter of probe | mm | 1.27 |
| `probe_length` | Total length of probe | mm | 35 |
| `needle_diameter` | Diameter of needle | mm | 1.27 |
| `inner_sensor` | Distance from probe tip to inner sensor | mm | 7.5 |
| `outer_sensor` | Distance from probe tip to outer sensor | mm | 22.5 |
| `manufacturer` | Probe manufacturer | - | "ICT International" |
| `model` | Probe model number | - | "SFM1x" |

### Derived Probe Parameters:

These are calculated from the above:

| Parameter | Description | Calculation | Units |
|-----------|-------------|-------------|-------|
| `x` | Probe spacing for calculations | `(upstream_distance + downstream_distance) / 2 / 10` | cm |
| `config_type` | Configuration type | "symmetric" if upstream == downstream | - |

### Method Compatibility:

Also stored in probe config (which methods work with this probe geometry):

- `compatible_methods`: List of compatible calculation methods
- `recommended_methods`: Recommended methods for this probe
- `priority_order`: Priority ranking of methods

---

## 2. Wood Properties Parameters

**What:** Thermal and physical properties of the wood/tree
**Source:** Literature values, species-specific measurements
**Frequency of change:** Changes per species/tree/site conditions
**YAML file:** `inst/configurations/wood_*.yaml`

### Parameters:

| Parameter | Description | Units | Typical Range |
|-----------|-------------|-------|---------------|
| `thermal_diffusivity` | Thermal diffusivity of sapwood | cm²/s | 0.002-0.004 |
| `thermal_conductivity` | Thermal conductivity | W/(m·K) | 0.1-0.5 |
| `volumetric_heat_capacity` | Volumetric heat capacity | J/(m³·K) | 1.5e6-2.5e6 |
| `dry_density` | Oven-dry wood density | kg/m³ | 300-800 |
| `basic_density` | Basic density (oven-dry mass / green volume) | kg/m³ | 250-650 |
| `moisture_content` | Wood moisture content | % | 20-50 |
| `sapwood_water_content` | Volumetric water content | 0-1 | 0.3-0.6 |
| `species` | Tree species | - | "Eucalyptus grandis" |
| `wood_type` | Wood classification | - | "hardwood"/"softwood" |
| `temperature` | Typical wood temperature | °C | 15-30 |

### Tree-Specific Measurements:

Also in wood properties (but per-tree, not per-species):

| Parameter | Description | Units | Example |
|-----------|-------------|-------|---------|
| `dbh` | Diameter at breast height | cm | 45 |
| `bark_thickness` | Bark thickness | cm | 1.5 |
| `sapwood_depth` | Sapwood depth | cm | 3.0 |
| `heartwood_radius` | Heartwood radius | cm | 10 |
| `sapwood_area` | Conducting sapwood area | cm² | 150 |

### Important Relationships:

**Thermal diffusivity can be:**
1. Measured directly
2. Estimated from wood density and moisture
3. Estimated in real-time from temperature data

Formula: `thermal_diffusivity = thermal_conductivity / volumetric_heat_capacity`

---

## 3. Calculation Parameters

**What:** Method-specific analysis settings and windows
**Source:** Method papers, user preferences, data characteristics
**Frequency of change:** Can be tuned per analysis, per pulse
**Storage:** Function parameters (not YAML, or optional advanced YAML)

### Parameters:

| Parameter | Description | Units | Default | Used By |
|-----------|-------------|-------|---------|---------|
| `pre_pulse` | Pre-pulse baseline period | seconds | 30 | All methods |
| `HRM_start` | Start of HRM sampling window | seconds | 60 | HRM, MHR, DMA |
| `HRM_end` | End of HRM sampling window | seconds | 100 | HRM, MHR, DMA |
| `L` | Lower proportion of deltaTmax for HRMX | proportion | 0.5 | HRMXa, HRMXb |
| `H` | Higher proportion of deltaTmax for HRMX | proportion | 0.8 | HRMXa, HRMXb |
| `tp_1` | Heat pulse duration | seconds | 2 | Tmax_Klu, DMA |
| `min_signal_ratio` | Minimum acceptable signal-to-noise ratio | - | 3 | Quality control |
| `max_velocity` | Maximum reasonable velocity | cm/hr | 200 | Quality control |
| `min_velocity` | Minimum reasonable velocity | cm/hr | -50 | Quality control |

### Quality Control Thresholds:

These could be in wood properties OR calculation parameters:

| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `temperature_range` | Acceptable temperature range | °C | [-10, 60] |
| `voltage_range` | Acceptable voltage range | V | [0, 30] |

---

## Correct Parameter Assignment

### ✅ Probe Configuration (Hardware)

```yaml
# inst/configurations/probe_symmetrical.yaml
metadata:
  config_name: "ICT SFM1x Symmetric"
  manufacturer: "ICT International"
  model: "SFM1x"
  default: true

probe:
  # Physical hardware specs only
  heater_position: 0           # mm
  upstream_distance: 5         # mm
  downstream_distance: 5       # mm
  diameter: 1.27              # mm
  length: 35                  # mm
  inner_sensor: 7.5           # mm
  outer_sensor: 22.5          # mm

methods:
  # Which methods work with THIS PROBE GEOMETRY
  compatible: [HRM, MHR, DMA, Tmax_Coh, Tmax_Klu, HRMx]
  recommended: [HRM, DMA, MHR]
  priority_order: [HRM, DMA, MHR, Tmax_Coh]
```

### ✅ Wood Properties (Tree/Species)

```yaml
# inst/configurations/wood_eucalyptus.yaml
metadata:
  config_name: "Eucalyptus Wood Properties"
  species: "Eucalyptus grandis"
  region: "Temperate"

wood_property:
  # Thermal properties
  thermal_diffusivity: 0.00143    # cm²/s
  thermal_conductivity: 0.27      # W/(m·K)
  volumetric_heat_capacity: 1886000  # J/(m³·K)

  # Physical properties
  dry_density: 550              # kg/m³
  basic_density: 480            # kg/m³
  moisture_content: 25          # %
  sapwood_water_content: 0.45   # volumetric (0-1)

  # Classification
  wood_type: "hardwood"

  # Tree dimensions (optional - can override per tree)
  typical_sapwood_depth: 3.0    # cm
  typical_bark_thickness: 1.2   # cm

scaling:
  # For flux density calculations
  sapwood_density: 1000         # kg/m³ (fresh sapwood)
  correction_factor: 1.0        # species-specific correction

quality:
  # Species-specific quality thresholds
  typical_velocity_range: [0, 150]  # cm/hr
  typical_temperature: [15, 35]     # °C
```

### ✅ Calculation Parameters (Analysis Settings)

```r
# In R code - passed as function parameters
calc_params <- list(
  # Sampling windows
  pre_pulse = 30,
  HRM_start = 60,
  HRM_end = 100,

  # Method-specific
  L = 0.5,
  H = 0.8,
  tp_1 = 2,

  # Quality thresholds (could also be in wood properties)
  min_signal_ratio = 3,
  max_velocity = 200,
  min_velocity = -50
)

results <- calc_heat_pulse_velocity(
  sap_data,
  methods = c("HRM", "MHR"),
  parameters = calc_params
)
```

---

## What Was Wrong in Original Plan

### ❌ Incorrectly Placed in Probe Config:

```yaml
# WRONG - these are wood properties, not probe specs!
thermal:
  diffusivity: 0.0025         # This is WOOD property
  diffusivity_range: [0.002, 0.004]
  estimate_from_data: false

quality:
  min_signal_ratio: 3.0       # This is calculation parameter
  max_velocity_cm_hr: 200
```

**Why wrong:**
- `diffusivity` depends on wood species and moisture, not probe hardware
- Quality thresholds are analysis choices, not probe specifications

---

## Proposed Architecture

### 1. Probe Configuration System

```r
# Load probe configuration (hardware specs)
probe_config <- load_probe_config("symmetrical")
# or
probe_config <- load_probe_config("path/to/custom_probe.yaml")

# List available probe configs
list_available_probe_configs()

# Create custom probe in R
probe_config <- create_custom_probe_config(
  config_name = "Custom Probe",
  upstream_distance = 6,    # mm
  downstream_distance = 6   # mm
)
```

### 2. Wood Properties System (NEW - to be implemented)

```r
# Load wood properties
wood_props <- load_wood_properties("eucalyptus")
# or
wood_props <- load_wood_properties("path/to/custom_wood.yaml")

# List available wood property configs
list_available_wood_properties()

# Override specific properties
wood_props <- load_wood_properties(
  "eucalyptus",
  overrides = list(
    thermal_diffusivity = 0.0028,
    moisture_content = 30
  )
)

# Create custom wood properties in R
wood_props <- create_custom_wood_properties(
  species = "Pinus radiata",
  thermal_diffusivity = 0.0025,
  dry_density = 450,
  moisture_content = 28
)
```

### 3. Tree-Specific Measurements (Per-Tree)

```r
# Individual tree measurements
tree_data <- list(
  tree_id = "Tree_01",
  dbh = 45.2,              # cm
  bark_thickness = 1.5,    # cm
  sapwood_depth = 3.2,     # cm
  # ... measured on this specific tree
)
```

### 4. Combined Usage in Pipeline

```r
# Complete workflow with all three configuration types
results <- process_sap_data(
  sap_data,

  # 1. Probe configuration (hardware)
  probe_config = "symmetrical",  # or load_probe_config()

  # 2. Wood properties (species/tree)
  wood_properties = "eucalyptus",  # or load_wood_properties()

  # 3. Tree measurements (individual)
  dbh = 45.2,
  sapwood_depth = 3.2,

  # 4. Calculation parameters (analysis settings)
  parameters = list(
    pre_pulse = 30,
    HRM_start = 60,
    HRM_end = 100
  ),

  # 5. Methods to use
  methods = c("HRM", "MHR", "DMA")
)
```

---

## Function Signature Updates

### Current (Mixed Up):

```r
calc_heat_pulse_velocity(sap_data,
                         methods = c("HRM", "MHR"),
                         parameters = NULL)  # Everything mixed together!
```

### Proposed (Clear Separation):

```r
calc_heat_pulse_velocity(
  sap_data,

  # Probe hardware
  probe_config = NULL,        # ProbeConfiguration object, name, or path

  # Wood properties
  wood_properties = NULL,     # WoodProperties object, name, or path

  # Calculation settings
  parameters = NULL,          # Method-specific parameters
  methods = c("HRM", "MHR"),

  # Individual overrides
  diffusivity = NULL,         # Override wood property
  x = NULL                    # Override probe spacing (rare)
)
```

---

## Default Behavior

If user doesn't specify configurations, use sensible defaults:

```r
# This should work out-of-the-box
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)

# Defaults used:
# - probe_config: "symmetrical" (ICT SFM1x standard)
# - wood_properties: "generic_softwood" (temperate softwood)
# - parameters: get_default_parameters()
```

---

## Implementation Priority

1. **Phase 1:** Probe Configuration System (hardware specs only)
2. **Phase 2:** Wood Properties System (thermal/physical properties)
3. **Phase 3:** Unified workflow with clear separation

---

## Questions for Clarification

1. **Thermal diffusivity:** Should this be:
   - In wood properties? ✅ (species-dependent)
   - User-adjustable per analysis? ✅ (via override)
   - Estimated from data? ✅ (optional feature)

2. **Quality thresholds:** Should these be:
   - In wood properties? (species-specific ranges)
   - In calculation parameters? (analysis-specific choices)
   - Both? ✅ (wood props for defaults, calc params for overrides)

3. **Tree measurements (DBH, sapwood area):** Should these be:
   - In wood properties YAML? (if typical values)
   - Function parameters? ✅ (measured per tree)
   - Separate tree_data object? (for multi-tree studies)

---

## Next Steps

1. ✅ Clarify parameter categories (this document)
2. ⬜ Refactor probe config system (hardware only)
3. ⬜ Create wood properties system (species/thermal)
4. ⬜ Update function signatures
5. ⬜ Update documentation

---

**Status:** Ready for review - please confirm categorization is correct!
