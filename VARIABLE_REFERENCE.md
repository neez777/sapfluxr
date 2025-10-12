# Variable Categorization Quick Reference

**Date:** 2025-10-11
**Purpose:** Quick lookup for where each variable belongs

---

## Quick Decision Tree

**Ask yourself:**
1. **Is it about the physical probe hardware?** → PROBE CONFIG
2. **Is it about the wood/tree/species?** → WOOD PROPERTIES
3. **Is it about the analysis method/settings?** → CALCULATION PARAMETERS

---

## Category 1: PROBE CONFIGURATION
**File location:** `inst/configurations/probe_*.yaml` → `probe:` section
**When it changes:** Never (fixed per probe model)
**Loaded via:** `load_probe_config()`

### Hardware Specifications
| Variable | Units | Example | Notes |
|----------|-------|---------|-------|
| heater_position | mm | 0 | Fixed at origin |
| upstream_distance | mm | 5 | Sensor distance upstream |
| downstream_distance | mm | 5 or 10 | Sensor distance downstream |
| diameter | mm | 1.27 | Physical probe diameter |
| length | mm | 35 | Total probe length |
| needle_diameter | mm | 1.27 | Needle diameter |
| inner_sensor | mm | 7.5 | Distance from probe tip |
| outer_sensor | mm | 22.5 | Distance from probe tip |
| manufacturer | - | "ICT" | Manufacturer name |
| model | - | "SFM1" | Model number |
| **heat_pulse_duration** | **seconds** | **2** | **Pulse duration** |

### Method Compatibility (also in probe config)
- `methods.compatible` - Which methods work with this probe geometry
- `methods.recommended` - Recommended methods
- `methods.priority_order` - Priority ranking

---

## Category 2: WOOD PROPERTIES
**File location:** `inst/configurations/wood_*.yaml` → `wood_property:` section
**When it changes:** Per species, site, or individual tree
**Loaded via:** `load_wood_properties()`
**Default:** `wood_generic_sw.yaml`

### Thermal Properties (CRITICAL for calculations)
| Variable | Units | Typical Range | Notes |
|----------|-------|---------------|-------|
| **thermal_diffusivity** | **cm²/s** | **0.002-0.004** | **Most important parameter** |
| thermal_conductivity | W/(m·K) | 0.1-0.5 | Can be estimated |
| volumetric_heat_capacity | J/(m³·K) | 1.5e6-2.5e6 | Can be estimated |

### Physical Properties
| Variable | Units | Typical Range | Notes |
|----------|-------|---------------|-------|
| dry_density | kg/m³ | 300-800 | Oven-dry density |
| basic_density | kg/m³ | 250-650 | Dry mass / green volume |
| moisture_content | % | 20-50 | % on dry weight basis |

### Classification
| Variable | Type | Examples |
|----------|------|----------|
| species | text | "Eucalyptus spp.", "Pinus radiata" |
| wood_type | text | "softwood", "hardwood" |
| temperature | °C | Typical: 15-30 |

### Tree Measurements (optional in YAML, overridable)
**File location:** `wood_property:` → `tree_measurements:` section

| Variable | Units | Purpose |
|----------|-------|---------|
| dbh | cm | Diameter at breast height |
| bark_thickness | cm | For sapwood calculations |
| sapwood_depth | cm | For flux scaling |
| sapwood_area | cm² | For tree-level water use |
| heartwood_radius | cm | Optional |

### Quality Thresholds (species defaults)
**File location:** `wood_property:` → `quality_thresholds:` section

| Variable | Units | Default | Purpose |
|----------|-------|---------|---------|
| max_velocity_cm_hr | cm/hr | 200 | Upper QC bound |
| min_velocity_cm_hr | cm/hr | -50 | Lower QC bound (allows reverse) |
| temperature_range | °C | [-10, 60] | Acceptable temp range |

---

## Category 3: CALCULATION PARAMETERS
**File location:** R function parameters (NOT in YAML)
**When it changes:** Per analysis, per user preference
**Passed via:** `parameters` list in functions

### Sampling Windows
| Variable | Units | Default | Used By |
|----------|-------|---------|---------|
| pre_pulse | seconds | 30 | All methods |
| HRM_start | seconds | 60 | HRM, MHR, DMA |
| HRM_end | seconds | 100 | HRM, MHR, DMA |

### Method-Specific Parameters
| Variable | Units | Default | Used By |
|----------|-------|---------|---------|
| L | proportion | 0.5 | HRMXa, HRMXb |
| H | proportion | 0.8 | HRMXa, HRMXb |

### Quality Control (analysis-specific overrides)
| Variable | Units | Default | Purpose |
|----------|-------|---------|---------|
| min_signal_ratio | - | 3.0 | Signal-to-noise threshold |

**Note:** Quality thresholds can be overridden from wood properties defaults

---

## Special Cases & Moved Variables

### ✅ Moved: heat_pulse_duration (tp_1)
- **Was:** Calculation parameter
- **Now:** Probe configuration (hardware-dependent)
- **Can still be overridden** via `parameters` list if needed

### ✅ Decided: thermal_diffusivity
- **Category:** Wood properties (species-dependent)
- **NOT:** Probe configuration
- **Rationale:** Varies by wood species, moisture, temperature

### ✅ Decided: Quality thresholds
- **Primary location:** Wood properties (species defaults)
- **Can be overridden:** Via calculation parameters
- **Rationale:** Different species have different typical ranges

---

## Parameter Override Hierarchy

**Priority (highest to lowest):**

1. 🥇 **Individual function parameters**
   ```r
   calc_heat_pulse_velocity(..., diffusivity = 0.003)
   ```

2. 🥈 **`parameters` list**
   ```r
   calc_heat_pulse_velocity(..., parameters = list(diffusivity = 0.003))
   ```

3. 🥉 **Configuration objects**
   ```r
   calc_heat_pulse_velocity(..., wood_properties = "eucalyptus")  # Has diffusivity = 0.00143
   ```

4. 4️⃣ **Hardcoded defaults**
   ```r
   # If nothing specified, uses function defaults
   ```

---

## Common Questions

### Q: Where does `x` (probe spacing) come from?
**A:** Derived from probe configuration (upstream_distance + downstream_distance) / 2 / 10
**Can override:** Yes, via `x = 0.6` parameter in function call

### Q: Where does `diffusivity` come from?
**A:** From wood properties YAML (species-specific)
**Can override:** Yes, via `diffusivity = 0.003` parameter or `parameters` list

### Q: Where does `tp_1` (heat pulse duration) come from?
**A:** From probe configuration YAML (hardware-specific)
**Can override:** Yes, via `parameters = list(tp_1 = 3)` if needed

### Q: Can I specify DBH in the wood YAML or function parameter?
**A:** Both! Optional in YAML, always overridable via function parameter
**Use YAML for:** Typical values for multi-tree studies
**Use function param for:** Individual tree measurements

### Q: What if I don't specify any configurations?
**A:** Defaults used:
- Probe: `probe_symmetrical.yaml` (ICT SFM1x standard)
- Wood: `wood_generic_sw.yaml` (generic softwood)
- Calculation params: Hardcoded defaults in function

---

## YAML File Structure Examples

### Probe Configuration YAML
```yaml
metadata:
  config_name: "ICT SFM1x Symmetric"
  description: "Standard symmetric probe"
  default: true

probe:
  heater_position: 0
  upstream_distance: 5
  downstream_distance: 5
  diameter: 1.27
  length: 35
  needle_diameter: 1.27
  inner_sensor: 7.5
  outer_sensor: 22.5
  manufacturer: "ICT"
  model: "SFM1"
  heat_pulse_duration: 2        # NEW!

methods:
  compatible: [HRM, MHR, DMA, Tmax_Coh, Tmax_Klu]
  recommended: [HRM, DMA, MHR]
  priority_order: [HRM, DMA, MHR]
```

### Wood Properties YAML
```yaml
metadata:
  config_name: "Generic Softwood"
  description: "Default softwood properties"
  default: true

wood_property:
  thermal_diffusivity: 0.0025
  thermal_conductivity: null
  volumetric_heat_capacity: null
  dry_density: 400
  basic_density: 350
  moisture_content: 30
  species: "unknown"
  wood_type: "softwood"
  temperature: 20

tree_measurements:                # NEW! Optional
  dbh: null
  bark_thickness: null
  sapwood_depth: null
  sapwood_area: null
  heartwood_radius: null

quality_thresholds:               # NEW!
  max_velocity_cm_hr: 200
  min_velocity_cm_hr: -50
  temperature_range: [-10, 60]
```

---

## Function Signatures (After Refactor)

### calc_heat_pulse_velocity()
```r
calc_heat_pulse_velocity(
  sap_data,
  pulse_ids = NULL,
  methods = c("HRM", "MHR", "DMA"),

  # Configuration system
  probe_config = NULL,           # Name, path, or object
  wood_properties = NULL,        # Name, path, or object

  # Parameters
  parameters = NULL,             # List of calc params

  # Individual overrides
  diffusivity = NULL,            # Override wood property
  x = NULL,                      # Override probe spacing

  plot_results = FALSE
)
```

### load_probe_config()
```r
load_probe_config(
  config_name = "symmetrical",   # Name or path
  custom_params = NULL           # List of overrides
)
```

### load_wood_properties()
```r
load_wood_properties(
  config_name = "generic_sw",    # Name or path
  overrides = NULL,              # Wood property overrides
  tree_overrides = NULL          # Tree measurement overrides
)
```

---

**Quick Reference Complete!**
See IMPLEMENTATION_PLAN.md for full implementation details.
