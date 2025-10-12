# Configuration System Guide

## Overview

sapFluxR uses a flexible, YAML-based configuration system to manage probe hardware specifications and wood/species properties. This guide explains how to work with configurations effectively.

## Architecture

The configuration system has three distinct layers:

1. **Probe Configuration** - Hardware specifications (probe spacing, geometry, heat pulse duration)
2. **Wood Properties** - Species-specific thermal and physical properties
3. **Calculation Parameters** - Analysis settings (time windows, thresholds)

This separation allows you to:
- Mix and match probe hardware with different wood species
- Override specific parameters without modifying YAML files
- Create custom configurations programmatically
- Share and version control your configurations

---

## Probe Configuration

### Built-in Configurations

sapFluxR includes two standard probe configurations:

| Configuration | Description | Use Case |
|---------------|-------------|----------|
| **symmetrical** | ICT SFM1x standard (5mm upstream, 5mm downstream) | HRM, MHR, DMA, Tmax methods |
| **asymmetrical** | CHPM optimized (5mm upstream, 10mm downstream) | CHPM method specifically |

### Loading Probe Configurations

#### Use Default Configuration

```r
# Loads symmetrical configuration automatically
results <- calc_heat_pulse_velocity(sap_data)
```

#### Specify Configuration by Name

```r
# Load specific configuration
config <- load_probe_config("symmetrical")
results <- calc_heat_pulse_velocity(sap_data, probe_config = config)

# Or pass name directly
results <- calc_heat_pulse_velocity(sap_data, probe_config = "asymmetrical")
```

#### List Available Configurations

```r
# See all available probe configurations
configs <- list_available_probe_configs()
print(configs)
#   name          description                              file                      default
#   symmetrical   ICT standard symmetric probe             probe_symmetrical.yaml    TRUE
#   asymmetrical  CHPM asymmetric probe                    probe_asymmetrical.yaml   FALSE
```

### Creating Custom Probe Configurations

#### Method 1: Programmatic Creation

```r
# Create custom probe with different spacing
custom_probe <- create_custom_probe_config(
  config_name = "My Custom Probe",
  upstream_distance = 6,      # mm
  downstream_distance = 8,    # mm
  heat_pulse_duration = 2.5,  # seconds
  probe_diameter = 1.27       # mm
)

# Use in calculations
results <- calc_heat_pulse_velocity(sap_data, probe_config = custom_probe)
```

#### Method 2: Parameter Overrides

```r
# Start with a built-in config and override specific parameters
config <- load_probe_config("symmetrical",
                           custom_params = list(
                             heat_pulse_duration = 3,
                             upstream_distance = 6
                           ))

results <- calc_heat_pulse_velocity(sap_data, probe_config = config)
```

#### Method 3: Create Custom YAML File

Create a new YAML file in `inst/configurations/`:

```yaml
# probe_custom.yaml
metadata:
  config_name: Custom Research Probe
  description: Modified probe for research application
  created_date: '2025-01-15'
  version: '1.0'

probe:
  heater_position: 0
  upstream_distance: 7        # mm
  downstream_distance: 7      # mm
  diameter: 1.5              # mm
  heat_pulse_duration: 2.5   # seconds
  manufacturer: Custom
  model: Research v1

methods:
  compatible:
    - HRM
    - MHR
    - DMA
  priority_order:
    - HRM
    - DMA
    - MHR
```

Load it:

```r
config <- load_probe_config("inst/configurations/probe_custom.yaml")
# Or if installed in package: load_probe_config("custom")
```

### Probe Configuration Contents

Each probe configuration specifies:

- **Sensor positions**: Distances from heater (negative = upstream, positive = downstream)
- **Heat pulse duration**: How long the heat pulse lasts (typically 2 seconds)
- **Probe diameter**: Physical diameter in mm
- **Compatible methods**: Which HPV calculation methods work with this probe
- **Method priorities**: Recommended method ranking

---

## Wood Properties

### Built-in Wood Property Configurations

sapFluxR includes three species configurations:

| Configuration | Species | Wood Type | Thermal Diffusivity (cm²/s) | Use Case |
|---------------|---------|-----------|------------------------------|----------|
| **generic_sw** | Generic softwood | Softwood | 0.0025 | Default, conservative |
| **eucalyptus** | Eucalyptus spp. | Hardwood | 0.00143 | Eucalyptus trees |
| **pine** | Pinus spp. | Softwood | 0.00145 | Pine species |

### Loading Wood Properties

#### Use Default

```r
# Uses generic_sw automatically
results <- calc_heat_pulse_velocity(sap_data)
```

#### Specify Species

```r
# Load specific species properties
wood <- load_wood_properties("eucalyptus")
results <- calc_heat_pulse_velocity(sap_data, wood_properties = wood)

# Or pass name directly
results <- calc_heat_pulse_velocity(sap_data, wood_properties = "pine")
```

#### List Available Wood Properties

```r
# See all available wood property configurations
woods <- list_available_wood_properties()
print(woods)
#   name         species          description                  file                    default
#   generic_sw   Generic          Generic softwood properties  wood_generic_sw.yaml    TRUE
#   eucalyptus   Eucalyptus spp.  Eucalyptus properties        wood_eucalyptus.yaml    FALSE
#   pine         Pinus spp.       Pine properties              wood_pine.yaml          FALSE
```

### Creating Custom Wood Properties

#### Method 1: Programmatic Creation

```r
# Create custom wood properties for your species
custom_wood <- create_custom_wood_properties(
  species = "Quercus robur",
  thermal_diffusivity = 0.0018,  # cm²/s - measured or from literature
  dry_density = 720,              # kg/m³
  moisture_content = 40,          # %
  # Optional tree measurements
  dbh = 42.5,                     # cm
  sapwood_depth = 4.2,            # cm
  # Optional quality thresholds
  max_velocity_cm_hr = 180,
  min_velocity_cm_hr = -40,
  temperature_range = c(-5, 45)
)

results <- calc_heat_pulse_velocity(sap_data, wood_properties = custom_wood)
```

#### Method 2: Override Existing Properties

```r
# Start with eucalyptus, but override thermal diffusivity
wood <- load_wood_properties("eucalyptus",
                             overrides = list(
                               thermal_diffusivity = 0.0015
                             ))

# Override tree measurements
wood <- load_wood_properties("pine",
                             tree_overrides = list(
                               dbh = 38.2,
                               sapwood_depth = 3.8
                             ))

# Override both
wood <- load_wood_properties("generic_sw",
                             overrides = list(thermal_diffusivity = 0.0028),
                             tree_overrides = list(dbh = 45, sapwood_area = 250))
```

#### Method 3: Create Custom YAML File

```yaml
# wood_oak.yaml
metadata:
  config_name: European Oak
  species: Quercus robur
  description: European oak wood properties
  created_date: '2025-01-15'
  version: '1.0'
  default: false

wood_property:
  thermal_diffusivity: 0.0018   # cm²/s
  thermal_conductivity: 0.35    # W/(m·K)
  volumetric_heat_capacity: 1.94e6  # J/(m³·K)
  dry_density: 720              # kg/m³
  moisture_content: 40          # %
  wood_type: hardwood

tree_measurements:
  dbh: null
  bark_thickness: null
  sapwood_depth: null
  sapwood_area: null
  heartwood_radius: null

quality_thresholds:
  max_velocity_cm_hr: 180
  min_velocity_cm_hr: -40
  temperature_range: [-5, 45]
```

### Wood Property Contents

Each wood property configuration includes:

- **Thermal properties**: Diffusivity, conductivity, heat capacity
- **Physical properties**: Density, moisture content, wood type
- **Tree measurements** (optional): DBH, sapwood depth/area, bark thickness
- **Quality thresholds**: Species-specific velocity and temperature ranges

---

## Parameter Override Hierarchy

Parameters follow a clear priority order (highest to lowest):

1. **Individual function parameters** - Highest priority
2. **`parameters` list** - User-supplied parameter list
3. **Configuration objects** - From probe/wood configs
4. **Hardcoded defaults** - Lowest priority, fallback values

### Example: Parameter Priority

```r
# Load configs (provides diffusivity = 0.00143 from eucalyptus)
wood <- load_wood_properties("eucalyptus")

# parameters list (overrides config, sets diffusivity = 0.0020)
params <- list(
  diffusivity = 0.0020,
  HRM_start = 70,
  HRM_end = 110
)

# Individual parameter (overrides everything, sets diffusivity = 0.0025)
results <- calc_heat_pulse_velocity(
  sap_data,
  wood_properties = wood,      # diffusivity = 0.00143
  parameters = params,          # diffusivity = 0.0020
  diffusivity = 0.0025          # WINS! diffusivity = 0.0025
)

# Final diffusivity used: 0.0025
```

This hierarchy allows you to:
- Start with sensible defaults
- Use species-specific values from configs
- Override globally with `parameters`
- Fine-tune individual parameters when needed

---

## Complete Workflow Examples

### Example 1: Standard Analysis with Defaults

```r
library(sapFluxR)

# Read data
sap_data <- read_sap_data("data/tree01_2024.csv")

# Run analysis (uses defaults: symmetrical probe + generic softwood)
results <- calc_heat_pulse_velocity(sap_data)

# Or use full pipeline
processed <- process_sap_data(sap_data)
```

### Example 2: Eucalyptus with Custom Probe

```r
# Create custom probe configuration
probe <- create_custom_probe_config(
  config_name = "Modified ICT",
  upstream_distance = 6,
  downstream_distance = 6
)

# Load eucalyptus properties
wood <- load_wood_properties("eucalyptus")

# Run analysis
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = probe,
  wood_properties = wood,
  methods = c("HRM", "MHR", "DMA")
)
```

### Example 3: Custom Species with Tree Measurements

```r
# Define custom wood properties with tree measurements
wood <- create_custom_wood_properties(
  species = "Pinus radiata",
  thermal_diffusivity = 0.00152,
  dry_density = 450,
  moisture_content = 38,
  dbh = 42.3,
  sapwood_depth = 4.5,
  sapwood_area = 220
)

# Use asymmetric probe for CHPM method
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = "asymmetrical",
  wood_properties = wood,
  methods = "CHPM"
)
```

### Example 4: Fine-Tuned Parameter Control

```r
# Start with built-in configs
probe <- load_probe_config("symmetrical")
wood <- load_wood_properties("pine")

# Define custom analysis parameters
params <- list(
  pre_pulse = 25,        # Pre-pulse baseline period (seconds)
  HRM_start = 65,        # HRM analysis window start
  HRM_end = 105,         # HRM analysis window end
  MHR_peak_window = 30   # MHR peak detection window
)

# Override diffusivity specifically
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = probe,
  wood_properties = wood,
  parameters = params,
  diffusivity = 0.00160,  # Measured value for this specific tree
  methods = c("HRM", "MHR", "DMA")
)
```

### Example 5: Processing Multiple Trees

```r
# Create species-specific configuration
oak_wood <- create_custom_wood_properties(
  species = "Quercus robur",
  thermal_diffusivity = 0.0018,
  max_velocity_cm_hr = 170
)

# Process multiple trees with same configuration
tree_files <- c("tree01.csv", "tree02.csv", "tree03.csv")

results_list <- lapply(tree_files, function(file) {
  sap_data <- read_sap_data(file)
  calc_heat_pulse_velocity(
    sap_data,
    wood_properties = oak_wood,
    methods = "DMA"
  )
})
```

---

## Best Practices

### 1. Start with Built-in Configurations

Use built-in configurations as templates and override only what you need:

```r
# Good: Start with eucalyptus, override diffusivity
wood <- load_wood_properties("eucalyptus",
                             overrides = list(thermal_diffusivity = 0.0015))

# Avoid: Creating everything from scratch unnecessarily
wood <- create_custom_wood_properties(...)  # Only if truly custom species
```

### 2. Document Custom Configurations

When creating custom YAML files, include detailed metadata:

```yaml
metadata:
  config_name: Modified Eucalyptus for Site A
  species: Eucalyptus globulus
  description: Thermal diffusivity measured on-site, Jan 2025
  reference: "Smith et al. 2025, TreePhys 45:123-145"
  created_date: '2025-01-15'
  site_id: AU-TUM
  contact: j.smith@university.edu
```

### 3. Use Appropriate Thermal Diffusivity

| Source | Reliability | When to Use |
|--------|-------------|-------------|
| Measured on-site | Highest | Best option if available |
| Species-specific literature | High | Use published values for your species |
| Generic values | Moderate | Acceptable for exploratory analysis |
| Default (0.0025) | Conservative | Suitable for softwoods, may overestimate for hardwoods |

### 4. Validate Your Configuration

```r
# Check probe configuration
config <- load_probe_config("custom")
print(config)  # Review sensor positions and compatible methods

# Check wood properties
wood <- load_wood_properties("custom_species")
print(wood)    # Review thermal properties and thresholds

# Test with sample data before full analysis
test_results <- calc_heat_pulse_velocity(
  sap_data[1:10, ],  # Small subset
  probe_config = config,
  wood_properties = wood
)
```

### 5. Version Control Your Configurations

Keep custom YAML files in version control (git) alongside your analysis code:

```
project/
├── data/
├── analysis/
│   └── analyze_trees.R
├── configurations/
│   ├── probe_custom.yaml
│   └── wood_site_specific.yaml
└── README.md
```

---

## Troubleshooting

### Configuration Not Found

```r
Error: Probe configuration 'my_config' not found.
Available: symmetrical, asymmetrical
```

**Solution**: Check spelling or provide full path to YAML file:

```r
config <- load_probe_config("inst/configurations/my_config.yaml")
```

### Invalid Parameter Override

```r
Warning: thermal_diffusivity (0.1) is outside typical range (0.0005 - 0.01)
```

**Solution**: Check your value. Thermal diffusivity should be in cm²/s:
- Typical range: 0.0005 - 0.01 cm²/s
- Most wood: 0.001 - 0.003 cm²/s

### Method Not Compatible

```r
Error: Method 'CHPM' not compatible with 'symmetrical' configuration
```

**Solution**: Use appropriate probe configuration:

```r
# CHPM requires asymmetric probe
results <- calc_heat_pulse_velocity(sap_data,
                                    probe_config = "asymmetrical",
                                    methods = "CHPM")
```

### Missing Required Parameters

```r
Error: thermal_diffusivity is required but not provided
```

**Solution**: Ensure wood properties are loaded or provide parameter:

```r
results <- calc_heat_pulse_velocity(sap_data,
                                    wood_properties = "eucalyptus",
                                    # OR
                                    diffusivity = 0.00143)
```

---

## Additional Resources

- **Migration Guide**: See `MIGRATION_GUIDE.md` for upgrading from old detection-based system
- **README**: Package overview and quick start
- **Function Documentation**: `?load_probe_config`, `?load_wood_properties`
- **Method Guide**: See package vignettes for heat pulse velocity method details

---

## Questions or Issues?

If you encounter issues or have questions about the configuration system:

1. Check function documentation: `?load_probe_config`
2. Review this guide and the Migration Guide
3. Open an issue on GitHub with a reproducible example
