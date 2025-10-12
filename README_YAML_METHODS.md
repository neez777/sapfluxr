# YAML-Based Heat Pulse Velocity Methods

## Overview

This system allows researchers to develop new heat pulse velocity calculation methods using YAML configuration files instead of writing R code. This makes method development more accessible to researchers who may not be familiar with R programming but understand the underlying physics and mathematics of heat pulse velocity calculations.

## Key Benefits

1. **Accessibility**: Researchers can develop methods without learning R programming
2. **Transparency**: Method definitions are human-readable and self-documenting
3. **Flexibility**: Easy to modify parameters and add new methods
4. **Validation**: Built-in input validation and quality assessment
5. **Integration**: Seamlessly works with existing sapFluxR functions

## Quick Start

### 1. List Available Methods
```r
# See what methods are available
methods <- list_available_methods()
print(methods)
```

### 2. Use YAML Methods
```r
# Load your sap flow data
sap_data <- read_sap_data("your_data.txt")

# Calculate velocities using YAML methods
results <- calc_heat_pulse_velocity_yaml(
  sap_data,
  methods = c("HRM", "MHR", "Tmax_Coh")
)
```

### 3. Create a New Method
Create a file `inst/methods/my_method.yaml` following the template in the development guide.

## Available Methods

### Standard Methods (Converted to YAML)
- **HRM** - Heat Ratio Method (Burgess et al. 2001)
- **MHR** - Maximum Heat Ratio Method (Lopez et al. 2021)
- **Tmax_Coh** - T-max Cohen Method (Cohen et al. 1981)

### Example Method
- **SIMPLE_RATIO** - Simple demonstration method

## File Structure

```
inst/
├── methods/
│   ├── hrm_method.yaml           # Heat Ratio Method
│   ├── mhr_method.yaml           # Maximum Heat Ratio Method
│   ├── tmax_coh_method.yaml      # T-max Cohen Method
│   └── simple_ratio_method.yaml  # Example method
├── configurations/               # Existing probe configurations
└── extdata/                     # Sample data files

R/
└── yaml_method_parser.R         # YAML parser and execution engine

docs/
└── yaml_method_development_guide.md  # Comprehensive development guide
```

## Method Development

### Basic YAML Structure
```yaml
method:
  name: "METHOD_NAME"
  full_name: "Full Method Name"
  description: "Method description"
  reference: "Citation"

inputs:
  required:
    - name: "variable_name"
      type: "variable_type"

outputs:
  - name: "output_name"
    type: "output_type"

calculation:
  steps:
    - name: "step_name"
      expression: "R expression"

parameters:
  - name: "param_name"
    type: "parameter_type"
    default: default_value
```

### Available Variables

#### Raw Temperature Measurements
- `do`, `di`, `uo`, `ui` - Raw temperature readings

#### Derived Variables
- `delatT_do`, `delatT_di`, `delatT_uo`, `delatT_ui` - Temperature differences
- `dTratio_douo`, `dTratio_diui` - Temperature ratios
- `HRM_period` - Sampling window logical vector
- `pre_pulse_period` - Pre-pulse period logical vector

#### Parameters
- `diffusivity` - Thermal diffusivity (default: 0.0025 cm²/s)
- `x` - Probe spacing (default: 0.5 cm)
- `pre_pulse` - Pre-pulse period (default: 30 sec)
- `HRM_start`, `HRM_end` - Sampling window (default: 60-100 sec)

## Example: Creating a Custom Method

Here's a simple example of creating a new method:

### 1. Create the YAML File
```yaml
# inst/methods/my_custom_method.yaml
method:
  name: "MY_CUSTOM"
  full_name: "My Custom Method"
  description: "A custom velocity calculation method"
  reference: "My Paper (2024)"
  category: "ratio_based"

inputs:
  required:
    - name: "delatT_do"
      description: "Temperature difference downstream outer"
      type: "numeric_vector"
    - name: "delatT_uo"
      description: "Temperature difference upstream outer"
      type: "numeric_vector"
    - name: "diffusivity"
      description: "Thermal diffusivity"
      type: "numeric_scalar"
      default: 0.0025
    - name: "x"
      description: "Probe spacing"
      type: "numeric_scalar"
      default: 0.5

outputs:
  - name: "Vh_outer"
    description: "Heat pulse velocity (cm/hr)"
    type: "numeric_scalar"

calculation:
  steps:
    - name: "calculate_velocity"
      description: "Calculate velocity from maximum ratio"
      expression: |
        max_do <- max(delatT_do, na.rm = TRUE)
        max_uo <- max(delatT_uo, na.rm = TRUE)

        if (max_do > 0 && max_uo > 0) {
          ratio <- max_do / max_uo
          Vh_outer <- (diffusivity / x) * log(ratio) * 3600
        } else {
          Vh_outer <- NA_real_
        }

parameters:
  - name: "diffusivity"
    description: "Thermal diffusivity"
    type: "numeric"
    range: [0, 0.01]
    default: 0.0025
    units: "cm²/s"

quality_criteria:
  - name: "positive_ratio"
    description: "Temperature ratio is positive"
    check: "max_do > 0 && max_uo > 0"
    severity: "error"
  - name: "reasonable_velocity"
    description: "Velocity is within reasonable range"
    check: "abs(Vh_outer) < 200"
    severity: "warning"
```

### 2. Test Your Method
```r
# Load test data
sap_data <- create_test_sap_data()

# Test your new method
results <- calc_heat_pulse_velocity_yaml(
  sap_data,
  methods = "MY_CUSTOM"
)

# Check results
print(results)
```

## Advanced Features

### Quality Assessment
Methods can include quality criteria that automatically assess data quality:

```yaml
quality_criteria:
  - name: "input_validity"
    description: "All inputs are valid"
    check: "all(is.finite(delatT_do))"
    severity: "error"
  - name: "velocity_range"
    description: "Velocity is within expected range"
    check: "abs(Vh_outer) >= 0 && abs(Vh_outer) <= 200"
    severity: "warning"
```

### Parameter Validation
Parameters can include validation rules:

```yaml
parameters:
  - name: "diffusivity"
    description: "Thermal diffusivity"
    type: "numeric"
    range: [0, 0.01]  # Valid range
    default: 0.0025
    units: "cm²/s"
```

### Method Limitations
Document method limitations and applicability:

```yaml
limitations:
  - "Requires clear temperature peaks"
  - "Sensitive to measurement noise"

applicability:
  velocity_range: [5, 100]
  probe_configurations: ["symmetrical"]
  best_for: ["moderate_velocities"]
```

## Integration with Existing System

YAML methods work seamlessly with existing sapFluxR functions:

```r
# Mix YAML and traditional methods
results <- calc_heat_pulse_velocity(
  sap_data,
  methods = c("HRM", "MY_YAML_METHOD")
)

# Use only YAML methods
results <- calc_heat_pulse_velocity_yaml(
  sap_data,
  methods = c("HRM", "MHR", "Tmax_Coh")
)

# Get method information
config <- load_method_config("HRM")
print(config$method$description)
```

## Troubleshooting

### Common Issues

1. **YAML Syntax Errors**
   - Check indentation (use spaces, not tabs)
   - Validate YAML syntax
   - Ensure proper string quoting

2. **Missing Variables**
   - Verify all required inputs are available
   - Check variable names match exactly

3. **Calculation Errors**
   - Add input validation steps
   - Use tryCatch for error handling
   - Check for division by zero

### Getting Help

1. Check the development guide: `docs/yaml_method_development_guide.md`
2. Look at existing method files for examples
3. Use `list_available_methods()` to see working methods
4. Test with simple expressions first

## Dependencies

- `yaml` package for YAML parsing
- Standard R packages (no additional dependencies required)

## Future Enhancements

Potential future improvements to the YAML system:

1. **Visual Method Builder**: GUI for creating methods
2. **Method Validation**: Automated testing framework
3. **Performance Optimization**: Compiled method execution
4. **Method Sharing**: Repository for community methods
5. **Documentation Generation**: Auto-generated method documentation

## Contributing

To contribute new methods:

1. Create your YAML method file following the guidelines
2. Test thoroughly with sample data
3. Document limitations and applicability
4. Submit for inclusion in the package

## References

- Burgess, S.S.O., et al. (2001). An improved heat pulse method to measure low and reverse rates of sap flow in woody plants. Tree Physiology 21:589-598.
- Lopez, B.C., et al. (2021). Maximum heat ratio method for sap flow measurement. Plant Soil 469:503-523.
- Cohen, Y., et al. (1981). Improvement of the heat pulse method for determining sap flow in trees. Plant, Cell and Environment 4:391-397.

## License

This YAML method system is part of the sapFluxR package and follows the same licensing terms.