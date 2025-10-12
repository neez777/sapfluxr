# sapFluxR Function Flow Diagram Guide

## Overview

The `sapFluxR_function_flow.mmd` file contains a comprehensive Mermaid diagram showing the complete function and data flow through the sapFluxR package. Unlike the workflow diagram (`sapFluxR_workflow.mmd`) which shows the scientific/conceptual workflow, this diagram shows **actual R functions and their interactions**.

## What's Different from the Workflow Diagram?

| Workflow Diagram | Function Flow Diagram |
|------------------|----------------------|
| Conceptual steps (e.g., "Process Raw Temperatures") | Actual functions (e.g., `calc_heat_pulse_velocity()`) |
| Scientific workflow stages | Implementation pathways |
| Algorithm choices | Function calls and data objects |
| Linear progression | Multiple entry points and alternative paths |

## Color Coding

The diagram uses color coding to distinguish workflow stages:

- **Light Blue** - Data Import & Parsing
- **Orange** - Configuration (Probe & Wood Properties)
- **Purple** - Main Processing Pipeline
- **Green** - HPV Calculations
- **Yellow** - Quality Control & Diagnostics
- **Teal** - Temporal Processing
- **Pink** - Flux Scaling & Tree Water Use
- **Brown** - Export & Reporting
- **Bright Blue (thick border)** - Data Objects
- **Red** - Decision Points
- **Light Grey** - Utility Functions
- **Green (thick border)** - Final Outputs

## Key Features Shown

### 1. **Multiple Entry Points**
The diagram shows three main ways to use the package:
- **Unified Pipeline**: `process_sap_data()` - Automated, recommended for most users
- **Manual Step-by-Step**: Individual function calls - For advanced users who want control
- **Advanced/Research**: `calc_unified_hpv()` or YAML methods - For method development

### 2. **Configuration Pathways**
Shows how to configure the analysis:

**Probe Configuration:**
- Auto-detect from data
- Load preset configurations (symmetric, asymmetric, etc.)
- Create custom configurations
- List available options

**Wood Properties:**
- Load species-specific properties (eucalyptus, pine, generic)
- Load from custom YAML files
- Create custom properties programmatically
- Use package defaults

### 3. **Method Selection**
Multiple pathways for choosing calculation methods:
- Automatic recommendation based on data quality and probe configuration
- User-specified methods
- Automatic optimal method detection per pulse
- Try all compatible methods and compare

### 4. **Quality Control Loops**
Shows optional QC steps:
- Method comparison and validation
- Sensor diagnostics
- Outlier detection
- Probe alignment validation
- Diagnostic plotting

### 5. **Temporal Processing**
Optional data processing steps:
- Temporal aggregation (hourly, daily, weekly, monthly)
- Gap filling and interpolation
- Can be combined or used separately

### 6. **Scaling to Tree Level**
Complete pathway from velocity to tree water use:
- Velocity (Vh) → Flux Density (Js)
- Flux Density → Tree Sap Flux (Q)
- Optional spatial variation corrections (radial, azimuthal)
- Sapwood area estimation or direct measurement

### 7. **Multi-Tree Processing**
Shows batch processing options:
- Individual tree processing
- Parallel batch processing
- Cross-tree comparison and statistics

### 8. **Export & Reporting**
Three main output types:
- **Reports**: Diagnostic or analysis reports (HTML/PDF/MD)
- **Data**: Export in multiple formats (CSV, Excel, JSON, RDS)
- **Visualizations**: Publication-ready plots (PNG/PDF/SVG)

## Important Data Objects

The diagram highlights key data structures (shown with thick borders):

1. **`sap_data`** - Imported data with diagnostics, measurements, metadata
2. **`ProbeConfiguration`** - R6 object with probe setup and method compatibility
3. **`WoodProperties`** - R6 object with thermal and physical properties
4. **`vh_results`** - Tibble with calculated velocities and quality flags
5. **`sap_flux_density`** - Scaled flux density values
6. **`tree_sap_flux`** - Whole-tree water use

## Example Workflows

### Simple Workflow (Beginner)
```
read_sap_data() → process_sap_data() → export_sap_data()
```

### Configured Workflow (Intermediate)
```
read_sap_data()
→ load_probe_config("symmetric")
→ load_wood_properties("eucalyptus")
→ process_sap_data()
→ generate_diagnostic_report()
```

### Manual Workflow (Advanced)
```
read_sap_data()
→ validate_sap_data()
→ detect_probe_config()
→ assess_data_quality()
→ recommend_methods()
→ calc_heat_pulse_velocity()
→ compare_hpv_methods()
→ calc_sap_flux_density()
→ calc_tree_sap_flux()
→ export results
```

### Research Workflow (Custom Methods)
```
read_sap_data()
→ create_custom_probe_config()
→ create_custom_wood_properties()
→ calc_heat_pulse_velocity_yaml()
→ compare_hpv_methods()
→ generate_analysis_report()
```

### Multi-Tree Workflow
```
read_multiple_sap_data()
→ batch_process_sap_flow()
→ compare_trees()
→ export results
```

## Using the Diagram

### To understand a specific function:
1. Find the function name in the diagram (use Ctrl+F in your editor)
2. Follow the arrows backward to see what data it needs
3. Follow the arrows forward to see what it produces

### To plan an analysis:
1. Start at "Data Import" (top left)
2. Choose your processing approach (unified vs. manual)
3. Follow the path through the stages
4. See what optional steps are available (QC, temporal processing, scaling)
5. Choose your export/reporting options

### To understand alternative paths:
- Look for decision diamonds (red boxes)
- These show where you have choices in the workflow
- Different paths lead to different outputs or levels of detail

## File Locations

- **Function Flow Diagram**: `sapFluxR_function_flow.mmd`
- **Workflow Diagram**: `sapFluxR_workflow.mmd` (scientific workflow)
- **Function Reference**: `FUNCTION_REFERENCE.md` (detailed function documentation)
- **User Inputs Guide**: `user_inputs.md` (parameter specifications)

## Viewing the Diagram

To view the Mermaid diagram:

1. **In VS Code**: Install "Markdown Preview Mermaid Support" extension
2. **Online**: Paste into [Mermaid Live Editor](https://mermaid.live/)
3. **In RStudio**: Use the DiagrammeR package:
   ```r
   library(DiagrammeR)
   mermaid(readLines("sapFluxR_function_flow.mmd"))
   ```

## Notes

- **Dotted arrows** (if present) represent optional or conditional flows
- **Thick arrows** represent main pathways
- **Utility functions** can be called from multiple points in the workflow
- The diagram shows all possibilities - most analyses will only use a subset of these functions

## Updates

This diagram should be updated when:
- New functions are added to the package
- Workflow pathways change
- New data objects are introduced
- Configuration options are added

---

**Last Updated**: 2025-10-12
**Package Version**: 0.1.0
**Diagram File**: `sapFluxR_function_flow.mmd`
