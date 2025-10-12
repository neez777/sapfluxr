# Configuration System Refactor - Quick Reference Card

**Print this page or keep it open for quick lookup during implementation**

---

## 🎯 Where Does This Variable Go?

| Variable | Category | File Location |
|----------|----------|---------------|
| upstream_distance | PROBE | probe_*.yaml |
| downstream_distance | PROBE | probe_*.yaml |
| heat_pulse_duration | PROBE | probe_*.yaml |
| thermal_diffusivity | WOOD | wood_*.yaml |
| dry_density | WOOD | wood_*.yaml |
| moisture_content | WOOD | wood_*.yaml |
| dbh | WOOD | wood_*.yaml (optional) |
| sapwood_depth | WOOD | wood_*.yaml (optional) |
| HRM_start | CALC | Function parameter |
| HRM_end | CALC | Function parameter |
| pre_pulse | CALC | Function parameter |

---

## 📂 File Locations

### YAML Config Files
```
inst/configurations/
├── probe_symmetrical.yaml        (DEFAULT probe)
├── probe_asymmetrical.yaml
├── wood_generic_sw.yaml           (DEFAULT wood)
├── wood_eucalyptus.yaml
└── wood_pine.yaml
```

### R Source Files
```
R/
├── 03_probe_configuration.R       (MODIFY - add new functions)
├── 03b_wood_properties.R          (CREATE - new file)
├── 04_heat_pulse_velocity_core.R  (MODIFY - update signature)
└── 14_processing_pipeline.R       (MODIFY - remove auto-detect)
```

### Test Files
```
tests/testthat/
├── test-probe-config.R            (MODIFY)
├── test-wood-properties.R         (CREATE)
├── test-heat-pulse-velocity.R     (MODIFY)
└── test-processing-pipeline.R     (MODIFY)
```

---

## 🔧 Key Functions to Implement

### Probe Configuration (in R/03_probe_configuration.R)
```r
load_probe_config(config_name = "symmetrical", custom_params = NULL)
get_default_probe_config()
list_available_probe_configs()
create_custom_probe_config(...)
```

### Wood Properties (NEW in R/03b_wood_properties.R)
```r
load_wood_properties(config_name = "generic_sw", overrides = NULL)
get_default_wood_properties()
list_available_wood_properties()
create_custom_wood_properties(...)
```

### Functions to REMOVE (in R/03_probe_configuration.R)
```r
detect_probe_config()           ❌ DELETE
detect_config_from_sensors()    ❌ DELETE
get_standard_configs()          ❌ DELETE
```

---

## 📋 Session Checklist

### Session 1 (3-4 hrs): YAML + Wood System
- [ ] Update probe_*.yaml (add heat_pulse_duration)
- [ ] Update wood_*.yaml (add tree_measurements, quality_thresholds)
- [ ] Create R/03b_wood_properties.R
- [ ] Implement all wood property functions
- [ ] Test: load_wood_properties() works

### Session 2 (2-3 hrs): Probe Refactor
- [ ] Add new functions to R/03_probe_configuration.R
- [ ] Comment out old functions (don't delete yet)
- [ ] Update ProbeConfiguration R6 class
- [ ] Test: load_probe_config() works

### Session 3 (2-3 hrs): Core Functions
- [ ] Update calc_heat_pulse_velocity() signature
- [ ] Update process_sap_data() - REMOVE detect_probe_config()
- [ ] Update other functions as needed
- [ ] Test: full workflow works

### Session 4 (2-3 hrs): Testing + Docs
- [ ] Update all test files
- [ ] Run devtools::test()
- [ ] Create CONFIGURATION_GUIDE.md
- [ ] Create MIGRATION_GUIDE.md

### Session 5 (1 hr): Cleanup
- [ ] DELETE old functions
- [ ] Run devtools::check()
- [ ] Update NEWS.md
- [ ] Final testing

---

## 🏗️ YAML Structure Templates

### Probe Configuration
```yaml
metadata:
  config_name: "Name"
  description: "Description"
  default: true  # or false

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
  heat_pulse_duration: 2     # NEW!

methods:
  compatible: [HRM, MHR, DMA]
  recommended: [HRM, DMA]
  priority_order: [HRM, DMA, MHR]
```

### Wood Properties
```yaml
metadata:
  config_name: "Name"
  description: "Description"
  default: true  # or false

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

tree_measurements:          # NEW! Optional
  dbh: null
  bark_thickness: null
  sapwood_depth: null
  sapwood_area: null
  heartwood_radius: null

quality_thresholds:         # NEW!
  max_velocity_cm_hr: 200
  min_velocity_cm_hr: -50
  temperature_range: [-10, 60]
```

---

## 🧪 Testing Commands

```r
# Load package
devtools::load_all()

# Test specific file
devtools::test_active_file()

# Run all tests
devtools::test()

# Full package check
devtools::check()

# Coverage (optional)
covr::package_coverage()

# Build documentation
devtools::document()
```

---

## 💡 Common Issues & Solutions

### Issue: YAML won't parse
**Solution:** Check YAML syntax with yaml::read_yaml("file.yaml")

### Issue: Function not found
**Solution:** Run devtools::load_all() and devtools::document()

### Issue: Tests failing
**Solution:** Run tests incrementally; check one test file at a time

### Issue: Can't find config file
**Solution:** Check system.file() returns valid path; verify file exists

---

## 📝 Parameter Override Hierarchy

**Priority (highest to lowest):**

1. 🥇 **Individual function parameters**
   ```r
   calc_heat_pulse_velocity(..., diffusivity = 0.003)
   ```

2. 🥈 **`parameters` list**
   ```r
   calc_heat_pulse_velocity(..., parameters = list(diffusivity = 0.003))
   ```

3. 🥉 **Config objects**
   ```r
   calc_heat_pulse_velocity(..., wood_properties = "eucalyptus")
   ```

4. 4️⃣ **Hardcoded defaults**

---

## ✅ Pre-Implementation Checklist

Before starting:
- [ ] Read REFACTOR_SUMMARY.md (5 min)
- [ ] Read VARIABLE_REFERENCE.md (5 min)
- [ ] Scan IMPLEMENTATION_PLAN.md (10 min)
- [ ] Open REFACTOR_CHECKLIST.md (working doc)
- [ ] Have test data file ready
- [ ] R environment set up (devtools, yaml packages)

---

## 🎯 Success Criteria

Quick check - can you do all these?
- [ ] `calc_heat_pulse_velocity(sap_data)` works (defaults)
- [ ] `load_probe_config("symmetrical")` returns config object
- [ ] `load_wood_properties("eucalyptus")` returns wood object
- [ ] `list_available_probe_configs()` shows configs
- [ ] `list_available_wood_properties()` shows wood configs
- [ ] Override: `diffusivity = 0.003` works
- [ ] Custom YAML file loads correctly
- [ ] `devtools::test()` passes
- [ ] `devtools::check()` - 0 errors, 0 warnings

---

## 🚀 Start Command

```r
# Open checklist
file.edit("REFACTOR_CHECKLIST.md")

# Begin Session 1, Phase 1.1
file.edit("inst/configurations/probe_symmetrical.yaml")
# Add: heat_pulse_duration: 2
```

---

**Keep this page open during implementation!**

**Questions?** Check IMPLEMENTATION_PLAN.md for details

**Stuck?** Check VARIABLE_REFERENCE.md for categorization

**Track progress:** Update REFACTOR_CHECKLIST.md
