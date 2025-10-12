# Configuration System Refactor - Executive Summary

**Date:** 2025-10-11
**Status:** Planning Complete - Ready for Implementation
**Priority:** High

---

## What We're Doing

Refactoring sapFluxR's parameter/configuration system to:
1. **Remove broken auto-detection** of probe configuration (can't work - no info in data)
2. **Implement YAML-based loading** for both probe and wood properties
3. **Provide sensible defaults** that work immediately
4. **Allow flexible overrides** at multiple levels

---

## Why This Matters

### Current Problems
- ❌ `detect_probe_config()` cannot reliably detect probe spacing from data alone
- ❌ Hardcoded defaults scattered throughout code
- ❌ No way to specify species-specific wood properties systematically
- ❌ Confusing what parameters go where
- ❌ Difficult to process multiple trees with different properties

### After Refactor
- ✅ Explicit configuration system - no magic auto-detection
- ✅ Works out-of-the-box with sensible defaults
- ✅ Species-specific wood properties in YAML files
- ✅ Clear separation: probe hardware vs wood properties vs calculation settings
- ✅ Easy to process multiple trees with per-tree YAML files
- ✅ Flexible parameter override system

---

## Three-Tier System

```
┌──────────────────────────────────────┐
│ 1. PROBE CONFIG (Hardware)          │  ← Fixed per probe model
│    probe_symmetrical.yaml (DEFAULT)  │     (upstream/downstream distances, etc.)
└──────────────────────────────────────┘
                ↓
┌──────────────────────────────────────┐
│ 2. WOOD PROPERTIES (Species/Tree)   │  ← Variable per species/tree
│    wood_generic_sw.yaml (DEFAULT)    │     (thermal_diffusivity, density, etc.)
└──────────────────────────────────────┘
                ↓
┌──────────────────────────────────────┐
│ 3. CALC PARAMETERS (Analysis)       │  ← Variable per analysis
│    Function parameters in R code     │     (HRM_start, HRM_end, etc.)
└──────────────────────────────────────┘
```

---

## Key Decisions Made

### 1. Variable Categorization ✅
- **Probe Config:** Physical hardware specs (upstream_distance, diameter, heat_pulse_duration)
- **Wood Properties:** Thermal/physical properties (thermal_diffusivity, density, moisture)
- **Calc Parameters:** Analysis settings (HRM_start, HRM_end, pre_pulse)

### 2. heat_pulse_duration Placement ✅
- **Location:** Probe configuration YAML (hardware-dependent)
- **Default:** 2 seconds
- **Can override:** Yes, via parameters list if needed

### 3. Quality Thresholds ✅
- **Primary location:** Wood properties (species defaults)
- **Can override:** Via calculation parameters
- **Rationale:** Different species have different typical velocity ranges

### 4. Tree Measurements (DBH, sapwood_depth) ✅
- **Optional in:** Wood property YAML files
- **Always overridable:** Via function parameters
- **Use case:** Per-tree YAML files for multi-tree studies

### 5. Default Configuration ✅
- **Probe:** probe_symmetrical.yaml (ICT SFM1x standard)
- **Wood:** wood_generic_sw.yaml (generic softwood)
- **User action required:** None - just works

---

## User Workflows After Refactor

### Simplest (Use Defaults)
```r
sap_data <- read_sap_data("data.txt")
results <- calc_heat_pulse_velocity(sap_data)
# Uses symmetric probe + generic softwood defaults
```

### Species-Specific
```r
results <- calc_heat_pulse_velocity(
  sap_data,
  wood_properties = "eucalyptus"  # Loads wood_eucalyptus.yaml
)
```

### Custom YAML Files
```r
results <- calc_heat_pulse_velocity(
  sap_data,
  probe_config = "my_probe.yaml",
  wood_properties = "my_tree.yaml"
)
```

### Multi-Tree with Per-Tree YAMLs
```r
tree_files <- c("tree01.txt", "tree02.txt", "tree03.txt")
wood_files <- c("tree01_wood.yaml", "tree02_wood.yaml", "tree03_wood.yaml")

results <- batch_process_sap_flow(
  data_files = tree_files,
  wood_property_files = wood_files
)
```

### Parameter Overrides
```r
results <- calc_heat_pulse_velocity(
  sap_data,
  wood_properties = "eucalyptus",
  diffusivity = 0.0028,           # Override thermal_diffusivity
  dbh = 45.2,                     # Add tree measurement
  parameters = list(HRM_start = 70)  # Override calc param
)
```

---

## Implementation Timeline

| Session | Tasks | Time | Status |
|---------|-------|------|--------|
| **Session 1** | Update YAMLs + Create wood property system | 3-4 hrs | ⬜ Not started |
| **Session 2** | Refactor probe config system | 2-3 hrs | ⬜ Not started |
| **Session 3** | Update core calculation functions | 2-3 hrs | ⬜ Not started |
| **Session 4** | Testing + Documentation | 2-3 hrs | ⬜ Not started |
| **Session 5** | Cleanup + Finalization | 1 hr | ⬜ Not started |
| **Total** | | **10-14 hrs** | **0% Complete** |

---

## Breaking Changes

### Functions Being Removed
- ❌ `detect_probe_config()` - Cannot work reliably
- ❌ `detect_config_from_sensors()` - Helper for broken detection
- ❌ `get_standard_configs()` - Replaced by new system

### Backward Compatibility
✅ **Most old code still works** - defaults kick in automatically
❌ **Only breaks** if code explicitly called `detect_probe_config()`

### Migration
**Old (broken):**
```r
probe_config <- detect_probe_config(sap_data)  # Can't detect from data!
```

**New (works):**
```r
# Option 1: Just omit it
results <- calc_heat_pulse_velocity(sap_data)  # Uses default

# Option 2: Explicit config
results <- calc_heat_pulse_velocity(sap_data, probe_config = "symmetrical")
```

---

## New Functions to Implement

### Probe Configuration
- `load_probe_config(name_or_path)` - Load probe config from YAML
- `get_default_probe_config()` - Get default symmetric config
- `list_available_probe_configs()` - List all available configs
- `create_custom_probe_config(...)` - Create config in R

### Wood Properties (NEW SYSTEM!)
- `load_wood_properties(name_or_path)` - Load wood properties from YAML
- `get_default_wood_properties()` - Get default generic softwood
- `list_available_wood_properties()` - List all available wood configs
- `create_custom_wood_properties(...)` - Create wood config in R

---

## Files Modified/Created

### Modified (11 files)
- `inst/configurations/probe_*.yaml` (2 files) - Add heat_pulse_duration
- `inst/configurations/wood_*.yaml` (3 files) - Add tree_measurements, quality_thresholds
- `R/03_probe_configuration.R` - Add new functions, remove old ones
- `R/04_heat_pulse_velocity_core.R` - Update calc_heat_pulse_velocity()
- `R/14_processing_pipeline.R` - Update process_sap_data()
- `R/05_heat_pulse_velocity_advanced.R` - Update config usage
- `R/08_quality_control.R` - Use quality thresholds from wood properties
- Various test files

### Created (8 files)
- `R/03b_wood_properties.R` - NEW wood property system
- `inst/configurations/probe_custom_example.yaml` - User template
- `inst/configurations/wood_custom_example.yaml` - User template
- `tests/testthat/test-wood-properties.R` - Tests for wood system
- Test YAML files for testing
- `CONFIGURATION_GUIDE.md` - User guide
- `MIGRATION_GUIDE.md` - Transition help

---

## Success Criteria

### Functional
- ✅ Default workflow works without any config specified
- ✅ Users can load built-in configs by name
- ✅ Users can load custom YAML files by path
- ✅ Users can override individual parameters
- ✅ Users can create configs in R without YAML
- ✅ Multi-tree workflow with per-tree YAMLs works

### Code Quality
- ✅ All tests pass (`devtools::test()`)
- ✅ No warnings (`devtools::check()`)
- ✅ All functions documented
- ✅ Examples work

### User Experience
- ✅ Clear error messages
- ✅ Comprehensive documentation
- ✅ Migration guide available
- ✅ Example YAML templates provided

---

## Documentation Structure

```
sapFluxR/
├── IMPLEMENTATION_PLAN.md          ← Detailed technical specs
├── VARIABLE_REFERENCE.md           ← Quick lookup: where does X go?
├── REFACTOR_CHECKLIST.md           ← Session-by-session tracking
├── REFACTOR_SUMMARY.md             ← This file (overview)
├── CONFIGURATION_GUIDE.md          ← To create: User guide
├── MIGRATION_GUIDE.md              ← To create: Old → new code
├── PARAMETER_CATEGORIZATION.md     ← Background: why this refactor
└── PROBE_CONFIG_REFACTOR_PLAN.md   ← Original plan (probe only)
```

---

## Quick Start for Implementation

### Step 1: Read the docs (5 min)
- ✅ This file (REFACTOR_SUMMARY.md) - Overview
- ✅ VARIABLE_REFERENCE.md - Where variables go
- ✅ IMPLEMENTATION_PLAN.md - Technical details

### Step 2: Open checklist (1 min)
- Open REFACTOR_CHECKLIST.md
- This is your working document across sessions

### Step 3: Start Session 1 (3-4 hrs)
- Follow Phase 1 & 2 in checklist
- Check off items as you complete them
- Test after each major step

### Step 4: Continue through sessions
- One session at a time
- Test frequently
- Update checklist
- Ask questions if stuck

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Breaking user code | Keep defaults working; provide migration guide |
| YAML parsing errors | Validate YAML; clear error messages |
| Parameter confusion | Document hierarchy; provide examples |
| Test failures | Test incrementally; keep old code until tests pass |

---

## Questions? Issues?

### During Implementation
1. Check IMPLEMENTATION_PLAN.md for technical details
2. Check VARIABLE_REFERENCE.md for categorization
3. Check REFACTOR_CHECKLIST.md for current task
4. Ask for clarification if needed

### After Implementation
1. Create CONFIGURATION_GUIDE.md for users
2. Create MIGRATION_GUIDE.md for transition
3. Update README.md with examples
4. Announce changes to users

---

## Next Action

**✅ Start Session 1:** Update YAML files and create wood property loading system

**Open:** REFACTOR_CHECKLIST.md and begin Phase 1

**Time:** 3-4 hours

**Goal:** Wood property YAML files updated, new `load_wood_properties()` function working

---

**Status:** Ready to implement! 🚀

**Last Updated:** 2025-10-11
