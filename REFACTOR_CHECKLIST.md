# Configuration System Refactor - Implementation Checklist

**Date Started:** 2025-10-11
**Status:** Not Started
**Current Phase:** Phase 0 - Planning Complete

---

## Progress Tracking

- [ ] Phase 1: Update YAML Files (1 hour)
- [ ] Phase 2: Create Wood Property Loading System (2-3 hours)
- [ ] Phase 3: Refactor Probe Configuration System (2-3 hours)
- [ ] Phase 4: Update Core Calculation Functions (2-3 hours)
- [ ] Phase 5: Testing (2-3 hours)
- [ ] Phase 6: Documentation (1-2 hours)
- [ ] Phase 7: Cleanup & Finalization (30 min)

**Total Estimated Time:** 10-14 hours across 5 sessions

---

## Session 1: YAML Updates & Wood Property System (3-4 hours)

### Phase 1.1: Update Probe YAML Files
- [ ] Add `heat_pulse_duration: 2` to `inst/configurations/probe_symmetrical.yaml`
- [ ] Add `heat_pulse_duration: 2` to `inst/configurations/probe_asymmetrical.yaml`
- [ ] Add `default: true` to metadata in `probe_symmetrical.yaml`
- [ ] Test: YAML files parse without errors

**Files modified:** 2

### Phase 1.2: Update Wood Property YAML Files
- [ ] Add `tree_measurements` section to `wood_generic_sw.yaml`
- [ ] Add `quality_thresholds` section to `wood_generic_sw.yaml`
- [ ] Add `default: true` to metadata in `wood_generic_sw.yaml`
- [ ] Add `tree_measurements` section to `wood_eucalyptus.yaml`
- [ ] Add `quality_thresholds` section to `wood_eucalyptus.yaml`
- [ ] Add `tree_measurements` section to `wood_pine.yaml`
- [ ] Add `quality_thresholds` section to `wood_pine.yaml`
- [ ] Test: All wood YAML files parse without errors

**Files modified:** 3

### Phase 2: Create Wood Property Loading System
- [ ] Create new file `R/03b_wood_properties.R`
- [ ] Implement `WoodProperties` R6 class
  - [ ] Define fields (thermal, physical, tree measurements, quality)
  - [ ] Implement `initialize()` method
  - [ ] Implement `print()` method
  - [ ] Implement `validate()` method
  - [ ] Add roxygen2 documentation
- [ ] Implement `load_wood_properties()` function
  - [ ] YAML file path resolution (name or path)
  - [ ] YAML parsing
  - [ ] Override support
  - [ ] Tree override support
  - [ ] Error handling
  - [ ] Add roxygen2 documentation with examples
- [ ] Implement `get_default_wood_properties()` function
  - [ ] Wrapper around load_wood_properties("generic_sw")
  - [ ] Add roxygen2 documentation
- [ ] Implement `list_available_wood_properties()` function
  - [ ] Find all wood_*.yaml files
  - [ ] Parse metadata
  - [ ] Return data frame
  - [ ] Add roxygen2 documentation
- [ ] Implement `create_custom_wood_properties()` function
  - [ ] R-based config creation (no YAML needed)
  - [ ] Parameter validation
  - [ ] Add roxygen2 documentation
- [ ] Test: Run `devtools::load_all()` - no errors
- [ ] Test: Try `load_wood_properties("generic_sw")` in console
- [ ] Test: Try `list_available_wood_properties()` in console

**Files created:** 1

### Session 1 Tests
- [ ] Create `tests/testthat/test-wood-properties.R`
- [ ] Test: `load_wood_properties("generic_sw")` works
- [ ] Test: `load_wood_properties("eucalyptus")` works
- [ ] Test: `load_wood_properties()` uses default
- [ ] Test: Override parameters work
- [ ] Test: Tree override parameters work
- [ ] Test: Invalid config name errors helpfully
- [ ] Test: Malformed YAML errors helpfully
- [ ] Run: `devtools::test_active_file()`
- [ ] All tests pass? If no, fix issues

**Session 1 Complete:** ✅ Yes / ❌ No

---

## Session 2: Probe Config Refactor (2-3 hours)

### Phase 3.1: Add New Functions to Probe Configuration

**File:** `R/03_probe_configuration.R`

- [ ] Backup: Copy file to `03_probe_configuration_BACKUP.R` (temporary)
- [ ] Implement `load_probe_config()` function
  - [ ] YAML file path resolution
  - [ ] YAML parsing
  - [ ] Read `heat_pulse_duration` from YAML (NEW!)
  - [ ] Override support
  - [ ] Create ProbeConfiguration object
  - [ ] Error handling
  - [ ] Add roxygen2 documentation with examples
- [ ] Implement `get_default_probe_config()` function
  - [ ] Wrapper around load_probe_config("symmetrical")
  - [ ] Add roxygen2 documentation
- [ ] Implement `list_available_probe_configs()` function
  - [ ] Find all probe_*.yaml files
  - [ ] Parse metadata
  - [ ] Return data frame
  - [ ] Add roxygen2 documentation
- [ ] Implement `create_custom_probe_config()` function
  - [ ] R-based config creation
  - [ ] Parameter validation
  - [ ] Add roxygen2 documentation
- [ ] Test: Run `devtools::load_all()` - no errors
- [ ] Test: Try `load_probe_config("symmetrical")` in console
- [ ] Test: Check `config$heat_pulse_duration` exists and equals 2

### Phase 3.2: Modify ProbeConfiguration R6 Class
- [ ] Add `heat_pulse_duration` field to class definition
- [ ] Update `initialize()` method to accept heat_pulse_duration parameter
- [ ] Update `print()` method to show heat_pulse_duration
- [ ] Test: Create ProbeConfiguration object manually, verify field works

### Phase 3.3: Comment Out Old Functions (DON'T DELETE YET)
- [ ] Comment out `detect_probe_config()` function
- [ ] Comment out `detect_config_from_sensors()` function
- [ ] Comment out `get_standard_configs()` function
- [ ] Add comment: "# DEPRECATED - Removed in refactor. See load_probe_config() instead."
- [ ] Test: `devtools::load_all()` - no errors
- [ ] Test: `devtools::check()` - check for issues

### Session 2 Tests
- [ ] Update `tests/testthat/test-probe-config.R`
- [ ] Test: `load_probe_config("symmetrical")` works
- [ ] Test: `load_probe_config("asymmetrical")` works
- [ ] Test: `load_probe_config()` uses default
- [ ] Test: Override parameters work
- [ ] Test: heat_pulse_duration is read correctly
- [ ] Test: Invalid config name errors helpfully
- [ ] Run: `devtools::test_active_file()`
- [ ] All tests pass? If no, fix issues

**Session 2 Complete:** ✅ Yes / ❌ No

---

## Session 3: Update Core Functions (2-3 hours)

### Phase 4.1: Update calc_heat_pulse_velocity()

**File:** `R/04_heat_pulse_velocity_core.R`

- [ ] Backup: Copy function signature and first 20 lines
- [ ] Update function signature
  - [ ] Add `probe_config = NULL` parameter
  - [ ] Add `wood_properties = NULL` parameter
  - [ ] Add `diffusivity = NULL` parameter (convenience override)
  - [ ] Add `x = NULL` parameter (convenience override)
- [ ] Update function body (after parameter validation)
  - [ ] Load probe config (if NULL, use default; if character, load it)
  - [ ] Load wood properties (if NULL, use default; if character, load it)
  - [ ] Build default_params from configs:
    - [ ] `diffusivity` from wood_properties$thermal_diffusivity
    - [ ] `x` from probe_config$required_parameters$x
    - [ ] `tp_1` from probe_config$heat_pulse_duration (NEW!)
  - [ ] Merge with user parameters (preserve existing logic)
  - [ ] Apply individual overrides (diffusivity, x if provided)
- [ ] Update roxygen2 documentation
  - [ ] Document new parameters
  - [ ] Add examples showing config usage
  - [ ] Update parameter descriptions
- [ ] Test: `devtools::load_all()`
- [ ] Test in console:
  ```r
  sap_data <- read_sap_data("test_data.txt")
  results <- calc_heat_pulse_velocity(sap_data)  # Should work with defaults
  ```

### Phase 4.2: Update process_sap_data()

**File:** `R/14_processing_pipeline.R`

- [ ] Find line ~87: `probe_config <- detect_probe_config(sap_data)`
- [ ] **DELETE** that line
- [ ] Replace with new config loading logic:
  ```r
  # Load probe configuration
  if (is.null(probe_config)) {
    probe_config <- get_default_probe_config()
    message("Using default probe configuration (ICT SFM1x symmetric)")
  } else if (is.character(probe_config)) {
    probe_config <- load_probe_config(probe_config)
    message("Loaded probe configuration: ", probe_config$config_name)
  }

  # Load wood properties
  if (is.null(wood_properties)) {
    wood_properties <- get_default_wood_properties()
    message("Using default wood properties (generic softwood)")
  } else if (is.character(wood_properties)) {
    wood_properties <- load_wood_properties(wood_properties)
    message("Loaded wood properties: ", wood_properties$config_name)
  }
  ```
- [ ] Update function signature
  - [ ] Add `probe_config = NULL` parameter
  - [ ] Add `wood_properties = NULL` parameter
  - [ ] Add tree measurement parameters (dbh, sapwood_depth, etc.)
- [ ] Update roxygen2 documentation
- [ ] Test: `devtools::load_all()`
- [ ] Test: `process_sap_data(sap_data)` works with defaults

### Phase 4.3: Update Other Functions (if needed)

- [ ] Check `R/05_heat_pulse_velocity_advanced.R`
  - [ ] Any functions using probe_config? Update if needed
  - [ ] Any functions needing wood_properties? Update if needed
- [ ] Check `R/08_quality_control.R`
  - [ ] Use quality thresholds from wood_properties
  - [ ] Update quality check functions
- [ ] Check `R/11_flux_density.R`
  - [ ] May need wood properties for scaling
- [ ] Check `R/13_tree_water_use.R`
  - [ ] May need tree measurements from wood_properties

### Session 3 Tests
- [ ] Update `tests/testthat/test-heat-pulse-velocity.R`
  - [ ] Test default behavior (no configs specified)
  - [ ] Test with wood_properties = "eucalyptus"
  - [ ] Test with custom configs
  - [ ] Test parameter override hierarchy
- [ ] Update `tests/testthat/test-processing-pipeline.R`
  - [ ] Test pipeline with defaults
  - [ ] Test pipeline with custom configs
- [ ] Run: `devtools::test()`
- [ ] All tests pass? If no, fix issues

**Session 3 Complete:** ✅ Yes / ❌ No

---

## Session 4: Testing & Documentation (2-3 hours)

### Phase 5: Comprehensive Testing

#### Phase 5.1: Create Test Data Files
- [ ] Create `tests/testthat/test_probe_custom.yaml` (custom probe for testing)
- [ ] Create `tests/testthat/test_wood_custom.yaml` (custom wood for testing)

#### Phase 5.2: Test All Workflows
- [ ] Test Workflow 1: Simple default
  ```r
  sap_data <- read_sap_data("data.txt")
  results <- calc_heat_pulse_velocity(sap_data)
  ```
- [ ] Test Workflow 2: Override parameters
  ```r
  results <- calc_heat_pulse_velocity(sap_data,
    wood_properties = "eucalyptus",
    diffusivity = 0.0028)
  ```
- [ ] Test Workflow 3: Use different built-in config
  ```r
  results <- calc_heat_pulse_velocity(sap_data,
    probe_config = "asymmetrical",
    wood_properties = "pine")
  ```
- [ ] Test Workflow 4: Load custom YAML
  ```r
  results <- calc_heat_pulse_velocity(sap_data,
    probe_config = "path/to/custom.yaml",
    wood_properties = "path/to/wood.yaml")
  ```
- [ ] Test Workflow 5: Create config in R
  ```r
  config <- create_custom_probe_config(upstream_distance = 6)
  results <- calc_heat_pulse_velocity(sap_data, probe_config = config)
  ```
- [ ] Test Workflow 6: List available configs
  ```r
  list_available_probe_configs()
  list_available_wood_properties()
  ```

#### Phase 5.3: Test Error Handling
- [ ] Test: Invalid probe config name → helpful error message
- [ ] Test: Invalid wood property name → helpful error message
- [ ] Test: Malformed YAML file → helpful error message
- [ ] Test: Missing required fields → helpful error message

#### Phase 5.4: Run Full Test Suite
- [ ] Run: `devtools::test()`
- [ ] All tests pass?
- [ ] Run: `devtools::check()`
- [ ] 0 errors, 0 warnings, 0 notes?
- [ ] Run: `covr::package_coverage()` (optional)
- [ ] Coverage >80% for new functions?

### Phase 6: Documentation

#### Phase 6.1: Update Function Documentation
- [ ] Run: `devtools::document()`
- [ ] Check: man pages created for all new functions?
- [ ] Review: Examples work correctly?

#### Phase 6.2: Create Example YAML Templates
- [ ] Create `inst/configurations/probe_custom_example.yaml`
  - [ ] Well-commented template for users
  - [ ] Explain each field
- [ ] Create `inst/configurations/wood_custom_example.yaml`
  - [ ] Well-commented template for users
  - [ ] Explain each field

#### Phase 6.3: Update Existing Documentation
- [ ] Update `README.md`
  - [ ] Add "Configuration System" section
  - [ ] Show basic usage examples
  - [ ] Link to detailed guides
- [ ] Update `user_inputs.md`
  - [ ] Document probe_config parameter
  - [ ] Document wood_properties parameter
  - [ ] Document parameter override hierarchy
  - [ ] Show YAML structure

#### Phase 6.4: Create New Guides
- [ ] Create `CONFIGURATION_GUIDE.md`
  - [ ] How to use defaults
  - [ ] How to create custom YAML files
  - [ ] Parameter override hierarchy explained
  - [ ] Multi-tree workflows
  - [ ] Troubleshooting section
- [ ] Create `MIGRATION_GUIDE.md`
  - [ ] What changed
  - [ ] Old code → New code examples
  - [ ] Breaking changes list
  - [ ] FAQ

**Session 4 Complete:** ✅ Yes / ❌ No

---

## Session 5: Cleanup & Finalization (1 hour)

### Phase 7.1: Remove Deprecated Functions

**File:** `R/03_probe_configuration.R`

- [ ] Delete commented-out `detect_probe_config()` function
- [ ] Delete commented-out `detect_config_from_sensors()` function
- [ ] Delete commented-out `get_standard_configs()` function
- [ ] Delete backup file `03_probe_configuration_BACKUP.R`
- [ ] Run: `devtools::document()` (updates NAMESPACE)
- [ ] Test: `devtools::load_all()` - no errors

### Phase 7.2: Final Checks

- [ ] Run: `devtools::test()` - All tests pass?
- [ ] Run: `devtools::check()` - 0 errors, 0 warnings?
- [ ] Run: `devtools::build()` - Package builds successfully?
- [ ] Manual test: Try all 6 workflows in fresh R session
- [ ] Check: All example code in documentation works?
- [ ] Check: Error messages are clear and helpful?

### Phase 7.3: Update NEWS.md

- [ ] Add entry for this refactor:
  ```markdown
  # sapFluxR 0.X.0

  ## Major Changes

  ### Configuration System Refactor
  - **Breaking change:** Removed unreliable `detect_probe_config()` function
  - **New:** YAML-based configuration for probe and wood properties
  - **New:** Functions: `load_probe_config()`, `load_wood_properties()`
  - **New:** Sensible defaults - works out-of-the-box
  - **New:** Flexible parameter override system
  - **New:** Per-tree YAML configuration support

  ### New Features
  - Heat pulse duration now in probe YAML files
  - Quality thresholds now species-specific in wood YAMLs
  - Tree measurements can be stored in wood property YAMLs
  - Multi-tree workflows with per-tree configs

  ### Migration
  - See MIGRATION_GUIDE.md for transition help
  - Old code without explicit configs uses sensible defaults
  ```

### Phase 7.4: Final Review

- [ ] Read through CONFIGURATION_GUIDE.md - clear?
- [ ] Read through MIGRATION_GUIDE.md - helpful?
- [ ] Test example code in guides - works?
- [ ] Check all modified files for TODOs or FIXMEs
- [ ] All documentation complete?

**Session 5 Complete:** ✅ Yes / ❌ No

---

## Post-Implementation

### Optional: Create Vignette
- [ ] Create `vignettes/configuration-system.Rmd`
- [ ] Show real-world examples
- [ ] Build: `devtools::build_vignettes()`

### Git Commit (if using version control)
- [ ] Stage all changes
- [ ] Create comprehensive commit message
- [ ] Include breaking changes note

### User Communication
- [ ] Announce changes to users
- [ ] Provide migration guide link
- [ ] Answer questions

---

## Quick Status Check

**Current Phase:**
**Date Last Updated:**
**Next Task:**
**Blockers:**

---

## Notes & Issues

### Session 1 Notes:


### Session 2 Notes:


### Session 3 Notes:


### Session 4 Notes:


### Session 5 Notes:


### Issues Encountered:


---

## Success Criteria ✅

- [ ] Default workflow works (no configs specified)
- [ ] Built-in configs load by name
- [ ] Custom YAML files load by path
- [ ] Individual parameter overrides work
- [ ] Programmatic config creation works
- [ ] Multi-tree workflow works
- [ ] Parameter override hierarchy correct
- [ ] All tests pass (devtools::test())
- [ ] No warnings (devtools::check())
- [ ] All functions documented
- [ ] Examples work
- [ ] Clear error messages
- [ ] Comprehensive documentation
- [ ] Migration guide complete
- [ ] Example YAML templates provided

**ALL CRITERIA MET:** ✅ Yes / ❌ No

---

**Refactor Complete!** 🎉
