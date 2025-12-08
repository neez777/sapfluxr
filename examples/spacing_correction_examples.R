#!/usr/bin/env Rscript
# Unified Spacing Correction Examples
# Demonstrates all four spacing correction methods using the unified interface

library(sapfluxr)

# Setup: Load data and calculate velocities
# This section prepares the data for all examples below

cat("=== SETUP: Loading data and calculating velocities ===\n\n")

# Load raw data
hp_raw <- read_heat_pulse_data(
  "E:/SapFlow/Tree data/Old data/SX01O201.txt",
  show_progress = FALSE,
  trim_incomplete_days = TRUE
)

cat("Data loaded:", nrow(hp_raw$measurements), "measurements\n")

# Load and calculate wood properties
wood <- load_wood_properties("E:/SapFlow/Tree data/SX01O201.yaml")
wood <- calculate_wood_properties(wood)

cat("Wood properties loaded\n")
cat("  Thermal diffusivity:", wood$derived_properties$thermal_diffusivity_actual_cm2_s, "cm²/s\n")
cat("  Sapwood depth:", wood$sapwood_depth_cm, "cm\n\n")

# Load probe configuration
probe <- load_probe_config("probe_symmetrical")

cat("Probe configuration loaded\n")
cat("  Probe spacing:", probe$probe_spacing_cm, "cm\n")
cat("  Inner sensor depth:", probe$sensor_depths$inner_cm, "cm\n")
cat("  Outer sensor depth:", probe$sensor_depths$outer_cm, "cm\n\n")

# Calculate heat pulse velocity for multiple methods
vh_methods <- c("HRM", "MHR", "HRMXa", "HRMXb", "Tmax_Coh", "Tmax_Klu")
vh_results <- calc_heat_pulse_velocity(
  hp_raw,
  methods = vh_methods,
  wood_properties = wood,
  confirm_parameters = FALSE
)

cat("Velocity calculated for", length(vh_methods), "methods\n")
cat("  Total observations:", nrow(vh_results), "\n\n")

# Load weather data (for VPD example)
# Note: Replace with your actual weather data file
weather_file <- "E:/SapFlow/Weather/weather_data.csv"
if (file.exists(weather_file)) {
  weather <- read.csv(weather_file)
  cat("Weather data loaded for VPD example\n\n")
} else {
  cat("Weather data not found - VPD example will be skipped\n\n")
  weather <- NULL
}

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 1: PELT Method (Automatic Changepoint Detection)
# ============================================================================

cat("=== EXAMPLE 1: PELT Method (Automatic Detection) ===\n\n")

cat("Use case: First-time analysis, objective changepoint detection\n")
cat("Advantages: Fully automatic, reproducible, no prior knowledge needed\n")
cat("Limitations: May detect spurious changepoints in noisy data\n\n")

# Apply PELT spacing correction with default settings
result_pelt_default <- apply_spacing_correction(
  vh_data = vh_results,
  method = "pelt",
  hpv_method = "HRM",
  wood_properties = wood,
  verbose = TRUE
)

cat("\n--- Results Summary ---\n")
cat("Method used:", result_pelt_default$method_used, "\n")
cat("Changepoints detected:", length(result_pelt_default$changepoints), "\n")
if (length(result_pelt_default$changepoints) > 0) {
  cat("Changepoint dates:\n")
  print(result_pelt_default$changepoints)
}
cat("Number of segments:", nrow(result_pelt_default$segments), "\n\n")

# Compare before and after correction
hrm_outer_before <- vh_results[vh_results$method == "HRM" &
                                vh_results$sensor_position == "outer", ]
hrm_outer_after <- result_pelt_default$vh_corrected[
  result_pelt_default$vh_corrected$method == "HRM" &
  result_pelt_default$vh_corrected$sensor_position == "outer", ]

cat("Impact on HRM outer sensor:\n")
cat("  Before correction: mean Vh =",
    round(mean(hrm_outer_before$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
cat("  After correction:  mean Vh =",
    round(mean(hrm_outer_after$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
cat("  Difference:",
    round(mean(hrm_outer_after$Vh_cm_hr, na.rm = TRUE) -
          mean(hrm_outer_before$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n\n")

# Try different penalty parameter for comparison
cat("--- Trying different penalty parameter ---\n\n")

result_pelt_aic <- apply_spacing_correction(
  vh_data = vh_results,
  method = "pelt",
  hpv_method = "HRM",
  wood_properties = wood,
  penalty = "AIC",  # Less strict than default MBIC
  verbose = FALSE
)

cat("MBIC penalty:", length(result_pelt_default$changepoints), "changepoints\n")
cat("AIC penalty:", length(result_pelt_aic$changepoints), "changepoints\n")
cat("(AIC typically detects more changepoints than MBIC)\n\n")

# Try adjusting minimum segment length
result_pelt_short <- apply_spacing_correction(
  vh_data = vh_results,
  method = "pelt",
  hpv_method = "HRM",
  wood_properties = wood,
  min_segment_days = 3,  # Shorter segments allowed
  verbose = FALSE
)

cat("Default (7 days min):", length(result_pelt_default$changepoints), "changepoints\n")
cat("Short segments (3 days min):", length(result_pelt_short$changepoints), "changepoints\n\n")

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 2: Manual Method (User-Specified Changepoints)
# ============================================================================

cat("=== EXAMPLE 2: Manual Method (User-Specified Changepoints) ===\n\n")

cat("Use case: Known dates from field observations, refining automatic detection\n")
cat("Advantages: Incorporates field knowledge, precise control\n")
cat("Limitations: Requires prior knowledge, subjective\n\n")

# Use changepoints from PELT as starting point, then manually adjust
if (length(result_pelt_default$changepoints) > 0) {
  # Example: Use detected changepoints but adjust dates slightly
  manual_dates <- as.character(result_pelt_default$changepoints[1:min(2, length(result_pelt_default$changepoints))])

  cat("Using", length(manual_dates), "manually specified changepoint(s)\n")
  cat("Dates:", paste(manual_dates, collapse = ", "), "\n\n")

  result_manual <- apply_spacing_correction(
    vh_data = vh_results,
    method = "manual",
    hpv_method = "HRM",
    wood_properties = wood,
    manual_changepoints = manual_dates,
    verbose = TRUE
  )

  cat("\n--- Results Summary ---\n")
  cat("Method used:", result_manual$method_used, "\n")
  cat("Changepoints specified:", length(result_manual$changepoints), "\n")
  cat("Number of segments:", nrow(result_manual$segments), "\n\n")

  # Compare manual vs PELT results
  hrm_manual <- result_manual$vh_corrected[
    result_manual$vh_corrected$method == "HRM" &
    result_manual$vh_corrected$sensor_position == "outer", ]

  cat("Impact comparison:\n")
  cat("  PELT correction: mean Vh =",
      round(mean(hrm_outer_after$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
  cat("  Manual correction: mean Vh =",
      round(mean(hrm_manual$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
  cat("  Difference:",
      round(mean(hrm_manual$Vh_cm_hr, na.rm = TRUE) -
            mean(hrm_outer_after$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n\n")

} else {
  cat("No changepoints detected by PELT - skipping manual example\n\n")
}

# Example with manual baseline overrides
cat("--- Example with manual baseline overrides ---\n\n")
cat("Use case: You know the actual zero-flow velocities for each segment\n\n")

if (length(result_pelt_default$changepoints) > 0) {
  # Extract baselines from PELT result for demonstration
  outer_baselines <- result_pelt_default$correction_info$outer$baselines

  cat("Detected baselines from PELT:\n")
  print(outer_baselines)
  cat("\n")

  # Create manual override list (here we use detected values, but user could modify)
  baseline_overrides <- as.list(outer_baselines$baseline_vh)
  names(baseline_overrides) <- as.character(outer_baselines$segment)

  result_manual_baseline <- apply_spacing_correction(
    vh_data = vh_results,
    method = "manual",
    hpv_method = "HRM",
    wood_properties = wood,
    manual_changepoints = manual_dates,
    baseline_overrides_outer = baseline_overrides,
    verbose = FALSE
  )

  cat("Manual baseline override applied\n")
  cat("(In practice, you would modify these values based on field knowledge)\n\n")
}

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 3: VPD Method (Environmental Suitability)
# ============================================================================

cat("=== EXAMPLE 3: VPD Method (Environmental Suitability) ===\n\n")

if (!is.null(weather)) {
  cat("Use case: Identify correction periods based on low VPD (minimal transpiration)\n")
  cat("Advantages: Links to environmental drivers, physiological validation\n")
  cat("Limitations: Requires weather data, assumes VPD controls sap flow\n\n")

  # Apply VPD-based spacing correction with default threshold
  result_vpd_default <- apply_spacing_correction(
    vh_data = vh_results,
    method = "vpd",
    hpv_method = "HRM",
    wood_properties = wood,
    weather_data = weather,
    vpd_threshold = 0.5,  # kPa
    verbose = TRUE
  )

  cat("\n--- Results Summary ---\n")
  cat("Method used:", result_vpd_default$method_used, "\n")
  cat("VPD threshold:", 0.5, "kPa\n")
  cat("Changepoints detected:", length(result_vpd_default$changepoints), "\n")
  if (length(result_vpd_default$changepoints) > 0) {
    cat("Changepoint dates:\n")
    print(result_vpd_default$changepoints)
  }
  cat("Number of segments:", nrow(result_vpd_default$segments), "\n\n")

  # Compare VPD vs PELT changepoints
  cat("Comparison with PELT:\n")
  cat("  PELT changepoints:", length(result_pelt_default$changepoints), "\n")
  cat("  VPD changepoints:", length(result_vpd_default$changepoints), "\n")

  if (length(result_pelt_default$changepoints) > 0 &&
      length(result_vpd_default$changepoints) > 0) {
    cat("\n  Overlap in detected dates:\n")
    # Check if any dates are within 3 days of each other
    for (vpd_date in result_vpd_default$changepoints) {
      closest_pelt <- result_pelt_default$changepoints[
        which.min(abs(as.numeric(result_pelt_default$changepoints - vpd_date)))
      ]
      diff_days <- abs(as.numeric(vpd_date - closest_pelt))
      if (diff_days <= 3) {
        cat("    VPD:", as.character(vpd_date),
            "≈ PELT:", as.character(closest_pelt),
            "(", diff_days, "days apart)\n")
      }
    }
  }
  cat("\n")

  # Try different VPD thresholds
  cat("--- Testing different VPD thresholds ---\n\n")

  result_vpd_conservative <- apply_spacing_correction(
    vh_data = vh_results,
    method = "vpd",
    hpv_method = "HRM",
    wood_properties = wood,
    weather_data = weather,
    vpd_threshold = 0.3,  # Very conservative
    verbose = FALSE
  )

  result_vpd_permissive <- apply_spacing_correction(
    vh_data = vh_results,
    method = "vpd",
    hpv_method = "HRM",
    wood_properties = wood,
    weather_data = weather,
    vpd_threshold = 0.8,  # More permissive
    verbose = FALSE
  )

  cat("VPD threshold comparison:\n")
  cat("  0.3 kPa (conservative):",
      length(result_vpd_conservative$changepoints), "changepoints\n")
  cat("  0.5 kPa (default):",
      length(result_vpd_default$changepoints), "changepoints\n")
  cat("  0.8 kPa (permissive):",
      length(result_vpd_permissive$changepoints), "changepoints\n")
  cat("(Lower threshold = fewer suitable periods = fewer changepoints)\n\n")

} else {
  cat("Weather data not available - VPD example skipped\n")
  cat("To run this example, provide weather data with VPD or humidity + temperature\n\n")
}

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 4: Heartwood Method (Continuous Reference)
# ============================================================================

cat("=== EXAMPLE 4: Heartwood Method (Continuous Reference) ===\n\n")

cat("Use case: Inner sensor in heartwood, continuous monitoring\n")
cat("Advantages: No segmentation, continuous correction, real-time capable\n")
cat("Limitations: Requires inner sensor beyond sapwood depth\n\n")

# First check if heartwood reference is available
heartwood_check <- check_heartwood_reference_available(
  vh_data = vh_results,
  wood_properties = wood,
  probe_config = probe
)

cat("Heartwood reference check:\n")
cat("  Available:", heartwood_check$available, "\n")
cat("  Inner sensor depth:", probe$sensor_depths$inner_cm, "cm\n")
cat("  Sapwood depth:", wood$sapwood_depth_cm, "cm\n")
cat("  Margin:", probe$sensor_depths$inner_cm - wood$sapwood_depth_cm, "cm\n")

if (heartwood_check$available) {
  cat("  Status: Inner sensor is sufficiently in heartwood\n\n")

  # Apply heartwood reference correction
  result_heartwood <- apply_spacing_correction(
    vh_data = vh_results,
    method = "heartwood",
    hpv_method = "HRM",
    wood_properties = wood,
    probe_config = probe,
    verbose = TRUE
  )

  cat("\n--- Results Summary ---\n")
  cat("Method used:", result_heartwood$method_used, "\n")
  cat("Changepoints:", ifelse(is.null(result_heartwood$changepoints),
                               "None (continuous correction)",
                               length(result_heartwood$changepoints)), "\n")
  cat("Segments:", ifelse(is.null(result_heartwood$segments),
                          "None (continuous correction)",
                          nrow(result_heartwood$segments)), "\n\n")

  # Compare heartwood vs PELT results
  hrm_heartwood <- result_heartwood$vh_corrected[
    result_heartwood$vh_corrected$method == "HRM" &
    result_heartwood$vh_corrected$sensor_position == "outer", ]

  cat("Impact comparison:\n")
  cat("  Uncorrected: mean Vh =",
      round(mean(hrm_outer_before$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
  cat("  PELT correction: mean Vh =",
      round(mean(hrm_outer_after$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n")
  cat("  Heartwood correction: mean Vh =",
      round(mean(hrm_heartwood$Vh_cm_hr, na.rm = TRUE), 3), "cm/hr\n\n")

  cat("Key difference: Heartwood method provides continuous correction\n")
  cat("without segmentation artifacts at changepoint boundaries.\n\n")

} else {
  cat("  Status:", heartwood_check$reason, "\n\n")
  cat("Heartwood method not available for this dataset.\n")
  cat("Options:\n")
  cat("  1. Verify sapwood depth measurement is correct\n")
  cat("  2. Use PELT, manual, or VPD method instead\n")
  cat("  3. If sapwood depth was overestimated, update wood properties:\n")
  cat("     wood$sapwood_depth_cm <- [corrected_value]\n\n")
}

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 5: Method Comparison Workflow
# ============================================================================

cat("=== EXAMPLE 5: Comprehensive Method Comparison ===\n\n")

cat("Recommended workflow: Compare multiple methods to select best approach\n\n")

# Collect all results
all_results <- list(
  uncorrected = list(
    name = "Uncorrected",
    data = vh_results
  ),
  pelt = list(
    name = "PELT (MBIC)",
    data = result_pelt_default$vh_corrected
  )
)

if (exists("result_manual") && !is.null(result_manual)) {
  all_results$manual <- list(
    name = "Manual",
    data = result_manual$vh_corrected
  )
}

if (exists("result_vpd_default") && !is.null(result_vpd_default)) {
  all_results$vpd <- list(
    name = "VPD (0.5 kPa)",
    data = result_vpd_default$vh_corrected
  )
}

if (exists("result_heartwood") && !is.null(result_heartwood)) {
  all_results$heartwood <- list(
    name = "Heartwood",
    data = result_heartwood$vh_corrected
  )
}

# Calculate mean velocities for each method
cat("Mean HRM outer sensor velocities:\n")
cat(strrep("-", 60), "\n")

comparison_table <- data.frame(
  Method = character(),
  Mean_Vh = numeric(),
  Diff_from_uncorrected = numeric(),
  Percent_change = numeric(),
  stringsAsFactors = FALSE
)

uncorrected_mean <- mean(hrm_outer_before$Vh_cm_hr, na.rm = TRUE)

for (method_name in names(all_results)) {
  method_data <- all_results[[method_name]]$data
  hrm_outer <- method_data[method_data$method == "HRM" &
                           method_data$sensor_position == "outer", ]
  mean_vh <- mean(hrm_outer$Vh_cm_hr, na.rm = TRUE)
  diff <- mean_vh - uncorrected_mean
  pct <- (diff / uncorrected_mean) * 100

  comparison_table <- rbind(comparison_table, data.frame(
    Method = all_results[[method_name]]$name,
    Mean_Vh = round(mean_vh, 4),
    Diff_from_uncorrected = round(diff, 4),
    Percent_change = round(pct, 2),
    stringsAsFactors = FALSE
  ))
}

print(comparison_table, row.names = FALSE)
cat("\n")

# Provide recommendations based on results
cat("Method selection recommendations:\n")
cat(strrep("-", 60), "\n")
cat("1. If methods agree closely (< 5% difference):\n")
cat("   → Any method is appropriate, use PELT for objectivity\n\n")
cat("2. If methods diverge substantially (> 10% difference):\n")
cat("   → Investigate data quality and underlying causes\n")
cat("   → Consider manual method with field observations\n")
cat("   → Review changepoint timing and segment baselines\n\n")
cat("3. For long-term monitoring (if heartwood available):\n")
cat("   → Prefer heartwood method to avoid segmentation\n\n")
cat("4. For environmental validation:\n")
cat("   → Compare PELT with VPD to check physiological plausibility\n\n")
cat("5. For publications requiring reproducibility:\n")
cat("   → Document PELT parameters clearly\n")
cat("   → Report sensitivity to penalty and min_segment_days\n\n")

cat(strrep("=", 80), "\n\n")

# ============================================================================
# EXAMPLE 6: Integration with Calibration Workflow
# ============================================================================

cat("=== EXAMPLE 6: Integration with Method Calibration ===\n\n")

cat("Demonstrating integration with dual-location calibration workflow\n\n")

# Apply spacing correction first
cat("Step 1: Apply spacing correction (PELT method)\n")
correction_result <- apply_spacing_correction(
  vh_data = vh_results,
  method = "pelt",
  hpv_method = "HRM",
  wood_properties = wood,
  verbose = FALSE
)
cat("  Spacing correction complete\n\n")

# Then apply late calibration + sDMA
cat("Step 2: Apply late calibration + sDMA\n")
late_result <- apply_late_calibration_sdma(
  vh_corrected = correction_result$vh_corrected,
  primary_method = "HRM",
  secondary_methods = c("MHR", "HRMXb"),
  sdma_methods = "MHR",
  sensor_position = "outer"
)
cat("  Calibration complete\n\n")

# Summary
cat("Final dataset includes:\n")
cat("  - Spacing-corrected individual methods:",
    paste(unique(correction_result$vh_corrected$method), collapse = ", "), "\n")
cat("  - Calibrated methods:",
    paste(setdiff(unique(late_result$vh_with_sdma$method),
                 unique(correction_result$vh_corrected$method)),
          collapse = ", "), "\n\n")

cat("All methods ready for flux density conversion\n\n")

cat(strrep("=", 80), "\n\n")

cat("=== EXAMPLES COMPLETE ===\n\n")

cat("Summary of demonstrated features:\n")
cat("  ✓ PELT automatic changepoint detection\n")
cat("  ✓ Manual changepoint specification\n")
if (!is.null(weather)) {
  cat("  ✓ VPD-based environmental suitability\n")
}
if (heartwood_check$available) {
  cat("  ✓ Heartwood continuous reference\n")
}
cat("  ✓ Parameter sensitivity testing\n")
cat("  ✓ Method comparison and validation\n")
cat("  ✓ Integration with calibration workflow\n\n")

cat("Next steps:\n")
cat("  1. Review changepoint timing and segment boundaries\n")
cat("  2. Visualise corrected velocities with plot functions\n")
cat("  3. Select most appropriate method for your data\n")
cat("  4. Proceed with flux density conversion\n\n")
