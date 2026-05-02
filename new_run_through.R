# Run these commands FIRST, then wait for installation to complete:
setwd("E:/R/project/sapfluxr")
devtools::load_all()
devtools::document()
devtools::install()

# After installation completes, load the package:
library(sapfluxr)


# ============================================================================
# COMPLETE SAP FLOW ANALYSIS PIPELINE
# ============================================================================
# This script demonstrates the full workflow from raw heat pulse data
# to daily tree water use estimates (Steps 1-9)
# ============================================================================

# ============================================================================
# 1. DATA IMPORT
# ============================================================================

# Import data with progress bar and trim incomplete days
raw_file_latest <- "E:/run_through_data/SX01O201.txt"
#raw_file_latest <- "E:/SapFlow/Tree data/SX01O201.txt"
hp_raw <- read_heat_pulse_data(
  raw_file_latest,
  show_progress = TRUE,
  trim_incomplete_days = TRUE,
  timezone = "Australia/Perth"   # re-labels UTC-stamped local-time data correctly
)

weather_data <- read_weather_data(
  "E:/SapFlow/sap_flow_data/MU_BankWoodland_110925.csv",
  timezone = "Australia/Perth"
)
vpd_data <- calc_vpd(weather_data)

# ALTERNATIVE: Download weather directly from SILO (Australian BOM gridded data)
# No API key required. Requires site coordinates — see site_location in Section 2.
# weather_data <- download_silo_weather(
#   latitude   = -32.069,   # decimal degrees, negative = south
#   longitude  = 115.834,
#   start_date = "2024-01-01",
#   end_date   = "2025-12-31"
# )
# vpd_data <- calc_vpd(weather_data)

# ============================================================================
# 2. WOOD PROPERTIES & PROBE CONFIGURATION
# ============================================================================

# Load eucalyptus configuration (using defaults)
#wood <- load_wood_properties("eucalyptus")
wood <- load_wood_properties("E:/SapFlow/Tree data/SX01O201_edited.yaml")
print(wood)

# Calculate derived wood properties (needed for flux density conversion)
wood <- calculate_wood_properties(wood)

# OPTIONAL: Add site coordinates — enables download_silo_weather() and
# dynamic predawn window calculation (see Section 15)
wood$site_location <- list(
 latitude  = -32.069,         # decimal degrees, negative = south
 longitude = 115.834,
 timezone  = "Australia/Perth"
)

# Load 5mm probe (symmetrical)
probe <- load_probe_config("symmetrical")
print(probe$yaml_data$probe$upstream_distance)  # Verify 5mm spacing


# ============================================================================
# 3. CALCULATE HEAT PULSE VELOCITY (ALL METHODS)
# ============================================================================

# PRE-PULSE BASELINE METHOD (default: mean_30s — 30-second mean, backwards compatible)
# Change before calling calc_heat_pulse_velocity() if needed:
#   set_analysis_param("baseline.method", "mean_3s")         # Last 3 s before pulse
#   set_analysis_param("baseline.method", "slope_intercept") # OLS drift correction
# Reset to default with:
#   set_analysis_param("baseline.method", "mean_30s")

# Calculate all core methods
vh_methods <- c("HRM", "MHR", "Tmax_Coh", "Tmax_Klu")
vh_results <- calc_heat_pulse_velocity(
  hp_raw,
  methods = vh_methods,
  wood_properties = wood,
  baseline_method = "mean_3s",
  confirm_parameters = FALSE
)

# View results summary
print(vh_results)
cat("\nMeasurements per method:\n")
print(table(vh_results$method))

# ============================================================================
# 3b. TIMEZONE ALIGNMENT DIAGNOSTIC
# ============================================================================
# Run this block to check whether HPV and VPD are in the same effective timezone.
# Loggers often record local time but the import stores datetimes with a UTC label.
# If that is the case, the predawn window (hours 2–6) already lines up without any
# timezone conversion — and calling find_dual_stable_periods() with timezone = ...
# would SHIFT the window to the wrong hours.
#
# Interpretation:
#   - VPD should peak around hour 12–14 (midday)
#   - HPV should be near-zero during the shaded predawn band
#   - If VPD peaks around hour 4 instead, your data is genuinely stored in UTC
#     and you should re-import with timezone = "Australia/Perth" (see below)

diag_tz   <- attr(vh_results$datetime, "tzone") %||% "UTC"
diag_days <- sort(na.omit(unique(as.Date(vh_results$datetime))))
diag_s    <- as.POSIXct(paste(format(diag_days[2], "%Y-%m-%d"), "00:00:00"), tz = diag_tz)
diag_e    <- as.POSIXct(paste(format(diag_days[3], "%Y-%m-%d"), "23:59:59"), tz = diag_tz)

diag_vh  <- vh_results[vh_results$datetime >= diag_s & vh_results$datetime <= diag_e &
                        vh_results$method == "HRM" & vh_results$sensor_position == "outer", ]
diag_vpd <- vpd_data[vpd_data$datetime >= diag_s & vpd_data$datetime <= diag_e, ]

cat("── Timezone diagnostic ──────────────────────────────\n")
cat("HPV datetime stored as:  ", diag_tz, "\n")
cat("VPD datetime stored as:  ", attr(vpd_data$datetime, "tzone") %||% "UTC", "\n")
if (nrow(diag_vpd) > 0) {
  vpd_peak_hr <- lubridate::hour(diag_vpd$datetime[which.max(diag_vpd$vpd_kpa)])
  cat(sprintf("VPD peak hour (as stored): %02d:00", vpd_peak_hr), "\n")
  cat("  → Expect ~12 if data is local time stored as UTC-labelled\n")
  cat("  → Expect ~04 if data is genuinely UTC (UTC+8 site, noon = 04:00 UTC)\n")
}
cat("─────────────────────────────────────────────────────\n")

if (requireNamespace("ggplot2",   quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE)) {

  diag_uniq_dates <- sort(na.omit(unique(c(as.Date(diag_vh$datetime),
                                           as.Date(diag_vpd$datetime)))))
  shade_df <- data.frame(
    xmin = as.POSIXct(paste(format(diag_uniq_dates, "%Y-%m-%d"), "02:00:00"), tz = diag_tz),
    xmax = as.POSIXct(paste(format(diag_uniq_dates, "%Y-%m-%d"), "06:00:00"), tz = diag_tz)
  )

  p_vh <- ggplot2::ggplot(diag_vh, ggplot2::aes(x = datetime, y = Vs_cm_hr)) +
    ggplot2::geom_rect(data = shade_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = "navy", alpha = 0.08, inherit.aes = FALSE) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.8) +
    ggplot2::labs(x = NULL, y = "HPV  Vs_cm_hr (cm/hr)",
                  subtitle = paste("Stored tz:", diag_tz,
                                   "| Blue shading = hours 02:00–06:00 as stored")) +
    ggplot2::theme_classic()

  p_vpd <- ggplot2::ggplot(diag_vpd, ggplot2::aes(x = datetime, y = vpd_kpa)) +
    ggplot2::geom_rect(data = shade_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = "navy", alpha = 0.08, inherit.aes = FALSE) +
    ggplot2::geom_line(colour = "darkorange", linewidth = 0.8) +
    ggplot2::labs(x = sprintf("Datetime (label: %s)", diag_tz), y = "VPD (kPa)") +
    ggplot2::theme_classic()

  print(patchwork::wrap_plots(p_vh, p_vpd, ncol = 1) +
    patchwork::plot_annotation(
      title   = "Timezone alignment diagnostic — days 2 & 3 of dataset",
      caption = paste(
        "If VPD peaks at midday and HPV is near-zero inside the shading → data is local time",
        "(no timezone= arg needed in find_dual_stable_periods).\n",
        "If VPD peaks ~04:00 and HPV activity falls outside the shading → data is genuine UTC",
        "→ re-import with timezone = \"Australia/Perth\" (see below)."
      )
    )
  )
} else {
  message("Install patchwork for the diagnostic plot: install.packages('patchwork')")
}

# ── If the diagnostic shows data IS local time (VPD peaks ~12:00) ────────────
# The fix is to correctly label the timezone on import so all downstream code
# (including find_dual_stable_periods) works without a timezone= argument:
#
#   hp_raw       <- read_heat_pulse_data(raw_file, timezone = "Australia/Perth")
#   weather_data <- read_weather_data(weather_file, timezone = "Australia/Perth")
#   vpd_data     <- calc_vpd(weather_data)   # inherits timezone from weather_data
#
# Then in find_dual_stable_periods(), omit timezone= (or leave it NULL).
# ─────────────────────────────────────────────────────────────────────────────

# ============================================================================
# 4. QUICK VISUAL CHECKS
# ============================================================================

# Timeseries with quality markers
plot_vh_timeseries(
  vh_results,
  sensor_position = "outer",
  show_quality_marker = TRUE
)

# Method comparison (HRM vs MHR)
plot_method_comparison(
  vh_results,
  method1 = "HRM",
  method2 = "MHR",
  sensor_position = "outer"
)

# Individual heat pulse trace (pulse #350)
plot_heat_pulse_trace(
  hp_raw,
  vh_results,
  pulse_id = 1272,
  show_methods = c("HRM", "MHR"),
  sensor_position = "both"
)


# ============================================================================
# 5. QUALITY CONTROL SUMMARY
# ============================================================================

# Check quality flags
cat("\nOverall quality flag distribution:\n")
print(table(vh_results$quality_flag))

cat("\nQuality flags by method:\n")
print(table(vh_results$method, vh_results$quality_flag))

# Examine bad results
bad_results <- vh_results[vh_results$quality_flag != "OK", ]
cat("\nBad results by quality flag and method:\n")
print(table(bad_results$quality_flag, bad_results$method))


# ============================================================================
# 6. SPACING CORRECTION (NEW 2-STEP ARCHITECTURE)
# ============================================================================
# Spacing correction is now a two-step process:
#   Step 1: IDENTIFICATION - Find zero-flow anchor points (PELT, Dual-Stable, or Manual)
#   Step 2: APPLICATION    - Apply the correction (choose Model and Math)

# ----------------------------------------------------------------------------
# STEP 1: IDENTIFICATION (PELT Automatic Detection)
# ----------------------------------------------------------------------------
cat("\n=== Step 1: Identifying zero-flow anchor points (PELT) ===\n")

# Run PELT identification to get anchor dates
# Passing the full vh_results directly - it calculates minima internally
anchors_pelt <- detect_changepoints(
  vh_data = vh_results,
  sensor_position = "outer",
  hpv_method = "HRM",
  penalty = "BIC",
  min_segment_days = 7
)

# Plot changepoints interactively
if (requireNamespace("plotly", quietly = TRUE)) {
  p_cpt <- plot_changepoints_interactive(
    daily_min = anchors_pelt$daily_min,
    changepoints = anchors_pelt$changepoints,
    segments = anchors_pelt$segments,
    title = "Daily Minimum Velocities with Changepoints (Outer Sensor)"
  )
  print(p_cpt)
}

# ----------------------------------------------------------------------------
# STEP 2: APPLICATION (Unified Interface)
# ----------------------------------------------------------------------------
cat("\n=== Step 2: Applying spacing correction (Segment + Burgess) ===\n")

# Options for apply_spacing_correction:
#   offset_model:    "segment" (step-wise) or "gradient" (smooth)
#   correction_math: "burgess" (physics)   or "linear"   (simple shift)

vh_results <- apply_spacing_correction(
  vh_data = vh_results,
  changepoints = anchors_pelt$changepoints,
  offset_model = "segment",
  correction_math = "burgess",
  sensor_position = "both",
  hpv_method = "HRM",
  wood_properties = wood,
  probe_spacing = probe$probe_spacing,
  verbose = TRUE
)

# Inspect results (stored as attrs on vh_results)
cat("\nCorrection applied to rows:", sum(vh_results$spacing_correction_applied, na.rm = TRUE), "\n")

# Compare before/after using diagnostic columns
cat("\n==== SPACING CORRECTION IMPACT ====\n")
hrm_outer <- vh_results[vh_results$method == "HRM" & vh_results$sensor_position == "outer", ]
cat("HRM Outer sensor:\n")
cat("  Before (raw):               mean Vh_cm_hr_raw =", round(mean(hrm_outer$Vh_cm_hr_raw, na.rm = TRUE), 3), "cm/hr\n")
cat("  After  (spacing-corrected): mean Vs_cm_hr     =", round(mean(hrm_outer$Vs_cm_hr, na.rm = TRUE), 3), "cm/hr\n")

# Plot spacing-corrected timeseries
plot_vh_timeseries(vh_results, sensor_position = "outer")

# Plot side-by-side comparison of correction stages
plot_correction_steps(
  vh_results,
  sensor_position = "outer",
  stages = c("raw", "spacing")
)


# ----------------------------------------------------------------------------
# ALTERNATIVE: DUAL-CRITERION (DYNAMIC) WITH 4 PERMUTATIONS
# ----------------------------------------------------------------------------
# Identifies anchors where BOTH sap flow AND VPD are simultaneously stable using
# dynamic dawn calculation, then tests all 4 offset and math permutations.

cat("\n=== Alternative: Dual-Criterion (Dynamic) with 4 Permutations ===\n")

# 1. Identify high-confidence anchors using dynamic pre-dawn (-4 to -0.5 hrs)
dual_results <- find_dual_stable_periods(
  vh_data        = vh_results,
  weather_data   = vpd_data,
  vh_col         = "Vs_cm_hr", # Use current best estimate
  predawn_window = c(4, 0.5),  # 4 to 0.5 hours before dawn
  mode           = "dynamic",
  site_location  = wood$site_location, # Dawn times automatically calculated!
  timezone       = NULL,       # Avoid with_tz shifts
  vpd_threshold  = 0.5,
  vh_threshold   = 2.0
)

# Define the 4 permutations to test
permutations <- list(
  list(model = "segment", math = "linear"),
  list(model = "segment", math = "burgess"),
  list(model = "gradient", math = "linear"),
  list(model = "gradient", math = "burgess")
)

# Loop through permutations, apply correction, and plot
for (perm in permutations) {
  cat(sprintf("\nTesting Permutation: %s / %s\n", toupper(perm$model), toupper(perm$math)))
  
  # Set up anchors format based on model
  if (perm$model == "segment") {
    anchors <- dual_results$changepoints$timestamp
  } else {
    anchors <- dual_results$changepoints
  }
  
  # Apply correction
  vh_test <- apply_spacing_correction(
    vh_data = vh_results,
    changepoints = anchors,
    offset_model = perm$model,
    correction_math = perm$math,
    sensor_position = "both",
    hpv_method = "HRM",
    wood_properties = wood,
    probe_spacing = probe$probe_spacing,
    verbose = FALSE
  )
  
  # Plot side-by-side comparison of correction stages
  if (requireNamespace("plotly", quietly = TRUE)) {
    p <- plot_correction_steps(
      vh_test,
      sensor_position = "outer",
      stages = c("raw", "spacing")
    )
    
    # Add title suffix for the specific permutation
    p <- p %>% plotly::layout(
      title = sprintf("Spacing Correction: %s Model / %s Math", 
                     tools::toTitleCase(perm$model), tools::toTitleCase(perm$math))
    )
    
    print(p)
    if (interactive()) Sys.sleep(1) # Pause slightly between interactive plots
  }
}

# ============================================================================
# 6B. WOUND CORRECTION
# ============================================================================
# Applied BEFORE calibration and sDMA so that the calibration regression
# is fitted on wound-corrected values and Peclet numbers are re-derived from
# the same corrected values before the sDMA switching threshold is applied.

vh_results <- apply_wound_correction(
  vh_results,
  wood_properties = wood,
  confirm_parameters = FALSE
  # wound_history = wound_history,  # Uncomment if reinstallations occurred
  # verbose = TRUE
)

cat("\nMethods after wound correction:\n")
print(unique(vh_results$method))

cat("\nWound correction impact (HRM outer):\n")
hrm_outer <- vh_results[vh_results$method == "HRM" & vh_results$sensor_position == "outer", ]
cat("  Before (spacing-corrected): mean Vh_cm_hr_sc =", round(mean(hrm_outer$Vh_cm_hr_sc, na.rm = TRUE), 3), "cm/hr\n")
cat("  After  (wound-corrected):   mean Vh_cm_hr_wc =", round(mean(hrm_outer$Vh_cm_hr_wc, na.rm = TRUE), 3), "cm/hr\n")

# Plot side-by-side comparison of correction stages (Raw vs Spacing vs Wound)
plot_correction_steps(
  vh_results,
  sensor_position = "outer",
  stages = c("raw", "spacing", "wound")
)


# ============================================================================
# 7E. SEGMENTED REGRESSION METHOD COMPARISON (NEW APPROACH)
# ============================================================================
# This section demonstrates the new segmented regression approach for
# identifying method divergence points (replacing R² optimization)

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("SEGMENTED REGRESSION: STATISTICAL BREAKPOINT DETECTION\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\nThis approach uses piecewise linear regression to statistically\n")
cat("identify THE breakpoint where methods diverge (hockey stick pattern)\n")
cat("\n")

# Compare HRM vs MHR using segmented regression
cat("\n--- Comparing HRM vs MHR ---\n")
segmented_mhr <- tryCatch({
  compare_methods_segmented(
    vh_results,
    primary_method = "HRM",
    secondary_method = "MHR",
    sensor_position = "outer",
    verbose = TRUE
  )
}, error = function(e) {
  cat("\n[SKIP] Segmented comparison failed for MHR:", e$message, "\n")
  NULL
})

# View the segmented regression plot
if (!is.null(segmented_mhr) && !is.null(segmented_mhr$plots)) {
  print(segmented_mhr$plots$segmented_plot)

  # Also show residuals plot
  if (!is.null(segmented_mhr$plots$residuals_plot)) {
    print(segmented_mhr$plots$residuals_plot)
  }
}

# Compare HRM vs Tmax_Coh using segmented regression
cat("\n--- Comparing HRM vs Tmax_Coh ---\n")
segmented_tmax <- tryCatch({
  compare_methods_segmented(
    vh_results,
    primary_method = "HRM",
    secondary_method = "Tmax_Coh",
    sensor_position = "outer",
    verbose = TRUE
  )
}, error = function(e) {
  cat("\n[SKIP] Segmented comparison failed for Tmax_Coh:", e$message, "\n")
  NULL
})

# View the segmented regression plot
if (!is.null(segmented_tmax) && !is.null(segmented_tmax$plots)) {
  print(segmented_tmax$plots$segmented_plot)
}

# Summary comparison of breakpoints
cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("BREAKPOINT COMPARISON SUMMARY\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\n")

comparison_table <- data.frame(
  Method = c("MHR", "Tmax_Coh"),
  Breakpoint = c(
    if (isTRUE(segmented_mhr$converged)) round(segmented_mhr$breakpoint, 2) else NA,
    if (isTRUE(segmented_tmax$converged)) round(segmented_tmax$breakpoint, 2) else NA
  ),
  CI_Lower = c(
    if (isTRUE(segmented_mhr$converged)) round(segmented_mhr$breakpoint_ci[1], 2) else NA,
    if (isTRUE(segmented_tmax$converged)) round(segmented_tmax$breakpoint_ci[1], 2) else NA
  ),
  CI_Upper = c(
    if (isTRUE(segmented_mhr$converged)) round(segmented_mhr$breakpoint_ci[2], 2) else NA,
    if (isTRUE(segmented_tmax$converged)) round(segmented_tmax$breakpoint_ci[2], 2) else NA
  ),
  R_squared = c(
    if (isTRUE(segmented_mhr$converged)) round(segmented_mhr$r_squared, 4) else NA,
    if (isTRUE(segmented_tmax$converged)) round(segmented_tmax$r_squared, 4) else NA
  ),
  Davies_p = c(
    if (isTRUE(segmented_mhr$converged)) format.pval(segmented_mhr$davies_test, digits = 3) else NA,
    if (isTRUE(segmented_tmax$converged)) format.pval(segmented_tmax$davies_test, digits = 3) else NA
  ),
  stringsAsFactors = FALSE
)

print(comparison_table)

cat("\n")
cat("KEY FINDINGS:\n")
cat("-------------\n")

# Identify which method has the highest breakpoint
if (all(!is.na(comparison_table$Breakpoint))) {
  max_breakpoint_idx <- which.max(comparison_table$Breakpoint)
  min_breakpoint_idx <- which.min(comparison_table$Breakpoint)

  cat("- Highest breakpoint:", comparison_table$Method[max_breakpoint_idx],
      "at", comparison_table$Breakpoint[max_breakpoint_idx], "cm/hr\n")
  cat("  (This method is valid over the widest range)\n\n")

  cat("- Lowest breakpoint:", comparison_table$Method[min_breakpoint_idx],
      "at", comparison_table$Breakpoint[min_breakpoint_idx], "cm/hr\n")
  cat("  (This method diverges earliest from HRM)\n\n")
}

# Check for significant breakpoints
significant <- comparison_table$Davies_p != "NA" &
               as.numeric(gsub("[<>]", "", comparison_table$Davies_p)) < 0.05

if (any(significant, na.rm = TRUE)) {
  cat("- Statistically significant breakpoints (p < 0.05):\n")
  for (i in which(significant)) {
    cat("  *", comparison_table$Method[i], "\n")
  }
  cat("\n")
}

cat("\n")
cat("INTERPRETATION:\n")
cat("---------------\n")
cat("The breakpoint represents the maximum velocity where HRM maintains\n")
cat("a linear relationship with the secondary method. Above this point,\n")
cat("HRM begins to underestimate compared to the secondary method.\n")
cat("\n")
cat("For calibration and method switching (sDMA), use the breakpoint as\n")
cat("the threshold where you transition from HRM to the secondary method.\n")
cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\n")

# ============================================================================
# 7C. CALIBRATE MHR TO HRM SCALE (CANONICAL PIPELINE WORKFLOW)
# ============================================================================
# Uses breakpoint detected by segmented regression above.
# This matches the workflow in the Shiny app (mod_6a_calibration.R).

# Build calibration objects for outer and inner sensors
cal_mhr_outer <- NULL
cal_mhr_inner <- NULL

if (isTRUE(segmented_mhr$converged)) {

  cat("\n--- Calibrating MHR (outer) to HRM scale ---\n")
  cal_mhr_outer <- tryCatch(
    calibrate_method_to_primary(
      vh_results,
      primary_method    = "HRM",
      secondary_method  = "MHR",
      sensor_position   = "outer",
      threshold_velocity = segmented_mhr$breakpoint,
      verbose = TRUE
    ),
    error = function(e) { cat("[WARN] outer calibration failed:", e$message, "\n"); NULL }
  )

  cat("\n--- Calibrating MHR (inner) to HRM scale ---\n")
  cal_mhr_inner <- tryCatch(
    calibrate_method_to_primary(
      vh_results,
      primary_method    = "HRM",
      secondary_method  = "MHR",
      sensor_position   = "inner",
      threshold_velocity = segmented_mhr$breakpoint,
      verbose = TRUE
    ),
    error = function(e) { cat("[WARN] inner calibration failed:", e$message, "\n"); NULL }
  )

}

# Apply calibration directly — calibrate_method_to_primary returns a
# method_calibration object that pairs with transform_secondary_method.
if (inherits(cal_mhr_outer, "method_calibration")) {
  vh_results <- tryCatch(
    transform_secondary_method(vh_results, calibration = cal_mhr_outer),
    error = function(e) { cat("[WARN] outer transform failed:", e$message, "\n"); vh_results }
  )
}
if (inherits(cal_mhr_inner, "method_calibration")) {
  vh_results <- tryCatch(
    transform_secondary_method(vh_results, calibration = cal_mhr_inner),
    error = function(e) { cat("[WARN] inner transform failed:", e$message, "\n"); vh_results }
  )
}
cat("\nCalibration applied. Methods in data:\n")
print(unique(vh_results$method))

# ============================================================================
# 7D. sDMA SWITCHING (HRM + MHR)
# ============================================================================

# Recalculate Peclet number from the wound-corrected (and calibrated) velocities
# before applying sDMA switching, so the switching threshold is applied
# to physically meaningful values.
vh_results <- recalculate_peclet(
  vh_results,
  wood_properties = wood,
  probe_config    = probe
)

cat("\n--- Applying sDMA switching (Peclet threshold = 1.0) ---\n")
vh_results <- tryCatch(
  apply_sdma_processing(
    vh_results,
    secondary_method  = "MHR",
    peclet_threshold  = 1.0
  ),
  error = function(e) {
    cat("[WARN] sDMA failed:", e$message, ". Continuing without switching.\n")
    vh_results
  }
)

cat("\nMethods after sDMA switching:\n")
print(unique(vh_results$method))


# ============================================================================
# 7F. ENHANCED SEGMENTED REGRESSION (NON-LINEAR DETECTION)
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("Section 7F: Enhanced Segmented Regression with Non-Linear Detection\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\n")

# The enhanced function automatically detects U-shaped or non-linear
# residual patterns and can fit a piecewise linear-quadratic model

cat("Using compare_methods_enhanced() for automatic pattern detection...\n")
cat("\n")

enhanced_mhr <- compare_methods_enhanced(
  vh_results,
  primary_method = "HRM",
  secondary_method = "MHR",
  sensor_position = "outer",
  try_quadratic = TRUE,
  verbose = TRUE
)

# View the standard segmented plot
if (!is.null(enhanced_mhr$plots$segmented_plot)) {
  print(enhanced_mhr$plots$segmented_plot)
}

# View the residuals plot (now with LOESS smoothing and pattern detection)
if (!is.null(enhanced_mhr$plots$residuals_plot)) {
  print(enhanced_mhr$plots$residuals_plot)
}

# If a U-shaped pattern was detected and quadratic model fitted
if (!is.null(enhanced_mhr$quadratic_model)) {
  cat("\n")
  cat("QUADRATIC MODEL FITTED\n")
  cat("=====================\n")
  cat("Recommendation:", enhanced_mhr$recommended_model, "\n")
  cat("\n")

  # View the quadratic model plot
  if (!is.null(enhanced_mhr$plots$quadratic_plot)) {
    print(enhanced_mhr$plots$quadratic_plot)
  }

  # Compare models
  cat("Model Comparison:\n")
  cat("  Linear segmented R²:", round(enhanced_mhr$r_squared, 4), "\n")
  cat("  Quadratic R²:", round(enhanced_mhr$quadratic_model$r_squared, 4), "\n")
  cat("  Improvement:", round(enhanced_mhr$quadratic_model$r_squared - enhanced_mhr$r_squared, 4), "\n")
  cat("\n")
}

# Check residual diagnostics
if (!is.null(enhanced_mhr$residual_diagnostics)) {
  cat("RESIDUAL DIAGNOSTICS\n")
  cat("===================\n")
  cat("Pattern detected:", enhanced_mhr$residual_diagnostics$pattern_detected, "\n")
  cat("Pattern type:", enhanced_mhr$residual_diagnostics$pattern_type, "\n")
  if (!is.na(enhanced_mhr$residual_diagnostics$quad_pvalue)) {
    cat("Quadratic test p-value:", format.pval(enhanced_mhr$residual_diagnostics$quad_pvalue, digits = 3), "\n")
  }
  cat("\n")
}

cat("INTERPRETATION:\n")
cat("---------------\n")
cat("The enhanced function automatically detects if the relationship after\n")
cat("the breakpoint is non-linear (U-shaped or inverted-U residuals).\n")
cat("\n")
cat("If detected, it fits a piecewise linear-quadratic model where:\n")
cat("  - First segment (before breakpoint): LINEAR\n")
cat("  - Second segment (after breakpoint): QUADRATIC\n")
cat("\n")
cat("This provides a better fit when methods have a curving relationship\n")
cat("at higher velocities.\n")
cat("\n")
cat("Use this when:\n")
cat("  - Standard segmented regression shows U-shaped residuals\n")
cat("  - The relationship clearly curves at higher velocities\n")
cat("  - You need a more accurate calibration across the full range\n")
cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\n")


# ============================================================================
# 8 & 9. EXPLORATORY CALIBRATION DEMOS (SKIPPED)
# HRMXa/HRMXb excluded (unpublished); Tmax_Coh has no data for this sensor.
# Pipeline continues from vh_results (single table, updated through Section 7).
# ============================================================================


# ============================================================================
# 10. BURGESS LOOKUP TABLE
# ============================================================================

# Calculate Burgess coefficients lookup table
lookup_table <- calculate_burgess_coefficients(
  k = wood$derived_properties$thermal_diffusivity_actual_cm2_s,
  x = 0.5,      # 5mm probe spacing (cm)
  t = 80        # Time after heat pulse (seconds)
)

cat("\nBurgess lookup table (first 10 rows):\n")
print(head(lookup_table, 10))


# ============================================================================
# 12. FLUX DENSITY CONVERSION (SAP FLUX DENSITY)
# ============================================================================

# Convert wound-corrected velocity to sap flux density (Jv)
# Wood properties already calculated in Step 2

# Apply flux density conversion (wound correction already applied in Section 6B)
flux_data <- apply_flux_conversion(
  vh_results,
  wood_properties = wood,
  velocity_col = "Vs_cm_hr",
  output_col = "Jv_cm3_cm2_hr"
)

# View flux density results
cat("\nFlux density range:\n")
cat("  Jv: ", range(flux_data$Jv_cm3_cm2_hr, na.rm = TRUE), " cm³/cm²/hr\n")

cat("\nAll methods converted to flux density:\n")
print(unique(flux_data$method))

# ============================================================================
# 12A. WORKING WITH INDIVIDUAL VS sDMA METHODS
# ============================================================================
# Users can now continue with:
# - Individual methods: HRM, MHR, Tmax_Coh, etc.
# - Combined sDMA methods: sDMA:MHR

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("INDIVIDUAL VS sDMA METHODS\n")
cat("=" , rep("=", 70), "=\n", sep = "")

# Example 1: Extract only HRM data
flux_hrm_only <- flux_data[flux_data$method == "HRM", ]
cat("\nHRM flux density:\n")
cat("  Measurements:", nrow(flux_hrm_only), "\n")
cat("  Mean Jv:", mean(flux_hrm_only$Jv_cm3_cm2_hr, na.rm = TRUE), "cm³/cm²/hr\n")

# Example 2: Extract only sDMA:MHR data
flux_sdma_mhr <- flux_data[flux_data$method == "sDMA:MHR", ]
cat("\nsDMA:MHR flux density:\n")
cat("  Measurements:", nrow(flux_sdma_mhr), "\n")
cat("  Mean Jv:", mean(flux_sdma_mhr$Jv_cm3_cm2_hr, na.rm = TRUE), "cm³/cm²/hr\n")

# Example 3: Compare method selection in sDMA:MHR
if ("selected_method" %in% names(flux_sdma_mhr)) {
  cat("\nsDMA:MHR method selection:\n")
  print(table(flux_sdma_mhr$selected_method))
  cat("  % using HRM:",
      100 * sum(flux_sdma_mhr$selected_method == "HRM", na.rm = TRUE) / nrow(flux_sdma_mhr),
      "%\n")
  cat("  % using MHR:",
      100 * sum(flux_sdma_mhr$selected_method == "MHR", na.rm = TRUE) / nrow(flux_sdma_mhr),
      "%\n")
}

# Example 4: Extract all sDMA methods
flux_sdma_all <- flux_data[grepl("^sDMA:", flux_data$method), ]
cat("\nAll sDMA methods:\n")
cat("  Methods:", unique(flux_sdma_all$method), "\n")
cat("  Measurements:", nrow(flux_sdma_all), "\n")

# For the rest of the workflow, we'll continue with ALL methods
# Users can filter to specific methods as needed for their analysis


# ============================================================================
# 13. SAPWOOD INTEGRATION (TREE-LEVEL SAP FLUX) - STEP 6
# ============================================================================
# NOTE: Sapwood integration works with ALL methods simultaneously
# The 'method' column is preserved, so you can analyze each method separately

# Calculate sapwood areas based on tree dimensions
sapwood_areas <- calc_sapwood_areas(
  dbh = 52.08,              # cm (from field measurements)
  bark_thickness = 0.5,  # cm
  sapwood_depth = 2.5,   # cm (from wood cores)
  sensor_positions = c("outer", "inner")
)

cat("\nSapwood area allocation:\n")
print(sapwood_areas$rings)
cat("\nTotal sapwood area:", sapwood_areas$total_sapwood_area_cm2, "cm²\n")

# Calculate total sap flux (Qp) by integrating over sapwood area
flux_integrated <- calc_sap_flux(
  flux_data,
  sapwood_areas,
  method = "weighted_average"
)

# View total flux results (across all methods)
cat("\nTotal sap flux range (all methods):\n")
cat("  Qp: ", range(flux_integrated$Q_cm3_hr, na.rm = TRUE), " cm³/hr\n")
cat("     ", range(flux_integrated$Q_L_hr, na.rm = TRUE), " L/hr\n")
cat("     ", range(flux_integrated$Q_L_day, na.rm = TRUE), " L/day\n")

cat("\nMethods after sapwood integration:\n")
print(unique(flux_integrated$method))


# ============================================================================
# 14. SAPWOOD METRICS (SIZE-NORMALIZED FLUX) - STEP 8
# ============================================================================

# Calculate sapwood-area-weighted mean flux density (Qps)
flux_metrics <- apply_sapwood_metrics(
  flux_integrated,
  sapwood_area = sapwood_areas$total_sapwood_area_cm2,
  normalise = TRUE,
  normalise_period = "global",
  leaf_area = 20.0,  # m² (if known, otherwise omit)
  leaf_area_units = "m2"
)

# View size-normalized metrics
cat("\nSize-normalized flux density (Qps):\n")
cat("  Range:", range(flux_metrics$Qps_cm_hr, na.rm = TRUE), "cm/hr\n")
cat("  Mean: ", mean(flux_metrics$Qps_cm_hr, na.rm = TRUE), "cm/hr\n")

cat("\nLeaf-area-specific flux (Qpl):\n")
cat("  Range:", range(flux_metrics$Qpl_cm3_hr_m2, na.rm = TRUE), "cm³/hr/m²\n")


# ============================================================================
# 15. DAILY AGGREGATION - STEP 9
# ============================================================================

# PREDAWN WINDOW FILTERING (optional — for zero-flow baseline or VPD calculations)
#
# Static mode: fixed clock hours, e.g. 2am–6am
# predawn_hours <- resolve_predawn_hours(c(2, 6), mode = "static")
# predawn_data  <- filter_predawn(flux_metrics, window = c(2, 6), mode = "static")
#
# Dynamic mode: hours relative to astronomical dawn (requires site_location set above)
# dawn_times   <- wood$get_dawn_times(unique(as.Date(flux_metrics$datetime)))
# predawn_data <- filter_predawn(
#   flux_metrics,
#   window     = c(2, 0),      # 2 hrs before dawn to dawn
#   mode       = "dynamic",
#   dawn_times = dawn_times
# )

# Aggregate to daily totals
daily_flux <- aggregate_daily(
  flux_metrics,
  interval = "auto",  # Auto-detect hourly/half-hourly
  require_complete_days = FALSE,
  min_measurements_per_day = 20  # Require at least 20/24 measurements
)

# Normalize daily flux by maximum
daily_flux <- normalise_daily(
  daily_flux,
  normalise_col = "Jvm_daily_cm3_cm2_day",
  period = "global"
)

# View daily results
cat("\nDaily flux density (Jvm_daily):\n")
cat("  Range:", range(daily_flux$Jvm_daily_mm_day, na.rm = TRUE), "mm/day\n")
cat("  Mean: ", mean(daily_flux$Jvm_daily_mm_day, na.rm = TRUE), "mm/day\n")

cat("\nDaily total flux (Qp_daily):\n")
cat("  Range:", range(daily_flux$Qp_daily_L_day, na.rm = TRUE), "L/day\n")
cat("  Total over period:", sum(daily_flux$Qp_daily_L_day, na.rm = TRUE), "L\n")


# ============================================================================
# 16. PLOTTING RESULTS
# ============================================================================

# Load ggplot2 if available
if (requireNamespace("ggplot2", quietly = TRUE)) {

  # Plot 1: Hourly timeseries of total flux (all methods)
  print(plot_sap_flux_timeseries(
    flux_metrics,
    y_col = "Q_L_hr",
    y_label = "Total Sap Flux (L/hr)",
    title = "Hourly Sap Flux Timeseries (All Methods)",
    smooth = TRUE
  ))

  # Plot 2: SF/Max_Qp (Tim's plot)
  print(plot_flux_proportion(
    flux_metrics,
    flux_col = "Q_cm3_hr",
    title = "Sap Flux as Proportion of Maximum",
    add_reference_lines = TRUE
  ))

  # Plot 3: Diurnal pattern
  print(plot_diurnal_pattern(
    flux_metrics,
    y_col = "Qps_cm_hr",
    y_label = "Flux Density (cm/hr)",
    stat = "mean",
    add_ci = TRUE
  ))

  # Plot 4: Daily timeseries
  print(plot_daily_timeseries(
    daily_flux,
    y_col = "Qp_daily_L_day",
    y_label = "Daily Total Flux (L/day)",
    add_trend = TRUE
  ))

} else {
  cat("\nggplot2 not installed. Skipping plots.\n")
  cat("Install with: install.packages('ggplot2')\n")
}


# ============================================================================
# 16A. COMPARE INDIVIDUAL METHODS VS sDMA METHODS
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("METHOD COMPARISON: HRM vs MHR vs sDMA:MHR\n")
cat("=" , rep("=", 70), "=\n", sep = "")

# Extract daily flux for each method
daily_hrm <- daily_flux[daily_flux$method == "HRM", ]
daily_mhr <- daily_flux[daily_flux$method == "MHR", ]
daily_sdma_mhr <- daily_flux[daily_flux$method == "sDMA:MHR", ]

cat("\nDaily water use comparison:\n")
cat("  HRM:       mean =", sprintf("%.2f", mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)),
    "L/day, total =", sprintf("%.2f", sum(daily_hrm$Qp_daily_L_day, na.rm = TRUE)), "L\n")
cat("  MHR:       mean =", sprintf("%.2f", mean(daily_mhr$Qp_daily_L_day, na.rm = TRUE)),
    "L/day, total =", sprintf("%.2f", sum(daily_mhr$Qp_daily_L_day, na.rm = TRUE)), "L\n")
cat("  sDMA:MHR:  mean =", sprintf("%.2f", mean(daily_sdma_mhr$Qp_daily_L_day, na.rm = TRUE)),
    "L/day, total =", sprintf("%.2f", sum(daily_sdma_mhr$Qp_daily_L_day, na.rm = TRUE)), "L\n")

# Calculate differences
diff_mhr_hrm <- mean(daily_mhr$Qp_daily_L_day, na.rm = TRUE) -
                mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)
diff_sdma_hrm <- mean(daily_sdma_mhr$Qp_daily_L_day, na.rm = TRUE) -
                 mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)

cat("\nDifferences from HRM:\n")
cat("  MHR - HRM:      ", sprintf("%+.2f", diff_mhr_hrm), "L/day",
    sprintf("(%+.1f%%)", 100 * diff_mhr_hrm / mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)), "\n")
cat("  sDMA:MHR - HRM: ", sprintf("%+.2f", diff_sdma_hrm), "L/day",
    sprintf("(%+.1f%%)", 100 * diff_sdma_hrm / mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)), "\n")

# Plot comparison if ggplot2 available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Create comparison dataframe
  comparison_df <- data.frame(
    date = daily_hrm$date,
    HRM = daily_hrm$Qp_daily_L_day,
    MHR = daily_mhr$Qp_daily_L_day,
    sDMA_MHR = daily_sdma_mhr$Qp_daily_L_day
  )

  # Reshape for plotting
  comparison_long <- tidyr::pivot_longer(
    comparison_df,
    cols = c(HRM, MHR, sDMA_MHR),
    names_to = "Method",
    values_to = "Qp_daily_L_day"
  )

  # Plot comparison
  p <- ggplot(comparison_long, aes(x = date, y = Qp_daily_L_day, color = Method)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = "Daily Water Use: HRM vs MHR vs sDMA:MHR",
      x = "Date",
      y = "Daily Water Use (L/day)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)

  # Scatter plot: HRM vs sDMA:MHR
  p2 <- ggplot(comparison_df, aes(x = HRM, y = sDMA_MHR)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Daily Water Use: HRM vs sDMA:MHR",
      x = "HRM (L/day)",
      y = "sDMA:MHR (L/day)"
    ) +
    theme_minimal()

  print(p2)
}


# ============================================================================
# 17. EXPORT FINAL RESULTS
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("EXPORTING RESULTS\n")
cat("=" , rep("=", 70), "=\n", sep = "")

# Export hourly flux data (ALL methods)
write.csv(
  flux_metrics,
  "SX01O201_hourly_flux_all_methods.csv",
  row.names = FALSE
)
cat("\n✓ Exported: SX01O201_hourly_flux_all_methods.csv\n")

# Export daily flux data (ALL methods)
write.csv(
  daily_flux,
  "SX01O201_daily_flux_all_methods.csv",
  row.names = FALSE
)
cat("✓ Exported: SX01O201_daily_flux_all_methods.csv\n")

# Export only OK quality hourly data
clean_hourly <- flux_metrics[flux_metrics$quality_flag == "OK", ]
write.csv(
  clean_hourly,
  "SX01O201_hourly_flux_clean.csv",
  row.names = FALSE
)
cat("✓ Exported: SX01O201_hourly_flux_clean.csv (OK quality only)\n")

# Export individual methods separately
cat("\nExporting individual methods:\n")

# HRM only
hrm_daily <- daily_flux[daily_flux$method == "HRM", ]
write.csv(hrm_daily, "SX01O201_daily_HRM.csv", row.names = FALSE)
cat("  ✓ SX01O201_daily_HRM.csv\n")

# MHR only
mhr_daily <- daily_flux[daily_flux$method == "MHR", ]
write.csv(mhr_daily, "SX01O201_daily_MHR.csv", row.names = FALSE)
cat("  ✓ SX01O201_daily_MHR.csv\n")

# sDMA:MHR only
sdma_mhr_daily <- daily_flux[daily_flux$method == "sDMA:MHR", ]
write.csv(sdma_mhr_daily, "SX01O201_daily_sDMA_MHR.csv", row.names = FALSE)
cat("  ✓ SX01O201_daily_sDMA_MHR.csv\n")


# Summary statistics
cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("FINAL SUMMARY\n")
cat("=" , rep("=", 70), "=\n", sep = "")

cat("\nMethods processed:\n")
cat("  Individual: HRM, MHR, Tmax_Coh, Tmax_Klu\n")
cat("  sDMA:       sDMA:MHR\n")
cat("  Total methods:", length(unique(daily_flux$method)), "\n")

cat("\nHourly data (clean):\n")
cat("  Measurements:", nrow(clean_hourly), "\n")
cat("  Date range:", as.character(range(clean_hourly$datetime)), "\n")
cat("  Mean Qp:", sprintf("%.2f", mean(clean_hourly$Q_L_hr, na.rm = TRUE)), "L/hr\n")
cat("  Mean Qps:", sprintf("%.2f", mean(clean_hourly$Qps_cm_hr, na.rm = TRUE)), "cm/hr\n")

cat("\nDaily data (all methods):\n")
cat("  Total measurements:", nrow(daily_flux), "\n")
cat("  Unique days:", length(unique(daily_flux$date)), "\n")
cat("  Date range:", as.character(range(daily_flux$date)), "\n")

cat("\nDaily water use by method:\n")
cat("  HRM:       mean =", sprintf("%.2f", mean(daily_hrm$Qp_daily_L_day, na.rm = TRUE)),
    "L/day\n")
cat("  MHR:       mean =", sprintf("%.2f", mean(daily_mhr$Qp_daily_L_day, na.rm = TRUE)),
    "L/day\n")
cat("  sDMA:MHR:  mean =", sprintf("%.2f", mean(daily_sdma_mhr$Qp_daily_L_day, na.rm = TRUE)),
    "L/day\n")

cat("\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("COMPLETE PIPELINE DEMONSTRATION FINISHED\n")
cat("=" , rep("=", 70), "=\n", sep = "")
cat("\nFeatures demonstrated:\n")
cat("  ✓ Dual-location calibration (early vs late)\n")
cat("  ✓ sDMA method switching (Peclet-based)\n")
cat("  ✓ Multiple HPV calculation methods\n")
cat("  ✓ Spacing and wound corrections\n")
cat("  ✓ Flux density conversion\n")
cat("  ✓ Sapwood integration\n")
cat("  ✓ Daily aggregation and normalization\n")
cat("\nFiles exported:\n")
cat("  - SX01O201_hourly_flux_all_methods.csv (all methods, all data)\n")
cat("  - SX01O201_hourly_flux_clean.csv (OK quality only)\n")
cat("  - SX01O201_daily_flux_all_methods.csv (daily, all methods)\n")
cat("  - SX01O201_daily_HRM.csv (HRM only)\n")
cat("  - SX01O201_daily_MHR.csv (MHR only)\n")
cat("  - SX01O201_daily_sDMA_MHR.csv (sDMA:MHR only)\n")
cat("\n")

