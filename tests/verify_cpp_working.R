# Verify C++ is working and identify actual bottleneck

# First, ensure package is loaded fresh
detach("package:sapfluxr", unload = TRUE)
devtools::load_all("E:/R/project/sapfluxr")

cat("\n=== C++ Function Verification ===\n\n")

# 1. Verify functions exist and are wrappers
cat("1. Function check:\n")
if (exists("detect_outliers_rolling_mean")) {
  cat("   ✓ detect_outliers_rolling_mean exists\n")
  body_text <- deparse(body(detect_outliers_rolling_mean))
  if (any(grepl("detect_outliers_rolling_mean_cpp", body_text))) {
    cat("   ✓ Calls C++ version\n")
  } else {
    cat("   ✗ Does NOT call C++ version!\n")
  }
} else {
  cat("   ✗ detect_outliers_rolling_mean NOT FOUND\n")
}

if (exists("detect_rate_of_change_outliers")) {
  cat("   ✓ detect_rate_of_change_outliers exists\n")
  body_text <- deparse(body(detect_rate_of_change_outliers))
  if (any(grepl("detect_rate_of_change_outliers_cpp", body_text))) {
    cat("   ✓ Calls C++ version\n")
  } else {
    cat("   ✗ Does NOT call C++ version!\n")
  }
} else {
  cat("   ✗ detect_rate_of_change_outliers NOT FOUND\n")
}

cat("\n")

# 2. Quick performance test
cat("2. Performance test (10,000 observations):\n\n")
set.seed(123)
test_data <- rnorm(10000, mean = 10, sd = 2)

# Test rolling mean
cat("   Rolling mean detection:\n")
t1 <- system.time({
  result1 <- detect_outliers_rolling_mean(test_data, window = 5, threshold = 3)
})
cat("      Time:", t1["elapsed"], "seconds\n")
cat("      Outliers found:", length(result1), "\n\n")

# Test rate of change
cat("   Rate of change detection:\n")
t2 <- system.time({
  result2 <- detect_rate_of_change_outliers(test_data, max_change = 4)
})
cat("      Time:", t2["elapsed"], "seconds\n")
cat("      Outliers found:", length(result2), "\n\n")

# 3. Profile the full quality check pipeline
cat("3. Profiling full flag_vh_quality() function:\n\n")

# Create realistic test data
vh_test <- data.frame(
  datetime = seq(Sys.time(), by = "30 min", length.out = 10000),
  pulse_id = 1:10000,
  method = "HRM",
  sensor_position = "outer",
  Vh_cm_hr = rnorm(10000, mean = 10, sd = 2)
)

cat("   Testing with 10,000 observations...\n")

# Time the full quality check
t_total <- system.time({
  result_qc <- flag_vh_quality(
    vh_test,
    detect_missing_pulses = TRUE,
    check_illogical = TRUE,
    detect_outliers = TRUE,
    detect_rate_of_change = TRUE,
    check_cross_sensor = FALSE,  # Only one sensor
    verbose = FALSE
  )
})

cat("   Total time:", t_total["elapsed"], "seconds\n\n")

# 4. Break down where time is spent
cat("4. Detailed timing breakdown:\n\n")

# Just missing pulse detection
t_missing <- system.time({
  result_missing <- flag_vh_quality(
    vh_test,
    detect_missing_pulses = TRUE,
    check_illogical = FALSE,
    detect_outliers = FALSE,
    detect_rate_of_change = FALSE,
    check_cross_sensor = FALSE,
    verbose = FALSE
  )
})
cat("   Missing pulse detection:", t_missing["elapsed"], "seconds\n")

# Just outlier detection
t_outliers <- system.time({
  result_outliers <- flag_vh_quality(
    vh_test,
    detect_missing_pulses = FALSE,
    check_illogical = FALSE,
    detect_outliers = TRUE,
    detect_rate_of_change = FALSE,
    check_cross_sensor = FALSE,
    verbose = FALSE
  )
})
cat("   Outlier detection (rolling mean):", t_outliers["elapsed"], "seconds\n")

# Just rate of change
t_rate <- system.time({
  result_rate <- flag_vh_quality(
    vh_test,
    detect_missing_pulses = FALSE,
    check_illogical = FALSE,
    detect_outliers = FALSE,
    detect_rate_of_change = TRUE,
    check_cross_sensor = FALSE,
    verbose = FALSE
  )
})
cat("   Rate of change detection:", t_rate["elapsed"], "seconds\n\n")

cat("=== Analysis ===\n\n")
cat("If outlier detection times are < 0.01 seconds, C++ is working!\n")
cat("If total time >> sum of parts, overhead is in:\n")
cat("  - Data validation and setup\n")
cat("  - Group-by operations (splitting by method/sensor)\n")
cat("  - dplyr operations\n")
cat("  - Shiny reactive updates\n\n")

cat("For Shiny performance, consider:\n")
cat("  1. Use isolate() to prevent excessive reactivity\n")
cat("  2. Debounce user inputs\n")
cat("  3. Show progress bars for long operations\n")
cat("  4. Cache results if data hasn't changed\n\n")
