# Profile the full quality control pipeline
# Identifies where time is actually spent

library(sapfluxr)

cat("\n")
cat("========================================================\n")
cat("  Quality Control Pipeline Profiling\n")
cat("========================================================\n\n")

# Generate realistic test data of different sizes
generate_vh_data <- function(n_obs) {
  data.frame(
    datetime = seq(Sys.time(), by = "30 min", length.out = n_obs),
    pulse_id = 1:n_obs,
    method = rep(c("HRM", "MHR"), length.out = n_obs),
    sensor_position = rep(c("outer", "inner"), each = n_obs/2),
    Vh_cm_hr = rnorm(n_obs, mean = 10, sd = 2),
    quality_flag = "OK"
  )
}

# Test with different data sizes
test_sizes <- c(5000, 10000, 20000)

for (n in test_sizes) {
  cat(sprintf("\n=== Testing with %d observations ===\n\n", n))

  vh_data <- generate_vh_data(n)

  # Full pipeline
  cat("Full pipeline:\n")
  t_full <- system.time({
    result_full <- flag_vh_quality(vh_data, verbose = FALSE)
  })
  cat(sprintf("  Total time: %.3f seconds\n\n", t_full["elapsed"]))

  # Individual components
  cat("Component breakdown:\n")

  # 1. Missing pulse detection
  t1 <- system.time({
    r1 <- flag_vh_quality(
      vh_data,
      detect_missing_pulses = TRUE,
      check_illogical = FALSE,
      detect_outliers = FALSE,
      detect_rate_of_change = FALSE,
      check_cross_sensor = FALSE,
      verbose = FALSE
    )
  })
  cat(sprintf("  1. Missing pulse detection:    %.3f sec (%.1f%%)\n",
              t1["elapsed"], 100 * t1["elapsed"] / t_full["elapsed"]))

  # 2. Illogical values
  t2 <- system.time({
    r2 <- flag_vh_quality(
      vh_data,
      detect_missing_pulses = FALSE,
      check_illogical = TRUE,
      detect_outliers = FALSE,
      detect_rate_of_change = FALSE,
      check_cross_sensor = FALSE,
      verbose = FALSE
    )
  })
  cat(sprintf("  2. Illogical value check:       %.3f sec (%.1f%%)\n",
              t2["elapsed"], 100 * t2["elapsed"] / t_full["elapsed"]))

  # 3. Outlier detection (rolling mean)
  t3 <- system.time({
    r3 <- flag_vh_quality(
      vh_data,
      detect_missing_pulses = FALSE,
      check_illogical = FALSE,
      detect_outliers = TRUE,
      detect_rate_of_change = FALSE,
      check_cross_sensor = FALSE,
      verbose = FALSE
    )
  })
  cat(sprintf("  3. Outlier detection (C++):      %.3f sec (%.1f%%)\n",
              t3["elapsed"], 100 * t3["elapsed"] / t_full["elapsed"]))

  # 4. Rate of change
  t4 <- system.time({
    r4 <- flag_vh_quality(
      vh_data,
      detect_missing_pulses = FALSE,
      check_illogical = FALSE,
      detect_outliers = FALSE,
      detect_rate_of_change = TRUE,
      check_cross_sensor = FALSE,
      verbose = FALSE
    )
  })
  cat(sprintf("  4. Rate of change (C++):         %.3f sec (%.1f%%)\n",
              t4["elapsed"], 100 * t4["elapsed"] / t_full["elapsed"]))

  # 5. Cross-sensor
  t5 <- system.time({
    r5 <- flag_vh_quality(
      vh_data,
      detect_missing_pulses = FALSE,
      check_illogical = FALSE,
      detect_outliers = FALSE,
      detect_rate_of_change = FALSE,
      check_cross_sensor = TRUE,
      verbose = FALSE
    )
  })
  cat(sprintf("  5. Cross-sensor check:           %.3f sec (%.1f%%)\n",
              t5["elapsed"], 100 * t5["elapsed"] / t_full["elapsed"]))

  component_sum <- t1["elapsed"] + t2["elapsed"] + t3["elapsed"] +
                   t4["elapsed"] + t5["elapsed"]
  overhead <- t_full["elapsed"] - component_sum

  cat(sprintf("\n  Component sum:                   %.3f sec\n", component_sum))
  cat(sprintf("  Overhead (setup/validation):     %.3f sec (%.1f%%)\n",
              overhead, 100 * overhead / t_full["elapsed"]))

  cat(sprintf("\n  -> Outlier detection is now %.1f%% of total time (down from ~90%% before C++)\n",
              100 * t3["elapsed"] / t_full["elapsed"]))
}

cat("\n")
cat("========================================================\n")
cat("  Recommendations\n")
cat("========================================================\n\n")

cat("If missing pulse detection is the bottleneck (>50% of time):\n")
cat("  -> Consider converting detect_and_fill_missing_pulses() to C++\n\n")

cat("If overhead is high (>30% of time):\n")
cat("  -> Optimise data validation and grouping operations\n\n")

cat("For Shiny performance:\n")
cat("  1. Use progress bars (progressr package) for user feedback\n")
cat("  2. Debounce reactive inputs to avoid re-running on every keystroke\n")
cat("  3. Cache results if data hasn't changed\n")
cat("  4. Consider async processing with promises/future\n\n")
