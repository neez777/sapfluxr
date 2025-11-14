# Benchmark: C++ vs R implementations of quality control functions
# Compares performance of rolling mean and rate of change outlier detection
# Run this after compiling the package with Rcpp support

library(sapfluxr)
library(microbenchmark)

# =============================================================================
# Setup: Create test data of varying sizes
# =============================================================================

cat("\n")
cat("========================================================\n")
cat("  Benchmarking C++ vs R Quality Control Functions\n")
cat("========================================================\n\n")

# Generate realistic sap flow data with outliers
generate_test_data <- function(n) {
  # Simulate realistic sap flow pattern (daily cycle + noise)
  time_hours <- seq(0, n * 0.5, by = 0.5)  # 30-min intervals

  # Daily sinusoidal pattern
  daily_pattern <- 10 + 8 * sin(2 * pi * time_hours / 24)

  # Add realistic noise
  noise <- rnorm(n, mean = 0, sd = 0.5)

  vh_values <- daily_pattern + noise

  # Add some random outliers (5% of data)
  outlier_idx <- sample(1:n, size = floor(n * 0.05))
  vh_values[outlier_idx] <- vh_values[outlier_idx] + rnorm(length(outlier_idx), mean = 0, sd = 10)

  # Add some NAs (2% of data)
  na_idx <- sample(1:n, size = floor(n * 0.02))
  vh_values[na_idx] <- NA

  vh_values
}

# =============================================================================
# Original R implementations (for comparison)
# =============================================================================

detect_outliers_rolling_mean_r <- function(vh_values, window = 5, threshold = 3) {
  n <- length(vh_values)
  outlier_indices <- integer(0)

  if (n < 2 * window + 1) {
    return(outlier_indices)
  }

  for (i in (window + 1):(n - window)) {
    if (is.na(vh_values[i])) next

    window_data <- vh_values[(i - window):(i + window)]
    window_mean <- mean(window_data, na.rm = TRUE)
    window_sd <- sd(window_data, na.rm = TRUE)

    if (!is.na(window_mean) && !is.na(window_sd) && window_sd > 0) {
      deviation <- abs(vh_values[i] - window_mean) / window_sd

      if (deviation > threshold) {
        outlier_indices <- c(outlier_indices, i)
      }
    }
  }

  outlier_indices
}

detect_rate_of_change_outliers_r <- function(vh_values, max_change = 4) {
  n <- length(vh_values)
  outlier_indices <- integer(0)

  if (n < 2) {
    return(outlier_indices)
  }

  for (i in 2:n) {
    if (is.na(vh_values[i]) || is.na(vh_values[i - 1])) next

    change <- abs(vh_values[i] - vh_values[i - 1])

    if (change > max_change) {
      outlier_indices <- c(outlier_indices, i)
    }
  }

  outlier_indices
}

# =============================================================================
# Test 1: Rolling Mean Outlier Detection
# =============================================================================

cat("Test 1: Rolling Mean Outlier Detection\n")
cat("----------------------------------------\n\n")

test_sizes <- c(1000, 5000, 10000, 20000)

rolling_mean_results <- list()

for (size in test_sizes) {
  cat(sprintf("Testing with %d observations...\n", size))

  test_data <- generate_test_data(size)

  # Verify results are identical
  r_result <- detect_outliers_rolling_mean_r(test_data)
  cpp_result <- detect_outliers_rolling_mean_cpp(test_data)

  if (!identical(r_result, cpp_result)) {
    warning(sprintf("Results differ for n=%d! R found %d outliers, C++ found %d",
                   size, length(r_result), length(cpp_result)))
  } else {
    cat(sprintf("  ✓ Results identical: %d outliers detected\n", length(r_result)))
  }

  # Benchmark
  benchmark_result <- microbenchmark(
    R = detect_outliers_rolling_mean_r(test_data),
    Cpp = detect_outliers_rolling_mean_cpp(test_data),
    times = 10
  )

  rolling_mean_results[[as.character(size)]] <- benchmark_result

  # Calculate speedup
  summary_stats <- summary(benchmark_result)
  r_median <- summary_stats$median[summary_stats$expr == "R"]
  cpp_median <- summary_stats$median[summary_stats$expr == "Cpp"]
  speedup <- r_median / cpp_median

  cat(sprintf("  R median:   %.2f ms\n", r_median / 1000))
  cat(sprintf("  C++ median: %.2f ms\n", cpp_median / 1000))
  cat(sprintf("  Speedup:    %.1fx faster\n\n", speedup))
}

# =============================================================================
# Test 2: Rate of Change Outlier Detection
# =============================================================================

cat("\nTest 2: Rate of Change Outlier Detection\n")
cat("------------------------------------------\n\n")

rate_change_results <- list()

for (size in test_sizes) {
  cat(sprintf("Testing with %d observations...\n", size))

  test_data <- generate_test_data(size)

  # Verify results are identical
  r_result <- detect_rate_of_change_outliers_r(test_data)
  cpp_result <- detect_rate_of_change_outliers_cpp(test_data)

  if (!identical(r_result, cpp_result)) {
    warning(sprintf("Results differ for n=%d! R found %d outliers, C++ found %d",
                   size, length(r_result), length(cpp_result)))
  } else {
    cat(sprintf("  ✓ Results identical: %d outliers detected\n", length(r_result)))
  }

  # Benchmark
  benchmark_result <- microbenchmark(
    R = detect_rate_of_change_outliers_r(test_data),
    Cpp = detect_rate_of_change_outliers_cpp(test_data),
    times = 10
  )

  rate_change_results[[as.character(size)]] <- benchmark_result

  # Calculate speedup
  summary_stats <- summary(benchmark_result)
  r_median <- summary_stats$median[summary_stats$expr == "R"]
  cpp_median <- summary_stats$median[summary_stats$expr == "Cpp"]
  speedup <- r_median / cpp_median

  cat(sprintf("  R median:   %.2f ms\n", r_median / 1000))
  cat(sprintf("  C++ median: %.2f ms\n", cpp_median / 1000))
  cat(sprintf("  Speedup:    %.1fx faster\n\n", speedup))
}

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("========================================================\n")
cat("  Summary\n")
cat("========================================================\n\n")

cat("Rolling Mean Detection:\n")
for (size in test_sizes) {
  summary_stats <- summary(rolling_mean_results[[as.character(size)]])
  r_median <- summary_stats$median[summary_stats$expr == "R"]
  cpp_median <- summary_stats$median[summary_stats$expr == "Cpp"]
  speedup <- r_median / cpp_median
  cat(sprintf("  n=%5d: %.1fx speedup\n", size, speedup))
}

cat("\nRate of Change Detection:\n")
for (size in test_sizes) {
  summary_stats <- summary(rate_change_results[[as.character(size)]])
  r_median <- summary_stats$median[summary_stats$expr == "R"]
  cpp_median <- summary_stats$median[summary_stats$expr == "Cpp"]
  speedup <- r_median / cpp_median
  cat(sprintf("  n=%5d: %.1fx speedup\n", size, speedup))
}

cat("\n")
cat("To visualise benchmarks, run:\n")
cat("  library(ggplot2)\n")
cat("  autoplot(rolling_mean_results[['10000']])\n")
cat("  autoplot(rate_change_results[['10000']])\n")
cat("\n")
