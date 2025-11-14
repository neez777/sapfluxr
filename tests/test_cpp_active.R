# Quick diagnostic: Is C++ actually being used?

library(sapfluxr)

cat("\n=== C++ Compilation Check ===\n\n")

# 1. Check if C++ functions exist
cat("1. Checking if C++ functions are available:\n")
cat("   detect_outliers_rolling_mean_cpp exists:",
    exists("detect_outliers_rolling_mean_cpp"), "\n")
cat("   detect_rate_of_change_outliers_cpp exists:",
    exists("detect_rate_of_change_outliers_cpp"), "\n\n")

# 2. Check if they're actually calling C++
cat("2. Checking function bodies:\n")
cat("   detect_outliers_rolling_mean body:\n")
print(body(detect_outliers_rolling_mean))
cat("\n")
cat("   detect_rate_of_change_outliers body:\n")
print(body(detect_rate_of_change_outliers))
cat("\n")

# 3. Quick performance test
cat("3. Quick performance test (1000 observations):\n")
test_data <- rnorm(1000, mean = 10, sd = 2)

# Time the rolling mean function
t1 <- system.time({
  result1 <- detect_outliers_rolling_mean(test_data, window = 5, threshold = 3)
})

cat("   Time for rolling mean detection:", t1["elapsed"], "seconds\n")
cat("   Found", length(result1), "outliers\n\n")

# 4. Test rate of change
t2 <- system.time({
  result2 <- detect_rate_of_change_outliers(test_data, max_change = 4)
})

cat("   Time for rate of change detection:", t2["elapsed"], "seconds\n")
cat("   Found", length(result2), "outliers\n\n")

# 5. Check if DLL is loaded
cat("4. Checking if C++ library is loaded:\n")
loaded_dlls <- getLoadedDLLs()
if ("sapfluxr" %in% names(loaded_dlls)) {
  cat("   ✓ sapfluxr.dll is loaded\n")
  cat("   Path:", loaded_dlls[["sapfluxr"]][["path"]], "\n")
} else {
  cat("   ✗ sapfluxr.dll is NOT loaded - C++ not compiled!\n")
}

cat("\n=== End Diagnostics ===\n\n")

# If you see the C++ function calls in the body and the DLL is loaded,
# then C++ is active. If performance is still slow, the bottleneck may
# be elsewhere in the quality check pipeline.
