# Test script for quality control functions
# Run this after loading your sapdata

library(sapfluxr)

cat("Testing Quality Control Functions\n")
cat("==================================\n\n")

# Example: Test with user's sapdata
# Assuming sapdata is already loaded in the environment
# and vh_results have been calculated

# If you haven't calculated Vh yet:
# vh_results <- calc_heat_pulse_velocity(sapdata, methods = "HRM")

# Run quality control
cat("Running quality control analysis...\n\n")

# Example command (uncomment and modify with your actual data):
# qc_results <- flag_vh_quality(
#   vh_results,
#   wood_properties = "wood_generic_sw",  # Or your custom wood properties
#   verbose = TRUE
# )

# View results:
# print(qc_results)
#
# # Check flag distribution
# cat("\nFlag Distribution:\n")
# print(table(qc_results$vh_flagged$quality_flag))
#
# # Check gap report
# if (!is.null(qc_results$gap_report) && nrow(qc_results$gap_report) > 0) {
#   cat("\nMissing Data Gaps:\n")
#   print(qc_results$gap_report)
# }
#
# # Check for spike period (2024-12-09 06:00:00 to 12:00:00)
# spike_period <- qc_results$vh_flagged$datetime >= as.POSIXct("2024-12-09 06:00:00") &
#                 qc_results$vh_flagged$datetime <= as.POSIXct("2024-12-09 12:00:00")
#
# cat("\nFlags during spike period (2024-12-09 06:00 to 12:00):\n")
# spike_flags <- table(qc_results$vh_flagged$quality_flag[spike_period])
# print(spike_flags)
#
# # Extract outliers detected during spike period
# outliers_in_spike <- qc_results$vh_flagged[spike_period &
#                                             qc_results$vh_flagged$quality_flag == "OUTLIER", ]
# if (nrow(outliers_in_spike) > 0) {
#   cat(sprintf("\nDetected %d outliers during spike period\n", nrow(outliers_in_spike)))
#   cat("First few outliers:\n")
#   print(head(outliers_in_spike))
# }

cat("\nTo use this script:\n")
cat("1. Load your data: sapdata <- read_heat_pulse_data('yourfile.txt')\n")
cat("2. Calculate Vh: vh_results <- calc_heat_pulse_velocity(sapdata)\n")
cat("3. Uncomment the code above and run quality control\n")
cat("4. The spike between 2024-12-09 06:00-12:00 should be flagged as OUTLIER\n")
