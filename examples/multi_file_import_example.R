# Multi-File Import Example
# This example demonstrates how to import and analyze multiple sap flow data files

# Load the sapFluxR package
library(sapFluxR)

# Example 1: Basic multi-file import with automatic tree identification
# ====================================================================

# Assume you have multiple data files for different trees
file_paths <- c(
  "data/tree1_sapflow.txt",
  "data/tree2_sapflow.txt",
  "data/tree3_sapflow.txt"
)

# Import all files at once
multi_data <- read_multiple_sap_data(file_paths)

# View the structure
str(multi_data)

# Check tree summary
print(multi_data$tree_summary)

# Example 2: Import with explicit tree names
# ==========================================

# Provide custom tree identifiers
tree_names <- c("Oak_North", "Oak_South", "Pine_Center")

multi_data_named <- read_multiple_sap_data(
  file_paths = file_paths,
  tree_ids = tree_names
)

# Example 3: Keep individual data objects for separate processing
# ===============================================================

# Import without combining (useful for different processing approaches)
individual_data <- read_multiple_sap_data(
  file_paths = file_paths,
  combine_data = FALSE
)

# Access individual tree data
tree1_data <- individual_data$tree1_sapflow
tree2_data <- individual_data$tree2_sapflow

# Example 4: Process combined multi-tree data
# ===========================================

# Process all trees with the same methods
results <- process_sap_data(multi_data)

# View results
str(results)

# Example 5: Compare trees using the comparison utilities
# =======================================================

# Compare trees across different dimensions
comparison <- compare_trees(
  velocity_results = results$velocity_results,
  sap_data = multi_data,
  comparison_type = c("summary", "temporal", "method", "statistical")
)

# View comparison results
print(comparison)

# Example 6: Access specific comparison results
# =============================================

# Summary statistics
summary_stats <- comparison$summary
print(summary_stats)

# Temporal correlations
if (!is.null(comparison$temporal$pairwise_correlations)) {
  correlations <- comparison$temporal$pairwise_correlations
  print(correlations)
}

# Method performance
if (!is.null(comparison$method$overall_method_performance)) {
  method_performance <- comparison$method$overall_method_performance
  print(method_performance)
}

# Example 7: Working with individual tree data
# ============================================

# Process trees individually with different methods
tree1_results <- process_sap_data(individual_data$tree1_sapflow,
                                 methods = c("HRM", "DMA"))
tree2_results <- process_sap_data(individual_data$tree2_sapflow,
                                 methods = c("MHR", "DMA"))

# Compare results manually
tree1_velocities <- tree1_results$velocity_results
tree2_velocities <- tree2_results$velocity_results

# Add tree identifiers
tree1_velocities$tree_id <- "Tree_1"
tree2_velocities$tree_id <- "Tree_2"

# Combine for comparison
combined_velocities <- rbind(tree1_velocities, tree2_velocities)

# Example 8: Quality control across multiple trees
# ================================================

# Check data quality for each tree
for (tree_id in names(individual_data)) {
  cat("Quality check for", tree_id, ":\n")
  quality_check <- validate_sap_data(individual_data[[tree_id]])
  print(quality_check)
  cat("\n")
}

# Example 9: Export multi-tree results
# ====================================

# Export combined results
export_results(results,
               output_file = "multi_tree_results.csv",
               format = "csv")

# Export individual tree results
for (tree_id in names(individual_data)) {
  tree_results <- process_sap_data(individual_data[[tree_id]])
  export_results(tree_results,
                 output_file = paste0(tree_id, "_results.csv"),
                 format = "csv")
}

# Example 10: Advanced multi-tree analysis
# ========================================

# Filter data by quality
good_quality_data <- multi_data
good_quality_data$measurements <- multi_data$measurements[
  multi_data$measurements$quality_flag == "good",
]

# Process filtered data
filtered_results <- process_sap_data(good_quality_data)

# Compare filtered vs unfiltered results
comparison_filtered <- compare_trees(
  velocity_results = filtered_results$velocity_results,
  sap_data = good_quality_data,
  comparison_type = "summary"
)

print("Filtered results summary:")
print(comparison_filtered$summary)

# Example 11: Temporal analysis across trees
# ==========================================

# Focus on temporal patterns
temporal_comparison <- compare_trees(
  velocity_results = results$velocity_results,
  sap_data = multi_data,
  comparison_type = "temporal",
  time_window = "day"
)

# Plot temporal correlations (if plotting functions are available)
if (exists("plot_temporal_correlations")) {
  plot_temporal_correlations(temporal_comparison)
}

# Example 12: Statistical testing between trees
# =============================================

# Perform statistical comparisons
statistical_comparison <- compare_trees(
  velocity_results = results$velocity_results,
  sap_data = multi_data,
  comparison_type = "statistical"
)

# View statistical test results
if (!is.null(statistical_comparison$pairwise_tests)) {
  print("Pairwise t-tests:")
  print(statistical_comparison$pairwise_tests)
}

# Example 13: Method selection across trees
# =========================================

# Compare method performance across all trees
method_comparison <- compare_trees(
  velocity_results = results$velocity_results,
  sap_data = multi_data,
  comparison_type = "method"
)

# Find best method for each tree
if (!is.null(method_comparison$method_performance)) {
  best_methods <- method_comparison$method_performance %>%
    dplyr::group_by(tree_id) %>%
    dplyr::slice_max(success_rate, n = 1)

  print("Best method for each tree:")
  print(best_methods)
}

# Example 14: Error handling and validation
# =========================================

# Check for import errors
if (!is.null(multi_data$import_summary)) {
  failed_imports <- multi_data$import_summary[
    !multi_data$import_summary$import_success,
  ]

  if (nrow(failed_imports) > 0) {
    cat("Failed imports:\n")
    print(failed_imports)
  }
}

# Validate combined data
validation_result <- validate_sap_data(multi_data)
if (!validation_result$valid) {
  cat("Validation issues found:\n")
  print(validation_result$issues)
}

# Example 15: Memory-efficient processing
# =======================================

# For large datasets, process trees individually to save memory
large_file_paths <- c(
  "data/large_tree1.txt",
  "data/large_tree2.txt",
  "data/large_tree3.txt"
)

# Import without combining
individual_large_data <- read_multiple_sap_data(
  file_paths = large_file_paths,
  combine_data = FALSE
)

# Process and save results individually
for (tree_id in names(individual_large_data)) {
  cat("Processing", tree_id, "...\n")

  # Process individual tree
  tree_results <- process_sap_data(individual_large_data[[tree_id]])

  # Save results
  saveRDS(tree_results, file = paste0(tree_id, "_results.rds"))

  # Clean up memory
  rm(tree_results)
  gc()
}

# Load results back for comparison
all_results <- list()
for (tree_id in names(individual_large_data)) {
  all_results[[tree_id]] <- readRDS(paste0(tree_id, "_results.rds"))
}

# Combine velocity results for final comparison
combined_velocity_results <- do.call(rbind, lapply(all_results, function(x) x$velocity_results))

# Final comparison
final_comparison <- compare_trees(
  velocity_results = combined_velocity_results,
  sap_data = individual_large_data,
  comparison_type = "summary"
)

print("Final comparison results:")
print(final_comparison$summary)