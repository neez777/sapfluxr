# R/multi_tree_utilities.R
# Utilities for comparing and analyzing multiple trees

#' Import required packages
#' @importFrom dplyr group_by summarise left_join select mutate filter
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date
#' @importFrom stats aov t.test cor
#' @name compare_trees
NULL
#' Provides comprehensive comparison of sap flow data across multiple trees,
#' including statistical summaries, temporal patterns, and method performance.
#'
#' @param velocity_results Data frame with velocity results containing tree_id column
#' @param sap_data Multi-tree sap_data object from read_multiple_sap_data()
#' @param comparison_type Character vector of comparison types: "summary", "temporal", "method", "statistical"
#' @param time_window Character, time window for temporal analysis: "hour", "day", "week"
#' @param quality_filter Character, quality filter: "all", "good", "flagged"
#' @param verbose Logical, whether to print comparison information
#'
#' @return List containing comparison results based on comparison_type
#'
#' @details
#' This function enables comprehensive multi-tree analysis:
#' - Summary: Basic statistics and data quality metrics per tree
#' - Temporal: Time-series patterns and correlations between trees
#' - Method: Performance comparison of different calculation methods
#' - Statistical: Statistical tests and relationships between trees
#'
#' @examples
#' \dontrun{
#' # Import and process multiple trees
#' multi_data <- read_multiple_sap_data(c("tree1.txt", "tree2.txt", "tree3.txt"))
#' results <- process_sap_data(multi_data)
#'
#' # Compare trees
#' comparison <- compare_trees(results$velocity_results, multi_data)
#'
#' # Specific comparison types
#' summary_comp <- compare_trees(results$velocity_results, multi_data,
#'                               comparison_type = "summary")
#' temporal_comp <- compare_trees(results$velocity_results, multi_data,
#'                                comparison_type = "temporal")
#' }
#'
#' @seealso \code{\link{read_multiple_sap_data}}, \code{\link{process_sap_data}}
#' @export
compare_trees <- function(velocity_results, sap_data,
                         comparison_type = c("summary", "temporal", "method", "statistical"),
                         time_window = "day", quality_filter = "good", verbose = TRUE) {

  # Input validation
  if (!"tree_id" %in% names(velocity_results)) {
    stop("velocity_results must contain 'tree_id' column for multi-tree comparison")
  }

  if (!inherits(sap_data, "multiple_sap_data")) {
    stop("sap_data must be a multiple_sap_data object from read_multiple_sap_data()")
  }

  # Filter data by quality if requested
  if (quality_filter != "all") {
    if ("quality_flag" %in% names(velocity_results)) {
      velocity_results <- velocity_results[velocity_results$quality_flag == quality_filter, ]
    } else {
      warning("No quality_flag column found, using all data")
    }
  }

  if (verbose) {
    cat("🌳 Comparing", length(unique(velocity_results$tree_id)), "trees\n")
    cat("📊 Total measurements:", nrow(velocity_results), "\n")
    cat("🔍 Comparison types:", paste(comparison_type, collapse = ", "), "\n\n")
  }

  results <- list()

  # Summary comparison
  if ("summary" %in% comparison_type) {
    if (verbose) cat("📈 Generating summary comparison...\n")
    results$summary <- compare_trees_summary(velocity_results, sap_data)
  }

  # Temporal comparison
  if ("temporal" %in% comparison_type) {
    if (verbose) cat("⏰ Analyzing temporal patterns...\n")
    results$temporal <- compare_trees_temporal(velocity_results, sap_data, time_window)
  }

  # Method comparison
  if ("method" %in% comparison_type) {
    if (verbose) cat("🔬 Comparing calculation methods...\n")
    results$method <- compare_trees_methods(velocity_results, sap_data)
  }

  # Statistical comparison
  if ("statistical" %in% comparison_type) {
    if (verbose) cat("📊 Performing statistical analysis...\n")
    results$statistical <- compare_trees_statistical(velocity_results, sap_data)
  }

  # Add metadata
  results$metadata <- list(
    n_trees = length(unique(velocity_results$tree_id)),
    tree_ids = sort(unique(velocity_results$tree_id)),
    comparison_time = Sys.time(),
    comparison_types = comparison_type,
    time_window = time_window,
    quality_filter = quality_filter,
    total_measurements = nrow(velocity_results)
  )

  class(results) <- c("tree_comparison", "list")

  if (verbose) {
    cat("✅ Tree comparison complete\n")
  }

  return(results)
}

#' Generate summary comparison of trees
#' @param velocity_results Data frame with velocity results
#' @param sap_data Multi-tree sap_data object
#' @return Data frame with summary statistics per tree
#' @keywords internal
compare_trees_summary <- function(velocity_results, sap_data) {

  # Basic statistics per tree
  tree_summary <- velocity_results %>%
    dplyr::group_by(.data$tree_id) %>%
    dplyr::summarise(
      n_measurements = dplyr::n(),
      n_pulses = length(unique(.data$pulse_id)),
      mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE),
      median_velocity = median(.data$Vh_cm_hr, na.rm = TRUE),
      sd_velocity = sd(.data$Vh_cm_hr, na.rm = TRUE),
      min_velocity = min(.data$Vh_cm_hr, na.rm = TRUE),
      max_velocity = max(.data$Vh_cm_hr, na.rm = TRUE),
      cv_velocity = .data$sd_velocity / .data$mean_velocity,
      q25_velocity = quantile(.data$Vh_cm_hr, 0.25, na.rm = TRUE),
      q75_velocity = quantile(.data$Vh_cm_hr, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # Add data quality metrics
  if ("quality_flag" %in% names(velocity_results)) {
    quality_summary <- velocity_results %>%
      dplyr::group_by(.data$tree_id, .data$quality_flag) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "quality_flag", values_from = "n", values_fill = 0)

    tree_summary <- tree_summary %>%
      dplyr::left_join(quality_summary, by = "tree_id")

    # Calculate success rates
    if ("good" %in% names(quality_summary)) {
      tree_summary$success_rate <- tree_summary$good / tree_summary$n_measurements
    }
  }

  # Add temporal information
  if ("datetime" %in% names(velocity_results)) {
    temporal_info <- velocity_results %>%
      dplyr::group_by(.data$tree_id) %>%
      dplyr::summarise(
        start_time = min(.data$datetime, na.rm = TRUE),
        end_time = max(.data$datetime, na.rm = TRUE),
        duration_days = as.numeric(difftime(max(.data$datetime, na.rm = TRUE),
                                           min(.data$datetime, na.rm = TRUE), units = "days")),
        .groups = "drop"
      )

    tree_summary <- tree_summary %>%
      dplyr::left_join(temporal_info, by = "tree_id")
  }

  # Add method information if available
  if ("method" %in% names(velocity_results)) {
    method_summary <- velocity_results %>%
      dplyr::group_by(.data$tree_id, .data$method) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "method", values_from = "n", values_fill = 0,
                        names_prefix = "method_")

    tree_summary <- tree_summary %>%
      dplyr::left_join(method_summary, by = "tree_id")
  }

  return(tree_summary)
}

#' Generate temporal comparison of trees
#' @param velocity_results Data frame with velocity results
#' @param sap_data Multi-tree sap_data object
#' @param time_window Character, time window for aggregation
#' @return List with temporal analysis results
#' @keywords internal
compare_trees_temporal <- function(velocity_results, sap_data, time_window) {

  if (!"datetime" %in% names(velocity_results)) {
    warning("No datetime column found, skipping temporal analysis")
    return(NULL)
  }

  # Create time-based aggregation
  if (time_window == "hour") {
    velocity_results$time_group <- lubridate::floor_date(velocity_results$datetime, "hour")
  } else if (time_window == "day") {
    velocity_results$time_group <- lubridate::floor_date(velocity_results$datetime, "day")
  } else if (time_window == "week") {
    velocity_results$time_group <- lubridate::floor_date(velocity_results$datetime, "week")
  } else {
    stop("time_window must be one of: hour, day, week")
  }

  # Aggregate by time and tree
  temporal_data <- velocity_results %>%
    dplyr::group_by(.data$time_group, .data$tree_id) %>%
    dplyr::summarise(
      mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE),
      median_velocity = median(.data$Vh_cm_hr, na.rm = TRUE),
      n_measurements = dplyr::n(),
      .groups = "drop"
    )

  # Calculate correlations between trees
  tree_ids <- unique(velocity_results$tree_id)
  if (length(tree_ids) > 1) {
    # Pivot to wide format for correlation
    wide_data <- temporal_data %>%
      dplyr::select(.data$time_group, .data$tree_id, .data$mean_velocity) %>%
      tidyr::pivot_wider(names_from = "tree_id", values_from = "mean_velocity")

    # Calculate correlation matrix
    cor_matrix <- cor(wide_data[, -1], use = "pairwise.complete.obs")

    # Calculate pairwise correlations
    pairwise_correlations <- data.frame(
      tree1 = character(),
      tree2 = character(),
      correlation = numeric(),
      n_observations = integer(),
      stringsAsFactors = FALSE
    )

    for (i in 1:(length(tree_ids) - 1)) {
      for (j in (i + 1):length(tree_ids)) {
        tree1 <- tree_ids[i]
        tree2 <- tree_ids[j]

        # Get common time points
        common_data <- wide_data[complete.cases(wide_data[, c(tree1, tree2)]), ]

        if (nrow(common_data) > 1) {
          cor_value <- cor(common_data[[tree1]], common_data[[tree2]])

          pairwise_correlations <- rbind(pairwise_correlations, data.frame(
            tree1 = tree1,
            tree2 = tree2,
            correlation = cor_value,
            n_observations = nrow(common_data),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  } else {
    cor_matrix <- NULL
    pairwise_correlations <- NULL
  }

  return(list(
    temporal_data = temporal_data,
    correlation_matrix = cor_matrix,
    pairwise_correlations = pairwise_correlations,
    time_window = time_window
  ))
}

#' Generate method comparison across trees
#' @param velocity_results Data frame with velocity results
#' @param sap_data Multi-tree sap_data object
#' @return Data frame with method performance comparison
#' @keywords internal
compare_trees_methods <- function(velocity_results, sap_data) {

  if (!"method" %in% names(velocity_results)) {
    warning("No method column found, skipping method comparison")
    return(NULL)
  }

  # Method performance per tree
  method_performance <- velocity_results %>%
    dplyr::group_by(.data$tree_id, .data$method) %>%
    dplyr::summarise(
      n_measurements = dplyr::n(),
      mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE),
      median_velocity = median(.data$Vh_cm_hr, na.rm = TRUE),
      sd_velocity = sd(.data$Vh_cm_hr, na.rm = TRUE),
      success_rate = sum(!is.na(.data$Vh_cm_hr)) / dplyr::n(),
      .groups = "drop"
    )

  # Overall method performance
  overall_method_performance <- velocity_results %>%
    dplyr::group_by(.data$method) %>%
    dplyr::summarise(
      n_measurements = dplyr::n(),
      n_trees = length(unique(.data$tree_id)),
      mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE),
      median_velocity = median(.data$Vh_cm_hr, na.rm = TRUE),
      sd_velocity = sd(.data$Vh_cm_hr, na.rm = TRUE),
      success_rate = sum(!is.na(.data$Vh_cm_hr)) / dplyr::n(),
      .groups = "drop"
    )

  # Method consistency across trees
  method_consistency <- velocity_results %>%
    dplyr::group_by(.data$method, .data$tree_id) %>%
    dplyr::summarise(mean_velocity = mean(.data$Vh_cm_hr, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$method) %>%
    dplyr::summarise(
      n_trees = dplyr::n(),
      mean_across_trees = mean(.data$mean_velocity, na.rm = TRUE),
      sd_across_trees = sd(.data$mean_velocity, na.rm = TRUE),
      cv_across_trees = .data$sd_across_trees / .data$mean_across_trees,
      .groups = "drop"
    )

  return(list(
    method_performance = method_performance,
    overall_method_performance = overall_method_performance,
    method_consistency = method_consistency
  ))
}

#' Generate statistical comparison of trees
#' @param velocity_results Data frame with velocity results
#' @param sap_data Multi-tree sap_data object
#' @return List with statistical test results
#' @keywords internal
compare_trees_statistical <- function(velocity_results, sap_data) {

  tree_ids <- unique(velocity_results$tree_id)

  if (length(tree_ids) < 2) {
    warning("Need at least 2 trees for statistical comparison")
    return(NULL)
  }

  # Extract velocity data by tree
  tree_velocities <- split(velocity_results$Vh_cm_hr, velocity_results$tree_id)
  tree_velocities <- tree_velocities[!sapply(tree_velocities, function(x) all(is.na(x)))]

  if (length(tree_velocities) < 2) {
    warning("Insufficient valid data for statistical comparison")
    return(NULL)
  }

  # ANOVA test
  anova_result <- tryCatch({
    # Create data frame for ANOVA
    anova_data <- data.frame(
      velocity = unlist(tree_velocities),
      tree_id = rep(names(tree_velocities), lengths(tree_velocities))
    )
    anova_data <- anova_data[!is.na(anova_data$velocity), ]

    if (nrow(anova_data) > 0 && length(unique(anova_data$tree_id)) > 1) {
      aov_result <- aov(velocity ~ tree_id, data = anova_data)
      summary(aov_result)
    } else {
      NULL
    }
  }, error = function(e) {
    warning("ANOVA test failed: ", e$message)
    NULL
  })

  # Pairwise t-tests
  pairwise_tests <- data.frame(
    tree1 = character(),
    tree2 = character(),
    p_value = numeric(),
    mean_diff = numeric(),
    stringsAsFactors = FALSE
  )

  if (length(tree_velocities) >= 2) {
    tree_names <- names(tree_velocities)

    for (i in 1:(length(tree_names) - 1)) {
      for (j in (i + 1):length(tree_names)) {
        tree1 <- tree_names[i]
        tree2 <- tree_names[j]

        # Perform t-test
        t_test_result <- tryCatch({
          t.test(tree_velocities[[tree1]], tree_velocities[[tree2]])
        }, error = function(e) {
          NULL
        })

        if (!is.null(t_test_result)) {
          pairwise_tests <- rbind(pairwise_tests, data.frame(
            tree1 = tree1,
            tree2 = tree2,
            p_value = t_test_result$p.value,
            mean_diff = t_test_result$estimate[1] - t_test_result$estimate[2],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  # Effect sizes (Cohen's d)
  effect_sizes <- data.frame(
    tree1 = character(),
    tree2 = character(),
    cohens_d = numeric(),
    stringsAsFactors = FALSE
  )

  if (nrow(pairwise_tests) > 0) {
    for (i in seq_len(nrow(pairwise_tests))) {
      tree1 <- pairwise_tests$tree1[i]
      tree2 <- pairwise_tests$tree2[i]

      # Calculate Cohen's d
      d_value <- tryCatch({
        cohens_d(tree_velocities[[tree1]], tree_velocities[[tree2]])
      }, error = function(e) {
        NA
      })

      effect_sizes <- rbind(effect_sizes, data.frame(
        tree1 = tree1,
        tree2 = tree2,
        cohens_d = d_value,
        stringsAsFactors = FALSE
      ))
    }

    # Merge with pairwise tests
    pairwise_tests <- pairwise_tests %>%
      dplyr::left_join(effect_sizes, by = c("tree1", "tree2"))
  }

  return(list(
    anova_result = anova_result,
    pairwise_tests = pairwise_tests,
    n_trees = length(tree_velocities),
    tree_names = names(tree_velocities)
  ))
}

#' Calculate Cohen's d effect size
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Cohen's d value
#' @keywords internal
cohens_d <- function(x, y) {
  # Remove NA values
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  if (length(x) == 0 || length(y) == 0) {
    return(NA)
  }

  # Calculate means and standard deviations
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x <- sd(x)
  sd_y <- sd(y)

  # Calculate pooled standard deviation
  n_x <- length(x)
  n_y <- length(y)
  pooled_sd <- sqrt(((n_x - 1) * sd_x^2 + (n_y - 1) * sd_y^2) / (n_x + n_y - 2))

  # Calculate Cohen's d
  d <- (mean_x - mean_y) / pooled_sd

  return(d)
}

#' Print tree comparison results
#' @param x Tree comparison object
#' @param ... Additional arguments
#' @export
print.tree_comparison <- function(x, ...) {
  cat("Tree Comparison Results\n")
  cat("======================\n\n")

  cat("Metadata:\n")
  cat("  Number of trees:", x$metadata$n_trees, "\n")
  cat("  Tree IDs:", paste(x$metadata$tree_ids, collapse = ", "), "\n")
  cat("  Total measurements:", x$metadata$total_measurements, "\n")
  cat("  Comparison types:", paste(x$metadata$comparison_types, collapse = ", "), "\n")
  cat("  Time window:", x$metadata$time_window, "\n")
  cat("  Quality filter:", x$metadata$quality_filter, "\n\n")

  if ("summary" %in% names(x)) {
    cat("Summary Statistics:\n")
    print(x$summary)
    cat("\n")
  }

  if ("temporal" %in% names(x) && !is.null(x$temporal)) {
    cat("Temporal Analysis:\n")
    if (!is.null(x$temporal$pairwise_correlations)) {
      cat("  Pairwise correlations:\n")
      print(x$temporal$pairwise_correlations)
    }
    cat("\n")
  }

  if ("method" %in% names(x) && !is.null(x$method)) {
    cat("Method Comparison:\n")
    if (!is.null(x$method$overall_method_performance)) {
      cat("  Overall method performance:\n")
      print(x$method$overall_method_performance)
    }
    cat("\n")
  }

  if ("statistical" %in% names(x) && !is.null(x$statistical)) {
    cat("Statistical Tests:\n")
    if (!is.null(x$statistical$pairwise_tests)) {
      cat("  Pairwise t-tests:\n")
      print(x$statistical$pairwise_tests)
    }
    cat("\n")
  }
}