#' Cross-Method Comparison and Validation Tools
#'
#' Advanced tools for comparing heat pulse velocity methods, validating
#' results across different calculation approaches, and identifying
#' method-specific issues.
#'
#' @name cross_method_comparison
NULL

#' Compare Heat Pulse Velocity Methods
#'
#' @description
#' Comprehensive comparison of different HPV calculation methods including
#' statistical analysis, agreement metrics, and method-specific diagnostics.
#'
#' @param vh_results A vh_results object containing multiple methods
#' @param reference_method Character, method to use as reference (default: "HRM")
#' @param agreement_tolerance Numeric, tolerance for method agreement in cm/hr (default: 5)
#' @param exclude_outliers Logical, whether to exclude outliers from comparison (default: TRUE)
#' @param confidence_level Numeric, confidence level for agreement intervals (default: 0.95)
#'
#' @return List containing:
#'   \describe{
#'     \item{method_summary}{Summary statistics for each method}
#'     \item{pairwise_comparison}{Pairwise comparison between methods}
#'     \item{agreement_analysis}{Method agreement analysis (Bland-Altman style)}
#'     \item{correlation_matrix}{Correlation matrix between methods}
#'     \item{bias_analysis}{Systematic bias analysis between methods}
#'     \item{recommendations}{Method-specific recommendations}
#'   }
#'
#' @details
#' Performs comprehensive cross-method validation including:
#' - **Statistical Comparison**: Mean, median, standard deviation for each method
#' - **Agreement Analysis**: Bland-Altman style analysis for method agreement
#' - **Bias Detection**: Systematic bias identification between methods
#' - **Correlation Analysis**: Pearson and Spearman correlations
#' - **Range-Specific Analysis**: Method performance in different velocity ranges
#'
#' @examples
#' \dontrun{
#' # Calculate multiple methods
#' vh_results <- calc_heat_pulse_velocity(sap_data, methods = c("HRM", "MHR", "Tmax_Klu", "DMA"))
#'
#' # Compare methods
#' comparison <- compare_hpv_methods(vh_results)
#' print(comparison$method_summary)
#' print(comparison$recommendations)
#' }
#'
#' @seealso \code{\link{calc_heat_pulse_velocity}}, \code{\link{plot_method_comparison}}
#' @export
compare_hpv_methods <- function(vh_results, reference_method = "HRM",
                                agreement_tolerance = 5, exclude_outliers = TRUE,
                                confidence_level = 0.95) {

  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object")
  }

  # Convert to data frame for easier manipulation
  results_df <- as.data.frame(vh_results)

  # Get available methods
  methods <- unique(results_df$method)
  if (length(methods) < 2) {
    stop("Need at least 2 methods for comparison")
  }

  # Check if reference method is available
  if (!reference_method %in% methods) {
    reference_method <- methods[1]
    warning("Reference method not found, using ", reference_method)
  }

  # Filter outliers if requested
  if (exclude_outliers) {
    results_df <- results_df[results_df$quality_flag %in% c("OK"), ]
  }

  if (nrow(results_df) == 0) {
    stop("No valid data remaining after filtering")
  }

  # Initialize results
  method_summary <- list()
  pairwise_comparison <- list()
  agreement_analysis <- list()
  bias_analysis <- list()
  recommendations <- character(0)

  # 1. Method Summary Statistics
  for (method in methods) {
    method_data <- results_df[results_df$method == method, "Vh_cm_hr"]
    method_data <- method_data[!is.na(method_data) & is.finite(method_data)]

    if (length(method_data) > 0) {
      method_summary[[method]] <- list(
        n_observations = length(method_data),
        mean = mean(method_data),
        median = median(method_data),
        std = sd(method_data),
        min = min(method_data),
        max = max(method_data),
        q25 = quantile(method_data, 0.25),
        q75 = quantile(method_data, 0.75),
        coefficient_of_variation = sd(method_data) / abs(mean(method_data))
      )
    } else {
      method_summary[[method]] <- list(
        n_observations = 0,
        mean = NA, median = NA, std = NA,
        min = NA, max = NA, q25 = NA, q75 = NA,
        coefficient_of_variation = NA
      )
    }
  }

  # 2. Pairwise Comparison Matrix
  correlation_matrix <- matrix(NA, length(methods), length(methods))
  rownames(correlation_matrix) <- methods
  colnames(correlation_matrix) <- methods

  # Create wide format for correlation analysis
  results_wide <- reshape(results_df[, c("datetime", "pulse_id", "method", "Vh_cm_hr")],
                          idvar = c("datetime", "pulse_id"),
                          timevar = "method",
                          direction = "wide")

  # Calculate correlations
  velocity_cols <- grep("Vh_cm_hr", names(results_wide), value = TRUE)
  if (length(velocity_cols) >= 2) {
    velocity_data <- results_wide[, velocity_cols, drop = FALSE]

    # Remove rows with any missing values for correlation
    complete_rows <- complete.cases(velocity_data)
    if (sum(complete_rows) >= 10) {
      velocity_complete <- velocity_data[complete_rows, ]

      # Calculate correlation matrix
      cor_matrix <- cor(velocity_complete, use = "complete.obs")

      # Map back to method names
      method_names_from_cols <- gsub("Vh_cm_hr\\.", "", colnames(velocity_complete))
      for (i in seq_along(method_names_from_cols)) {
        for (j in seq_along(method_names_from_cols)) {
          method_i <- method_names_from_cols[i]
          method_j <- method_names_from_cols[j]
          if (method_i %in% methods && method_j %in% methods) {
            correlation_matrix[method_i, method_j] <- cor_matrix[i, j]
          }
        }
      }
    }
  }

  # 3. Agreement Analysis (Bland-Altman style)
  for (method in methods) {
    if (method != reference_method) {
      # Get matched pairs
      ref_data <- results_df[results_df$method == reference_method, c("datetime", "pulse_id", "Vh_cm_hr")]
      method_data <- results_df[results_df$method == method, c("datetime", "pulse_id", "Vh_cm_hr")]

      # Merge by datetime and pulse_id
      merged_data <- merge(ref_data, method_data, by = c("datetime", "pulse_id"),
                           suffixes = c("_ref", "_method"))

      if (nrow(merged_data) >= 10) {
        # Remove infinite and missing values
        valid_pairs <- is.finite(merged_data$Vh_cm_hr_ref) &
          is.finite(merged_data$Vh_cm_hr_method)
        merged_clean <- merged_data[valid_pairs, ]

        if (nrow(merged_clean) >= 10) {
          # Calculate agreement metrics
          differences <- merged_clean$Vh_cm_hr_method - merged_clean$Vh_cm_hr_ref
          means <- (merged_clean$Vh_cm_hr_method + merged_clean$Vh_cm_hr_ref) / 2

          mean_difference <- mean(differences)
          sd_difference <- sd(differences)

          # Limits of agreement
          alpha <- 1 - confidence_level
          t_value <- qt(1 - alpha/2, length(differences) - 1)

          agreement_analysis[[paste(method, "vs", reference_method)]] <- list(
            n_pairs = nrow(merged_clean),
            mean_difference = mean_difference,
            sd_difference = sd_difference,
            lower_limit = mean_difference - 1.96 * sd_difference,
            upper_limit = mean_difference + 1.96 * sd_difference,
            pearson_correlation = cor(merged_clean$Vh_cm_hr_ref, merged_clean$Vh_cm_hr_method),
            within_tolerance = sum(abs(differences) <= agreement_tolerance) / length(differences) * 100
          )

          # Bias analysis
          if (abs(mean_difference) > agreement_tolerance) {
            bias_direction <- ifelse(mean_difference > 0, "overestimates", "underestimates")
            bias_analysis[[method]] <- paste(method, bias_direction, "compared to", reference_method,
                                             "by", round(abs(mean_difference), 2), "cm/hr on average")
          }
        }
      }
    }
  }

  # 4. Range-Specific Analysis
  # Define velocity ranges for analysis
  velocity_ranges <- list(
    "Low" = c(-Inf, 10),
    "Medium" = c(10, 50),
    "High" = c(50, Inf)
  )

  range_analysis <- list()
  for (range_name in names(velocity_ranges)) {
    range_limits <- velocity_ranges[[range_name]]

    # Filter data to range (using reference method)
    ref_data <- results_df[results_df$method == reference_method, ]
    in_range <- ref_data$Vh_cm_hr >= range_limits[1] & ref_data$Vh_cm_hr < range_limits[2]
    range_timestamps <- ref_data[in_range, c("datetime", "pulse_id")]

    if (nrow(range_timestamps) >= 5) {
      range_results <- merge(results_df, range_timestamps, by = c("datetime", "pulse_id"))

      range_summary <- aggregate(Vh_cm_hr ~ method, range_results,
                                 function(x) c(mean = mean(x, na.rm = TRUE),
                                               sd = sd(x, na.rm = TRUE),
                                               n = sum(!is.na(x))))

      range_analysis[[range_name]] <- range_summary
    }
  }

  # 5. Generate Recommendations

  # Check for high correlations
  if (!is.null(correlation_matrix)) {
    low_correlations <- which(correlation_matrix < 0.7 & correlation_matrix != 1, arr.ind = TRUE)
    if (nrow(low_correlations) > 0) {
      recommendations <- c(recommendations,
                           "Some methods show low correlation - check data quality and method appropriateness")
    }
  }

  # Check for significant biases
  if (length(bias_analysis) > 0) {
    recommendations <- c(recommendations,
                         "Systematic biases detected between methods - see bias_analysis for details")
  }

  # Check for high variability
  cv_values <- sapply(method_summary, function(x) x$coefficient_of_variation)
  high_cv <- names(cv_values)[cv_values > 1.0 & !is.na(cv_values)]
  if (length(high_cv) > 0) {
    recommendations <- c(recommendations,
                         paste("High variability in methods:", paste(high_cv, collapse = ", ")))
  }

  return(list(
    method_summary = method_summary,
    pairwise_comparison = list(correlation_matrix = correlation_matrix),
    agreement_analysis = agreement_analysis,
    bias_analysis = bias_analysis,
    range_analysis = range_analysis,
    recommendations = recommendations
  ))
}

#' Plot Method Comparison Diagnostics
#'
#' @description
#' Create comprehensive diagnostic plots for method comparison including
#' scatter plots, Bland-Altman plots, and time series comparisons.
#'
#' @param vh_results A vh_results object with multiple methods
#' @param comparison_results Optional, results from compare_hpv_methods()
#' @param plot_type Character, type of plot: "scatter", "bland_altman", "time_series", "correlation"
#' @param reference_method Character, reference method for comparison (default: "HRM")
#' @param exclude_outliers Logical, whether to exclude outliers (default: TRUE)
#'
#' @return Base R plot or ggplot object (if ggplot2 available)
#'
#' @examples
#' \dontrun{
#' # Scatter plot comparison
#' plot_method_comparison(vh_results, plot_type = "scatter")
#'
#' # Bland-Altman plot
#' plot_method_comparison(vh_results, plot_type = "bland_altman")
#'
#' # Time series comparison
#' plot_method_comparison(vh_results, plot_type = "time_series")
#' }
#'
#' @export
plot_method_comparison <- function(vh_results, comparison_results = NULL,
                                   plot_type = "scatter", reference_method = "HRM",
                                   exclude_outliers = TRUE) {

  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object")
  }

  results_df <- as.data.frame(vh_results)

  # Filter outliers if requested
  if (exclude_outliers) {
    results_df <- results_df[results_df$quality_flag %in% c("OK"), ]
  }

  methods <- unique(results_df$method)
  if (length(methods) < 2) {
    stop("Need at least 2 methods for comparison plots")
  }

  switch(plot_type,
         "scatter" = {
           # Create scatter plot matrix
           if (length(methods) == 2) {
             # Simple scatter plot for 2 methods
             method1_data <- results_df[results_df$method == methods[1], ]
             method2_data <- results_df[results_df$method == methods[2], ]

             merged_data <- merge(method1_data, method2_data,
                                  by = c("datetime", "pulse_id"),
                                  suffixes = c("_1", "_2"))

             if (nrow(merged_data) > 0) {
               plot(merged_data$Vh_cm_hr_1, merged_data$Vh_cm_hr_2,
                    xlab = paste(methods[1], "Velocity (cm/hr)"),
                    ylab = paste(methods[2], "Velocity (cm/hr)"),
                    main = paste("Method Comparison:", methods[1], "vs", methods[2]),
                    pch = 19, cex = 0.6, col = "blue")

               # Add 1:1 line
               abline(0, 1, col = "red", lty = 2, lwd = 2)

               # Add correlation info
               correlation <- cor(merged_data$Vh_cm_hr_1, merged_data$Vh_cm_hr_2, use = "complete.obs")
               text(min(merged_data$Vh_cm_hr_1, na.rm = TRUE),
                    max(merged_data$Vh_cm_hr_2, na.rm = TRUE),
                    paste("r =", round(correlation, 3)),
                    pos = 4, col = "red")

               grid()
             }
           } else {
             # Matrix plot for multiple methods
             pairs(~ ., data = results_df[, c("method", "Vh_cm_hr")],
                   main = "Method Comparison Matrix")
           }
         },

         "bland_altman" = {
           # Bland-Altman plot
           if (!reference_method %in% methods) {
             reference_method <- methods[1]
           }

           other_methods <- setdiff(methods, reference_method)

           par(mfrow = c(length(other_methods), 1))

           for (method in other_methods) {
             ref_data <- results_df[results_df$method == reference_method, ]
             method_data <- results_df[results_df$method == method, ]

             merged_data <- merge(ref_data, method_data,
                                  by = c("datetime", "pulse_id"),
                                  suffixes = c("_ref", "_method"))

             if (nrow(merged_data) >= 5) {
               differences <- merged_data$Vh_cm_hr_method - merged_data$Vh_cm_hr_ref
               means <- (merged_data$Vh_cm_hr_method + merged_data$Vh_cm_hr_ref) / 2

               plot(means, differences,
                    xlab = "Mean Velocity (cm/hr)",
                    ylab = "Difference (cm/hr)",
                    main = paste("Bland-Altman:", method, "vs", reference_method),
                    pch = 19, cex = 0.6, col = "blue")

               # Add mean difference line
               mean_diff <- mean(differences, na.rm = TRUE)
               sd_diff <- sd(differences, na.rm = TRUE)

               abline(h = mean_diff, col = "red", lwd = 2)
               abline(h = mean_diff + 1.96 * sd_diff, col = "red", lty = 2)
               abline(h = mean_diff - 1.96 * sd_diff, col = "red", lty = 2)

               # Add labels
               text(min(means, na.rm = TRUE), mean_diff + 1.96 * sd_diff,
                    paste("+1.96 SD =", round(mean_diff + 1.96 * sd_diff, 2)),
                    pos = 4, cex = 0.8)
               text(min(means, na.rm = TRUE), mean_diff,
                    paste("Mean =", round(mean_diff, 2)),
                    pos = 4, cex = 0.8)
               text(min(means, na.rm = TRUE), mean_diff - 1.96 * sd_diff,
                    paste("-1.96 SD =", round(mean_diff - 1.96 * sd_diff, 2)),
                    pos = 4, cex = 0.8)

               grid()
             }
           }
         },

         "time_series" = {
           # Time series comparison
           plot(range(results_df$datetime, na.rm = TRUE),
                range(results_df$Vh_cm_hr, na.rm = TRUE),
                type = "n",
                xlab = "Date/Time",
                ylab = "Velocity (cm/hr)",
                main = "Method Comparison - Time Series")

           colours <- rainbow(length(methods))

           for (i in seq_along(methods)) {
             method_data <- results_df[results_df$method == methods[i], ]
             lines(method_data$datetime, method_data$Vh_cm_hr,
                   col = colours[i], lwd = 1.5)
           }

           legend("topright", legend = methods, col = colours, lwd = 1.5)
           grid()
         },

         "correlation" = {
           # Correlation heatmap (simplified for base R)
           if (!is.null(comparison_results) &&
               !is.null(comparison_results$pairwise_comparison$correlation_matrix)) {

             cor_matrix <- comparison_results$pairwise_comparison$correlation_matrix

             # Simple correlation plot using image
             image(1:ncol(cor_matrix), 1:nrow(cor_matrix), t(cor_matrix),
                   xlab = "Method", ylab = "Method",
                   main = "Method Correlation Matrix",
                   axes = FALSE, col = heat.colours(20))

             axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2)
             axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix), las = 1)

             # Add correlation values as text
             for (i in 1:nrow(cor_matrix)) {
               for (j in 1:ncol(cor_matrix)) {
                 if (!is.na(cor_matrix[i, j])) {
                   text(j, i, round(cor_matrix[i, j], 2), cex = 0.8)
                 }
               }
             }
           } else {
             plot(1, 1, type = "n", main = "Correlation matrix not available")
             text(1, 1, "Run compare_hpv_methods() first")
           }
         },

         stop("Unsupported plot_type: ", plot_type)
  )
}