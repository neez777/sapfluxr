// src/01f_data_quality.cpp
// C++ implementations of computationally intensive quality control functions
// Corresponds to R/01f_data_quality.R

#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' Detect Outliers Using Rolling Mean (C++ Implementation)
//'
//' @description
//' Fast C++ implementation of rolling mean outlier detection.
//' Identifies points that deviate significantly from their local rolling mean.
//'
//' @param vh_values Numeric vector of Vh values
//' @param window Integer, half-width of rolling window (default: 5, meaning 11-point window)
//' @param threshold Numeric, standard deviation multiplier (default: 3 = 99.7% confidence)
//'
//' @return Integer vector of outlier indices (1-based for R compatibility)
//'
//' @details
//' For each point i, calculates mean and SD of window [(i-window):(i+window)].
//' Points deviating by more than threshold × SD from the local mean are flagged.
//'
//' **Performance:** O(n × window) but with fast C++ loops and minimal overhead.
//' Typically 10-50x faster than pure R implementation for large datasets.
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
IntegerVector detect_outliers_rolling_mean_cpp(NumericVector vh_values,
                                                int window = 5,
                                                double threshold = 3.0) {
  int n = vh_values.size();
  std::vector<int> outlier_indices;

  // Need at least 2*window + 1 points
  if (n < 2 * window + 1) {
    return IntegerVector(outlier_indices.begin(), outlier_indices.end());
  }

  // Loop through points that have full windows
  for (int i = window; i < n - window; i++) {
    // Skip NA values
    if (NumericVector::is_na(vh_values[i])) {
      continue;
    }

    // Calculate window statistics
    double sum = 0.0;
    double sum_sq = 0.0;
    int count = 0;

    for (int j = i - window; j <= i + window; j++) {
      if (!NumericVector::is_na(vh_values[j])) {
        sum += vh_values[j];
        sum_sq += vh_values[j] * vh_values[j];
        count++;
      }
    }

    // Need at least 2 non-NA values to calculate SD
    if (count < 2) {
      continue;
    }

    // Calculate mean and SD
    double window_mean = sum / count;
    double variance = (sum_sq - sum * sum / count) / (count - 1);

    // Handle potential negative variance from floating point errors
    if (variance < 0) {
      variance = 0;
    }

    double window_sd = std::sqrt(variance);

    // Check for outlier
    if (window_sd > 0) {
      double deviation = std::abs(vh_values[i] - window_mean) / window_sd;

      if (deviation > threshold) {
        outlier_indices.push_back(i + 1);  // +1 for R's 1-based indexing
      }
    }
  }

  return IntegerVector(outlier_indices.begin(), outlier_indices.end());
}


//' Detect Rate of Change Outliers (C++ Implementation)
//'
//' @description
//' Fast C++ implementation of rate of change outlier detection.
//' Identifies consecutive measurements that change by more than the specified threshold.
//'
//' @param vh_values Numeric vector of Vh values (must be sorted by time)
//' @param max_change Numeric, maximum allowed change between consecutive points (cm/hr)
//'
//' @return Integer vector of outlier indices (1-based, returns the second point in each pair)
//'
//' @details
//' Sap flow typically changes gradually. Large jumps (e.g., > 4 cm/hr between
//' 30-min intervals) are likely sensor errors or logging issues.
//'
//' **Performance:** O(n) with fast C++ loops.
//' Typically 20-100x faster than pure R implementation.
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
IntegerVector detect_rate_of_change_outliers_cpp(NumericVector vh_values,
                                                  double max_change = 4.0) {
  int n = vh_values.size();
  std::vector<int> outlier_indices;

  if (n < 2) {
    return IntegerVector(outlier_indices.begin(), outlier_indices.end());
  }

  // Loop through consecutive pairs
  for (int i = 1; i < n; i++) {
    // Skip if either value is NA
    if (NumericVector::is_na(vh_values[i]) || NumericVector::is_na(vh_values[i - 1])) {
      continue;
    }

    double change = std::abs(vh_values[i] - vh_values[i - 1]);

    if (change > max_change) {
      outlier_indices.push_back(i + 1);  // +1 for R's 1-based indexing
    }
  }

  return IntegerVector(outlier_indices.begin(), outlier_indices.end());
}
