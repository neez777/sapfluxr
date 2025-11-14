// src/02a_data_filtering.cpp
// C++ implementations for data filtering and interpolation
// Optimized for speed when processing quality-flagged velocity data

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>
using namespace Rcpp;


//' Identify Gaps in Time Series - C++ Implementation
//'
//' @description
//' Fast gap detection that identifies consecutive NA values and calculates gap durations.
//' Much faster than R version, especially for large datasets.
//'
//' @param datetimes Numeric vector of timestamps (as.numeric(POSIXct))
//' @param vh_values Numeric vector of velocities (may contain NA)
//' @param is_na Logical vector indicating which values are NA (pre-computed for efficiency)
//'
//' @return List containing gap information:
//'   \item{start_idx}{Integer vector of gap start indices (1-based)}
//'   \item{end_idx}{Integer vector of gap end indices (1-based)}
//'   \item{n_missing}{Integer vector of number of missing points per gap}
//'   \item{duration_hours}{Numeric vector of gap durations in hours}
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List identify_gaps_cpp(NumericVector datetimes,
                       NumericVector vh_values,
                       LogicalVector is_na) {

  int n = vh_values.size();

  // Find runs of NA values using efficient single-pass algorithm
  std::vector<int> gap_starts;
  std::vector<int> gap_ends;
  std::vector<int> gap_lengths;

  bool in_gap = false;
  int gap_start = 0;

  for (int i = 0; i < n; i++) {
    if (is_na[i]) {
      if (!in_gap) {
        // Start of new gap
        gap_start = i;
        in_gap = true;
      }
    } else {
      if (in_gap) {
        // End of gap
        gap_starts.push_back(gap_start + 1);  // +1 for R's 1-based indexing
        gap_ends.push_back(i);  // i is already 1 past the gap end in 0-based, so in 1-based it's correct
        gap_lengths.push_back(i - gap_start);
        in_gap = false;
      }
    }
  }

  // Handle gap at end
  if (in_gap) {
    gap_starts.push_back(gap_start + 1);
    gap_ends.push_back(n);  // n is 1 past end in 1-based
    gap_lengths.push_back(n - gap_start);
  }

  int n_gaps = gap_starts.size();

  // Calculate gap durations in hours
  NumericVector gap_durations(n_gaps);

  for (int i = 0; i < n_gaps; i++) {
    int start_idx = gap_starts[i] - 1;  // Convert to 0-based
    int end_idx = gap_ends[i] - 1;      // Convert to 0-based

    // Find surrounding valid timestamps
    int before_idx = -1;
    int after_idx = -1;

    if (start_idx > 0) before_idx = start_idx - 1;
    if (end_idx < n - 1) after_idx = end_idx + 1;

    if (before_idx >= 0 && after_idx >= 0) {
      // Both sides available - exact duration
      gap_durations[i] = (datetimes[after_idx] - datetimes[before_idx]) / 3600.0;
    } else if (before_idx >= 0 || after_idx >= 0) {
      // Only one side - estimate from pulse interval
      // Calculate median interval from available data
      std::vector<double> diffs;
      for (int j = 1; j < n; j++) {
        double diff = datetimes[j] - datetimes[j-1];
        if (diff > 0 && diff < 7200) {  // Between 0 and 2 hours (reasonable pulse intervals)
          diffs.push_back(diff);
        }
      }

      double interval = 1800.0;  // Default 30 minutes in seconds
      if (!diffs.empty()) {
        std::sort(diffs.begin(), diffs.end());
        interval = diffs[diffs.size() / 2];  // Median
      }

      gap_durations[i] = (gap_lengths[i] * interval) / 3600.0;
    } else {
      // All NA
      gap_durations[i] = NA_REAL;
    }
  }

  return List::create(
    Named("start_idx") = wrap(gap_starts),
    Named("end_idx") = wrap(gap_ends),
    Named("n_missing") = wrap(gap_lengths),
    Named("duration_hours") = gap_durations
  );
}


//' Linear Interpolation - C++ Implementation
//'
//' @description
//' Fast linear interpolation between valid points. Much faster than R's approx()
//' for large datasets.
//'
//' @param vh_values Numeric vector of velocities (with NAs to fill)
//' @param datetimes Numeric vector of timestamps
//' @param gap_start Integer, start index of gap (1-based)
//' @param gap_end Integer, end index of gap (1-based)
//'
//' @return Numeric vector of interpolated values for the gap
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
NumericVector interpolate_linear_cpp(NumericVector vh_values,
                                     NumericVector datetimes,
                                     int gap_start,
                                     int gap_end) {

  // Convert to 0-based indexing
  int start_idx = gap_start - 1;
  int end_idx = gap_end - 1;
  int gap_length = end_idx - start_idx + 1;

  NumericVector result(gap_length);

  // Find valid points before and after gap
  int before_idx = -1;
  int after_idx = -1;

  // Search backwards for valid point before gap
  for (int i = start_idx - 1; i >= 0; i--) {
    if (!NumericVector::is_na(vh_values[i])) {
      before_idx = i;
      break;
    }
  }

  // Search forwards for valid point after gap
  for (int i = end_idx + 1; i < vh_values.size(); i++) {
    if (!NumericVector::is_na(vh_values[i])) {
      after_idx = i;
      break;
    }
  }

  // Check if we can interpolate
  if (before_idx < 0 || after_idx < 0) {
    // Can't interpolate - return NAs
    std::fill(result.begin(), result.end(), NA_REAL);
    return result;
  }

  // Get boundary values and times
  double v_before = vh_values[before_idx];
  double v_after = vh_values[after_idx];
  double t_before = datetimes[before_idx];
  double t_after = datetimes[after_idx];

  // Linear interpolation
  double slope = (v_after - v_before) / (t_after - t_before);

  for (int i = 0; i < gap_length; i++) {
    int idx = start_idx + i;
    double t = datetimes[idx];
    result[i] = v_before + slope * (t - t_before);
  }

  return result;
}


//' Weighted Average Interpolation - C++ Implementation
//'
//' @description
//' Fast inverse-distance weighted interpolation. Uses surrounding valid points
//' with weights inversely proportional to temporal distance.
//'
//' @param vh_values Numeric vector of velocities
//' @param datetimes Numeric vector of timestamps
//' @param gap_start Integer, start index of gap (1-based)
//' @param gap_end Integer, end index of gap (1-based)
//' @param window_size Integer, number of points to consider on each side of gap
//'
//' @return Numeric vector of interpolated values for the gap
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
NumericVector interpolate_weighted_cpp(NumericVector vh_values,
                                       NumericVector datetimes,
                                       int gap_start,
                                       int gap_end,
                                       int window_size) {

  // Convert to 0-based indexing
  int start_idx = gap_start - 1;
  int end_idx = gap_end - 1;
  int gap_length = end_idx - start_idx + 1;
  int n = vh_values.size();

  NumericVector result(gap_length);

  // Ensure reasonable window size
  if (window_size < 2) window_size = std::max(3, gap_length + 2);

  for (int i = 0; i < gap_length; i++) {
    int gap_idx = start_idx + i;
    double t_gap = datetimes[gap_idx];

    // Define window around this gap point
    int window_start = std::max(0, gap_idx - window_size);
    int window_end = std::min(n - 1, gap_idx + window_size);

    // Find valid points in window
    std::vector<int> valid_indices;
    std::vector<double> valid_values;
    std::vector<double> distances;

    for (int j = window_start; j <= window_end; j++) {
      // Skip gap points and invalid values
      if (j >= start_idx && j <= end_idx) continue;
      if (NumericVector::is_na(vh_values[j])) continue;

      valid_indices.push_back(j);
      valid_values.push_back(vh_values[j]);
      distances.push_back(std::abs(datetimes[j] - t_gap));
    }

    // Check if we have enough points
    if (valid_values.size() < 2) {
      // Fall back to linear interpolation
      NumericVector linear_result = interpolate_linear_cpp(vh_values, datetimes,
                                                           gap_idx + 1, gap_idx + 1);
      result[i] = linear_result[0];
      continue;
    }

    // Calculate inverse distance weights
    std::vector<double> weights(valid_values.size());
    double weight_sum = 0.0;

    for (size_t j = 0; j < valid_values.size(); j++) {
      // Inverse distance with small epsilon to avoid division by zero
      weights[j] = 1.0 / (distances[j] + 1e-10);
      weight_sum += weights[j];
    }

    // Normalize weights
    for (size_t j = 0; j < weights.size(); j++) {
      weights[j] /= weight_sum;
    }

    // Calculate weighted average
    double interpolated_value = 0.0;
    for (size_t j = 0; j < valid_values.size(); j++) {
      interpolated_value += valid_values[j] * weights[j];
    }

    result[i] = interpolated_value;
  }

  return result;
}


//' Fast Gap Interpolation for Entire Dataset - C++ Implementation
//'
//' @description
//' Optimized function that interpolates all gaps in a dataset in one pass.
//' Much faster than looping through gaps in R.
//'
//' @param vh_values Numeric vector of velocities (with NAs to fill)
//' @param datetimes Numeric vector of timestamps
//' @param gap_starts Integer vector of gap start indices (1-based)
//' @param gap_ends Integer vector of gap end indices (1-based)
//' @param gap_durations Numeric vector of gap durations in hours
//' @param max_gap_hours Numeric, maximum gap duration to interpolate
//' @param method Character, interpolation method: "linear" or "weighted"
//' @param window_size Integer, window size for weighted interpolation
//'
//' @return List containing:
//'   \item{vh_values}{Numeric vector with gaps filled}
//'   \item{interpolated_indices}{Integer vector of indices that were interpolated (1-based)}
//'   \item{large_gap_indices}{Integer vector of indices of gaps too large to fill (1-based)}
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List interpolate_all_gaps_cpp(NumericVector vh_values,
                               NumericVector datetimes,
                               IntegerVector gap_starts,
                               IntegerVector gap_ends,
                               NumericVector gap_durations,
                               double max_gap_hours,
                               std::string method = "linear",
                               int window_size = 5) {

  // Make a copy to modify
  NumericVector result = clone(vh_values);

  std::vector<int> interpolated_indices;
  std::vector<int> large_gap_indices;

  int n_gaps = gap_starts.size();

  for (int i = 0; i < n_gaps; i++) {
    double gap_duration = gap_durations[i];

    if (NumericVector::is_na(gap_duration) || gap_duration > max_gap_hours) {
      // Gap too large - don't interpolate
      for (int j = gap_starts[i]; j <= gap_ends[i]; j++) {
        large_gap_indices.push_back(j);
      }
      continue;
    }

    // Interpolate this gap
    NumericVector interpolated;

    if (method == "linear") {
      interpolated = interpolate_linear_cpp(result, datetimes, gap_starts[i], gap_ends[i]);
    } else if (method == "weighted") {
      interpolated = interpolate_weighted_cpp(result, datetimes, gap_starts[i], gap_ends[i], window_size);
    } else {
      stop("method must be 'linear' or 'weighted'");
    }

    // Fill the gap
    int gap_length = gap_ends[i] - gap_starts[i] + 1;
    for (int j = 0; j < gap_length; j++) {
      int idx = gap_starts[i] - 1 + j;  // Convert to 0-based
      result[idx] = interpolated[j];
      interpolated_indices.push_back(idx + 1);  // Store as 1-based
    }
  }

  return List::create(
    Named("vh_values") = result,
    Named("interpolated_indices") = wrap(interpolated_indices),
    Named("large_gap_indices") = wrap(large_gap_indices)
  );
}


//' Smart Pre-filter for Flagged Rows - C++ Implementation
//'
//' @description
//' Efficiently identifies rows that need interpolation based on quality flags.
//' Returns indices grouped by method/sensor for efficient processing.
//'
//' @param quality_flags Character vector of quality flags
//' @param flags_to_interpolate Character vector of flags that should be interpolated
//' @param method_col Character vector of method names (optional)
//' @param sensor_col Character vector of sensor positions (optional)
//'
//' @return List containing:
//'   \item{flagged_indices}{Integer vector of all flagged row indices (1-based)}
//'   \item{n_flagged}{Number of flagged rows}
//'   \item{prop_flagged}{Proportion of data that's flagged}
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List prefilter_flagged_rows_cpp(CharacterVector quality_flags,
                                 CharacterVector flags_to_interpolate,
                                 Nullable<CharacterVector> method_col = R_NilValue,
                                 Nullable<CharacterVector> sensor_col = R_NilValue) {

  int n = quality_flags.size();
  std::vector<int> flagged_indices;

  // Create set of flags to interpolate for fast lookup
  std::set<String> flags_set;
  for (int i = 0; i < flags_to_interpolate.size(); i++) {
    flags_set.insert(flags_to_interpolate[i]);
  }

  // Find all flagged rows
  for (int i = 0; i < n; i++) {
    if (flags_set.count(quality_flags[i]) > 0) {
      flagged_indices.push_back(i + 1);  // 1-based indexing
    }
  }

  int n_flagged = flagged_indices.size();
  double prop_flagged = static_cast<double>(n_flagged) / n;

  return List::create(
    Named("flagged_indices") = wrap(flagged_indices),
    Named("n_flagged") = n_flagged,
    Named("prop_flagged") = prop_flagged
  );
}
