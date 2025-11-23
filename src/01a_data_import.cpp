// src/01a_data_import.cpp
// C++ implementations for fast data import and parsing
// Optimised for speed when importing large ICT sensor data files

#include <Rcpp.h>
#include <string>
#include <vector>
#include <cctype>
#include <cstdlib>
using namespace Rcpp;


//' Fast Number Extraction from Text Records - C++ Implementation
//'
//' @description
//' Fast extraction of all numeric values from text records (JSON-like format).
//' Much faster than regex-based extraction in R, especially for large datasets.
//'
//' @param records Character vector of text records to parse
//' @param expected_numbers_per_record Integer, expected number of values per record
//'
//' @return Numeric matrix where each row contains numbers from one record
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
NumericMatrix extract_numbers_from_records_cpp(CharacterVector records,
                                                 int expected_numbers_per_record) {

  int n_records = records.size();
  NumericMatrix result(n_records, expected_numbers_per_record);

  // Fill with NA by default
  std::fill(result.begin(), result.end(), NA_REAL);

  // Process each record
  for (int i = 0; i < n_records; i++) {
    std::string record = Rcpp::as<std::string>(records[i]);
    std::vector<double> numbers;
    numbers.reserve(expected_numbers_per_record);

    // Manual parsing - faster than regex for simple number extraction
    std::string current_number = "";
    bool in_number = false;
    bool has_decimal = false;
    bool is_negative = false;

    for (size_t j = 0; j < record.length(); j++) {
      char c = record[j];

      if (std::isdigit(c)) {
        // Digit - part of number
        current_number += c;
        in_number = true;
      } else if (c == '.' && in_number && !has_decimal) {
        // Decimal point - only one allowed per number
        current_number += c;
        has_decimal = true;
      } else if (c == '-' && !in_number && (j == 0 || !std::isdigit(record[j-1]))) {
        // Negative sign at start of number
        is_negative = true;
        in_number = true;
      } else if (c == 'e' || c == 'E') {
        // Scientific notation
        if (in_number && j + 1 < record.length()) {
          current_number += c;
          // Check for sign after 'e'
          if (record[j+1] == '+' || record[j+1] == '-') {
            j++;
            current_number += record[j];
          }
        }
      } else {
        // Not part of a number - finish current number if any
        // IMPORTANT: Only accept numbers with decimal points (matching R regex: [-+]?\d+\.\d+)
        if (in_number && !current_number.empty() && has_decimal) {
          double value = std::atof(current_number.c_str());
          if (is_negative) value = -value;
          numbers.push_back(value);

          // Reset for next number
          current_number = "";
          in_number = false;
          has_decimal = false;
          is_negative = false;
        } else if (in_number) {
          // Reset without adding (integer without decimal point)
          current_number = "";
          in_number = false;
          has_decimal = false;
          is_negative = false;
        }
      }
    }

    // Handle final number if record ends with a digit
    // Only accept if it has a decimal point
    if (in_number && !current_number.empty() && has_decimal) {
      double value = std::atof(current_number.c_str());
      if (is_negative) value = -value;
      numbers.push_back(value);
    }

    // Copy to result matrix
    int n_extracted = std::min(static_cast<int>(numbers.size()), expected_numbers_per_record);
    for (int k = 0; k < n_extracted; k++) {
      result(i, k) = numbers[k];
    }
  }

  return result;
}


//' Parse ICT Current Format Records - C++ Implementation
//'
//' @description
//' Fast batch parsing of ICT current format records into diagnostics and measurements.
//' Replaces R loop that processes records one-by-one.
//'
//' @param records Character vector of pulse records (without leading delimiter)
//' @param pulse_times Numeric vector of pulse timestamps (as.numeric(POSIXct))
//' @param measurement_interval Integer, seconds between consecutive measurements
//' @param expected_diagnostics Integer, number of diagnostic values per pulse (default: 5)
//'
//' @return List containing:
//'   \item{diagnostics}{Data frame with diagnostic values for each pulse}
//'   \item{measurements}{Data frame with temperature measurements}
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List parse_ict_records_cpp(CharacterVector records,
                            NumericVector pulse_times,
                            int measurement_interval,
                            int expected_diagnostics = 5) {

  int n_pulses = records.size();

  if (n_pulses != pulse_times.size()) {
    stop("Number of records must match number of pulse times");
  }

  // FIXED: Sample multiple pulses to determine structure (handles incomplete first pulse)
  // The first pulse may be truncated/incomplete, so we need to find the typical structure
  int n_sample = std::min(20, n_pulses);  // Sample up to 20 pulses
  int max_numbers = 0;

  for (int sample_idx = 0; sample_idx < n_sample; sample_idx++) {
    CharacterVector sample_record_vec = CharacterVector::create(records[sample_idx]);
    NumericMatrix sample_numbers = extract_numbers_from_records_cpp(sample_record_vec, 600);

    // Count non-NA values in this pulse
    int pulse_numbers = 0;
    for (int k = 0; k < 600; k++) {
      if (!NumericVector::is_na(sample_numbers(0, k))) {
        pulse_numbers++;
      } else {
        break;  // Stop at first NA
      }
    }

    // Track maximum (this will be the complete pulse structure)
    if (pulse_numbers > max_numbers) {
      max_numbers = pulse_numbers;
    }
  }

  int total_numbers = max_numbers;
  int temp_values_per_pulse = total_numbers - expected_diagnostics;

  // Calculate measurements per pulse (4 sensors × n measurements)
  int n_measurements_per_pulse = temp_values_per_pulse / 4;

  if (n_measurements_per_pulse < 1) {
    stop("Invalid data structure: insufficient temperature measurements");
  }

  // Extract all numbers from all records
  NumericMatrix all_numbers = extract_numbers_from_records_cpp(records, total_numbers);

  // Preallocate diagnostics vectors
  NumericVector diag_batt_volt(n_pulses);
  NumericVector diag_batt_current(n_pulses);
  NumericVector diag_batt_temp(n_pulses);
  NumericVector diag_ext1(n_pulses);
  NumericVector diag_ext2(n_pulses);
  IntegerVector diag_pulse_id(n_pulses);
  NumericVector diag_datetime(n_pulses);

  // Preallocate measurement vectors
  int total_measurements = n_pulses * n_measurements_per_pulse;
  IntegerVector meas_pulse_id(total_measurements);
  NumericVector meas_datetime(total_measurements);
  NumericVector meas_do(total_measurements);
  NumericVector meas_di(total_measurements);
  NumericVector meas_uo(total_measurements);
  NumericVector meas_ui(total_measurements);

  // Fill diagnostics and measurements
  int meas_idx = 0;

  for (int i = 0; i < n_pulses; i++) {
    // Extract diagnostics (first 5 values)
    diag_pulse_id[i] = i + 1;  // 1-based
    diag_datetime[i] = pulse_times[i];
    diag_batt_volt[i] = all_numbers(i, 0);
    diag_batt_current[i] = all_numbers(i, 1);
    diag_batt_temp[i] = all_numbers(i, 2);
    diag_ext1[i] = all_numbers(i, 3);
    diag_ext2[i] = all_numbers(i, 4);

    // Extract temperature measurements
    // Data structure: [diagnostics] [do1, di1, uo1, ui1, do2, di2, uo2, ui2, ...]
    for (int m = 0; m < n_measurements_per_pulse; m++) {
      int base_idx = expected_diagnostics + (m * 4);

      meas_pulse_id[meas_idx] = i + 1;  // 1-based
      meas_datetime[meas_idx] = pulse_times[i] + (m * measurement_interval);

      // Extract 4 temperature values
      if (base_idx + 3 < total_numbers) {
        meas_do[meas_idx] = all_numbers(i, base_idx);
        meas_di[meas_idx] = all_numbers(i, base_idx + 1);
        meas_uo[meas_idx] = all_numbers(i, base_idx + 2);
        meas_ui[meas_idx] = all_numbers(i, base_idx + 3);
      } else {
        // Handle edge case of incomplete data
        meas_do[meas_idx] = NA_REAL;
        meas_di[meas_idx] = NA_REAL;
        meas_uo[meas_idx] = NA_REAL;
        meas_ui[meas_idx] = NA_REAL;
      }

      meas_idx++;
    }
  }

  // Create data frames
  DataFrame diagnostics = DataFrame::create(
    Named("pulse_id") = diag_pulse_id,
    Named("datetime") = diag_datetime,
    Named("batt_volt") = diag_batt_volt,
    Named("batt_current") = diag_batt_current,
    Named("batt_temp") = diag_batt_temp,
    Named("ext1") = diag_ext1,
    Named("ext2") = diag_ext2
  );

  DataFrame measurements = DataFrame::create(
    Named("pulse_id") = meas_pulse_id,
    Named("datetime") = meas_datetime,
    Named("do") = meas_do,
    Named("di") = meas_di,
    Named("uo") = meas_uo,
    Named("ui") = meas_ui
  );

  return List::create(
    Named("diagnostics") = diagnostics,
    Named("measurements") = measurements,
    Named("n_measurements_per_pulse") = n_measurements_per_pulse
  );
}


//' Fast Datetime Extraction from Text Records - C++ Implementation
//'
//' @description
//' Fast extraction of datetime strings from JSON-like records.
//' Looks for "date" field and extracts the value.
//'
//' @param records Character vector of text records
//'
//' @return Character vector of datetime strings
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
CharacterVector extract_datetimes_cpp(CharacterVector records) {

  int n_records = records.size();
  CharacterVector result(n_records);

  for (int i = 0; i < n_records; i++) {
    std::string record = Rcpp::as<std::string>(records[i]);
    std::string datetime = "";

    // Find "date" field
    size_t date_pos = record.find("\"date\"");
    if (date_pos != std::string::npos) {
      // Find the value after "date":"
      size_t value_start = record.find("\"", date_pos + 6);
      if (value_start != std::string::npos) {
        value_start++;  // Skip the opening quote
        size_t value_end = record.find("\"", value_start);
        if (value_end != std::string::npos) {
          datetime = record.substr(value_start, value_end - value_start);
        }
      }
    }

    result[i] = datetime;
  }

  return result;
}
