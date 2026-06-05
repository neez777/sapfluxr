// src/01e_heat_pulse_velocity.cpp
// C++ implementations of heat pulse velocity calculations
// Corresponds to R/01e_heat_pulse_velocity_core.R

#include <Rcpp.h>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

// Helper struct to store peak information
struct PeakInfo {
  double dTdo_max, dTdi_max, dTuo_max, dTui_max;
  int idx_do, idx_di, idx_uo, idx_ui;
  double time_do, time_di, time_uo, time_ui;
};

// Helper function to find max value and index
inline void find_max_and_idx(const NumericVector& vec, double& max_val, int& max_idx) {
  max_val = R_NegInf;
  max_idx = 0;

  for (int i = 0; i < vec.size(); i++) {
    if (!NumericVector::is_na(vec[i]) && vec[i] > max_val) {
      max_val = vec[i];
      max_idx = i;
    }
  }
}

//' Calculate HRM (Heat Ratio Method) - C++ Implementation
//'
//' @param dTratio_douo Numeric vector of downstream/upstream temperature ratios (outer)
//' @param dTratio_diui Numeric vector of downstream/upstream temperature ratios (inner)
//' @param HRM_period Logical vector indicating HRM sampling window
//' @param tp Numeric vector of time after pulse (seconds)
//' @param diffusivity Thermal diffusivity (cm2/s)
//' @param probe_spacing Probe spacing (cm)
//'
//' @return List containing HRM results for outer and inner sensors
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calc_hrm_cpp(NumericVector dTratio_douo,
                  NumericVector dTratio_diui,
                  LogicalVector HRM_period,
                  NumericVector tp,
                  double diffusivity,
                  double probe_spacing) {

  int n = dTratio_douo.size();

  // Find HRM period indices
  std::vector<int> hrm_indices;
  for (int i = 0; i < n; i++) {
    if (HRM_period[i]) {
      hrm_indices.push_back(i);
    }
  }

  // Initialize return values
  double Vho_HRM = NA_REAL;
  double Vhi_HRM = NA_REAL;
  double dTratio_HRM_douo_mean = NA_REAL;
  double dTratio_HRM_diui_mean = NA_REAL;
  double window_start = NA_REAL;
  double window_end = NA_REAL;

  if (hrm_indices.size() == 0) {
    return List::create(
      Named("outer") = Vho_HRM,
      Named("inner") = Vhi_HRM,
      Named("temp_ratio_outer") = dTratio_HRM_douo_mean,
      Named("temp_ratio_inner") = dTratio_HRM_diui_mean,
      Named("window_start_outer") = window_start,
      Named("window_end_outer") = window_end,
      Named("window_start_inner") = window_start,
      Named("window_end_inner") = window_end,
      Named("calc_time_outer") = NA_REAL,
      Named("calc_time_inner") = NA_REAL
    );
  }

  // Get window boundaries in seconds
  int window_start_idx = hrm_indices[0];
  int window_end_idx = hrm_indices[hrm_indices.size() - 1];
  window_start = tp[window_start_idx];
  window_end = tp[window_end_idx];

  // Calculate means for outer sensors
  double sum_outer = 0.0;
  int count_outer = 0;
  for (int idx : hrm_indices) {
    if (!NumericVector::is_na(dTratio_douo[idx]) &&
        std::isfinite(dTratio_douo[idx]) &&
        dTratio_douo[idx] > 0) {
      sum_outer += dTratio_douo[idx];
      count_outer++;
    }
  }

  if (count_outer > 0) {
    dTratio_HRM_douo_mean = sum_outer / count_outer;
  }

  // Calculate means for inner sensors
  double sum_inner = 0.0;
  int count_inner = 0;
  for (int idx : hrm_indices) {
    if (!NumericVector::is_na(dTratio_diui[idx]) &&
        std::isfinite(dTratio_diui[idx]) &&
        dTratio_diui[idx] > 0) {
      sum_inner += dTratio_diui[idx];
      count_inner++;
    }
  }

  if (count_inner > 0) {
    dTratio_HRM_diui_mean = sum_inner / count_inner;
  }

  // Calculate velocities
  if (!NumericVector::is_na(dTratio_HRM_douo_mean) && dTratio_HRM_douo_mean > 0) {
    Vho_HRM = diffusivity / probe_spacing * std::log(dTratio_HRM_douo_mean) * 3600.0;
  }

  if (!NumericVector::is_na(dTratio_HRM_diui_mean) && dTratio_HRM_diui_mean > 0) {
    Vhi_HRM = diffusivity / probe_spacing * std::log(dTratio_HRM_diui_mean) * 3600.0;
  }

  return List::create(
    Named("outer") = Vho_HRM,
    Named("inner") = Vhi_HRM,
    Named("temp_ratio_outer") = dTratio_HRM_douo_mean,
    Named("temp_ratio_inner") = dTratio_HRM_diui_mean,
    Named("window_start_outer") = window_start,
    Named("window_end_outer") = window_end,
    Named("window_start_inner") = window_start,
    Named("window_end_inner") = window_end,
    Named("calc_time_outer") = NA_REAL,
    Named("calc_time_inner") = NA_REAL
  );
}


//' Calculate MHR (Maximum Heat Ratio) - C++ Implementation
//'
//' @param deltaT_do Numeric vector of delta temperatures (downstream outer)
//' @param deltaT_di Numeric vector of delta temperatures (downstream inner)
//' @param deltaT_uo Numeric vector of delta temperatures (upstream outer)
//' @param deltaT_ui Numeric vector of delta temperatures (upstream inner)
//' @param diffusivity Thermal diffusivity (cm2/s)
//' @param probe_spacing Probe spacing (cm)
//' @param pre_pulse_rows Integer, number of pre-pulse rows (NOT seconds)
//' @param sampling_interval Double, seconds between consecutive measurements
//'
//' @return List containing MHR results for outer and inner sensors
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calc_mhr_cpp(NumericVector deltaT_do,
                  NumericVector deltaT_di,
                  NumericVector deltaT_uo,
                  NumericVector deltaT_ui,
                  double diffusivity,
                  double probe_spacing,
                  int pre_pulse_rows,
                  double sampling_interval) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max, dTuo_max, dTui_max;
  int idx_do, idx_di, idx_uo, idx_ui;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);
  find_max_and_idx(deltaT_uo, dTuo_max, idx_uo);
  find_max_and_idx(deltaT_ui, dTui_max, idx_ui);

  // Calculate peak times relative to pulse injection (in seconds)
  double time_do = (idx_do - pre_pulse_rows) * sampling_interval;
  double time_di = (idx_di - pre_pulse_rows) * sampling_interval;
  double time_uo = (idx_uo - pre_pulse_rows) * sampling_interval;
  double time_ui = (idx_ui - pre_pulse_rows) * sampling_interval;

  // Check for valid maximums
  if (dTdo_max <= 0 || dTdi_max <= 0 || dTuo_max <= 0 || dTui_max <= 0 ||
      !std::isfinite(dTdo_max) || !std::isfinite(dTdi_max) ||
      !std::isfinite(dTuo_max) || !std::isfinite(dTui_max)) {
    return List::create(
      Named("outer") = NA_REAL,
      Named("inner") = NA_REAL,
      Named("temp_ratio_outer") = NA_REAL,
      Named("temp_ratio_inner") = NA_REAL,
      Named("window_start_outer") = NA_REAL,
      Named("window_end_outer") = NA_REAL,
      Named("window_start_inner") = NA_REAL,
      Named("window_end_inner") = NA_REAL,
      Named("calc_time_outer") = NA_REAL,
      Named("calc_time_inner") = NA_REAL
    );
  }

  // Calculate ratios
  double dTdo_max_dTuo_max = dTdo_max / dTuo_max;
  double dTdi_max_dTui_max = dTdi_max / dTui_max;

  // Calculate velocities
  double Vho_MHR = NA_REAL;
  double Vhi_MHR = NA_REAL;

  if (dTdo_max_dTuo_max > 0) {
    Vho_MHR = (diffusivity / probe_spacing) * std::log(dTdo_max_dTuo_max) * 3600.0;
  }

  if (dTdi_max_dTui_max > 0) {
    Vhi_MHR = (diffusivity / probe_spacing) * std::log(dTdi_max_dTui_max) * 3600.0;
  }

  return List::create(
    Named("outer") = Vho_MHR,
    Named("inner") = Vhi_MHR,
    Named("temp_ratio_outer") = dTdo_max_dTuo_max,
    Named("temp_ratio_inner") = dTdi_max_dTui_max,
    Named("window_start_outer") = time_uo,
    Named("window_end_outer") = time_do,
    Named("window_start_inner") = time_ui,
    Named("window_end_inner") = time_di,
    Named("calc_time_outer") = time_do,
    Named("calc_time_inner") = time_di
  );
}


//' Preprocess Pulse Data - C++ Implementation
//'
//' @description
//' Fast preprocessing of temperature data for a single pulse.
//' Calculates delta temperatures, ratios, and peak information in one pass.
//'
//' @param do_vec Numeric vector of downstream outer temperatures
//' @param di_vec Numeric vector of downstream inner temperatures
//' @param uo_vec Numeric vector of upstream outer temperatures
//' @param ui_vec Numeric vector of upstream inner temperatures
//' @param baseline_values Numeric vector of length 4: pre-pulse baseline
//'   temperatures in order do, di, uo, ui. Computed by
//'   \code{calculate_baseline()} using the configured baseline method.
//' @param pre_pulse_rows Integer, number of pre-pulse rows (NOT seconds).
//'   Used as the time-origin marker for peak-time calculations.
//' @param sampling_interval Double, seconds between consecutive measurements
//'
//' @return List containing delta temps, ratios, and peak info
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List preprocess_pulse_data_cpp(NumericVector do_vec,
                                NumericVector di_vec,
                                NumericVector uo_vec,
                                NumericVector ui_vec,
                                NumericVector baseline_values,
                                int pre_pulse_rows,
                                double sampling_interval) {

  int n = do_vec.size();

  // Baseline values computed in R (via calculate_baseline())
  // baseline_values order: do, di, uo, ui
  double do_mu_pre = baseline_values[0];
  double di_mu_pre = baseline_values[1];
  double uo_mu_pre = baseline_values[2];
  double ui_mu_pre = baseline_values[3];

  // Calculate delta temperatures
  NumericVector deltaT_do(n);
  NumericVector deltaT_di(n);
  NumericVector deltaT_uo(n);
  NumericVector deltaT_ui(n);

  for (int i = 0; i < n; i++) {
    if (i < pre_pulse_rows) {
      deltaT_do[i] = NA_REAL;
      deltaT_di[i] = NA_REAL;
      deltaT_uo[i] = NA_REAL;
      deltaT_ui[i] = NA_REAL;
    } else {
      deltaT_do[i] = do_vec[i] - do_mu_pre;
      deltaT_di[i] = di_vec[i] - di_mu_pre;
      deltaT_uo[i] = uo_vec[i] - uo_mu_pre;
      deltaT_ui[i] = ui_vec[i] - ui_mu_pre;
    }
  }

  // Calculate temperature ratios
  NumericVector dTratio_douo(n);
  NumericVector dTratio_diui(n);

  for (int i = 0; i < n; i++) {
    dTratio_douo[i] = deltaT_do[i] / deltaT_uo[i];
    dTratio_diui[i] = deltaT_di[i] / deltaT_ui[i];
  }

  // Find peaks
  PeakInfo peak_info;
  find_max_and_idx(deltaT_do, peak_info.dTdo_max, peak_info.idx_do);
  find_max_and_idx(deltaT_di, peak_info.dTdi_max, peak_info.idx_di);
  find_max_and_idx(deltaT_uo, peak_info.dTuo_max, peak_info.idx_uo);
  find_max_and_idx(deltaT_ui, peak_info.dTui_max, peak_info.idx_ui);

  // Calculate peak times relative to pulse injection (in seconds)
  peak_info.time_do = (peak_info.idx_do - pre_pulse_rows) * sampling_interval;
  peak_info.time_di = (peak_info.idx_di - pre_pulse_rows) * sampling_interval;
  peak_info.time_uo = (peak_info.idx_uo - pre_pulse_rows) * sampling_interval;
  peak_info.time_ui = (peak_info.idx_ui - pre_pulse_rows) * sampling_interval;

  return List::create(
    Named("deltaT_do") = deltaT_do,
    Named("deltaT_di") = deltaT_di,
    Named("deltaT_uo") = deltaT_uo,
    Named("deltaT_ui") = deltaT_ui,
    Named("dTratio_douo") = dTratio_douo,
    Named("dTratio_diui") = dTratio_diui,
    Named("peak_info") = List::create(
      Named("dTdo_max") = peak_info.dTdo_max,
      Named("dTdi_max") = peak_info.dTdi_max,
      Named("dTuo_max") = peak_info.dTuo_max,
      Named("dTui_max") = peak_info.dTui_max,
      Named("idx_do") = peak_info.idx_do + 1,  // +1 for R's 1-based indexing
      Named("idx_di") = peak_info.idx_di + 1,
      Named("idx_uo") = peak_info.idx_uo + 1,
      Named("idx_ui") = peak_info.idx_ui + 1,
      Named("time_do") = peak_info.time_do,
      Named("time_di") = peak_info.time_di,
      Named("time_uo") = peak_info.time_uo,
      Named("time_ui") = peak_info.time_ui
    )
  );
}


//' Calculate Tmax Cohen - C++ Implementation
//'
//' @param deltaT_do Numeric vector of delta temperatures (downstream outer)
//' @param deltaT_di Numeric vector of delta temperatures (downstream inner)
//' @param diffusivity Thermal diffusivity (cm2/s)
//' @param probe_spacing Probe spacing (cm)
//' @param pre_pulse_rows Number of pre-pulse rows
//' @param sampling_interval Sampling interval in seconds (e.g. 1.0 for 1Hz, 0.5 for 2Hz)
//'
//' @return List containing Tmax_Coh results
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calc_tmax_coh_cpp(NumericVector deltaT_do,
                       NumericVector deltaT_di,
                       double diffusivity,
                       double probe_spacing,
                       int pre_pulse_rows,
                       double sampling_interval) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max;
  int idx_do, idx_di;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);

  // Calculate peak times in seconds relative to pulse injection
  double tmo = (idx_do - pre_pulse_rows) * sampling_interval;
  double tmi = (idx_di - pre_pulse_rows) * sampling_interval;

  // Initialize results
  double Vho_Tmax_Coh = NA_REAL;
  double Vhi_Tmax_Coh = NA_REAL;

  // Check for valid time to maximum
  if (tmo > 0) {
    // Convert to meters for calculation
    double x_m = probe_spacing / 100.0;
    double D_m = diffusivity / 10000.0;  // cm2/s to m2/s

    double discriminant_outer = x_m * x_m - 4.0 * D_m * tmo;

    if (discriminant_outer >= 0) {
      Vho_Tmax_Coh = std::sqrt(discriminant_outer) / tmo * 100.0 * 3600.0;  // Convert m/s to cm/hr
    }
  }

  if (tmi > 0) {
    double x_m = probe_spacing / 100.0;
    double D_m = diffusivity / 10000.0;

    double discriminant_inner = x_m * x_m - 4.0 * D_m * tmi;

    if (discriminant_inner >= 0) {
      Vhi_Tmax_Coh = std::sqrt(discriminant_inner) / tmi * 100.0 * 3600.0;
    }
  }

  return List::create(
    Named("outer") = Vho_Tmax_Coh,
    Named("inner") = Vhi_Tmax_Coh,
    Named("window_start_outer") = NA_REAL,
    Named("window_end_outer") = NA_REAL,
    Named("window_start_inner") = NA_REAL,
    Named("window_end_inner") = NA_REAL,
    Named("calc_time_outer") = tmo,
    Named("calc_time_inner") = tmi
  );
}


//' Calculate Tmax Kluitenberg - C++ Implementation
//'
//' @param deltaT_do Numeric vector of delta temperatures (downstream outer)
//' @param deltaT_di Numeric vector of delta temperatures (downstream inner)
//' @param diffusivity Thermal diffusivity (cm2/s)
//' @param probe_spacing Probe spacing (cm)
//' @param tp_1 Heat pulse duration (seconds)
//' @param pre_pulse_rows Number of pre-pulse rows
//' @param sampling_interval Sampling interval in seconds (e.g. 1.0 for 1Hz, 0.5 for 2Hz)
//'
//' @return List containing Tmax_Klu results
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calc_tmax_klu_cpp(NumericVector deltaT_do,
                       NumericVector deltaT_di,
                       double diffusivity,
                       double probe_spacing,
                       double tp_1,
                       int pre_pulse_rows,
                       double sampling_interval) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max;
  int idx_do, idx_di;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);

  // Calculate peak times in seconds relative to pulse injection
  double tmo = (idx_do - pre_pulse_rows) * sampling_interval;
  double tmi = (idx_di - pre_pulse_rows) * sampling_interval;

  // Initialize results
  double Vho_Tmax_Klu = NA_REAL;
  double Vhi_Tmax_Klu = NA_REAL;

  // Outer sensor calculation
  if (tmo > tp_1) {
    double log_arg_outer = 1.0 - (tp_1 / tmo);

    if (log_arg_outer > 0) {
      double x_m = probe_spacing / 100.0;
      double D_m = diffusivity / 10000.0;

      double discriminant_outer = 4.0 * (D_m / tp_1) * std::log(log_arg_outer) +
        (x_m * x_m) / (tmo * (tmo - tp_1));

      if (discriminant_outer >= 0) {
        Vho_Tmax_Klu = std::sqrt(discriminant_outer) * 100.0 * 3600.0;
      }
    }
  }

  // Inner sensor calculation
  if (tmi > tp_1) {
    double log_arg_inner = 1.0 - (tp_1 / tmi);

    if (log_arg_inner > 0) {
      double x_m = probe_spacing / 100.0;
      double D_m = diffusivity / 10000.0;

      double discriminant_inner = 4.0 * (D_m / tp_1) * std::log(log_arg_inner) +
        (x_m * x_m) / (tmi * (tmi - tp_1));

      if (discriminant_inner >= 0) {
        Vhi_Tmax_Klu = std::sqrt(discriminant_inner) * 100.0 * 3600.0;
      }
    }
  }

  return List::create(
    Named("outer") = Vho_Tmax_Klu,
    Named("inner") = Vhi_Tmax_Klu,
    Named("window_start_outer") = NA_REAL,
    Named("window_end_outer") = NA_REAL,
    Named("window_start_inner") = NA_REAL,
    Named("window_end_inner") = NA_REAL,
    Named("calc_time_outer") = tmo,
    Named("calc_time_inner") = tmi
  );
}
