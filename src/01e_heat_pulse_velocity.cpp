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
//' @param diffusivity Thermal diffusivity (cm²/s)
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
  double Pe_outer = NA_REAL;
  double Pe_inner = NA_REAL;

  if (hrm_indices.size() == 0) {
    return List::create(
      Named("outer") = Vho_HRM,
      Named("inner") = Vhi_HRM,
      Named("temp_ratio_outer") = dTratio_HRM_douo_mean,
      Named("temp_ratio_inner") = dTratio_HRM_diui_mean,
      Named("peclet_outer") = Pe_outer,
      Named("peclet_inner") = Pe_inner,
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

    // Calculate Peclet number
    if (std::isfinite(Vho_HRM)) {
      Pe_outer = (Vho_HRM * probe_spacing) / (diffusivity * 3600.0);
    }
  }

  if (!NumericVector::is_na(dTratio_HRM_diui_mean) && dTratio_HRM_diui_mean > 0) {
    Vhi_HRM = diffusivity / probe_spacing * std::log(dTratio_HRM_diui_mean) * 3600.0;

    // Calculate Peclet number
    if (std::isfinite(Vhi_HRM)) {
      Pe_inner = (Vhi_HRM * probe_spacing) / (diffusivity * 3600.0);
    }
  }

  return List::create(
    Named("outer") = Vho_HRM,
    Named("inner") = Vhi_HRM,
    Named("temp_ratio_outer") = dTratio_HRM_douo_mean,
    Named("temp_ratio_inner") = dTratio_HRM_diui_mean,
    Named("peclet_outer") = Pe_outer,
    Named("peclet_inner") = Pe_inner,
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
//' @param diffusivity Thermal diffusivity (cm²/s)
//' @param probe_spacing Probe spacing (cm)
//' @param pre_pulse Pre-pulse period (seconds)
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
                  int pre_pulse) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max, dTuo_max, dTui_max;
  int idx_do, idx_di, idx_uo, idx_ui;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);
  find_max_and_idx(deltaT_uo, dTuo_max, idx_uo);
  find_max_and_idx(deltaT_ui, dTui_max, idx_ui);

  // Calculate peak times relative to pulse injection
  double time_do = idx_do - pre_pulse;
  double time_di = idx_di - pre_pulse;
  double time_uo = idx_uo - pre_pulse;
  double time_ui = idx_ui - pre_pulse;

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
//' @param pre_pulse Integer, number of pre-pulse measurements
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
                                int pre_pulse) {

  int n = do_vec.size();

  // Calculate pre-pulse means
  double do_mu_pre = 0.0, di_mu_pre = 0.0, uo_mu_pre = 0.0, ui_mu_pre = 0.0;
  int count = 0;

  int pre_pulse_end = std::min(pre_pulse, n);
  for (int i = 0; i < pre_pulse_end; i++) {
    if (!NumericVector::is_na(do_vec[i])) {
      do_mu_pre += do_vec[i];
    }
    if (!NumericVector::is_na(di_vec[i])) {
      di_mu_pre += di_vec[i];
    }
    if (!NumericVector::is_na(uo_vec[i])) {
      uo_mu_pre += uo_vec[i];
    }
    if (!NumericVector::is_na(ui_vec[i])) {
      ui_mu_pre += ui_vec[i];
    }
    count++;
  }

  if (count > 0) {
    do_mu_pre /= count;
    di_mu_pre /= count;
    uo_mu_pre /= count;
    ui_mu_pre /= count;
  }

  // Calculate delta temperatures
  NumericVector deltaT_do(n);
  NumericVector deltaT_di(n);
  NumericVector deltaT_uo(n);
  NumericVector deltaT_ui(n);

  for (int i = 0; i < n; i++) {
    if (i < pre_pulse) {
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

  // Calculate peak times relative to pulse injection
  peak_info.time_do = peak_info.idx_do - pre_pulse;
  peak_info.time_di = peak_info.idx_di - pre_pulse;
  peak_info.time_uo = peak_info.idx_uo - pre_pulse;
  peak_info.time_ui = peak_info.idx_ui - pre_pulse;

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
//' @param diffusivity Thermal diffusivity (cm²/s)
//' @param probe_spacing Probe spacing (cm)
//' @param pre_pulse Pre-pulse period (seconds)
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
                       int pre_pulse) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max;
  int idx_do, idx_di;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);

  // Calculate peak times relative to pulse injection
  double tmo = idx_do - pre_pulse;
  double tmi = idx_di - pre_pulse;

  // Initialize results
  double Vho_Tmax_Coh = NA_REAL;
  double Vhi_Tmax_Coh = NA_REAL;

  // Check for valid time to maximum
  if (tmo > 0) {
    // Convert to meters for calculation
    double x_m = probe_spacing / 100.0;
    double D_m = diffusivity / 10000.0;  // cm²/s to m²/s

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
//' @param diffusivity Thermal diffusivity (cm²/s)
//' @param probe_spacing Probe spacing (cm)
//' @param tp_1 Heat pulse duration (seconds)
//' @param pre_pulse Pre-pulse period (seconds)
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
                       int pre_pulse) {

  // Find maximum values and their indices
  double dTdo_max, dTdi_max;
  int idx_do, idx_di;

  find_max_and_idx(deltaT_do, dTdo_max, idx_do);
  find_max_and_idx(deltaT_di, dTdi_max, idx_di);

  // Calculate peak times relative to pulse injection
  double tmo = idx_do - pre_pulse;
  double tmi = idx_di - pre_pulse;

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


//' Calculate HRMX (Modified Heat Ratio) - C++ Implementation
//'
//' @param deltaT_do Numeric vector of delta temperatures (downstream outer)
//' @param deltaT_di Numeric vector of delta temperatures (downstream inner)
//' @param deltaT_uo Numeric vector of delta temperatures (upstream outer)
//' @param deltaT_ui Numeric vector of delta temperatures (upstream inner)
//' @param dTratio_douo Numeric vector of temperature ratios (do/uo)
//' @param dTratio_diui Numeric vector of temperature ratios (di/ui)
//' @param tp Numeric vector of time after pulse (seconds)
//' @param L Lower proportion of deltaTmax for sampling window
//' @param H Higher proportion of deltaTmax for sampling window
//' @param diffusivity Thermal diffusivity (cm²/s)
//' @param probe_spacing Probe spacing (cm)
//' @param idx_do_max Index of max for downstream outer (1-based from R)
//' @param idx_di_max Index of max for downstream inner (1-based from R)
//' @param idx_uo_max Index of max for upstream outer (1-based from R)
//' @param idx_ui_max Index of max for upstream inner (1-based from R)
//' @param dTdo_max Maximum value for downstream outer
//' @param dTdi_max Maximum value for downstream inner
//' @param dTuo_max Maximum value for upstream outer
//' @param dTui_max Maximum value for upstream inner
//'
//' @return List containing HRMXa and HRMXb results
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calc_hrmx_cpp(NumericVector deltaT_do,
                   NumericVector deltaT_di,
                   NumericVector deltaT_uo,
                   NumericVector deltaT_ui,
                   NumericVector dTratio_douo,
                   NumericVector dTratio_diui,
                   NumericVector tp,
                   double L,
                   double H,
                   double diffusivity,
                   double probe_spacing,
                   int idx_do_max,
                   int idx_di_max,
                   int idx_uo_max,
                   int idx_ui_max,
                   double dTdo_max,
                   double dTdi_max,
                   double dTuo_max,
                   double dTui_max) {

  int n = deltaT_do.size();

  // Convert R 1-based indices to C++ 0-based
  int idx_do = idx_do_max - 1;
  int idx_di = idx_di_max - 1;
  int idx_uo = idx_uo_max - 1;
  int idx_ui = idx_ui_max - 1;

  // Calculate pre-max values (only on rising limb BEFORE maximum)
  NumericVector dTdo_premax(n, NA_REAL);
  NumericVector dTdi_premax(n, NA_REAL);
  NumericVector dTuo_premax(n, NA_REAL);
  NumericVector dTui_premax(n, NA_REAL);

  // Identify rising limb points before peak
  for (int i = 1; i < n; i++) {
    if (i < idx_do && !NumericVector::is_na(deltaT_do[i]) && !NumericVector::is_na(deltaT_do[i-1])) {
      if (deltaT_do[i] > deltaT_do[i-1]) {
        dTdo_premax[i] = deltaT_do[i];
      }
    }
    if (i < idx_di && !NumericVector::is_na(deltaT_di[i]) && !NumericVector::is_na(deltaT_di[i-1])) {
      if (deltaT_di[i] > deltaT_di[i-1]) {
        dTdi_premax[i] = deltaT_di[i];
      }
    }
    if (i < idx_uo && !NumericVector::is_na(deltaT_uo[i]) && !NumericVector::is_na(deltaT_uo[i-1])) {
      if (deltaT_uo[i] > deltaT_uo[i-1]) {
        dTuo_premax[i] = deltaT_uo[i];
      }
    }
    if (i < idx_ui && !NumericVector::is_na(deltaT_ui[i]) && !NumericVector::is_na(deltaT_ui[i-1])) {
      if (deltaT_ui[i] > deltaT_ui[i-1]) {
        dTui_premax[i] = deltaT_ui[i];
      }
    }
  }

  // Calculate window bounds
  double dTdo_max_L = dTdo_max * L;
  double dTdi_max_L = dTdi_max * L;
  double dTuo_max_L = dTuo_max * L;
  double dTui_max_L = dTui_max * L;

  double dTdo_max_H = dTdo_max * H;
  double dTdi_max_H = dTdi_max * H;
  double dTuo_max_H = dTuo_max * H;
  double dTui_max_H = dTui_max * H;

  // Apply HRMX windows (filter to L-H range)
  NumericVector dTdo_HRMX(n, NA_REAL);
  NumericVector dTdi_HRMX(n, NA_REAL);
  NumericVector dTuo_HRMX(n, NA_REAL);
  NumericVector dTui_HRMX(n, NA_REAL);

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(dTdo_premax[i])) {
      if (dTdo_premax[i] >= dTdo_max_L && dTdo_premax[i] <= dTdo_max_H) {
        dTdo_HRMX[i] = dTdo_premax[i];
      }
    }
    if (!NumericVector::is_na(dTdi_premax[i])) {
      if (dTdi_premax[i] >= dTdi_max_L && dTdi_premax[i] <= dTdi_max_H) {
        dTdi_HRMX[i] = dTdi_premax[i];
      }
    }
    if (!NumericVector::is_na(dTuo_premax[i])) {
      if (dTuo_premax[i] >= dTuo_max_L && dTuo_premax[i] <= dTuo_max_H) {
        dTuo_HRMX[i] = dTuo_premax[i];
      }
    }
    if (!NumericVector::is_na(dTui_premax[i])) {
      if (dTui_premax[i] >= dTui_max_L && dTui_premax[i] <= dTui_max_H) {
        dTui_HRMX[i] = dTui_premax[i];
      }
    }
  }

  // Calculate means
  double dTdo_HRMX_mean = 0, dTdi_HRMX_mean = 0, dTuo_HRMX_mean = 0, dTui_HRMX_mean = 0;
  int count_do = 0, count_di = 0, count_uo = 0, count_ui = 0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(dTdo_HRMX[i])) { dTdo_HRMX_mean += dTdo_HRMX[i]; count_do++; }
    if (!NumericVector::is_na(dTdi_HRMX[i])) { dTdi_HRMX_mean += dTdi_HRMX[i]; count_di++; }
    if (!NumericVector::is_na(dTuo_HRMX[i])) { dTuo_HRMX_mean += dTuo_HRMX[i]; count_uo++; }
    if (!NumericVector::is_na(dTui_HRMX[i])) { dTui_HRMX_mean += dTui_HRMX[i]; count_ui++; }
  }

  if (count_do > 0) dTdo_HRMX_mean /= count_do; else dTdo_HRMX_mean = NA_REAL;
  if (count_di > 0) dTdi_HRMX_mean /= count_di; else dTdi_HRMX_mean = NA_REAL;
  if (count_uo > 0) dTuo_HRMX_mean /= count_uo; else dTuo_HRMX_mean = NA_REAL;
  if (count_ui > 0) dTui_HRMX_mean /= count_ui; else dTui_HRMX_mean = NA_REAL;

  // HRMXa: Calculate ratios within window, select method based on sensor means
  double dTo_ratio_HRMX_mean = NA_REAL;
  double dTi_ratio_HRMX_mean = NA_REAL;

  // For outer sensors
  if (!NumericVector::is_na(dTdo_HRMX_mean) && !NumericVector::is_na(dTuo_HRMX_mean)) {
    double sum_ratio = 0.0;
    int count_ratio = 0;

    if (dTdo_HRMX_mean > dTuo_HRMX_mean) {
      // Use downstream window
      for (int i = 0; i < n; i++) {
        if (!NumericVector::is_na(dTdo_HRMX[i]) && !NumericVector::is_na(dTratio_douo[i])) {
          sum_ratio += dTratio_douo[i];
          count_ratio++;
        }
      }
    } else {
      // Use upstream window
      for (int i = 0; i < n; i++) {
        if (!NumericVector::is_na(dTuo_HRMX[i]) && !NumericVector::is_na(dTratio_douo[i])) {
          sum_ratio += dTratio_douo[i];
          count_ratio++;
        }
      }
    }

    if (count_ratio > 0) {
      dTo_ratio_HRMX_mean = sum_ratio / count_ratio;
    }
  }

  // For inner sensors
  if (!NumericVector::is_na(dTdi_HRMX_mean) && !NumericVector::is_na(dTui_HRMX_mean)) {
    double sum_ratio = 0.0;
    int count_ratio = 0;

    if (dTdi_HRMX_mean > dTui_HRMX_mean) {
      // Use downstream window
      for (int i = 0; i < n; i++) {
        if (!NumericVector::is_na(dTdi_HRMX[i]) && !NumericVector::is_na(dTratio_diui[i])) {
          sum_ratio += dTratio_diui[i];
          count_ratio++;
        }
      }
    } else {
      // Use upstream window
      for (int i = 0; i < n; i++) {
        if (!NumericVector::is_na(dTui_HRMX[i]) && !NumericVector::is_na(dTratio_diui[i])) {
          sum_ratio += dTratio_diui[i];
          count_ratio++;
        }
      }
    }

    if (count_ratio > 0) {
      dTi_ratio_HRMX_mean = sum_ratio / count_ratio;
    }
  }

  // HRMXb: Ratio of means
  double dT_ratio_douo_HRMX_mean = NA_REAL;
  double dT_ratio_diui_HRMX_mean = NA_REAL;

  if (!NumericVector::is_na(dTdo_HRMX_mean) && !NumericVector::is_na(dTuo_HRMX_mean) && dTuo_HRMX_mean != 0) {
    dT_ratio_douo_HRMX_mean = dTdo_HRMX_mean / dTuo_HRMX_mean;
  }
  if (!NumericVector::is_na(dTdi_HRMX_mean) && !NumericVector::is_na(dTui_HRMX_mean) && dTui_HRMX_mean != 0) {
    dT_ratio_diui_HRMX_mean = dTdi_HRMX_mean / dTui_HRMX_mean;
  }

  // Calculate velocities
  double Vho_HRMXa = NA_REAL;
  double Vhi_HRMXa = NA_REAL;
  double Vho_HRMXb = NA_REAL;
  double Vhi_HRMXb = NA_REAL;

  if (!NumericVector::is_na(dTo_ratio_HRMX_mean) && dTo_ratio_HRMX_mean > 0) {
    Vho_HRMXa = (diffusivity / probe_spacing) * std::log(dTo_ratio_HRMX_mean) * 3600.0;
  }
  if (!NumericVector::is_na(dTi_ratio_HRMX_mean) && dTi_ratio_HRMX_mean > 0) {
    Vhi_HRMXa = (diffusivity / probe_spacing) * std::log(dTi_ratio_HRMX_mean) * 3600.0;
  }
  if (!NumericVector::is_na(dT_ratio_douo_HRMX_mean) && dT_ratio_douo_HRMX_mean > 0) {
    Vho_HRMXb = (diffusivity / probe_spacing) * std::log(dT_ratio_douo_HRMX_mean) * 3600.0;
  }
  if (!NumericVector::is_na(dT_ratio_diui_HRMX_mean) && dT_ratio_diui_HRMX_mean > 0) {
    Vhi_HRMXb = (diffusivity / probe_spacing) * std::log(dT_ratio_diui_HRMX_mean) * 3600.0;
  }

  // Find window boundaries for HRMXa (depends on which sensor was used)
  std::vector<int> hrmxa_outer_indices, hrmxa_inner_indices;

  if (!NumericVector::is_na(dTdo_HRMX_mean) && !NumericVector::is_na(dTuo_HRMX_mean)) {
    if (dTdo_HRMX_mean > dTuo_HRMX_mean) {
      for (int i = 0; i < n; i++) if (!NumericVector::is_na(dTdo_HRMX[i])) hrmxa_outer_indices.push_back(i);
    } else {
      for (int i = 0; i < n; i++) if (!NumericVector::is_na(dTuo_HRMX[i])) hrmxa_outer_indices.push_back(i);
    }
  }

  if (!NumericVector::is_na(dTdi_HRMX_mean) && !NumericVector::is_na(dTui_HRMX_mean)) {
    if (dTdi_HRMX_mean > dTui_HRMX_mean) {
      for (int i = 0; i < n; i++) if (!NumericVector::is_na(dTdi_HRMX[i])) hrmxa_inner_indices.push_back(i);
    } else {
      for (int i = 0; i < n; i++) if (!NumericVector::is_na(dTui_HRMX[i])) hrmxa_inner_indices.push_back(i);
    }
  }

  double hrmxa_window_start_outer = NA_REAL, hrmxa_window_end_outer = NA_REAL;
  double hrmxa_window_start_inner = NA_REAL, hrmxa_window_end_inner = NA_REAL;

  if (hrmxa_outer_indices.size() > 0) {
    hrmxa_window_start_outer = tp[*std::min_element(hrmxa_outer_indices.begin(), hrmxa_outer_indices.end())];
    hrmxa_window_end_outer = tp[*std::max_element(hrmxa_outer_indices.begin(), hrmxa_outer_indices.end())];
  }
  if (hrmxa_inner_indices.size() > 0) {
    hrmxa_window_start_inner = tp[*std::min_element(hrmxa_inner_indices.begin(), hrmxa_inner_indices.end())];
    hrmxa_window_end_inner = tp[*std::max_element(hrmxa_inner_indices.begin(), hrmxa_inner_indices.end())];
  }

  // Find window boundaries for HRMXb (uses all valid points from both sensors)
  std::vector<int> hrmxb_outer_indices, hrmxb_inner_indices;
  std::vector<int> do_indices, uo_indices, di_indices, ui_indices;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(dTdo_HRMX[i])) { hrmxb_outer_indices.push_back(i); do_indices.push_back(i); }
    if (!NumericVector::is_na(dTuo_HRMX[i])) { hrmxb_outer_indices.push_back(i); uo_indices.push_back(i); }
    if (!NumericVector::is_na(dTdi_HRMX[i])) { hrmxb_inner_indices.push_back(i); di_indices.push_back(i); }
    if (!NumericVector::is_na(dTui_HRMX[i])) { hrmxb_inner_indices.push_back(i); ui_indices.push_back(i); }
  }

  // Remove duplicates and sort
  std::sort(hrmxb_outer_indices.begin(), hrmxb_outer_indices.end());
  hrmxb_outer_indices.erase(std::unique(hrmxb_outer_indices.begin(), hrmxb_outer_indices.end()), hrmxb_outer_indices.end());
  std::sort(hrmxb_inner_indices.begin(), hrmxb_inner_indices.end());
  hrmxb_inner_indices.erase(std::unique(hrmxb_inner_indices.begin(), hrmxb_inner_indices.end()), hrmxb_inner_indices.end());

  double hrmxb_window_start_outer = NA_REAL, hrmxb_window_end_outer = NA_REAL;
  double hrmxb_window_start_inner = NA_REAL, hrmxb_window_end_inner = NA_REAL;

  if (hrmxb_outer_indices.size() > 0) {
    hrmxb_window_start_outer = tp[hrmxb_outer_indices[0]];
    hrmxb_window_end_outer = tp[hrmxb_outer_indices[hrmxb_outer_indices.size()-1]];
  }
  if (hrmxb_inner_indices.size() > 0) {
    hrmxb_window_start_inner = tp[hrmxb_inner_indices[0]];
    hrmxb_window_end_inner = tp[hrmxb_inner_indices[hrmxb_inner_indices.size()-1]];
  }

  // Separate downstream/upstream windows for HRMXb
  double hrmxb_downstream_start_outer = NA_REAL, hrmxb_downstream_end_outer = NA_REAL;
  double hrmxb_upstream_start_outer = NA_REAL, hrmxb_upstream_end_outer = NA_REAL;
  double hrmxb_downstream_start_inner = NA_REAL, hrmxb_downstream_end_inner = NA_REAL;
  double hrmxb_upstream_start_inner = NA_REAL, hrmxb_upstream_end_inner = NA_REAL;

  if (do_indices.size() > 0) {
    hrmxb_downstream_start_outer = tp[*std::min_element(do_indices.begin(), do_indices.end())];
    hrmxb_downstream_end_outer = tp[*std::max_element(do_indices.begin(), do_indices.end())];
  }
  if (uo_indices.size() > 0) {
    hrmxb_upstream_start_outer = tp[*std::min_element(uo_indices.begin(), uo_indices.end())];
    hrmxb_upstream_end_outer = tp[*std::max_element(uo_indices.begin(), uo_indices.end())];
  }
  if (di_indices.size() > 0) {
    hrmxb_downstream_start_inner = tp[*std::min_element(di_indices.begin(), di_indices.end())];
    hrmxb_downstream_end_inner = tp[*std::max_element(di_indices.begin(), di_indices.end())];
  }
  if (ui_indices.size() > 0) {
    hrmxb_upstream_start_inner = tp[*std::min_element(ui_indices.begin(), ui_indices.end())];
    hrmxb_upstream_end_inner = tp[*std::max_element(ui_indices.begin(), ui_indices.end())];
  }

  // Return both HRMXa and HRMXb results
  return List::create(
    Named("HRMXa") = List::create(
      Named("outer") = Vho_HRMXa,
      Named("inner") = Vhi_HRMXa,
      Named("temp_ratio_outer") = dTo_ratio_HRMX_mean,
      Named("temp_ratio_inner") = dTi_ratio_HRMX_mean,
      Named("window_start_outer") = hrmxa_window_start_outer,
      Named("window_end_outer") = hrmxa_window_end_outer,
      Named("window_start_inner") = hrmxa_window_start_inner,
      Named("window_end_inner") = hrmxa_window_end_inner,
      Named("calc_time_outer") = NA_REAL,
      Named("calc_time_inner") = NA_REAL
    ),
    Named("HRMXb") = List::create(
      Named("outer") = Vho_HRMXb,
      Named("inner") = Vhi_HRMXb,
      Named("temp_ratio_outer") = dT_ratio_douo_HRMX_mean,
      Named("temp_ratio_inner") = dT_ratio_diui_HRMX_mean,
      Named("window_start_outer") = hrmxb_window_start_outer,
      Named("window_end_outer") = hrmxb_window_end_outer,
      Named("window_start_inner") = hrmxb_window_start_inner,
      Named("window_end_inner") = hrmxb_window_end_inner,
      Named("calc_time_outer") = NA_REAL,
      Named("calc_time_inner") = NA_REAL,
      Named("downstream_window_start_outer") = hrmxb_downstream_start_outer,
      Named("downstream_window_end_outer") = hrmxb_downstream_end_outer,
      Named("upstream_window_start_outer") = hrmxb_upstream_start_outer,
      Named("upstream_window_end_outer") = hrmxb_upstream_end_outer,
      Named("downstream_window_start_inner") = hrmxb_downstream_start_inner,
      Named("downstream_window_end_inner") = hrmxb_downstream_end_inner,
      Named("upstream_window_start_inner") = hrmxb_upstream_start_inner,
      Named("upstream_window_end_inner") = hrmxb_upstream_end_inner
    )
  );
}
