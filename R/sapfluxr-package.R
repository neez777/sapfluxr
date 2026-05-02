#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib sapfluxr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom stats approx coef lm median predict residuals sd setNames as.formula quantile fitted complete.cases runif cor ave
#' @importFrom utils packageVersion head read.csv write.csv modifyList
#' @importFrom graphics abline axis box grid legend lines par plot points polygon rect text axis.POSIXct mtext title
#' @importFrom grDevices adjustcolor col2rgb rgb dev.off pdf png hcl
#' @importFrom methods is new
#' @importFrom tools showNonASCIIfile
## usethis namespace: end
NULL

# Silence global variable notes for NSE (dplyr/ggplot2)
utils::globalVariables(c(
  ".data", "datetime", "pulse_id", "sensor_position", "method", "Vh_cm_hr",
  "Vh_cm_hr_raw", "Vh_cm_hr_sc", "Vh_cm_hr_wc", "Vh_calibrated",
  "method_used", "peclet_number", "quality_flag", "is_missing_pulse",
  "old_pulse_id", "new_pulse_id", "Jv_cm3_cm2_hr", "primary_velocity",
  "secondary_velocity", "predicted", "import_time", "file_name", "date",
  "min_value", "segment_id", "Q_cm3_hr", "Qp_daily", "vpd_kpa", "min_predawn_vpd",
  "import_success", "n_pulses", "n_measurements", "file_size_mb", "threshold",
  "r_squared", "Vh_cm_hr_secondary", "Vh_cm_hr_primary", "x", "y", "fitted_quad",
  "pulse_datetime", "zero_vh", "value", "coefficient", "label", "time_bin",
  "series", "Velocity", "Correction_Stage", "hour", "deltaT_do", "deltaT_di",
  "deltaT_uo", "deltaT_ui", "is_outlier", "abs_diff_from_median", "mad_val",
  "rate_of_change", "is_roi_outlier", "diff_mean", "mean_abs_diff", "sd_val",
  "is_cross_sensor_outlier", "is_illogical", "correction_formula", "B_linear",
  "wound_diameter_cm", "B", "is_reinstall_boundary", "time_sec", "deltaT",
  "sensor_label", "velocity", "xmin", "xmax", "Vh", "Type", "difference",
  "t_max_downstream", "t_max_upstream", "status", "time_rel", "temperature",
  "probe", "time_tmax", "Vh_plot", "flag_label", "is_interpolated",
  "sensor", "k_value", "k_nominal", "k_sd"
))
