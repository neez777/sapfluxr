# R/01e_missing_pulse_results.R
# Helper for creating placeholder results for missing pulses

#' Create Placeholder Results for Missing Pulses
#'
#' When diagnostics indicate a missing pulse (is_missing_pulse = TRUE),
#' create result rows with NA values and DATA_MISSING flag.
#'
#' @param diagnostics Diagnostics tibble with is_missing_pulse column
#' @param methods Character vector of methods to calculate
#' @return Tibble with placeholder results for missing pulses
#' @keywords internal
create_missing_pulse_results <- function(diagnostics, methods) {

  # Find missing pulses
  missing_pulses <- diagnostics %>%
    dplyr::filter(is_missing_pulse)

  if (nrow(missing_pulses) == 0) {
    return(NULL)
  }

  # Create placeholder rows for each missing pulse × method × sensor
  results_list <- vector("list", nrow(missing_pulses) * length(methods) * 2)
  idx <- 1

  for (i in seq_len(nrow(missing_pulses))) {
    pulse_row <- missing_pulses[i, ]

    for (method in methods) {
      # Outer sensor
      results_list[[idx]] <- tibble::tibble(
        datetime = pulse_row$datetime,
        pulse_id = pulse_row$pulse_id,
        method = method,
        sensor_position = "outer",
        Vh_cm_hr = NA_real_,
        temp_ratio = NA_real_,
        calc_window_start_sec = NA_real_,
        calc_window_end_sec = NA_real_,
        calc_time_sec = NA_real_,
        peclet_number = NA_real_,
        selected_method = NA_character_,
        downstream_window_start_sec = NA_real_,
        downstream_window_end_sec = NA_real_,
        upstream_window_start_sec = NA_real_,
        upstream_window_end_sec = NA_real_,
        quality_flag = "DATA_MISSING"
      )
      idx <- idx + 1

      # Inner sensor
      results_list[[idx]] <- tibble::tibble(
        datetime = pulse_row$datetime,
        pulse_id = pulse_row$pulse_id,
        method = method,
        sensor_position = "inner",
        Vh_cm_hr = NA_real_,
        temp_ratio = NA_real_,
        calc_window_start_sec = NA_real_,
        calc_window_end_sec = NA_real_,
        calc_time_sec = NA_real_,
        peclet_number = NA_real_,
        selected_method = NA_character_,
        downstream_window_start_sec = NA_real_,
        downstream_window_end_sec = NA_real_,
        upstream_window_start_sec = NA_real_,
        upstream_window_end_sec = NA_real_,
        quality_flag = "DATA_MISSING"
      )
      idx <- idx + 1
    }
  }

  dplyr::bind_rows(results_list)
}
