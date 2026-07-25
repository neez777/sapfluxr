# tests/testthat/test-sdma-methods.R
# Tests for sDMA (selectable dual-method) processing

library(testthat)

# Minimal vh_results with HRM + MHR, both sensors, a pre-computed peclet_number
# (so no probe/wood config is needed) and a per-pulse prepulse_temp_c.
make_sdma_input <- function(n = 20) {
  dt <- as.POSIXct("2024-01-01", tz = "UTC") + 3600 * (seq_len(n) - 1)
  # Per-pulse/per-sensor pre-pulse temperature (same across methods for a pulse)
  temp_outer <- 15 + 5 * sin(seq_len(n))
  temp_inner <- 14 + 4 * sin(seq_len(n))
  # Mix of low and high peclet so switching selects both HRM and the secondary
  peclet_outer <- rep(c(0.5, 1.5), length.out = n)
  peclet_inner <- rep(c(0.4, 1.6), length.out = n)

  one <- function(method, sensor, temp, peclet) {
    data.frame(
      datetime = dt,
      pulse_id = seq_len(n),
      method = method,
      sensor_position = sensor,
      Vh_cm_hr = runif(n, 0, 30),
      Vs_cm_hr = runif(n, 0, 30),
      peclet_number = if (method == "HRM") peclet else NA_real_,
      prepulse_temp_c = temp,
      stringsAsFactors = FALSE
    )
  }

  rbind(
    one("HRM", "outer", temp_outer, peclet_outer),
    one("HRM", "inner", temp_inner, peclet_inner),
    one("MHR", "outer", temp_outer, peclet_outer),
    one("MHR", "inner", temp_inner, peclet_inner)
  )
}

test_that("apply_sdma_processing() carries prepulse_temp_c onto sDMA rows", {
  vh <- make_sdma_input()

  out <- apply_sdma_processing(vh, secondary_method = "MHR", show_progress = FALSE)

  sdma_rows <- out[out$method == "sDMA:MHR", ]
  expect_gt(nrow(sdma_rows), 0)

  # Column present and never NA (the bug produced all-NA here)
  expect_true("prepulse_temp_c" %in% names(sdma_rows))
  expect_false(any(is.na(sdma_rows$prepulse_temp_c)))

  # Row-aligned to the HRM source: same value per (pulse_id, sensor_position)
  hrm_rows <- vh[vh$method == "HRM", ]
  key <- function(d) paste(d$pulse_id, d$sensor_position)
  expected <- hrm_rows$prepulse_temp_c[match(key(sdma_rows), key(hrm_rows))]
  expect_equal(sdma_rows$prepulse_temp_c, expected)
})

test_that("dynamic Becker & Edwards flux is non-NA for sDMA rows", {
  vh <- make_sdma_input()
  out <- apply_sdma_processing(vh, secondary_method = "MHR", show_progress = FALSE)

  wood <- calculate_wood_properties(WoodProperties$new(
    wood_measurements = list(fresh_weight_g = 80, dry_weight_g = 55, fresh_volume_cm3 = 100),
    wood_constants = list(
      rho_sap_kg_m3 = 1000, rho_cell_wall_kg_m3 = 1540, K_sap_W_m_K = 0.6,
      c_sap_J_kg_K = 4186, c_dry_wood_J_kg_K = 1200,
      thermal_diffusivity_default_cm2_s = 0.0025
    )
  ))

  sdma_rows <- out[out$method == "sDMA:MHR", ]
  jv <- calc_sap_flux_density(
    Vh = sdma_rows$Vs_cm_hr,
    wood_properties = wood,
    temperature_mode = "dynamic",
    temperature = sdma_rows$prepulse_temp_c
  )

  expect_equal(length(jv), nrow(sdma_rows))
  expect_false(any(is.na(jv)))
})
