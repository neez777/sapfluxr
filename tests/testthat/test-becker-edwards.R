# Tests for the Becker & Edwards (1999) temperature-dependent heat-capacity
# coefficient and the temperature-dependent sap-flux conversion.

test_that("calc_becker_edwards_k matches the published quadratic", {
  sapfluxr::reset_analysis_config()

  # k = 0.400 + 0.00214*T - 0.000006*T^2
  expect_equal(sapfluxr::calc_becker_edwards_k(0), 0.400, tolerance = 1e-9)
  expect_equal(sapfluxr::calc_becker_edwards_k(20), 0.4404, tolerance = 1e-6)
  expect_equal(sapfluxr::calc_becker_edwards_k(50),
               0.400 + 0.00214 * 50 - 0.000006 * 50^2, tolerance = 1e-9)

  # Vectorised and monotone increasing across 0-50 degC
  ks <- sapfluxr::calc_becker_edwards_k(c(0, 25, 50))
  expect_length(ks, 3)
  expect_true(all(diff(ks) > 0))

  expect_error(sapfluxr::calc_becker_edwards_k("warm"), "numeric")
})


# Build a WoodProperties object with derived properties for conversion tests.
make_wood <- function() {
  wp <- sapfluxr::load_wood_properties("eucalyptus")
  wp$wood_measurements$density_dry_kg_m3 <- 550
  wp$wood_measurements$density_fresh_kg_m3 <- 900
  suppressWarnings(utils::capture.output(
    wp <- sapfluxr::calculate_wood_properties(wp)
  ))
  wp
}


test_that("static and dynamic conversion agree when temperature is constant", {
  sapfluxr::reset_analysis_config()
  wp <- make_wood()

  df <- data.frame(
    Vh_cm_hr = c(5, 10, -2, 0),
    prepulse_temp_c = rep(20, 4)
  )

  static <- suppressWarnings(utils::capture.output(
    out_static <- sapfluxr::calc_sap_flux_density(
      wood_properties = wp, vh_data = df, velocity_col = "Vh_cm_hr",
      temperature_mode = "static", temperature = 20
    )
  ))
  dynamic <- suppressWarnings(utils::capture.output(
    out_dynamic <- sapfluxr::calc_sap_flux_density(
      wood_properties = wp, vh_data = df, velocity_col = "Vh_cm_hr",
      temperature_mode = "dynamic"
    )
  ))

  expect_equal(out_static$Jv_cm3_cm2_hr, out_dynamic$Jv_cm3_cm2_hr)

  # And both equal Vh * Z(20)
  Z20 <- sapfluxr::sap_flux_conversion_factor_at_temp(wp, 20)
  expect_equal(out_static$Jv_cm3_cm2_hr, df$Vh_cm_hr * Z20)
})


test_that("constant mode reproduces the fixed Z conversion", {
  sapfluxr::reset_analysis_config()
  wp <- make_wood()

  df <- data.frame(Vh_cm_hr = c(3, 7, 12))
  Z <- wp$derived_properties$sap_flux_conversion_factor

  out <- suppressWarnings(utils::capture.output(
    res <- sapfluxr::calc_sap_flux_density(
      wood_properties = wp, vh_data = df, velocity_col = "Vh_cm_hr",
      temperature_mode = "constant"
    )
  ))
  expect_equal(res$Jv_cm3_cm2_hr, df$Vh_cm_hr * Z)
})


test_that("dynamic mode errors without the prepulse temperature column", {
  sapfluxr::reset_analysis_config()
  wp <- make_wood()
  df <- data.frame(Vh_cm_hr = c(1, 2, 3))
  expect_error(
    suppressWarnings(sapfluxr::calc_sap_flux_density(
      wood_properties = wp, vh_data = df, velocity_col = "Vh_cm_hr",
      temperature_mode = "dynamic"
    )),
    "prepulse_temp_c"
  )
})


test_that("calc_heat_pulse_velocity stores prepulse_temp_c per pulse", {
  hpd <- create_test_sap_data(n_points = 120, n_pulses = 3, add_noise = FALSE)

  res <- suppressWarnings(suppressMessages(
    sapfluxr::calc_heat_pulse_velocity(
      hpd, methods = "HRM",
      confirm_parameters = FALSE, show_progress = FALSE,
      fill_missing_pulses = FALSE
    )
  ))

  expect_true("prepulse_temp_c" %in% names(res))

  # Outer ~ mean(18.8, 18.9) = 18.85; inner ~ mean(18.6, 18.7) = 18.65
  outer_temp <- res$prepulse_temp_c[res$sensor_position == "outer"]
  inner_temp <- res$prepulse_temp_c[res$sensor_position == "inner"]
  expect_equal(mean(outer_temp), 18.85, tolerance = 0.1)
  expect_equal(mean(inner_temp), 18.65, tolerance = 0.1)
})
