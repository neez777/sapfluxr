# R/01g_ui_helpers.R
# UI Helper Functions for Interactive Parameter Confirmation and Display

#' Format Parameter Summary for Display
#'
#' @param probe_config ProbeConfiguration object
#' @param wood_properties WoodProperties object
#' @param params List of calculation parameters
#' @param methods Character vector of methods
#' @param n_pulses Number of pulses to process
#' @return Character string with formatted summary
#' @keywords internal
format_parameter_summary <- function(probe_config, wood_properties, params, methods, n_pulses, k_source = NULL) {

  # Build header
  header <- paste0(
    "\n",
    strrep("=", 67), "\n",
    "  HEAT PULSE VELOCITY CALCULATION - PARAMETER SUMMARY\n",
    strrep("=", 67), "\n\n"
  )

  # Probe configuration section
  probe_section <- paste0(
    "\U0001F4CD PROBE CONFIGURATION\n",
    sprintf("  Name:               %s\n", probe_config$config_name),
    sprintf("  Type:               %s\n", tools::toTitleCase(probe_config$config_type)),
    sprintf("  Upstream spacing:   %.1f mm (%.2f cm)\n",
            abs(probe_config$sensor_positions$upstream_outer) * 10,
            abs(probe_config$sensor_positions$upstream_outer)),
    sprintf("  Downstream spacing: %.1f mm (%.2f cm)\n",
            abs(probe_config$sensor_positions$downstream_outer) * 10,
            abs(probe_config$sensor_positions$downstream_outer)),
    sprintf("  Pulse duration:     %s seconds\n", probe_config$heat_pulse_duration),
    sprintf("  Compatible methods: %s\n", paste(probe_config$compatible_methods, collapse = ", ")),
    sprintf("  Source:             %s\n\n",
            ifelse(is.null(probe_config$yaml_source), "Programmatic",
                   basename(probe_config$yaml_source)))
  )

  # Wood properties section
  # Extract values with safe defaults
  species_val <- if (!is.null(wood_properties$wood_property$species)) {
    wood_properties$wood_property$species
  } else {
    "Unknown"
  }

  diffusivity_val <- if (!is.null(wood_properties$wood_constants$thermal_diffusivity_default_cm2_s)) {
    wood_properties$wood_constants$thermal_diffusivity_default_cm2_s
  } else {
    0.0025  # fallback default
  }

  wood_type_val <- if (!is.null(wood_properties$wood_property$wood_type)) {
    tools::toTitleCase(wood_properties$wood_property$wood_type)
  } else {
    "Unknown"
  }

  wood_section <- paste0(
    "\U0001F333 WOOD PROPERTIES\n",
    sprintf("  Configuration:      %s\n", wood_properties$config_name),
    sprintf("  Species:            %s\n", species_val),
    sprintf("  Thermal diffusivity: %.5f cm\U00B2/s\n", diffusivity_val),
    sprintf("  Wood type:          %s\n", wood_type_val)
  )

  # Add optional wood properties (derived properties)
  if (!is.null(wood_properties$derived_properties$mc_kg_kg) &&
      !is.na(wood_properties$derived_properties$mc_kg_kg)) {
    wood_section <- paste0(wood_section,
                           sprintf("  Moisture content:   %.1f%%\n",
                                   wood_properties$derived_properties$mc_kg_kg * 100))
  }
  if (!is.null(wood_properties$tree_measurements$dbh)) {
    wood_section <- paste0(wood_section,
                           sprintf("  DBH:                %.1f cm\n", wood_properties$tree_measurements$dbh))
  }
  if (!is.null(wood_properties$tree_measurements$sapwood_depth)) {
    wood_section <- paste0(wood_section,
                           sprintf("  Sapwood depth:      %.2f cm\n", wood_properties$tree_measurements$sapwood_depth))
  }

  wood_section <- paste0(wood_section,
                         sprintf("  Source:             %s\n\n",
                                 ifelse(is.null(wood_properties$yaml_source), "Programmatic",
                                        basename(wood_properties$yaml_source))))

  # Calculation parameters section
  # Build diffusivity line with source information
  if (!is.null(k_source)) {
    diffusivity_line <- sprintf("  Diffusivity:        %.5f cm\U00B2/s (%s)\n", params$diffusivity, k_source)
  } else {
    diffusivity_line <- sprintf("  Diffusivity:        %.5f cm\U00B2/s (from wood properties)\n", params$diffusivity)
  }

  calc_section <- paste0(
    "\U00002699\U0000FE0F  CALCULATION PARAMETERS\n",
    sprintf("  Methods:            %s\n", paste(methods, collapse = ", ")),
    diffusivity_line,
    sprintf("  Probe spacing:      %.2f cm (from probe config)\n", params$probe_spacing),
    sprintf("  Pre-pulse period:   %s sec\n", params$pre_pulse),
    sprintf("  HRM window:         %s-%s sec\n", params$HRM_start, params$HRM_end),
    sprintf("  Pulse duration:     %s sec (for Tmax_Klu)\n\n", params$tp_1)
  )

  # Processing section
  est_time_min <- max(1, round(n_pulses * length(methods) * 0.001, 1))
  processing_section <- paste0(
    "\U0001F4CA PROCESSING\n",
    sprintf("  Pulses to process:  %s\n", format(n_pulses, big.mark = ",")),
    sprintf("  Estimated time:     ~%.1f minute%s\n\n",
            est_time_min, ifelse(est_time_min == 1, "", "s"))
  )

  # Footer
  footer <- paste0(strrep("=", 67), "\n")

  # Combine all sections
  summary <- paste0(header, probe_section, wood_section, calc_section, processing_section, footer)

  return(summary)
}


#' Prompt User for Parameter Confirmation
#'
#' @param probe_config ProbeConfiguration object
#' @param wood_properties WoodProperties object
#' @param params List of calculation parameters
#' @param methods Character vector of methods
#' @param n_pulses Number of pulses
#' @return Logical indicating whether user confirmed
#' @keywords internal
prompt_parameter_confirmation <- function(probe_config, wood_properties, params, methods, n_pulses, k_source = NULL) {

  # Show summary
  summary <- format_parameter_summary(probe_config, wood_properties, params, methods, n_pulses, k_source)
  cat(summary)

  # Prompt for confirmation
  cat("\nAre these parameters correct? (yes/no): ")
  response <- tolower(trimws(readline()))

  if (response %in% c("y", "yes")) {
    return(TRUE)
  } else {
    # Provide guidance
    cat("\nTo modify these parameters:\n\n")
    cat("1. PROBE CONFIGURATION:\n")
    cat("   - Use different config: probe_config = \"asymmetrical\"\n")
    cat("   - Load custom YAML: probe_config = load_probe_config(\"path/to/config.yaml\")\n")
    cat("   - Create custom: probe_config = create_custom_probe_config(...)\n\n")

    cat("2. WOOD PROPERTIES:\n")
    cat("   - Use different config: wood_properties = \"eucalyptus\"\n")
    cat("   - Load custom YAML: wood_properties = load_wood_properties(\"path/to/wood.yaml\")\n")
    cat("   - Create custom: wood_properties = create_custom_wood_properties(...)\n")
    cat("   - Override specific values: wood_properties = load_wood_properties(\"eucalyptus\",\n")
    cat("                                 overrides = list(thermal_diffusivity = 0.003))\n\n")

    cat("3. CALCULATION PARAMETERS:\n")
    cat("   - Override diffusivity: diffusivity = 0.003\n")
    cat("   - Override probe spacing: probe_spacing = 0.6\n")
    cat("   - Override multiple: parameters = list(HRM_start = 70, HRM_end = 110)\n\n")

    cat("4. METHODS:\n")
    cat("   - Change methods: methods = c(\"HRM\", \"MHR\", \"Tmax_Coh\")\n\n")

    cat("See ?calc_heat_pulse_velocity for full documentation.\n")

    return(FALSE)
  }
}


#' Print Calculation Summary
#'
#' Prints a summary of calculation results including success rates per method
#'
#' @param results Results tibble from calc_heat_pulse_velocity
#' @param n_pulses Total number of pulses processed
#' @keywords internal
print_calculation_summary <- function(results, n_pulses) {

  cat("\n")
  cat(strrep("=", 67), "\n")
  cat("  CALCULATION SUMMARY\n")
  cat(strrep("=", 67), "\n\n")

  cat(sprintf("Total pulses processed: %s\n\n", format(n_pulses, big.mark = ",")))

  # Calculate success rates per method
  methods <- unique(results$method)

  cat("Success rates by method:\n")
  cat(strrep("-", 67), "\n")
  cat(sprintf("%-15s %10s %10s %10s\n", "Method", "Calculated", "Missing", "Success %"))
  cat(strrep("-", 67), "\n")

  for (method in methods) {
    method_results <- results[results$method == method, ]
    total <- nrow(method_results)
    calculated <- sum(!is.na(method_results$Vh_cm_hr) & is.finite(method_results$Vh_cm_hr))
    missing <- total - calculated
    success_pct <- round(100 * calculated / total, 1)

    cat(sprintf("%-15s %10s %10s %9.1f%%\n",
                method,
                format(calculated, big.mark = ","),
                format(missing, big.mark = ","),
                success_pct))
  }

  cat(strrep("-", 67), "\n\n")

  # Quality flag summary
  cat("Quality flags:\n")
  flag_counts <- table(results$quality_flag)
  for (flag in names(flag_counts)) {
    cat(sprintf("  %-20s: %s\n", flag, format(flag_counts[flag], big.mark = ",")))
  }

  cat("\n")
  cat(strrep("=", 67), "\n\n")
}
