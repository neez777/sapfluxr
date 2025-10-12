#' YAML-Based Heat Pulse Velocity Method System
#'
#' This module provides functionality to dynamically load and execute heat pulse
#' velocity calculation methods defined in YAML files. This allows researchers
#' to develop new methods without writing R code.
#'
#' @name yaml_method_parser
NULL

#' Load Method Configuration from YAML
#'
#' Loads a heat pulse velocity method configuration from a YAML file.
#'
#' @param method_name Character string specifying the method name
#' @param method_file Character string specifying path to YAML file (optional)
#' @param methods_dir Character string specifying directory containing method YAML files
#'
#' @return A list containing the method configuration
#'
#' @details
#' If method_file is not provided, the function will look for a file named
#' "{method_name}_method.yaml" in the methods_dir directory.
#'
#' @export
load_method_config <- function(method_name, method_file = NULL, methods_dir = NULL) {

  if (is.null(methods_dir)) {
    methods_dir <- system.file("methods", package = "sapFluxR")
  }

  if (is.null(method_file)) {
    method_file <- file.path(methods_dir, paste0(method_name, "_method.yaml"))
  }

  if (!file.exists(method_file)) {
    stop("Method file not found: ", method_file)
  }

  # Load YAML configuration
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package is required but not installed. Please install with: install.packages('yaml')")
  }

  config <- yaml::read_yaml(method_file)

  # Validate configuration structure
  validate_method_config(config, method_name)

  return(config)
}

#' List Available Methods
#'
#' Lists all available heat pulse velocity methods from YAML files.
#'
#' @param methods_dir Character string specifying directory containing method YAML files
#'
#' @return A data frame with method information
#'
#' @export
list_available_methods <- function(methods_dir = NULL) {

  if (is.null(methods_dir)) {
    methods_dir <- system.file("methods", package = "sapFluxR")
  }

  if (!dir.exists(methods_dir)) {
    warning("Methods directory not found: ", methods_dir)
    return(data.frame())
  }

  # Find all YAML method files
  yaml_files <- list.files(methods_dir, pattern = "_method\\.yaml$", full.names = TRUE)

  if (length(yaml_files) == 0) {
    return(data.frame())
  }

  # Load method information
  method_info <- data.frame()

  for (file in yaml_files) {
    tryCatch({
      config <- yaml::read_yaml(file)

      method_info <- rbind(method_info, data.frame(
        name = config$method$name %||% NA_character_,
        full_name = config$method$full_name %||% NA_character_,
        description = config$method$description %||% NA_character_,
        category = config$method$category %||% NA_character_,
        reference = config$method$reference %||% NA_character_,
        file = basename(file),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      warning("Error loading method file ", file, ": ", e$message)
    })
  }

  return(method_info)
}

#' Execute YAML-Based Method
#'
#' Executes a heat pulse velocity calculation method defined in YAML.
#'
#' @param method_name Character string specifying the method name
#' @param inputs List containing input variables required by the method
#' @param parameters List containing method parameters (optional)
#' @param config Method configuration list (optional, will be loaded if not provided)
#'
#' @return A list containing the calculated velocities and any intermediate results
#'
#' @export
execute_yaml_method <- function(method_name, inputs, parameters = NULL, config = NULL) {

  # Load configuration if not provided
  if (is.null(config)) {
    config <- load_method_config(method_name)
  }

  # Merge parameters with defaults
  if (!is.null(parameters)) {
    method_params <- config$parameters %||% list()
    param_names <- sapply(method_params, function(p) p$name)
    param_defaults <- sapply(method_params, function(p) p$default %||% NA)
    names(param_defaults) <- param_names

    # Merge user parameters with defaults
    final_params <- modifyList(as.list(param_defaults), parameters)
  } else {
    final_params <- list()
  }

  # Add parameters to inputs
  inputs <- c(inputs, final_params)

  # Validate inputs
  validate_method_inputs(inputs, config)

  # Create execution environment
  env <- list2env(inputs, parent = emptyenv())

  # Execute calculation steps
  results <- execute_calculation_steps(config$calculation$steps, env)

  # Perform quality assessment
  quality_results <- assess_method_quality(results, config)

  # Return results
  return(list(
    method = method_name,
    results = results,
    quality = quality_results,
    parameters_used = final_params
  ))
}

#' Validate Method Configuration
#'
#' Validates that a method configuration has the required structure.
#'
#' @param config List containing method configuration
#' @param method_name Character string for error messages
#'
#' @keywords internal
validate_method_config <- function(config, method_name) {

  required_sections <- c("method", "inputs", "outputs", "calculation")
  missing_sections <- setdiff(required_sections, names(config))

  if (length(missing_sections) > 0) {
    stop("Method configuration for '", method_name, "' missing required sections: ",
         paste(missing_sections, collapse = ", "))
  }

  # Validate method section
  required_method_fields <- c("name", "description")
  missing_method_fields <- setdiff(required_method_fields, names(config$method))

  if (length(missing_method_fields) > 0) {
    stop("Method '", method_name, "' missing required fields: ",
         paste(missing_method_fields, collapse = ", "))
  }

  # Validate calculation section
  if (!"steps" %in% names(config$calculation)) {
    stop("Method '", method_name, "' calculation section must contain 'steps'")
  }

  if (!is.list(config$calculation$steps) || length(config$calculation$steps) == 0) {
    stop("Method '", method_name, "' must have at least one calculation step")
  }
}

#' Validate Method Inputs
#'
#' Validates that all required inputs are provided and have correct types.
#'
#' @param inputs List containing input variables
#' @param config Method configuration list
#'
#' @keywords internal
validate_method_inputs <- function(inputs, config) {

  required_inputs <- config$inputs$required %||% list()

  for (input_def in required_inputs) {
    input_name <- input_def$name
    input_type <- input_def$type %||% "any"

    if (!input_name %in% names(inputs)) {
      stop("Required input '", input_name, "' not provided")
    }

    # Basic type validation
    input_value <- inputs[[input_name]]

    if (input_type == "numeric_vector" && (!is.numeric(input_value) || length(input_value) == 0)) {
      stop("Input '", input_name, "' must be a numeric vector")
    }

    if (input_type == "numeric_scalar" && (!is.numeric(input_value) || length(input_value) != 1)) {
      stop("Input '", input_name, "' must be a numeric scalar")
    }

    if (input_type == "logical_vector" && (!is.logical(input_value) || length(input_value) == 0)) {
      stop("Input '", input_name, "' must be a logical vector")
    }
  }
}

#' Execute Calculation Steps
#'
#' Executes the calculation steps defined in the method configuration.
#'
#' @param steps List of calculation steps
#' @param env Environment containing input variables
#'
#' @return List containing calculation results
#'
#' @keywords internal
execute_calculation_steps <- function(steps, env) {

  results <- list()

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    step_name <- step$name %||% paste0("step_", i)
    step_expr <- step$expression %||% ""

    if (nchar(step_expr) == 0) {
      next
    }

    tryCatch({
      # Execute the step expression
      eval(parse(text = step_expr), envir = env)

      # Extract any new variables created in this step
      step_vars <- ls(env)
      for (var_name in step_vars) {
        if (!var_name %in% names(results)) {
          results[[var_name]] <- get(var_name, envir = env)
        }
      }

    }, error = function(e) {
      stop("Error in calculation step '", step_name, "': ", e$message)
    })
  }

  return(results)
}

#' Assess Method Quality
#'
#' Performs quality assessment based on the method's quality criteria.
#'
#' @param results List containing calculation results
#' @param config Method configuration list
#'
#' @return List containing quality assessment results
#'
#' @keywords internal
assess_method_quality <- function(results, config) {

  quality_criteria <- config$quality_criteria %||% list()
  quality_results <- list()

  for (criterion in quality_criteria) {
    criterion_name <- criterion$name
    criterion_check <- criterion$check
    criterion_severity <- criterion$severity %||% "warning"

    if (is.null(criterion_check) || nchar(criterion_check) == 0) {
      next
    }

    tryCatch({
      # Create evaluation environment with results
      eval_env <- list2env(results, parent = emptyenv())

      # Evaluate the quality check
      check_result <- eval(parse(text = criterion_check), envir = eval_env)

      quality_results[[criterion_name]] <- list(
        passed = isTRUE(check_result),
        severity = criterion_severity,
        description = criterion$description %||% ""
      )

    }, error = function(e) {
      quality_results[[criterion_name]] <- list(
        passed = FALSE,
        severity = criterion_severity,
        description = criterion$description %||% "",
        error = e$message
      )
    })
  }

  return(quality_results)
}

#' Calculate Heat Pulse Velocity with YAML Methods
#'
#' Main function to calculate heat pulse velocity using YAML-defined methods.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param methods Character vector of method names to use
#' @param parameters List of calculation parameters
#' @param yaml_methods_dir Directory containing YAML method files
#'
#' @return A tibble containing calculated heat pulse velocities
#'
#' @export
calc_heat_pulse_velocity_yaml <- function(sap_data,
                                         methods,
                                         parameters = NULL,
                                         yaml_methods_dir = NULL) {

  if (!inherits(sap_data, "sap_data")) {
    stop("Input must be a sap_data object from read_sap_data()")
  }

  # Set default parameters
  default_params <- list(
    diffusivity = 0.0025,
    x = 0.5,
    L = 0.5,
    H = 0.8,
    tp_1 = 2,
    HRM_start = 60,
    HRM_end = 100,
    pre_pulse = 30
  )

  # Merge user parameters with defaults
  if (!is.null(parameters)) {
    params <- modifyList(default_params, parameters)
  } else {
    params <- default_params
  }

  # Get pulse IDs to process
  measurements <- sap_data$measurements
  pulse_ids <- unique(measurements$pulse_id)

  # Process each pulse
  all_results <- list()

  for (i in seq_along(pulse_ids)) {
    pid <- pulse_ids[i]

    tryCatch({
      pulse_result <- calc_vh_single_pulse_yaml(pid, measurements, params, methods, yaml_methods_dir)
      all_results[[i]] <- pulse_result
    }, error = function(e) {
      message("Error processing pulse ", pid, ": ", e$message)
    })
  }

  if (length(all_results) == 0) {
    stop("No pulses were successfully processed")
  }

  # Combine results
  combined_results <- dplyr::bind_rows(all_results)

  # Add quality flags
  combined_results <- add_quality_flags(combined_results)

  # Add vh_results class
  class(combined_results) <- c("vh_results", class(combined_results))

  return(combined_results)
}

#' Calculate velocity for single pulse using YAML methods
#'
#' @param pulse_id ID of pulse to process
#' @param measurements Data frame with measurement data
#' @param parameters List of parameters
#' @param methods Character vector of method names
#' @param yaml_methods_dir Directory containing YAML method files
#' @return Data frame with results for this pulse
#' @keywords internal
calc_vh_single_pulse_yaml <- function(pulse_id, measurements, parameters, methods, yaml_methods_dir = NULL) {

  # Extract data for this pulse
  pulse_data <- measurements[measurements$pulse_id == pulse_id, ]

  if (nrow(pulse_data) == 0) {
    stop("No data found for pulse ID: ", pulse_id)
  }

  # Check required temperature columns
  temp_cols <- c("do", "di", "uo", "ui")
  missing_cols <- setdiff(temp_cols, names(pulse_data))
  if (length(missing_cols) > 0) {
    stop("Missing temperature columns: ", paste(missing_cols, collapse = ", "))
  }

  # Extract parameters
  diffusivity <- parameters$diffusivity
  x <- parameters$x
  L <- parameters$L
  H <- parameters$H
  tp_1 <- parameters$tp_1
  HRM_start <- parameters$HRM_start
  HRM_end <- parameters$HRM_end
  pre_pulse <- parameters$pre_pulse

  # Calculate time after heat pulse
  pre_pulse_period <- 1:min(pre_pulse, nrow(pulse_data))
  tp <- pmax(1:nrow(pulse_data) - pre_pulse, 0)
  HRM_period <- tp >= HRM_start & tp < HRM_end

  # Calculate pre-pulse mean temperatures
  do_mu_pre <- mean(pulse_data$do[pre_pulse_period], na.rm = TRUE)
  di_mu_pre <- mean(pulse_data$di[pre_pulse_period], na.rm = TRUE)
  uo_mu_pre <- mean(pulse_data$uo[pre_pulse_period], na.rm = TRUE)
  ui_mu_pre <- mean(pulse_data$ui[pre_pulse_period], na.rm = TRUE)

  # Calculate delta temperatures
  delatT_do <- pulse_data$do - do_mu_pre
  delatT_do[pre_pulse_period] <- NA
  delatT_di <- pulse_data$di - di_mu_pre
  delatT_di[pre_pulse_period] <- NA
  delatT_uo <- pulse_data$uo - uo_mu_pre
  delatT_uo[pre_pulse_period] <- NA
  delatT_ui <- pulse_data$ui - ui_mu_pre
  delatT_ui[pre_pulse_period] <- NA

  # Calculate temperature ratios
  dTratio_douo <- delatT_do / delatT_uo
  dTratio_diui <- delatT_di / delatT_ui

  # Initialize results
  method_results <- list()

  # Process each method using YAML configuration
  for (method_name in methods) {
    tryCatch({
      # Prepare inputs for this method
      method_inputs <- list(
        delatT_do = delatT_do,
        delatT_di = delatT_di,
        delatT_uo = delatT_uo,
        delatT_ui = delatT_ui,
        dTratio_douo = dTratio_douo,
        dTratio_diui = dTratio_diui,
        HRM_period = HRM_period,
        pre_pulse_period = pre_pulse_period,
        tp = tp
      )

      # Execute YAML method
      yaml_result <- execute_yaml_method(method_name, method_inputs, parameters,
                                        config = NULL, yaml_methods_dir = yaml_methods_dir)

      # Extract velocity results
      if ("Vh_outer" %in% names(yaml_result$results) && "Vh_inner" %in% names(yaml_result$results)) {
        method_results[[method_name]] <- list(
          outer = yaml_result$results$Vh_outer,
          inner = yaml_result$results$Vh_inner
        )
      } else {
        warning("Method '", method_name, "' did not return expected velocity outputs")
      }

    }, error = function(e) {
      warning("Error executing YAML method '", method_name, "': ", e$message)
    })
  }

  # Create output data frame
  datetime_pulse <- pulse_data$datetime[1]
  result_rows <- list()

  for (method_name in names(method_results)) {
    method_result <- method_results[[method_name]]
    result_rows[[paste0(method_name, "_outer")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "outer",
      Vh_cm_hr = method_result$outer,
      stringsAsFactors = FALSE
    )
    result_rows[[paste0(method_name, "_inner")]] <- data.frame(
      datetime = datetime_pulse,
      pulse_id = pulse_id,
      method = method_name,
      sensor_position = "inner",
      Vh_cm_hr = method_result$inner,
      stringsAsFactors = FALSE
    )
  }

  if (length(result_rows) == 0) {
    return(data.frame())
  }

  result_df <- dplyr::bind_rows(result_rows)
  return(result_df)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x