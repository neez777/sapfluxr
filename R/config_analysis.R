# R/config_analysis.R
# Analysis Configuration Loader

#' Load Analysis Configuration
#'
#' Loads analysis computation defaults from the package YAML or a custom
#' override file. The result is cached in
#' \code{options(sapfluxr.analysis_config)} so the YAML is read only once per
#' session.
#'
#' @param custom_path Optional path to a custom YAML file whose values are
#'   merged (via \code{utils::modifyList}) over the package defaults. If the
#'   file cannot be read, a warning is issued and defaults are used.
#'
#' @return The merged configuration list (invisibly). Use
#'   \code{get_analysis_param()} to retrieve individual values.
#'
#' @family config functions
#' @export
load_analysis_config <- function(custom_path = NULL) {
  default_path <- system.file(
    "configurations", "analysis_defaults.yaml",
    package = "sapfluxr"
  )

  if (default_path == "") {
    stop("[sapfluxr config] Default configuration file not found in package installation.")
  }

  config <- tryCatch(
    yaml::read_yaml(default_path),
    error = function(e) {
      stop("[sapfluxr config] Failed to read default configuration: ", e$message)
    }
  )

  # Merge custom overrides if supplied
  if (!is.null(custom_path)) {
    if (!file.exists(custom_path)) {
      warning(
        "[sapfluxr config] Custom configuration not found at '", custom_path,
        "'. Using package defaults."
      )
    } else {
      custom_config <- tryCatch(
        yaml::read_yaml(custom_path),
        error = function(e) {
          warning(
            "[sapfluxr config] Failed to read custom configuration at '",
            custom_path, "': ", e$message, ". Using package defaults."
          )
          NULL
        }
      )
      if (!is.null(custom_config)) {
        config <- utils::modifyList(config, custom_config)
      }
    }
  }

  # Validate structure
  check <- validate_analysis_config(config)
  if (!check$valid) {
    warning(
      "[sapfluxr config] Configuration validation failed:\n",
      paste(" -", check$messages, collapse = "\n")
    )
  }

  # Version guard: warn if YAML major version differs from package major version
  pkg_major <- strsplit(
    as.character(utils::packageVersion("sapfluxr")), "\\."
  )[[1]][1]
  yaml_major <- strsplit(
    as.character(config$metadata$version), "\\."
  )[[1]][1]

  if (!is.null(yaml_major) && pkg_major != yaml_major) {
    warning(
      "[sapfluxr config] Configuration major version (", yaml_major,
      ") differs from package major version (", pkg_major,
      "). Some defaults may be stale."
    )
  }

  options(sapfluxr.analysis_config = config)
  return(invisible(config))
}


#' Get Current Analysis Configuration
#'
#' Returns the cached configuration, auto-loading defaults if not yet
#' initialised.
#'
#' @return Named list of analysis parameters.
#'
#' @keywords internal
#' @family config functions
get_analysis_config <- function() {
  conf <- getOption("sapfluxr.analysis_config")
  if (is.null(conf)) {
    conf <- suppressWarnings(load_analysis_config())
  }
  conf
}


#' Get a Single Analysis Parameter by Dotted Key
#'
#' Retrieves one value from the analysis configuration using a dotted path
#' string such as \code{"quality_calculation.rolling_window_size"}.
#'
#' @param key Character string with dot-separated path to the parameter (e.g.
#'   \code{"hrm.start_seconds"}).
#'
#' @return The parameter value (scalar or list, depending on the key).
#'
#' @family config functions
#' @export
get_analysis_param <- function(key) {
  config <- get_analysis_config()
  parts  <- strsplit(key, ".", fixed = TRUE)[[1]]
  val    <- config

  for (p in parts) {
    if (!is.list(val) || !(p %in% names(val))) {
      stop("[sapfluxr config] Configuration key '", key, "' not found.")
    }
    val <- val[[p]]
  }

  val
}


#' Set a Single Analysis Parameter by Dotted Key
#'
#' Overrides one value in the cached analysis configuration using a dotted
#' path string such as \code{"baseline.method"}. Changes persist for the
#' current R session. Call \code{reset_analysis_config()} to restore defaults.
#'
#' @param key Character string with dot-separated path to the parameter (e.g.
#'   \code{"baseline.method"}).
#' @param value The new value to assign.
#'
#' @return \code{invisible(NULL)}
#'
#' @examples
#' \dontrun{
#' set_analysis_param("baseline.method", "mean_3s")
#' set_analysis_param("baseline.method", "mean_30s")  # restore default
#' }
#'
#' @family config functions
#' @export
set_analysis_param <- function(key, value) {
  set_nested <- function(cfg, keys, val) {
    if (length(keys) == 1L) {
      cfg[[keys]] <- val
      return(cfg)
    }
    if (!is.list(cfg[[keys[1]]])) cfg[[keys[1]]] <- list()
    cfg[[keys[1]]] <- set_nested(cfg[[keys[1]]], keys[-1], val)
    cfg
  }

  config <- get_analysis_config()
  parts  <- strsplit(key, ".", fixed = TRUE)[[1]]
  config <- set_nested(config, parts, value)

  check <- validate_analysis_config(config)
  if (!check$valid) {
    warning(
      "[sapfluxr config] Validation failed after set_analysis_param('", key, "'):\n",
      paste(" -", check$messages, collapse = "\n")
    )
  }

  options(sapfluxr.analysis_config = config)
  invisible(NULL)
}


#' Reset Cached Analysis Configuration
#'
#' Clears \code{options(sapfluxr.analysis_config)} so the next call to any
#' config-reading function reloads from YAML. Useful in tests and after
#' modifying a custom YAML file in-session.
#'
#' @return \code{invisible(NULL)}
#'
#' @family config functions
#' @export
reset_analysis_config <- function() {
  options(sapfluxr.analysis_config = NULL)
  invisible(NULL)
}


#' Validate an Analysis Configuration List
#'
#' Checks the structure and key constraint of a configuration list.  Does not
#' stop on failure  - callers decide what to do with the result.
#'
#' @param config Named list as returned by \code{load_analysis_config()}.
#'
#' @return A list with two components:
#'   \code{valid} (logical) and \code{messages} (character vector of issues).
#'
#' @keywords internal
#' @family config functions
validate_analysis_config <- function(config) {
  msgs  <- character(0)
  valid <- TRUE

  # Required top-level sections
  required <- c("baseline", "hrm", "quality_calculation", "burgess")
  missing  <- setdiff(required, names(config))
  if (length(missing) > 0) {
    valid <- FALSE
    msgs  <- c(msgs, paste("Missing required sections:", paste(missing, collapse = ", ")))
  }

  # baseline$method
  if (!is.null(config$baseline$method)) {
    allowed <- c("mean_30s", "mean_3s", "slope_intercept")
    if (!(config$baseline$method %in% allowed)) {
      valid <- FALSE
      msgs  <- c(msgs, paste0(
        "baseline.method must be one of: ",
        paste(allowed, collapse = ", "),
        ". Got: '", config$baseline$method, "'."
      ))
    }
  }

  # hrm window ordering
  if (!is.null(config$hrm$start_seconds) && !is.null(config$hrm$end_seconds)) {
    if (config$hrm$start_seconds >= config$hrm$end_seconds) {
      valid <- FALSE
      msgs  <- c(msgs, "hrm.start_seconds must be less than hrm.end_seconds.")
    }
  }

  # burgess threshold positive
  if (!is.null(config$burgess$fallback_threshold_cm_hr)) {
    if (config$burgess$fallback_threshold_cm_hr <= 0) {
      valid <- FALSE
      msgs  <- c(msgs, "burgess.fallback_threshold_cm_hr must be greater than 0.")
    }
  }

  list(valid = valid, messages = msgs)
}
