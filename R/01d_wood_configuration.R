# R/02b_wood_configuration.R
# Wood Properties Configuration System
# Manages wood thermal and physical properties for sap flow calculations

#' Wood Properties R6 Class
#'
#' R6 class to store and manage wood thermal and physical properties.
#' This includes thermal diffusivity, density, moisture content, tree measurements,
#' and quality thresholds.
#'
#' @field config_name Name of the configuration
#' @field thermal_diffusivity Thermal diffusivity of sapwood (cm²/s)
#' @field thermal_conductivity Thermal conductivity (W/(m·K))
#' @field volumetric_heat_capacity Volumetric heat capacity (J/(m³·K))
#' @field dry_density Oven-dry wood density (kg/m³)
#' @field basic_density Basic density - oven-dry mass / green volume (kg/m³)
#' @field moisture_content Wood moisture content (%)
#' @field species Tree species identification
#' @field wood_type Wood classification ("softwood"/"hardwood")
#' @field temperature Typical wood temperature (°C)
#' @field tree_measurements List of tree-specific measurements (dbh, sapwood_depth, etc.)
#' @field quality_thresholds List of quality control thresholds
#' @field yaml_source Path to source YAML file (if loaded from file)
#' @field yaml_data Raw YAML data (if loaded from file)
#'
#' @export
WoodProperties <- R6::R6Class(
  "WoodProperties",

  public = list(
    config_name = NULL,
    thermal_diffusivity = NULL,
    thermal_conductivity = NULL,
    volumetric_heat_capacity = NULL,
    dry_density = NULL,
    basic_density = NULL,
    moisture_content = NULL,
    species = NULL,
    wood_type = NULL,
    temperature = NULL,
    tree_measurements = NULL,
    quality_thresholds = NULL,
    yaml_source = NULL,
    yaml_data = NULL,

    #' @description
    #' Initialize a new WoodProperties object
    #' @param config_name Name of the configuration
    #' @param thermal_diffusivity Thermal diffusivity (cm²/s)
    #' @param thermal_conductivity Thermal conductivity (W/(m·K))
    #' @param volumetric_heat_capacity Volumetric heat capacity (J/(m³·K))
    #' @param dry_density Oven-dry wood density (kg/m³)
    #' @param basic_density Basic density (kg/m³)
    #' @param moisture_content Moisture content (%)
    #' @param species Species name
    #' @param wood_type Wood type
    #' @param temperature Typical temperature (°C)
    #' @param tree_measurements List of tree measurements
    #' @param quality_thresholds List of quality thresholds
    #' @param yaml_source Path to source YAML file
    #' @param yaml_data Raw YAML data
    initialize = function(config_name = "Custom Wood Properties",
                         thermal_diffusivity = 0.0025,
                         thermal_conductivity = NULL,
                         volumetric_heat_capacity = NULL,
                         dry_density = NULL,
                         basic_density = NULL,
                         moisture_content = NULL,
                         species = "unknown",
                         wood_type = "softwood",
                         temperature = 20,
                         tree_measurements = NULL,
                         quality_thresholds = NULL,
                         yaml_source = NULL,
                         yaml_data = NULL) {

      self$config_name <- config_name
      self$thermal_diffusivity <- thermal_diffusivity
      self$thermal_conductivity <- thermal_conductivity
      self$volumetric_heat_capacity <- volumetric_heat_capacity
      self$dry_density <- dry_density
      self$basic_density <- basic_density
      self$moisture_content <- moisture_content
      self$species <- species
      self$wood_type <- wood_type
      self$temperature <- temperature

      # Initialize tree measurements if not provided
      if (is.null(tree_measurements)) {
        self$tree_measurements <- list(
          dbh = NULL,
          bark_thickness = NULL,
          sapwood_depth = NULL,
          sapwood_area = NULL,
          heartwood_radius = NULL
        )
      } else {
        self$tree_measurements <- tree_measurements
      }

      # Initialize quality thresholds if not provided
      if (is.null(quality_thresholds)) {
        self$quality_thresholds <- list(
          max_velocity_cm_hr = 200,
          min_velocity_cm_hr = -50,
          temperature_range = c(-10, 60)
        )
      } else {
        self$quality_thresholds <- quality_thresholds
      }

      self$yaml_source <- yaml_source
      self$yaml_data <- yaml_data

      # Validate
      self$validate()
    },

    #' @description
    #' Validate wood properties
    validate = function() {
      # Check thermal diffusivity
      if (is.null(self$thermal_diffusivity)) {
        stop("thermal_diffusivity is required")
      }

      if (!is.numeric(self$thermal_diffusivity)) {
        stop("thermal_diffusivity must be numeric")
      }

      if (self$thermal_diffusivity <= 0 || self$thermal_diffusivity > 0.01) {
        warning("thermal_diffusivity is outside typical range (0.001-0.005 cm²/s): ",
                self$thermal_diffusivity)
      }

      # Check wood type
      if (!is.null(self$wood_type)) {
        if (!self$wood_type %in% c("softwood", "hardwood", "unknown")) {
          warning("wood_type should be 'softwood', 'hardwood', or 'unknown'. Got: ",
                  self$wood_type)
        }
      }

      return(TRUE)
    },

    #' @description
    #' Print wood properties summary
    print = function() {
      cat("Wood Properties Configuration\n")
      cat("==============================\n")
      cat("Name:", self$config_name, "\n")
      cat("Species:", self$species, "\n")
      cat("Type:", self$wood_type, "\n\n")

      cat("Thermal Properties:\n")
      cat("  Thermal diffusivity:", self$thermal_diffusivity, "cm²/s\n")
      if (!is.null(self$thermal_conductivity)) {
        cat("  Thermal conductivity:", self$thermal_conductivity, "W/(m·K)\n")
      }
      if (!is.null(self$volumetric_heat_capacity)) {
        cat("  Volumetric heat capacity:", self$volumetric_heat_capacity, "J/(m³·K)\n")
      }

      cat("\nPhysical Properties:\n")
      if (!is.null(self$dry_density)) {
        cat("  Dry density:", self$dry_density, "kg/m³\n")
      }
      if (!is.null(self$basic_density)) {
        cat("  Basic density:", self$basic_density, "kg/m³\n")
      }
      if (!is.null(self$moisture_content)) {
        cat("  Moisture content:", self$moisture_content, "%\n")
      }

      cat("\nTree Measurements:\n")
      has_measurements <- FALSE
      if (!is.null(self$tree_measurements$dbh)) {
        cat("  DBH:", self$tree_measurements$dbh, "cm\n")
        has_measurements <- TRUE
      }
      if (!is.null(self$tree_measurements$sapwood_depth)) {
        cat("  Sapwood depth:", self$tree_measurements$sapwood_depth, "cm\n")
        has_measurements <- TRUE
      }
      if (!is.null(self$tree_measurements$sapwood_area)) {
        cat("  Sapwood area:", self$tree_measurements$sapwood_area, "cm²\n")
        has_measurements <- TRUE
      }
      if (!has_measurements) {
        cat("  (None specified)\n")
      }

      cat("\nQuality Thresholds:\n")
      cat("  Max velocity:", self$quality_thresholds$max_velocity_cm_hr, "cm/hr\n")
      cat("  Min velocity:", self$quality_thresholds$min_velocity_cm_hr, "cm/hr\n")
      cat("  Temperature range:",
          paste(self$quality_thresholds$temperature_range, collapse = " to "), "°C\n")

      if (!is.null(self$yaml_source)) {
        cat("\nSource:", basename(self$yaml_source), "\n")
      }

      invisible(self)
    }
  )
)


#' Load Wood Properties from YAML Configuration
#'
#' Loads wood properties from a YAML configuration file and returns a
#' WoodProperties object. Can load built-in configurations by name or
#' custom YAML files by path.
#'
#' @param config_name Character string specifying configuration name
#'   ("generic_sw", "eucalyptus", "pine") or path to custom YAML file.
#'   If NULL, uses default ("generic_sw").
#' @param overrides Named list of wood property parameters to override from YAML.
#'   For example: list(thermal_diffusivity = 0.003, moisture_content = 35)
#' @param tree_overrides Named list of tree measurement parameters to override.
#'   For example: list(dbh = 45.2, sapwood_depth = 3.2)
#'
#' @return WoodProperties R6 object
#'
#' @examples
#' \dontrun{
#' # Load default generic softwood properties
#' wood <- load_wood_properties()
#'
#' # Load eucalyptus properties
#' wood <- load_wood_properties("eucalyptus")
#'
#' # Load with overrides
#' wood <- load_wood_properties("eucalyptus",
#'                              overrides = list(thermal_diffusivity = 0.0015),
#'                              tree_overrides = list(dbh = 45.2))
#'
#' # Load custom YAML file
#' wood <- load_wood_properties("path/to/my_tree.yaml")
#' }
#'
#' @family wood property functions
#' @export
load_wood_properties <- function(config_name = NULL,
                                  overrides = NULL,
                                  tree_overrides = NULL) {

  # Use default if not specified
  if (is.null(config_name)) {
    config_name <- "generic_sw"
  }

  # Determine YAML file path
  if (file.exists(config_name)) {
    # User provided full path to custom YAML
    yaml_file <- config_name
  } else {
    # Look in package configurations
    yaml_file <- system.file(
      "configurations",
      paste0("wood_", config_name, ".yaml"),
      package = "sapfluxr"
    )

    if (!file.exists(yaml_file) || yaml_file == "") {
      available <- list_available_wood_properties()
      stop("Wood properties configuration '", config_name, "' not found.\n",
           "Available configurations: ",
           paste(available$name, collapse = ", "), "\n",
           "Use list_available_wood_properties() to see details.")
    }
  }

  # Parse YAML
  config_data <- tryCatch(
    yaml::read_yaml(yaml_file),
    error = function(e) {
      stop("Failed to parse YAML file: ", yaml_file, "\n",
           "Error: ", e$message)
    }
  )

  # Extract wood properties
  wood_props <- config_data$wood_property
  if (is.null(wood_props)) {
    stop("YAML file must contain 'wood_property' section: ", yaml_file)
  }

  # Apply wood property overrides
  if (!is.null(overrides)) {
    wood_props <- modifyList(wood_props, overrides)
  }

  # Extract tree measurements
  tree_meas <- config_data$tree_measurements
  if (is.null(tree_meas)) {
    tree_meas <- list()
  }

  # Apply tree measurement overrides
  if (!is.null(tree_overrides)) {
    tree_meas <- modifyList(tree_meas, tree_overrides)
  }

  # Extract quality thresholds
  quality <- config_data$quality_thresholds
  if (is.null(quality)) {
    quality <- list(
      max_velocity_cm_hr = 200,
      min_velocity_cm_hr = -50,
      temperature_range = c(-10, 60)
    )
  }

  # Create WoodProperties object
  wood_config <- WoodProperties$new(
    config_name = config_data$metadata$config_name,
    thermal_diffusivity = wood_props$thermal_diffusivity,
    thermal_conductivity = wood_props$thermal_conductivity,
    volumetric_heat_capacity = wood_props$volumetric_heat_capacity,
    dry_density = wood_props$dry_density,
    basic_density = wood_props$basic_density,
    moisture_content = wood_props$moisture_content,
    species = wood_props$species,
    wood_type = wood_props$wood_type,
    temperature = wood_props$temperature,
    tree_measurements = tree_meas,
    quality_thresholds = quality,
    yaml_source = yaml_file,
    yaml_data = config_data
  )

  return(wood_config)
}


#' Get Default Wood Properties
#'
#' Returns the default wood properties configuration (generic softwood).
#' This is a convenience wrapper around load_wood_properties("generic_sw").
#'
#' @param overrides Named list of parameters to override. Optional.
#' @param tree_overrides Named list of tree measurements to override. Optional.
#'
#' @return WoodProperties R6 object
#'
#' @examples
#' \dontrun{
#' # Get default properties
#' wood <- get_default_wood_properties()
#'
#' # Get default with overrides
#' wood <- get_default_wood_properties(
#'   overrides = list(thermal_diffusivity = 0.003),
#'   tree_overrides = list(dbh = 40)
#' )
#' }
#'
#' @family wood property functions
#' @export
get_default_wood_properties <- function(overrides = NULL,
                                        tree_overrides = NULL) {
  load_wood_properties("generic_sw",
                      overrides = overrides,
                      tree_overrides = tree_overrides)
}


#' List Available Wood Property Configurations
#'
#' Lists all wood property configurations available in the package,
#' including configuration names, species, and descriptions.
#'
#' @return Data frame with columns:
#'   \item{name}{Configuration name (use with load_wood_properties())}
#'   \item{species}{Species name}
#'   \item{description}{Configuration description}
#'   \item{file}{Full path to YAML file}
#'   \item{default}{Logical indicating if this is the default}
#'
#' @examples
#' \dontrun{
#' # List all available configurations
#' configs <- list_available_wood_properties()
#' print(configs)
#'
#' # Load a specific configuration
#' wood <- load_wood_properties(configs$name[2])
#' }
#'
#' @family wood property functions
#' @export
list_available_wood_properties <- function() {

  # Find all wood_*.yaml files in package configurations
  config_dir <- system.file("configurations", package = "sapfluxr")

  if (config_dir == "" || !dir.exists(config_dir)) {
    warning("Package configurations directory not found")
    return(data.frame(
      name = character(0),
      species = character(0),
      description = character(0),
      file = character(0),
      default = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  yaml_files <- list.files(
    config_dir,
    pattern = "^wood_.*\\.yaml$",
    full.names = TRUE
  )

  if (length(yaml_files) == 0) {
    return(data.frame(
      name = character(0),
      species = character(0),
      description = character(0),
      file = character(0),
      default = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  # Parse each YAML to get metadata
  configs <- lapply(yaml_files, function(f) {
    config_data <- tryCatch(
      yaml::read_yaml(f),
      error = function(e) NULL
    )

    if (is.null(config_data)) {
      return(NULL)
    }

    data.frame(
      name = gsub("^wood_(.*)\\.yaml$", "\\1", basename(f)),
      species = ifelse(is.null(config_data$wood_property$species),
                      "unknown",
                      config_data$wood_property$species),
      description = ifelse(is.null(config_data$metadata$description),
                          "",
                          config_data$metadata$description),
      file = f,
      default = isTRUE(config_data$metadata$default),
      stringsAsFactors = FALSE
    )
  })

  # Remove NULLs and combine
  configs <- configs[!sapply(configs, is.null)]
  if (length(configs) == 0) {
    return(data.frame(
      name = character(0),
      species = character(0),
      description = character(0),
      file = character(0),
      default = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, configs)

  # Sort with default first
  result <- result[order(-result$default, result$name), ]
  rownames(result) <- NULL

  return(result)
}


#' Create Custom Wood Properties Configuration
#'
#' Helper function to create a custom wood properties configuration in R
#' without needing a YAML file. Useful for quick testing or one-off configurations.
#'
#' @param config_name Name for the configuration
#' @param species Species identification
#' @param thermal_diffusivity Thermal diffusivity (cm²/s). Required.
#' @param thermal_conductivity Thermal conductivity (W/(m·K)). Optional.
#' @param volumetric_heat_capacity Volumetric heat capacity (J/(m³·K)). Optional.
#' @param dry_density Oven-dry wood density (kg/m³). Optional.
#' @param basic_density Basic density (kg/m³). Optional.
#' @param moisture_content Moisture content (%). Optional.
#' @param wood_type Wood type ("softwood"/"hardwood"). Default: "softwood"
#' @param temperature Typical wood temperature (°C). Default: 20
#' @param dbh Diameter at breast height (cm). Optional.
#' @param bark_thickness Bark thickness (cm). Optional.
#' @param sapwood_depth Sapwood depth (cm). Optional.
#' @param sapwood_area Sapwood area (cm²). Optional.
#' @param heartwood_radius Heartwood radius (cm). Optional.
#' @param max_velocity_cm_hr Maximum velocity threshold (cm/hr). Default: 200
#' @param min_velocity_cm_hr Minimum velocity threshold (cm/hr). Default: -50
#' @param temperature_range Temperature range (°C). Default: c(-10, 60)
#'
#' @return WoodProperties R6 object
#'
#' @examples
#' \dontrun{
#' # Create custom wood properties
#' custom_wood <- create_custom_wood_properties(
#'   config_name = "My Custom Wood",
#'   species = "Pinus radiata",
#'   thermal_diffusivity = 0.0028,
#'   dry_density = 450,
#'   moisture_content = 35,
#'   dbh = 45.2,
#'   sapwood_depth = 3.5
#' )
#'
#' # Use in calculations
#' results <- calc_heat_pulse_velocity(sap_data, wood_properties = custom_wood)
#' }
#'
#' @family wood property functions
#' @export
create_custom_wood_properties <- function(config_name = "Custom Wood Properties",
                                          species = "unknown",
                                          thermal_diffusivity = 0.0025,
                                          thermal_conductivity = NULL,
                                          volumetric_heat_capacity = NULL,
                                          dry_density = NULL,
                                          basic_density = NULL,
                                          moisture_content = NULL,
                                          wood_type = "softwood",
                                          temperature = 20,
                                          dbh = NULL,
                                          bark_thickness = NULL,
                                          sapwood_depth = NULL,
                                          sapwood_area = NULL,
                                          heartwood_radius = NULL,
                                          max_velocity_cm_hr = 200,
                                          min_velocity_cm_hr = -50,
                                          temperature_range = c(-10, 60)) {

  # Create tree measurements list
  tree_measurements <- list(
    dbh = dbh,
    bark_thickness = bark_thickness,
    sapwood_depth = sapwood_depth,
    sapwood_area = sapwood_area,
    heartwood_radius = heartwood_radius
  )

  # Create quality thresholds list
  quality_thresholds <- list(
    max_velocity_cm_hr = max_velocity_cm_hr,
    min_velocity_cm_hr = min_velocity_cm_hr,
    temperature_range = temperature_range
  )

  # Create WoodProperties object
  wood_config <- WoodProperties$new(
    config_name = config_name,
    thermal_diffusivity = thermal_diffusivity,
    thermal_conductivity = thermal_conductivity,
    volumetric_heat_capacity = volumetric_heat_capacity,
    dry_density = dry_density,
    basic_density = basic_density,
    moisture_content = moisture_content,
    species = species,
    wood_type = wood_type,
    temperature = temperature,
    tree_measurements = tree_measurements,
    quality_thresholds = quality_thresholds
  )

  return(wood_config)
}
