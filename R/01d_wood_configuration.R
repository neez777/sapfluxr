# R/02b_wood_configuration.R
# Wood Properties Configuration System
# Manages wood thermal and physical properties for sap flow calculations

# Hardcoded fallback defaults ----

#' Get Hardcoded Wood Properties Defaults
#'
#' Returns hardcoded wood properties data as a list matching YAML structure.
#' This serves as a fallback if YAML files cannot be found.
#'
#' @param config_name Name of configuration ("generic_sw", "eucalyptus", "pine")
#' @return List with wood properties data
#' @keywords internal
get_hardcoded_wood_defaults <- function(config_name = "generic_sw") {

  if (config_name == "generic_sw") {
    # Generic Softwood Properties
    list(
      metadata = list(
        config_name = "Typical Softwood Properties",
        description = "Default wood properties for temperate softwood species",
        version = "1.0",
        default = TRUE
      ),
      wood_property = list(
        thermal_diffusivity = 0.0025,       # cm²/s
        thermal_conductivity = NULL,        # W/(m·K) - will be estimated
        volumetric_heat_capacity = NULL,    # J/(m³·K) - will be estimated
        dry_density = 400,                  # kg/m³
        fresh_density = 350,                # kg/m³
        moisture_content = 30,              # %
        species = "unknown",
        wood_type = "softwood",
        temperature = 20                    # °C
      ),
      tree_measurements = list(
        dbh = NULL,
        bark_thickness = NULL,
        sapwood_depth = NULL,
        sapwood_area = NULL,
        heartwood_radius = NULL
      ),
      quality_thresholds = list(
        max_velocity_cm_hr = 200,
        min_velocity_cm_hr = -50,
        temperature_range = c(-10, 60)
      )
    )

  } else if (config_name == "eucalyptus") {
    # Eucalyptus Properties
    list(
      metadata = list(
        config_name = "Eucalyptus Wood Properties",
        description = "Typical wood properties for Eucalyptus species",
        version = "1.0",
        default = FALSE
      ),
      wood_property = list(
        thermal_diffusivity = 0.00143,
        thermal_conductivity = 0.27,
        volumetric_heat_capacity = 1886000,
        dry_density = 550,
        fresh_density = 480,
        moisture_content = 25,
        species = "Eucalyptus spp.",
        wood_type = "hardwood",
        temperature = 25
      ),
      tree_measurements = list(
        dbh = NULL,
        bark_thickness = NULL,
        sapwood_depth = NULL,
        sapwood_area = NULL,
        heartwood_radius = NULL
      ),
      quality_thresholds = list(
        max_velocity_cm_hr = 150,
        min_velocity_cm_hr = -50,
        temperature_range = c(0, 50)
      )
    )

  } else if (config_name == "pine") {
    # Pine Properties
    list(
      metadata = list(
        config_name = "Pine Wood Properties",
        description = "Typical wood properties for Pine species",
        version = "1.0",
        default = FALSE
      ),
      wood_property = list(
        thermal_diffusivity = 0.00145,
        thermal_conductivity = 0.22,
        volumetric_heat_capacity = 963700,
        dry_density = 450,
        fresh_density = 380,
        moisture_content = 35,
        species = "Pinus spp.",
        wood_type = "softwood",
        temperature = 15
      ),
      tree_measurements = list(
        dbh = NULL,
        bark_thickness = NULL,
        sapwood_depth = NULL,
        sapwood_area = NULL,
        heartwood_radius = NULL
      ),
      quality_thresholds = list(
        max_velocity_cm_hr = 180,
        min_velocity_cm_hr = -50,
        temperature_range = c(-15, 45)
      )
    )

  } else {
    # Default to generic softwood if unknown
    get_hardcoded_wood_defaults("generic_sw")
  }
}

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
#' @field fresh_density Basic density - oven-dry mass / green volume (kg/m³)
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
    fresh_density = NULL,
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
    #' @param fresh_density Basic density (kg/m³)
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
                         fresh_density = NULL,
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
      self$fresh_density <- fresh_density
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
      if (!is.null(self$fresh_density)) {
        cat("  Basic density:", self$fresh_density, "kg/m³\n")
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

  # Try to load from YAML file
  config_data <- NULL
  yaml_source <- NULL

  if (file.exists(config_name)) {
    # User provided full path to custom YAML
    yaml_file <- config_name
    yaml_source <- yaml_file
    config_data <- tryCatch(
      yaml::read_yaml(yaml_file),
      error = function(e) {
        warning("Failed to parse YAML file: ", e$message,
               "\nFalling back to hardcoded defaults.")
        NULL
      }
    )

  } else {
    # Look in package configurations
    yaml_file <- system.file(
      "configurations",
      paste0("wood_", config_name, ".yaml"),
      package = "sapfluxr"
    )

    if (file.exists(yaml_file) && yaml_file != "") {
      yaml_source <- yaml_file
      config_data <- tryCatch(
        yaml::read_yaml(yaml_file),
        error = function(e) {
          warning("Failed to parse YAML file: ", e$message,
                 "\nFalling back to hardcoded defaults.")
          NULL
        }
      )
    }
  }

  # Fall back to hardcoded defaults if YAML not found or failed to parse
  if (is.null(config_data)) {
    message("Using hardcoded wood properties defaults for '", config_name, "'")
    config_data <- get_hardcoded_wood_defaults(config_name)
    yaml_source <- "hardcoded_defaults"
  }

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
    fresh_density = wood_props$fresh_density,
    moisture_content = wood_props$moisture_content,
    species = wood_props$species,
    wood_type = wood_props$wood_type,
    temperature = wood_props$temperature,
    tree_measurements = tree_meas,
    quality_thresholds = quality,
    yaml_source = yaml_source,
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
#' without needing a YAML file. Automatically derives missing properties from
#' available measurements (e.g., calculates moisture content from densities).
#'
#' @param config_name Name for the configuration
#' @param species Species identification
#' @param thermal_diffusivity Thermal diffusivity (cm²/s). Default: 0.0025.
#' @param thermal_conductivity Thermal conductivity (W/(m·K)). Optional.
#' @param volumetric_heat_capacity Volumetric heat capacity (J/(m³·K)). Optional.
#' @param dry_density Oven-dry wood density (kg/m³). Optional.
#' @param fresh_density Basic density (kg/m³). Optional.
#' @param moisture_content Moisture content (%). Optional.
#' @param fresh_weight Fresh/wet weight of wood sample (g). Optional.
#' @param dry_weight Oven-dry weight of wood sample (g). Optional.
#' @param fresh_volume Fresh/green volume of wood sample (cm³). Optional.
#' @param dry_volume Dry volume of wood sample (cm³). Optional.
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
#' @param assume_no_shrinkage Logical. If TRUE, assumes fresh_volume = dry_volume
#'   when deriving properties. This allows moisture content to be calculated from
#'   fresh_density and dry_density alone. Default: TRUE.
#' @param verbose Logical. If TRUE, prints information about derived properties.
#'   Default: TRUE.
#'
#' @return WoodProperties R6 object with automatically derived missing properties
#'
#' @details
#' This function automatically calls `derive_wood_properties()` to fill in any
#' missing wood property values from the measurements you provide. This is
#' particularly useful when you have:
#'
#' - **Both densities but no moisture content**: With `assume_no_shrinkage = TRUE`,
#'   MC will be calculated as (ρ_fresh - ρ_dry) / ρ_dry × 100
#' - **Weights but no densities**: Provide volume to calculate densities
#' - **Partial measurements**: The function will derive as much as possible
#'
#' The derivation respects the physical relationships between properties and will
#' warn if values are inconsistent or outside typical ranges.
#'
#' @examples
#' \dontrun{
#' # Example 1: Traditional usage with complete data
#' wood1 <- create_custom_wood_properties(
#'   config_name = "My Pine",
#'   species = "Pinus radiata",
#'   thermal_diffusivity = 0.0028,
#'   dry_density = 450,
#'   moisture_content = 35,
#'   dbh = 45.2
#' )
#'
#' # Example 2: Derive MC from two densities (no shrinkage assumption)
#' wood2 <- create_custom_wood_properties(
#'   config_name = "My Eucalypt",
#'   species = "Eucalyptus",
#'   fresh_density = 380,    # kg/m³ (fresh density at constant volume)
#'   dry_density = 450,      # kg/m³ (dry density at same volume)
#'   assume_no_shrinkage = TRUE
#'   # Automatically derives: MC = 18.4%
#' )
#'
#' # Example 3: From wood core measurements
#' wood3 <- create_custom_wood_properties(
#'   fresh_weight = 2.45,    # grams
#'   dry_weight = 1.38,      # grams
#'   fresh_volume = 2.73     # cm³
#'   # Automatically derives: fresh_density, moisture_content
#' )
#'
#' # Example 4: Mix of measurements
#' wood4 <- create_custom_wood_properties(
#'   dry_density = 450,
#'   moisture_content = 35,
#'   dry_weight = 10,
#'   assume_no_shrinkage = TRUE
#'   # Automatically derives: fresh_weight, fresh_density, volumes
#' )
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
                                          fresh_density = NULL,
                                          moisture_content = NULL,
                                          fresh_weight = NULL,
                                          dry_weight = NULL,
                                          fresh_volume = NULL,
                                          dry_volume = NULL,
                                          wood_type = "softwood",
                                          temperature = 20,
                                          dbh = NULL,
                                          bark_thickness = NULL,
                                          sapwood_depth = NULL,
                                          sapwood_area = NULL,
                                          heartwood_radius = NULL,
                                          max_velocity_cm_hr = 200,
                                          min_velocity_cm_hr = -50,
                                          temperature_range = c(-10, 60),
                                          assume_no_shrinkage = TRUE,
                                          verbose = TRUE) {

  # Check if we have any measurements that could be used for derivation
  has_measurements <- !is.null(fresh_weight) || !is.null(dry_weight) ||
                      !is.null(fresh_volume) || !is.null(dry_volume) ||
                      !is.null(fresh_density) || !is.null(dry_density) ||
                      !is.null(moisture_content)

  # If we have measurements, try to derive missing properties
  if (has_measurements) {
    # Call derive_wood_properties to fill in missing values
    derived <- derive_wood_properties(
      fresh_weight = fresh_weight,
      dry_weight = dry_weight,
      fresh_volume = fresh_volume,
      dry_volume = dry_volume,
      fresh_density = fresh_density,
      dry_density = dry_density,
      moisture_content = moisture_content,
      assume_no_shrinkage = assume_no_shrinkage,
      density_units = "auto",
      mc_units = "auto"
    )

    # Use derived values (prefer derived over NULL, but keep user-provided values)
    if (is.null(fresh_density) && !is.null(derived$fresh_density_kg_m3)) {
      fresh_density <- derived$fresh_density_kg_m3
    }
    if (is.null(dry_density) && !is.null(derived$dry_density_kg_m3)) {
      dry_density <- derived$dry_density_kg_m3
    }
    if (is.null(moisture_content) && !is.null(derived$moisture_content_percent)) {
      moisture_content <- derived$moisture_content_percent
    }

    # Print derivation summary if verbose
    if (verbose && length(derived$derived) > 0) {
      message("Automatically derived wood properties:")
      for (d in derived$derived) {
        message("  - ", d)
      }
      if (!is.null(derived$fresh_density)) {
        message(sprintf("  Basic density: %.0f kg/m³", derived$fresh_density_kg_m3))
      }
      if (!is.null(derived$dry_density)) {
        message(sprintf("  Dry density: %.0f kg/m³", derived$dry_density_kg_m3))
      }
      if (!is.null(derived$moisture_content)) {
        message(sprintf("  Moisture content: %.1f%%", derived$moisture_content_percent))
      }
      if (length(derived$assumptions) > 0) {
        message("Assumptions:")
        for (a in derived$assumptions) {
          message("  - ", a)
        }
      }
      if (derived$quality_flag != "OK") {
        message("Quality: ", derived$quality_flag)
        if (length(derived$notes) > 0) {
          for (n in derived$notes) {
            message("  Note: ", n)
          }
        }
      }
    }
  }

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
    fresh_density = fresh_density,
    moisture_content = moisture_content,
    species = species,
    wood_type = wood_type,
    temperature = temperature,
    tree_measurements = tree_measurements,
    quality_thresholds = quality_thresholds
  )

  return(wood_config)
}


#' Derive Wood Properties from Available Measurements
#'
#' Flexible function to calculate wood properties from any combination of
#' available measurements. Useful when you have partial data and need to
#' derive other properties.
#'
#' @param fresh_weight Fresh/wet weight of wood sample (g). Optional.
#' @param dry_weight Oven-dry weight of wood sample (g). Optional.
#' @param fresh_volume Fresh/green volume of wood sample (cm³). Optional.
#' @param dry_volume Dry volume of wood sample (cm³). Optional.
#' @param fresh_density Basic density - dry mass / green volume (g/cm³ or kg/m³). Optional.
#' @param dry_density Dry density - dry mass / dry volume (g/cm³ or kg/m³). Optional.
#' @param moisture_content Moisture content on dry weight basis (% or decimal). Optional.
#' @param assume_no_shrinkage Logical. If TRUE, assumes fresh_volume = dry_volume.
#'   This simplifies calculations but is not physically accurate. Default: TRUE.
#' @param density_units Character. Units for density inputs: "g_cm3" or "kg_m3".
#'   Default: "auto". Automatically detected if values > 50.
#' @param mc_units Character. Units for moisture_content: "percent" or "decimal".
#'   Default: "auto". Automatically detected if value < 1.
#'
#' @return List containing:
#'   \describe{
#'     \item{fresh_weight}{Fresh weight (g)}
#'     \item{dry_weight}{Dry weight (g)}
#'     \item{fresh_volume}{Fresh volume (cm³)}
#'     \item{dry_volume}{Dry volume (cm³)}
#'     \item{fresh_density}{Basic density (g/cm³)}
#'     \item{dry_density}{Dry density (g/cm³)}
#'     \item{moisture_content}{Moisture content (decimal)}
#'     \item{moisture_content_percent}{Moisture content (%)}
#'     \item{water_weight}{Weight of water (g)}
#'     \item{shrinkage_factor}{Volumetric shrinkage ratio (dry_vol / fresh_vol)}
#'     \item{provided}{Character vector of provided inputs}
#'     \item{derived}{Character vector of derived outputs}
#'     \item{assumptions}{Character vector of assumptions made}
#'     \item{quality_flag}{"OK", "WARNING", or "ERROR"}
#'     \item{notes}{Character vector of notes and warnings}
#'   }
#'
#' @details
#' **Fundamental Relationships:**
#' - fresh_density = dry_weight / fresh_volume
#' - dry_density = dry_weight / dry_volume
#' - moisture_content = (fresh_weight - dry_weight) / dry_weight
#' - fresh_weight = dry_weight * (1 + moisture_content)
#'
#' **Assumptions:**
#' If `assume_no_shrinkage = TRUE`, the function assumes fresh_volume = dry_volume.
#' This means fresh_density = dry_density. This is a simplification - in reality,
#' wood shrinks as it dries. Use this option when you only have density values
#' and need to estimate moisture content.
#'
#' **Minimum Required Inputs:**
#' You must provide enough information to solve the system. Examples:
#' - fresh_weight + dry_weight (→ can get MC, but not densities without volume)
#' - dry_weight + fresh_volume + fresh_density (→ can solve for most)
#' - fresh_density + dry_density + any one mass or volume
#' - etc.
#'
#' @examples
#' \dontrun{
#' # Example 1: From weights only
#' result <- derive_wood_properties(
#'   fresh_weight = 2.45,
#'   dry_weight = 1.38
#' )
#' # Returns: MC = 77.5%, but no density info
#'
#' # Example 2: From both densities (assuming no shrinkage)
#' result <- derive_wood_properties(
#'   fresh_density = 380,  # kg/m³
#'   dry_density = 450,    # kg/m³
#'   assume_no_shrinkage = TRUE
#' )
#' # Returns: Warning - conflicting densities under no-shrinkage assumption
#'
#' # Example 3: Complete measurements
#' result <- derive_wood_properties(
#'   fresh_weight = 2.45,
#'   dry_weight = 1.38,
#'   fresh_volume = 2.73
#' )
#' # Returns: All properties calculated
#'
#' # Example 4: From dry density and moisture content
#' result <- derive_wood_properties(
#'   dry_density = 450,     # kg/m³
#'   moisture_content = 35, # %
#'   dry_weight = 10        # g (for scaling)
#' )
#' }
#'
#' @family wood property functions
#' @export
derive_wood_properties <- function(fresh_weight = NULL,
                                   dry_weight = NULL,
                                   fresh_volume = NULL,
                                   dry_volume = NULL,
                                   fresh_density = NULL,
                                   dry_density = NULL,
                                   moisture_content = NULL,
                                   assume_no_shrinkage = TRUE,
                                   density_units = "auto",
                                   mc_units = "auto") {

  # Initialize tracking
  provided <- character(0)
  derived <- character(0)
  assumptions <- character(0)
  notes <- character(0)
  quality_flag <- "OK"

  # Track what was provided
  if (!is.null(fresh_weight)) provided <- c(provided, "fresh_weight")
  if (!is.null(dry_weight)) provided <- c(provided, "dry_weight")
  if (!is.null(fresh_volume)) provided <- c(provided, "fresh_volume")
  if (!is.null(dry_volume)) provided <- c(provided, "dry_volume")
  if (!is.null(fresh_density)) provided <- c(provided, "fresh_density")
  if (!is.null(dry_density)) provided <- c(provided, "dry_density")
  if (!is.null(moisture_content)) provided <- c(provided, "moisture_content")

  if (length(provided) == 0) {
    stop("No inputs provided. You must provide at least some measurements.")
  }

  # Auto-detect and convert units
  # Densities: if > 50, assume kg/m³, otherwise g/cm³
  if (!is.null(fresh_density)) {
    if (density_units == "auto") {
      if (fresh_density > 50) {
        fresh_density <- fresh_density / 1000  # Convert kg/m³ to g/cm³
        notes <- c(notes, "Auto-detected fresh_density in kg/m³, converted to g/cm³")
      }
    } else if (density_units == "kg_m3") {
      fresh_density <- fresh_density / 1000
    }
  }

  if (!is.null(dry_density)) {
    if (density_units == "auto") {
      if (dry_density > 50) {
        dry_density <- dry_density / 1000  # Convert kg/m³ to g/cm³
        notes <- c(notes, "Auto-detected dry_density in kg/m³, converted to g/cm³")
      }
    } else if (density_units == "kg_m3") {
      dry_density <- dry_density / 1000
    }
  }

  # Moisture content: if > 1, assume %, otherwise decimal
  if (!is.null(moisture_content)) {
    if (mc_units == "auto") {
      if (moisture_content > 1) {
        moisture_content <- moisture_content / 100  # Convert % to decimal
        notes <- c(notes, "Auto-detected moisture_content in %, converted to decimal")
      }
    } else if (mc_units == "percent") {
      moisture_content <- moisture_content / 100
    }
  }

  # Apply no-shrinkage assumption if requested
  if (assume_no_shrinkage) {
    assumptions <- c(assumptions, "Assuming no volumetric shrinkage (fresh_volume = dry_volume)")

    if (!is.null(fresh_volume) && !is.null(dry_volume)) {
      if (abs(fresh_volume - dry_volume) / fresh_volume > 0.01) {
        quality_flag <- "WARNING"
        notes <- c(notes, "fresh_volume and dry_volume differ despite assume_no_shrinkage = TRUE")
      }
    }

    # If we have one volume, use it for both
    if (!is.null(fresh_volume) && is.null(dry_volume)) {
      dry_volume <- fresh_volume
      derived <- c(derived, "dry_volume (from no-shrinkage assumption)")
    } else if (!is.null(dry_volume) && is.null(fresh_volume)) {
      fresh_volume <- dry_volume
      derived <- c(derived, "fresh_volume (from no-shrinkage assumption)")
    }

    # CRITICAL: If we have both densities under no-shrinkage, we can derive MC!
    # Under no-shrinkage assumption with constant volume V:
    # - fresh_density (or fresh_density) = fresh_mass / V
    # - dry_density (at same V) = dry_mass / V
    # - MC = (fresh_mass - dry_mass) / dry_mass
    #      = (fresh_density × V - dry_density × V) / (dry_density × V)
    #      = (fresh_density - dry_density) / dry_density
    # Volume cancels out!
    if (!is.null(fresh_density) && !is.null(dry_density)) {
      # If they differ, interpret fresh_density as fresh/green density
      # and dry_density as dry density (both at same volume)
      if (abs(fresh_density - dry_density) / max(fresh_density, dry_density) > 0.01) {
        # Determine which is fresh and which is dry
        # Fresh density should be higher (includes water)
        rho_fresh <- max(fresh_density, dry_density)
        rho_dry <- min(fresh_density, dry_density)

        # Calculate moisture content: MC = (ρ_f - ρ_d) / ρ_d
        if (is.null(moisture_content)) {
          moisture_content <- (rho_fresh - rho_dry) / rho_dry
          derived <- c(derived, "moisture_content (from density difference under no-shrinkage)")
          notes <- c(notes,
                    sprintf("Derived MC = %.1f%% from density difference (%.3f - %.3f) / %.3f",
                           moisture_content * 100, rho_fresh, rho_dry, rho_dry))
        }

        # Under no-shrinkage, fresh_density should equal dry_density at same volume
        # But if we have fresh and dry densities, set them appropriately
        if (fresh_density > dry_density) {
          # fresh_density was actually fresh/green density
          # Keep both as-is - they represent different states
          notes <- c(notes,
                    "Interpreting fresh_density as fresh/green density and dry_density as dry density (both at same volume)")
        } else {
          # dry_density was actually fresh density (larger value)
          # Swap the interpretation
          temp <- fresh_density
          fresh_density <- dry_density
          dry_density <- temp
          derived <- c(derived, "swapped fresh_density and dry_density interpretation")
          notes <- c(notes,
                    "Swapped density values: larger value interpreted as fresh density, smaller as dry density")
        }
      } else {
        # Densities are nearly equal - truly no moisture difference or same measurement
        if (is.null(moisture_content)) {
          moisture_content <- 0
          derived <- c(derived, "moisture_content (assumed zero - densities equal)")
          notes <- c(notes, "Densities nearly equal under no-shrinkage - assuming no moisture")
        }
      }
    } else if (!is.null(fresh_density) && is.null(dry_density)) {
      dry_density <- fresh_density
      derived <- c(derived, "dry_density (from no-shrinkage assumption)")
    } else if (!is.null(dry_density) && is.null(fresh_density)) {
      fresh_density <- dry_density
      derived <- c(derived, "fresh_density (from no-shrinkage assumption)")
    }
  }

  # Derivation logic - iteratively solve
  max_iterations <- 10
  iteration <- 0
  something_changed <- TRUE

  while (something_changed && iteration < max_iterations) {
    something_changed <- FALSE
    iteration <- iteration + 1

    # Rule 1: fresh_weight = dry_weight * (1 + moisture_content)
    if (!is.null(dry_weight) && !is.null(moisture_content) && is.null(fresh_weight)) {
      fresh_weight <- dry_weight * (1 + moisture_content)
      derived <- c(derived, "fresh_weight")
      something_changed <- TRUE
    }

    # Rule 2: moisture_content = (fresh_weight - dry_weight) / dry_weight
    if (!is.null(fresh_weight) && !is.null(dry_weight) && is.null(moisture_content)) {
      moisture_content <- (fresh_weight - dry_weight) / dry_weight
      derived <- c(derived, "moisture_content")
      something_changed <- TRUE
    }

    # Rule 3: dry_weight = fresh_weight / (1 + moisture_content)
    if (!is.null(fresh_weight) && !is.null(moisture_content) && is.null(dry_weight)) {
      dry_weight <- fresh_weight / (1 + moisture_content)
      derived <- c(derived, "dry_weight")
      something_changed <- TRUE
    }

    # Rule 4: fresh_density = dry_weight / fresh_volume
    if (!is.null(dry_weight) && !is.null(fresh_volume) && is.null(fresh_density)) {
      fresh_density <- dry_weight / fresh_volume
      derived <- c(derived, "fresh_density")
      something_changed <- TRUE
    }

    # Rule 5: dry_weight = fresh_density * fresh_volume
    if (!is.null(fresh_density) && !is.null(fresh_volume) && is.null(dry_weight)) {
      dry_weight <- fresh_density * fresh_volume
      derived <- c(derived, "dry_weight")
      something_changed <- TRUE
    }

    # Rule 6: fresh_volume = dry_weight / fresh_density
    if (!is.null(dry_weight) && !is.null(fresh_density) && is.null(fresh_volume)) {
      fresh_volume <- dry_weight / fresh_density
      derived <- c(derived, "fresh_volume")
      something_changed <- TRUE
    }

    # Rule 7: dry_density = dry_weight / dry_volume
    if (!is.null(dry_weight) && !is.null(dry_volume) && is.null(dry_density)) {
      dry_density <- dry_weight / dry_volume
      derived <- c(derived, "dry_density")
      something_changed <- TRUE
    }

    # Rule 8: dry_weight = dry_density * dry_volume
    if (!is.null(dry_density) && !is.null(dry_volume) && is.null(dry_weight)) {
      dry_weight <- dry_density * dry_volume
      derived <- c(derived, "dry_weight")
      something_changed <- TRUE
    }

    # Rule 9: dry_volume = dry_weight / dry_density
    if (!is.null(dry_weight) && !is.null(dry_density) && is.null(dry_volume)) {
      dry_volume <- dry_weight / dry_density
      derived <- c(derived, "dry_volume")
      something_changed <- TRUE
    }

    # Rule 10: If no-shrinkage and we have fresh_volume, set dry_volume
    if (assume_no_shrinkage && !is.null(fresh_volume) && is.null(dry_volume)) {
      dry_volume <- fresh_volume
      derived <- c(derived, "dry_volume (no-shrinkage)")
      something_changed <- TRUE
    }

    # Rule 11: If no-shrinkage and we have dry_volume, set fresh_volume
    if (assume_no_shrinkage && !is.null(dry_volume) && is.null(fresh_volume)) {
      fresh_volume <- dry_volume
      derived <- c(derived, "fresh_volume (no-shrinkage)")
      something_changed <- TRUE
    }
  }

  # Calculate derived properties
  water_weight <- NULL
  if (!is.null(fresh_weight) && !is.null(dry_weight)) {
    water_weight <- fresh_weight - dry_weight
  }

  shrinkage_factor <- NULL
  if (!is.null(dry_volume) && !is.null(fresh_volume)) {
    shrinkage_factor <- dry_volume / fresh_volume
    if (shrinkage_factor < 0.7 || shrinkage_factor > 1.0) {
      quality_flag <- "WARNING"
      notes <- c(notes,
                sprintf("Shrinkage factor (%.3f) outside typical range (0.7-1.0)", shrinkage_factor))
    }
  }

  # Validation
  if (!is.null(moisture_content)) {
    if (moisture_content < 0.1 || moisture_content > 1.0) {
      quality_flag <- "WARNING"
      notes <- c(notes,
                sprintf("Moisture content (%.1f%%) outside typical range (10-100%%)",
                       moisture_content * 100))
    }
  }

  if (!is.null(fresh_density)) {
    if (fresh_density < 0.2 || fresh_density > 1.0) {
      quality_flag <- "WARNING"
      notes <- c(notes,
                sprintf("Basic density (%.3f g/cm³) outside typical range (0.2-1.0)", fresh_density))
    }
  }

  if (!is.null(dry_density)) {
    if (dry_density < 0.2 || dry_density > 1.2) {
      quality_flag <- "WARNING"
      notes <- c(notes,
                sprintf("Dry density (%.3f g/cm³) outside typical range (0.2-1.2)", dry_density))
    }
  }

  # Check for consistency
  if (!is.null(fresh_density) && !is.null(dry_density) && !assume_no_shrinkage) {
    if (dry_density < fresh_density) {
      quality_flag <- "ERROR"
      notes <- c(notes, "Dry density cannot be less than basic density (dry_density < fresh_density)")
    }
  }

  # Prepare output
  result <- list(
    fresh_weight = fresh_weight,
    dry_weight = dry_weight,
    fresh_volume = fresh_volume,
    dry_volume = dry_volume,
    fresh_density = fresh_density,
    fresh_density_kg_m3 = if (!is.null(fresh_density)) fresh_density * 1000 else NULL,
    dry_density = dry_density,
    dry_density_kg_m3 = if (!is.null(dry_density)) dry_density * 1000 else NULL,
    moisture_content = moisture_content,
    moisture_content_percent = if (!is.null(moisture_content)) moisture_content * 100 else NULL,
    water_weight = water_weight,
    shrinkage_factor = shrinkage_factor,
    provided = provided,
    derived = unique(derived),
    assumptions = assumptions,
    quality_flag = quality_flag,
    notes = notes
  )

  class(result) <- c("wood_properties_derivation", "list")
  return(result)
}


#' Print Method for Wood Properties Derivation
#'
#' @param x A wood_properties_derivation object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.wood_properties_derivation <- function(x, ...) {
  cat("Wood Properties Derivation\n")
  cat("==========================\n\n")

  cat("Quality:", x$quality_flag, "\n\n")

  if (length(x$provided) > 0) {
    cat("Provided inputs:\n")
    for (p in x$provided) {
      cat("  -", p, "\n")
    }
    cat("\n")
  }

  if (length(x$derived) > 0) {
    cat("Derived outputs:\n")
    for (d in x$derived) {
      cat("  -", d, "\n")
    }
    cat("\n")
  }

  cat("Results:\n")
  if (!is.null(x$fresh_weight)) cat(sprintf("  Fresh weight: %.3f g\n", x$fresh_weight))
  if (!is.null(x$dry_weight)) cat(sprintf("  Dry weight: %.3f g\n", x$dry_weight))
  if (!is.null(x$water_weight)) cat(sprintf("  Water weight: %.3f g\n", x$water_weight))
  if (!is.null(x$fresh_volume)) cat(sprintf("  Fresh volume: %.3f cm³\n", x$fresh_volume))
  if (!is.null(x$dry_volume)) cat(sprintf("  Dry volume: %.3f cm³\n", x$dry_volume))
  if (!is.null(x$fresh_density)) {
    cat(sprintf("  Basic density: %.3f g/cm³ (%.0f kg/m³)\n",
               x$fresh_density, x$fresh_density_kg_m3))
  }
  if (!is.null(x$dry_density)) {
    cat(sprintf("  Dry density: %.3f g/cm³ (%.0f kg/m³)\n",
               x$dry_density, x$dry_density_kg_m3))
  }
  if (!is.null(x$moisture_content)) {
    cat(sprintf("  Moisture content: %.1f%%\n", x$moisture_content_percent))
  }
  if (!is.null(x$shrinkage_factor)) {
    cat(sprintf("  Shrinkage factor: %.3f\n", x$shrinkage_factor))
  }

  if (length(x$assumptions) > 0) {
    cat("\nAssumptions:\n")
    for (a in x$assumptions) {
      cat("  -", a, "\n")
    }
  }

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (n in x$notes) {
      cat("  -", n, "\n")
    }
  }

  invisible(x)
}
