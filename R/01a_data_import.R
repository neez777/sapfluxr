# R/01a_data_import.R
# Sap flow data import with character-based chunking for malformed JSON

# Import required packages
#' @importFrom dplyr bind_rows filter select mutate group_by summarise slice ungroup
#' @importFrom tools file_path_sans_ext
#' @importFrom progressr progressor
NULL

#' Detect if running in Shiny environment
#' @return Logical indicating if in Shiny
#' @keywords internal
is_shiny <- function() {
  !is.null(shiny::getDefaultReactiveDomain())
}

#' Display message in console or Shiny UI
#' @param msg Character string message to display
#' @param type Type of notification for Shiny ("default", "message", "warning", "error")
#' @param duration Duration in seconds for Shiny notification (NULL for auto)
#' @keywords internal
show_message <- function(msg, type = "message", duration = NULL) {
  if (is_shiny()) {
    # In Shiny, use showNotification
    if (requireNamespace("shiny", quietly = TRUE)) {
      # Remove emoji from Shiny notifications for cleaner display
      # Use iconv instead of regex to avoid Windows Unicode issues
      clean_msg <- iconv(msg, to = "ASCII//TRANSLIT", sub = "")
      clean_msg <- trimws(clean_msg)

      shiny::showNotification(
        clean_msg,
        type = type,
        duration = duration
      )
    }
  } else {
    # In console, use cat
    cat(msg)
  }
}
#' Read Heat Pulse Data from ICT SFM1x Sensors
#'
#' Imports and parses raw heat pulse temperature data from ICT SFM1x sensors using character-based chunking.
#' Handles malformed JSON where entire datasets are on single lines.
#'
#' Progress reporting works in both command-line and Shiny applications. In console,
#' progress messages are displayed via \code{cat()}. In Shiny applications, status
#' messages automatically appear as \code{shiny::showNotification()} popups. Progress
#' bars work through the \code{progressr} package and must be wrapped in
#' \code{progressr::with_progress({})}.
#'
#' @param file_path Character string specifying the path to the data file
#' @param format Character string specifying format. Auto-detected if NULL. Currently ict_current, ict_legacy & csv.
#' @param validate_data Logical indicating whether to validate imported data (default: TRUE)
#' @param chunk_size Integer specifying characters per chunk (default: auto-sized)
#' @param show_progress Logical indicating whether to show progress (default: TRUE)
#' @param ... Additional arguments passed to specific import functions
#'
#' @return A list containing:
#'   \item{diagnostics}{Data frame with sensor diagnostics}
#'   \item{measurements}{Data frame with temperature measurements and timestamps}
#'   \item{metadata}{List containing file information and processing parameters}
#'
#' @examples
#' \dontrun{
#' # Basic usage in R console (with progress bar)
#' progressr::with_progress({
#'   heat_pulse_data <- read_heat_pulse_data("data/tree1_data.txt")
#' })
#'
#' # Usage in Shiny application - notifications appear automatically
#' server <- function(input, output, session) {
#'   observeEvent(input$import_button, {
#'     # Status messages will show as Shiny notifications
#'     # Progress bar updates automatically
#'     progressr::with_progress({
#'       heat_pulse_data <- read_heat_pulse_data(input$file$datapath)
#'     })
#'   })
#' }
#'
#' # Without progress reporting
#' heat_pulse_data <- read_heat_pulse_data("data/tree1_data.txt", show_progress = FALSE)
#'
#' # Specify format explicitly
#' heat_pulse_data <- read_heat_pulse_data("data/tree1_data.txt", format = "ict_current")
#' }
#'
#' @seealso \code{\link{read_multiple_heat_pulse_data}} for importing multiple files
#' @export
read_heat_pulse_data <- function(file_path,
                                 format = NULL,
                                 validate_data = TRUE,
                                 chunk_size = NULL,
                                 show_progress = NULL,
                                 ...) {

  # Check file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Get file info
  file_info <- file.info(file_path)
  file_size_mb <- file_info$size / 1e6

  # Set defaults
  if (is.null(show_progress)) {
    show_progress <- TRUE  # Always show progress by default
  }

  if (is.null(chunk_size)) {
    # Character chunk size (only used for files > 100MB)
    # For files < 100MB, entire file is read at once for maximum speed
    chunk_size <- if (file_size_mb < 10) {
      100000  # 100KB chunks (if needed)
    } else if (file_size_mb < 100) {
      500000  # 500KB chunks (if needed)
    } else if (file_size_mb < 500) {
      5000000 # 5MB chunks for large files (100-500MB)
    } else {
      10000000 # 10MB chunks for very large files (>500MB)
    }
  }

  if (show_progress) {
    show_message(sprintf("📊 Reading heat pulse data: %s (%.1f MB)\n",
                        basename(file_path), file_size_mb))
  }

  # Auto-detect format using first 3000 characters
  if (is.null(format)) {
    format <- detect_format(file_path)
    if (show_progress) {
      show_message(sprintf("🔍 Auto-detected format: %s\n", format))
    }
  }

  # Import based on format
  result <- switch(format,
                   "ict_current" = read_ict_current(file_path, chunk_size, show_progress, ...),
                   "ict_legacy" = read_ict_legacy(file_path, chunk_size, show_progress, ...),
                   "csv" = read_csv_format(file_path, show_progress, ...),
                   stop("Unsupported format: ", format)
  )

  # Add metadata
  result$metadata <- list(
    file_path = file_path,
    file_name = basename(file_path),
    format = format,
    import_time = Sys.time(),
    file_size = file_info$size,
    file_size_mb = file_size_mb,
    n_pulses = nrow(result$diagnostics),
    n_measurements = nrow(result$measurements),
    chunk_size = chunk_size,
    r_version = R.version.string,
    package_version = tryCatch(utils::packageVersion("sapFluxR"),
                               error = function(e) "dev")
  )

  class(result) <- c("heat_pulse_data", "list")

  # Detect and fill missing pulses BEFORE validation
  # This ensures completeness calculations are accurate
  if (nrow(result$measurements) > 1 && "datetime" %in% names(result$measurements)) {
    if (show_progress) {
      show_message("🔍 Detecting missing pulses...\n")
    }

    tryCatch({
      gap_detection_result <- detect_and_fill_missing_pulse_timestamps(
        measurements = result$measurements,
        expected_interval_hours = NULL,  # Auto-detect
        max_gap_to_fill_hours = 24,
        verbose = show_progress
      )

      # Update measurements with filled gaps
      result$measurements <- gap_detection_result$measurements_complete

      # Store gap detection results
      result$gap_detection <- list(
        gap_report = gap_detection_result$gap_report,
        summary = gap_detection_result$summary
      )

      # Report gap findings
      if (show_progress && !is.null(gap_detection_result$gap_report) && nrow(gap_detection_result$gap_report) > 0) {
        n_gaps <- nrow(gap_detection_result$gap_report)
        n_filled <- gap_detection_result$summary$n_filled
        show_message(sprintf("⚠️  Found %d gap(s), filled %d missing pulse(s) with interpolated timestamps\n",
                            n_gaps, n_filled))
      }

    }, error = function(e) {
      if (show_progress) {
        show_message(sprintf("⚠️  Gap detection failed: %s\n", e$message))
        message("    ERROR details: ", e$message)
        message("    ERROR call: ", paste(deparse(e$call), collapse = " "))
      }
      # Store empty gap detection on error
      result$gap_detection <- NULL
    })
  }

  # Validate if requested (now runs AFTER gap detection)
  if (validate_data) {
    if (show_progress) {
      show_message("✔️ Validating data structure and ranges...\n")
    }
    validation_result <- validate_heat_pulse_data(result)
    if (!validation_result$valid) {
      warning("Data validation issues found: ",
              paste(validation_result$issues, collapse = "; "))
    }
    result$validation <- validation_result
  }

  if (show_progress) {
    summary_msg <- sprintf("🎉 Import complete: %s pulses, %s measurements",
                          format(nrow(result$diagnostics), big.mark = ","),
                          format(nrow(result$measurements), big.mark = ","))

    if (nrow(result$measurements) > 0 && "datetime" %in% names(result$measurements)) {
      valid_times <- !is.na(result$measurements$datetime)
      if (any(valid_times)) {
        time_range <- range(result$measurements$datetime[valid_times])
        duration_days <- as.numeric(difftime(time_range[2], time_range[1], units = "days"))
        summary_msg <- sprintf("%s (%.1f days)\n", summary_msg, duration_days)
      } else {
        summary_msg <- paste0(summary_msg, "\n")
      }
    } else {
      summary_msg <- paste0(summary_msg, "\n")
    }

    show_message(summary_msg, type = "message")
  }

  return(result)
}

#' Format detection using first 3000 characters
#'
#' @param file_path Path to data file
#' @return Character string indicating format
#' @keywords internal
detect_format <- function(file_path) {

  # Read first 3000 characters
  #con <- file(file_path, "r")
  con <- file(file_path, "rb")
  on.exit(close(con))

  #sample_text <- readChar(con, nchars = 3000)

  # Read as raw bytes first
  raw_bytes <- readBin(con, "raw", n = 3000)

  # Convert to character
  sample_text <- rawToChar(raw_bytes)

  # Check if file is empty or contains only whitespace
  if (nchar(sample_text) == 0 || nchar(trimws(sample_text)) == 0) {
    stop("File appears to be empty")
  }

  # Check for CSV format (tab or comma delimited with headers)
  first_lines <- strsplit(sample_text, "\n")[[1]][1:5]
  if (length(first_lines) > 1) {
    header <- first_lines[1]
    if (grepl("Date.*Time.*Pulse_ID.*DO.*DI.*UO.*UI", header, ignore.case = TRUE) ||
        grepl("datetime.*pulse.*do.*di.*uo.*ui", header, ignore.case = TRUE) ||
        (grepl("\t", header) && grepl("(do|di|uo|ui)", header, ignore.case = TRUE))) {
      return("csv")
    }
  }

  # Check for ICT Legacy format (nested JSON with "Rdg" and "T" arrays)
  if (grepl('"Rdg".*\\[.*"k".*"v"', sample_text)) {
    return("ict_legacy")
  }

  # Check for ICT Current format (simpler JSON with direct fields)
  if (grepl('"date".*"bv".*"bc".*"bt"', sample_text) ||
      grepl('"date".*:\\s*"\\d{4}-\\d{2}-\\d{2}', sample_text)) {
    return("ict_current")
  }

  # Default to current if has JSON-like structure
  if (grepl('\\{.*"date"', sample_text)) {
    return("ict_current")
  }

  stop("Unable to detect format for file: ", basename(file_path))
}

#' Read ICT Current format using optimised reading strategy
#'
#' @param file_path Path to data file
#' @param chunk_size Characters per chunk (used only for files > 100MB)
#' @param show_progress Show progress updates
#' @param ... Additional arguments
#' @keywords internal
read_ict_current <- function(file_path, chunk_size, show_progress, ...) {

  file_size <- file.info(file_path)$size
  file_size_mb <- file_size / 1e6

  if (show_progress) {
    show_message("📖 Reading file...\n")
  }

  con <- file(file_path, "rb")
  on.exit(close(con))

  # Strategy: For files < 100MB, read entire file at once (much faster)
  # For larger files, use list accumulation to avoid repeated string concatenation
  if (file_size_mb < 100) {
    # Read entire file at once - fastest for typical sap flow data files
    # Create progress reporter for reading
    show_prog <- if (show_progress) {
      progressr::progressor(steps = 2)
    } else {
      NULL
    }

    if (show_progress && !is.null(show_prog)) {
      show_prog(amount = 1, message = sprintf("Reading file... %.1f MB", file_size_mb))
    }

    all_content <- rawToChar(readBin(con, "raw", n = file_size))

    if (show_progress && !is.null(show_prog)) {
      show_prog(amount = 1, message = "File reading complete")
    }

    if (show_progress) {
      show_message("✅ File reading complete\n")
    }

  } else {
    # For very large files, use list accumulation (avoids O(n²) string concatenation)
    if (show_progress) {
      show_message("📖 Reading large file in chunks...\n")
    }

    chunks <- list()
    chunk_count <- 0
    bytes_read <- 0

    # Create progress reporter
    show_prog <- if (show_progress) {
      progressr::progressor(steps = file_size)
    } else {
      NULL
    }

    while (TRUE) {
      chunk <- readBin(con, "raw", n = chunk_size)
      if (length(chunk) == 0) break

      chunk_count <- chunk_count + 1
      chunks[[chunk_count]] <- rawToChar(chunk)
      bytes_read <- bytes_read + length(chunk)

      if (show_progress) {
        show_prog(amount = length(chunk),
          message = sprintf("Reading... %.1f MB / %.1f MB", bytes_read / 1e6, file_size / 1e6))
      }
    }

    if (show_progress) {
      show_message("✅ File reading complete\n")
    }

    # Join all chunks once at the end - much faster than repeated paste0
    all_content <- paste(chunks, collapse = "")
  }

  if (show_progress) {
    show_message("🔧 Parsing data...\n")
  }

  # Ultra-fast parsing: extract all data at once using vectorized operations
  result <- parse_ict_current_vectorized(all_content, show_progress, file_size_mb)

  diagnostics <- result$diagnostics
  measurements <- result$measurements

  return(list(
    diagnostics = diagnostics,
    measurements = measurements
  ))
}

#' Parse ICT current format using ultra-fast vectorized approach
#'
#' @param content Full file content as single string
#' @param show_progress Show progress messages
#' @param file_size_mb File size in MB (for progress reporting)
#' @return List with diagnostics and measurements data frames
#' @keywords internal
parse_ict_current_vectorized <- function(content, show_progress, file_size_mb = 0) {

  # Extract all datetimes at once
  datetime_pattern <- '"date":"([^"]+)"'
  datetime_matches <- gregexpr(datetime_pattern, content, perl = TRUE)
  datetimes <- regmatches(content, datetime_matches)[[1]]
  datetimes <- gsub('"date":"', '', datetimes)
  datetimes <- gsub('"', '', datetimes)

  n_pulses <- length(datetimes)

  if (n_pulses == 0) {
    return(list(
      diagnostics = create_empty_diagnostics(),
      measurements = create_empty_measurements()
    ))
  }

  if (show_progress) {
    show_message(sprintf("Found %s pulses, parsing timestamps...\n",
                        format(n_pulses, big.mark = ",")))
  }

  # Parse datetimes vectorized
  pulse_times <- as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

  # Split into individual records for number extraction
  records <- strsplit(content, split = '\\{"date"')[[1]]
  if (length(records) > 0 && nchar(records[1]) < 10) {
    records <- records[-1]
  }

  if (show_progress) {
    show_message("Extracting measurements (C++ optimised)...\n")
  }

  # OPTIMISATION: Use C++ for fast batch parsing (6x faster than R loop)
  # This replaces the entire R loop that previously processed each pulse individually
  pulse_times_numeric <- as.numeric(pulse_times)

  # Progress reporter (simple progress for C++ batch operation)
  show_prog <- if (show_progress && n_pulses > 0) {
    progressr::progressor(steps = 1)
  } else {
    NULL
  }

  if (show_progress && !is.null(show_prog)) {
    show_prog(amount = 0,
              message = sprintf("Parsing %s pulses with C++...",
                               format(n_pulses, big.mark = ",")))
  }

  # Call C++ function to do all the heavy lifting
  parsed_data <- parse_ict_records_cpp(
    records = records,
    pulse_times = pulse_times_numeric,
    measurement_interval = 1,  # 1 second between measurements
    expected_diagnostics = 5
  )

  if (show_progress && !is.null(show_prog)) {
    show_prog(amount = 1,
              message = "Parsing complete!")
  }

  # C++ returns data frames directly - just need to fix datetime classes
  diagnostics <- parsed_data$diagnostics
  measurements <- parsed_data$measurements

  # Convert numeric datetimes back to POSIXct
  diagnostics$datetime <- as.POSIXct(diagnostics$datetime, origin = "1970-01-01", tz = "UTC")
  measurements$datetime <- as.POSIXct(measurements$datetime, origin = "1970-01-01", tz = "UTC")

  # Rename columns to match expected output
  names(diagnostics)[names(diagnostics) == "ext1"] <- "external_volt"
  names(diagnostics)[names(diagnostics) == "ext2"] <- "external_current"

  if (show_progress) {
    show_message(sprintf("Successfully parsed %s pulses with %s measurements\n",
                        format(nrow(diagnostics), big.mark = ","),
                        format(nrow(measurements), big.mark = ",")))
  }

  return(list(
    diagnostics = diagnostics,
    measurements = measurements
  ))
}

#' Read ICT Legacy format
#'
#' @param file_path Path to data file
#' @param chunk_size Characters per chunk
#' @param show_progress Show progress updates
#' @param ... Additional arguments
#' @keywords internal
read_ict_legacy <- function(file_path, chunk_size, show_progress, ...) {

  if (show_progress) {
    show_message("📖 Reading ICT legacy format...\n")
  }

  # Read file content
  con <- file(file_path, "rb")
  on.exit(close(con))

  file_content <- readBin(con, "raw", n = file.info(file_path)$size)
  file_content <- rawToChar(file_content)

  # Extract Rdg sections with nested structure
  # This format has "Rdg" arrays with key-value pairs and "T" arrays for temperatures

  # Find all Rdg blocks
  rdg_pattern <- '\\{"Rdg":\\[([^\\]]+)\\]'
  rdg_matches <- gregexpr(rdg_pattern, file_content, perl = TRUE)

  if (rdg_matches[[1]][1] == -1) {
    stop("No 'Rdg' sections found in legacy format")
  }

  # Extract and process each Rdg block
  # Note: This would need full implementation based on actual legacy format structure

  warning("Legacy format parsing needs full implementation for your specific format")

  return(list(
    diagnostics = create_empty_diagnostics(),
    measurements = create_empty_measurements()
  ))
}

#' Read CSV format (tab or comma delimited)
#'
#' @param file_path Path to data file
#' @param show_progress Show progress updates
#' @param ... Additional arguments
#' @keywords internal
read_csv_format <- function(file_path, show_progress, ...) {

  if (show_progress) {
    show_message("📖 Reading CSV/tab-delimited format...\n")
  }

  # Detect delimiter
  first_line <- readLines(file_path, n = 1)
  delimiter <- if (grepl("\t", first_line)) {
    "\t"
  } else {
    ","
  }

  if (show_progress) {
    show_message(sprintf("Detected delimiter: %s\n",
                        if(delimiter == "\t") "tab" else "comma"))
  }

  # Read the CSV file
  data <- readr::read_delim(
    file_path,
    delim = delimiter,
    show_col_types = FALSE,
    progress = show_progress
  )

  # Standardise column names
  names(data) <- tolower(names(data))
  names(data) <- gsub("\\.", "_", names(data))

  # Check for required columns
  required_cols <- c("do", "di", "uo", "ui")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle datetime column (various possible names)
  datetime_col <- NULL
  possible_datetime <- c("datetime", "date_time", "timestamp", "date", "time")

  for (col in possible_datetime) {
    if (col %in% names(data)) {
      datetime_col <- col
      break
    }
  }

  # If separate date and time columns, combine them
  if (is.null(datetime_col) && all(c("date", "time") %in% names(data))) {
    data$datetime <- as.POSIXct(paste(data$date, data$time))
    datetime_col <- "datetime"
  }

  if (is.null(datetime_col)) {
    stop("No datetime column found in CSV")
  }

  # Rename datetime column if needed
  if (datetime_col != "datetime") {
    names(data)[names(data) == datetime_col] <- "datetime"
  }

  # Ensure datetime is POSIXct
  if (!inherits(data$datetime, "POSIXct")) {
    data$datetime <- as.POSIXct(data$datetime)
  }

  # Add pulse_id if missing
  if (!"pulse_id" %in% names(data)) {
    # Assume new pulse every 30 minutes (1800 seconds)
    time_diffs <- c(0, diff(as.numeric(data$datetime)))
    data$pulse_id <- cumsum(time_diffs > 1800) + 1
  }

  # Separate diagnostics and measurements
  diagnostic_cols <- intersect(names(data),
                               c("pulse_id", "datetime", "batt_volt", "batt_v", "batteryvoltage",
                                 "batt_current", "batt_c", "batterycurrent",
                                 "batt_temp", "batt_t", "batterytemperature",
                                 "external_volt", "ext_v", "externalvoltage",
                                 "external_current", "ext_c", "externalcurrent"))

  measurement_cols <- c("pulse_id", "datetime", "do", "di", "uo", "ui")

  # Create diagnostics (one per pulse)
  if (length(diagnostic_cols) > 2) {
    diagnostics <- data %>%
      dplyr::select(dplyr::all_of(diagnostic_cols)) %>%
      dplyr::group_by(pulse_id) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Standardise column names
    names(diagnostics) <- gsub("batt_v$|batteryvoltage", "batt_volt", names(diagnostics))
    names(diagnostics) <- gsub("batt_c$|batterycurrent", "batt_current", names(diagnostics))
    names(diagnostics) <- gsub("batt_t$|batterytemperature", "batt_temp", names(diagnostics))
    names(diagnostics) <- gsub("ext_v$|externalvoltage", "external_volt", names(diagnostics))
    names(diagnostics) <- gsub("ext_c$|externalcurrent", "external_current", names(diagnostics))

  } else {
    # Create minimal diagnostics
    diagnostics <- data %>%
      dplyr::group_by(pulse_id) %>%
      dplyr::summarise(
        datetime = dplyr::first(datetime),
        .groups = "drop"
      )
  }

  # Create measurements
  measurements <- data %>%
    dplyr::select(dplyr::all_of(measurement_cols))

  return(list(
    diagnostics = as.data.frame(diagnostics),
    measurements = as.data.frame(measurements)
  ))
}

#' Create empty diagnostics data frame
#' @return Empty data frame with correct structure
#' @keywords internal
create_empty_diagnostics <- function() {
  data.frame(
    pulse_id = integer(0),
    datetime = as.POSIXct(character(0)),
    batt_volt = numeric(0),
    batt_current = numeric(0),
    batt_temp = numeric(0),
    external_volt = numeric(0),
    external_current = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Create empty measurements data frame
#' @return Empty data frame with correct structure
#' @keywords internal
create_empty_measurements <- function() {
  data.frame(
    pulse_id = integer(0),
    datetime = as.POSIXct(character(0)),
    do = numeric(0),
    di = numeric(0),
    uo = numeric(0),
    ui = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Read Multiple Heat Pulse Data Files
#'
#' Imports and combines raw heat pulse temperature data from multiple files, typically representing
#' different trees or sensors. Each file is processed individually and then combined
#' with tree/sensor identification for comparative analysis.
#'
#' @param file_paths Character vector of file paths to import
#' @param tree_ids Character vector of tree/sensor identifiers. If NULL, derived from filenames
#' @param format Character string specifying format. Auto-detected if NULL
#' @param validate_data Logical indicating whether to validate imported data (default: TRUE)
#' @param chunk_size Integer specifying characters per chunk (default: auto-sized)
#' @param show_progress Logical indicating whether to show progress (default: TRUE)
#' @param combine_data Logical indicating whether to combine all data into single object (default: TRUE)
#' @param ... Additional arguments passed to read_sap_data()
#'
#' @return If combine_data = TRUE, a list containing:
#'   \item{diagnostics}{Combined data frame with sensor diagnostics and tree_id}
#'   \item{measurements}{Combined data frame with temperature measurements and tree_id}
#'   \item{metadata}{List containing combined file information}
#'   \item{individual_data}{List of individual heat_pulse_data objects for each file}
#'   \item{tree_summary}{Data frame summarizing each tree's data}
#'
#'   If combine_data = FALSE, a list of individual heat_pulse_data objects named by tree_id
#'
#' @details
#' This function enables comparative analysis across multiple trees by:
#' 1. Importing each file individually using read_heat_pulse_data()
#' 2. Adding tree/sensor identification to all data
#' 3. Optionally combining data for unified analysis
#' 4. Providing summary statistics for each tree
#'
#' Tree IDs can be:
#' - Explicitly provided via tree_ids parameter
#' - Derived from filenames (removes path and extension)
#' - Auto-generated as "Tree_1", "Tree_2", etc.
#'
#' @examples
#' \dontrun{
#' # Import multiple files with automatic tree identification (console)
#' files <- c("tree1_data.txt", "tree2_data.txt", "tree3_data.txt")
#' progressr::with_progress({
#'   multi_data <- read_multiple_heat_pulse_data(files)
#' })
#'
#' # Import with explicit tree names
#' progressr::with_progress({
#'   multi_data <- read_multiple_heat_pulse_data(
#'     files,
#'     tree_ids = c("Oak_1", "Oak_2", "Pine_1")
#'   )
#' })
#'
#' # Usage in Shiny application
#' server <- function(input, output, session) {
#'   observeEvent(input$import_multiple_button, {
#'     file_paths <- input$files$datapath
#'     progressr::with_progress({
#'       multi_data <- read_multiple_heat_pulse_data(file_paths)
#'     })
#'   })
#' }
#'
#' # Keep individual data objects for separate processing
#' individual_data <- read_multiple_heat_pulse_data(files, combine_data = FALSE)
#'
#' # Process combined data
#' results <- process_heat_pulse_data(multi_data)
#' }
#'
#' @seealso \code{\link{read_heat_pulse_data}}
#' @export
read_multiple_heat_pulse_data <- function(file_paths,
                                   tree_ids = NULL,
                                   format = NULL,
                                   validate_data = TRUE,
                                   chunk_size = NULL,
                                   show_progress = TRUE,
                                   combine_data = TRUE,
                                   ...) {

  # Input validation
  if (!is.character(file_paths) || length(file_paths) == 0) {
    stop("file_paths must be a non-empty character vector")
  }

  # Check all files exist
  missing_files <- file_paths[!file.exists(file_paths)]
  if (length(missing_files) > 0) {
    stop("Files not found: ", paste(missing_files, collapse = ", "))
  }

  # Generate tree IDs if not provided
  if (is.null(tree_ids)) {
    tree_ids <- generate_tree_ids(file_paths)
  } else {
    if (length(tree_ids) != length(file_paths)) {
      stop("tree_ids length (", length(tree_ids),
           ") must match file_paths length (", length(file_paths), ")")
    }
  }

  if (show_progress) {
    show_message(sprintf("🌳 Importing %d heat pulse data files...\n", length(file_paths)))
    show_message(sprintf("📁 Files: %s\n", paste(basename(file_paths), collapse = ", ")))
    show_message(sprintf("🏷️  Tree IDs: %s\n\n", paste(tree_ids, collapse = ", ")))
  }

  # Import each file individually
  individual_data <- list()
  import_summary <- data.frame(
    tree_id = tree_ids,
    file_path = file_paths,
    file_name = basename(file_paths),
    n_pulses = integer(length(file_paths)),
    n_measurements = integer(length(file_paths)),
    file_size_mb = numeric(length(file_paths)),
    import_success = logical(length(file_paths)),
    import_errors = character(length(file_paths)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(file_paths)) {
    if (show_progress) {
      show_message(sprintf("📊 [%d/%d] Importing %s (%s)...\n",
                          i, length(file_paths), tree_ids[i], basename(file_paths[i])))
    }

    tryCatch({
      # Import individual file
      heat_pulse_data <- read_heat_pulse_data(
        file_path = file_paths[i],
        format = format,
        validate_data = validate_data,
        chunk_size = chunk_size,
        show_progress = FALSE,  # Suppress individual progress
        ...
      )

      # Add tree identification to data
      heat_pulse_data$diagnostics$tree_id <- tree_ids[i]
      heat_pulse_data$measurements$tree_id <- tree_ids[i]

      # Update metadata
      heat_pulse_data$metadata$tree_id <- tree_ids[i]
      heat_pulse_data$metadata$file_index <- i

      # Store individual data
      individual_data[[tree_ids[i]]] <- heat_pulse_data

      # Update summary
      import_summary$n_pulses[i] <- nrow(heat_pulse_data$diagnostics)
      import_summary$n_measurements[i] <- nrow(heat_pulse_data$measurements)
      import_summary$file_size_mb[i] <- heat_pulse_data$metadata$file_size_mb
      import_summary$import_success[i] <- TRUE

      if (show_progress) {
        show_message(sprintf("   ✅ Success: %s pulses, %s measurements\n",
                            format(nrow(heat_pulse_data$diagnostics), big.mark = ","),
                            format(nrow(heat_pulse_data$measurements), big.mark = ",")))
      }

    }, error = function(e) {
      # Handle import errors
      import_summary$import_success[i] <- FALSE
      import_summary$import_errors[i] <- as.character(e$message)

      if (show_progress) {
        show_message(sprintf("   ❌ Error: %s\n", e$message), type = "error")
      }

      # Create empty data structure for failed imports
      individual_data[[tree_ids[i]]] <- create_empty_heat_pulse_data(tree_ids[i], file_paths[i])
    })
  }

  if (show_progress) {
    successful_imports <- sum(import_summary$import_success)
    show_message(sprintf("\n🎉 Import complete: %d/%d files successful\n",
                        successful_imports, length(file_paths)))
  }

  # Return format based on combine_data parameter
  if (combine_data) {
    return(combine_multiple_heat_pulse_data(individual_data, import_summary, show_progress))
  } else {
    # Add summary as attribute
    attr(individual_data, "import_summary") <- import_summary
    class(individual_data) <- c("multiple_heat_pulse_data", "list")
    return(individual_data)
  }
}

#' Generate tree IDs from file paths
#' @param file_paths Character vector of file paths
#' @return Character vector of tree IDs
#' @keywords internal
generate_tree_ids <- function(file_paths) {
  # Extract filename without path and extension
  base_names <- tools::file_path_sans_ext(basename(file_paths))

  # Clean up names (remove common prefixes/suffixes)
  clean_names <- gsub("^(tree|sensor|data|sap)_?", "", base_names, ignore.case = TRUE)
  clean_names <- gsub("_(tree|sensor|data|sap)$", "", clean_names, ignore.case = TRUE)
  clean_names <- gsub("[-_\\.]", "_", clean_names)

  # Ensure uniqueness
  if (any(duplicated(clean_names))) {
    clean_names <- make.unique(clean_names, sep = "_")
  }

  # Fallback to generic names if needed
  if (any(nchar(clean_names) == 0)) {
    empty_idx <- which(nchar(clean_names) == 0)
    clean_names[empty_idx] <- paste0("Tree_", empty_idx)
  }

  return(clean_names)
}

#' Combine multiple heat_pulse_data objects into unified structure
#' @param individual_data List of heat_pulse_data objects
#' @param import_summary Data frame with import summary
#' @param show_progress Logical, whether to show progress
#' @return Combined heat_pulse_data object
#' @keywords internal
combine_multiple_heat_pulse_data <- function(individual_data, import_summary, show_progress) {

  if (show_progress) {
    show_message("🔄 Combining data from multiple trees...\n")
  }

  # Filter successful imports
  successful_data <- individual_data[import_summary$import_success]

  if (length(successful_data) == 0) {
    stop("No files were successfully imported")
  }

  # Combine diagnostics
  all_diagnostics <- lapply(successful_data, function(x) x$diagnostics)
  combined_diagnostics <- dplyr::bind_rows(all_diagnostics)

  # Combine measurements
  all_measurements <- lapply(successful_data, function(x) x$measurements)
  combined_measurements <- dplyr::bind_rows(all_measurements)

  # Create combined metadata
  combined_metadata <- list(
    n_trees = length(successful_data),
    tree_ids = names(successful_data),
    total_pulses = nrow(combined_diagnostics),
    total_measurements = nrow(combined_measurements),
    import_time = Sys.time(),
    individual_metadata = lapply(successful_data, function(x) x$metadata),
    r_version = R.version.string,
    package_version = tryCatch(utils::packageVersion("sapFluxR"),
                              error = function(e) "dev")
  )

  # Create tree summary
  tree_summary <- import_summary %>%
    dplyr::filter(import_success) %>%
    dplyr::select(tree_id, file_name, n_pulses, n_measurements, file_size_mb) %>%
    dplyr::mutate(
      duration_days = NA_real_,
      mean_pulses_per_day = NA_real_
    )

  # Calculate duration and pulse frequency for each tree
  for (i in seq_len(nrow(tree_summary))) {
    tree_id <- tree_summary$tree_id[i]
    tree_measurements <- combined_measurements[combined_measurements$tree_id == tree_id, ]

    if (nrow(tree_measurements) > 0 && "datetime" %in% names(tree_measurements)) {
      valid_times <- !is.na(tree_measurements$datetime)
      if (any(valid_times)) {
        time_range <- range(tree_measurements$datetime[valid_times])
        tree_summary$duration_days[i] <- as.numeric(difftime(time_range[2], time_range[1], units = "days"))
        tree_summary$mean_pulses_per_day[i] <- tree_summary$n_pulses[i] / max(tree_summary$duration_days[i], 1)
      }
    }
  }

  # Create combined validation
  combined_validation <- list(
    valid = all(vapply(successful_data, function(x) {
      # Safely extract validation$valid, defaulting to FALSE if missing
      if (is.null(x$validation) || is.null(x$validation$valid)) {
        return(FALSE)
      }
      return(isTRUE(x$validation$valid))
    }, FUN.VALUE = logical(1))),
    issues = unique(unlist(lapply(successful_data, function(x) {
      # Safely extract issues
      if (is.null(x$validation) || is.null(x$validation$issues)) {
        return(character(0))
      }
      return(as.character(x$validation$issues))
    }))),
    tree_validations = lapply(successful_data, function(x) x$validation)
  )

  # Create result object
  result <- list(
    diagnostics = combined_diagnostics,
    measurements = combined_measurements,
    metadata = combined_metadata,
    validation = combined_validation,
    individual_data = individual_data,
    tree_summary = tree_summary,
    import_summary = import_summary
  )

  class(result) <- c("multiple_heat_pulse_data", "heat_pulse_data", "list")

  if (show_progress) {
    show_message(sprintf("✅ Combined: %d trees, %s total pulses, %s total measurements\n",
                        length(successful_data),
                        format(nrow(combined_diagnostics), big.mark = ","),
                        format(nrow(combined_measurements), big.mark = ",")))
  }

  return(result)
}

#' Create empty heat_pulse_data object for failed imports
#' @param tree_id Character, tree identifier
#' @param file_path Character, file path
#' @return Empty heat_pulse_data object
#' @keywords internal
create_empty_heat_pulse_data <- function(tree_id, file_path) {
  result <- list(
    diagnostics = create_empty_diagnostics(),
    measurements = create_empty_measurements(),
    metadata = list(
      tree_id = tree_id,
      file_path = file_path,
      file_name = basename(file_path),
      format = "unknown",
      import_time = Sys.time(),
      file_size = 0,
      file_size_mb = 0,
      n_pulses = 0,
      n_measurements = 0,
      import_error = TRUE
    ),
    validation = list(
      valid = FALSE,
      issues = "Import failed"
    )
  )

  class(result) <- c("heat_pulse_data", "list")
  return(result)
}


#' Fix Clock Drift in Device-Collected Data
#'
#' Corrects clock drift in device-collected data by applying a linear correction
#' based on a single calibration point observed when the device is connected to
#' a laptop or known accurate time source.
#'
#' @details
#' This function assumes that:
#' \itemize{
#'   \item The device clock was accurate at the start of the dataset
#'   \item Clock drift accumulated linearly over time
#'   \item A single calibration point is available (observed when connecting device)
#' }
#'
#' The correction is calculated as:
#' \deqn{corrected\_time = device\_time + (drift \times proportion\_elapsed)}
#'
#' Where \code{drift} is the total drift at calibration, and \code{proportion_elapsed}
#' is the fraction of time elapsed from dataset start to each measurement.
#'
#' **Important:** Original device timestamps are preserved in a new column called
#' \code{device_datetime} so you can always revert the correction if needed.
#'
#' @param data A heat_pulse_data object from read_heat_pulse_data() or a data frame containing the raw data
#' @param device_time_col Character string naming the column containing device
#'   timestamps (default: "datetime")
#' @param observed_device_time POSIXct timestamp showing what the device clock
#'   displayed at calibration point
#' @param observed_actual_time POSIXct timestamp showing the actual correct time
#'   at calibration point
#'
#' @return The input data frame with:
#'   \itemize{
#'     \item Original timestamps preserved in \code{device_datetime} column
#'     \item Corrected timestamps in the original \code{device_time_col} column
#'     \item Correction metadata stored as attributes on the corrected column
#'   }
#'
#' @section Attributes:
#' The corrected timestamp column will have the following attributes:
#' \describe{
#'   \item{drift_corrected}{Logical TRUE indicating correction was applied}
#'   \item{total_drift_seconds}{Numeric total drift in seconds at calibration}
#'   \item{calibration_date}{POSIXct actual time at calibration}
#' }
#'
#' @section Warnings:
#' \itemize{
#'   \item If correction has already been applied, the function returns data
#'         unchanged with a warning
#'   \item If any timestamps are later than the calibration point, a warning is
#'         issued that correction is being extrapolated
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Correct clock drift in heat_pulse_data object
#' heat_pulse_data <- read_heat_pulse_data("data.txt")
#'
#' # When device was retrieved, it showed 2025-01-16 08:05:00
#' # but actual time was 2025-01-16 08:00:00 (5 min drift)
#' observed_device <- as.POSIXct("2025-01-16 08:05:00", tz = "UTC")
#' observed_actual <- as.POSIXct("2025-01-16 08:00:00", tz = "UTC")
#'
#' corrected_data <- fix_clock_drift(
#'   data = heat_pulse_data,
#'   observed_device_time = observed_device,
#'   observed_actual_time = observed_actual
#' )
#'
#' # Now you can use corrected data with calc_heat_pulse_velocity()
#' results <- calc_heat_pulse_velocity(corrected_data)
#'
#' # Example 2: Correct clock drift in a data frame
#' # Device clock was 5 minutes fast by the end of a 24-hour period
#' start_time <- as.POSIXct("2025-01-15 08:00:00", tz = "UTC")
#' device_times <- seq(start_time, by = "30 min", length.out = 48)
#'
#' # Add simulated drift (5 minutes over 24 hours)
#' drift_per_hour <- 5 / 24  # minutes per hour
#' hours_elapsed <- seq(0, 23.5, by = 0.5)
#' drift_minutes <- hours_elapsed * drift_per_hour
#' device_times_with_drift <- device_times + drift_minutes * 60  # Convert to seconds
#'
#' example_data <- data.frame(
#'   datetime = device_times_with_drift,
#'   temperature = rnorm(48, mean = 25, sd = 2)
#' )
#'
#' # At 24 hours, we connected the device and observed:
#' # Device showed: 2025-01-16 08:05:00
#' # Actual time was: 2025-01-16 08:00:00
#' observed_device <- as.POSIXct("2025-01-16 08:05:00", tz = "UTC")
#' observed_actual <- as.POSIXct("2025-01-16 08:00:00", tz = "UTC")
#'
#' # Apply correction
#' corrected_data <- fix_clock_drift(
#'   data = example_data,
#'   device_time_col = "datetime",
#'   observed_device_time = observed_device,
#'   observed_actual_time = observed_actual
#' )
#'
#' # Check correction
#' head(corrected_data)
#' attributes(corrected_data$datetime)
#'
#' # Original device times are preserved
#' head(corrected_data$device_datetime)
#' }
#'
#' @family data preprocessing functions
#' @export
fix_clock_drift <- function(data,
                            device_time_col = "datetime",
                            observed_device_time,
                            observed_actual_time) {

  # Handle heat_pulse_data objects
  if (inherits(data, "heat_pulse_data")) {
    # Apply correction to measurements
    if (!is.null(data$measurements) && nrow(data$measurements) > 0) {
      data$measurements <- fix_clock_drift(
        data = data$measurements,
        device_time_col = device_time_col,
        observed_device_time = observed_device_time,
        observed_actual_time = observed_actual_time
      )
    }

    # Apply correction to diagnostics
    if (!is.null(data$diagnostics) && nrow(data$diagnostics) > 0) {
      data$diagnostics <- fix_clock_drift(
        data = data$diagnostics,
        device_time_col = device_time_col,
        observed_device_time = observed_device_time,
        observed_actual_time = observed_actual_time
      )
    }

    # Update metadata to indicate correction was applied
    if (!is.null(data$metadata)) {
      data$metadata$clock_drift_corrected <- TRUE
      data$metadata$drift_correction_time <- Sys.time()
    }

    # Ensure class is preserved (especially for multiple_heat_pulse_data)
    original_class <- class(data)
    class(data) <- original_class

    return(data)
  }

  # Input validation for data frames
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or heat_pulse_data object")
  }

  if (!is.character(device_time_col) || length(device_time_col) != 1) {
    stop("'device_time_col' must be a single character string")
  }

  if (!device_time_col %in% names(data)) {
    stop("Column '", device_time_col, "' not found in data")
  }

  if (!inherits(observed_device_time, "POSIXct")) {
    stop("'observed_device_time' must be a POSIXct object")
  }

  if (!inherits(observed_actual_time, "POSIXct")) {
    stop("'observed_actual_time' must be a POSIXct object")
  }

  # Check if correction already applied
  has_device_datetime <- "device_datetime" %in% names(data)
  has_drift_attr <- !is.null(attr(data[[device_time_col]], "drift_corrected"))

  if (has_device_datetime && has_drift_attr) {
    warning(
      "Clock drift correction appears to have already been applied to this dataset. ",
      "Original timestamps are in 'device_datetime' column."
    )
    return(data)
  }

  # Convert device_time_col to POSIXct if needed
  if (!inherits(data[[device_time_col]], "POSIXct")) {
    data[[device_time_col]] <- as.POSIXct(data[[device_time_col]])
  }

  # Preserve original timestamps
  data$device_datetime <- data[[device_time_col]]

  # Get dataset start time (assume device was accurate at start)
  dataset_start <- min(data[[device_time_col]], na.rm = TRUE)

  # Calculate total drift at calibration point
  total_drift_seconds <- as.numeric(
    difftime(observed_actual_time, observed_device_time, units = "secs")
  )

  # Calculate time span from start to calibration
  total_time_span <- as.numeric(
    difftime(observed_device_time, dataset_start, units = "secs")
  )

  if (total_time_span <= 0) {
    stop(
      "Calibration point must be after the dataset start time. ",
      "Dataset start: ", format(dataset_start), ", ",
      "Calibration: ", format(observed_device_time)
    )
  }

  # Check for extrapolation beyond calibration point
  max_device_time <- max(data[[device_time_col]], na.rm = TRUE)
  if (max_device_time > observed_device_time) {
    n_extrapolated <- sum(data[[device_time_col]] > observed_device_time, na.rm = TRUE)
    warning(
      "Clock drift correction is being extrapolated beyond the calibration point. ",
      n_extrapolated, " timestamp(s) are later than the calibration point. ",
      "Correction accuracy may be reduced for these points."
    )
  }

  # Calculate proportional correction for each timestamp
  time_from_start <- as.numeric(
    difftime(data[[device_time_col]], dataset_start, units = "secs")
  )

  proportion_elapsed <- time_from_start / total_time_span

  # Apply linear drift correction
  correction_seconds <- total_drift_seconds * proportion_elapsed

  # Create corrected timestamps
  corrected_times <- data[[device_time_col]] + correction_seconds

  # Replace original column with corrected times
  data[[device_time_col]] <- corrected_times

  # Add metadata attributes to corrected column
  attr(data[[device_time_col]], "drift_corrected") <- TRUE
  attr(data[[device_time_col]], "total_drift_seconds") <- total_drift_seconds
  attr(data[[device_time_col]], "calibration_date") <- observed_actual_time

  return(data)
}
