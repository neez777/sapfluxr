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
      clean_msg <- gsub("[\U0001F300-\U0001F9FF]", "", msg)
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
#' Read Sap Flow Data from ICT SFM1x Sensors
#'
#' Imports and parses sap flow data from ICT SFM1x sensors using character-based chunking.
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
#'   sap_data <- read_sap_data("data/tree1_data.txt")
#' })
#'
#' # Usage in Shiny application - notifications appear automatically
#' server <- function(input, output, session) {
#'   observeEvent(input$import_button, {
#'     # Status messages will show as Shiny notifications
#'     # Progress bar updates automatically
#'     progressr::with_progress({
#'       sap_data <- read_sap_data(input$file$datapath)
#'     })
#'   })
#' }
#'
#' # Without progress reporting
#' sap_data <- read_sap_data("data/tree1_data.txt", show_progress = FALSE)
#'
#' # Specify format explicitly
#' sap_data <- read_sap_data("data/tree1_data.txt", format = "ict_current")
#' }
#'
#' @seealso \code{\link{read_multiple_sap_data}} for importing multiple files
#' @export
read_sap_data <- function(file_path,
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
    show_message(sprintf("📊 Reading sap flow data: %s (%.1f MB)\n",
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

  class(result) <- c("sap_data", "list")

  # Validate if requested
  if (validate_data) {
    if (show_progress) {
      show_message("✔️ Validating data structure and ranges...\n")
    }
    validation_result <- validate_sap_data(result)
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
    show_message("Extracting measurements (optimised)...\n")
  }

  # OPTIMIZATION: Pre-allocate diagnostic vectors instead of list-of-lists
  diag_pulse_id <- integer(n_pulses)
  diag_datetime <- rep(as.POSIXct(NA), n_pulses)
  diag_batt_volt <- numeric(n_pulses)
  diag_batt_current <- numeric(n_pulses)
  diag_batt_temp <- numeric(n_pulses)
  diag_external_volt <- numeric(n_pulses)
  diag_external_current <- numeric(n_pulses)

  # Pre-allocate measurement list
  meas_list <- vector("list", n_pulses)

  # Pre-compile regex for numbers (use base R gregexpr for better performance)
  num_pattern <- "[-+]?\\d+\\.\\d+"

  # Progress reporter for all files (always create if show_progress = TRUE)
  show_prog <- if (show_progress && n_pulses > 0) {
    progressr::progressor(steps = n_pulses)
  } else {
    NULL
  }

  # Determine update frequency based on file size
  # Small files: update more frequently for better feedback
  # Large files: update less frequently for better performance
  update_interval <- if (n_pulses < 50) {
    5   # Update every 5 pulses for tiny files
  } else if (n_pulses < 100) {
    10  # Update every 10 pulses for small files
  } else if (n_pulses < 1000) {
    25  # Update every 25 pulses for medium files
  } else if (n_pulses < 10000) {
    100  # Update every 100 pulses for large files
  } else {
    500  # Update every 500 pulses for very large files
  }

  # OPTIMIZATION: Use base R regex instead of stringr (faster for this use case)
  last_reported <- 0  # Track last reported position for accurate progress updates

  for (i in seq_len(n_pulses)) {

    # Update progress at appropriate intervals
    if (show_progress && !is.null(show_prog) && (i %% update_interval == 0 || i == n_pulses)) {
      # Calculate actual amount processed since last update
      amount_to_report <- i - last_reported
      show_prog(amount = amount_to_report,
        message = sprintf("Parsing pulse %s / %s (%.0f%%)",
                         format(i, big.mark = ","),
                         format(n_pulses, big.mark = ","),
                         100 * i / n_pulses))
      last_reported <- i
    }

    # Extract all numbers from this record using base R (faster than stringr)
    matches <- gregexpr(num_pattern, records[i], perl = TRUE)
    all_numbers_raw <- regmatches(records[i], matches)[[1]]

    if (length(all_numbers_raw) < 5) next

    all_numbers <- as.numeric(all_numbers_raw)

    # First 5 are diagnostics - store directly in vectors
    diag_pulse_id[i] <- i
    diag_datetime[i] <- pulse_times[i]
    diag_batt_volt[i] <- all_numbers[1]
    diag_batt_current[i] <- all_numbers[2]
    diag_batt_temp[i] <- all_numbers[3]
    diag_external_volt[i] <- if(length(all_numbers) >= 4) all_numbers[4] else NA_real_
    diag_external_current[i] <- if(length(all_numbers) >= 5) all_numbers[5] else NA_real_

    # Remaining are temperature measurements
    temp_numbers <- all_numbers[-(1:5)]

    if (length(temp_numbers) >= 4) {
      n_measurements <- length(temp_numbers) %/% 4
      idx_base <- seq(1, length(temp_numbers) - 3, by = 4)

      meas_list[[i]] <- data.frame(
        pulse_id = i,
        datetime = pulse_times[i] + seq(0, n_measurements - 1),
        do = temp_numbers[idx_base],
        di = temp_numbers[idx_base + 1],
        uo = temp_numbers[idx_base + 2],
        ui = temp_numbers[idx_base + 3],
        stringsAsFactors = FALSE
      )
    }
  }

  if (show_progress) {
    show_message("Combining results...\n")
  }

  # OPTIMIZATION: Create diagnostics from pre-allocated vectors (much faster)
  diagnostics <- data.frame(
    pulse_id = diag_pulse_id,
    datetime = diag_datetime,
    batt_volt = diag_batt_volt,
    batt_current = diag_batt_current,
    batt_temp = diag_batt_temp,
    external_volt = diag_external_volt,
    external_current = diag_external_current,
    stringsAsFactors = FALSE
  )

  # Remove NA rows (where pulse_id is 0)
  diagnostics <- diagnostics[diag_pulse_id > 0, ]

  # Combine measurements - use data.table::rbindlist if available (faster than dplyr)
  if (requireNamespace("data.table", quietly = TRUE)) {
    measurements <- data.table::rbindlist(meas_list)
    measurements <- as.data.frame(measurements)
  } else {
    measurements <- dplyr::bind_rows(meas_list)
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

#' Read Multiple Sap Flow Data Files
#'
#' Imports and combines sap flow data from multiple files, typically representing
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
#'   \item{individual_data}{List of individual sap_data objects for each file}
#'   \item{tree_summary}{Data frame summarizing each tree's data}
#'
#'   If combine_data = FALSE, a list of individual sap_data objects named by tree_id
#'
#' @details
#' This function enables comparative analysis across multiple trees by:
#' 1. Importing each file individually using read_sap_data()
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
#'   multi_data <- read_multiple_sap_data(files)
#' })
#'
#' # Import with explicit tree names
#' progressr::with_progress({
#'   multi_data <- read_multiple_sap_data(
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
#'       multi_data <- read_multiple_sap_data(file_paths)
#'     })
#'   })
#' }
#'
#' # Keep individual data objects for separate processing
#' individual_data <- read_multiple_sap_data(files, combine_data = FALSE)
#'
#' # Process combined data
#' results <- process_sap_data(multi_data)
#' }
#'
#' @seealso \code{\link{read_sap_data}}, \code{\link{process_sap_data}}
#' @export
read_multiple_sap_data <- function(file_paths,
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
    show_message(sprintf("🌳 Importing %d sap flow data files...\n", length(file_paths)))
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
      sap_data <- read_sap_data(
        file_path = file_paths[i],
        format = format,
        validate_data = validate_data,
        chunk_size = chunk_size,
        show_progress = FALSE,  # Suppress individual progress
        ...
      )

      # Add tree identification to data
      sap_data$diagnostics$tree_id <- tree_ids[i]
      sap_data$measurements$tree_id <- tree_ids[i]

      # Update metadata
      sap_data$metadata$tree_id <- tree_ids[i]
      sap_data$metadata$file_index <- i

      # Store individual data
      individual_data[[tree_ids[i]]] <- sap_data

      # Update summary
      import_summary$n_pulses[i] <- nrow(sap_data$diagnostics)
      import_summary$n_measurements[i] <- nrow(sap_data$measurements)
      import_summary$file_size_mb[i] <- sap_data$metadata$file_size_mb
      import_summary$import_success[i] <- TRUE

      if (show_progress) {
        show_message(sprintf("   ✅ Success: %s pulses, %s measurements\n",
                            format(nrow(sap_data$diagnostics), big.mark = ","),
                            format(nrow(sap_data$measurements), big.mark = ",")))
      }

    }, error = function(e) {
      # Handle import errors
      import_summary$import_success[i] <- FALSE
      import_summary$import_errors[i] <- as.character(e$message)

      if (show_progress) {
        show_message(sprintf("   ❌ Error: %s\n", e$message), type = "error")
      }

      # Create empty data structure for failed imports
      individual_data[[tree_ids[i]]] <- create_empty_sap_data(tree_ids[i], file_paths[i])
    })
  }

  if (show_progress) {
    successful_imports <- sum(import_summary$import_success)
    show_message(sprintf("\n🎉 Import complete: %d/%d files successful\n",
                        successful_imports, length(file_paths)))
  }

  # Return format based on combine_data parameter
  if (combine_data) {
    return(combine_multiple_sap_data(individual_data, import_summary, show_progress))
  } else {
    # Add summary as attribute
    attr(individual_data, "import_summary") <- import_summary
    class(individual_data) <- c("multiple_sap_data", "list")
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

#' Combine multiple sap_data objects into unified structure
#' @param individual_data List of sap_data objects
#' @param import_summary Data frame with import summary
#' @param show_progress Logical, whether to show progress
#' @return Combined sap_data object
#' @keywords internal
combine_multiple_sap_data <- function(individual_data, import_summary, show_progress) {

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

  class(result) <- c("multiple_sap_data", "sap_data", "list")

  if (show_progress) {
    show_message(sprintf("✅ Combined: %d trees, %s total pulses, %s total measurements\n",
                        length(successful_data),
                        format(nrow(combined_diagnostics), big.mark = ","),
                        format(nrow(combined_measurements), big.mark = ",")))
  }

  return(result)
}

#' Create empty sap_data object for failed imports
#' @param tree_id Character, tree identifier
#' @param file_path Character, file path
#' @return Empty sap_data object
#' @keywords internal
create_empty_sap_data <- function(tree_id, file_path) {
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

  class(result) <- c("sap_data", "list")
  return(result)
}
