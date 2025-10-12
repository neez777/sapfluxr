# R/export_reporting.R
# Chat 9: Comprehensive Export and Reporting Functions
# sapFluxR Package Development

#' @name export_reporting
#' @title Comprehensive Export and Reporting Functions
#' @description
#' Advanced export and reporting capabilities for sapFluxR package including
#' multiple output formats, automated report generation, and batch processing.
NULL

#' Export Sap Flow Data to Multiple Formats
#'
#' @description
#' Comprehensive export function supporting multiple output formats including
#' CSV, Excel, JSON, and standardised research formats. Preserves metadata
#' and provides flexible data selection options.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param vh_results Optional, vh_results object from calc_heat_pulse_velocity()
#' @param file_path Character string specifying output file path (extension determines format)
#' @param format Character string specifying export format: "csv", "xlsx", "json", "txt", "rds", "research_standard"
#' @param include_metadata Logical, whether to include metadata in export (default: TRUE)
#' @param include_diagnostics Logical, whether to include diagnostics data (default: TRUE)
#' @param include_quality_flags Logical, whether to include quality flags (default: TRUE)
#' @param filter_options List of filtering options (methods, quality_flags, date_range, etc.)
#' @param compression Logical, whether to compress output files (default: FALSE)
#' @param overwrite Logical, whether to overwrite existing files (default: FALSE)
#'
#' @return List containing:
#'   \describe{
#'     \item{export_path}{Full path to exported file}
#'     \item{export_summary}{Summary of exported data}
#'     \item{metadata}{Preserved metadata information}
#'     \item{export_log}{Export processing log}
#'   }
#'
#' @details
#' Supported formats:
#' - **CSV**: Comma-separated values with metadata header
#' - **Excel**: Multi-sheet workbook with data, metadata, and summary sheets
#' - **JSON**: Structured JSON with full data preservation
#' - **TXT**: Tab-delimited text file
#' - **RDS**: Native R data format (preserves all object attributes)
#' - **Research Standard**: Standardised format for scientific publications
#'
#' @examples
#' \dontrun{
#' # Export to Excel with all data
#' export_result <- export_sap_data(
#'   sap_data, vh_results,
#'   "sap_flow_analysis.xlsx",
#'   format = "xlsx"
#' )
#'
#' # Export filtered CSV
#' export_result <- export_sap_data(
#'   sap_data, vh_results,
#'   "filtered_data.csv",
#'   filter_options = list(
#'     quality_flags = "OK",
#'     methods = c("HRM", "MHR"),
#'     date_range = c("2024-01-01", "2024-12-31")
#'   )
#' )
#' }
#'
#' @export
export_sap_data <- function(sap_data, vh_results = NULL, file_path,
                            format = NULL, include_metadata = TRUE,
                            include_diagnostics = TRUE, include_quality_flags = TRUE,
                            filter_options = NULL, compression = FALSE,
                            overwrite = FALSE) {

  # Validate inputs
  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  if (!is.null(vh_results) && !inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object")
  }

  # Determine format from file extension if not specified
  if (is.null(format)) {
    format <- tools::file_ext(file_path)
    if (format == "") {
      stop("Cannot determine format from file_path. Please specify format parameter.")
    }
  }

  # Check if file exists and handle overwrite
  if (file.exists(file_path) && !overwrite) {
    stop("File already exists. Set overwrite = TRUE to replace it.")
  }

  # Apply filters if specified
  if (!is.null(filter_options)) {
    if (!is.null(vh_results)) {
      vh_results <- apply_export_filters(vh_results, filter_options)
    }
    sap_data <- apply_export_filters_sap_data(sap_data, filter_options)
  }

  # Prepare export data
  export_data <- prepare_export_data(
    sap_data, vh_results, include_metadata,
    include_diagnostics, include_quality_flags
  )

  # Export based on format
  export_result <- switch(
    tolower(format),
    "csv" = export_to_csv(export_data, file_path, compression),
    "xlsx" = export_to_excel(export_data, file_path),
    "json" = export_to_json(export_data, file_path, compression),
    "txt" = export_to_txt(export_data, file_path, compression),
    "rds" = export_to_rds(export_data, file_path, compression),
    "research_standard" = export_to_research_standard(export_data, file_path),
    stop("Unsupported format: ", format)
  )

  # Create export summary
  export_summary <- create_export_summary(export_data, file_path, format)

  # Create export log
  export_log <- list(
    export_time = Sys.time(),
    format = format,
    file_size = file.info(file_path)$size,
    compression = compression,
    filters_applied = !is.null(filter_options),
    filter_details = filter_options
  )

  message("Export completed: ", file_path)

  # Combine export result details (e.g., sheets_created for Excel)
  result <- list(
    export_path = normalizePath(file_path),
    export_summary = export_summary,
    metadata = export_data$metadata,
    export_log = export_log
  )

  # Add format-specific details from export_result
  if (is.list(export_result)) {
    result <- c(result, export_result)
  }

  return(result)
}

#' Generate Comprehensive Analysis Report
#'
#' @description
#' Creates a comprehensive analysis report combining data summary, quality assessment,
#' statistical analysis, method comparison, and visualization exports. Supports
#' multiple output formats including HTML, PDF, and Word documents.
#'
#' @param sap_data A sap_data object from read_sap_data()
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param report_type Character string: "executive", "technical", "full" (default: "full")
#' @param output_file Character string specifying output file path
#' @param output_format Character string: "html", "pdf", "word", "markdown" (default: "html")
#' @param include_plots Logical, whether to include diagnostic plots (default: TRUE)
#' @param include_raw_data Logical, whether to include raw data tables (default: FALSE)
#' @param custom_title Character string, custom report title
#' @param author_info List containing author information (name, affiliation, contact)
#' @param template_path Character string, path to custom report template (optional)
#'
#' @return List containing:
#'   \describe{
#'     \item{report_path}{Path to generated report file}
#'     \item{report_summary}{Summary of report contents}
#'     \item{figures_created}{List of figures created and their paths}
#'     \item{tables_created}{List of tables created}
#'   }
#'
#' @details
#' Report types:
#' - **Executive**: High-level summary with key findings and recommendations
#' - **Technical**: Detailed technical analysis with method comparisons and diagnostics
#' - **Full**: Comprehensive report with all available analyses and appendices
#'
#' The report includes:
#' - Data overview and quality assessment
#' - Statistical summaries and descriptive statistics
#' - Method comparison and performance analysis
#' - Quality control flags and outlier detection
#' - Temporal patterns and trends analysis
#' - Diagnostic plots and visualizations
#' - Recommendations and best practices
#'
#' @examples
#' \dontrun{
#' # Generate full HTML report
#' report_result <- generate_analysis_report(
#'   sap_data, vh_results,
#'   report_type = "full",
#'   output_file = "sap_flow_analysis_report.html"
#' )
#'
#' # Generate executive PDF report
#' report_result <- generate_analysis_report(
#'   sap_data, vh_results,
#'   report_type = "executive",
#'   output_format = "pdf",
#'   custom_title = "Sap Flow Analysis - Site XYZ",
#'   author_info = list(
#'     name = "Dr. Jane Smith",
#'     affiliation = "University Research Lab",
#'     contact = "jane.smith@university.edu"
#'   )
#' )
#' }
#'
#' @export
generate_analysis_report <- function(sap_data, vh_results,
                                     report_type = "full",
                                     output_file = NULL,
                                     output_format = "html",
                                     include_plots = TRUE,
                                     include_raw_data = FALSE,
                                     custom_title = NULL,
                                     author_info = NULL,
                                     template_path = NULL) {

  # Validate inputs
  if (!inherits(sap_data, "sap_data")) {
    stop("sap_data must be a sap_data object")
  }

  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object")
  }

  # Set default output file if not provided
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_ext <- switch(output_format,
                       "html" = ".html", "pdf" = ".pdf",
                       "word" = ".docx", "markdown" = ".md")
    output_file <- paste0("sap_flow_report_", timestamp, file_ext)
  }

  message("Generating ", report_type, " analysis report...")

  # Generate report components based on type
  report_components <- generate_report_components(
    sap_data, vh_results, report_type, include_plots, include_raw_data
  )

  # Create figures directory for plots
  figures_dir <- paste0(tools::file_path_sans_ext(output_file), "_files")
  if (include_plots && !dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }

  # Generate and save plots if requested
  figures_created <- list()
  if (include_plots) {
    figures_created <- generate_report_plots(vh_results, figures_dir)
  }

  # Generate statistical tables
  tables_created <- generate_report_tables(sap_data, vh_results, report_type)

  # Create report content
  report_content <- create_report_content(
    report_components, tables_created, figures_created,
    report_type, custom_title, author_info
  )

  # Render report to specified format
  render_report(report_content, output_file, output_format, template_path)

  # Create report summary
  report_summary <- list(
    report_type = report_type,
    output_format = output_format,
    data_period = range(sap_data$measurements$datetime),
    methods_analysed = unique(vh_results$method),
    total_measurements = nrow(vh_results),
    figures_included = length(figures_created),
    tables_included = length(tables_created),
    generation_time = Sys.time()
  )

  message("Report generated successfully: ", output_file)

  return(list(
    report_path = normalizePath(output_file),
    report_summary = report_summary,
    figures_created = figures_created,
    tables_created = names(tables_created)
  ))
}

#' Calculate Enhanced Summary Statistics
#'
#' @description
#' Calculates comprehensive summary statistics for sap flow velocity data
#' with advanced statistical measures, temporal patterns, and method comparisons.
#' Extends the existing calc_velocity_stats() function with additional metrics.
#'
#' @param vh_results A vh_results object from calc_heat_pulse_velocity()
#' @param group_by Character vector specifying grouping variables:
#'   "method", "sensor_position", "quality_flag", "temporal" (default: c("method"))
#' @param temporal_aggregation Character string: "hourly", "daily", "weekly", "monthly" (default: "daily")
#' @param include_percentiles Logical, whether to include percentile statistics (default: TRUE)
#' @param include_confidence_intervals Logical, whether to calculate confidence intervals (default: TRUE)
#' @param confidence_level Numeric, confidence level for intervals (default: 0.95)
#' @param include_outlier_analysis Logical, whether to perform outlier analysis (default: TRUE)
#' @param include_distribution_tests Logical, whether to perform normality tests (default: FALSE)
#'
#' @return List containing:
#'   \describe{
#'     \item{summary_stats}{Data frame with comprehensive summary statistics}
#'     \item{temporal_patterns}{Data frame with temporal aggregation statistics}
#'     \item{method_comparison}{Data frame with method comparison metrics}
#'     \item{quality_assessment}{Data frame with quality flag statistics}
#'     \item{outlier_analysis}{List with outlier detection results}
#'     \item{distribution_tests}{List with distribution test results (if requested)}
#'     \item{correlation_matrix}{Correlation matrix between methods}
#'   }
#'
#' @details
#' Enhanced statistics include:
#' - Basic descriptives: mean, median, SD, IQR, min, max
#' - Percentiles: 5th, 10th, 25th, 75th, 90th, 95th
#' - Confidence intervals for means
#' - Outlier detection using multiple methods (IQR, z-score, modified z-score)
#' - Temporal patterns and trends
#' - Method agreement and correlation analysis
#' - Quality flag distributions and patterns
#' - Distribution tests (Shapiro-Wilk, Kolmogorov-Smirnov)
#'
#' @examples
#' \dontrun{
#' # Basic enhanced statistics
#' enhanced_stats <- calc_enhanced_statistics(vh_results)
#' print(enhanced_stats$summary_stats)
#'
#' # Temporal analysis with hourly aggregation
#' temporal_stats <- calc_enhanced_statistics(
#'   vh_results,
#'   group_by = c("method", "temporal"),
#'   temporal_aggregation = "hourly",
#'   include_distribution_tests = TRUE
#' )
#' }
#'
#' @export
calc_enhanced_statistics <- function(vh_results,
                                     group_by = c("method"),
                                     temporal_aggregation = "daily",
                                     include_percentiles = TRUE,
                                     include_confidence_intervals = TRUE,
                                     confidence_level = 0.95,
                                     include_outlier_analysis = TRUE,
                                     include_distribution_tests = FALSE) {

  if (!inherits(vh_results, "vh_results")) {
    stop("vh_results must be a vh_results object")
  }

  message("Calculating enhanced summary statistics...")

  # Convert to data frame for easier manipulation
  data_df <- as.data.frame(vh_results)

  # Calculate basic summary statistics by groups
  summary_stats <- calculate_grouped_statistics(
    data_df, group_by, include_percentiles,
    include_confidence_intervals, confidence_level
  )

  # Calculate temporal patterns
  temporal_patterns <- NULL
  if ("temporal" %in% group_by) {
    temporal_patterns <- calculate_temporal_statistics(
      data_df, temporal_aggregation
    )
  }

  # Method comparison analysis
  method_comparison <- NULL
  if (length(unique(data_df$method)) > 1) {
    method_comparison <- calculate_method_comparison_stats(data_df)
  }

  # Quality assessment statistics
  quality_assessment <- calculate_quality_statistics(data_df)

  # Outlier analysis
  outlier_analysis <- NULL
  if (include_outlier_analysis) {
    outlier_analysis <- perform_outlier_analysis(data_df)
  }

  # Distribution tests
  distribution_tests <- NULL
  if (include_distribution_tests) {
    distribution_tests <- perform_distribution_tests(data_df)
  }

  # Correlation matrix between methods
  correlation_matrix <- NULL
  if (length(unique(data_df$method)) > 1) {
    correlation_matrix <- calculate_method_correlations(data_df)
  }

  message("Enhanced statistics calculation completed!")

  return(list(
    summary_stats = summary_stats,
    temporal_patterns = temporal_patterns,
    method_comparison = method_comparison,
    quality_assessment = quality_assessment,
    outlier_analysis = outlier_analysis,
    distribution_tests = distribution_tests,
    correlation_matrix = correlation_matrix
  ))
}

#' Batch Process Multiple Sap Flow Files
#'
#' @description
#' Process multiple sap flow data files in batch with consistent parameters,
#' automatic report generation, and consolidated output. Ideal for processing
#' multiple sensors, sites, or time periods.
#'
#' @param file_paths Character vector of file paths to process
#' @param output_directory Character string specifying output directory
#' @param methods Character vector of calculation methods to apply to all files
#' @param parameters List of calculation parameters (applied to all files)
#' @param generate_individual_reports Logical, whether to generate individual reports (default: TRUE)
#' @param generate_consolidated_report Logical, whether to generate consolidated report (default: TRUE)
#' @param export_format Character string: "csv", "xlsx", "json" (default: "xlsx")
#' @param parallel_processing Logical, whether to use parallel processing (default: FALSE)
#' @param n_cores Integer, number of cores for parallel processing (default: NULL = auto-detect)
#' @param error_handling Character string: "stop", "skip", "warn" (default: "warn")
#' @param progress_bar Logical, whether to show progress bar (default: TRUE)
#'
#' @return List containing:
#'   \describe{
#'     \item{processing_summary}{Data frame with processing results for each file}
#'     \item{consolidated_results}{Combined results from all files}
#'     \item{failed_files}{List of files that failed processing}
#'     \item{output_files}{List of all generated output files}
#'     \item{processing_time}{Total processing time}
#'   }
#'
#' @details
#' The batch processing system:
#' - Processes files sequentially or in parallel
#' - Handles errors gracefully with multiple error handling options
#' - Generates individual reports and/or consolidated summary
#' - Preserves metadata and processing logs
#' - Supports progress tracking and time estimation
#' - Creates standardised file naming conventions
#'
#' @examples
#' \dontrun{
#' # Process multiple files with default settings
#' file_list <- list.files("data/", pattern = "\\.txt$", full.names = TRUE)
#' batch_result <- batch_process_sap_flow(
#'   file_paths = file_list,
#'   output_directory = "results/",
#'   methods = c("HRM", "MHR", "DMA")
#' )
#'
#' # Parallel processing with custom parameters
#' batch_result <- batch_process_sap_flow(
#'   file_paths = file_list,
#'   output_directory = "results/",
#'   methods = c("HRM", "DMA"),
#'   parallel_processing = TRUE,
#'   n_cores = 4,
#'   export_format = "csv"
#' )
#' }
#'
#' @export
batch_process_sap_flow <- function(file_paths, output_directory,
                                   methods = c("HRM", "MHR", "DMA"),
                                   parameters = NULL,
                                   generate_individual_reports = TRUE,
                                   generate_consolidated_report = TRUE,
                                   export_format = "xlsx",
                                   parallel_processing = FALSE,
                                   n_cores = NULL,
                                   error_handling = "warn",
                                   progress_bar = TRUE) {

  # Validate inputs
  if (!all(file.exists(file_paths))) {
    missing_files <- file_paths[!file.exists(file_paths)]
    stop("Files not found: ", paste(missing_files, collapse = ", "))
  }

  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
    message("Created output directory: ", output_directory)
  }

  # Setup parallel processing if requested
  if (parallel_processing) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    message("Setting up parallel processing with ", n_cores, " cores...")
  }

  # Initialize tracking variables
  start_time <- Sys.time()
  processing_results <- list()
  failed_files <- list()
  consolidated_results <- list()
  output_files <- list()

  # Setup progress bar
  if (progress_bar && !parallel_processing) {
    pb <- txtProgressBar(min = 0, max = length(file_paths), style = 3)
  }

  message("Starting batch processing of ", length(file_paths), " files...")

  # Process files
  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    base_name <- tools::file_path_sans_ext(basename(file_path))

    tryCatch({
      # Import data
      sap_data <- read_sap_data(file_path)

      # Calculate velocities
      vh_results <- calc_heat_pulse_velocity(
        sap_data, methods = methods, parameters = parameters
      )

      # Generate individual report if requested
      if (generate_individual_reports) {
        report_file <- file.path(output_directory,
                                 paste0(base_name, "_report.html"))
        generate_analysis_report(sap_data, vh_results,
                                 output_file = report_file,
                                 report_type = "technical")
        output_files[[paste0(base_name, "_report")]] <- report_file
      }

      # Export data
      export_file <- file.path(output_directory,
                               paste0(base_name, "_results.", export_format))
      export_sap_data(sap_data, vh_results, export_file, format = export_format)
      output_files[[paste0(base_name, "_data")]] <- export_file

      # Store results for consolidation
      vh_results$source_file <- base_name
      consolidated_results[[base_name]] <- vh_results

      # Record processing success
      processing_results[[base_name]] <- list(
        file_path = file_path,
        status = "success",
        n_measurements = nrow(vh_results),
        methods_used = unique(vh_results$method),
        processing_time = Sys.time() - start_time
      )

    }, error = function(e) {
      # Handle processing errors
      error_msg <- as.character(e)
      failed_files[[base_name]] <- list(
        file_path = file_path,
        error = error_msg,
        timestamp = Sys.time()
      )

      processing_results[[base_name]] <- list(
        file_path = file_path,
        status = "failed",
        error = error_msg,
        processing_time = NA
      )

      if (error_handling == "stop") {
        stop("Processing failed for file: ", file_path, "\nError: ", error_msg)
      } else if (error_handling == "warn") {
        warning("Processing failed for file: ", file_path, "\nError: ", error_msg)
      }
      # If error_handling == "skip", continue silently
    })

    # Update progress bar
    if (progress_bar && !parallel_processing) {
      setTxtProgressBar(pb, i)
    }
  }

  if (progress_bar && !parallel_processing) {
    close(pb)
  }

  # Generate consolidated report if requested
  if (generate_consolidated_report && length(consolidated_results) > 0) {
    consolidated_data <- do.call(rbind, consolidated_results)
    consolidated_report_file <- file.path(output_directory,
                                          "consolidated_report.html")

    # Create a consolidated sap_data object (simplified)
    consolidated_sap_data <- list(
      measurements = data.frame(), # Placeholder
      diagnostics = data.frame(), # Placeholder
      metadata = list(
        files_processed = names(consolidated_results),
        processing_date = Sys.time(),
        batch_processing = TRUE
      )
    )
    class(consolidated_sap_data) <- "sap_data"

    generate_analysis_report(
      consolidated_sap_data, consolidated_data,
      output_file = consolidated_report_file,
      report_type = "full",
      custom_title = "Consolidated Sap Flow Analysis Report"
    )

    output_files[["consolidated_report"]] <- consolidated_report_file
  }

  # Create processing summary
  processing_summary <- data.frame(
    file_name = names(processing_results),
    status = sapply(processing_results, function(x) x$status),
    n_measurements = sapply(processing_results, function(x) x$n_measurements %||% 0),
    processing_time = sapply(processing_results, function(x) x$processing_time %||% NA),
    stringsAsFactors = FALSE
  )

  end_time <- Sys.time()
  total_time <- end_time - start_time

  message("Batch processing completed!")
  message("Successfully processed: ", sum(processing_summary$status == "success"), " files")
  message("Failed: ", length(failed_files), " files")
  message("Total processing time: ", round(as.numeric(total_time, units = "mins"), 2), " minutes")

  return(list(
    processing_summary = processing_summary,
    consolidated_results = if(length(consolidated_results) > 0) do.call(rbind, consolidated_results) else NULL,
    failed_files = failed_files,
    output_files = output_files,
    processing_time = total_time
  ))
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

#' Create Quality Control Report Templates
#'
#' @description
#' Creates standardised quality control report templates for consistent
#' reporting across different analyses and research projects.
#'
#' @param template_type Character string: "research_paper", "technical_memo",
#'   "monitoring_report", "troubleshooting" (default: "research_paper")
#' @param output_path Character string specifying where to save template
#' @param include_sample_data Logical, whether to include sample data sections (default: TRUE)
#' @param custom_sections List of custom sections to add to template
#'
#' @return Character string path to created template file
#'
#' @details
#' Template types:
#' - **Research Paper**: Academic publication format with methods, results, discussion
#' - **Technical Memo**: Brief technical summary for internal use
#' - **Monitoring Report**: Routine monitoring format with trend analysis
#' - **Troubleshooting**: Diagnostic-focused template for problem identification
#'
#' @examples
#' \dontrun{
#' # Create research paper template
#' template_path <- create_qc_report_template(
#'   "research_paper",
#'   "templates/research_template.Rmd"
#' )
#'
#' # Create custom monitoring template
#' template_path <- create_qc_report_template(
#'   "monitoring_report",
#'   "templates/monitoring_template.Rmd",
#'   custom_sections = list(
#'     "Environmental Conditions" = "## Environmental Conditions\n\n",
#'     "Maintenance Log" = "## Maintenance Log\n\n"
#'   )
#' )
#' }
#'
#' @export
create_qc_report_template <- function(template_type = "research_paper",
                                      output_path = NULL,
                                      include_sample_data = TRUE,
                                      custom_sections = NULL) {

  if (is.null(output_path)) {
    output_path <- paste0(template_type, "_template.Rmd")
  }

  # Generate template content based on type
  template_content <- switch(
    template_type,
    "research_paper" = create_research_template(include_sample_data, custom_sections),
    "technical_memo" = create_technical_memo_template(include_sample_data, custom_sections),
    "monitoring_report" = create_monitoring_template(include_sample_data, custom_sections),
    "troubleshooting" = create_troubleshooting_template(include_sample_data, custom_sections),
    stop("Unsupported template type: ", template_type)
  )

  # Write template to file
  writeLines(template_content, output_path)

  message("Quality control report template created: ", output_path)

  return(normalizePath(output_path))
}# R/export_reporting_helpers.R
# Chat 9: Helper Functions for Export and Reporting System
# sapFluxR Package Development

#' Helper Functions for Export and Reporting
#'
#' @description
#' Internal helper functions supporting the comprehensive export and reporting system.
#' These functions handle data preparation, format conversion, statistical calculations,
#' and report generation components.
#'
#' @name export_reporting_helpers
#' @keywords internal
NULL

# =============================================================================
# DATA PREPARATION HELPERS
# =============================================================================

#' Prepare Data for Export
#'
#' @description Internal function to prepare sap flow data for export
#' @param sap_data A sap_data object
#' @param vh_results Optional vh_results object
#' @param include_metadata Logical
#' @param include_diagnostics Logical
#' @param include_quality_flags Logical
#' @return List with prepared export data
#' @keywords internal
prepare_export_data <- function(sap_data, vh_results = NULL,
                                include_metadata = TRUE,
                                include_diagnostics = TRUE,
                                include_quality_flags = TRUE) {

  export_data <- list()

  # Core measurements data
  export_data$measurements <- sap_data$measurements

  # Diagnostics data
  if (include_diagnostics && !is.null(sap_data$diagnostics)) {
    export_data$diagnostics <- sap_data$diagnostics
  }

  # Velocity results
  if (!is.null(vh_results)) {
    vh_df <- as.data.frame(vh_results)

    # Filter quality flags if not requested
    if (!include_quality_flags) {
      vh_df$quality_flag <- NULL
    }

    export_data$velocity_results <- vh_df
  }

  # Metadata
  if (include_metadata) {
    metadata <- sap_data$metadata
    metadata$export_timestamp <- Sys.time()
    metadata$export_version <- utils::packageVersion("sapFluxR")
    export_data$metadata <- metadata
  }

  return(export_data)
}

#' Apply Export Filters
#'
#' @description Internal function to apply filtering options to velocity results
#' @param vh_results A vh_results object
#' @param filter_options List of filtering options
#' @return Filtered vh_results object
#' @keywords internal
apply_export_filters <- function(vh_results, filter_options) {

  if (is.null(filter_options)) return(vh_results)

  result <- vh_results

  # Filter by methods
  if (!is.null(filter_options$methods)) {
    result <- result[result$method %in% filter_options$methods, ]
  }

  # Filter by quality flags
  if (!is.null(filter_options$quality_flags)) {
    result <- result[result$quality_flag %in% filter_options$quality_flags, ]
  }

  # Filter by date range
  if (!is.null(filter_options$date_range)) {
    date_range <- as.POSIXct(filter_options$date_range)
    result <- result[result$datetime >= date_range[1] &
                       result$datetime <= date_range[2], ]
  }

  # Filter by velocity range
  if (!is.null(filter_options$velocity_range)) {
    vel_range <- filter_options$velocity_range
    result <- result[result$Vh_cm_hr >= vel_range[1] &
                       result$Vh_cm_hr <= vel_range[2], ]
  }

  # Maintain class attributes
  class(result) <- class(vh_results)

  return(result)
}

#' Apply Export Filters to Sap Data
#'
#' @description Internal function to apply filtering options to sap data
#' @param sap_data A sap_data object
#' @param filter_options List of filtering options
#' @return Filtered sap_data object
#' @keywords internal
apply_export_filters_sap_data <- function(sap_data, filter_options) {

  if (is.null(filter_options) || is.null(filter_options$date_range)) {
    return(sap_data)
  }

  # Filter measurements by date range
  if (!is.null(filter_options$date_range)) {
    date_range <- as.POSIXct(filter_options$date_range)

    # Filter measurements
    sap_data$measurements <- sap_data$measurements[
      sap_data$measurements$datetime >= date_range[1] &
        sap_data$measurements$datetime <= date_range[2],
    ]

    # Filter diagnostics
    if (!is.null(sap_data$diagnostics)) {
      sap_data$diagnostics <- sap_data$diagnostics[
        sap_data$diagnostics$datetime >= date_range[1] &
          sap_data$diagnostics$datetime <= date_range[2],
      ]
    }
  }

  return(sap_data)
}

# =============================================================================
# FORMAT-SPECIFIC EXPORT FUNCTIONS
# =============================================================================

#' Export to CSV Format
#'
#' @description Internal function for CSV export
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @param compression Logical, whether to compress
#' @return Export result list
#' @keywords internal
export_to_csv <- function(export_data, file_path, compression = FALSE) {

  # Create metadata header
  if (!is.null(export_data$metadata)) {
    metadata_lines <- create_csv_metadata_header(export_data$metadata)
  } else {
    metadata_lines <- character(0)
  }

  # Combine all data tables
  combined_data <- combine_data_for_csv(export_data)

  # Write metadata header
  if (length(metadata_lines) > 0) {
    writeLines(metadata_lines, file_path)
    readr::write_csv(combined_data, file_path, append = TRUE)
  } else {
    readr::write_csv(combined_data, file_path)
  }

  # Apply compression if requested
  if (compression) {
    R.utils::gzip(file_path, remove = TRUE)
    file_path <- paste0(file_path, ".gz")
  }

  return(list(
    format = "csv",
    compressed = compression,
    file_size = file.info(file_path)$size
  ))
}

#' Export to Excel Format
#'
#' @description Internal function for Excel export with multiple sheets
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @return Export result list
#' @keywords internal
export_to_excel <- function(export_data, file_path) {

  # Check for required packages
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' required for Excel export. Please install it.")
  }

  # Create workbook
  wb <- openxlsx::createWorkbook()
  sheets_created <- character(0)

  # Add data sheets
  if (!is.null(export_data$velocity_results)) {
    openxlsx::addWorksheet(wb, "Velocity_Results")
    openxlsx::writeData(wb, "Velocity_Results", export_data$velocity_results)
    sheets_created <- c(sheets_created, "Velocity_Results")
  }

  if (!is.null(export_data$measurements)) {
    openxlsx::addWorksheet(wb, "Measurements")
    openxlsx::writeData(wb, "Measurements", export_data$measurements)
    sheets_created <- c(sheets_created, "Measurements")
  }

  if (!is.null(export_data$diagnostics)) {
    openxlsx::addWorksheet(wb, "Diagnostics")
    openxlsx::writeData(wb, "Diagnostics", export_data$diagnostics)
    sheets_created <- c(sheets_created, "Diagnostics")
  }

  # Add metadata sheet
  if (!is.null(export_data$metadata)) {
    metadata_df <- data.frame(
      Parameter = names(export_data$metadata),
      Value = sapply(export_data$metadata, function(x) {
        if (is.list(x)) paste(unlist(x), collapse = ", ") else as.character(x)
      }),
      stringsAsFactors = FALSE
    )
    openxlsx::addWorksheet(wb, "Metadata")
    openxlsx::writeData(wb, "Metadata", metadata_df)
    sheets_created <- c(sheets_created, "Metadata")
  }

  # Add summary sheet
  if (!is.null(export_data$velocity_results)) {
    summary_data <- create_summary_sheet(export_data$velocity_results)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_data)
    sheets_created <- c(sheets_created, "Summary")
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  return(list(
    format = "xlsx",
    sheets_created = sheets_created,
    file_size = file.info(file_path)$size
  ))
}

#' Export to JSON Format
#'
#' @description Internal function for JSON export
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @param compression Logical, whether to compress
#' @return Export result list
#' @keywords internal
export_to_json <- function(export_data, file_path, compression = FALSE) {

  # Convert data frames to lists for better JSON structure
  json_data <- export_data

  if (!is.null(json_data$measurements)) {
    json_data$measurements <- as.list(json_data$measurements)
  }

  if (!is.null(json_data$velocity_results)) {
    json_data$velocity_results <- as.list(json_data$velocity_results)
  }

  if (!is.null(json_data$diagnostics)) {
    json_data$diagnostics <- as.list(json_data$diagnostics)
  }

  # Convert metadata to ensure JSON compatibility
  if (!is.null(json_data$metadata)) {
    json_data$metadata <- lapply(json_data$metadata, function(x) {
      if (inherits(x, "numeric_version")) {
        as.character(x)
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
        format(x, "%Y-%m-%d %H:%M:%S")
      } else if (is.list(x)) {
        paste(unlist(x), collapse = ", ")
      } else {
        as.character(x)
      }
    })
  }

  # Write JSON with pretty formatting
  jsonlite::write_json(json_data, file_path, pretty = TRUE, auto_unbox = TRUE)

  # Apply compression if requested
  if (compression) {
    R.utils::gzip(file_path, remove = TRUE)
    file_path <- paste0(file_path, ".gz")
  }

  return(list(
    format = "json",
    compressed = compression,
    file_size = file.info(file_path)$size
  ))
}

#' Export to Text Format
#'
#' @description Internal function for tab-delimited text export
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @param compression Logical, whether to compress
#' @return Export result list
#' @keywords internal
export_to_txt <- function(export_data, file_path, compression = FALSE) {

  # Create metadata header
  if (!is.null(export_data$metadata)) {
    metadata_lines <- create_txt_metadata_header(export_data$metadata)
    writeLines(metadata_lines, file_path)
  }

  # Combine data tables
  combined_data <- combine_data_for_csv(export_data)

  # Write tab-delimited data
  readr::write_tsv(combined_data, file_path, append = !is.null(export_data$metadata))

  # Apply compression if requested
  if (compression) {
    R.utils::gzip(file_path, remove = TRUE)
    file_path <- paste0(file_path, ".gz")
  }

  return(list(
    format = "txt",
    compressed = compression,
    file_size = file.info(file_path)$size
  ))
}

#' Export to RDS Format
#'
#' @description Internal function for R native RDS export
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @param compression Logical, whether to compress
#' @return Export result list
#' @keywords internal
export_to_rds <- function(export_data, file_path, compression = FALSE) {

  # Save as RDS with optional compression
  if (compression) {
    saveRDS(export_data, file_path, compress = "gzip")
  } else {
    saveRDS(export_data, file_path, compress = FALSE)
  }

  return(list(
    format = "rds",
    compressed = compression,
    file_size = file.info(file_path)$size
  ))
}

#' Export to Research Standard Format
#'
#' @description Internal function for standardised research format export
#' @param export_data Prepared export data list
#' @param file_path Output file path
#' @return Export result list
#' @keywords internal
export_to_research_standard <- function(export_data, file_path) {

  # Create research standard CSV with specific column order and formatting
  if (!is.null(export_data$velocity_results)) {

    # Standardise column names and order
    standard_data <- standardise_for_research(export_data$velocity_results)

    # Create comprehensive metadata section
    metadata_section <- create_research_metadata_section(export_data$metadata)

    # Write metadata first
    writeLines(metadata_section, file_path)

    # Write standardised data
    readr::write_csv(standard_data, file_path, append = TRUE)

  } else {
    stop("No velocity results available for research standard export")
  }

  return(list(
    format = "research_standard",
    standardised = TRUE,
    file_size = file.info(file_path)$size
  ))
}

# =============================================================================
# STATISTICAL CALCULATION HELPERS
# =============================================================================

#' Calculate Grouped Statistics
#'
#' @description Internal function for calculating grouped summary statistics
#' @param data_df Data frame with velocity results
#' @param group_by Character vector of grouping variables
#' @param include_percentiles Logical
#' @param include_confidence_intervals Logical
#' @param confidence_level Numeric
#' @return Data frame with grouped statistics
#' @keywords internal
calculate_grouped_statistics <- function(data_df, group_by, include_percentiles = TRUE,
                                         include_confidence_intervals = TRUE,
                                         confidence_level = 0.95) {

  # Handle temporal grouping by creating temporal columns if needed
  if ("temporal" %in% group_by) {
    # Remove temporal from group_by and handle it separately
    group_by <- group_by[group_by != "temporal"]
    # This will be handled by the temporal analysis function
    # For now, just group by the remaining variables
  }

  # Create grouping variables
  if (length(group_by) == 0 || (length(group_by) == 1 && group_by[1] == "method")) {
    groups <- list(method = data_df$method)
  } else {
    # Check that all group_by columns exist
    valid_columns <- intersect(group_by, names(data_df))
    if (length(valid_columns) == 0) {
      groups <- list(method = data_df$method)
    } else {
      groups <- data_df[, valid_columns, drop = FALSE]
      if (!"method" %in% names(groups)) {
        groups$method <- data_df$method
      }
    }
  }

  # Calculate statistics by group
  stats_list <- by(data_df$Vh_cm_hr, groups, function(x) {
    x <- x[is.finite(x)]  # Remove infinite/NA values

    if (length(x) == 0) {
      return(data.frame(
        n = 0, mean = NA, median = NA, sd = NA, min = NA, max = NA,
        q25 = NA, q75 = NA, iqr = NA
      ))
    }

    basic_stats <- data.frame(
      n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      q25 = quantile(x, 0.25, na.rm = TRUE),
      q75 = quantile(x, 0.75, na.rm = TRUE),
      iqr = IQR(x, na.rm = TRUE)
    )

    # Add percentiles if requested
    if (include_percentiles) {
      percentiles <- data.frame(
        p5 = quantile(x, 0.05, na.rm = TRUE),
        p10 = quantile(x, 0.10, na.rm = TRUE),
        p90 = quantile(x, 0.90, na.rm = TRUE),
        p95 = quantile(x, 0.95, na.rm = TRUE)
      )
      basic_stats <- cbind(basic_stats, percentiles)
    }

    # Add confidence intervals if requested
    if (include_confidence_intervals && length(x) > 1) {
      alpha <- 1 - confidence_level
      t_val <- qt(1 - alpha/2, length(x) - 1)
      se <- sd(x, na.rm = TRUE) / sqrt(length(x))

      ci_stats <- data.frame(
        ci_lower = mean(x, na.rm = TRUE) - t_val * se,
        ci_upper = mean(x, na.rm = TRUE) + t_val * se,
        se = se
      )
      basic_stats <- cbind(basic_stats, ci_stats)
    }

    return(basic_stats)
  })

  # Handle empty stats_list
  if (length(stats_list) == 0) {
    return(data.frame())
  }

  # Combine results - handle case where stats_list might be empty or contain NULLs
  valid_stats <- stats_list[!sapply(stats_list, is.null)]

  if (length(valid_stats) == 0) {
    return(data.frame())
  }

  result_df <- do.call(rbind, valid_stats)

  # Add group identifiers
  if (is.list(groups) && length(groups) == 1 && names(groups)[1] == "method") {
    result_df$method <- names(valid_stats)
  }

  return(result_df)
}

#' Calculate Temporal Statistics
#'
#' @description Internal function for temporal aggregation statistics
#' @param data_df Data frame with velocity results
#' @param temporal_aggregation Character string for aggregation level
#' @return Data frame with temporal statistics
#' @keywords internal
calculate_temporal_statistics <- function(data_df, temporal_aggregation = "daily") {

  # Add temporal grouping variable
  data_df$temporal_group <- switch(
    temporal_aggregation,
    "hourly" = format(data_df$datetime, "%Y-%m-%d %H"),
    "daily" = format(data_df$datetime, "%Y-%m-%d"),
    "weekly" = format(data_df$datetime, "%Y-W%V"),
    "monthly" = format(data_df$datetime, "%Y-%m"),
    format(data_df$datetime, "%Y-%m-%d")  # Default to daily
  )

  # Calculate statistics by temporal group
  temporal_stats <- aggregate(
    Vh_cm_hr ~ temporal_group + method,
    data = data_df,
    FUN = function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(c(n=0, mean=NA, sd=NA, min=NA, max=NA))
      c(n = length(x),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE))
    }
  )

  # Convert to proper data frame format
  stats_matrix <- temporal_stats$Vh_cm_hr
  temporal_result <- data.frame(
    temporal_group = temporal_stats$temporal_group,
    method = temporal_stats$method,
    n = stats_matrix[, "n"],
    mean = stats_matrix[, "mean"],
    sd = stats_matrix[, "sd"],
    min = stats_matrix[, "min"],
    max = stats_matrix[, "max"],
    stringsAsFactors = FALSE
  )

  return(temporal_result)
}

#' Calculate Method Comparison Statistics
#'
#' @description Internal function for method comparison analysis
#' @param data_df Data frame with velocity results
#' @return Data frame with method comparison metrics
#' @keywords internal
calculate_method_comparison_stats <- function(data_df) {

  methods <- unique(data_df$method)

  if (length(methods) < 2) {
    return(NULL)
  }

  # Create comparison matrix
  comparison_results <- data.frame()

  for (i in 1:(length(methods)-1)) {
    for (j in (i+1):length(methods)) {
      method1 <- methods[i]
      method2 <- methods[j]

      # Get paired data
      data1 <- data_df[data_df$method == method1, ]
      data2 <- data_df[data_df$method == method2, ]

      # Match by datetime and pulse_id for proper pairing
      merged_data <- merge(
        data1[, c("datetime", "pulse_id", "Vh_cm_hr")],
        data2[, c("datetime", "pulse_id", "Vh_cm_hr")],
        by = c("datetime", "pulse_id"),
        suffixes = c("_1", "_2")
      )

      if (nrow(merged_data) > 0) {
        # Calculate comparison metrics
        correlation <- cor(merged_data$Vh_cm_hr_1, merged_data$Vh_cm_hr_2,
                           use = "complete.obs")

        mean_diff <- mean(merged_data$Vh_cm_hr_1 - merged_data$Vh_cm_hr_2,
                          na.rm = TRUE)

        rmse <- sqrt(mean((merged_data$Vh_cm_hr_1 - merged_data$Vh_cm_hr_2)^2,
                          na.rm = TRUE))

        # Linear regression
        if (sum(is.finite(merged_data$Vh_cm_hr_1) & is.finite(merged_data$Vh_cm_hr_2)) > 2) {
          lm_result <- lm(Vh_cm_hr_2 ~ Vh_cm_hr_1, data = merged_data)
          slope <- coef(lm_result)[2]
          intercept <- coef(lm_result)[1]
          r_squared <- summary(lm_result)$r.squared
        } else {
          slope <- NA
          intercept <- NA
          r_squared <- NA
        }

        comparison_row <- data.frame(
          method_1 = method1,
          method_2 = method2,
          n_paired = nrow(merged_data),
          correlation = correlation,
          mean_difference = mean_diff,
          rmse = rmse,
          slope = slope,
          intercept = intercept,
          r_squared = r_squared,
          stringsAsFactors = FALSE
        )

        comparison_results <- rbind(comparison_results, comparison_row)
      }
    }
  }

  # If no comparisons were made, return empty data frame with expected structure
  if (nrow(comparison_results) == 0) {
    comparison_results <- data.frame(
      method_1 = character(0),
      method_2 = character(0),
      n_paired = numeric(0),
      correlation = numeric(0),
      mean_difference = numeric(0),
      rmse = numeric(0),
      slope = numeric(0),
      intercept = numeric(0),
      r_squared = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  return(comparison_results)
}

#' Calculate Quality Statistics
#'
#' @description Internal function for quality flag analysis
#' @param data_df Data frame with velocity results
#' @return Data frame with quality statistics
#' @keywords internal
calculate_quality_statistics <- function(data_df) {

  if (!"quality_flag" %in% names(data_df)) {
    return(NULL)
  }

  # Overall quality summary
  quality_summary <- table(data_df$quality_flag, useNA = "ifany")
  quality_percentages <- prop.table(quality_summary) * 100

  overall_quality <- data.frame(
    quality_flag = names(quality_summary),
    count = as.numeric(quality_summary),
    percentage = as.numeric(quality_percentages),
    stringsAsFactors = FALSE
  )

  # Quality by method
  if (nrow(data_df) > 0 && "method" %in% names(data_df)) {
    method_quality <- table(data_df$method, data_df$quality_flag, useNA = "ifany")
    method_quality_df <- as.data.frame(method_quality)
    names(method_quality_df) <- c("method", "quality_flag", "count")
  } else {
    # Return empty data frame with expected structure
    method_quality_df <- data.frame(
      method = character(0),
      quality_flag = character(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    overall_quality = overall_quality,
    quality_by_method = method_quality_df
  ))
}

#' Perform Outlier Analysis
#'
#' @description Internal function for outlier detection using multiple methods
#' @param data_df Data frame with velocity results
#' @return List with outlier analysis results
#' @keywords internal
perform_outlier_analysis <- function(data_df) {

  outlier_results <- list()

  for (method in unique(data_df$method)) {
    method_data <- data_df[data_df$method == method, "Vh_cm_hr"]
    method_data <- method_data[is.finite(method_data)]

    if (length(method_data) == 0) next

    # IQR-based outliers
    q25 <- quantile(method_data, 0.25)
    q75 <- quantile(method_data, 0.75)
    iqr <- q75 - q25
    iqr_lower <- q25 - 1.5 * iqr
    iqr_upper <- q75 + 1.5 * iqr
    iqr_outliers <- method_data < iqr_lower | method_data > iqr_upper

    # Z-score outliers (threshold = 3)
    z_scores <- abs((method_data - mean(method_data)) / sd(method_data))
    z_outliers <- z_scores > 3

    # Modified z-score outliers
    median_val <- median(method_data)
    mad_val <- mad(method_data)
    modified_z_scores <- 0.6745 * (method_data - median_val) / mad_val
    modified_z_outliers <- abs(modified_z_scores) > 3.5

    outlier_results[[method]] <- list(
      n_observations = length(method_data),
      iqr_outliers = sum(iqr_outliers),
      iqr_percentage = sum(iqr_outliers) / length(method_data) * 100,
      z_outliers = sum(z_outliers, na.rm = TRUE),
      z_percentage = sum(z_outliers, na.rm = TRUE) / length(method_data) * 100,
      modified_z_outliers = sum(modified_z_outliers, na.rm = TRUE),
      modified_z_percentage = sum(modified_z_outliers, na.rm = TRUE) / length(method_data) * 100,
      outlier_values = list(
        iqr = method_data[iqr_outliers],
        z_score = method_data[z_outliers],
        modified_z = method_data[modified_z_outliers]
      )
    )
  }

  return(outlier_results)
}

#' Perform Distribution Tests
#'
#' @description Internal function for distribution normality tests
#' @param data_df Data frame with velocity results
#' @return List with distribution test results
#' @keywords internal
perform_distribution_tests <- function(data_df) {

  distribution_results <- list()

  for (method in unique(data_df$method)) {
    method_data <- data_df[data_df$method == method, "Vh_cm_hr"]
    method_data <- method_data[is.finite(method_data)]

    if (length(method_data) < 3) {
      distribution_results[[method]] <- list(
        n_observations = length(method_data),
        shapiro_test = list(p_value = NA, statistic = NA),
        note = "Insufficient data for distribution tests"
      )
      next
    }

    # Shapiro-Wilk test (for n <= 5000)
    shapiro_result <- NULL
    if (length(method_data) <= 5000) {
      tryCatch({
        shapiro_result <- shapiro.test(method_data)
      }, error = function(e) {
        shapiro_result <- list(p.value = NA, statistic = NA,
                               error = as.character(e))
      })
    }

    # Kolmogorov-Smirnov test against normal distribution
    ks_result <- NULL
    tryCatch({
      ks_result <- ks.test(method_data, "pnorm",
                           mean = mean(method_data), sd = sd(method_data))
    }, error = function(e) {
      ks_result <- list(p.value = NA, statistic = NA, error = as.character(e))
    })

    distribution_results[[method]] <- list(
      n_observations = length(method_data),
      shapiro_test = if (!is.null(shapiro_result)) {
        list(p_value = shapiro_result$p.value, statistic = shapiro_result$statistic)
      } else {
        list(p_value = NA, statistic = NA, note = "Sample too large for Shapiro test")
      },
      ks_test = list(p_value = ks_result$p.value, statistic = ks_result$statistic),
      normality_interpretation = interpret_normality_tests(shapiro_result, ks_result)
    )
  }

  return(distribution_results)
}

#' Calculate Method Correlations
#'
#' @description Internal function to calculate correlation matrix between methods
#' @param data_df Data frame with velocity results
#' @return Correlation matrix
#' @keywords internal
calculate_method_correlations <- function(data_df) {

  # Reshape data for correlation analysis
  wide_data <- reshape(
    data_df[, c("datetime", "pulse_id", "method", "Vh_cm_hr")],
    direction = "wide",
    idvar = c("datetime", "pulse_id"),
    timevar = "method"
  )

  # Extract just the velocity columns
  vel_cols <- grep("^Vh_cm_hr\\.", names(wide_data), value = TRUE)

  if (length(vel_cols) < 2) {
    return(NULL)
  }

  vel_data <- wide_data[, vel_cols, drop = FALSE]
  names(vel_data) <- gsub("^Vh_cm_hr\\.", "", names(vel_data))

  # Calculate correlation matrix
  cor_matrix <- cor(vel_data, use = "pairwise.complete.obs")

  return(cor_matrix)
}

# =============================================================================
# HELPER UTILITY FUNCTIONS
# =============================================================================

#' Create CSV Metadata Header
#'
#' @description Internal function to create metadata header for CSV files
#' @param metadata List with metadata information
#' @return Character vector with header lines
#' @keywords internal
create_csv_metadata_header <- function(metadata) {

  header_lines <- c(
    "# Sap Flow Data Export",
    paste("# Export Date:", Sys.time()),
    paste("# Package Version:", metadata$export_version %||% "Unknown"),
    paste("# Original File:", metadata$file_path %||% "Unknown"),
    paste("# Data Format:", metadata$format %||% "Unknown"),
    paste("# Import Date:", metadata$import_time %||% "Unknown"),
    "#",
    "# Columns:"
  )

  return(header_lines)
}

#' Create Text Metadata Header
#'
#' @description Internal function to create metadata header for text files
#' @param metadata List with metadata information
#' @return Character vector with header lines
#' @keywords internal
create_txt_metadata_header <- function(metadata) {

  header_lines <- c(
    "# Sap Flow Data Export - Tab Delimited Format",
    paste("# Export Date:", Sys.time()),
    paste("# Package Version:", metadata$export_version %||% "Unknown"),
    paste("# Original File:", metadata$file_path %||% "Unknown"),
    paste("# Data Format:", metadata$format %||% "Unknown"),
    "#"
  )

  return(header_lines)
}

#' Combine Data for CSV Export
#'
#' @description Internal function to combine multiple data tables for export
#' @param export_data List with export data
#' @return Data frame with combined data
#' @keywords internal
combine_data_for_csv <- function(export_data) {

  # Start with velocity results if available
  if (!is.null(export_data$velocity_results)) {
    combined_data <- export_data$velocity_results
  } else if (!is.null(export_data$measurements)) {
    combined_data <- export_data$measurements
  } else {
    stop("No data available for export")
  }

  return(combined_data)
}

#' Create Summary Sheet for Excel
#'
#' @description Internal function to create summary sheet for Excel export
#' @param velocity_results Data frame with velocity results
#' @return Data frame with summary information
#' @keywords internal
create_summary_sheet <- function(velocity_results) {

  # Basic summary statistics
  summary_stats <- data.frame(
    Metric = c("Total Measurements", "Methods Used", "Date Range Start",
               "Date Range End", "Mean Velocity (cm/hr)", "Median Velocity (cm/hr)",
               "Min Velocity (cm/hr)", "Max Velocity (cm/hr)"),
    Value = c(
      nrow(velocity_results),
      length(unique(velocity_results$method)),
      as.character(min(velocity_results$datetime, na.rm = TRUE)),
      as.character(max(velocity_results$datetime, na.rm = TRUE)),
      round(mean(velocity_results$Vh_cm_hr, na.rm = TRUE), 2),
      round(median(velocity_results$Vh_cm_hr, na.rm = TRUE), 2),
      round(min(velocity_results$Vh_cm_hr, na.rm = TRUE), 2),
      round(max(velocity_results$Vh_cm_hr, na.rm = TRUE), 2)
    ),
    stringsAsFactors = FALSE
  )

  return(summary_stats)
}

#' Standardise Data for Research Format
#'
#' @description Internal function to standardise data for research publications
#' @param velocity_results Data frame with velocity results
#' @return Data frame with standardised format
#' @keywords internal
standardise_for_research <- function(velocity_results) {

  # Standardise column names and order
  standard_cols <- c("datetime", "method", "velocity_cm_per_hr", "quality_flag",
                     "sensor_position", "pulse_id")

  standardised_data <- velocity_results

  # Rename columns to standard names
  if ("Vh_cm_hr" %in% names(standardised_data)) {
    names(standardised_data)[names(standardised_data) == "Vh_cm_hr"] <- "velocity_cm_per_hr"
  }

  # Ensure all standard columns exist
  for (col in standard_cols) {
    if (!col %in% names(standardised_data)) {
      standardised_data[[col]] <- NA
    }
  }

  # Select and order columns
  standardised_data <- standardised_data[, standard_cols[standard_cols %in% names(standardised_data)]]

  # Sort by datetime and method
  standardised_data <- standardised_data[order(standardised_data$datetime, standardised_data$method), ]

  return(standardised_data)
}

#' Create Research Metadata Section
#'
#' @description Internal function to create metadata section for research format
#' @param metadata List with metadata information
#' @return Character vector with metadata lines
#' @keywords internal
create_research_metadata_section <- function(metadata) {

  metadata_lines <- c(
    "# RESEARCH STANDARD FORMAT - SAP FLOW VELOCITY DATA",
    "#",
    "# Dataset Information:",
    paste("#   Original File:", metadata$file_path %||% "Not specified"),
    paste("#   Data Format:", metadata$format %||% "Not specified"),
    paste("#   Processing Date:", Sys.time()),
    paste("#   Package Version:", metadata$export_version %||% "Unknown"),
    "#",
    "# Column Definitions:",
    "#   datetime: Measurement timestamp (ISO 8601 format)",
    "#   method: Heat pulse velocity calculation method",
    "#   velocity_cm_per_hr: Sap flow velocity in cm per hour",
    "#   quality_flag: Data quality assessment flag",
    "#   sensor_position: Sensor position (inner/outer)",
    "#   pulse_id: Heat pulse identification number",
    "#",
    "# Quality Flags:",
    "#   OK: Normal measurement",
    "#   HIGH_VELOCITY: Unusually high velocity values",
    "#   NEGATIVE_FLOW: Reverse flow detected",
    "#   INFINITE: Mathematical calculation issues",
    "#   MISSING: Missing or invalid data",
    "#",
    "# Data begins below this line:",
    "#"
  )

  return(metadata_lines)
}

#' Create Export Summary
#'
#' @description Internal function to create export summary information
#' @param export_data List with export data
#' @param file_path Output file path
#' @param format Export format
#' @return List with export summary
#' @keywords internal
create_export_summary <- function(export_data, file_path, format) {

  summary_info <- list(
    format = format,
    file_path = file_path,
    export_time = Sys.time(),
    file_size_mb = round(file.info(file_path)$size / (1024^2), 2)
  )

  # Add data-specific summary
  if (!is.null(export_data$velocity_results)) {
    summary_info$velocity_measurements <- nrow(export_data$velocity_results)
    summary_info$methods_included <- unique(export_data$velocity_results$method)
    summary_info$date_range <- range(export_data$velocity_results$datetime, na.rm = TRUE)
  }

  if (!is.null(export_data$measurements)) {
    summary_info$raw_measurements <- nrow(export_data$measurements)
  }

  if (!is.null(export_data$diagnostics)) {
    summary_info$diagnostic_records <- nrow(export_data$diagnostics)
  }

  return(summary_info)
}

#' Interpret Normality Tests
#'
#' @description Internal function to interpret normality test results
#' @param shapiro_result Shapiro test result
#' @param ks_result Kolmogorov-Smirnov test result
#' @return Character string with interpretation
#' @keywords internal
interpret_normality_tests <- function(shapiro_result, ks_result) {

  # Significance level
  alpha <- 0.05

  interpretations <- c()

  # Shapiro-Wilk interpretation
  if (!is.null(shapiro_result) && !is.na(shapiro_result$p.value)) {
    if (shapiro_result$p.value > alpha) {
      interpretations <- c(interpretations, "Shapiro: Consistent with normal distribution")
    } else {
      interpretations <- c(interpretations, "Shapiro: Deviates from normal distribution")
    }
  }

  # Kolmogorov-Smirnov interpretation
  if (!is.null(ks_result) && !is.na(ks_result$p.value)) {
    if (ks_result$p.value > alpha) {
      interpretations <- c(interpretations, "KS: Consistent with normal distribution")
    } else {
      interpretations <- c(interpretations, "KS: Deviates from normal distribution")
    }
  }

  if (length(interpretations) == 0) {
    return("Unable to assess normality")
  }

  return(paste(interpretations, collapse = "; "))
}