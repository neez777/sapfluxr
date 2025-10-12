# =============================================================================
# FILE: R/24_plot_utilities.R
# Plot Customization and Utility Functions for sapFluxR Package
# =============================================================================

#' Create Publication-Ready Plot Layout
#'
#' Arrange multiple plots in a publication-ready layout with proper sizing,
#' spacing, and labels for scientific publications.
#'
#' @param plots List of ggplot objects to arrange
#' @param layout Character string or matrix specifying layout: "grid", "column", "row", or custom matrix
#' @param title Optional overall title for the figure (default: NULL)
#' @param tag_levels Character vector for plot tags: "a", "A", "1", "i", "I" (default: "a")
#' @param widths Numeric vector of relative widths for columns (default: NULL for equal)
#' @param heights Numeric vector of relative heights for rows (default: NULL for equal)
#' @param guides Character string for legend collection: "collect", "keep", "auto" (default: "auto")
#'
#' @return A patchwork object (if patchwork available) or combined plot
#'
#' @examples
#' \dontrun{
#' # Create multiple plots
#' p1 <- plot_hpv_timeseries(vh_results, methods = "HRM")
#' p2 <- plot_method_comparison(vh_results)
#' p3 <- plot_diagnostics(vh_results)
#'
#' # Arrange in publication layout
#' pub_plot <- create_publication_layout(
#'   plots = list(p1, p2, p3),
#'   layout = "grid",
#'   title = "Sap Flow Analysis Results",
#'   tag_levels = "A"
#' )
#' }
#'
#' @export
create_publication_layout <- function(plots, layout = "grid", title = NULL,
                                      tag_levels = "a", widths = NULL, heights = NULL,
                                      guides = "auto") {

  if (!is.list(plots) || length(plots) == 0) {
    stop("plots must be a non-empty list of ggplot objects")
  }

  # Check that all elements are ggplot objects
  plot_classes <- sapply(plots, function(x) inherits(x, "ggplot"))
  if (!all(plot_classes)) {
    stop("All elements in plots list must be ggplot objects")
  }

  # Try to use patchwork if available
  if (requireNamespace("patchwork", quietly = TRUE)) {

    # Add plot tags
    tagged_plots <- patchwork::plot_annotation(tag_levels = tag_levels)

    # Create layout based on specification
    if (layout == "column") {
      result <- patchwork::wrap_plots(plots, ncol = 1, widths = widths, heights = heights)
    } else if (layout == "row") {
      result <- patchwork::wrap_plots(plots, nrow = 1, widths = widths, heights = heights)
    } else if (layout == "grid") {
      # Auto-determine best grid layout
      n_plots <- length(plots)
      if (n_plots <= 2) {
        ncol <- n_plots
      } else if (n_plots <= 4) {
        ncol <- 2
      } else if (n_plots <= 6) {
        ncol <- 3
      } else {
        ncol <- ceiling(sqrt(n_plots))
      }
      result <- patchwork::wrap_plots(plots, ncol = ncol, widths = widths, heights = heights)
    } else if (is.matrix(layout)) {
      # Custom layout matrix
      result <- patchwork::wrap_plots(plots, design = layout, widths = widths, heights = heights)
    } else {
      stop("layout must be 'grid', 'column', 'row', or a matrix")
    }

    # Handle legend collection
    if (guides == "collect") {
      result <- result + patchwork::plot_layout(guides = "collect")
    } else if (guides == "auto") {
      # Auto-collect if more than 2 plots with legends
      if (length(plots) > 2) {
        result <- result + patchwork::plot_layout(guides = "collect")
      }
    }

    # Add overall title and plot tags
    result <- result + tagged_plots
    if (!is.null(title)) {
      result <- result + patchwork::plot_annotation(title = title)
    }

    return(result)

  } else {
    warning("patchwork package not available. Returning first plot only. ",
            "Install patchwork with install.packages('patchwork') for full layout functionality.")
    return(plots[[1]])
  }
}

#' Save Plot with Publication Settings
#'
#' Save plots with optimised settings for publication, including proper DPI,
#' dimensions, and file formats commonly used in scientific journals.
#'
#' @param plot A ggplot object or patchwork object to save
#' @param filename Character string specifying the output filename with extension
#' @param format Character string specifying format: "pdf", "png", "tiff", "eps", "svg" (default: auto-detect from filename)
#' @param width Numeric width in inches (default: 7 for single column, 14 for double column)
#' @param height Numeric height in inches (default: 5, or auto-calculated to maintain aspect ratio)
#' @param dpi Numeric DPI for raster formats (default: 300 for publication quality)
#' @param units Character string for units: "in", "cm", "mm" (default: "in")
#' @param device Character string specifying device: "cairo", "ragg", "default" (default: "cairo" if available)
#' @param background Character string for background colour (default: "white")
#' @param compression Character string for TIFF compression: "lzw", "none", "jpeg" (default: "lzw")
#'
#' @return Invisibly returns the filename
#'
#' @examples
#' \dontrun{
#' # Save single plot for publication
#' p <- plot_hpv_timeseries(vh_results)
#' save_publication_plot(p, "sap_flow_timeseries.pdf", width = 7, height = 5)
#'
#' # Save high-resolution PNG
#' save_publication_plot(p, "figure1.png", width = 8, height = 6, dpi = 600)
#'
#' # Save TIFF for journal submission
#' save_publication_plot(p, "figure1.tiff", width = 174, height = 120, units = "mm")
#' }
#'
#' @export
save_publication_plot <- function(plot, filename, format = NULL, width = NULL, height = NULL,
                                  dpi = 300, units = "in", device = "cairo",
                                  background = "white", compression = "lzw") {

  if (!inherits(plot, c("ggplot", "patchwork"))) {
    stop("plot must be a ggplot or patchwork object")
  }

  # Auto-detect format from filename if not specified
  if (is.null(format)) {
    format <- tools::file_ext(filename)
    if (format == "") {
      stop("Cannot determine format from filename. Please specify format explicitly or use filename with extension.")
    }
  }

  format <- tolower(format)

  # Set default dimensions based on format and common journal requirements
  if (is.null(width)) {
    # Standard single-column width for most journals is ~3.5-4 inches
    # Double-column width is ~7 inches
    # Full page width is ~8.5 inches
    width <- if (format %in% c("pdf", "eps", "svg")) 7 else 8
  }

  if (is.null(height)) {
    # Auto-calculate height to maintain reasonable aspect ratio
    height <- width * 0.7  # Golden ratio approximation
  }

  # Choose appropriate device
  if (device == "cairo") {
    if (!capabilities("cairo")) {
      warning("Cairo not available. Using default device.")
      device <- "default"
    }
  } else if (device == "ragg") {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      warning("ragg package not available. Using default device.")
      device <- "default"
    }
  }

  # Create directory if it doesn't exist
  output_dir <- dirname(filename)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save based on format
  tryCatch({
    if (format == "pdf") {
      if (device == "cairo") {
        ggplot2::ggsave(filename, plot, device = cairo_pdf, width = width, height = height,
                        units = units, bg = background)
      } else {
        ggplot2::ggsave(filename, plot, device = "pdf", width = width, height = height,
                        units = units, bg = background)
      }
    } else if (format == "png") {
      if (device == "cairo") {
        ggplot2::ggsave(filename, plot, device = "cairo", width = width, height = height,
                        dpi = dpi, units = units, bg = background)
      } else if (device == "ragg") {
        ggplot2::ggsave(filename, plot, device = ragg::agg_png, width = width, height = height,
                        dpi = dpi, units = units, bg = background)
      } else {
        ggplot2::ggsave(filename, plot, device = "png", width = width, height = height,
                        dpi = dpi, units = units, bg = background)
      }
    } else if (format %in% c("tiff", "tif")) {
      ggplot2::ggsave(filename, plot, device = "tiff", width = width, height = height,
                      dpi = dpi, units = units, bg = background,
                      compression = compression)
    } else if (format == "eps") {
      if (device == "cairo") {
        ggplot2::ggsave(filename, plot, device = cairo_ps, width = width, height = height,
                        units = units, bg = background)
      } else {
        ggplot2::ggsave(filename, plot, device = "ps", width = width, height = height,
                        units = units, bg = background)
      }
    } else if (format == "svg") {
      ggplot2::ggsave(filename, plot, device = "svg", width = width, height = height,
                      units = units, bg = background)
    } else {
      stop("Unsupported format: ", format, ". Supported formats are: pdf, png, tiff, eps, svg")
    }

    message("Plot saved successfully to: ", filename)
    message("Dimensions: ", width, " x ", height, " ", units)
    if (format %in% c("png", "tiff")) {
      message("Resolution: ", dpi, " DPI")
    }

  }, error = function(e) {
    stop("Failed to save plot: ", e$message)
  })

  invisible(filename)
}

#' Create Plot with Inset
#'
#' Add an inset plot to a main plot, useful for showing detailed views,
#' correlation plots, or supplementary information.
#'
#' @param main_plot A ggplot object for the main plot
#' @param inset_plot A ggplot object for the inset plot
#' @param position Character string or numeric vector specifying inset position:
#'   "topleft", "topright", "bottomleft", "bottomright", or c(xmin, xmax, ymin, ymax) in npc units
#' @param width Numeric width of inset as fraction of main plot (default: 0.4)
#' @param height Numeric height of inset as fraction of main plot (default: 0.4)
#' @param border Logical whether to add border around inset (default: TRUE)
#' @param border_colour Character string for border colour (default: "black")
#' @param background Character string for inset background colour (default: "white")
#'
#' @return A ggplot object with inset
#'
#' @examples
#' \dontrun{
#' # Create main time series plot
#' main_plot <- plot_hpv_timeseries(vh_results)
#'
#' # Create correlation inset
#' inset_plot <- plot_method_comparison(vh_results, comparison_type = "correlation")
#'
#' # Combine with inset
#' combined_plot <- create_plot_with_inset(main_plot, inset_plot, position = "topright")
#' }
#'
#' @export
create_plot_with_inset <- function(main_plot, inset_plot, position = "topright",
                                   width = 0.4, height = 0.4, border = TRUE,
                                   border_colour = "black", background = "white") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for inset functionality")
  }

  if (!inherits(main_plot, "ggplot") || !inherits(inset_plot, "ggplot")) {
    stop("Both main_plot and inset_plot must be ggplot objects")
  }

  # Convert position to coordinates if needed
  if (is.character(position)) {
    coords <- switch(position,
                     "topleft" = c(0.05, 0.05 + width, 1 - height - 0.05, 0.95),
                     "topright" = c(0.95 - width, 0.95, 1 - height - 0.05, 0.95),
                     "bottomleft" = c(0.05, 0.05 + width, 0.05, 0.05 + height),
                     "bottomright" = c(0.95 - width, 0.95, 0.05, 0.05 + height),
                     stop("Invalid position. Use 'topleft', 'topright', 'bottomleft', 'bottomright', or numeric coordinates.")
    )
  } else if (is.numeric(position) && length(position) == 4) {
    coords <- position
  } else {
    stop("position must be a character string or numeric vector of length 4")
  }

  # Modify inset plot for embedding
  inset_modified <- inset_plot +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background, colour = if(border) border_colour else NA),
      panel.border = if(border) ggplot2::element_rect(colour = border_colour, fill = NA) else ggplot2::element_blank(),
      legend.position = "none",  # Remove legend from inset
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 6),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )

  # Try to use patchwork for inset functionality
  if (requireNamespace("patchwork", quietly = TRUE)) {
    result <- main_plot +
      patchwork::inset_element(inset_modified,
                               left = coords[1], bottom = coords[3],
                               right = coords[2], top = coords[4])
    return(result)
  } else {
    warning("patchwork package not available for inset functionality. Returning main plot only.")
    return(main_plot)
  }
}

#' Apply Journal-Specific Formatting
#'
#' Apply formatting specifications for common scientific journals to ensure
#' plots meet submission requirements.
#'
#' @param plot A ggplot object to format
#' @param journal Character string specifying journal: "nature", "science", "plos", "frontiers", "elsevier", "springer", "generic"
#' @param figure_type Character string: "single_column", "double_column", "full_page"
#' @param font_family Character string for font family (default: journal-specific)
#' @param base_size Numeric base font size (default: journal-specific)
#'
#' @return A ggplot object with journal-specific formatting applied
#'
#' @examples
#' \dontrun{
#' # Format for Nature submission
#' p <- plot_hpv_timeseries(vh_results)
#' p_nature <- apply_journal_formatting(p, journal = "nature", figure_type = "single_column")
#'
#' # Format for PLoS ONE
#' p_plos <- apply_journal_formatting(p, journal = "plos", figure_type = "double_column")
#' }
#'
#' @export
apply_journal_formatting <- function(plot, journal = "generic", figure_type = "single_column",
                                     font_family = NULL, base_size = NULL) {

  if (!inherits(plot, "ggplot")) {
    stop("plot must be a ggplot object")
  }

  journal <- tolower(journal)
  figure_type <- match.arg(figure_type, c("single_column", "double_column", "full_page"))

  # Journal-specific formatting parameters
  journal_specs <- list(
    nature = list(
      font_family = "Arial",
      single_column_size = 8,
      double_column_size = 9,
      full_page_size = 10,
      line_width = 0.5,
      point_size = 1.5
    ),
    science = list(
      font_family = "Arial",
      single_column_size = 7,
      double_column_size = 8,
      full_page_size = 9,
      line_width = 0.5,
      point_size = 1.2
    ),
    plos = list(
      font_family = "Arial",
      single_column_size = 8,
      double_column_size = 10,
      full_page_size = 12,
      line_width = 0.6,
      point_size = 1.5
    ),
    frontiers = list(
      font_family = "Arial",
      single_column_size = 9,
      double_column_size = 11,
      full_page_size = 12,
      line_width = 0.7,
      point_size = 2.0
    ),
    elsevier = list(
      font_family = "Arial",
      single_column_size = 8,
      double_column_size = 9,
      full_page_size = 10,
      line_width = 0.5,
      point_size = 1.5
    ),
    springer = list(
      font_family = "Arial",
      single_column_size = 8,
      double_column_size = 9,
      full_page_size = 10,
      line_width = 0.5,
      point_size = 1.5
    ),
    generic = list(
      font_family = "",
      single_column_size = 10,
      double_column_size = 12,
      full_page_size = 14,
      line_width = 0.7,
      point_size = 2.0
    )
  )

  if (!journal %in% names(journal_specs)) {
    stop("Unknown journal. Available options: ", paste(names(journal_specs), collapse = ", "))
  }

  specs <- journal_specs[[journal]]

  # Determine font size based on figure type
  if (is.null(base_size)) {
    base_size <- switch(figure_type,
                        "single_column" = specs$single_column_size,
                        "double_column" = specs$double_column_size,
                        "full_page" = specs$full_page_size
    )
  }

  if (is.null(font_family)) {
    font_family <- specs$font_family
  }

  # Apply journal-specific theme modifications
  journal_theme <- ggplot2::theme(
    text = ggplot2::element_text(family = font_family, size = base_size),
    axis.title = ggplot2::element_text(size = base_size),
    axis.text = ggplot2::element_text(size = base_size - 1),
    legend.title = ggplot2::element_text(size = base_size),
    legend.text = ggplot2::element_text(size = base_size - 1),
    plot.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
    strip.text = ggplot2::element_text(size = base_size),

    # Line and point sizes
    line = ggplot2::element_line(size = specs$line_width),
    axis.line = ggplot2::element_line(size = specs$line_width),
    axis.ticks = ggplot2::element_line(size = specs$line_width),
    panel.border = ggplot2::element_rect(size = specs$line_width),

    # Remove plot background for most journals
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),

    # Adjust margins for compact layout
    plot.margin = ggplot2::margin(5, 5, 5, 5)
  )

  # Apply the formatting
  formatted_plot <- plot + journal_theme

  # Add specific adjustments for different journals
  if (journal %in% c("nature", "science")) {
    # Very compact style for high-impact journals
    formatted_plot <- formatted_plot +
      ggplot2::theme(
        legend.key.size = ggplot2::unit(0.8, "lines"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
        panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.25),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (journal == "plos") {
    # PLoS style with slightly more spacing
    formatted_plot <- formatted_plot +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "grey85", size = 0.3),
        panel.grid.minor = ggplot2::element_line(colour = "grey95", size = 0.2)
      )
  }

  return(formatted_plot)
}

#' Create Colour Palette for Publications
#'
#' Create colourblind-friendly and print-friendly colour palettes optimised
#' for scientific publications.
#'
#' @param n_colours Integer number of colours needed
#' @param palette_type Character string: "colourblind_friendly", "print_friendly", "high_contrast", "sequential", "diverging"
#' @param reverse Logical whether to reverse the palette (default: FALSE)
#'
#' @return A character vector of colour codes
#'
#' @examples
#' \dontrun{
#' # Colourblind-friendly palette for method comparison
#' colours <- create_publication_palette(4, "colourblind_friendly")
#'
#' # High contrast for black and white printing
#' colours_bw <- create_publication_palette(3, "print_friendly")
#' }
#'
#' @export
create_publication_palette <- function(n_colours, palette_type = "colourblind_friendly", reverse = FALSE) {

  if (!is.numeric(n_colours) || n_colours <= 0) {
    stop("n_colours must be a positive integer")
  }

  palette_type <- match.arg(palette_type, c("colourblind_friendly", "print_friendly", "high_contrast", "sequential", "diverging"))

  # Define colour palettes
  palettes <- list(
    colourblind_friendly = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0173B2", "#D55E00", "#CC78BC", "#999999"),
    print_friendly = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0173B2", "#D55E00", "#CC78BC"),
    high_contrast = c("#000000", "#FFFFFF", "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF"),
    sequential = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000"),
    diverging = c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", "#C7EAE5", "#80CDC1", "#35978F", "#01665E")
  )

  base_palette <- palettes[[palette_type]]

  if (n_colours <= length(base_palette)) {
    colours <- base_palette[1:n_colours]
  } else {
    # Use colourRampPalette to interpolate more colours
    colour_function <- grDevices::colourRampPalette(base_palette)
    colours <- colour_function(n_colours)
  }

  if (reverse) {
    colours <- rev(colours)
  }

  return(colours)
}

#' Add Plot Annotations for Publications
#'
#' Add common annotations needed for scientific publications such as
#' statistical significance indicators, reference lines, and text annotations.
#'
#' @param plot A ggplot object to annotate
#' @param annotations List of annotation specifications
#' @param ... Additional arguments passed to annotation functions
#'
#' @return A ggplot object with annotations added
#'
#' @examples
#' \dontrun{
#' # Add significance indicators and reference line
#' p <- plot_method_comparison(vh_results)
#' annotations <- list(
#'   reference_line = list(type = "hline", yintercept = 0, linetype = "dashed"),
#'   significance = list(type = "text", x = 10, y = 20, label = "p < 0.001", size = 3),
#'   equation = list(type = "text", x = 15, y = 18, label = "y = 1.2x + 0.5", size = 3)
#' )
#' p_annotated <- add_plot_annotations(p, annotations)
#' }
#'
#' @export
add_plot_annotations <- function(plot, annotations, ...) {

  if (!inherits(plot, "ggplot")) {
    stop("plot must be a ggplot object")
  }

  if (!is.list(annotations)) {
    stop("annotations must be a list")
  }

  annotated_plot <- plot

  for (ann_name in names(annotations)) {
    ann <- annotations[[ann_name]]

    if (!is.list(ann) || is.null(ann$type)) {
      warning("Skipping annotation '", ann_name, "': must be a list with 'type' element")
      next
    }

    tryCatch({
      if (ann$type == "hline") {
        annotated_plot <- annotated_plot +
          ggplot2::geom_hline(yintercept = ann$yintercept,
                              linetype = ann$linetype %||% "solid",
                              colour = ann$colour %||% "black",
                              size = ann$size %||% 0.5,
                              alpha = ann$alpha %||% 1)
      } else if (ann$type == "vline") {
        annotated_plot <- annotated_plot +
          ggplot2::geom_vline(xintercept = ann$xintercept,
                              linetype = ann$linetype %||% "solid",
                              colour = ann$colour %||% "black",
                              size = ann$size %||% 0.5,
                              alpha = ann$alpha %||% 1)
      } else if (ann$type == "text") {
        annotated_plot <- annotated_plot +
          ggplot2::annotate("text", x = ann$x, y = ann$y,
                            label = ann$label,
                            size = ann$size %||% 3,
                            colour = ann$colour %||% "black",
                            hjust = ann$hjust %||% 0.5,
                            vjust = ann$vjust %||% 0.5,
                            fontface = ann$fontface %||% "plain")
      } else if (ann$type == "rect") {
        annotated_plot <- annotated_plot +
          ggplot2::annotate("rect", xmin = ann$xmin, xmax = ann$xmax,
                            ymin = ann$ymin, ymax = ann$ymax,
                            fill = ann$fill %||% "grey",
                            alpha = ann$alpha %||% 0.3,
                            colour = ann$colour %||% NA)
      } else if (ann$type == "arrow") {
        annotated_plot <- annotated_plot +
          ggplot2::annotate("segment", x = ann$x, y = ann$y,
                            xend = ann$xend, yend = ann$yend,
                            colour = ann$colour %||% "black",
                            size = ann$size %||% 0.5,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")))
      } else {
        warning("Unknown annotation type: ", ann$type)
      }
    }, error = function(e) {
      warning("Error adding annotation '", ann_name, "': ", e$message)
    })
  }

  return(annotated_plot)
}

# Helper function for null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a