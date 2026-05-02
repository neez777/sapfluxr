# R/utils_predawn.R
# Predawn window filtering utilities
#
# Provides consistent predawn-period filtering for VPD stability detection
# and sap flow baseline calculations. Supports static (fixed-clock) and
# dynamic (dawn-relative) windows, both with and without midnight wrapping.

#' Resolve Predawn Window to Integer Hour Vector
#'
#' Converts a 2-element window specification and mode into an integer vector
#' of hours (0-23) that fall within the predawn period.
#'
#' @param window Numeric vector of length 2:
#'   \itemize{
#'     \item \strong{static}: \code{c(start_hour, end_hour)}  - inclusive start,
#'       exclusive end. E.g., \code{c(2, 6)} yields hours 2, 3, 4, 5.
#'       Wraps midnight if \code{start >= end}: \code{c(22, 4)} yields
#'       22, 23, 0, 1, 2, 3.
#'     \item \strong{dynamic}: \code{c(hours_before_dawn, hours_after_dawn)}  -
#'       both values specify how many hours \emph{before} astronomical dawn to
#'       include. E.g., \code{c(4, 1)} with \code{dawn_hour = 6.5} yields
#'       hours 2, 3, 4, 5.
#'   }
#' @param mode Character, one of \code{"static"} (default) or \code{"dynamic"}.
#' @param dawn_hour Numeric. Decimal hour of astronomical dawn (0-24). Required
#'   when \code{mode = "dynamic"}; ignored otherwise.
#'
#' @return Integer vector of hours (elements in 0-23).
#' @export
resolve_predawn_hours <- function(window,
                                  mode      = c("static", "dynamic"),
                                  dawn_hour = NULL) {
  mode <- match.arg(mode)

  if (length(window) != 2) {
    stop("window must be a numeric vector of length 2.")
  }

  start <- window[1]
  end   <- window[2]

  if (mode == "dynamic") {
    if (is.null(dawn_hour)) {
      stop("dawn_hour is required when mode = 'dynamic'.")
    }
    start <- floor((dawn_hour - start) %% 24)
    end   <- floor((dawn_hour - end)   %% 24)
  }

  start <- as.integer(start)
  end   <- as.integer(end)

  if (start < end) {
    seq(start, end - 1L)
  } else {
    c(seq(start, 23L), seq(0L, end - 1L))
  }
}


#' Filter Data Frame to Predawn Hours
#'
#' Subsets a data frame to rows whose \code{datetime} column falls within the
#' predawn window. Supports static (fixed-clock) and dynamic (dawn-relative)
#' windows via \code{\link{resolve_predawn_hours}}.
#'
#' @param data A data frame with a \code{datetime} (POSIXct) column.
#' @param window Numeric vector of length 2. See \code{\link{resolve_predawn_hours}}.
#' @param mode Character, \code{"static"} (default) or \code{"dynamic"}.
#' @param dawn_times POSIXct vector of dawn times, one per unique date in
#'   \code{data}. Required when \code{mode = "dynamic"}, ignored otherwise.
#' @param tz Character. IANA timezone name (e.g. \code{"Australia/Perth"}).
#'   When supplied, both \code{data$datetime} and \code{dawn_times} are
#'   converted to this timezone before hour extraction so that the predawn
#'   window is always evaluated in local time. If \code{NULL} (default),
#'   datetimes are used as-is — caller is responsible for ensuring both sides
#'   of the comparison are already in the same timezone.
#'
#' @return A subset of \code{data} restricted to predawn rows.
#' @export
filter_predawn <- function(data,
                           window,
                           mode       = "static",
                           dawn_times = NULL,
                           tz         = NULL) {

  if (!inherits(data$datetime, "POSIXct")) {
    data$datetime <- as.POSIXct(data$datetime)
  }

  # Align both datetimes to the site timezone before extracting hours so that
  # UTC-stored logger data and suncalc dawn times (returned in the requested tz)
  # are compared on equal footing.
  if (!is.null(tz) && nzchar(tz)) {
    data$datetime <- lubridate::with_tz(data$datetime, tz)
    if (!is.null(dawn_times)) {
      dawn_times <- lubridate::with_tz(dawn_times, tz)
    }
  }

  data$hour <- lubridate::hour(data$datetime)
  data$date <- as.Date(data$datetime, tz = if (!is.null(tz) && nzchar(tz)) tz else "UTC")

  if (mode == "static") {
    hours <- resolve_predawn_hours(window, mode = "static")
    return(data[data$hour %in% hours, , drop = FALSE])
  }

  if (is.null(dawn_times)) {
    stop("dawn_times is required when mode = 'dynamic'.")
  }

  dawn_df <- data.frame(
    date      = as.Date(format(dawn_times, "%Y-%m-%d")),
    dawn_hour = as.numeric(format(dawn_times, "%H")) +
                as.numeric(format(dawn_times, "%M")) / 60,
    stringsAsFactors = FALSE
  )

  data <- merge(data, dawn_df, by = "date", all.x = FALSE)

  keep <- vapply(seq_len(nrow(data)), function(i) {
    hours <- resolve_predawn_hours(window, mode = "dynamic",
                                   dawn_hour = data$dawn_hour[i])
    data$hour[i] %in% hours
  }, logical(1L))

  data[keep, setdiff(names(data), "dawn_hour"), drop = FALSE]
}
