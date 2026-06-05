# Documentation for bundled example data.
#
# The objects below are built by data-raw/example_data.R from a 10-day slice of
# a thesis deployment on Eucalyptus marginata (jarrah) near Perth, Western
# Australia. See that script to regenerate the slice from a fresh export.

#' Example heat pulse data
#'
#' A parsed heat pulse dataset covering a 10-day window (2024-11-01 to
#' 2024-11-10, UTC) from a single ICT SFM1 sap flow sensor deployed on a
#' *Eucalyptus marginata* (jarrah) tree near Perth, Western Australia. It is the
#' object returned by [read_heat_pulse_data()] applied to the bundled raw file
#' `Sample_HeatPulse_Data.txt` (see [sapfluxr_example_files]), and is intended
#' for examples, vignettes, and testing the downstream pipeline (heat pulse
#' velocity, corrections, sDMA, flux density, aggregation).
#'
#' Pulses are recorded every 30 minutes. The window contains one gap in the
#' pulse sequence, which the importer flags and back-fills with a placeholder
#' row, so the dataset also demonstrates gap detection.
#'
#' @format A list of class `heat_pulse_data` with the elements:
#' \describe{
#'   \item{measurements}{A tibble of 67,060 per-second temperature readings with
#'     columns `datetime` (POSIXct, UTC), `pulse_id` (integer), and the four
#'     probe temperatures `do`, `di`, `uo`, `ui` (numeric, degrees Celsius) for
#'     the outer/inner downstream and outer/inner upstream sensors.}
#'   \item{diagnostics}{A tibble with one row per pulse (480 rows) of logger
#'     diagnostics: `pulse_id`, `datetime`, battery voltage/current/temperature,
#'     external voltage/current, and `is_missing_pulse`.}
#'   \item{missing_pulse_info}{Details of any gaps detected in the pulse
#'     sequence.}
#'   \item{metadata}{A list describing the import: file name, detected format,
#'     pulse and measurement counts, and package version.}
#'   \item{gap_detection}{Summary of the pulse-sequence gap check.}
#'   \item{validation}{The data-quality validation result, including `valid`
#'     (logical) and any issues or warnings.}
#' }
#'
#' @source A 10-day slice of unpublished thesis field data (G. Joyce),
#'   *Eucalyptus marginata*, Perth, Western Australia. Raw temperatures only;
#'   no derived ratios.
#'
#' @seealso [example_weather], [sapfluxr_example_files], [read_heat_pulse_data()]
#'
#' @examples
#' data(example_heat_pulse)
#' str(example_heat_pulse$measurements)
#' range(example_heat_pulse$measurements$datetime)
"example_heat_pulse"

#' Example meteorological data
#'
#' Ten days of weather-station observations (2024-11-01 to 2024-11-10, local
#' time) recorded near the [example_heat_pulse] sap flow sensor, at 10-minute
#' intervals. It is the object returned by [read_weather_data()] applied to the
#' bundled raw file `Sample_Meteorological_Data.txt` (see
#' [sapfluxr_example_files]), and pairs with [example_heat_pulse] for
#' demonstrating vapour pressure deficit (VPD) and environmental analyses.
#'
#' The station records local clock time, so `datetime` carries the
#' `Australia/Perth` timezone (note this differs from the UTC timestamps in
#' [example_heat_pulse]).
#'
#' @format A tibble of 1,440 rows and 4 columns:
#' \describe{
#'   \item{datetime}{POSIXct timestamp (timezone `Australia/Perth`).}
#'   \item{air_temp_c}{Air temperature (degrees Celsius).}
#'   \item{relative_humidity}{Relative humidity (percent, 0-100).}
#'   \item{pressure_kpa}{Atmospheric pressure (kPa).}
#' }
#'
#' @source A 10-day slice of weather observations accompanying the thesis field
#'   data (G. Joyce), Perth, Western Australia.
#'
#' @seealso [example_heat_pulse], [sapfluxr_example_files], [read_weather_data()]
#'
#' @examples
#' data(example_weather)
#' head(example_weather)
#' summary(example_weather$air_temp_c)
"example_weather"

#' Bundled raw example files
#'
#' In addition to the loaded objects [example_heat_pulse] and [example_weather],
#' the package ships the original raw files they are built from in its
#' `extdata` directory, so the full import workflow can be demonstrated from
#' scratch. Reach them with [system.file()]:
#'
#' \describe{
#'   \item{`Sample_HeatPulse_Data.txt`}{Raw ICT "current" format heat pulse
#'     export (single-line JSON of raw probe temperatures). Read with
#'     [read_heat_pulse_data()].}
#'   \item{`Sample_Meteorological_Data.txt`}{Raw weather-station CSV (air
#'     temperature, relative humidity, atmospheric pressure). Read with
#'     [read_weather_data()].}
#'   \item{`Sample_Wood_Config.yml`}{An example wood/tree/site configuration for
#'     *Eucalyptus marginata*, including wood properties, sapwood and tree
#'     measurements, wound-correction parameters, and site location.}
#' }
#'
#' @examples
#' hp_file <- system.file(
#'   "extdata", "Sample_HeatPulse_Data.txt",
#'   package = "sapfluxr"
#' )
#' hp <- read_heat_pulse_data(hp_file)
#'
#' wx_file <- system.file(
#'   "extdata", "Sample_Meteorological_Data.txt",
#'   package = "sapfluxr"
#' )
#' wx <- read_weather_data(wx_file, confirm = FALSE, timezone = "Australia/Perth")
#'
#' @name sapfluxr_example_files
#' @seealso [example_heat_pulse], [example_weather]
NULL
