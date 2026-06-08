# data-raw/example_data.R
#
# Reproducible build script for the package's bundled example data.
#
# Full thesis exports (Eucalyptus marginata, Perth) are kept in
# data-raw/source/ (not shipped — data-raw is build-ignored). This script trims
# them to a 10-day window, writes the trimmed raw files into inst/extdata/, and
# builds the lazy-loaded data() objects. Re-run after replacing a source export:
#
#   Rscript data-raw/example_data.R
#
# Inputs (full, untrimmed):
#   data-raw/source/Sample_HeatPulse_Data.txt
#   data-raw/source/Sample_Meteorological_Data.txt
# Outputs:
#   inst/extdata/Sample_HeatPulse_Data.txt      (trimmed raw, ICT current JSON)
#   inst/extdata/Sample_Meteorological_Data.txt (trimmed raw, CSV)
#   data/example_heat_pulse.rda
#   data/example_weather.rda

library(sapfluxr)

# --- Configuration -----------------------------------------------------------

pkg_root  <- "E:/R/project/sapfluxr"
extdata   <- file.path(pkg_root, "inst", "extdata")
source_d  <- file.path(pkg_root, "data-raw", "source")

src_hp    <- file.path(source_d, "Sample_HeatPulse_Data.txt")
src_met   <- file.path(source_d, "Sample_Meteorological_Data.txt")
hp_file   <- file.path(extdata, "Sample_HeatPulse_Data.txt")
met_file  <- file.path(extdata, "Sample_Meteorological_Data.txt")

# Keep pulses/records strictly before this UTC instant (10-day window from the
# first record at 2024-11-01 00:00:00Z -> through 2024-11-10 23:30).
window_start <- as.POSIXct("2024-11-01 00:00:00", tz = "UTC")
cutoff       <- window_start + as.difftime(10, units = "days")

# --- 1. Trim the raw heat pulse file ----------------------------------------
# The file is a single line of "malformed" JSON: pulse records each introduced
# by the literal substring [{"date":" (the leading '[' belongs to the record),
# joined by commas, with the final record ending in "}," (no closing bracket).
# We split on that delimiter, keep the pulses whose timestamp is before the
# cutoff, and reassemble in the identical style so read_heat_pulse_data() parses
# the slice exactly as it would the original.

delim <- '[{"date":"'
raw   <- readChar(src_hp, file.info(src_hp)$size, useBytes = TRUE)

parts <- strsplit(raw, delim, fixed = TRUE)[[1]]
# parts[1] is empty (the file opens with the delimiter); parts[-1] are the pulse
# bodies, each starting with its date and ending with "}," .
stopifnot(parts[1] == "")
bodies <- parts[-1]

pulse_dates <- as.POSIXct(
  substr(bodies, 1, 20),               # "2024-11-01T00:00:00Z"
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)
keep_n <- sum(pulse_dates < cutoff)
message(sprintf("Heat pulse: keeping %d of %d pulses (cutoff %s)",
                keep_n, length(bodies), format(cutoff, tz = "UTC")))

trimmed <- paste0(delim, paste(bodies[seq_len(keep_n)], collapse = delim))
writeBin(charToRaw(trimmed), hp_file)
message(sprintf("Heat pulse: wrote %.2f MB", file.info(hp_file)$size / 1e6))

# --- 2. Trim the raw meteorological file ------------------------------------
# CSV with a UTF-8 BOM and a degree symbol in the header; preserve both by
# operating on raw bytes. Date_Time format is d/m/Y H:M (local clock).

met_raw <- readChar(src_met, file.info(src_met)$size, useBytes = TRUE)
nl      <- if (grepl("\r\n", met_raw, fixed = TRUE)) "\r\n" else "\n"
lines   <- strsplit(met_raw, nl, fixed = TRUE)[[1]]

header  <- lines[1]                       # retains BOM + degree symbol bytes
data_ln <- lines[-1]
data_ln <- data_ln[nzchar(data_ln)]       # drop any trailing blank line

met_dt  <- as.POSIXct(sub(",.*$", "", data_ln),
                      format = "%d/%m/%Y %H:%M", tz = "UTC")
met_keep <- met_dt < cutoff
message(sprintf("Weather: keeping %d of %d records", sum(met_keep), length(data_ln)))

met_out <- paste0(paste(c(header, data_ln[met_keep]), collapse = nl), nl)
writeBin(charToRaw(met_out), met_file)
message(sprintf("Weather: wrote %.1f KB", file.info(met_file)$size / 1e3))

# --- 3. Build the lazy-loaded data() objects --------------------------------

example_heat_pulse <- read_heat_pulse_data(
  hp_file,
  validate_data = TRUE,
  show_progress = FALSE
)
# Scrub the machine-specific absolute build path before shipping the object.
example_heat_pulse$metadata$file_path <- "inst/extdata/Sample_HeatPulse_Data.txt"
example_heat_pulse$metadata$file_name <- "Sample_HeatPulse_Data.txt"

example_weather <- read_weather_data(
  met_file,
  confirm  = FALSE,
  timezone = "Australia/Perth"
)

dir.create(file.path(pkg_root, "data"), showWarnings = FALSE)
save(example_heat_pulse,
     file = file.path(pkg_root, "data", "example_heat_pulse.rda"),
     compress = "xz")
save(example_weather,
     file = file.path(pkg_root, "data", "example_weather.rda"),
     compress = "xz")

message(sprintf("data/example_heat_pulse.rda: %.1f KB",
                file.info(file.path(pkg_root, "data", "example_heat_pulse.rda"))$size / 1e3))
message(sprintf("data/example_weather.rda: %.1f KB",
                file.info(file.path(pkg_root, "data", "example_weather.rda"))$size / 1e3))
message("Done.")
