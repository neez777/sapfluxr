devtools::load_all()
hp_raw <- read_heat_pulse_data("E:/SapFlow/Tree data/Old data/SX01O201.txt", show_progress = FALSE, trim_incomplete_days = TRUE)
vh_results <- calc_heat_pulse_velocity(hp_raw, methods = "HRM", wood_properties = "eucalyptus", show_progress = FALSE)
sample_datetime <- hp_raw$measurements$datetime[5000]
cat("Testing datetime input:\n")
cat("Sample datetime:", format(sample_datetime, "%Y-%m-%d %H:%M:%S"), "\n\n")
p <- plot_heat_pulse_trace(hp_raw, vh_results, datetime = sample_datetime, sensor_position = "outer")
cat("\nSUCCESS! Plot created with datetime input\n")
cat("Title:", p$labels$title, "\n")
