# Weather Data Import and VPD Calculation Example
#
# This example demonstrates how to import weather data and calculate
# vapor pressure deficit (VPD) using sapfluxr

library(sapfluxr)
library(ggplot2)
library(dplyr)

# Example 1: Auto-detect columns (recommended)
# ============================================

# Import weather data - columns are auto-detected
weather <- read_weather_data("MU_BankWoodland_110925.csv")

# View the imported data
print(weather)

# Check the column mapping that was detected
attr(weather, "column_mapping")

# View summary statistics
summary(weather)


# Example 2: Manual column specification
# =======================================

# If auto-detection fails, specify columns manually
weather_manual <- read_weather_data(
  "MU_BankWoodland_110925.csv",
  datetime_col = "Date_Time",
  temp_col = "Air Temperature_°C ",  # Note: there's a trailing space
  rh_col = "Relative Humidity_%",
  pressure_col = "Atmospheric Pressure_kPa"
)


# Example 3: Calculate VPD (default Magnus equation)
# ==================================================

# Calculate VPD with default parameters
weather_vpd <- calc_vpd(weather)

# View VPD values
head(weather_vpd)

# VPD summary statistics
summary(weather_vpd$vpd_kpa)


# Example 4: VPD with all components
# ===================================

# Get saturated vapor pressure (SVP) and actual vapor pressure (AVP) too
weather_full <- calc_vpd(weather, return_components = TRUE)

# View all vapor pressure components
head(weather_full %>% select(datetime, air_temp_c, relative_humidity,
                              svp_kpa, avp_kpa, vpd_kpa))

# Verify VPD calculation: VPD = SVP - AVP
all.equal(weather_full$vpd_kpa,
          weather_full$svp_kpa - weather_full$avp_kpa)


# Example 5: Custom Magnus parameters
# ====================================

# For sub-zero temperatures or special applications
weather_custom <- calc_vpd(
  weather,
  magnus_coef = 17.5,       # Modified Magnus coefficient (α)
  magnus_base_temp = 240.0  # Modified Magnus base temperature (β, °C)
)


# Example 6: VPD from custom data frame
# ======================================

# You can calculate VPD from any data frame with temperature and RH
my_data <- data.frame(
  timestamp = seq(as.POSIXct("2024-01-01 00:00"), by = "hour", length.out = 24),
  temp = c(12, 11, 10, 10, 11, 13, 16, 19, 22, 24, 26, 27,
           28, 27, 26, 24, 21, 18, 16, 14, 13, 12, 12, 11),
  humidity = c(85, 88, 90, 92, 90, 85, 75, 65, 55, 50, 45, 42,
               40, 42, 45, 50, 58, 65, 72, 78, 82, 84, 85, 86)
)

my_data_vpd <- calc_vpd(
  my_data,
  temp_col = "temp",
  rh_col = "humidity"
)

print(my_data_vpd)


# Example 7: Visualise Weather Data
# ==================================

# Time series plot
library(tidyr)

weather_plot <- weather_vpd %>%
  select(datetime, air_temp_c, relative_humidity, vpd_kpa) %>%
  pivot_longer(
    cols = -datetime,
    names_to = "variable",
    values_to = "value"
  )

ggplot(weather_plot, aes(x = datetime, y = value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~variable, ncol = 1, scales = "free_y",
             labeller = labeller(variable = c(
               air_temp_c = "Air Temperature (°C)",
               relative_humidity = "Relative Humidity (%)",
               vpd_kpa = "VPD (kPa)"
             ))) +
  theme_minimal() +
  labs(title = "Weather Variables Over Time",
       x = "Datetime", y = "Value")

# VPD vs Temperature scatter plot
ggplot(weather_vpd, aes(x = air_temp_c, y = vpd_kpa)) +
  geom_point(aes(color = relative_humidity), size = 2, alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red", name = "RH (%)") +
  geom_smooth(method = "loess", se = TRUE, color = "black",
              linetype = "dashed") +
  theme_minimal() +
  labs(title = "VPD Increases with Temperature, Decreases with Humidity",
       x = "Air Temperature (°C)",
       y = "VPD (kPa)")

# Diurnal VPD pattern
weather_vpd %>%
  mutate(hour = as.numeric(format(datetime, "%H")) +
                as.numeric(format(datetime, "%M")) / 60) %>%
  ggplot(aes(x = hour, y = vpd_kpa)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  theme_minimal() +
  labs(title = "Diurnal VPD Pattern",
       x = "Hour of Day",
       y = "VPD (kPa)")


# Example 8: Identify Stable Environmental Conditions
# ====================================================

# For spacing correction, we want periods with stable VPD
stable_periods <- weather_vpd %>%
  mutate(date = as.Date(datetime),
         hour = as.numeric(format(datetime, "%H"))) %>%
  group_by(date) %>%
  summarise(
    vpd_mean = mean(vpd_kpa, na.rm = TRUE),
    vpd_sd = sd(vpd_kpa, na.rm = TRUE),
    vpd_cv = vpd_sd / vpd_mean,  # Coefficient of variation
    suitable_for_calibration = vpd_cv < 0.3  # Low variability
  )

# View stable periods
stable_periods %>% filter(suitable_for_calibration)


# Example 9: Handling Missing Data
# =================================

# Check for missing values
cat("Missing temperature values:", sum(is.na(weather_vpd$air_temp_c)), "\n")
cat("Missing RH values:", sum(is.na(weather_vpd$relative_humidity)), "\n")
cat("Missing VPD values:", sum(is.na(weather_vpd$vpd_kpa)), "\n")

# Identify temporal gaps
weather_gaps <- weather_vpd %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime),
                                         units = "mins"))) %>%
  filter(time_diff > 10)  # Expected interval: 10 minutes

if (nrow(weather_gaps) > 0) {
  cat("\nTemporal gaps detected:\n")
  print(weather_gaps %>% select(datetime, time_diff))
}


# Example 10: Data Quality Checks
# ================================

# Check for out-of-range values
quality_issues <- weather_vpd %>%
  filter(
    air_temp_c < -40 | air_temp_c > 50 |
    relative_humidity < 0 | relative_humidity > 100 |
    vpd_kpa < 0 | is.na(vpd_kpa)
  )

if (nrow(quality_issues) > 0) {
  cat("\nData quality issues detected:\n")
  print(quality_issues)
} else {
  cat("\nAll data passed quality checks!\n")
}


# Example 11: Export Processed Data
# ==================================

# Save processed weather data with VPD
readr::write_csv(weather_vpd, "weather_processed_with_vpd.csv")

# Save summary statistics
weather_summary <- weather_vpd %>%
  summarise(
    start_date = min(datetime),
    end_date = max(datetime),
    n_records = n(),
    temp_min = min(air_temp_c, na.rm = TRUE),
    temp_mean = mean(air_temp_c, na.rm = TRUE),
    temp_max = max(air_temp_c, na.rm = TRUE),
    rh_min = min(relative_humidity, na.rm = TRUE),
    rh_mean = mean(relative_humidity, na.rm = TRUE),
    rh_max = max(relative_humidity, na.rm = TRUE),
    vpd_min = min(vpd_kpa, na.rm = TRUE),
    vpd_mean = mean(vpd_kpa, na.rm = TRUE),
    vpd_max = max(vpd_kpa, na.rm = TRUE)
  )

readr::write_csv(weather_summary, "weather_summary.csv")

cat("\nProcessed data and summary exported successfully!\n")
