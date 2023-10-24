library(tidyverse)
library(lubridate)
library(janitor)


# 1. Load and process pressure temp ---------------------------------------

# NOTE: Have not checked the date parsing super thoroughly yet. Only enough to
# make sure that it seems to be working.

# Crazy Lake data
crazy_air <- read.csv(file = "../data/water_level_data/Crazy_AirPressure_2010_2017.csv",
                      skip = 2)

crazy_air <- crazy_air %>%
  clean_names() %>%
  mutate(date_time = mdy_hms(date_time))

crazy_mid <- read.csv(file = "../data/water_level_data/Crazy_MidPressure_2011_2018.csv",
                  skip = 2)

crazy_mid <- crazy_mid %>%
  clean_names() %>%
  mutate(date_time = parse_date_time(crazy_mid$Date.Time,
                                     orders = "%m/%d/%Y %H:%M", exact = TRUE))

# Join the datasets and create a column for differences between air and mid pressure
combined_data <- inner_join(x = crazy_air %>%
                              select(date_time,
                                     air_value = value),
                            y = crazy_mid %>%
                              select(date_time,
                                     mid_value = value),
                            by = c("date_time")) %>%
  mutate(mid_less_air = mid_value - air_value)


# 2. Make plots -----------------------------------------------------------

# Plot air and mid pressure against time
air_mid_plot <- ggplot() +
  geom_point(data = crazy_air,
             aes(x = date_time, y = value), color = "red", size = 1) +
  geom_point(data = crazy_mid,
             aes(x = date_time, y = value), color = "blue", size = 1) +
  ggtitle(label = "Crazy Lake Air and Mid Water Pressure",
          subtitle = "Blue = mid pressure, red = air pressure")

ggsave(filename = "../figures/pressure_plots/crazy_air_mid_plot.png",
       plot = air_mid_plot, device = "png", width = 7, height = 5, units = "in")

# Plot mid - air ~ time
diff_plot <- ggplot(data = combined_data) +
  geom_point(aes(x = date_time, y = mid_less_air), size = 1) +
  ggtitle(label = "Difference between mid and air pressure")

ggsave(filename = "../figures/pressure_plots/crazy_pressure_diff_plot.png",
       plot = diff_plot, device = "png", width = 7, height = 5, units = "in")
