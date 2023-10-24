
# 1. Load packages --------------------------------------------------------

library(data.table)
library(janitor)
library(lubridate)
library(plyr)
library(tidyverse)
library(zoo)


# 2. Read in all csv files ------------------------------------------------

# Create a list of all files provided
all_temp_files <- list.files(path = file.path("..",
                                              "data",
                                              "FCA_ContinuousTempData"),
                             recursive = T, pattern = "*.csv", full.names = T)

# Read in all files simultaneously
temp_file_contents <- sapply(X = all_temp_files, FUN = fread,
                             stringsAsFactors = FALSE, integer64 = "character",
                             skip = "Date-Time", data.table = FALSE,
                             simplify = FALSE)

# This creates a list of 80 data frames, one for each csv
str(temp_file_contents)

# Same number of file names and data frames created?
length(all_temp_files) == length(temp_file_contents)

# Shorten the filenames we pulled to use as a source of metadata
short_names <- gsub(x = all_temp_files,
                    pattern = "../data/FCA_ContinuousTempData/FCA_ContinuousTempData/",
                    replacement = "")

# Combine data with corresponding filenames and collapse to a single data frame
all_temp_merge <- map2_df(.x = temp_file_contents, .y = short_names,
                          .f = ~update_list(.x, location = .y))

# No longer needed:
rm(temp_file_contents)

head(all_temp_merge)
tail(all_temp_merge)


# 3. Clean the dataset ----------------------------------------------------

all_temp_merge <- all_temp_merge %>%
  # Separate the filename into park and measurement*lake data
  separate(col = "location", into = c("Park", "Lake_string"), sep = "/") %>%
  mutate(Lake_string = gsub(x = Lake_string,
                            pattern = "_POR.csv",
                            replacement = "")) %>%
  # Separate the measurement*lake data into separate columns
  separate(col = "Lake_string", into = c("Measure", "Lake"), sep = "_")

# Note to self: Might be best to go back and pull the lake codes from the
# top line of each excel doc?

# Reformat names for use
all_temp_merge <- clean_names(all_temp_merge)

# Reformat date-time for use
all_temp_merge <- all_temp_merge %>%
  mutate(date_time = mdy_hms(x = date_time))

head(all_temp_merge)


# 4. Sumarize dataset -----------------------------------------------------

# By day:
daily_summary_temp_data <- all_temp_merge %>%
  mutate(obs_year = year(date_time),
         obs_month = month(date_time),
         obs_day = day(date_time)) %>%
  group_by(obs_year, obs_month, obs_day, park, measure, lake) %>%
  summarise(mean_value = mean(value, na.rm = T),
            min_value = min(value, na.rm = T), 
            max_value = max(value, na.rm = T),
            median_value = median(value, na.rm = T),
            perc_10 = quantile(value, probs = 0.1, na.rm = T),
            perc_90 = quantile(value, probs = 0.9, na.rm = T))

# Warnings thrown by min() and max() functions here. Disappear if na.rm = FALSE
# And checking the values for infinites doesn't produce anything:
daily_summary_temp_data %>%
  filter_all(all_vars(is.infinite(.)))

# Export
write.csv(x = daily_summary_temp_data,
          file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "all-daily-temp-summaries.csv"),
          row.names = FALSE)

# By month:
monthly_summary_temp_data <- all_temp_merge %>%
  mutate(obs_year = year(date_time),
         obs_month = month(date_time)) %>%
  group_by(obs_year, obs_month, park, measure, lake) %>%
  summarise(mean_value = mean(value, na.rm = T),
            min_value = min(value, na.rm = T), 
            max_value = max(value, na.rm = T),
            median_value = median(value, na.rm = T),
            perc_10 = quantile(value, probs = 0.1, na.rm = T),
            perc_90 = quantile(value, probs = 0.9, na.rm = T))

# Warnings thrown by min() and max() functions here. Disappear if na.rm = FALSE
# And checking the values for infinites doesn't produce anything. However a
# skim in Excel shows that there are 1712 Inf values produced for min_value.
monthly_summary_temp_data %>%
  filter_all(all_vars(is.infinite(.)))

# Export
write.csv(x = monthly_summary_temp_data,
          file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "all-monthly-temp-summaries.csv"),
          row.names = FALSE)


