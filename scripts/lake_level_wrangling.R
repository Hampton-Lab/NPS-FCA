# This script will eventually bring in lake level data, and pressure transducer
# data, and do some badass QA/QC and stuff


# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(plyr)
library(tidyverse)
library(readxl)


# 2. Read in revelant data files ------------------------------------------

# Read in data
crazy_air0 <- read.csv(file = file.path("..",
                                        "data",
                                        "water_level_data",
                                        "Crazy_AirPressure_2010_2017.csv"),
                       stringsAsFactors = FALSE,
                       skip = 2)

crazy_mid0 <- read.csv(file = file.path("..",
                                        "data",
                                        "water_level_data",
                                        "Crazy_MidPressure_2011_2018.csv"),
                       stringsAsFactors = FALSE,
                       skip = 2)

milk_air0 <- read.csv(file = file.path("..",
                                       "data",
                                       "water_level_data",
                                       "Milk_AirPressure_2008_2018.csv"),
                      stringsAsFactors = FALSE,
                      skip = 2)

milk_mid0 <- read.csv(file = file.path("..",
                                       "data",
                                       "water_level_data",
                                       "Milk_Mid_Pressure_2008_2018.csv"),
                      stringsAsFactors = FALSE,
                      skip = 2)

crazy_air <- crazy_air0
crazy_mid <- crazy_mid0
milk_air <- milk_air0
milk_mid <- milk_mid0

# Lake Level
lake_level0 <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b304_Lake_Level_Events_20190923_121946.xlsx"))

lake_level <- lake_level0


# Clean + wrangle data
crazy_air <- clean_names(crazy_air) %>%
  mutate(lakename = "Crazy",
         hour = hour(mdy_hms(date_time)),
         date = date(mdy_hms(date_time))) %>%
  dplyr::rename(value_air = value)

crazy_mid <- clean_names(crazy_mid)  %>%
  mutate(lakename = "Crazy",
         hour = hour(mdy_hms(date_time, truncated = 1)),
         date = date(mdy_hms(date_time, truncated = 1))) %>%
  dplyr::rename(value_mid = value)

milk_air <- clean_names(milk_air)  %>%
  mutate(lakename = "Milk",
         hour = hour(mdy_hms(date_time, truncated = 1)),
         date = date(mdy_hms(date_time, truncated = 1))) %>%
  dplyr::rename(value_air = value)

milk_mid <- clean_names(milk_mid)  %>%
  mutate(lakename = "Milk",
         hour = hour(mdy_hms(date_time, truncated = 1)),
         date = date(mdy_hms(date_time, truncated = 1))) %>%
  dplyr::rename(value_mid = value)


lake_level <- clean_names(lake_level)

# Note that if we want to have a true timestamp for lake_level we can extract
# a start time for some of the event_ids using the Events_export excel file


crazy_merge <- inner_join(x = crazy_air, y = crazy_mid,
                          by = c("lakename", "date", "hour"))

milk_merge <- inner_join(x = milk_air, y = milk_mid,
                         by = c("lakename", "date", "hour"))

pressure <- rbind(crazy_merge, milk_merge)

pressure <- pressure %>%
  mutate(value_midminusair = value_mid - value_air)


# Plot data





# QA/QC checks



# Make some decisions 



# Generate output files



