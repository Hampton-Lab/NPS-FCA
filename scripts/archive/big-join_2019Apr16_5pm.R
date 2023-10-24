
# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)


# 2. Read in revelant data files ------------------------------------------

# Physiography
physio <- read_excel(
  path = file.path("..",
                   "documents",
                   "study-site-tables.xlsx"))

# Water metrics
lake_level <- read_excel(
  path = file.path("..",
                   "NPS_NCCN_Mtn_Lakes_Exports_20180917",
                   "qs_b304_Lake_Level_Events_20180917_170103.xlsx"))

water_chem <- read_excel(
  path = file.path("..",
                   "NPS_NCCN_Mtn_Lakes_Exports_20180917",
                   "qs_b344_Water_Chemistry_Data_select_20180917_170340.xlsx"))

water_prof <- read_excel(
  path = file.path("..",
                   "NPS_NCCN_Mtn_Lakes_Exports_20180917",
                   "qs_b364_Water_Column_Profile_Data_20180917_170436.xlsx"))

# Temp data summary
daily_temperature <- read.csv(
  file = file.path("..",
                   "all-daily-temp-summaries.csv"),
  stringsAsFactors = FALSE)

monthly_temperature <- read.csv(
  file = file.path("..",
                   "all-monthly-temp-summaries.csv"),
  stringsAsFactors = FALSE)

# 2. Pre-join processing --------------------------------------------------

water_chem <- clean_names(water_chem) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

# I rename the columns, should we just leave as-is?
daily_temperature <- daily_temperature %>%
  mutate(event_date = as.Date(paste(obs_year, obs_month, obs_day, sep = "-"),
                              format = "%Y-%m-%d")) %>%
  rename(event_year = obs_year, event_month = obs_month, event_day = obs_day,
         park_code = park)

# This is sketchy, sorry! Some crosswalking that should be automated.
daily_temperature <- daily_temperature %>%
  mutate(site_code = case_when(
    lake == "Milk" ~ "498",
    lake == "Heather" ~ "263",
    lake == "Connie" ~ "627",
    lake == "LP19" ~ lake,        
    lake == "Pallisades" ~ "LH14",
    lake == "Allen" ~ "LN03",
    lake == "LowerBlum" ~ "LS-07-01",
    lake == "UpperTriplet" ~ "SM-02-02",
    lake == "Ferry" ~ "138",
    lake == "LH15" ~ lake,
    lake == "Blue" ~ "LZ35",
    lake == "Deadwood" ~ "LW32",
    lake == "Bowan" ~ "MR-12-01",
    lake == "EasyRidge" ~ "MC-03-01",   
    lake == "LowerEast" ~ "MC-14-02",
    lake == "LowerSilent" ~ "MA-03-01",
    lake == "Gladys" ~ "106",
    lake == "Crazy" ~ "426",
    lake == "LaCrosse" ~ "520"
  ))

# daily_temperature_long <- gather(data = daily_temperature, key = variable,
#                                  value = value)

# temps: variable = mean_month_air

monthly_temperature <- monthly_temperature %>%
  rename(event_year = obs_year, event_month = obs_month, park_code = park)

monthly_temperature <- monthly_temperature %>%
  mutate(site_code = case_when(
    lake == "Milk" ~ "498",
    lake == "Heather" ~ "263",
    lake == "Connie" ~ "627",
    lake == "LP19" ~ lake,        
    lake == "Pallisades" ~ "LH14",
    lake == "Allen" ~ "LN03",
    lake == "LowerBlum" ~ "LS-07-01",
    lake == "UpperTriplet" ~ "SM-02-02",
    lake == "Ferry" ~ "138",
    lake == "LH15" ~ lake,
    lake == "Blue" ~ "LZ35",
    lake == "Deadwood" ~ "LW32",
    lake == "Bowan" ~ "MR-12-01",
    lake == "EasyRidge" ~ "MC-03-01",   
    lake == "LowerEast" ~ "MC-14-02",
    lake == "LowerSilent" ~ "MA-03-01",
    lake == "Gladys" ~ "106",
    lake == "Crazy" ~ "426",
    lake == "LaCrosse" ~ "520"
  ))

physio <- physio %>%
  mutate(site_code = case_when(
    Lake == "Milk Lake" ~ "498",
    Lake == "Heather Lake" ~ "263",
    Lake == "Lake Connie" ~ "627",
    Lake == "Lake LP19" ~ "LP19",        
    Lake == "Upper Palisades Lake" ~ "LH14",
    Lake == "Lake Allen" ~ "LN03",
    Lake == "Lower Blum Lake" ~ "LS-07-01",
    Lake == "Upper Triplet Lake" ~ "SM-02-02",
    Lake == "Ferry Lake" ~ "138",
    Lake == "Lake LH15" ~ "LH15",
    Lake == "Blue Lake" ~ "LZ35",
    Lake == "Upper Deadwood Lake" ~ "LW32",
    Lake == "Bowan Lake" ~ "MR-12-01",
    Lake == "Easy Ridge Lake" ~ "MC-03-01",   
    Lake == "Lower East Lake" ~ "MC-14-02",
    Lake == "Lower Silent Lake" ~ "MA-03-01",
    Lake == "Gladys Lake" ~ "106",
    Lake == "Crazy Lake" ~ "426",
    Lake == "Lake La Crosse" ~ "520"))


# 3. Trim datasets --------------------------------------------------------

water_chem_trim <- water_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = analyte, value)

daily_temperature 

# 4. Join datasets --------------------------------------------------------

# Actually don't do this first...
phys_chem_join <- full_join(x = water_chem_trim, y = physio,
                            by = c("park_code" = "Park_code", "site_code"))

daily_temp_join <- full_join(x = phys_chem_join, y = daily_temperature,
                                  by = c("park_code",
                                         "site_code",
                                         "start_date" = "event_date",
                                         "event_year",
                                         "event_month"))









