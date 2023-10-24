# A script to process and join several datasets together

# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(plyr)
library(tidyverse)


# 2. Read in revelant data files ------------------------------------------

# Physiography
physio <- read_excel(
  path = file.path("..",
                   "data",
                   "analysis_outputs",
                   "study-site-tables.xlsx"))

# correct erroneous aspect for Upper Triplet, see NCCN study sites report
physio$Aspect[which(physio$Lake=="Upper Triplet Lake")]<-"Northwest"

# Water metrics
lake_level <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports",
                   "qs_b304_Lake_Level_Events_20190923_121946.xlsx")) %>%
  as.data.frame()

water_chem <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports",
                   "qs_b344_Water_Chemistry_Data_all_20190923_160236.xlsx")) %>%
  as.data.frame()

water_prof <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports",
                   "qs_b364_Water_Column_Profile_Data_20191112_141009.xlsx")) %>%
  as.data.frame()
#qs_b364_Water_Column_Profile_Data_20190923_124746

secchi <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports",
                   "qs_b314_Water_Clarity_Events_20190923_122124.xlsx")) %>%
  as.data.frame()

# Fish
fish_species <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports",
                   "qs_b214_Fish_VES_Event_Counts_20190923_120342.xlsx")) %>%
  as.data.frame()

# Temp data summary
daily_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-daily-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

monthly_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-monthly-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

# Snow water equivalent (SWE)
snowwater <- read_csv(
  file = file.path("..",
                   "data",
                   "precip_solar_spreadsheets_from_WB_2019Apr23",
                   "SWE_MonthlyData.csv")) %>%
  as.data.frame()

# Ice data
ice_data <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "ice_data_harmonized.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))


# 2. Processing before the large join -------------------------------------

# Format date and related columns
water_chem <- clean_names(water_chem) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

water_prof <- clean_names(water_prof) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

lake_level <- clean_names(lake_level) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"),
         variable = "lakelevelcm")

secchi <- clean_names(secchi) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

fish_species <- clean_names(fish_species) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

# I rename the columns, should we just leave as-is?
daily_temperature <- daily_temperature %>%
  mutate(event_date = as.Date(paste(obs_year, obs_month, obs_day, sep = "-"),
                              format = "%Y-%m-%d")) %>%
  dplyr::rename(event_year = obs_year, event_month = obs_month, event_day = obs_day,
         park_code = park)

# Crosswalk common names for sites to site_codes
daily_temperature <- left_join(x = daily_temperature, y = match_sites,
                               by = c("lake" = "old_name"))

monthly_temperature <- monthly_temperature %>%
  dplyr::rename(event_year = obs_year, event_month = obs_month, park_code = park) %>%
  left_join(x = ., y = match_sites, by = c("lake" = "old_name"))

physio <- left_join(x = physio, y = match_sites, by = c("Lake" = "old_name"))

ice_data <- left_join(x = ice_data, y = match_sites,
                      by = c("lake" = "old_name"))

# Clean up snow water equivalent layout
snowwater <- t(snowwater) %>%
  as.data.frame()

names(snowwater) <- c("month", as.character(2006:2018))

snowwater <- snowwater[-1, ]
snowwater$snowsite <- row.names(snowwater)
snowwater$snowsite[c(1, 3, 5, 7, 9, 11, 13)] <- snowwater$snowsite[c(2, 4, 6, 8, 10, 12, 14)]
snowwater$snowsite[c(16)] <- snowwater$snowsite[c(15)]
snowwater$snowsite[c(17, 19)] <- snowwater$snowsite[c(18, 20)]
row.names(snowwater) <- NULL

# Make spellings match
snowwater <- snowwater %>%
  mutate(snowsite = case_when(
    snowsite == "Brown Top" ~ "Browntop",
    TRUE ~ snowsite))

# Make long version
snowwater <- gather(data = snowwater, key = year, value = value,
                    as.character(2006:2018), factor_key = TRUE) %>%
  clean_names() %>%
  as.data.frame()

# Join the SWE data with the physiography data
lakesnow <- inner_join(x = snowwater, y = physio,
                       by = c("snowsite" = "Snowsite"))

#merge(physio,snowwater,by.x="Snowsite",by.y="snowsite")


lakesnow <- lakesnow %>%
  select(Park_code, Lake, site_code, month, year, value) %>%
  mutate(variable = paste("SWE_", month, sep = ""),
#         year = as.character(year), # change year from factor
         year = as.numeric(as.character(year)),
         value = as.numeric(value) * 2.54) # change to cm

# Remove replicates and field blanks for chem
water_chem <- filter(.data = water_chem,
                     replicate_tf == 0,
                     field_blank_tf == 0)


# 3. Trim cols from datasets ----------------------------------------------

water_chem_trim <- water_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = analyte, value) %>%
  # Harmonize multiple values per time point/site by averaging
  group_by(park_code, site_code, start_date, event_year, event_month, variable) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
  as.data.frame()

lake_level_trim <- lake_level %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = mean_level_cm)

secchi_trim <- secchi %>%
  mutate(variable = "secchi_value_m") %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = secchi_value_m)

ice_data_trim <- ice_data %>%
  select(site_code, park_code = location, water_year, contains("_doy"))


# 3a. Further process temperature datasets --------------------------------

# Create a long version of daily temp data & format new col
daily_temp_long <- daily_temperature %>%
  gather(key = variable, value = value, mean_value) %>%
  unite(col = variable, measure, variable) %>%
  mutate(variable = gsub("_mean_value", "", variable),
         variable = gsub("BottomTemp", "BotTemp", variable),
         variable = gsub("SurfaceTemp", "SurfTemp", variable))

# Filter daily temp by water_chem_trim sampling metadata
daily_temp_chemdate_match <- semi_join(x = daily_temp_long, y = water_chem_trim,
                                       by = c("park_code",
                                              "site_code",
                                              "event_date" = "start_date",
                                              "event_year",
                                              "event_month")) %>%
  select(park_code, site_code, start_date = event_date, event_year, event_month,
         variable, value)

# Create a long version of monthly temp data & format new col
monthly_temp_long <- monthly_temperature %>%
  gather(key = variable, value = value, mean_value) %>%
  unite(col = variable, measure, variable, event_month, remove = F)

# Subset months of interest and further format variable col
monthly_temp_long <- monthly_temp_long %>%
  filter(event_month <= 8) %>%
  select(-measure) %>%
  na.omit() %>%
  mutate(variable = gsub("_mean_value", "", variable),
         variable = gsub("BottomTemp", "BotTemp", variable),
         variable = gsub("SurfaceTemp", "SurfTemp", variable))

# Filter monthly temp by water_chem_trim sampling metadata
monthly_temp_chemdate_match <- inner_join(x = monthly_temp_long,
                                          y = water_chem_trim,
                                          by = c("park_code",
                                                 "site_code",
                                                 "event_year")) %>%
  select(park_code, site_code, start_date, event_year,
         event_month = event_month.y, variable = variable.x, value = value.x) %>%
  # Remove duplicate rows from when there were additional cols:
  unique()


# 4. Bin & summarize water profiles ---------------------------------------

water_prof_binned <- water_prof %>%
  mutate(parameter = if_else(condition = parameter == "Temperature",
                             true = "ProfTemp",
                             false = parameter),
         depth_groupbin = case_when(depth_bin_m <= 2 ~ "top2m",
                                    depth_bin_m > 2 ~ "below2m"))

# Harmonize multiple values per time point/site by averaging and combine
# depth and variable cols
# unite syntax was having ambiguous behavior (sometimes throwing errors) on SP machine
water_prof_summary <- water_prof_binned %>%
  group_by(park_code, site_code, event_year, event_month,
           start_date, depth_groupbin, parameter) %>%
  dplyr::summarize(mean = mean(parameter_value, na.rm = TRUE)) %>%
  unite(variable, parameter, depth_groupbin) %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = mean) %>%
  as.data.frame()
# 
#water_prof_summary <- water_prof_binned %>%
#  group_by(park_code, site_code, event_year, event_month,
#           start_date, depth_groupbin,parameter) %>%
#  dplyr::summarize(mean = mean(parameter_value, na.rm = TRUE)) %>%
#  select(park_code, site_code, start_date, event_year, event_month,
#         variable=parameter, value = mean) %>%
#  as.data.frame()


# 5. Process fish data ----------------------------------------------------


# 5a. Expand fish data to show 0 counts -----------------------------------

fish_species <- fish_species %>%
  mutate(event_month = month(start_date),
         taxon_lifestage = paste(fish_taxon_code, "_", fish_life_stage,
                                 sep = ""))

# Prep sampling data for fish
fish_park <- water_chem_trim %>%
  select(park_code, site_code, start_date, event_year, event_month) %>%
  unique()

taxon_lifestages <- sort(unique(fish_species$taxon_lifestage))
taxon_lifestages <- taxon_lifestages[-which(taxon_lifestages == "NA_NA")]

# Combine taxon data with park data to stage for a join with fish_species
# and add preliminary count column
fish_park_taxa <- ldply(.data = taxon_lifestages,
                        .fun = function(x) bind_rows(data.frame(fish_park,
                                                                taxon_lifestage = x,
                                                                stringsAsFactors = F))) %>%
  mutate(true_count = 0)


# Remove NAs
fish_species <- filter(.data = fish_species, taxon_lifestage != "NA_NA")

# Join fish sampling data with species data
fish_species_full <- full_join(x = fish_park_taxa, y = fish_species,
                               by = c("park_code", "site_code",
                                      "start_date", "event_year",
                                      "event_month", "taxon_lifestage"))

fish_species_full <- fish_species_full %>%
  mutate(true_count =  case_when(!is.na(count_n) ~ count_n,
                                 TRUE ~ true_count),
         true_taxon = substr(taxon_lifestage, 1, 3))

head(fish_species_full)


# 5b. Summarize fish data by time intervals -------------------------------

fish_lifestages_yearly <- fish_species_full %>%
  group_by(park_code, site_code, event_year, event_month, start_date,
           variable = taxon_lifestage) %>%
  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
  arrange(site_code, event_year, variable) %>%
  as.data.frame()

fish_species_yearly <- fish_species_full %>%
  group_by(park_code, site_code, event_year, event_month, start_date,
           variable = true_taxon) %>%
  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
  arrange(site_code, event_year, variable) %>%
  as.data.frame()

fish_species_ever <- fish_species_full %>%
  group_by(park_code, site_code,
           variable = paste(true_taxon, "_ever", sep = "")) %>%
  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
  arrange(site_code, variable) %>%
  as.data.frame()

#fish_salmo_adult_ever <- fish_species_full %>% filter(fish_life_stage=="Adult",fish_taxon_code!="TSS") %>%
#  group_by(park_code, site_code,
#           variable = "salmo_adult_ever") %>%
#  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
#  arrange(site_code, variable) %>%
#  as.data.frame()

fish_salmo_adult_ever<-fish_species_full %>% mutate(salmo= fish_species_full$true_taxon %in% c("RBT","CCT","BRK","WCT")) %>%
  group_by(park_code, site_code,
           variable = "salmo_adult_ever") %>%
  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
  arrange(site_code, variable) %>%
  as.data.frame()



# 6. Format datasets using water_chem template ----------------------------

# Fish
fish_species_yearly_chem <- inner_join(x = fish_species_yearly,
                                       y = water_chem_trim,
                                       by = c("park_code", "site_code",
                                              "event_year", "start_date",
                                              "event_month"))

fish_species_yearly_format <- fish_species_yearly_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = variable.x, value = value.x)

fish_species_ever_chem <- inner_join(x = fish_species_ever,
                                     y = water_chem_trim,
                                     by = c("park_code", "site_code"))

fish_species_ever_format <- fish_species_ever_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = variable.x, value = value.x)

fish_salmo_adult_ever_chem <- inner_join(x = fish_salmo_adult_ever,
                                     y = water_chem_trim,
                                     by = c("park_code", "site_code"))

fish_salmo_adult_ever_format <- fish_salmo_adult_ever_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = variable.x, value = value.x)

# SWE/Physiography
lakesnow_chem <- full_join(x = lakesnow, y = water_chem_trim,
                           by = c("Park_code" = "park_code", "site_code",
                                  "year" = "event_year"))

lakesnow_format <- lakesnow_chem %>%
  select(park_code = Park_code, site_code, start_date, event_year = year,
         event_month, variable = variable.x, value = value.x)

ice_data_format <- ice_data_trim %>%
  gather(key = variable, value = value, -c(site_code:water_year)) %>%
  dplyr::rename(event_year = water_year)


# 7. Join datasets --------------------------------------------------------

limno <- bind_rows(
  water_chem_trim,
  daily_temp_chemdate_match,
  monthly_temp_chemdate_match,
  water_prof_summary,
  lake_level_trim,
  secchi_trim,
  fish_species_ever_format,
  fish_species_yearly_format,
  lakesnow_format,
  ice_data_format) %>%
  as.data.frame()

bigjoin0 <- full_join(x = limno, y = physio,
                     by = c("park_code" = "Park_code", "site_code")) %>%
  as.data.frame()

iceout_format <- filter(ice_data_format,
                        variable == "ice_out_doy") %>%
  select(site_code, park_code, event_year, ice_out_doy = value)

icein_format <- filter(ice_data_format,
                       variable == "ice_in_doy") %>%
  select(site_code, park_code, event_year, ice_in_doy = value)

icein_format$event_year <- icein_format$event_year + 1

SWE_May_format <-  unique(filter(lakesnow_format,
                      variable %in% "SWE_May") %>%
  select(site_code, park_code, event_year, SWE_May = value))

SWE_April_format <-  unique(filter(lakesnow_format,
                          variable %in% "SWE_April") %>%
  select(site_code, park_code, event_year, SWE_April = value))

fish_salmo_adult_ever_format <-  unique(filter(fish_salmo_adult_ever_format,
                                   variable %in% "salmo_adult_ever") %>%
                              select(site_code, park_code, event_year, salmo_adult_ever = value))


bigjoin_ice1 <- left_join(x = bigjoin0, y = iceout_format,
                           by = c("site_code", "park_code", "event_year"))
bigjoin_ice2 <- left_join(x = bigjoin_ice1, y = icein_format,
                           by = c("site_code", "park_code", "event_year"))
bigjoin_ice3 <- left_join(x = bigjoin_ice2, y = SWE_May_format,
                          by = c("site_code", "park_code", "event_year"))
bigjoin_ice4 <- left_join(x = bigjoin_ice3, y = SWE_April_format,
                          by = c("site_code", "park_code", "event_year"))

bigjoin_salmo_adult<-left_join(x = bigjoin_ice4, y = fish_salmo_adult_ever_format,
                               by = c("site_code", "park_code", "event_year"))
#bigjoin0 <- bigjoin_ice4
bigjoin <- bigjoin_salmo_adult




str(bigjoin)

# Remove now redundant datasets
rm(water_chem_trim, daily_temp_chemdate_match, monthly_temp_chemdate_match,
   water_prof_summary, lake_level_trim, secchi_trim, fish_species_ever_format,
   fish_species_yearly_format, lakesnow_format, limno, physio,
   ice_data_format, bigjoin_ice1, bigjoin_ice2, bigjoin_ice3, bigjoin_ice4, bigjoin0)


# 8. Additional processing post-join --------------------------------------

# Add water_chem units
water_chem_units <- water_chem %>%
  select(analyte, units) %>%
  unique() %>%
  mutate(units_label = paste0("(", units, ")"))

bigjoin <- full_join(x = bigjoin, y = water_chem_units,
                     by = c("variable" = "analyte")) %>%
  mutate(units_label = case_when(is.na(units_label) ~ "",
                                 TRUE ~ units_label))

bigjoin <- bigjoin %>%
  filter(!is.na(value)) %>%
  mutate(year = event_year,
         park_site = paste(park_code, Lake),
         variable_park_site = paste0(variable, " ", park_site),
         all = 1) %>% # I think this column can go...
  unique()

# add flushing index to big join
bigjoin$flush_index_noSWE<-10^4*bigjoin$Watershed_area_ha/bigjoin$Volume_m3
bigjoin$WRT_index_SWE_May<-bigjoin$Volume_m3/((bigjoin$SWE_May/100)*10^4*bigjoin$Watershed_area_ha)
bigjoin$WRT_index_SWE_Apr<-bigjoin$Volume_m3/((bigjoin$SWE_Apr/100)*10^4*bigjoin$Watershed_area_ha)
bigjoin$flush_index_SWE_May<-1/bigjoin$WRT_index_SWE_May
bigjoin$flush_index_SWE_Apr<-1/bigjoin$WRT_index_SWE_Apr

bigjoin_extend_ice_out_doy<-bigjoin[duplicated(data.frame(bigjoin$park_code,bigjoin$site_code,bigjoin$year,bigjoin$ice_out_doy))==FALSE,]
bigjoin_extend_flush_index_noSWE<-bigjoin[duplicated(data.frame(bigjoin$park_code,bigjoin$site_code,bigjoin$year,bigjoin$flush_index_noSWE))==FALSE,]
bigjoin_extend_flush_index_SWE_May<-bigjoin[duplicated(data.frame(bigjoin$park_code,bigjoin$site_code,bigjoin$year,bigjoin$flush_index_SWE_May))==FALSE,]
bigjoin_extend_WRT_index_SWE_May<-bigjoin[duplicated(data.frame(bigjoin$park_code,bigjoin$site_code,bigjoin$year,bigjoin$WRT_index_SWE_May))==FALSE,]

bigjoin_extend_ice_out_doy$variable<-"ice_out_doy"
bigjoin_extend_ice_out_doy$value<-bigjoin_extend_ice_out_doy$ice_out_doy
bigjoin_extend_flush_index_noSWE$variable<-"flush_index_noSWE"
bigjoin_extend_flush_index_noSWE$value<-bigjoin_extend_flush_index_noSWE$flush_index_noSWE
bigjoin_extend_flush_index_SWE_May$variable<-"flush_index_SWE_May"
bigjoin_extend_flush_index_SWE_May$value<-bigjoin_extend_flush_index_SWE_May$flush_index_SWE_May
bigjoin_extend_WRT_index_SWE_May$variable<-"WRT_index_SWE_May"
bigjoin_extend_WRT_index_SWE_May$value<-bigjoin_extend_WRT_index_SWE_May$WRT_index_SWE_May

bigjoin_extend_ice_out_doy$units<-bigjoin_extend_ice_out_doy$units_label<-""
bigjoin_extend_flush_index_noSWE$units<-bigjoin_extend_flush_index_noSWE$units_label<-""
bigjoin_extend_flush_index_SWE_May$units<-bigjoin_extend_flush_index_SWE_May$units_label<-""
bigjoin_extend_WRT_index_SWE_May$units<-bigjoin_extend_WRT_index_SWE_May$units_label<-""

# stack the ice/snow/hydro data to bottom of big join
bigjoin<-rbind(bigjoin,bigjoin_extend_ice_out_doy,bigjoin_extend_flush_index_noSWE,bigjoin_extend_flush_index_SWE_May,bigjoin_extend_WRT_index_SWE_May)


# Save current version of bigjoin as an R-specific filetype for use outside
# of this script by other R processes. Less fuss over column types, etc.
saveRDS(object = bigjoin, file = file.path("..",
                                           "data",
                                           "analysis_outputs",
                                           "bigjoin.rds"))

# Create data summaries by location, year and variable with relevant units
univar_summaries <- bigjoin %>%
  group_by(park_site, variable, year, units) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
  as.data.frame()

# Probably not necessary to arrange, but do so to be sure
univar_summaries <- univar_summaries %>%
  arrange(variable, park_site, year)

write.csv(file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "univar_summaries.csv"),
          x = univar_summaries, row.names = FALSE)
