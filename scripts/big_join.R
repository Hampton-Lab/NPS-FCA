# A script to process and join many different datasets from NPS and external
# sources together.

# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(plyr)
library(tidyverse)
library(devtools)
# install_github("GLEON/LakeMetabolizer")
library(LakeMetabolizer)


# 2. Read in relevant data files ------------------------------------------

# Physiography
physio <- read_excel(
  path = file.path("..",
                   "data",
                   "analysis_outputs",
                   "study-site-tables.xlsx"))

# Water metrics
lake_level <- read_excel(
  path = dir(path = file.path("..",
                              "data",
                              "NPS_NCCN_Mtn_Lakes_Exports"),
             pattern = "Lake_Level_Events",
             full.names = TRUE)) %>%
  as.data.frame()

water_chem <- read_excel(
  path = dir(path = file.path("..",
                              "data",
                              "NPS_NCCN_Mtn_Lakes_Exports"),
             pattern = "Water_Chemistry_Data_all",
             full.names = TRUE)) %>%
  as.data.frame()

water_prof <- read_excel(
  path = dir(path = file.path("..",
                              "data",
                              "NPS_NCCN_Mtn_Lakes_Exports"),
             pattern = "Water_Column_Profile_Data",
             full.names = TRUE)) %>%
  as.data.frame()

secchi <- read_excel(
  path = dir(path = file.path("..",
                              "data",
                              "NPS_NCCN_Mtn_Lakes_Exports"),
             pattern = "Water_Clarity_Events",
             full.names = TRUE)) %>%
  as.data.frame()

# Fish
fish_species <- read_excel(
  path = dir(path = file.path("..",
                              "data",
                              "NPS_NCCN_Mtn_Lakes_Exports"),
             pattern = "Fish_VES_Event_Counts",
             full.names = TRUE)) %>%
  as.data.frame()

# Zooplankton
zoop <- read_excel(path = file.path("..",
                                    "data",
                                    "NCCN_Zoops_Combined_For_FCA_May_2019.xlsx"))

# Temp data summary
daily_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-daily-temp-summaries.csv"),
  stringsAsFactors = FALSE)

monthly_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-monthly-temp-summaries.csv"),
  stringsAsFactors = FALSE)

# Aspect
aspect_lake <- read_csv(
  file = file.path("..",
                   "data",
                   "aspect_EarthEnv",
                   "aspectns_NCCN_LakePoly.csv")) %>%
  select(NAME, ParkCode, GIS_code,
         aspectns_lake = aspns_mean) %>%
  as.data.frame()

aspect_wshed <- read_csv(
  file = file.path("..",
                   "data",
                   "aspect_EarthEnv",
                   "aspectns_NCCN_LakeWtshd.csv")) %>%
  select(Name, ParkCode, GIS_code,
         aspectns_wshed = aspns_mean) %>%
  as.data.frame()

# Elevation
elev_wshed <- read_csv(
  file = file.path("..",
                   "data",
                   "dem90m",
                   "elevs_wshed.csv")) %>%
  select(Name, ParkCode, GIS_code,
         elevmean_wshed = elevmean,
         elevmax_wshed = elevmax) %>%
  as.data.frame()

# Snow water equivalent (SWE)
snowwater_snodas_lake <- read_csv(
  file = file.path("..",
                   "data",
                   "SWE_snodas",
                   "swe_snodas_NCCNLakePolys.csv")) %>%
  select(NAME, ParkCode, GIS_code,
         m2007 = swem07mean, m2008 = swem08mean, m2009 = swem09mean,
         m2010 = swem10mean, m2011 = swem11mean, m2012 = swem12mean,
         m2013 = swem13mean, m2014 = swem14mean, m2015 = swem15mean,
         m2016 = swem16mean, m2017 = swem17mean, m2018 = swem18mean,
         m2019 = swem19mean, a2007 = swea07mean, a2008 = swea08mean,
         a2009 = swea09mean, a2010 = swea10mean, a2011 = swea11mean,
         a2012 = swea12mean, a2013 = swea13mean, a2014 = swea14mean,
         a2015 = swea15mean, a2016 = swea16mean, a2017 = swea17mean,
         a2018 = swea18mean, a2019 = swea19mean) %>%
  as.data.frame()

snowwater_snodas_wshed <- read_csv(
  file = file.path("..",
                   "data",
                   "SWE_snodas",
                   "swe_snodas_NCCNLakeWatersheds.csv")) %>%
  select(Name, ParkCode, GIS_code,
         m2007 = swem07mean, m2008 = swem08mean, m2009 = swem09mean,
         m2010 = swem10mean, m2011 = swem11mean, m2012 = swem12mean,
         m2013 = swem13mean, m2014 = swem14mean, m2015 = swem15mean,
         m2016 = swem16mean, m2017 = swem17mean, m2018 = swem18mean,
         m2019 = swem19mean, a2007 = swea07mean, a2008 = swea08mean,
         a2009 = swea09mean, a2010 = swea10mean, a2011 = swea11mean,
         a2012 = swea12mean, a2013 = swea13mean, a2014 = swea14mean,
         a2015 = swea15mean, a2016 = swea16mean, a2017 = swea17mean,
         a2018 = swea18mean, a2019 = swea19mean) %>%
  as.data.frame()

snowwater <- read_csv(
  file = file.path("..",
                   "data",
                   "precip_solar_spreadsheets_from_WB_2019Apr23",
                   "SWE_MonthlyData.csv")) %>%
  as.data.frame()

solar <- read_excel(
  path = file.path("..",
                   "data",
                   "precip_solar_spreadsheets_from_WB_2019Apr23",
                   "10mDEM_NCCN_Solar.xlsx")) %>%
  as.data.frame()

# Ice data
ice_data <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "ice_data_harmonized.csv"),
  stringsAsFactors = FALSE)

# Vegetation cover data
veg_noca <- read_excel(path = file.path("..",
                                        "data",
                                        "MOLA_Watershed_Veg.xlsx"),
                       sheet = "NOCA mola")

veg_mora <- read_excel(path = file.path("..",
                                        "data",
                                        "MOLA_Watershed_Veg.xlsx"),
                       sheet = "MORA mola")

veg_olym <- read_excel(path = file.path("..",
                                        "data",
                                        "MOLA_Watershed_Veg.xlsx"),
                       sheet = "OLYM mola")

# Bedrock geology
geol <- read.csv(
  file = file.path("..",
                   "data",
                   "geology",
                   "bedrock_pct_watershed.csv"),
  stringsAsFactors = FALSE)


#deposition data
depo0 <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "deposition_zonal_stats.csv"),
  stringsAsFactors = FALSE)
depo<-depo0
depo$varlong<-paste(depo$measure,"_",depo$variable,sep="")
depo$varshort<-paste(substr(depo$variable,1,nchar(depo$variable)-9),"_dep",sep="")
depo$year<-substr(depo$variable,nchar(depo$variable)-3,nchar(depo$variable))

# prepare No3/nh4 data for summing as N
depo_N<-depo
depo_N$value[which(depo_N$varshort=="NO3_dep")]<-(14/(14+48))*depo_N$value[which(depo_N$varshort=="NO3_dep")]
depo_N$value[which(depo_N$varshort=="NH4_dep")]<-(14/(14+4))*depo_N$value[which(depo_N$varshort=="NH4_dep")]
depo_N <- depo_N %>% filter(varshort %in% c("NO3_dep","NH4_dep"), measure=="mean") %>% 
  mutate(varshort="N_dep",variable=paste("N_dep_",year,sep=""),varlong=paste("mean_",variable,sep="")) %>%
  group_by(lake,measure,year,variable,varshort,varlong) %>% 
  dplyr::summarize(mean=sum(value,na.rm=TRUE))

#combined summed N with rest of dep data
depo_N_linedup<-depo_N %>% select(value=mean,lake,variable,measure,varlong,varshort,year) %>% as.data.frame()
depo_linedup<-depo %>% select(value,lake,variable,measure,varlong,varshort,year) %>% as.data.frame()
depo_prepped1 <-rbind(depo_linedup,depo_N_linedup)   

# summarize depo for different time periods
depo_prepped2<-depo_prepped1 %>% mutate(is1985_2015=year %in% c(1985:2015),is1985_1989=year %in% c(1985:1989),
                                        is1990_1999=year %in% c(1990:1999),is2000_2009=year %in% c(2000:2009),
                                        is2010_2014=year %in% c(2010:2014)) %>%
  group_by(lake,varshort) %>%
  dplyr::summarize(mean_1985_2015=mean(value[is1985_2015==TRUE],na.rm=TRUE),
                   mean_1985_1989=mean(value[is1985_1989==TRUE],na.rm=TRUE),
                   mean_1990_1999=mean(value[is1990_1999==TRUE],na.rm=TRUE),
                   mean_2000_2009=mean(value[is2000_2009==TRUE],na.rm=TRUE),
                   mean_2010_2014=mean(value[is2010_2014==TRUE],na.rm=TRUE),) %>% as.data.frame()

# pivot the previous to long
depo_prepped3<-depo_prepped2 %>%  pivot_longer(
  cols = starts_with("mean"),
  names_to = "period",
  names_prefix = "mean_",
  values_to = "value",
  values_drop_na = TRUE
)
depo_prepped3$varlong<-paste(depo_prepped3$varshort,"_",depo_prepped3$period,sep="")

# pivot the previous to even wider
depo_prepped4<-
  depo_prepped3 %>% pivot_wider(-c(varshort,period),names_from=varlong,values_from=value) %>% as.data.frame()
depo_prepped4$lake[depo_prepped4$lake == "LH15"] <- "Lake LH15"
depo_prepped4$lake[depo_prepped4$lake == "LP19"] <- "Lake LP19"
depo_prepped4$lake[depo_prepped4$lake == "Lake LaCrosse"] <- "Lake La Crosse"

# At last!  
depo<-depo_prepped4

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))


# 3. Processing before the large join -------------------------------------

# Update the geology dataset
geol <- geol %>%
  mutate(ROCKTYPE1 = gsub(pattern = " ",
                          replacement = "",
                          x = ROCKTYPE1)) %>%
  select(Name, ROCKTYPE1, pct_geol) %>%
  pivot_wider(names_from = "ROCKTYPE1", values_from = "pct_geol",
              values_fill = 0) %>%
  mutate(Name = case_when(Name == "LH15" ~ "Lake LH15",
                          Name == "LP19" ~ "Lake LP19",
                          Name ==  "Lake LaCrosse" ~ "Lake La Crosse",
                          TRUE ~ Name))

# Correct erroneous aspect for Upper Triplet, see NCCN study sites report
physio$Aspect[physio$Lake == "Upper Triplet Lake"] <- "Northwest"

# Format date and related columns
water_chem <- clean_names(water_chem) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

# new part

# note that some Total P values are reported as 0 but not flagged as BDL. 
# here is the proof using samples that had no processing flags
water_chem %>% filter(is.na(processing_flag)==TRUE) %>% group_by(analyte,lab) %>% 
  dplyr::summarize(min=min(value,na.rm=TRUE),mean=mean(value,na.rm=TRUE)) %>% 
  as.data.frame()

# With missing BDL flags, what can we do?
# Let's look at the highest and mean values were for samples that were flagged as BDL
bdl<-water_chem %>% filter(processing_flag=="BDL") %>% group_by(analyte,lab) %>% 
  dplyr::summarize(bdl_max=max(value,na.rm=TRUE),bdl_mean=mean(value,na.rm=TRUE))
bdl 

bdl$dl_use<-0
bdl$dl_use[which(bdl$analyte=="Total P")]<-0.001 #
bdl$dl_use[which(bdl$analyte=="Total N")]<-0.01

# Comment from A.R. 
#The MDL for TDP is 0.001 and the MLQ is 0.003. 
#Given this, it seems like we should annotate that the difference between hi and mi isn't significant. 
#Same goes for TDN: MDL = 0.01 and MQL =  0.03. 

# now merge the bdl table with water_chem
water_chemx<-merge(water_chem,bdl,by=c("analyte","lab"),all=TRUE)

water_chemx$value[which(water_chemx$value<water_chemx$dl_use)]<-
  0.5*water_chemx$dl_use[which(water_chemx$value<water_chemx$dl_use)]

water_chem_bdl<-water_chemx
water_chem_bdl$bdl<-0
water_chem_bdl$bdl[which(water_chem_bdl$value<water_chem_bdl$dl_use)]<-1

bdl_TDN<-water_chem_bdl %>% filter(analyte=="Total N") %>% unique() 
prop_bdl_TDN<-length(which(bdl_TDN$bdl==1))/length(bdl_TDN[,1])  
bdl_TDP<-water_chem_bdl %>% filter(analyte=="Total P") %>% unique() 
prop_bdl_TDP<-length(which(bdl_TDP$bdl==1))/length(bdl_TDP[,1])  
prop_bdl_TDN
prop_bdl_TDP
# replace values any values that below "BDL" (bdl_max) with the bdl_max
#water_chemx$value[which(water_chemx$value<water_chemx$bdl_max)]<-
#  water_chemx$bdl_max[which(water_chemx$value<water_chemx$bdl_max)]

#water_chemx$value[which(water_chemx$processing_flag=="BDL" & water_chemx$value<water_chemx$bdl_max)]<-
#  water_chemx$bdl_max[which(water_chemx$processing_flag=="BDL" & water_chemx$value<water_chemx$bdl_max)]

# resave the BDL-corrected data after removing bdl columns
water_chem<-water_chemx %>% select(-bdl_max,-bdl_mean,-dl_use)

water_chem_trim<-water_chem

# end new part

water_prof <- clean_names(water_prof) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))
water_prof$parameter[water_prof$parameter == "DO_sat"] <- "DO_sat_incomplete"
water_prof <- merge(x = as.data.frame(physio), y = water_prof,
                    by.x = "site_code", by.y = "site_code")

# Add oxygen concentration calc
water_prof_sol <- filter(.data = water_prof, parameter == "Temperature")
water_prof_sol$parameter <- "DOsol"
water_prof_sol$parameter_value <- o2.at.sat.base(water_prof_sol$parameter_value,
                                                 altitude = water_prof_sol$Elevation_m,
                                                 salinity = 0,
                                                 model = "garcia-benson")
water_prof <- rbind(water_prof, water_prof_sol)
water_prof_DO0 <- filter(.data = water_prof, parameter == "DO")
water_prof_DOsol0 <- filter(.data = water_prof, parameter == "DOsol")
bynames <- names(water_prof_DO0)[- which(names(water_prof_DO0) %in%
                                           c("parameter_value", "parameter"))]
water_prof_DOsat <- merge(x = water_prof_DO0, y = water_prof_DOsol0,
                          by = bynames, suffixes = c("", ".sol"))
water_prof_DOsat$parameter_value <- 100 * water_prof_DOsat$parameter_value /
  water_prof_DOsat$parameter_value.sol
water_prof_DOsat <- water_prof_DOsat %>%
  select(-parameter_value.sol, -parameter.sol)
water_prof_DOsat$parameter <- "DOsat"
water_prof <- rbind(water_prof, water_prof_DOsat)

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

solar <- clean_names(solar)
solar$solar_jas <- solar$july + solar$august + solar$september
solar$solar_dec <- solar$december
solar <- solar %>%
  select(lake, solar_jas, solar_dec)
solar$lake[solar$lake == "LP19"] <- "Lake LP19"
solar$lake[solar$lake == "LH15"] <- "Lake LH15"
solar$lake[solar$lake == "Lake LaCrosse"] <- "Lake La Crosse"

aspect_lake$NAME[aspect_lake$NAME == "LP19"] <- "Lake LP19"
aspect_lake$NAME[aspect_lake$NAME == "LH15"] <- "Lake LH15"
aspect_lake$NAME[aspect_lake$NAME == "Lake LaCrosse"] <- "Lake La Crosse"
aspect_lake$northface_lake <- 0
aspect_lake$northface_lake[aspect_lake$aspectns_lake <= 0.5] <- 1

aspect_wshed$Name[aspect_wshed$Name == "LP19"] <- "Lake LP19"
aspect_wshed$Name[aspect_wshed$Name == "LH15"] <- "Lake LH15"
aspect_wshed$Name[aspect_wshed$Name == "Lake LaCrosse"] <- "Lake La Crosse"
aspect_wshed$northface_wshed <- 0
aspect_wshed$northface_wshed[aspect_wshed$aspectns_wshed <= 0.5] <- 1

elev_wshed$Name[elev_wshed$Name == "LP19"] <- "Lake LP19"
elev_wshed$Name[elev_wshed$Name == "LH15"] <- "Lake LH15"
elev_wshed$Name[elev_wshed$Name == "Lake LaCrosse"] <- "Lake La Crosse"

physio <- merge(physio, solar,
                by.x = "Lake", by.y = "lake")
physio <- merge(physio, aspect_lake %>%
                  select(NAME, aspectns_lake, northface_lake),
                by.x = "Lake", by.y = "NAME")
physio <- merge(physio, aspect_wshed %>%
                  select(Name, aspectns_wshed, northface_wshed),
                by.x = "Lake", by.y = "Name")
physio <- merge(physio, elev_wshed %>%
                  select(Name, elevmean_wshed, elevmax_wshed),
                by.x = "Lake", by.y = "Name")
physio <- merge(physio, geol %>%
                  select(Name, andesite, basalt,biotitegneiss,granodiorite,
                         graywacke,quartzmonzodiorite,quartzmonzonite,sandstone,
                         tholeiite),
                by.x = "Lake", by.y = "Name")

physio <- merge(physio, depo,# %>%
                #                  select(lake, Cl_dep,NH4_dep,NO3_dep,SO4_dep,N_dep),
                by.x = "Lake", by.y = "lake")

# I rename the columns, should we just leave as-is?
daily_temperature <- daily_temperature %>%
  mutate(event_date = as.Date(paste(obs_year, obs_month, obs_day, sep = "-"),
                              format = "%Y-%m-%d")) %>%
  dplyr::rename(event_year = obs_year,
                event_month = obs_month,
                event_day = obs_day,
                park_code = park)

# Crosswalk common names for sites to site_codes
daily_temperature <- left_join(x = daily_temperature, y = match_sites,
                               by = c("lake" = "old_name"))

monthly_temperature <- monthly_temperature %>%
  dplyr::rename(event_year = obs_year,
                event_month = obs_month,
                park_code = park) %>%
  left_join(x = ., y = match_sites, by = c("lake" = "old_name"))

ice_data <- left_join(x = ice_data, y = match_sites,
                      by = c("lake" = "old_name")) %>%
  filter(!is.na(water_year))

# Clean up snow water equivalent layout
snowwater_snodas_lake <- gather(data = snowwater_snodas_lake,
                                key = year, value = value,
                                c(paste("m", as.character(2007:2019), sep = ""),
                                  paste("a", as.character(2007:2019), sep = "")),
                                factor_key = TRUE) %>%
  clean_names() %>%
  as.data.frame()

snowwater_snodas_wshed <- gather(data = snowwater_snodas_wshed,
                                 key = year, value = value,
                                 c(paste("m", as.character(2007:2019), sep = ""),
                                   paste("a", as.character(2007:2019), sep = "")),
                                 factor_key = TRUE) %>%
  clean_names() %>%
  as.data.frame()


snowwater_snodas_lake$month <- "May"
snowwater_snodas_lake$month[substr(snowwater_snodas_lake$year, 1, 1) == "a"] <- "Apr"
snowwater_snodas_lake$year <- substr(snowwater_snodas_lake$year, 2, 5)
snowwater_snodas_lake$name[snowwater_snodas_lake$name == "LH15"] <- "Lake LH15"
snowwater_snodas_lake$name[snowwater_snodas_lake$name == "LP19"] <- "Lake LP19"
snowwater_snodas_lake$name[snowwater_snodas_lake$name == "Lake LaCrosse"] <- "Lake La Crosse"


snowwater_snodas_wshed$month <- "May"
snowwater_snodas_wshed$month[substr(snowwater_snodas_wshed$year, 1, 1) == "a"] <- "Apr"
snowwater_snodas_wshed$year <- substr(snowwater_snodas_wshed$year, 2, 5)
snowwater_snodas_wshed$name[snowwater_snodas_wshed$name == "LH15"] <- "Lake LH15"
snowwater_snodas_wshed$name[snowwater_snodas_wshed$name == "LP19"] <- "Lake LP19"
snowwater_snodas_wshed$name[snowwater_snodas_wshed$name == "Lake LaCrosse"] <- "Lake La Crosse"

snowwater_snodas_lake <- snowwater_snodas_lake %>%
  select(month, Lake = name, year, value)
snowwater_snodas_wshed <- snowwater_snodas_wshed %>%
  select(month, Lake = name, year, value)

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
snowwater_snodas_lake <- inner_join(x = snowwater_snodas_lake, y = physio,
                                    by = c("Lake" = "Lake"))
snowwater_snodas_wshed <- inner_join(x = snowwater_snodas_wshed, y = physio,
                                     by = c("Lake" = "Lake"))

lakesnow <- inner_join(x = snowwater, y = physio,
                       by = c("snowsite" = "Snowsite"))

lakesnow_snodas_lake <- snowwater_snodas_lake %>%
  select(Park_code, Lake, site_code, month, year, value) %>%
  mutate(variable = paste("SWE_", month, sep = ""),
         year = as.numeric(as.character(year)),
         value = as.numeric(value) / 10) # change to cm

lakesnow_snodas_wshed <- snowwater_snodas_wshed %>%
  select(Park_code, Lake, site_code, month, year, value) %>%
  mutate(variable = paste("SWE_", month, sep = ""),
         year = as.numeric(as.character(year)),
         value = as.numeric(value) / 10) # change to cm

lakesnow_snotel <- lakesnow %>%
  select(Park_code, Lake, site_code, month, year, value) %>%
  mutate(variable = paste("SWE_", month, "_snotel", sep = ""),
         #         year = as.character(year), # change year from factor
         year = as.numeric(as.character(year)),
         value = as.numeric(value) * 2.54) # change to cm

lakesnow <- lakesnow_snodas_wshed

lakesnow <- rbind(lakesnow_snodas_wshed, lakesnow_snotel)

# Remove replicates and field blanks for chem
water_chem <- filter(.data = water_chem,
                     replicate_tf == 0,
                     field_blank_tf == 0)

# Aggregate veg cover by site and make it a single data frame
park_veg <- merge(map_df(.x = list(veg_noca, veg_mora, veg_olym),
                         .f = ~ .x %>%
                           group_by(ParkCode, Name, MAP_CLASS) %>%
                           dplyr::summarize(sum_group = sum(Shape_Area))),
                  map_df(.x = list(veg_noca, veg_mora, veg_olym),
                         .f = ~ .x %>%
                           group_by(Name) %>%
                           dplyr::summarize(sum_tot = sum(Shape_Area))),
                  by = "Name")
park_veg$veg_pct <- 100 * park_veg$sum_group / park_veg$sum_tot
sort(unique(park_veg$MAP_CLASS[grep(pattern = "forest|woodland",
                                    x = park_veg$MAP_CLASS,
                                    ignore.case = TRUE)]))
sort(unique(park_veg$MAP_CLASS[grep(pattern = "shrub",
                                    x = park_veg$MAP_CLASS,
                                    ignore.case = TRUE)]))
sort(unique(park_veg$MAP_CLASS[grep(pattern = "meadow",
                                    x = park_veg$MAP_CLASS,
                                    ignore.case = TRUE)]))
sort(unique(park_veg$MAP_CLASS[grep(pattern = "barren",
                                    x = park_veg$MAP_CLASS,
                                    ignore.case = TRUE)]))


# There are 46 unique classes of veg cover listed (each will become a var level)
unique(park_veg$MAP_CLASS)
veg_key <- rbind(veg_noca, veg_mora, veg_olym) %>%
  group_by(MAP_CLASS) %>%
  dplyr::summarize(sum_group = sum(Shape_Area)) %>%
  arrange(-sum_group) %>%
  as.data.frame()

# Sync veg cover site names and reformat MAP_CLASS. Split the metadata and veg
# codes. There's a .docx explaining classes. "Additional pieces discarded" = OK
park_veg <- left_join(x = park_veg, y = match_sites,
                      by = c("Name" = "old_name")) %>%
  filter(!is.na(ParkCode)) %>%
  separate(col = MAP_CLASS, into = c("veg_code", "meta"), sep = " ") %>%
  mutate(veg_code = paste0("veg_", veg_code))


# 4. Trim cols from datasets ----------------------------------------------

water_chem_trim <- water_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = analyte, value) %>%
  # Harmonize multiple values per time point/site by averaging
  group_by(park_code, site_code, start_date,
           event_year, event_month, variable) %>%
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
  select(site_code, park_code = location, water_year, contains("_doy"),
         ice_duration_days = duration, ice_free_days = ice_free)

park_veg_trim <- park_veg %>%
  ungroup() %>%
  select(site_code, park_code = ParkCode, veg_code, veg_pct) %>%
  spread(key = veg_code, value = veg_pct, fill = 0)

park_veg_trim$forest <- rowSums(park_veg_trim %>%
                                  select(veg_M01Y, veg_M06, veg_M07D,
                                         veg_M07W, veg_M17E, veg_M17M,
                                         veg_M24, veg_M33, veg_M43M,
                                         veg_M46A, veg_M46B, veg_M46D,
                                         veg_M46S, veg_M47, veg_M50))
park_veg_trim$shrub <- rowSums(park_veg_trim %>%
                                 select(veg_M18, veg_M21, veg_M50,
                                        veg_M74A, veg_M74S, veg_M85))
park_veg_trim$meadow <- rowSums(park_veg_trim %>%
                                  select(veg_M52, veg_M58, veg_M61,
                                         veg_M62, veg_M67E, veg_M67W,
                                         veg_M86))
park_veg_trim$barren <- rowSums(park_veg_trim %>%
                                  select(veg_M90, veg_M91, veg_M92,
                                         veg_M93))
park_veg_trim$snowice <- rowSums(park_veg_trim %>%
                                   select(veg_M97))

park_veg_trim <- park_veg_trim %>%
  select(site_code, park_code, forest, shrub, meadow, barren, snowice)

# Integrate the park veg data with physio, since they both lack year-level data
physio <- inner_join(x = physio, y = park_veg_trim,
                     by = c("site_code",
                            "Park_code" = "park_code"))


# 4a. Further process temperature datasets --------------------------------

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


# 5. Bin & summarize water profiles ---------------------------------------

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


# 6. Process fish data ----------------------------------------------------


# 6a. Expand fish data to show 0 counts -----------------------------------

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
  mutate(true_count = case_when(!is.na(count_n) ~ count_n,
                                TRUE ~ true_count),
         true_taxon = substr(taxon_lifestage, 1, 3))

head(fish_species_full)


# 6b. Summarize fish data by time intervals -------------------------------

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

fish_salmo_adult_ever <- fish_species_full %>%
  mutate(salmo = fish_species_full$true_taxon %in%
           c("RBT", "CCT", "BRK", "WCT")) %>%
  group_by(park_code, site_code, variable = "salmo_adult_ever") %>%
  dplyr::summarize(value = as.numeric(max(true_count) > 0)) %>%
  arrange(site_code, variable) %>%
  as.data.frame()


# 7. Format datasets using water_chem template ----------------------------

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
  dplyr::rename(event_year = water_year) %>%
  mutate(
    event_year = case_when(
      variable == "ice_duration_days" ~ event_year - 1,
      variable == "ice_in_doy" ~ event_year + 1,
      TRUE ~ as.numeric(event_year))
  ) %>%
  pivot_wider(names_from = event_year, values_from = value) %>%
  pivot_longer(names_to = "event_year", values_to = "value",
               -c(site_code, park_code, variable)) %>%
  mutate(event_year = as.numeric(event_year))


# 8. Wrangle zoops --------------------------------------------------------

#  Clean zooplankton data
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Crosswalk common names for sites to site_codes
zoop <- left_join(x = zoop, y = match_sites,
                  by = c("Lake" = "old_name")) %>%
  clean_names()

# Aggregate zooplankton data by taxa
zoop_taxa <- zoop %>%
  group_by(year, lake, site_code, taxa) %>%
  dplyr::summarise(density = sum(density)) %>%
  ungroup() %>%
  filter(taxa %in% c("COPE", "CLAD", "ROT")) %>%
  select(site_code,event_year=year,variable=taxa,value=density) %>%
  as.data.frame()

zoop_expanded<-expand.grid(unique(zoop_taxa$site_code),
                           unique(zoop_taxa$event_year),
                           unique(zoop_taxa$variable))
names(zoop_expanded)<-c("site_code","event_year","variable")

zoop_taxa_wzero<-merge(zoop_expanded,zoop_taxa,by=c("site_code","event_year","variable"),all=TRUE)
zoop_taxa_wzero$value[which(is.na(zoop_taxa_wzero$value)==TRUE)]<-0

# Build a comprehensive data frame of sampling metadata to pair to zooplankton
sampling_metadata <- map_df(.x = list(water_chem_trim,
                                      water_prof_summary,
                                      lake_level_trim,
                                      secchi_trim,
                                      fish_species_ever_format,
                                      fish_species_yearly_format),
                            .f =  ~ .x %>%
                              select(park_code, site_code, start_date,
                                     event_year, event_month) %>%
                              unique()) %>%
  unique() %>%
  # No data for this site*year
  add_row(park_code = "NOCA", site_code = "MC-03-01", event_year = 2016)

# Check for unexpected duplicates
sampling_metadata %>%
  count(site_code, event_year) %>%
  arrange(desc(n)) %>%
  head()

# There are multiple potential sample dates and months per year in some cases.
# Randomly select one per year for this dataset. (Note that this causes
# start_date to vary from version to version in the dataset)
sampling_metadata <- sampling_metadata %>%
  group_by(park_code, site_code, event_year) %>%
  sample_n(1)

# Now add in the extra columns. Number of rows after join should be identical
# to the number of rows in zoop_taxa_wzero
zoop_taxa_format <- left_join(x = zoop_taxa_wzero, y = sampling_metadata,
                              by = c("site_code", "event_year")) 


# 9. Join datasets --------------------------------------------------------

# Prepare SWE and physiographic variables for join
lakesnow_format_flush <- lakesnow_format %>%
  unique() %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  left_join(x = ., y = physio, by = c("park_code" = "Park_code", "site_code")) %>%
  mutate(flush_index_noSWE = 10 ^ 4 * Watershed_area_ha / Volume_m3,
         WRT_index_SWE_May = Volume_m3 / ((SWE_May_snotel / 100) *
                                            10 ^ 4 * Watershed_area_ha),
         WRT_index_SWE_Apr = Volume_m3 / ((SWE_April_snotel / 100) *
                                            10 ^ 4 * Watershed_area_ha),
         flush_index_SWE_May = 1 / WRT_index_SWE_May,
         flush_index_SWE_Apr = 1 / WRT_index_SWE_Apr) %>%
  select(park_code, site_code, start_date, event_year, event_month, contains("SWE")) %>%
  pivot_longer(cols = contains("SWE"), names_to = "variable", values_to = "value")

# Bind all limnological datasets together for upcoming joins
limno <- bind_rows(
  water_chem_trim,
  daily_temp_chemdate_match,
  monthly_temp_chemdate_match,
  water_prof_summary,
  lake_level_trim,
  secchi_trim,
  zoop_taxa_format,
  fish_species_ever_format,
  fish_species_yearly_format,
  lakesnow_format_flush,
  ice_data_format) %>%
  as.data.frame()

bigjoin0 <- full_join(x = limno, y = physio,
                      by = c("park_code" = "Park_code", "site_code")) %>%
  as.data.frame()

lake_ice_fish_format <- lakesnow_format_flush %>%
  unique() %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  rename(SWE_Apr_snotel = SWE_April_snotel) %>%
  select(-event_month, -start_date) %>%
  left_join(x = .,
            # ice data
            y = ice_data_format %>%
              pivot_wider(names_from = "variable", values_from = "value"),
            by = c("park_code", "site_code", "event_year")) %>%
  left_join(x = .,
            y = fish_salmo_adult_ever_format %>%
              unique() %>%
              pivot_wider(names_from = variable, values_from = value) %>%
              select(-event_month, -start_date) %>%
              unique(),
            by = c("park_code", "site_code")) %>%
  dplyr::rename(event_year = event_year.x) %>%
  select(-event_year.y)

bigjoin <- left_join(x = bigjoin0, y = lake_ice_fish_format,
                     by = c("park_code", "site_code", "event_year"))

# Remove now redundant datasets
rm(water_chem_trim, daily_temp_chemdate_match, monthly_temp_chemdate_match,
   water_prof_summary, lake_level_trim, secchi_trim, fish_species_ever_format,
   fish_species_yearly_format, lakesnow_format, limno, physio,
   ice_data_format, bigjoin0)


# 10. Additional processing post-join -------------------------------------

# Isolate water_chem units info
water_chem_units <- water_chem %>%
  select(analyte, units) %>%
  unique() %>%
  mutate(units_label = paste0("(", units, ")"))

# Add units columns to bigjoin
bigjoin <- left_join(x = bigjoin, y = water_chem_units,
                     by = c("variable" = "analyte")) %>%
  mutate(units_label = case_when(is.na(units_label) ~ "",
                                 TRUE ~ units_label))

# Remove NAs and add cols with identifier combos
bigjoin <- bigjoin %>%
  filter(!is.na(value)) %>%
  mutate(year = event_year,
         park_site = paste(park_code, Lake),
         variable_park_site = paste0(variable, " ", park_site),
         all = 1) %>% # Do we need this column?
  unique()

dim(bigjoin)

# Check for presence of units cols
bigjoin %>%
  filter(variable %in% c("ice_out_doy", "flush_index_noSWE",
                         "flush_index_SWE_May", 
                         "WRT_index_SWE_May")) %>%
  select(park_code, site_code, variable,
         contains("unit")) %>%
  unique()

# Change some NAs to blanks for plotting purposes
bigjoin <- bigjoin %>%
  mutate(units = case_when(
    variable %in% c("ice_out_doy", "flush_index_noSWE",
                    "flush_index_SWE_May", "WRT_index_SWE_May") ~ "",
    TRUE ~ units),
    units_label = case_when(
      variable %in% c("ice_out_doy", "flush_index_noSWE",
                      "flush_index_SWE_May", "WRT_index_SWE_May") ~ "",
      TRUE ~ units_label))

# Test again
bigjoin %>%
  filter(variable %in% c("ice_out_doy", "flush_index_noSWE",
                         "flush_index_SWE_May", 
                         "WRT_index_SWE_May")) %>%
  select(park_code, site_code, variable,
         contains("unit")) %>%
  unique()


# 11. Export dataset ------------------------------------------------------

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


###################################3

# two depths check
#(data_forstats,labels_tbl$variable)
#labels_tbl
water_chem_trim_2depthscheck <- water_chem %>% select(park_code, site_code, start_date, event_year, event_month,water_sample_depth,
                                                      variable = analyte, value) %>%
  # Aggregate multiple values per time point/site by averaging
  group_by(park_code, site_code, start_date,water_sample_depth,
           event_year, event_month, variable) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
  as.data.frame()
water_chem_trim_2depthscheck_surf<-water_chem_trim_2depthscheck %>% filter(water_sample_depth %in% c("Surface")) %>% select(-water_sample_depth)
water_chem_trim_2depthscheck_bottom<-water_chem_trim_2depthscheck %>% filter(water_sample_depth %in% c("Bottom")) %>% select(-water_sample_depth)
water_chem_trim_2depthscheck<-merge(water_chem_trim_2depthscheck_surf,water_chem_trim_2depthscheck_bottom,
                                    by=c("park_code","site_code","start_date","event_year","event_month","variable"),
                                    suffixes=c("surf","bott"))
water_chem_trim_2depthscheck$surfbott<-(water_chem_trim_2depthscheck$valuesurf-water_chem_trim_2depthscheck$valuebott)/(0.5*(water_chem_trim_2depthscheck$valuesurf+water_chem_trim_2depthscheck$valuebott))

ct_waterchemall<-water_chem %>% select(park_code, site_code, start_date, event_year, event_month) %>% unique() %>% nrow()
ct_waterchem_2depths<-water_chem_trim_2depthscheck %>% select(park_code, site_code, start_date, event_year, event_month) %>% unique() %>% nrow()
ct_waterchemall
ct_waterchem_2depths
