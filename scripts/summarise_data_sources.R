# A script to process and join several datasets together

# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(plyr)
library(tidyverse)
library(devtools)
# install_github("GLEON/LakeMetabolizer")
library(LakeMetabolizer)


# 2. Read in revelant data files ------------------------------------------

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

# Temp data summary
daily_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-daily-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>%
  as.data.frame()

monthly_temperature <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "all-monthly-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>%
  as.data.frame()

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
  stringsAsFactors = FALSE) %>%
  as.data.frame()

# Veg cover data
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

# bedrock geology
geol <- read.csv(
  file = file.path("..",
                   "data",
                   "geology",
                   "bedrock_pct_watershed.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

geol$ROCKTYPE1<-gsub(" ", "",geol$ROCKTYPE1)

geol <- geol %>% select(Name,ROCKTYPE1,pct_geol) %>% spread(ROCKTYPE1,pct_geol)
geol[is.na(geol)] <- 0
# correction for Gladys Lake which had ~ 5% water, but all land is graywacke
geol$graywacke[which(geol$Name=="Gladys Lake")]<- max(geol$graywacke[which(geol$Name=="Gladys Lake")])
geol$Name[geol$Name == "LH15"] <- "Lake LH15"
geol$Name[geol$Name == "LP19"] <- "Lake LP19"
geol$Name[geol$Name == "Lake LaCrosse"] <- "Lake La Crosse"

#deposition data
depo0 <- read.csv(
  file = file.path("..",
                   "data",
                   "analysis_outputs",
                   "deposition_zonal_stats.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()
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



# 3. Get info on datasets -------------------------------------------------


# Lake level --------------------------------------------------------------

# Range of years
lake_level %>%
  group_by(Park_code, Site_code) %>%
  summarise(min = min(Event_year), max = max(Event_year))

# Range full dataset
lake_level %>%
  summarise(min = min(Event_year), max = max(Event_year))

# Counts per year and lake
lake_level %>%
  count(Park_code, Site_code, Event_year)


# Water chem --------------------------------------------------------------

# Range of years
water_chem %>%
  group_by(Park_code, Site_code) %>%
  summarise(min = min(Event_year), max = max(Event_year))

# Range full dataset
water_chem %>%
  summarise(min = min(Event_year), max = max(Event_year))

# Counts per year and lake
water_chem %>%
  count(Park_code, Site_code, Analyte, Event_year)


# Atmospheric Deposition --------------------------------------------------

# http://nadp.slh.wisc.edu/NTN/annualmapsByYear.aspx#2018

unique(depo0$variable)


# Elevation ---------------------------------------------------------------

elev_wshed


# Ice ---------------------------------------------------------------------

# Range of years
ice_data %>%
  group_by(location, lake) %>%
  summarise(min = min(water_year, na.rm = T), max = max(water_year, na.rm = T))

# Range full dataset
ice_data %>%
  summarise(min = min(water_year, na.rm = T), max = max(water_year, na.rm = T))

# Counts per year and lake
ice_data %>%
  count(location, lake, water_year)


# Profile -----------------------------------------------------------------

# Range of years
water_prof %>%
  group_by(Park_code, Site_code) %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Range full dataset
water_prof %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Counts per year and lake
water_prof %>%
  count(Park_code, Site_code, Parameter, Event_year)

# Parameters measured
water_prof %>%
  pull(Parameter) %>%
  unique()


# Secchi ------------------------------------------------------------------

# Range of years
secchi %>%
  group_by(Park_code, Site_code) %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Range full dataset
secchi %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Counts per year and lake
secchi %>%
  count(Park_code, Site_code, Event_year)


# Fish --------------------------------------------------------------------

# Range of years
fish_species %>%
  group_by(Park_code, Site_code) %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Range full dataset
fish_species %>%
  summarise(min = min(Event_year, na.rm = T), max = max(Event_year, na.rm = T))

# Counts per year and lake
fish_species %>%
  count(Park_code, Site_code, Event_year)



# Temperature -------------------------------------------------------------

# Range of years
daily_temperature %>%
  group_by(park, lake) %>%
  summarise(min = min(obs_year, na.rm = T), max = max(obs_year, na.rm = T))

# Range full dataset
daily_temperature %>%
  summarise(min = min(obs_year, na.rm = T), max = max(obs_year, na.rm = T))

