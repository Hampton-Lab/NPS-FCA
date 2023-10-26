# Package ID: knb-lter-nwt.175.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Dissolved oxygen data for the Green Lake 4 buoy, 2018 - ongoing.
# Data set creator:  Pieter Johnson -
# Data set creator:  Samuel Yevak -
# Data set creator:  Stephanie Dykema -
# Data set creator:  Kelly Loria -
# Data set creator:    - Niwot Ridge LTER
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/175/3/f7348b7a7020dc2c8317af5fc0ada175"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "LTER_site",
                 "local_site",
                 "year",
                 "sensor",
                 "deployment",
                 "timestamp",
                 "depth",
                 "temperature",
                 "DO",
                 "DO_saturation",
                 "battery",
                 "Q",
                 "flag_temperature",
                 "flag_DO",
                 "flag_Q",
                 "flag_battery"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)
if (class(dt1$sensor)!="factor") dt1$sensor<- as.factor(dt1$sensor)
if (class(dt1$deployment)!="factor") dt1$deployment<- as.factor(dt1$deployment)
# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp1timestamp<-as.POSIXct(dt1$timestamp,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){dt1$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1timestamp)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$temperature)=="factor") dt1$temperature <-as.numeric(levels(dt1$temperature))[as.integer(dt1$temperature) ]
if (class(dt1$temperature)=="character") dt1$temperature <-as.numeric(dt1$temperature)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$DO_saturation)=="factor") dt1$DO_saturation <-as.numeric(levels(dt1$DO_saturation))[as.integer(dt1$DO_saturation) ]
if (class(dt1$DO_saturation)=="character") dt1$DO_saturation <-as.numeric(dt1$DO_saturation)
if (class(dt1$battery)=="factor") dt1$battery <-as.numeric(levels(dt1$battery))[as.integer(dt1$battery) ]
if (class(dt1$battery)=="character") dt1$battery <-as.numeric(dt1$battery)
if (class(dt1$Q)=="factor") dt1$Q <-as.numeric(levels(dt1$Q))[as.integer(dt1$Q) ]
if (class(dt1$Q)=="character") dt1$Q <-as.numeric(dt1$Q)
if (class(dt1$flag_temperature)!="factor") dt1$flag_temperature<- as.factor(dt1$flag_temperature)
if (class(dt1$flag_DO)!="factor") dt1$flag_DO<- as.factor(dt1$flag_DO)
if (class(dt1$flag_Q)!="factor") dt1$flag_Q<- as.factor(dt1$flag_Q)
if (class(dt1$flag_battery)!="factor") dt1$flag_battery<- as.factor(dt1$flag_battery)

# Convert Missing Values to NA for non-dates

dt1$temperature <- ifelse((trimws(as.character(dt1$temperature))==trimws("NaN")),NA,dt1$temperature)
suppressWarnings(dt1$temperature <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$temperature))==as.character(as.numeric("NaN"))),NA,dt1$temperature))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NaN")),NA,dt1$DO)
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NaN"))),NA,dt1$DO))
dt1$DO_saturation <- ifelse((trimws(as.character(dt1$DO_saturation))==trimws("NaN")),NA,dt1$DO_saturation)
suppressWarnings(dt1$DO_saturation <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO_saturation))==as.character(as.numeric("NaN"))),NA,dt1$DO_saturation))
dt1$battery <- ifelse((trimws(as.character(dt1$battery))==trimws("NaN")),NA,dt1$battery)
suppressWarnings(dt1$battery <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$battery))==as.character(as.numeric("NaN"))),NA,dt1$battery))
dt1$Q <- ifelse((trimws(as.character(dt1$Q))==trimws("NaN")),NA,dt1$Q)
suppressWarnings(dt1$Q <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$Q))==as.character(as.numeric("NaN"))),NA,dt1$Q))


# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(LTER_site)
summary(local_site)
summary(year)
summary(sensor)
summary(deployment)
summary(timestamp)
summary(depth)
summary(temperature)
summary(DO)
summary(DO_saturation)
summary(battery)
summary(Q)
summary(flag_temperature)
summary(flag_DO)
summary(flag_Q)
summary(flag_battery)
# Get more details on character variables

summary(as.factor(dt1$LTER_site))
summary(as.factor(dt1$local_site))
summary(as.factor(dt1$sensor))
summary(as.factor(dt1$deployment))
summary(as.factor(dt1$flag_temperature))
summary(as.factor(dt1$flag_DO))
summary(as.factor(dt1$flag_Q))
summary(as.factor(dt1$flag_battery))
detach(dt1)

green_lake4_do <- dt1 %>% filter(depth >= 11) %>%
  filter(deployment %in% c("Summer2018"))%>%
  mutate(date = lubridate::ymd_hms(timestamp),
         week = lubridate::week(date))%>%
  group_by(week)%>%
  summarise(temp = mean(temperature, na.rm = T),
            do = mean(DO, na.rm = T)) %>%
  filter(week != 34)%>%
  mutate(elevation = 3561)




