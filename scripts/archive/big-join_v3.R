## SP+MB to do... Find missing info for last two lakes in "physio"- Lake Sun Up, Lake Connie
# I fixed Lake Sunup. Couldn't find any issues with Connie though. Lake Hoh doesn't
# exist in the Mountain Lake Study Sites in the North Coast and Cascades Network
# PDF from what I can tell, so no data to match. Should only have that as a missing
# lake now.

# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(nlme) # needed for mixed model
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
                   "NPS_NCCN_Mtn_Lakes_Exports_20190417",
                   "qs_b304_Lake_Level_Events_20190417_103059.xlsx")) %>%
  as.data.frame()

water_chem <- read_excel(
  path = file.path("..",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190417",
                   "qs_b344_Water_Chemistry_Data_select_20190417_103434.xlsx")) %>%
  as.data.frame()

water_prof <- read_excel(
  path = file.path("..",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190417",
                   "qs_b364_Water_Column_Profile_Data_20190417_103525.xlsx")) %>%
  as.data.frame()

# Temp data summary
daily_temperature <- read.csv(
  file = file.path("..",
                   "all-daily-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

monthly_temperature <- read.csv(
  file = file.path("..",
                   "all-monthly-temp-summaries.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

# 2. Pre-join processing --------------------------------------------------

water_chem <- clean_names(water_chem) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

water_prof <- clean_names(water_prof) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))

lake_level <- clean_names(lake_level) %>%
  mutate(event_month = month(start_date),
         start_date = as.Date(start_date, format = "%Y-%m-%d"))
lake_level$variable<-"lakelevelcm"

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
    lake == "LaCrosse" ~ "520",
    lake == "Lake Sunup" ~ "623"    
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
    lake == "LaCrosse" ~ "520",
    lake == "Lake Sunup" ~ "623"
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
    Lake == "Lake La Crosse" ~ "520",
    Lake == "Lake Sunup" ~ "623"))


# 3. Trim datasets --------------------------------------------------------

water_chem <- subset(water_chem,water_chem$replicate_tf==0)

water_chem_trim <- water_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = analyte, value)



lake_level <- lake_level %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = mean_level_cm)


# 3a. Work up water temperatures ------------------------------------------

daily_temperature 

tempmeans<-daily_temperature
tempmeans$analyte<-paste(tempmeans$measure,"meandaily",sep="")
tempmeans$value<-tempmeans$mean_value

templower<-daily_temperature
templower$analyte<-paste(templower$measure,"lodaily",sep="")
templower$value<-templower$perc_10

temphigher<-daily_temperature
temphigher$analyte<-paste(temphigher$measure,"hidaily",sep="")
temphigher$value<-temphigher$perc_90

daily_temperature_melted <-rbind(tempmeans,templower,temphigher)

tempmeans<-monthly_temperature
tempmeans$variable<-paste(tempmeans$measure,"meanmonthly",tempmeans$event_month,sep="")
tempmeans$value<-tempmeans$mean_value

templower<-monthly_temperature
templower$variable<-paste(templower$measure,"lomonthly",templower$event_month,sep="")
templower$value<-templower$perc_10

temphigher<-monthly_temperature
temphigher$variable<-paste(temphigher$measure,"himonthly",temphigher$event_month,sep="")
temphigher$value<-temphigher$perc_90

#monthly_temperature_melted <-rbind(tempmeans,templower,temphigher)
monthly_temperature_melted <-tempmeans
monthly_temperature_melted<-na.omit(monthly_temperature_melted)
monthly_temperature_melted<-subset(monthly_temperature_melted,monthly_temperature_melted$event_month<=7)

waterchemdates<-merge(monthly_temperature_melted,water_chem_trim,by=c("park_code", "site_code", "event_year"),all=FALSE)
monthly_temperature_melted<- waterchemdates %>% select(park_code, site_code, start_date, event_year, event_month=event_month.y,
                                                       variable=variable.x, value=value.x)

#monthly_temperature_melted$event_date<-NA

daily_temperature_melted <-daily_temperature_melted  %>% select(park_code, site_code, start_date=event_date, event_year, event_month,
                                                                variable = analyte, value)
monthly_temperature_melted <-monthly_temperature_melted  %>% select(park_code, site_code, start_date, event_year, event_month,
                                                                    variable, value)


test<-merge(daily_temperature_melted,water_chem_trim,by = c("park_code",
                                                "site_code",
                                                "start_date",
                                                "event_year",
                                                "event_month")) %>% as.data.frame()

temperature_onchemdates<-inner_join(daily_temperature_melted,water_chem_trim,by = c("park_code",
                                                                                    "site_code",
                                                                                    "start_date",
                                                                                    "event_year",
                                                                                    "event_month")) %>% as.data.frame()

temperature_onchemdates<-temperature_onchemdates %>% select(park_code, site_code, start_date, event_year, event_month,
                                                            variable=variable.x, value=value.x)

# 4. Workup profiles --------------------------------------------------------
water_prof$parameter[which(water_prof$parameter=="Temperature")]<-"ProfTemp"
water_prof$depth_groupbin<-""
water_prof$depth_groupbin[which(water_prof$depth_bin_m<=2)]<-"top2m"
water_prof$depth_groupbin[which(water_prof$depth_bin_m>2)]<-"below2m"


water_prof_binned<-water_prof %>% group_by(park_code,site_code,event_year,event_month,start_date,depth_groupbin,parameter) %>%
  dplyr::summarize(mean=mean(parameter_value,na.rm=TRUE)) %>% as.data.frame()


water_prof_binned$variable<-paste(water_prof_binned$parameter,water_prof_binned$depth_groupbin,sep="")
water_prof_binned$value<-water_prof_binned$mean

water_prof_binned<-water_prof_binned %>% select(park_code, site_code, start_date, event_year, event_month,
                                                variable, value)

# 5. Join datasets --------------------------------------------------------

limno <- rbind(
    water_chem_trim,
    temperature_onchemdates,
    monthly_temperature_melted,
    water_prof_binned,
    lake_level) %>%
  as.data.frame()

bigjoin <- full_join(x = limno, y = physio,
                     by = c("park_code" = "Park_code", "site_code")) %>%
  as.data.frame()


## bigjoin[which(is.na(bigjoin$variable==TRUE)),]


# 6 units

waterchemunits<-unique(data.frame(analyte=water_chem$analyte,units=water_chem$units))
waterchemunits$unitslabel<-paste("(",waterchemunits$units,")",sep="")

bigjoin<-merge(bigjoin,waterchemunits,by.x=c("variable"),by.y=c("analyte"),all=TRUE)
bigjoin$unitslabel[which(is.na(bigjoin$unitslabel)==TRUE)]<-""

# 7. lme and many plots --------------------------------------------------------

model.data<-bigjoin
#model.data<-model.data[-which(is.na(model.data$variable)==TRUE),]
model.data<-model.data[-which(is.na(model.data$value)==TRUE),]
model.data$year<-model.data$event_year
model.data$ParkSite<-paste(model.data$park_code,model.data$Lake)
model.data$VariableParksite<-paste(model.data$variable," ",model.data$ParkSite,sep="")
model.data$all<-1
model.data<-unique(model.data)

univar_summaries<-model.data %>% group_by(ParkSite,variable,year,units) %>% 
  dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% as.data.frame()
univar_summaries<-univar_summaries[(order(univar_summaries$variable,univar_summaries$ParkSite,univar_summaries$year)),]
write.csv(file="univar_summaries.csv",univar_summaries)


# Fit simple "univariate" change model
my.lme <-  lme(value ~ year:variable-1, random=~1|ParkSite, model.data)
summary(my.lme)
#tTable0<-summary(my.lme)$tTable
#tTable<-signif(tTable0,digits=3)
#write.csv(file="tTable_univar.csv",tTable)

variables<-unique(model.data$variable)
nVariables<-unique(variables)
for (i in 1:length(nVariables)){
  variablei<-nVariables[i]
  whichdatai<-subset(model.data,model.data$variable==variablei)
  filenameai<-paste("../figures/",variablei,"bylake.png",sep="")
  filenamebi<-paste("../figures/",variablei,".png",sep="")  
  plotai <- ggplot(whichdatai, aes(x = year, y = value,color=ParkSite)) +
    geom_point()+
    ylab(paste("value ",whichdatai$unitslabel,sep=""))+
    scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018))+
    facet_wrap(~ParkSite)+
    theme_bw()
  #  ploti
  ggsave(plot = plotai,width=14,height=6,units="in",
         filename = filenameai)
  plotbi <- ggplot(whichdatai, aes(x = year, y = value,color=ParkSite)) +
    geom_point()+
    ylab(paste("value ",whichdatai$unitslabel,sep=""))+    
    scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018))+
    theme_bw()
  #  ploti
  ggsave(plot = plotbi,width=9,height=4,units="in",
         filename = filenamebi)
}

#variables_choose<-c(variables[1:19],variables[20:23],variables[c(38,39,46)],variables[74],variables[c(61:65,67:71)])
variables_choose<-c(variables[1:19],variables[c(38,39,46)],variables[74],variables[c(61:63,65,67:69,71)])
model.data.simp<-model.data %>% select(park_code,ParkSite,year,variable,value,unitslabel) %>% as.data.frame()
model.data.simp<-model.data.simp[which(model.data.simp$variable %in% variables_choose),]

model.data.cross<-merge(model.data.simp,model.data.simp,by=c("park_code","ParkSite","year"))
model.data.cross$variable_pair<-paste(model.data.cross$variable.x,"_",model.data.cross$variable.y,sep="")
model.data.cross<-subset(model.data.cross,model.data.cross$variable.x!=model.data.cross$variable.y)
model.data.cross$all<-1
model.data.cross$VariablePairParksite<-paste(model.data.cross$variable_pair,"_",model.data.cross$ParkSite,sep="")
model.data.cross$VariablePairParkcode<-paste(model.data.cross$variable_pair,"_",model.data.cross$park_code,sep="")
which1<-grep("Temp",model.data.cross$variable.x)
which2<-grep("Temp",model.data.cross$variable.y)
model.data.cross2<-model.data.cross[-which1[which1 %in% which2],]
model.data.cross2<-unique(model.data.cross2)

# big job!
#my.lme.pairs <-  lme(value.y ~ value.x:variable_pair-1, random=~1|ParkSite, model.data.cross2)
#summary(my.lme.pairs)

#tTable0<-summary(my.lme.pairs)$tTable
#tTable<-signif(tTable0,digits=3)
#write.csv(file="tTable.csv",tTable)


# many plots

plotdata<-model.data.cross2
nVariables<-unique(plotdata$variable.x)
for (i in 1:length(nVariables)){
  variablei<-nVariables[i]
  plotdatai<-subset(plotdata,substr(plotdata$variable_pair,1,nchar(variablei))==variablei)
  filenamei<-paste("../figures/",variablei,"_bivar.png",sep="")
  cross.ploti <- ggplot(plotdatai, aes(x = value.x, y = value.y,color=ParkSite)) +
    geom_point(size=1)+
    ylab(paste("value ",whichdatai$unitslabel.y,sep=""))+
    ylab(paste("value ",whichdatai$unitslabel.x,sep=""))+    
    facet_wrap(~variable_pair,scales="free")+
    theme_bw()
  #  cross.ploti
  ggsave(plot = cross.ploti,width=16,height=10,units="in",dpi=150,
         filename = filenamei)
}


