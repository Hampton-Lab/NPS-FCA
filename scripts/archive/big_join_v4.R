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
    Lake == "Lake Sunup" ~ "623",
    Lake == "Hoh" ~ "Hoh"))


# 3. Trim datasets --------------------------------------------------------

water_chem <- subset(water_chem,water_chem$replicate_tf==0)

#water_chem_trim <- water_chem %>%
#  select(park_code, site_code, start_date, event_year, event_month,
#         variable = analyte, value)

water_chem_trim <- water_chem %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable = analyte, value) %>% 
  group_by(park_code, site_code, start_date, event_year, event_month,variable) %>%
    dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% as.data.frame()

lake_level <- lake_level %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = mean_level_cm)


# 3a. Work up water temperatures ------------------------------------------

# Daily - Note that if gathering only a single column that the gather
# statement will be pointless.
daily_temp_long <- daily_temperature %>%
  gather(key = variable, value = value, mean_value) %>%
#  gather(key = variable, value = value, mean_value, perc_10, perc_90) %>%  
  unite(col = variable, measure, variable)

daily_temp_long$variable<-gsub("_mean_value","",daily_temp_long$variable)
daily_temp_long$variable<-gsub("BottomTemp","BotTemp",daily_temp_long$variable)
daily_temp_long$variable<-gsub("SurfaceTemp","SurfTemp",daily_temp_long$variable)    

#daily_temp_long$variable[which(daily_temp_long$variable=="AirTemp_mean_value")]<-"AirTempMean"
#daily_temp_long$variable[which(daily_temp_long$variable=="BottomTemp_mean_value")]<-"BotTempMean"
#daily_temp_long$variable[which(daily_temp_long$variable=="MidTemp_mean_value")]<-"MidTempMean"
#daily_temp_long$variable[which(daily_temp_long$variable=="SurfaceTemp_mean_value")]<-"SurfTempMean"


#testmerge<-merge(water_chem_trim,daily_temp_long,by.x=c("park_code","site_code","start_date","event_year","event_month"),
#                 by.y=c("park_code","site_code","event_date","event_year","event_month"),all=TRUE)


daily_temp_chemdate_match <- semi_join(daily_temp_long, water_chem_trim,
                                       by = c("park_code",
                                              "site_code",
                                              "event_date" = "start_date",
                                              "event_year",
                                              "event_month")) %>%
  select(park_code, site_code, start_date = event_date, event_year, event_month, variable,
         value)

# Monthly
monthly_temp_long <- monthly_temperature %>%
  gather(key = variable, value = value, mean_value) %>%
#  gather(key = variable, value = value, mean_value, perc_10, perc_90) %>%
  unite(col = variable, measure, variable, event_month, remove = F)

monthly_temp_long <- monthly_temp_long %>%
  filter(event_month <= 8) %>%
  select(-measure) %>%
  na.omit()

monthly_temp_long$variable<-gsub("_mean_value","",monthly_temp_long$variable)
monthly_temp_long$variable<-gsub("BottomTemp","BotTemp",monthly_temp_long$variable)
monthly_temp_long$variable<-gsub("SurfaceTemp","SurfTemp",monthly_temp_long$variable)    
#monthly_temp_long$variable<-gsub("_","_m",monthly_temp_long$variable)   

# Filter for mean measures only
#monthly_temp_means_long <- monthly_temp_long %>%
#  filter(grepl(pattern = "mean", x = variable))

monthly_temp_chemdate_match <- inner_join(monthly_temp_long,
                                          water_chem_trim,
                                          by = c("park_code",
                                                 "site_code",
                                                 "event_year")) %>%
  select(park_code, site_code, start_date, event_year,
         event_month = event_month.y, variable = variable.x, value = value.x) %>%
  unique() # this removes duplicate rows from when there were additional cols

# 4. Workup profiles --------------------------------------------------------
water_prof_old <- water_prof %>%
  mutate(parameter = if_else(condition = parameter == "Temperature",
                             true = "ProfTemp",
                             false = parameter),
         depth_groupbin = case_when(depth_bin_m <= 2 ~ "top2m",
                                    depth_bin_m > 2 ~ "below2m"))

water_prof_binned <- water_prof_old %>%
  group_by(park_code, site_code, event_year, event_month,
           start_date, depth_groupbin, parameter) %>%
  summarize(mean = mean(parameter_value, na.rm = TRUE)) %>%
  unite(col = variable, parameter, depth_groupbin) %>%
  select(park_code, site_code, start_date, event_year, event_month,
         variable, value = mean) %>%
  as.data.frame()


# 5. Join datasets --------------------------------------------------------

limno <- bind_rows(
    water_chem_trim,
    daily_temp_chemdate_match,
    monthly_temp_chemdate_match,
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

bigjoin0<-bigjoin
#model.data<-model.data[-which(is.na(model.data$variable)==TRUE),]
bigjoin<-bigjoin[-which(is.na(bigjoin$value)==TRUE),]
bigjoin$year<-bigjoin$event_year
bigjoin$ParkSite<-paste(bigjoin$park_code,bigjoin$Lake)
bigjoin$VariableParksite<-paste(bigjoin$variable," ",bigjoin$ParkSite,sep="")
bigjoin$all<-1
bigjoin<-unique(bigjoin)

model.data<-bigjoin

univar_summaries<-model.data %>% group_by(ParkSite,variable,year,units) %>% 
  dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% as.data.frame()
univar_summaries<-univar_summaries[(order(univar_summaries$variable,univar_summaries$ParkSite,univar_summaries$year)),]
write.csv(file="../univar_summaries.csv",univar_summaries)


# Fit simple "univariate" change model
my.lme <-  lme(value ~ year:variable-1, random=~1|ParkSite, model.data)
summary(my.lme)
tTable0<-summary(my.lme)$tTable
tTable<-signif(tTable0,digits=3)
write.csv(file="../tTable_univar.csv",tTable)

variables<-unique(model.data$variable)
nVariables<-unique(variables)
for (i in 1:length(nVariables)){
  variablei<-nVariables[i]
  whichdatai<-subset(model.data,model.data$variable==variablei)
  filenameai<-paste("../figures/bylake_",variablei,".png",sep="")
  filenamebi<-paste("../figures/onepanel_",variablei,".png",sep="")  
  plotai <- ggplot(whichdatai, aes(x = year, y = value,color=park_code)) +
    geom_point()+
#    ylab(paste("value ",whichdatai$unitslabel,sep=""))+
#    ylab(paste("value ",whichdatai$unitslabel.y,sep=""))+
    ylab(paste(whichdatai$variable[1], " ",whichdatai$unitslabel[1],sep=""))+    
    scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018))+
    facet_wrap(~ParkSite)+
    theme_bw()+
    theme(legend.position = "none")
  #  ploti
  ggsave(plot = plotai,width=10,height=6,units="in",
         filename = filenameai)
  plotbi <- ggplot(whichdatai, aes(x = year, y = value,color=ParkSite)) +
    geom_point()+
#    ylab(paste("value ",whichdatai$unitslabel,sep=""))+    
    ylab(paste(whichdatai$variable[1], " ",whichdatai$unitslabel[1],sep=""))+        
    scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018))+
    theme_bw()+
    guides(col=guide_legend(ncol=2))
  #  ploti
  ggsave(plot = plotbi,width=9,height=4,units="in",
         filename = filenamebi)
}

#variables_choose<-c(variables[1:19],variables[20:23],variables[c(38,39,46)],variables[74],variables[c(61:65,67:71)])
#variables_choose<-c(variables[1:19],variables[c(38,39,46)],variables[74],variables[c(61:63,65,67:69,71)])
whichvars_nontemp<-grep("temp",variables,ignore.case=TRUE)
varschoose1<-variables[-whichvars_nontemp]
varschoose2<-c("BottomTemp_mean_value_6","SurfaceTemp_mean_value_6","MidTemp_mean_value_6","AirTemp_mean_value_6",
               "BottomTemp_mean_value","SurfaceTemp_mean_value","MidTemp_mean_value","AirTemp_mean_value",
               "ProfTemp_top2m","ProfTemp_below2m")
variables_choose<-c(varschoose1,varschoose2)

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
  filenamei<-paste("../figures/bivar_",variablei,".png",sep="")
  cross.ploti <- ggplot(plotdatai, aes(x = value.x, y = value.y,color=ParkSite)) +
    geom_point(size=1)+
    ylab("value")+    
#    ylab(paste("value ",plotdatai$unitslabel.y,sep=""))+
#    ylab(paste("value ",whichdatai$unitslabel.x,sep=""))+ 
    xlab(paste(plotdatai$variable.x, " ",plotdatai$unitslabel.x,sep=""))+        
#    facet_wrap(~variable_pair,scales="free")+
    facet_wrap(~paste(plotdatai$variable.y, " ",plotdatai$unitslabel.y,sep=""),scales="free")+    
    theme_bw()+
    guides(col=guide_legend(ncol=1))
  #  cross.ploti
  ggsave(plot = cross.ploti,width=16,height=10,units="in",dpi=150,
         filename = filenamei)
}


dataslots<- expand.grid(ParkSite=sort(unique(bigjoin$ParkSite)),event_year=sort(unique(bigjoin$event_year)),variable=sort(unique(bigjoin$variable)))


expose_missing<-merge(dataslots,bigjoin,by=c("ParkSite","event_year","variable"),all=TRUE)
expose_missing$hasdata<-1
expose_missing$hasdata[which(is.na(expose_missing$value)==TRUE)]<-0

dataornot<-expose_missing %>% group_by(ParkSite, event_year, variable) %>%
  dplyr::summarize(hasdata=max(hasdata)) %>%  
  arrange(ParkSite, variable,event_year) %>%  as.data.frame()

dataornot_wide<-dataornot %>% spread(variable,hasdata,fill=0)

write.csv(file="../data1_missing0_long.csv",dataornot)
write.csv(file="../data1_missing0_wide.csv",dataornot_wide)


