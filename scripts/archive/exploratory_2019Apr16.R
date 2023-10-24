
library(readxl)
library(tidyverse)
library(lubridate)
library(reshape2)
library(nlme) # needed for mixed model


# Bumbling around:

# Site = physical location in park?
site_meta <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b014_Sites_export_20190923_111555.xlsx")) %>%
  as.data.frame()

# Location = type of sampling that occurred? 
loc_meta <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b024_Locations_export_20190923_111440.xlsx")) %>%
  as.data.frame()

# Lake level
lake_level <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b304_Lake_Level_Events_20190923_121946.xlsx")) %>%
  as.data.frame()


loc_meta
site_meta

# Something with more direction: 

######################
waterchem_samples <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b334_Water_Chemistry_Samples_select_20190923_160153.xlsx")) %>% as.data.frame()

waterchem_data <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b344_Water_Chemistry_Data_select_20190923_160319.xlsx")) %>% as.data.frame()

colnames<-c(unique(names(waterchem_samples)),unique(names(waterchem_data)))
colnames_both<-colnames[duplicated(colnames)]

merged_waterchem<-merge(waterchem_samples,waterchem_data,by=colnames_both)
merged_waterchem$ParkSite<-paste(merged_waterchem$Park_code,merged_waterchem$Site_name)
merged_waterchem$month<-as.numeric(month(merged_waterchem$Start_date))


unique(data.frame(analyte=waterchem_data$Analyte,depth=waterchem_data$Water_sample_depth))


# Exclude replicates? Important
merged_waterchem <- subset(merged_waterchem,merged_waterchem$Replicate_tf==0)

##########################################
# check for missingness
# set up vectors for site+depth+year+analyte combos
years<-sort(unique(merged_waterchem$Event_year))
years<-years[-which(years==2017)]
years<-years[-which(years==2007)]
sites<-sort(unique(merged_waterchem$Site_name))
depths<-sort(unique(merged_waterchem$Water_sample_depth))
analytes<-sort(unique(merged_waterchem$Analyte))
# make dataframe of all site+depth+year+analyte combos
samplingcombos<-unique(expand.grid(sites=sites,depths=depths,years=years,analytes=analytes))

# merge the sampling combos with waterchem data (all=TRUE)
missing<-merge(samplingcombos,merged_waterchem,by.y=c("Site_name","Water_sample_depth","Analyte","Event_year"),by.x=c("sites","depths","analytes","years"),all=TRUE)
missing$missingTF<-0
missing$missingTF[which(is.na(missing$Value))]<-1 # mark rows that have a missing value
missing<-missing %>% arrange(analytes,sites,depths)

# split into two groups: missing surface values, missing mid-depth values
missing.surface<-subset(missing,missing$depths=="Surface" & missing$missingTF==1)
missing.mid<-subset(missing,missing$depths=="Mid-Depth" & missing$missingTF==1)

# write lists of 'the missing' to csv
write.csv(subset(missing.surface,missing.surface$missingTF==1),"../data/analysis_outputs/missing_surfwaterchem.csv")
write.csv(subset(missing.mid,missing.mid$missingTF==1),"../data/analysis_outputs/missing_midwaterchem.csv")

#########################################
# do manual fixes, as needed

# Ambiguous entries for analyte= NO3/NO3n ? Placeholder, needs checking with Nat Parks folks!!
subset(merged_waterchem,merged_waterchem$Analyte=="NO3n")
which<-which(merged_waterchem$Analyte=="NO3n")
#merged_waterchem$Value[which]<-merged_waterchem$Value[which]*14/62 #correction from NO3 to NO3n
merged_waterchem$Analyte[which]<-"NO3"

# Was LH15 2015 chlorophyll sample was incorrectly labeled as field blank? If so, do correction...
subset(merged_waterchem,merged_waterchem$Site_name=="LH15" & merged_waterchem$Water_sample_ID==20 & merged_waterchem$Water_sample_type=="Chlorophyll" & merged_waterchem$Event_year==2015)
which<-which(merged_waterchem$Site_name=="LH15" & merged_waterchem$Water_sample_ID==20 & merged_waterchem$Water_sample_type=="Chlorophyll" & merged_waterchem$Event_year==2015)
merged_waterchem$Field_blank_tf[which]<-0

################################

merged_waterchem <- subset(merged_waterchem,merged_waterchem$Replicate_tf==0 & merged_waterchem$Lab_replicate_tf==0 & merged_waterchem$Field_blank_tf==0)

#doc<-subset(merged_waterchem, merged_waterchem$Analyte=="DOC")
#chl<-subset(merged_waterchem, merged_waterchem$Analyte=="Chlorophyll")
doc<-subset(merged_waterchem, merged_waterchem$Analyte=="DOC" & merged_waterchem$Water_sample_depth=="Surface")
chl<-subset(merged_waterchem, merged_waterchem$Analyte=="Chlorophyll" & merged_waterchem$Water_sample_depth=="Mid-Depth")
#doc<-subset(merged_waterchem, merged_waterchem$Analyte=="DOC" & merged_waterchem$Water_sample_depth=="Surface")
#chl<-subset(merged_waterchem, merged_waterchem$Analyte=="Chlorophyll" & merged_waterchem$Water_sample_depth=="Surface")

anc<-subset(merged_waterchem, merged_waterchem$Analyte=="ANC" & merged_waterchem$Water_sample_depth=="Mid-Depth")
ca<-subset(merged_waterchem, merged_waterchem$Analyte=="Ca" & merged_waterchem$Water_sample_depth=="Mid-Depth")
cl<-subset(merged_waterchem, merged_waterchem$Analyte=="Cl" & merged_waterchem$Water_sample_depth=="Mid-Depth")
k<-subset(merged_waterchem, merged_waterchem$Analyte=="K" & merged_waterchem$Water_sample_depth=="Mid-Depth")
mg<-subset(merged_waterchem, merged_waterchem$Analyte=="Mg" & merged_waterchem$Water_sample_depth=="Mid-Depth")
sod<-subset(merged_waterchem, merged_waterchem$Analyte=="Na" & merged_waterchem$Water_sample_depth=="Mid-Depth")
nh3<-subset(merged_waterchem, merged_waterchem$Analyte=="NH3" & merged_waterchem$Water_sample_depth=="Mid-Depth")
nh4<-subset(merged_waterchem, merged_waterchem$Analyte=="NH4" & merged_waterchem$Water_sample_depth=="Mid-Depth")
no3<-subset(merged_waterchem, merged_waterchem$Analyte=="no3" & merged_waterchem$Water_sample_depth=="Mid-Depth")
no3no2<-subset(merged_waterchem, merged_waterchem$Analyte=="NO3+NO2" & merged_waterchem$Water_sample_depth=="Mid-Depth")
pheo<-subset(merged_waterchem, merged_waterchem$Analyte=="Pheophytin" & merged_waterchem$Water_sample_depth=="Mid-Depth")
po4<-subset(merged_waterchem, merged_waterchem$Analyte=="PO4" & merged_waterchem$Water_sample_depth=="Mid-Depth")
so4<-subset(merged_waterchem, merged_waterchem$Analyte=="SO4" & merged_waterchem$Water_sample_depth=="Mid-Depth")
tds<-subset(merged_waterchem, merged_waterchem$Analyte=="TDS" & merged_waterchem$Water_sample_depth=="Mid-Depth")
totn<-subset(merged_waterchem, merged_waterchem$Analyte=="Total N" & merged_waterchem$Water_sample_depth=="Mid-Depth")
totp<-subset(merged_waterchem, merged_waterchem$Analyte=="Total P" & merged_waterchem$Water_sample_depth=="Mid-Depth")

#check duplicates
chl.checkdups<-chl %>% select(Park_code,Site_code,Site_name,Start_date,Location_code,Analyte,Water_sample_depth_value)
sum(duplicated(chl.checkdups))

# units check
unique(chl$Units)
unique(doc$Units)

chldoc0<-subset(merged_waterchem, merged_waterchem$Analyte %in% c("Chlorophyll","DOC"))
chldoc.trim<-chldoc0 %>%  select(ParkSite,Park_code,Site_code,Site_name,Event_year,Start_date,Water_sample_depth,Analyte,value=Value,Units)
chldoc<-dcast(chldoc.trim, ParkSite+Park_code+Site_code+Site_name+Start_date~ Analyte, mean)
names(chldoc)[which(names(chldoc)=="Chlorophyll")]<-"chl"
names(chldoc)[which(names(chldoc)=="DOC")]<-"doc"

#########################







##########################

do<-0
if(do==1){
  
  #########################
  # Chlorophyll and DOC plots
  
  chla.plot.bysite <- ggplot(chl, aes(x = Start_date, y = log10(Value+0.03),color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',chl$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  
  chla.plot <- ggplot(chl, aes(x = Start_date, y = log10(Value+0.03),color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',chl$Units[1],')',sep=""))
  
  doc.plot.bysite <- ggplot(doc, aes(x = Start_date, y = (Value),color=ParkSite)) +
    geom_point() +
    #  ylim(0,50)+
    ylab(paste('Value (',doc$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)#, scales = "free" )
  
  doc.plot <- ggplot(doc, aes(x = Start_date, y = log10(Value),color=ParkSite)) +
    geom_point()+
    ylab(paste('Value (',doc$Units[1],')',sep=""))
  
  chldoc.biplot.bysite <- ggplot(chldoc, aes(x = chl, y = doc,color=ParkSite)) +
    geom_point() +
    facet_wrap(~ParkSite)
  
  chldoc.biplot <- ggplot(chldoc, aes(x = log10(chl+0.03), y = log10(doc),color=ParkSite)) +
    geom_point()+
    facet_wrap(~Park_code)
  
  chla.plot.bysite
  chla.plot
  doc.plot.bysite
  doc.plot
  chldoc.biplot.bysite
  chldoc.biplot
  
  #################################
  
  # Other plots
  
  anc.plot.bysite <- ggplot(anc, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',anc$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  anc.plot <- ggplot(anc, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',anc$Units[1],')',sep=""))
  
  ca.plot.bysite <- ggplot(ca, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',ca$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  ca.plot <- ggplot(ca, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',ca$Units[1],')',sep=""))
  
  cl.plot.bysite <- ggplot(cl, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',cl$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  cl.plot <- ggplot(cl, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',cl$Units[1],')',sep=""))
  
  k.plot.bysite <- ggplot(k, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',k$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  k.plot <- ggplot(k, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',k$Units[1],')',sep=""))
  
  mg.plot.bysite <- ggplot(mg, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',mg$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  mg.plot <- ggplot(mg, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',mg$Units[1],')',sep=""))
  
  sod.plot.bysite <- ggplot(sod, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',sod$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  sod.plot <- ggplot(sod, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',sod$Units[1],')',sep=""))
  
  nh3.plot.bysite <- ggplot(nh3, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',nh3$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  nh3.plot <- ggplot(nh3, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',nh3$Units[1],')',sep=""))
  
  nh4.plot.bysite <- ggplot(nh4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',nh4$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  nh4.plot <- ggplot(nh4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',nh4$Units[1],')',sep=""))
  
  no3.plot.bysite <- ggplot(no3, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',no3$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  no3.plot <- ggplot(no3, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',no3$Units[1],')',sep=""))
  
  no3no2.plot.bysite <- ggplot(no3no2, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',no3no2$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  no3no2.plot <- ggplot(no3no2, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',no3no2$Units[1],')',sep=""))
  
  pheo.plot.bysite <- ggplot(pheo, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',pheo$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  pheo.plot <- ggplot(pheo, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',pheo$Units[1],')',sep=""))
  
  po4.plot.bysite <- ggplot(po4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',po4$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  po4.plot <- ggplot(po4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',po4$Units[1],')',sep=""))
  
  so4.plot.bysite <- ggplot(so4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',so4$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  so4.plot <- ggplot(so4, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',so4$Units[1],')',sep=""))
  
  tds.plot.bysite <- ggplot(tds, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',tds$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  tds.plot <- ggplot(tds, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',tds$Units[1],')',sep=""))
  
  totn.plot.bysite <- ggplot(totn, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totn$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  totn.plot <- ggplot(totn, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totn$Units[1],')',sep=""))
  
  totp.plot.bysite <- ggplot(totp, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totp$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  totp.plot <- ggplot(totp, aes(x = Start_date, y = Value,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totp$Units[1],')',sep=""))
  
  
  #################################
  
  months.plot.bysite <- ggplot(chl, aes(x = Start_date, y = month,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totp$Units[1],')',sep=""))+
    facet_wrap(~ParkSite)
  months.plot <- ggplot(chl, aes(x = Start_date, y = month,color=ParkSite)) +
    geom_point() +
    ylab(paste('Value (',totp$Units[1],')',sep=""))
  
  #################################
  # Save Chlorophyll and DOC plot images
  
  ggsave(plot = chla.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/chla.plot.bysite.png")
  
  ggsave(plot = doc.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/doc.plot.bysite.png")
  
  ggsave(plot = chla.plot,width=10,height=3.5,units="in",
         filename = "../figures/chla.plot.png")
  
  ggsave(plot = doc.plot,width=10,height=3.5,units="in",
         filename = "../figures/doc.plot.png")
  
  ggsave(plot = chldoc.biplot,width=15,units="in",
         filename = "../figures/chldoc.biplot.png")
  
  ###################
  # Save other plot images
  
  ggsave(plot = anc.plot,width=10,height=3.5,units="in",
         filename = "../figures/anc.plot.png")
  ggsave(plot = ca.plot,width=10,height=3.5,units="in",
         filename = "../figures/ca.plot.png")
  ggsave(plot = cl.plot,width=10,height=3.5,units="in",
         filename = "../figures/cl.plot.png")
  ggsave(plot = k.plot,width=10,height=3.5,units="in",
         filename = "../figures/k.plot.png")
  ggsave(plot = mg.plot,width=10,height=3.5,units="in",
         filename = "../figures/mg.plot.png")
  ggsave(plot = sod.plot,width=10,height=3.5,units="in",
         filename = "../figures/sod.plot.png")
  ggsave(plot = nh3.plot,width=10,height=3.5,units="in",
         filename = "../figures/nh3.plot.png")
  ggsave(plot = nh4.plot,width=10,height=3.5,units="in",
         filename = "../figures/nh4.plot.png")
  ggsave(plot = no3.plot,width=10,height=3.5,units="in",
         filename = "../figures/no3.plot.png")
  ggsave(plot = no3no2.plot,width=10,height=3.5,units="in",
         filename = "../figures/no3no2.plot.png")
  ggsave(plot = pheo.plot,width=10,height=3.5,units="in",
         filename = "../figures/pheo.plot.png")
  ggsave(plot = po4.plot,width=10,height=3.5,units="in",
         filename = "../figures/po4.plot.png")
  ggsave(plot = so4.plot,width=10,height=3.5,units="in",
         filename = "../figures/so4.plot.png")
  ggsave(plot = tds.plot,width=10,height=3.5,units="in",
         filename = "../figures/tds.plot.png")
  ggsave(plot = totn.plot,width=10,height=3.5,units="in",
         filename = "../figures/totn.plot.png")
  ggsave(plot = totp.plot,width=10,height=3.5,units="in",
         filename = "../figures/totp.plot.png")
  
  ggsave(plot = anc.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/anc.plot.bysite.png")
  ggsave(plot = ca.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/ca.plot.bysite.png")
  ggsave(plot = cl.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/cl.plot.bysite.png")
  ggsave(plot = k.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/k.plot.bysite.png")
  ggsave(plot = mg.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/mg.plot.bysite.png")
  ggsave(plot = sod.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/sod.plot.bysite.png")
  ggsave(plot = nh3.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/nh3.plot.bysite.png")
  ggsave(plot = nh4.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/nh4.plot.bysite.png")
  ggsave(plot = no3.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/no3.plot.bysite.png")
  ggsave(plot = no3no2.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/no3no2.plot.bysite.png")
  ggsave(plot = pheo.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/pheo.plot.bysite.png")
  ggsave(plot = po4.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/po4.plot.bysite.png")
  ggsave(plot = so4.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/so4.plot.bysite.png")
  ggsave(plot = tds.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/tds.plot.bysite.png")
  ggsave(plot = totn.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/totn.plot.bysite.png")
  ggsave(plot = totp.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/totp.plot.bysite.png")
  
  #################################
  ggsave(plot = months.plot,width=10,height=3.5,units="in",
         filename = "../figures/months.plot.png")
  ggsave(plot = months.plot.bysite,width=14,height=6,units="in",
         filename = "../figures/months.bysite.png")
  
  ################################
  # Questions
  # Which precip data (external) should we use ?
  # Looks like we have inconsistent units for N variables (e.g., nitrate conc vs. nitrate-N conc ? How to correct? (no3n, no3, nh4,nh3, no3no2)
  
}






model.data<-merged_waterchem
model.data$year<-model.data$Event_year
model.data$AnalyteParksite<-paste(model.data$Analyte,"_",model.data$ParkSite,sep="")
model.data$all<-1

model.data.simp<-model.data %>% select(Park_code,ParkSite,year,Analyte,Value) %>% as.data.frame()
model.data.cross<-merge(model.data.simp,model.data.simp,by=c("Park_code","ParkSite","year"))
model.data.cross$Analyte_pair<-paste(model.data.cross$Analyte.x,"_",model.data.cross$Analyte.y,sep="")
model.data.cross<-subset(model.data.cross,model.data.cross$Analyte.x!=model.data.cross$Analyte.y)
model.data.cross$all<-1
model.data.cross$AnalytePairParksite<-paste(model.data.cross$Analyte_pair,"_",model.data.cross$ParkSite,sep="")
model.data.cross$AnalytePairParkcode<-paste(model.data.cross$Analyte_pair,"_",model.data.cross$Park_code,sep="")

# Fit simple change model
#my.lme <-  lme(Value ~ year*Analyte, random=~1|ParkSite, model.data)
my.lme <-  lme(Value ~ year:Analyte-1, random=~1|ParkSite, model.data)
summary(my.lme)


#model.data.cross2<-subset(model.data.cross,model.data.cross$ParkSite=="MORA Lake Allen")
model.data.cross2<-subset(model.data.cross,model.data.cross$Park_code=="MORA")

# next line is hard work
# my.lme.pairs <-  lme(Value.y ~ Value.x*Analyte_pair, random=~1|ParkSite, model.data.cross)
#my.lme.pairs <-  lme(Value.y ~ Value.x+Analyte_pair, random=~1|ParkSite, model.data.cross)
#my.lme.pairs <-  lme(Value.y ~ Value.x+Analyte_pair+ParkSite, random=~1|all, model.data.cross)
#my.lme.pairs <-  lme(Value.y ~ Value.x:Analyte_pair:ParkSite, random=~1|all, model.data.cross)
#my.lme.pairs <-  lme(Value.y ~ Value.x+Analyte_pair:ParkSite, random=~1|all, model.data.cross)
#my.lme.pairs <-  lme(Value.y ~ Value.x:AnalytePairParksite, random=~1|all, model.data.cross2)
#my.lme.pairs <-  lme(Value.y ~ Value.x:AnalytePairParkcode, random=~1|ParkSite, model.data.cross2)
#my.lme.pairs <-  lme(Value.y ~ Value.x:AnalytePairParkcode, random=~1|ParkSite, model.data.cross)
my.lme.pairs <-  lme(Value.y ~ Value.x:Analyte_pair, random=~1|ParkSite, model.data.cross)
summary(my.lme.pairs)

test<-subset(model.data.cross2,model.data.cross2$AnalytePairParkcode=="K_Mg_MORA")


nAnalytes<-unique(model.data.cross$Analyte.x)
for (i in 1:length(nAnalytes)){
  analytei<-nAnalytes[i]
  whichcrossi<-subset(model.data.cross,substr(model.data.cross$Analyte_pair,1,nchar(analytei))==analytei)
  filenamei<-paste("../figures/",analytei,"_bivar.png",sep="")
  cross.ploti <- ggplot(whichcrossi, aes(x = Value.x, y = Value.y,color=ParkSite)) +
    geom_point()+
    facet_wrap(~Analyte_pair,scales="free")+
    theme_bw()
  cross.ploti
  ggsave(plot = cross.ploti,width=14,height=6,units="in",
         filename = filenamei)
}

nAnalytePairs<-unique(model.data.cross$Analyte_pair)
for (i in 1:length(nAnalytePairs)){
  analytei<-nAnalytePairs[i]
  whichcrossi<-subset(model.data.cross,substr(model.data.cross$Analyte_pair,1,nchar(analytei))==analytei)
  filenamei<-paste("../figures/bivarbylake/",analytei,"_bivarbylake.png",sep="")
  cross.ploti <- ggplot(whichcrossi, aes(x = Value.x, y = Value.y,color=ParkSite)) +
    geom_point()+
    facet_wrap(~ParkSite)+
    theme_bw()
  cross.ploti
  ggsave(plot = cross.ploti,width=14,height=6,units="in",
         filename = filenamei)
}





plot(test$Value.x,test$Value.y)

whichcross<-subset(model.data.cross,substr(model.data.cross$Analyte_pair,1,2)=="Na")
cross.plot <- ggplot(whichcross, aes(x = Value.x, y = Value.y,color=ParkSite)) +
  geom_point()+
  facet_wrap(~Analyte_pair,scales="free")
cross.plot



whichcross<-subset(model.data.cross,model.data.cross$Analyte_pair=="Chlorophyll_Mg")
cross.plot <- ggplot(model.data.cross, aes(x = Value.x, y = Value.y,color=ParkSite)) +
  geom_point()+
  facet_wrap(~ParkSite)
cross.plot


#my.lme <-  lme(month ~ ParkSite, random=~1, model.data)
my.lme <-  lme(month ~ Park_code, random=~1|ParkSite, model.data)
summary(my.lme)

#my.aov<-aov(month ~ ParkSite,model.data)
my.aov<-aov(month ~ Park_code,model.data)
summary(my.aov)
TukeyHSD(my.aov)

#pred.data<-model.data
#pred.data$preds<-as.numeric(predict(my.lme.ln.U.Fmed))
pred.data<-model.data
pred.data$xpred<-seq(from=min(model.data$Fmed),to=max(model.data$Fmed),length.out=length(model.data[,1]))
pred.data$preds<-as.numeric(coef(my.lme.ln.U.Fmed)[1,2])*pred.data$xpred+as.numeric(coef(my.lme.ln.U.Fmed)[1,1])

one<-ggplot(pred.data, aes(x =Fmed, y = ln.U) ) +
  geom_point(size=0.5) +
  ylab("")+
  #  ylab("Areal retention of eDNA (log U)")+
  xlab("Fmed (%)")+  
  geom_line(aes(x =xpred, y = preds), size = 1,color="dark gray")+  
  #  geom_line(aes(y = preds), size = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
one