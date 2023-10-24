library(nlme) # needed for mixed model
library(broom);library(tidyr);library(tidyverse);library(broom.mixed)
library(car);library(reshape2);library(corrplot)
library(ggplot2);library(ggrepel);library(gridExtra)
library(lme4)
library(viridis)
library(tuple)

bigjoin <- readRDS(file = file.path("..","data", "analysis_outputs", "bigjoin.rds"))

short_codes <- tribble(
  ~Lake, ~short_code,
  "Upper Palisades Lake",     "PA",
  "Lake LH15",     "15",
  "Lake Allen",     "AL",
  "Lake LP19",     "19",
  "Upper Deadwood Lake",     "DW",
  "Blue Lake",     "BL",
  "Lower Blum Lake",     "LB",
  "Lower Silent Lake",     "SI",
  "Easy Ridge Lake",     "ER",
  "Lower East Lake",     "LE",
  "Bowan Lake",     "BO",
  "Upper Triplet Lake",     "TR",
  "Gladys Lake",     "GL",
  "Ferry Lake",     "FE",
  "Heather Lake",     "HE",
  "Crazy Lake",     "CR",
  "Milk Lake",     "MI",
  "Lake La Crosse",     "LC",
  "Lake Sunup",     "SU",
  "Lake Connie",     "CO",
  "Hoh Lake",     "HO"
)

labels_tbl <- tribble(
  ~variable, ~axis_label, ~var_abbrev1, ~var_abbrev2,
  "secchi_value_m",     "Secchi depth (m)", "Secchi","Secchi(m)",
  "ProfTemp_top2m", paste0("Water temperature (", intToUtf8(176), "C), top 2m"),"Watertemp",paste0("Watertemp(", intToUtf8(176), "C)"),
  "SWE_May", "May SWE (cm)","SWE","SWE(cm)",
  "DO_top2m", "D.O. (mg/L), top 2m","DO","DO(mg/L)",
  "pH_top2m", "pH, top 2m","pH","pH",
  "SpCond_top2m", paste0("Sp. conductivity (", intToUtf8(956), "S/cm), top 2m"),"Cond",paste0("Cond(", intToUtf8(956), "S/cm)"),
  "Chlorophyll", paste0("Chlorophyll (", intToUtf8(956), "g/L)"),"Chl",paste0("Chl(", intToUtf8(956), "g/L)"),
  "Na", "Na (mg/L)","Na", "Na(mg/L)",
  "Cl", "Cl (mg/L)","Cl","Cl(mg/L)",
  "Mg", "Mg (mg/L)","Mg","Mg(mg/L)",
  "K", "K (mg/L)","K","K(mg/L)",
  "Ca", "Ca (mg/L)","Ca","Ca(mg/L)",
  "SO4", "SO4 (mg/L)","SO4","SO4(mg/L)",
  "Total N", "TDN (mg/L)", "TDN","TDN(mg/L)",# IMPORTANT! - these were filtered samples 
  "Total P", "TDP (mg/L)", "TDP", "TDP(mg/L)",# IMPORTANT! - these were filtered samples   
  "ice_out_doy","Ice out day of year", "Iceout day","Iceout day",
  "flush_index_SWE_May","Catchment snow vol : lake vol","Snowvol:lakevol","Snowvol:lakevol"
)

snowsite_tbl <- tribble(
  ~Snowsite, ~snowsite_code,
  "Paradise",     "Para",
  "Cayuse", "Cayu",
  "Easy", "Easy",
  "Browntop","Brow",
  "ParkCreek", "Park",
  "Harts","Hart",
  "Buckinghorse","Buck",
  "Waterhole","Wate"
)

# convert units of insolation to per day
bigjoin$solar_jas<-bigjoin$solar_jas/(365*.25)
bigjoin$solar_dec<-bigjoin$solar_dec/31

bigjoin<- bigjoin %>% 
  rename(SWE_May_snodas=SWE_May, SWE_Apr_snodas=SWE_Apr,
         flush_index_SWE_May_snodas=flush_index_SWE_May,flush_index_SWE_Apr_snodas=flush_index_SWE_Apr) %>%
  mutate(WRT_index_SWE_May_snotel=Volume_m3/((SWE_May_snotel/100)*10^4*Watershed_area_ha),
         WRT_index_SWE_Apr_snotel=Volume_m3/((SWE_Apr_snotel/100)*10^4*Watershed_area_ha),
         flush_index_SWE_May_snotel=1/WRT_index_SWE_May_snotel,
         flush_index_SWE_Apr_snotel=1/WRT_index_SWE_Apr_snotel)

#count the 1 theoleiite lake as "basalt" (similar geochemstry for our purposes)
bigjoin$basalt[which(bigjoin$Lake=="Milk Lake")]<-bigjoin$tholeiite[which(bigjoin$Lake=="Milk Lake")]
bigjoin<-bigjoin %>% select(-tholeiite)
###############################
# add a few columns to bigjoin
bigjoin.stats <- full_join(x = bigjoin, y = short_codes, by = c("Lake"))
bigjoin.stats$park_siteshort<-paste(bigjoin.stats$park_code,"-",bigjoin.stats$short_code,sep="")
bigjoin.stats <- full_join(x = bigjoin.stats, y = labels_tbl, by = c("variable"))
bigjoin.stats <- full_join(x = bigjoin.stats, y = snowsite_tbl, by = c("Snowsite"))
bigjoin.stats$hydro<-"";bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
#bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
#bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
#bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-2
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-3
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-1
bigjoin.stats$hydro<-as.numeric(bigjoin.stats$hydro)
bigjoin.stats$inlet<-0;bigjoin.stats$inlet[which(bigjoin.stats$BlueLineInlet=="T")]<-1
bigjoin.stats$outlet<-0;bigjoin.stats$outlet[which(bigjoin.stats$BlueLineOutlet=="T")]<-1
bigjoin.stats$northface<-0;bigjoin.stats$northface[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-1

######################################
# correct errors
bigjoin.stats$value[which(bigjoin.stats$variable=="ProfTemp_top2m" & bigjoin.stats$value<0)]<-0
bigjoin.stats$value[which(bigjoin.stats$variable=="Total N" & bigjoin.stats$value<0.002)]<-0.002
######################################
# sites table
sitestable0<-bigjoin.stats %>% select(park_code,short_code,Elevation_m,elevmean_wshed,Watershed_area_ha,Surface_area_ha,Volume_m3,Depth_max,Depth_mean_m,Shoreline_length_m,Watershed_area_to_lake_area_ratio,Aspect,solar_dec,solar_jas,snowice,forest,barren,hydro,snowsite_code)
sitestable<-unique(sitestable0)
sitestable<-sitestable[order(sitestable$park_code,sitestable$Elevation_m),]
sitestable<-sitestable %>% mutate_if(is.numeric, signif,digits=2)
sitestable$Aspect[which(sitestable$Aspect=="East")]<-"E";sitestable$Aspect[which(sitestable$Aspect=="West")]<-"W"
sitestable$Aspect[which(sitestable$Aspect=="North")]<-"N";sitestable$Aspect[which(sitestable$Aspect=="South")]<-"S"
sitestable$Aspect[which(sitestable$Aspect=="Southeast")]<-"SE";sitestable$Aspect[which(sitestable$Aspect=="Southwest")]<-"SW"
sitestable$Aspect[which(sitestable$Aspect=="Northeast")]<-"NE";sitestable$Aspect[which(sitestable$Aspect=="Northwest")]<-"NW"
sitestable$Vol_m3xe6<-sitestable$Volume_m3/(10^6)

sitestable<-sitestable %>% dplyr::rename(Park=park_code,Lake=short_code,
                                         SurfArea_ha=Surface_area_ha,
                                         ShedArea_ha=Watershed_area_ha,
                                         ShedAreaLakeArea_ratio=Watershed_area_to_lake_area_ratio,                               
                                         Elev_m=Elevation_m,Elevwshed_m=elevmean_wshed,
                                         Depthmax_m=Depth_max,Depthmean_m=Depth_mean_m,
                                         Shorelength_m=Shoreline_length_m,
                                         SolarJAS_Whrperm2=solar_jas,SolarDec_Whrperm2=solar_dec,
                                         Snowsite=snowsite_code) %>% 
  select(Park,Lake,Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,
         ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,hydro) %>% 
  arrange(Park,Elev_m,Elevwshed_m,SurfArea_ha)

write.csv(sitestable,"../figures/analysispowers/sitestable.csv")

# functions for combining statistics
rangefun<-function(x){
  pasted<-paste(min(x),"-",max(x),sep="")
  return(pasted)
}
meanrangefun<-function(x){
  pasted<-paste(signif(mean(x),digits=2)," (",min(x),"-",max(x),")",sep="")
  return(pasted)
}

sitestablesimple_meansbypark<-sitestable %>% group_by(Park) %>% 
  select(Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,#Shorelength_m,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,
         hydro) %>%
  dplyr::summarise_if(is.numeric, meanrangefun) %>% as.data.frame()

sitestablesimple_means<-sitestable %>% mutate(Park="overall") %>% group_by(Park) %>% 
  select(Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,#Shorelength_m,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,
         hydro) %>%
  dplyr::summarise_if(is.numeric, meanrangefun) %>% as.data.frame()

sitestablesimple<-t(rbind(sitestablesimple_meansbypark,sitestablesimple_means))
write.csv(sitestablesimple,"../figures/analysispowers/sitestable_simplified.csv")

placenames<-bigjoin.stats %>% select(Lake,short_code,Park,park_code,lat,lon,Snowsite) %>% unique() %>%
  mutate(lat=round(lat,digits=4),lon=round(lon,digits=4)) %>%
  arrange(park_code,Lake) %>% as.data.frame()
write.csv(placenames,"../figures/analysispowers/placenames.csv")

# and now a verbose way of combining factors into a concise table
one<-bigjoin.stats %>% select(park_code,Lake,hydro) %>% unique()
two<-one %>% mutate(hydro=factor(hydro)) %>% group_by(park_code,hydro,.drop=FALSE) %>% dplyr::summarize(count=length(Lake))
three<-two %>% mutate(text=paste(hydro,"=",count,sep="")) %>% select(-hydro,-count)
hydrocats_bypark<-three %>% group_by(park_code) %>% summarise_each(funs(paste(., collapse = ", ")))
two<-one %>% mutate(park_code="overall",hydro=factor(hydro)) %>% group_by(park_code,hydro,.drop=FALSE) %>% dplyr::summarize(count=length(Lake))
three<-two %>% mutate(text=paste(hydro,"=",count,sep="")) %>% select(-hydro,-count)
hydrocats<-three %>% summarise_each(funs(paste(., collapse = ", "))) %>% as.data.frame()

one<-bigjoin.stats %>% select(park_code,Lake,salmo_adult_ever) %>% unique()
two<-one %>% group_by(park_code,salmo_adult_ever,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(count1=sum(count*salmo_adult_ever))
salmocats_bypark<-two %>% mutate(text=count1)
two<-one %>% mutate(park_code="overall") %>% group_by(park_code,salmo_adult_ever,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(count1=sum(count*salmo_adult_ever))#,
salmocats<-two %>% mutate(text=count1)
hydrocats_bypark<- hydrocats_bypark %>% rename(hydrotype="text")  %>% as.data.frame()
hydrocats<- hydrocats %>% rename(hydrotype="text")  %>% as.data.frame()
salmocats_bypark<- salmocats_bypark %>% rename(salmoadults="text")  %>% as.data.frame()
salmocats<- salmocats %>% rename(salmoadults="text") %>% as.data.frame()

hydrocats_prepped<-rbind(hydrocats_bypark,hydrocats)
salmocats_prepped<-rbind(salmocats_bypark,salmocats) %>% select(-count1)#,-count2)
categoricals<-t(merge(hydrocats_prepped,salmocats_prepped,by="park_code"))
write.csv(categoricals,"../figures/analysispowers/CountUpHydroTypesAndSalmos.csv")

###################################
# plot SWE interannual variability, and ice out date
iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","HE","HO","LC"))

iceout_SWE_plot<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snodas,x=SWE_May_snodas,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snodas,x=SWE_May_snodas),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snodas,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnodas_plot.png",
       plot = iceout_SWE_plot, width = 8, height = 5, units = "in")

iceout_SWE_plot<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot.png",
       plot = iceout_SWE_plot, width = 8, height = 5, units = "in")

snodas_snotel_compare<-bigjoin.stats %>% select(park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel) %>% as.data.frame()
snodas_snotel_compare<-unique(snodas_snotel_compare)
snodas_snotel_compare$year_label<-as.character(substr(snodas_snotel_compare$year,3,4))

snotel_wtemp_compare<-bigjoin.stats  %>% filter(variable=="ProfTemp_top2m") %>% select(axis_label,park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel,value) %>% as.data.frame()
snotel_wtemp_compare<-unique(snotel_wtemp_compare)
snotel_wtemp_compare$year_label<-as.character(substr(snotel_wtemp_compare$year,3,4))

snodas_wtemp_compare<-bigjoin.stats  %>% filter(variable=="ProfTemp_top2m") %>% select(axis_label,park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel,value) %>% as.data.frame()
snodas_wtemp_compare<-unique(snodas_wtemp_compare)
snodas_wtemp_compare$year_label<-as.character(substr(snodas_wtemp_compare$year,3,4))

snodas_snotel_plot<-ggplot(snodas_snotel_compare,aes(y=SWE_May_snodas,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_text(size=3)+
  ylab("snodas May SWE (cm)")+xlab("snotel May SWE (cm)")+
  facet_wrap(~short_code,ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snodas_snotel_compare.png",
       plot = snodas_snotel_plot, width = 7, height = 5, units = "in")

snodas_wtemp_plot<-ggplot(snodas_wtemp_compare,aes(x=SWE_May_snodas,y=value,color=park_code,label=year_label))+
  geom_text(size=3)+
  xlab("snodas May SWE (cm)")+ylab(snodas_wtemp_compare$axis_label)+
  facet_wrap(~short_code,ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snodas_wtemp.png",
       plot = snodas_wtemp_plot, width = 7, height = 5, units = "in")

snotel_wtemp_plot<-ggplot(snotel_wtemp_compare,aes(x=SWE_May_snotel,y=value,color=park_code,label=year_label))+
  geom_text(size=3)+
  xlab("snotel May SWE (cm)")+ylab(snotel_wtemp_compare$axis_label)+
  facet_wrap(~short_code,ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snotel_wtemp.png",
       plot = snotel_wtemp_plot, width = 7, height = 5, units = "in")

########################################
# reduce data to the main limno variables
dep_names<-names(bigjoin.stats)[grep("_dep_",names(bigjoin.stats))]
data_forstats<- bigjoin.stats %>% 
  dplyr::filter(#year %in% c(2011, 2015),
    variable %in% c("ProfTemp_top2m",
                    "DO_top2m", #"DOsat_top2m",#"DO_below2m",
                    "Chlorophyll", 
                    "secchi_value_m" , "pH_top2m",#"pH_below2m",
                    "SpCond_top2m",
                    "Ca","Cl","K","Mg","SO4","Total N","Total P",
                    "Na")) %>% 
  select(variable,var_abbrev1,var_abbrev2,axis_label,year,short_code,park_code,park_site,park_siteshort,Depth_mean_m,Volume_m3,Surface_area_ha,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,elevmean_wshed,#Asp,
         solar_jas,solar_dec,
         barren,forest,snowice,#shrub,meadow,
         value,
         ice_out_doy,ice_in_doy,SWE_May_snodas,SWE_May_snotel,flush_index_SWE_May_snodas,flush_index_SWE_May_snotel,BlueLineInlet,BlueLineOutlet,hydro,
         salmo_adult_ever,
         northface,inlet,outlet,
         andesite,basalt,biotitegneiss,granodiorite,graywacke,quartzmonzodiorite,quartzmonzonite,sandstone,
         dep_names) %>%         
#         N_dep,SO4_dep,Cl_dep,NO3_dep,NH4_dep) %>%
  unique() %>% as.data.frame()

# Identify the min/max SWE years. Rules...
# 1- Don't use SWE years that lacked temp profile data, aka the lake was not / could not be visited that year
# 2- If two years had the same minimum (zero!), choose the first.
# 3- year>2008 omits 2008, which had incomplete data for some NOCA sites, though may need to use 2010(?)
minyears_snotel<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May_snotel) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(min_SWE_May_snotel=min(SWE_May_snotel,na.rm=TRUE),minYear_SWE_May_snotel=year[which(SWE_May_snotel == min(SWE_May_snotel,na.rm=TRUE))][1]) %>% as.data.frame()
maxyears_snotel<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May_snotel) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(max_SWE_May_snotel=max(SWE_May_snotel,na.rm=TRUE),maxYear_SWE_May_snotel=year[which(SWE_May_snotel == max(SWE_May_snotel,na.rm=TRUE))][1]) %>% as.data.frame()
minyears_snodas<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May_snodas) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(min_SWE_May_snodas=min(SWE_May_snodas,na.rm=TRUE),minYear_SWE_May_snodas=year[which(SWE_May_snodas == min(SWE_May_snodas,na.rm=TRUE))][1]) %>% as.data.frame()
maxyears_snodas<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May_snodas) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(max_SWE_May_snodas=max(SWE_May_snodas,na.rm=TRUE),maxYear_SWE_May_snodas=year[which(SWE_May_snodas == max(SWE_May_snodas,na.rm=TRUE))][1]) %>% as.data.frame()

# Merge the min/max snow year tags with data_forstats
data_forstats<-merge(data_forstats,minyears_snotel,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears_snotel,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,minyears_snodas,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears_snodas,by=c("park_site","short_code"))
# assign snow year dummy variables
data_forstats$snowyrlo_snotel<-0;data_forstats$snowyrhi_snotel<-0;data_forstats$snowyrmid_snotel<-0
data_forstats$snowyrlo_snodas<-0;data_forstats$snowyrhi_snodas<-0;data_forstats$snowyrmid_snodas<-0
data_forstats$snowyrlo_snotel[which(data_forstats$year==data_forstats$minYear_SWE_May_snotel)]<-1
data_forstats$snowyrhi_snotel[which(data_forstats$year==data_forstats$maxYear_SWE_May_snotel)]<-1
data_forstats$snowyrmid_snotel[which(data_forstats$SWE_May_snotel!=data_forstats$min_SWE_May_snotel & 
                                       data_forstats$SWE_May_snotel!=data_forstats$max_SWE_May_snotel)]<-1
data_forstats$snowyrlo_snodas[which(data_forstats$year==data_forstats$minYear_SWE_May_snodas)]<-1
data_forstats$snowyrhi_snodas[which(data_forstats$year==data_forstats$maxYear_SWE_May_snodas)]<-1
data_forstats$snowyrmid_snodas[which(data_forstats$SWE_May_snodas!=data_forstats$min_SWE_May_snodas & 
                                       data_forstats$SWE_May_snodas!=data_forstats$max_SWE_May_snodas)]<-1
data_forstats$BlueLineInlet<-dplyr::recode(data_forstats$BlueLineInlet, F = 0, T=1)
data_forstats$BlueLineOutlet<-dplyr::recode(data_forstats$BlueLineOutlet, F = 0, T=1)
data_forstats$snowyr<-"mid"
data_forstats$snowyr[which(data_forstats$snowyrlo_snotel==1)]<-"lo"
data_forstats$snowyr[which(data_forstats$snowyrhi_snotel==1)]<-"hi"
data_forstats$snowyr<-factor(data_forstats$snowyr,levels=c("mid","lo","hi"))

SWEz_df<-data_forstats %>% select(park_site,year,SWE_May_snotel) %>% unique()
SWEz_df$SWE_May_snotel_z<-SWEz_df$SWE_May_snotel
for(j in 1:length(unique(SWEz_df$park_site))){
  lakej<-unique(SWEz_df$park_site)[j]
  scaledij<-as.numeric(scale(SWEz_df$SWE_May_snotel[which(SWEz_df$park_site==lakej)]))
  SWEz_df$SWE_May_snotel_z[which(SWEz_df$park_site==lakej)]<-scaledij
}
iceoutz_df<-data_forstats %>% select(park_site,year,ice_out_doy) %>% unique()
iceoutz_df$ice_out_doy_z<-iceoutz_df$ice_out_doy
for(j in 1:length(unique(iceoutz_df$park_site))){
  lakej<-unique(iceoutz_df$park_site)[j]
  scaledij<-as.numeric(scale(iceoutz_df$ice_out_doy[which(iceoutz_df$park_site==lakej)]))
  iceoutz_df$ice_out_doy_z[which(iceoutz_df$park_site==lakej)]<-scaledij
}

data_forstats<-merge(data_forstats,SWEz_df,by=c("park_site","year","SWE_May_snotel"))
data_forstats<-merge(data_forstats,iceoutz_df,by=c("park_site","year","ice_out_doy"))


# calculate simple means by snow year type, and write to table
meanval_SWEmidlohi<-data_forstats %>% group_by(variable,snowyr) %>% #snowyrmid_snotel,snowyrlo_snotel,snowyrhi_snotel) %>%
  dplyr::summarize(mean=mean(value, na.rm=TRUE),
                   min=min(value,na.rm=TRUE),
                   q25th=quantile(value,probs=c(.25),na.rm=TRUE),
                   q75th=quantile(value,probs=c(.75),na.rm=TRUE),
                   max=max(value,na.rm=TRUE)) %>% as.data.frame()
meanval_SWEoverall<-data_forstats %>% mutate(snowyr="overall")  %>% group_by(variable,snowyr) %>% #snowyrmid_snotel,snowyrlo_snotel,snowyrhi_snotel) %>%
  dplyr::summarize(mean=mean(value, na.rm=TRUE),
                   min=min(value,na.rm=TRUE),
                   q25th=quantile(value,probs=c(.25),na.rm=TRUE),
                   q75th=quantile(value,probs=c(.75),na.rm=TRUE),
                   max=max(value,na.rm=TRUE)) %>% as.data.frame()
meanval_SWEmidlohi_swe<-data_forstats %>%  mutate(variable="SWE_May") %>% group_by(variable,snowyr) %>% #snowyrmid_snotel,snowyrlo_snotel,snowyrhi_snotel) %>%
  dplyr::summarize(mean=mean(SWE_May_snotel, na.rm=TRUE),
                   min=min(SWE_May_snotel,na.rm=TRUE),
                   q25th=quantile(SWE_May_snotel,probs=c(.25),na.rm=TRUE),
                   q75th=quantile(SWE_May_snotel,probs=c(.75),na.rm=TRUE),
                   max=max(SWE_May_snotel,na.rm=TRUE)) %>% as.data.frame()
meanval_SWEmidlohi_swe_overall<-data_forstats %>% mutate(variable="SWE_May",snowyr="overall")  %>% group_by(variable,snowyr) %>% #snowyrmid_snotel,snowyrlo_snotel,snowyrhi_snotel) %>%
  dplyr::summarize(mean=mean(SWE_May_snotel, na.rm=TRUE),
                   min=min(SWE_May_snotel,na.rm=TRUE),
                   q25th=quantile(SWE_May_snotel,probs=c(.25),na.rm=TRUE),
                   q75th=quantile(SWE_May_snotel,probs=c(.75),na.rm=TRUE),
                   max=max(SWE_May_snotel,na.rm=TRUE)) %>% as.data.frame()
meanval_SWEmidlohi<-rbind(meanval_SWEmidlohi,meanval_SWEoverall,meanval_SWEmidlohi_swe,meanval_SWEmidlohi_swe_overall) %>% 
  arrange(variable,snowyr) %>% mutate_if(is.numeric, signif, 2)

write.csv(meanval_SWEmidlohi,"../figures/analysispowers/SimpleStats.csv")

# do z scores? 
#for (i in 1:length(vars)){
#  vari<-vars[i]
#  scaledi<-scale(data_forstats[,which(names(data_forstats)==vari)])
#  data_forstats[,which(names(data_forstats)==vari)]<-scaledi
#}
data_forstats$value_z<-data_forstats$value
for (i in 1:length(unique(data_forstats$variable))){
  for(j in 1:length(unique(data_forstats$park_site))){
    vari<-unique(data_forstats$variable)[i]
    lakej<-unique(data_forstats$park_site)[j]
    #  vari<-vars[i]
    scaledij<-as.numeric(scale(data_forstats$value[which(data_forstats$variable==vari & data_forstats$park_site==lakej)]))
    #  scaledi<-scale(data_forstats[,which(names(data_forstats)==vari)])
    data_forstats$value_z[which(data_forstats$variable==vari & data_forstats$park_site==lakej)]<-scaledij
  }
}

#lakessnowyears<-data_forstats %>% select(park_site,snowyr,year,SWE_May_snotel) %>% unique()
#lakeshiloyears<-lakessnowyears %>% filter(snowyr %in% c("lo","hi"))
#write.csv(lakessnowyears,file="lakessnowyears.csv")
#write.csv(lakeshiloyears,file="lakeshiloyears.csv")

plotit<-ggplot(data = data_forstats %>% filter(snowyr =="mid")) +
  geom_boxplot(aes(x = variable, y = value_z), alpha = 0.7)+
  geom_jitter(data = filter(data_forstats, snowyr %in% c("lo","hi")),
              aes(x = variable, y = value_z, color = snowyr)) + 
  scale_colour_viridis_d(end = 0.8,direction=-1) +
  theme_bw() +
  ylim(c(-4, 4))
#plotit
png(filename = "../figures/analysispowers/bplot.png",
    width = 12, height = 6, units = "in",res=300)
plotit
dev.off()

#,
#               outlier.shape = NA) +
#  geom_jitter(data = data_forstats,
#              aes(x = variable, y = value_z)) + 

# group the data for lme()
by_variablei <- group_by(data_forstats, variable)
# fit lme models, no covariates
mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
mi_glance<-do(by_variablei, glance(lme(value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% select(variable,sigma,logLik,AIC,BIC,model)
mi<-merge(mi,mi_glance,by=c("variable","model"))
m_0covar<-mi

detailedvar<-"ProfTemp_top2m"
data_forstats_detail<-data_forstats %>% filter(variable==detailedvar)
data_forstats_detail$yearlab<-substr(data_forstats_detail$year,3,4)
lme_detail<- lme(value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data=data_forstats_detail)
#tt<-tidy(lme_detail,effects="fixed")
# wtemp mixed model residuals
residfit<-plot(lme_detail)
residfit_bysnowyr<-plot(lme_detail, resid(., type = "p") ~ fitted(.) | snowyr, abline = 0)
valuefit_bysnowyr<-plot(lme_detail, value ~ fitted(.) | snowyr, abline = c(0,1))

png(filename = "../figures/analysispowers/residfit_wtemp.png",
    width = 6, height = 6, units = "in",res=300)
residfit
dev.off()
png(filename = "../figures/analysispowers/residfit_bysnowyr_wtemp.png",
    width = 10, height = 5, units = "in",res=300)
residfit_bysnowyr
dev.off()
png(filename = "../figures/analysispowers/valuefit_bysnowyr_wtemp.png",
    width = 10, height = 5, units = "in",res=300)
valuefit_bysnowyr
dev.off()
png(filename = "../figures/analysispowers/residvaluesfit_multi_wtemp.png",
    width = 11, height = 8, units = "in",res=300)
grid.arrange(residfit_bysnowyr,valuefit_bysnowyr)
dev.off()

# fit intercept only model to identify spurrious-ness
m_0covar_intonly<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ 1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=0)
m_0covar_intonly_glance<-do(by_variablei, glance(lme(value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly<-merge(m_0covar_intonly,m_0covar_intonly_glance,by=c("variable","model"))

nparam<-length(unique(m_0covar$term))
nparam<-1
m_0covar<-m_0covar[order(m_0covar$variable,m_0covar$model),]
m_0covar$sig<-"";m_0covar$sig[which(m_0covar$p.value<=0.05/nparam)]<-"*"
m_0covar$sig[which(m_0covar$p.value<=0.01/nparam)]<-"**"
m_0covar$sig[which(m_0covar$p.value<=0.005/nparam)]<-"***"

# clean up and split out needed stats parts
m_0covar <- m_0covar %>% mutate_if(is.numeric, signif, digits=2) %>% arrange(variable, term)
m_0covar_mainint<-subset(m_0covar,m_0covar$term!="(Intercept)"|(m_0covar$term=="(Intercept)"))
#m#_0covar_allvar<-m_0covar %>% arrange(variable,term) %>% rename(val=effect)
m_0covar_allvar<-m_0covar_mainint %>% select(-df,-statistic,-effect,-group,variable,term,val=estimate,se=std.error,pval=p.value) %>% as.data.frame()
m_0covar_allvar_int<-subset(m_0covar_allvar,m_0covar_allvar$term=="(Intercept)") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval) %>% dplyr::rename(val.int=val,se.int=se,sig.int=sig)
m_0covar_allvar_slope_snowyrlo<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrlo_snotel") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)
m_0covar_allvar_slope_snowyrhi<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrhi_snotel") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)

# recombine stats parts and reformat the way we want
m_0covar_allvar_slope<-merge(m_0covar_allvar_slope_snowyrlo,m_0covar_allvar_slope_snowyrhi,by=c("variable"),suffixes=c(".lo",".hi"))
m_0covar_allvar_out<- merge(m_0covar_allvar_int,m_0covar_allvar_slope,by=c("variable"),suffixes=c(".int",""))
m_0covar_allvar_out<-m_0covar_allvar_out %>% mutate(b0=paste(val.int," ± ",se.int,sep=""),
                                                    Mlo=paste(val.lo," ± ",se.lo,sig.lo,sep=""), Mhi=paste(val.hi," ± ",se.hi,sig.hi,sep="")) %>% 
  select(variable,b0,Mlo,Mhi)

write.csv(m_0covar_allvar_out,file="../figures/analysispowers/LimnoCoefficients.csv",fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)

################################3

# do plots 
dataplot<-data_forstats %>% filter((snowyrlo_snotel==1 | snowyrhi_snotel==1), variable %in% c("ProfTemp_top2m"))
dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"

plot_wtempSWE_snotel<-ggplot(dataplot, aes(x=SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  xlab("May SWE (cm), SNOTEL")+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85) +
  xlim(x=c(0,325))

plot_wtempSWE_snodas<-ggplot(dataplot, aes(x=SWE_May_snodas, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  xlab("May SWE (cm), SNODAS")+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85) +
  xlim(x=c(0,325))

plot_wtemp_flush<-ggplot(dataplot, aes(x=flush_index_SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  annotation_logticks(sides="b",color="gray",size=0.2)+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  xlab(dataplot$xaxis_label)+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/wtemp_snow.png",width=6,height=2.8,units="in",res=600)
grid.arrange(plot_wtempSWE_snotel,plot_wtempSWE_snodas,ncol=2)
dev.off()

dataplot<-unique(subset(bigjoin.stats,bigjoin.stats$variable=="SWE_May_snotel") %>% select(park_code,Snowsite,year,value))
SWE_timeseries <- ggplot(dataplot,aes(x = year, y = value, color = park_code,shape=factor(Snowsite),label=Snowsite)) +
  geom_point()+#aes(x = year, y = value, color = park_code,group=Snowsite)) +
  scale_shape_manual(values=1:nlevels(factor(dataplot$Snowsite))) +
  facet_wrap(~park_code,ncol=1)+
  ylab("May SWE (cm)")+ xlab("Year") +
  scale_color_discrete(name = "Park")+
  theme_bw() + #theme(legend.position="none")+
  guides(color = FALSE)+
  scale_x_continuous(breaks=c(2011:2018),limits=c(2011,2018))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/swe_timeseries.png",width=6,height=4,units="in",res=300)
SWE_timeseries
dev.off()

dataplot<-data_forstats %>% filter((snowyrlo_snotel==1 | snowyrhi_snotel==1), variable %in% c("DO_top2m","Chlorophyll", 
                                                                                              "secchi_value_m" , "pH_top2m"))
checktiedyears<- dataplot %>% filter(snowyrlo_snotel==1) %>% group_by(variable,short_code) %>% unique() %>%
  dplyr::summarize(minyr=min(year),maxyr=max(year)) %>% as.data.frame()
#tiedyears<-checktiedyears[which(checktiedyears$minyr!=checktiedyears$maxyr),]

# for plot, use only first low year if two low years share the same value
#dataplot<-dataplot[-which(dataplot$variable %in% tiedyears$variable & dataplot$short_code %in% tiedyears$short_code & 
#        dataplot$year %in% tiedyears$maxyr),]

dataplot$xaxis_label<-"May SWE (cm)"
dataplot$value[which(dataplot$short_code=="TR" & dataplot$snowyrlo_snotel==1 & dataplot$variable=="Chlorophyll")]<-2.5 
dataplot$short_code[which(dataplot$short_code=="TR" &  dataplot$variable=="Chlorophyll")]<-"TR*"
dataplot$value[which(dataplot$short_code=="MI" & dataplot$snowyrlo_snotel==1 & dataplot$variable=="secchi_value_m")]<-1.0 
dataplot$short_code[which(dataplot$short_code=="MI" &  dataplot$variable=="secchi_value_m")]<-"MI*"

plot_limno<-ggplot(dataplot, aes(x=SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  scale_y_log10()+
  annotation_logticks(sides="l",color="gray",size=0.3)+
  xlab(dataplot$xaxis_label)+ylab("Value")+
  xlim(c(-30,max(dataplot$SWE_May_snotel)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/limnoSWEhighlow.png",width=4.5,height=4,units="in",res=600)
plot_limno
dev.off()

dataplot<-data_forstats %>% filter((year==minYear_SWE_May_snotel | snowyrhi_snotel==1), variable %in% c("Cl", "Mg", "SO4","Total N"))
checktiedyears<- dataplot %>% filter(snowyrlo_snotel==1) %>% group_by(variable,short_code) %>% unique() %>%
  dplyr::summarize(minyr=min(year),maxyr=max(year)) %>% as.data.frame()
tiedyears<-checktiedyears[which(checktiedyears$minyr!=checktiedyears$maxyr),]

# for plot, use only first low year if two low years share the same value
#dataplot<-dataplot[-which(dataplot$variable %in% tiedyears$variable & dataplot$short_code %in% tiedyears$short_code & 
#                            dataplot$year %in% tiedyears$maxyr),]
dataplot$xaxis_label<-"May SWE (cm)"

dataplot$value[which(dataplot$short_code=="LE" & dataplot$year==2011 & dataplot$variable=="SO4")]<-8.5 
dataplot$short_code[which(dataplot$short_code=="LE" &  dataplot$variable=="SO4")]<-"LE*"

plot_chems<-ggplot(dataplot, aes(x=SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  scale_y_log10() +
  xlab(dataplot$xaxis_label)+ylab("Value")+
  annotation_logticks(sides="l",color="gray",size=0.3)+
  xlim(c(-30,max(dataplot$SWE_May_snotel)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/chemSWEhighlow.png",width=4.8,height=4,units="in",res=600)
plot_chems
dev.off()
############################### 
# do cross correlation plots
crosscorr<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$snowyrhi_snotel==1) %>% 
  select(park_site,Elevation_m,elevmean_wshed,Volume_m3,Depth_mean_m,
         Watershed_area_ha,Surface_area_ha,Watershed_area_to_lake_area_ratio,
         SWE_May_snotel,SWE_May_snodas,
         solar_dec,solar_jas,northface,
         flush_index_SWE_May_snotel,flush_index_SWE_May_snodas,
         forest,snowice,barren,                                                                                              
         BlueLineInlet,BlueLineOutlet,salmo_adult_ever) %>% 
  group_by(park_site) %>%
  summarise_all(list(~mean(.))) %>% 
  select(-park_site) %>% as.data.frame()

crosscorr<-crosscorr %>% dplyr::rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio)
crosscorr.m<-as.matrix(crosscorr)
res1 <- cor.mtest(crosscorr, conf.level = 0.95)

corrs_physio<-cor(crosscorr.m)
png(filename = "../figures/analysispowers/corrplot_landscape.png", width = 6, height = 6, units = "in",res=300)
corrplot_physio<-corrplot(cor(crosscorr.m), method = "ellipse", tl.col = "black", p.mat = res1$p, sig.level = 0.1,insig="blank",order = "AOE") #order = "FPC")
dev.off()

crosscorr_hiy<-subset(data_forstats,data_forstats$snowyrhi_snotel==1) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)
crosscorr_loy<-subset(data_forstats,data_forstats$year==data_forstats$minYear_SWE_May_snotel) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)
crosscorr_allyr<-data_forstats %>% group_by(park_site,variable) %>% dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% spread(variable,value) %>% as.data.frame() %>% select(-park_site)

for(i in 1:ncol(crosscorr_loy)){
  crosscorr_loy[is.na(crosscorr_loy[,i]), i] <- mean(crosscorr_loy[,i], na.rm = TRUE)
}

crosscorr_hiy$secchi_value_m[is.na(crosscorr_hiy$secchi_value_m)]<-mean(crosscorr_hiy$secchi_value_m,na.rm=TRUE)
crosscorr_loy$secchi_value_m[is.na(crosscorr_loy$secchi_value_m)]<-mean(crosscorr_loy$secchi_value_m,na.rm=TRUE)

crosscorr_hiy.m<-as.matrix(crosscorr_hiy);crosscorr_loy.m<-as.matrix(crosscorr_loy)
crosscorr_allyr.m<-as.matrix(crosscorr_allyr)
corrs_hiy<-cor(crosscorr_hiy.m);corrs_loy<-cor(crosscorr_loy.m)
corrs_ally<-cor(crosscorr_allyr.m)

res_hiy <- cor.mtest(crosscorr_hiy, conf.level = 0.95)
res_loy <- cor.mtest(crosscorr_loy, conf.level = 0.95)
res_ally <- cor.mtest(crosscorr_allyr, conf.level = 0.95)

png(filename = "../figures/analysispowers/corrploty_hiSWE.png", width = 6, height = 6, units = "in",res=300)
corrplothiy<-corrplot(cor(crosscorr_hiy.m), method = "ellipse", tl.col = "black", p.mat = res_hiy$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()
png(filename = "../figures/analysispowers/corrploty_loSWE.png", width = 6, height = 6, units = "in",res=300)
corrplotloy<-corrplot(cor(crosscorr_loy.m), method = "ellipse", tl.col = "black", p.mat = res_loy$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()
png(filename = "../figures/analysispowers/corrploty_allSWE.png", width = 6, height = 6, units = "in",res=300)
corrplotally<-corrplot(cor(crosscorr_allyr.m), method = "ellipse", tl.col = "black", p.mat = res_ally$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()


###########################
mod_covar<-function(covars,data,varname,valuecol,fixedpart,randompart){
  for (i in 1:length(covars[1,])){
    daterr<-data
    daterr$value_model<-daterr[,which(names(daterr)==valuecol)]
    
    covarsi<-covars[,i];
    daterr<-daterr %>% select(value_model,covarsi,park_site,park_code,year,snowyr,SWE_May_snotel,SWE_May_snotel_z) %>% na.omit()
    daterr<-daterr %>% unique()
    daterr$nothing<-1

    print(c(paste(i,varname,covars[,i])))
    
    fixedpart_named<-fixedpart
    for (j in 1:length(covarsi)){
      if(j==1){fixedpart_named<-gsub("vari1",covarsi[1],fixedpart_named)}
      if(j==2){fixedpart_named<-gsub("vari2",covarsi[2],fixedpart_named)} 
      if(j==3){fixedpart_named<-gsub("vari3",covarsi[3],fixedpart_named)} 
    }
    
    modelformula<-formula(paste("value_model~",fixedpart_named,sep=""))
    
    if(length(covarsi)==2 & 
       identical(daterr[,which(names(daterr)==covarsi[1])],daterr[,which(names(daterr)==covarsi[2])])==TRUE){
      print("perfectly correlated vars...");print(covarsi);print("on to the next set...")
      next()}
    
#    mi<-lme(na.action=na.omit, modelformula, random = randompart, method="ML",data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
#    mi_glance<-glance(lme(na.action=na.omit, modelformula, random = randompart, method="ML",data = daterr)) %>% as.data.frame() #%>% mutate(model=paste("2_",i,sep="")) %>% select(variable,sigma,logLik,AIC,BIC,model)
#    mi<-summary(mi)$tTable %>% as.data.frame()
    
    mi<-glm(na.action=na.omit, modelformula, data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
    mi<-summary(mi)$coefficients %>% as.data.frame()
    names(mi)<-c("Value",  "Std.Error", "t-value",      "p-value")
    mi_glance<-glance(glm(na.action=na.omit, modelformula, data = daterr)) %>% as.data.frame() #%>% mutate(model=paste("2_",i,sep="")) %>% select(variable,sigma,logLik,AIC,BIC,model)
    
    mi<-data.frame(varname=rep(varname,length(mi[,1])),term=row.names(mi),mi,row.names=NULL)
    mi$sigma<- mi_glance$sigma
    mi$logLik<- mi_glance$logLik
    mi$AIC<- mi_glance$AIC
    mi$BIC<- mi_glance$BIC
    mi$deviance<- mi_glance$deviance
    mi$nfix<-length(mi[,1])
    
    mi<-mi %>% rename(std.error=Std.Error,estimate=Value)#,df=DF)
    

    mi$nparam<-mi$nfix+2
    mi$modname<-paste(varname,"_",fixedpart_named,sep="")
    mi$modname2<-fixedpart
#    mi$modelformula<-list(modelformula)
    mi$modelformula<-paste(as.character(modelformula)[2],as.character(modelformula)[1],as.character(modelformula)[3])
    mi$randompart<-as.character(randompart)
    mi$valuecol<-valuecol
    #mi<-data.frame(varname=varname,mi)
    
   # mi$covar<-""
   # mi$covar[which()]
    
    mi$var0<-""
    mi$var0[which(mi$term =="(Intercept)")]<-0
    mi$var1<-""
    mi$var1[which(mi$term %in% covarsi[1])]<-1
    mi$var2<-""
    mi$var2[which(mi$term %in% covarsi[2])]<-2
    mi$var3<-""
    mi$var3[which(mi$term %in% covarsi[3])]<-3
    
    varint_length<-length(grep(":",mi$term))
    mi$varint1<-""
    mi$varint2<-""
    mi$varint3<-""
    mi$varint1[grep(":",mi$term)[1]]<-4
    mi$varint2[grep(":",mi$term)[2]]<-5
    mi$varint3[grep(":",mi$term)[3]]<-6
#    mi$varint3[grep(":",mi$term)[4]]<-7
    
#    mi$varint1<-""
#    mi$varint1<-grep(covarsi[1],mi$term),grep(":")
    
#    mi$varint1<-""
#    mi$varint1[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[2],mi$term))))==TRUE)]<-"12"
#    mi$varint2<-""
#    mi$varint2[which(duplicated(c(na.omit(grep(covarsi[2],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-"23"
#    mi$varint3<-""
#    mi$varint3[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-"13"
    
#    mi$var13[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-13
    
#    mi$var12<-""
    do<-0
    if(do==1){
    mi$var1[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[2],mi$term))))==TRUE)]<-12
    mi$var1[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-13
    mi$var2[which(duplicated(c(na.omit(grep(covarsi[2],mi$term)),na.omit(grep(covarsi[1],mi$term))))==TRUE)]<-12
    mi$var2[which(duplicated(c(na.omit(grep(covarsi[2],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-23
    mi$var3[which(duplicated(c(na.omit(grep(covarsi[3],mi$term)),na.omit(grep(covarsi[1],mi$term))))==TRUE)]<-13
    mi$var3[which(duplicated(c(na.omit(grep(covarsi[3],mi$term)),na.omit(grep(covarsi[2],mi$term))))==TRUE)]<-23
    }
    
#    mi$var23<-""
#    mi$var23[which(duplicated(c(na.omit(grep(covarsi[2],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-23
#    mi$var13<-""
#    mi$var13[which(duplicated(c(na.omit(grep(covarsi[1],mi$term)),na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-13
#    mi$var123<-""
#    mi$var123[which(triplicated(c(na.omit(grep(covarsi[1],mi$term)),
#                                 na.omit(grep(covarsi[2],mi$term)),
 #                                na.omit(grep(covarsi[3],mi$term))))==TRUE)]<-123
    
    
    
    if(i==1){m_covar<-mi}
    if(i>1){m_covar<-rbind(m_covar,mi)}
  }
  
  m_covar$nobs<-length(is.na(daterr$value_model)==FALSE)
  m_covar<-m_covar %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
  minAICc<-m_covar %>% dplyr::summarize(miAICc=min(AICc,na.rm=TRUE))
  m_covar$minAICc<-rep(minAICc,length(m_covar[,1])) %>% as.numeric()
  
  
  bestmodel<-m_covar %>% filter(AICc==minAICc) %>% select(varname,modname,modelformula,valuecol) %>% unique()
  bestmodel$fixedpart<-substr(bestmodel$modname,nchar(varname)+2,nchar(bestmodel$modname))
  bestmodel_covars<-as.character(strsplit(bestmodel$modelformula,c("\\:|\\+|\\*|\\~"))[[1]])
  bestmodel_covars<-gsub(" ","",bestmodel_covars)
#  bestmodel_covars<-gsub("\\(|\\)| |2|\\^","",bestmodel_covars)
  bestmodel_covars<-gsub("\\(|\\)| |\\^2","",bestmodel_covars)
  bestmodel_covars<-bestmodel_covars[-c(1)]
  bestmodel$ncovars<-length(bestmodel_covars)
  
  daterr<-data
  daterr$value_model<-daterr[,which(names(daterr)==bestmodel$valuecol)]

#  covarsi<-covars[,i];
  daterr<-daterr[,which(names(daterr) %in% c("value_model","park_site","park_code","year","snowyr","SWE_May_snotel","SWE_May_snotel_z",
                                            bestmodel_covars))]
#  daterr<-daterr %>% select(value_model,bestmodel_covars,park_site,year,snowyr,SWE_May_snotel,SWE_May_snotel_z) %>% na.omit()
  daterr<-daterr %>% na.omit() %>% unique()
  daterr$nothing<-1
  
  bestmodelformula<-formula(bestmodel$modelformula)
  
  
  
#  mi<-lme(na.action=na.omit, bestmodelformula, random = randompart, method="ML",data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))

  mi<-glm(na.action=na.omit, bestmodelformula, data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
#  mi<-summary(mi)$coefficients %>% as.data.frame()
#  names(mi)<-c("Value",  "Std.Error", "t-value",      "p-value")

#  mi<-summary(mi)$tTable %>% as.data.frame()
  
  daterr$var1<-daterr[,which(names(daterr) == bestmodel_covars[1])]
  daterr$fitted<-as.numeric(predict(mi))
#  bestmodelfile1<-paste("../figures/analysispowers/best_",varname,"_",bestmodel_covars[1],".png",sep="")
  
  daterr_wallgrp<-daterr
  daterr_wallgrp$park_site<-"all data"
  daterr_parkgrp<-daterr
  daterr_parkgrp$park_site<-paste("all",daterr_parkgrp$park_code)
  daterr_wallgrp<-rbind(daterr,daterr_wallgrp,daterr_parkgrp)
  
  daterr_wallgrp$park_site_<-substr(daterr_wallgrp$park_site,1,15)
  
  do<-0
  
if(do==1) {
  bestmodelfile_preds<-paste("../figures/analysispowers/best_fitval_",varname,"_",bestmodel$ncovars,"var_",
                             bestmodel$fixedpart,".png",sep="")
  
  plotit<-ggplot(daterr_wallgrp, aes(y = fitted, x = value_model, col=park_code))+#,group=nothing)) +
    facet_wrap(~park_site_,ncol=5)+#substr(park_site,1,15),ncol=6)+
#    geom_smooth(method="lm",se=FALSE,alpha = 0,size=0.5,color="black")+
    geom_point(alpha = 0.5,daterr_wallgrp, mapping=aes(y = fitted, x = value_model,shape=snowyr))+#, col=park_code,shape=snowyr)) + #facet_wrap(~park_site) + 
    guides(shape=FALSE)+
    geom_smooth(daterr_wallgrp,method="lm",se=FALSE,size=0.75,
                mapping=aes(y = fitted, x = value_model),col="dark gray")+#,col=park_code))+#,color=park_code)+
#    geom_smooth(daterr_wallgrp %>% filter(park_site!="alldata"),method="lm",se=FALSE,size=0.5,
#                mapping=aes(y = fitted, x = value_model,col=park_code))+#,color=park_code)+
#    geom_smooth(daterr_wallgrp %>% filter(park_site=="alldata"),method="lm",se=FALSE,size=0.5,
#                mapping=aes(y = fitted, x = value_model),color="black")+
    scale_color_viridis_d(end = 0.85)+
    theme_bw()+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab(paste(varname,"value"))+
    ylab(paste(varname,"fitted"))
  png(filename = bestmodelfile_preds, width = 7, height = 7, units = "in",res=300)
  print(plotit)
  dev.off()
  
  
  bestmodelfile1<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"var_",
                        bestmodel_covars[1],"_",bestmodel$fixedpart,".png",sep="")
  
  plotit1<-ggplot(daterr, aes(x = var1, y = value_model, col=park_site))+#,shape=snowyr)) +
  geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var1, y = y.hat)) + 
  geom_point(alpha = 0.5,aes(x = var1, y = value_model, col=park_site,shape=snowyr))+
#    theme(legend.position="none")+
    guides(shape=FALSE)+
    theme_bw()+
    xlab(bestmodel_covars[1])+
    ylab(varname)
  png(filename = bestmodelfile1, width = 9, height = 4, units = "in",res=300)
  print(plotit1)
  dev.off()
  

  if(length(bestmodel_covars)==2){
  #Sys.sleep(3) 
  daterr$var2<-daterr[,which(names(daterr) == bestmodel_covars[2])]
  bestmodelfile2<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"var_",
                        bestmodel_covars[2],"_",bestmodel$fixedpart,".png",sep="")
  
  plotit2<-ggplot(daterr, aes(x = var2, y = value_model, col=park_site))+#,shape=snowyr)) +
    geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var2, y = y.hat)) + 
    geom_point(alpha = 0.5,aes(x = var2, y = value_model, col=park_site,shape=snowyr))+    
#    theme(legend.position="none")+
    guides(shape=FALSE)+
    theme_bw()+
    xlab(bestmodel_covars[2])+
    ylab(varname)
  
  png(filename = bestmodelfile2, width = 9, height = 4, units = "in",res=300)
  print(plotit2)
  dev.off()
  }
  
  if(length(bestmodel_covars)==3){
    #Sys.sleep(3) 
    daterr$var3<-daterr[,which(names(daterr) == bestmodel_covars[3])]
    bestmodelfile3<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"var_",
                          bestmodel_covars[3],"_",bestmodel$fixedpart,".png",sep="")
    
    plotit3<-ggplot(daterr, aes(x = var3, y = value_model, col=park_site))+#,shape=snowyr)) +
      geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var3, y = y.hat)) + 
      geom_point(alpha = 0.5,aes(x = var3, y = value_model, col=park_site,shape=snowyr))+    
      #    theme(legend.position="none")+
      guides(shape=FALSE)+
      theme_bw()+
      xlab(bestmodel_covars[3])+
      ylab(varname)
    
    png(filename = bestmodelfile3, width = 9, height = 4, units = "in",res=300)
    print(plotit3)
    dev.off()
  }
}

  return(m_covar)
}

file_models_zscore<-"../figures/analysispowers/modelouts_zscore"
file_models_nozscore<-"../figures/analysispowers/modelouts"


fileout<-file_models_nozscore
data_forstats$value_model<-data_forstats$value
data_forstats$SWE_May_snotel_model<-data_forstats$SWE_May_snotel
data_forstats$ice_out_doy_model<-data_forstats$ice_out_doy
usenlme<-0
source("models_hypoth.R")


fileout<-file_models_zscore
data_forstats$value_model<-data_forstats$value_z
data_forstats$SWE_May_snotel_model<-data_forstats$SWE_May_snotel_z
data_forstats$ice_out_doy_model<-data_forstats$ice_out_doy_z
usenlme<-0
source("models_hypoth.R")

#######################################


do<-0
if(do==1){
  
  
  varparts<-function(data){
    for (i in 1:(length(unique(data$variable))+1)){
      if(i<(length(unique(data$variable))+1)){
        vari<-unique(data$variable)[i]
        
        daterr<-data %>% filter(variable==vari)
        #    fit <- lm(value ~ park_site+time, daterr)
        lme_test<- lmer(value ~ (1|park_site)+(1|year), data=daterr)}
      if(i==(length(unique(data$variable))+1)){
        vari<-"SWE_May_snotel"
        lme_test<- lmer(SWE_May_snotel ~ (1|park_site)+(1|year), data=daterr)
      }
      var_lake<-as.numeric(VarCorr(lme_test)$park_site)
      var_year<-as.numeric(VarCorr(lme_test)$year)
      var_resid<-attr(VarCorr(lme_test), "sc")^2
      var_lake_fraction<-var_lake/(var_lake+var_year+var_resid)
      var_year_fraction<-var_year/(var_lake+var_year+var_resid)
      var_resid_fraction<-var_resid/(var_lake+var_year+var_resid)
      #    varsi<-c(var_lake_fraction,var_year_fraction)
      if(i==1){vars<-data.frame(variable=vari,lake=var_lake_fraction,year=var_year_fraction,residuals=var_resid_fraction)}
      if(i>1){vars<-rbind(vars,data.frame(variable=vari,lake=var_lake_fraction,year=var_year_fraction,residuals=var_resid_fraction))}
    }
    return(vars)
  }
  varparts.df<-varparts(data_forstats) %>% mutate_if(is.numeric, signif, digits=2) %>% arrange(variable)
  write.csv(varparts.df,"../figures/analysispowers/varianceparts.csv")  
  
  
dataplot<-data_forstats %>% filter(variable=="Total N")
  lm<-lm(value~N_dep_1985_2015,data=dataplot)  
  lm<-lm(value~NH4_dep_1985_2015,data=dataplot)  
  lm<-lm(value~NO3_dep_1985_2015,data=dataplot)  
  lm<-lm(value~N_dep_1990_1999,data=dataplot)  
  lm<-lm(value~NH4_dep_1990_1999,data=dataplot)  
  lm<-lm(value~NO3_dep_1990_1999,data=dataplot)  
  lm<-lm(value~N_dep_2000_2009,data=dataplot)  
  lm<-lm(value~NH4_dep_2000_2009,data=dataplot)  
  lm<-lm(value~NO3_dep_2000_2009,data=dataplot)    
  
dataplot<-data_forstats %>% filter(variable=="ProfTemp_top2m") %>% select(value,value_z,SWE_May_snotel,SWE_May_snotel_z,solar_jas,park_code) %>% na.omit()
fileout<-"../figures/analysispowers/models_wtempswesolar.txt"

lm1<-lm(value_z~SWE_May_snotel_z,dataplot)
dataplot$fitted<-as.numeric(lm1$fitted.values)
dataplot$residuals_z<-as.numeric(lm1$residuals)
lm2<-lm(residuals_z~solar_jas,dataplot)
sink(fileout)
summary(lm1)
summary(lm2)
sink()

png(filename = "../figures/analysispowers/wtempz_SWEz_solar.png", width = 7, height = 4, units = "in",res=300)
par(mfrow=c(1,2))
plot(x=dataplot$SWE_May_snotel_z,y=dataplot$value_z,
     xlab="SWE",ylab="Water Temperature (top2m)")
abline(lm1)
plot(x=dataplot$solar_jas,y=dataplot$residuals,
     xlab="Solar insolation",ylab="Residuals")
dev.off()     


lm1<-lm(value~SWE_May_snotel,dataplot)
dataplot$fitted<-as.numeric(lm1$fitted.values)
dataplot$residuals<-as.numeric(lm1$residuals)
lm2<-lm(residuals~solar_jas,dataplot)
sink(fileout,append=TRUE)
summary(lm1)
summary(lm2)
sink()

png(filename = "../figures/analysispowers/wtemp_SWE_solar.png", width = 7, height = 4, units = "in",res=300)
par(mfrow=c(1,2))
plot(x=dataplot$SWE_May_snotel,y=dataplot$value,
     xlab="SWE",ylab="Water Temperature (top2m)")
abline(lm1)
plot(x=dataplot$solar_jas,y=dataplot$residuals,
     xlab="Solar insolation",ylab="Residuals")
dev.off()                           

}