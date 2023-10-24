library(nlme) # needed for mixed model
library(broom);library(tidyr);library(tidyverse);library(broom.mixed)
library(car);library(reshape2);library(corrplot)
library(ggplot2);library(ggrepel);library(gridExtra)


geol <- read.csv(
  file = file.path("..",
                   "data",
                   "geology",
                   "bedrock_pct_watershed.csv"),
  stringsAsFactors = FALSE) %>% as.data.frame()

geol$ROCKTYPE1<-gsub(" ", "",geol$ROCKTYPE1)

geol <- geol %>% select(Name,ROCKTYPE1,pct_geol) %>% spread(ROCKTYPE1,pct_geol)
geol[is.na(geol)] <- 0

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

axis_label_tbl <- tribble(
  ~variable, ~axis_label,
  "secchi_value_m",     "Secchi depth (m)",
  "ProfTemp_top2m", paste0("Water temperature (", intToUtf8(176), "C), top 2m"),
  "SWE_May", "May SWE (cm)",
  "DO_top2m", "D.O. (mg/L), top 2m",
  "pH_top2m", "pH, top 2m",
  "SpCond_top2m", paste0("Sp. conductivity (", intToUtf8(956), "S/cm), top 2m"),
  "Chlorophyll", paste0("Chlorophyll (", intToUtf8(956), "g/L)"),
  "Na", "Na (mg/L)",
  "Cl", "Cl (mg/L)",
  "Mg", "Mg (mg/L)",
  "Ca", "Ca (mg/L)",
  "SO4", "SO4 (mg/L)",
  "Total N", "TDN (mg/L)", # IMPORTANT! - these were filtered samples 
  "Total P", "TDP (mg/L)", # IMPORTANT! - these were filtered samples   
  "ice_out_doy","Ice out day of year",
  "flush_index_SWE_May","Catchment snow vol : lake vol"
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
###############################
# add a few columns to bigjoin
bigjoin.stats <- full_join(x = bigjoin, y = short_codes, by = c("Lake"))
bigjoin.stats$park_siteshort<-paste(bigjoin.stats$park_code,"-",bigjoin.stats$short_code,sep="")
bigjoin.stats <- full_join(x = bigjoin.stats, y = axis_label_tbl, by = c("variable"))
bigjoin.stats <- full_join(x = bigjoin.stats, y = snowsite_tbl, by = c("Snowsite"))
bigjoin.stats$hydro<-"";bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
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
data_forstats<- bigjoin.stats %>% 
  dplyr::filter(#year %in% c(2011, 2015),
    variable %in% c("ProfTemp_top2m",
        "DO_top2m", #"DOsat_top2m",#"DO_below2m",
        "Chlorophyll", 
        "secchi_value_m" , "pH_top2m",#"pH_below2m",
        "SpCond_top2m",
        "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>% 
  select(variable,axis_label,year,short_code,park_code,park_site,park_siteshort,Depth_mean_m,Volume_m3,Surface_area_ha,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,elevmean_wshed,#Asp,
         solar_jas,solar_dec,
         barren,forest,snowice,#shrub,meadow,
         value,
         ice_out_doy,SWE_May_snodas,SWE_May_snotel,flush_index_SWE_May_snodas,flush_index_SWE_May_snotel,BlueLineInlet,BlueLineOutlet,hydro,
         salmo_adult_ever,
         northface,inlet,outlet) %>%
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

# choose variables for testing interactions
vars<-c("snowyrlo_snotel","snowyrhi_snotel",
        "Depth_mean_m","Volume_m3","Watershed_area_ha","Elevation_m","elevmean_wshed",
        "SWE_May_snotel","SWE_May_snodas",#"SWE_Apr",
        "Watershed_area_to_lake_area_ratio","northface",#"aspectns_lake","aspectns_wshed","northface_wshed",
        "flush_index_SWE_May_snodas",#"flush_index_SWE_Apr",#"park_code",
        "salmo_adult_ever",
        "solar_jas","solar_dec","barren","forest",#"shrub","meadow",
        "inlet","outlet")
vars_1covar<-vars[-which(vars %in% c("snowyrlo_snotel","snowyrmid_snotel","snowyrhi_snotel"))] 
vars_2covar<-combn(vars[-which(vars %in% c("yr","park_code","snowyrlo_snotel","snowyrmid_snotel","snowyrhi_snotel","SWE_May_snodas","SWE_May_snotel","flush_index_SWE_May_snodas"))],2)

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

lakessnowyears<-data_forstats %>% select(park_site,snowyr,year,SWE_May_snotel) %>% unique()
lakeshiloyears<-lakessnowyears %>% filter(snowyr %in% c("lo","hi"))
write.csv(lakessnowyears,file="lakessnowyears.csv")
write.csv(lakeshiloyears,file="lakeshiloyears.csv")

plotit<-ggplot(data = data_forstats %>% filter(snowyr =="mid")) +
  geom_boxplot(aes(x = variable, y = value_z), alpha = 0.7)+
  geom_jitter(data = filter(data_forstats, snowyr %in% c("lo","hi")),
              aes(x = variable, y = value_z, color = snowyr)) + 
  theme_bw() +
  ylim(c(-4, 4))
png(filename = "../figures/analysispowers/bplot.png",
    width = 10, height = 6, units = "in",res=300)
plotit
dev.off()


ggplot(data = data_forstats) +
  geom_boxplot(aes(x = variable, y = value_z), alpha = 0.7)+
  geom_jitter(data = filter(data_forstats, snowyr %in% c("lo","hi")),
              aes(x = variable, y = value_z, color = snowyr)) + 
  theme_bw() +
  ylim(c(-4, 4))


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

lme_test<- lmer(value ~ (1|park_site)+(1|year), data=data_forstats_detail)
var_lake<-as.numeric(VarCorr(lme_test)$park_site)
var_year<-as.numeric(VarCorr(lme_test)$year)
var_resid<-attr(VarCorr(lme_test), "sc")^2
var_lake_fraction<-var_lake/(var_lake+var_year+var_resid)
var_year_fraction<-var_year/(var_lake+var_year+var_resid)

varparts<-function(data){
  for (i in 1:(length(unique(data$variable))+1)){
    if(i<(length(unique(data$variable))+1)){
    vari<-unique(data$variable)[i]
    
    fit <- lm(value ~ park_site+time, daterr)
    
    
    daterr<-data %>% filter(variable==vari)
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
m#_0covar_allvar<-m_0covar %>% arrange(variable,term) %>% rename(val=effect)
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

# function to fit mixed models using 1 predictor
mod_1covar<-function(covars,data){
  for (i in 1:length(covars)){
    vari<-covars[i]
    daterr<-data
    daterr$vari<-daterr[,which(names(daterr)==vari)]
    by_variablei <- group_by(daterr, variable)
    mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit,value ~ snowyr:vari-1, random = ~ 1|park_site/snowyr, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars))
    mi_glance<-do(by_variablei, glance(lme(na.action=na.omit,value ~ snowyr:vari-1, random = ~ 1|park_site/snowyr, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars)) %>% select(variable,sigma,logLik,AIC,BIC,model)
    mi<-merge(mi,mi_glance,by=c("variable","model"))
    mi$snowyr<-substr(mi$term,7,nchar(mi$term)-5)
    mi$term<-vari 
    if(i==1){m_1covar<-mi}
    if(i>1){m_1covar<-rbind(m_1covar,mi)}
  }
  return(m_1covar)
}
# fit lme models, 1 covariate
m_1covar<-mod_1covar(covars=vars_1covar,data=data_forstats)
m_1covar$nobs<-170+21+21
m_1covar$nparam<-3
m_1covar<-m_1covar %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1))) %>% select(snowyr,variable,model,term,val=estimate,std.error,pval=p.value,AICc)

# identify the best model (AICc) for each variable
m_1covar_minAICc<-m_1covar %>% group_by(variable,snowyr) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_1covar<-merge(m_1covar,m_1covar_minAICc,by=c("variable","snowyr"))
m_1covar$AICcdiff<-m_1covar$AICc-m_1covar$minAICc
relate <-m_1covar %>% mutate_if(is.numeric, signif, digits=3) %>% select(-model) %>% arrange(variable,AICcdiff) %>% 
  filter(AICcdiff<=2)
write.csv(relate,"../figures/analysispowers/TopModels_OnePredictor.csv")

# identify significant covariates
#nterms<-1
nterms<-length(unique(relate$term))
relate_sig<-subset(relate,relate$pval<=(0.05/nterms))
relate_sig$dir<-"0";relate_sig$dir[which(relate_sig$val>0)]<-' +'
relate_sig$dir[which(relate_sig$val<=0)]<-' -'
relate_sig$signvar<-paste(relate_sig$dir,relate_sig$variable,sep="")

# split out covariate stats parts for wide format
widestats_snowyrhi<-relate_sig %>% filter(snowyr=="hi") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()
widestats_snowyrlo<-relate_sig %>% filter(snowyr=="lo") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()
widestats_snowyrmid<-relate_sig %>% filter(snowyr=="mid") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()

# recombine coviarte stats parts to wide format
widestats_out<-merge(widestats_snowyrmid,widestats_snowyrlo,by=c("term"),all=TRUE,suffixes=c("","_lo"))
widestats_out<-merge(widestats_out,widestats_snowyrhi,by=c("term"),all=TRUE,suffixes=c("_mid","_hi"))

widestats_out[is.na(widestats_out)] <- ""
widestats_out<-widestats_out
varorder <- tribble(
  ~term, ~order,
  "Elevation_m",     "1",
  "elevmean_wshed",   "2",
  "Depth_mean_m",     "3",
  "SWE_May_snotel",     "4",
  "Watershed_area_to_lake_area_ratio",     "5",
  "flush_index_SWE_May_snodas","6",
  "solar_jas",     "7",
  "northface",     "8",
  "forest",     "9",
  "barren",     "99")
widestats_out <- full_join(x = widestats_out, y = varorder, by = c("term")) %>% arrange(order) %>% select(-order)
write.csv(widestats_out,"../figures/analysispowers/SummaryOfSignifEffectsOnLimno.csv")
#############################
# function to fit mixed models using 2 predictors
mod_2covar<-function(covars,data){
  for (i in 1:length(covars[1,])){
    var1i<-covars[,i][1];var2i<-covars[,i][2]
    daterr<-data
    daterr$vari1<-daterr[,which(names(daterr)==var1i[1])]
    daterr$vari2<-daterr[,which(names(daterr)==var2i[1])]
    by_variablei <- group_by(daterr, variable)
    mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit, value ~ snowyr:vari1+snowyr:vari2-1, random = ~ 1|park_site/snowyr, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
    mi_glance<-do(by_variablei, glance(lme(na.action=na.omit, value ~ snowyr:vari1+snowyr:vari2-1, random = ~ 1|park_site/snowyr, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=paste("2_",i,sep="")) %>% select(variable,sigma,logLik,AIC,BIC,model)
    mi<-merge(mi,mi_glance,by=c("variable","model"))
    mi$snowyr<-substr(mi$term,7,nchar(mi$term)-6)
    mi$term[grep("vari1",mi$term)]<-var1i
    mi$term[grep("vari2",mi$term)]<-var2i
    if(i==1){m_2covar<-mi}
    if(i>1){m_2covar<-rbind(m_2covar,mi)}
  }
  return(m_2covar)
}
# fit lme models, 2 covariates
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_forstats)
m_2covar<-m_2covar0
m_2covar$nparam<-6
m_2covar$nobs<-170+21+21
m_2covar<-m_2covar %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))

# combine stats parts
m_2covar<-m_2covar %>% select(snowyr,variable,model,term,val=estimate,std.error,pval=p.value,AICc)
m_2covar_minAICc<-m_2covar %>% group_by(variable,snowyr) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_2covar<-merge(m_2covar,m_2covar_minAICc,by=c("variable","snowyr"))
m_2covar$AICcdiff<-m_2covar$AICc-m_2covar$minAICc

m_2covar <-m_2covar %>% mutate_if(is.numeric, signif, digits=3) %>% filter(AICcdiff<=2)  %>% 
  select(-minAICc) %>% arrange(variable,AICcdiff)
write.csv(m_2covar,"../figures/analysispowers/TopModels_TwoPredictors.csv",row.names=FALSE)
###########################
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