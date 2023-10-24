library(relaimpo) # load first or else can break dplyr and maybe other things too
library(nlme) # needed for mixed model
library(vctrs)
# Hello!

library(broom);library(tidyr);library(tidyverse);library(broom.mixed)
library(car);library(reshape2);library(corrplot)
library(ggplot2);library(ggrepel);library(gridExtra)
library(lme4);library(viridis);library(tuple)
#library(piecewiseSEM);library(MuMIn);library(mgcv);library(r2glmm)
library(relaimpo)
library(lubridate)
library(r2glmm)
#library(rsq)
library(MuMIn)

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
  "ProfTemp_top2m", paste0("Water temp. (", intToUtf8(176), "C), top 2m"),"Watertemp",paste0("Watertemp(", intToUtf8(176), "C)"),
  "SWE_May", "May SWE (cm)","SWE","SWE(cm)",
  "DO_top2m", "D.O. (mg/L), top 2m","DO","DO(mg/L)",
  "pH_top2m", "pH, top 2m","pH","pH",
  "SpCond_top2m", paste0("Cond. (", intToUtf8(956), "S/cm), top 2m"),"Cond",paste0("Cond(", intToUtf8(956), "S/cm)"),
  "Chlorophyll", paste0("Chlorophyll (", intToUtf8(956), "g/L)"),"Chl",paste0("Chl(", intToUtf8(956), "g/L)"),
  "Na", "Na (mg/L)","Na", "Na(mg/L)",
  "Cl", "Cl (mg/L)","Cl","Cl(mg/L)",
  "Mg", "Mg (mg/L)","Mg","Mg(mg/L)",
  "K", "K (mg/L)","K","K(mg/L)",
  "Ca", "Ca (mg/L)","Ca","Ca(mg/L)",
  "SO4", "SO4 (mg/L)","SO4","SO4(mg/L)",
  "Total N", "TDN (mg/L)", "TDN","TDN(mg/L)",# IMPORTANT! - these were filtered samples 
  "Total P", "TDP (mg/L)", "TDP", "TDP(mg/L)",# IMPORTANT! - these were filtered samples
  "CLAD","Clad. (ind/m3)","clad","clad(ind/m3)",
  "ROT","Rot. (ind/m3)","rot","rot(ind/m3)",
  "COPE","Cope. (ind/m3)","cope","cope(ind/m3)",
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

select<-dplyr::select

# convert units of insolation to per day
bigjoin$solar_jas<-bigjoin$solar_jas/(365*.25)
bigjoin$solar_dec<-bigjoin$solar_dec/31

bigjoin<- bigjoin %>% 
#  rename(SWE_May=SWE_May_snodas, SWE_Apr=SWE_Apr_snodas) %>%
  rename(SWE_May_snodas=SWE_May, SWE_Apr_snodas=SWE_Apr,
#    rename(SWE_May=SWE_May_snodas, SWE_Apr=SWE_Apr_snodas,
         flush_index_SWE_May_snodas=flush_index_SWE_May,flush_index_SWE_Apr_snodas=flush_index_SWE_Apr) %>%
  mutate(WRT_index_SWE_May_snotel=Volume_m3/((SWE_May_snotel/100)*10^4*Watershed_area_ha),
         WRT_index_SWE_Apr_snotel=Volume_m3/((SWE_Apr_snotel/100)*10^4*Watershed_area_ha),
         flush_index_SWE_May_snotel=1/WRT_index_SWE_May_snotel,
         flush_index_SWE_Apr_snotel=1/WRT_index_SWE_Apr_snotel)

#count the 1 theoleiite lake as "basalt" (similar geochemstry for our purposes)
bigjoin$basalt[which(bigjoin$Lake=="Milk Lake")]<-bigjoin$tholeiite[which(bigjoin$Lake=="Milk Lake")]
bigjoin<-bigjoin %>% dplyr::select(-tholeiite)


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

bigjoin.stats$days_since_iceout<-0;
bigjoin.stats$days_since_iceout<-yday(bigjoin.stats$start_date)-bigjoin.stats$ice_out_doy

uniqueparklakes<-bigjoin.stats %>% dplyr::select(park_code,Lake) %>% unique() %>% arrange(park_code,Lake)
uniqueparklakes$parklakenum<-0
uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="OLYM")]<-c(1:length(uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="OLYM")]))
uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="MORA")]<-c(1:length(uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="MORA")]))
uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="NOCA")]<-c(1:length(uniqueparklakes$parklakenum[which(uniqueparklakes$park_code=="NOCA")]))
bigjoin.stats<-merge(bigjoin.stats,uniqueparklakes,by=c("park_code","Lake"),all=TRUE)

bigjoin.stats$days_since_iceout<-yday(bigjoin.stats$start_date)-bigjoin.stats$ice_out_doy
######################################
# correct errors
bigjoin.stats$value[which(bigjoin.stats$variable=="ProfTemp_top2m" & bigjoin.stats$value<0)]<-0
bigjoin.stats$value[which(bigjoin.stats$variable=="Total N" & bigjoin.stats$value<0.002)]<-0.002
bigjoin.stats$value[which(bigjoin.stats$variable=="DO_top2m" & bigjoin.stats$value>50)]<-NA
######################################
# sites table
sitestable0<-bigjoin.stats %>% dplyr::select(park_code,short_code,Elevation_m,elevmean_wshed,Watershed_area_ha,Surface_area_ha,Volume_m3,Depth_max,Depth_mean_m,Shoreline_length_m,Watershed_area_to_lake_area_ratio,Aspect,solar_dec,solar_jas,snowice,forest,barren,hydro,
                                      SWE_May_snotel,ice_out_doy,ice_free_days,
                                      snowsite_code,
                                      bst=basalt,btg=biotitegneiss,grd=granodiorite,grw=graywacke,qzm=quartzmonzonite)
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
                                         SWE=SWE_May_snotel,iceout_doy=ice_out_doy,
                                         Snowsite=snowsite_code) %>% 
 select(Park,Lake,Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,
         ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,hydro,SWE,iceout_doy,ice_free_days,
         bst,btg,grd,grw,qzm) %>% 
  arrange(Park,Elev_m,Elevwshed_m,SurfArea_ha)

write.csv(sitestable,"../figures/analysispowers/sitestable.csv")

# functions for combining statistics
rangefun<-function(x){
  pasted<-paste(min(x,na.rm=TRUE),"-",max(x,na.rm=TRUE),sep="")
  return(pasted)
}
meanrangefun<-function(x){
  pasted<-paste(signif(mean(x,na.rm=TRUE),digits=2)," (",min(x,na.rm=TRUE),"-",max(x,na.rm=TRUE),")",sep="")
  return(pasted)
}

sitestablesimple_meansbypark<-sitestable %>% group_by(Park) %>% 
 select(Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,#Shorelength_m,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,
         hydro,
         SWE,iceout_doy,ice_free_days,
         bst,btg,grd,grw,qzm) %>%
  dplyr::summarise_if(is.numeric, meanrangefun) %>% as.data.frame()

sitestablesimple_means<-sitestable %>% mutate(Park="overall") %>% group_by(Park) %>% 
 select(Elev_m,Elevwshed_m,
         SurfArea_ha,ShedArea_ha,ShedAreaLakeArea_ratio,
         Depthmax_m,Depthmean_m,Vol_m3xe6,#Shorelength_m,
         Aspect,SolarJAS_Whrperm2,SolarDec_Whrperm2,
         snowice,forest,barren,
         hydro,
         SWE,iceout_doy,ice_free_days,
         bst,btg,grd,grw,qzm) %>%
  dplyr::summarise_if(is.numeric, meanrangefun) %>% as.data.frame()

sitestablesimple<-t(rbind(sitestablesimple_meansbypark,sitestablesimple_means))
sitestablesimple_main<-sitestablesimple[-which(row.names(sitestablesimple) %in% c("bst","btg","grd","grd","grw","qzm")),]
write.csv(sitestablesimple_main,"../figures/analysispowers/sitestable_simplified.csv")

sitestablesimple_geol<-sitestablesimple[c(1,which(row.names(sitestablesimple) %in% c("bst","btg","grd","grd","grw","qzm"))),]
write.csv(sitestablesimple_geol,"../figures/analysispowers/sitestable_geol_simplified.csv")


placenames<-bigjoin.stats %>% dplyr::select(Lake,short_code,Park,park_code,lat,lon,Snowsite) %>% unique() %>%
  mutate(lat=round(lat,digits=4),lon=round(lon,digits=4)) %>%
  arrange(park_code,Lake) %>% as.data.frame()
write.csv(placenames,"../figures/analysispowers/placenames.csv")

# and now a verbose way of combining factors into a concise table
one<-bigjoin.stats %>% dplyr::select(park_code,Lake,hydro) %>% mutate(hydro1=as.numeric(hydro==1),hydro2=as.numeric(hydro==2),hydro3=as.numeric(hydro==3),) %>% unique()
hydro1_bypark<-one %>% group_by(park_code,hydro1,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd1=sum(count*hydro1))
hydro1_overall<-one %>% mutate(park_code="overall") %>% group_by(park_code,hydro1,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd1=sum(count*hydro1))#,
hydro2_bypark<-one %>% group_by(park_code,hydro2,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd2=sum(count*hydro2))
hydro2_overall<-one %>% mutate(park_code="overall") %>% group_by(park_code,hydro2,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd2=sum(count*hydro2))#,
hydro3_bypark<-one %>% group_by(park_code,hydro3,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd3=sum(count*hydro3))
hydro3_overall<-one %>% mutate(park_code="overall") %>% group_by(park_code,hydro3,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(hyd3=sum(count*hydro3))#,

hydro123<-(cbind(rbind(hydro1_bypark,hydro1_overall),
rbind(hydro2_bypark,hydro2_overall),
rbind(hydro3_bypark,hydro3_overall)))
hydrocats_prepped<-hydro123<-hydro123[,-c(3,5)]


one<-bigjoin.stats %>% dplyr::select(park_code,Lake,salmo_adult_ever) %>% unique()
two<-one %>% group_by(park_code,salmo_adult_ever,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(count1=sum(count*salmo_adult_ever))
salmocats_bypark<-two %>% mutate(text=count1)
two<-one %>% mutate(park_code="overall") %>% group_by(park_code,salmo_adult_ever,.drop=FALSE) %>% dplyr::summarize(count=length(Lake)) %>% 
  dplyr::summarize(count1=sum(count*salmo_adult_ever))#,
salmocats<-two %>% mutate(text=count1)
salmocats_bypark<- salmocats_bypark %>% rename(salmoadults="text")  %>% as.data.frame()
salmocats<- salmocats %>% rename(salmoadults="text") %>% as.data.frame()

salmocats_prepped<-rbind(salmocats_bypark,salmocats) %>% dplyr::select(-count1)#,-count2)

one0<-bigjoin.stats %>% mutate(andesite=as.numeric(andesite>50),basalt=as.numeric(basalt>50),biotitegneiss=as.numeric(biotitegneiss>50),
                              granodiorite=as.numeric(granodiorite>50),graywacke=as.numeric(graywacke>50),
                              quartzmonzonite=as.numeric(quartzmonzonite>50), quartzmonzodiorite=as.numeric(quartzmonzodiorite>50),
                              sandstone=as.numeric(sandstone>50)
                              ) %>%  
 select(park_code,Lake,andesite,basalt,biotitegneiss,granodiorite,graywacke,quartzmonzonite,quartzmonzodiorite,sandstone) %>%
  unique()
one<-one0 %>% group_by(park_code) %>% dplyr::select(Lake,park_code,andesite,basalt,biotitegneiss,granodiorite,graywacke,quartzmonzonite,quartzmonzodiorite,sandstone) %>% 
  dplyr::summarize(andesite=sum(andesite),
                   basalt=sum(basalt),
                   biotitegneiss=sum(biotitegneiss),
                   granodiorite=sum(granodiorite),
                   graywacke=sum(graywacke),
                   quartzmonzonite=sum(quartzmonzonite),
                   quartzmonzodiorite=sum(quartzmonzodiorite),
                   sandstone=sum(sandstone)) %>% as.data.frame()
two<-one0 %>% mutate(park_code=as.character("overall")) %>% dplyr::select(park_code,andesite,basalt,biotitegneiss,granodiorite,graywacke,quartzmonzonite,quartzmonzodiorite,sandstone) %>% 
  group_by(park_code) %>%
  dplyr::summarize(andesite=sum(andesite),
                   basalt=sum(basalt),
                   biotitegneiss=sum(biotitegneiss),
                   granodiorite=sum(granodiorite),
                   graywacke=sum(graywacke),
                   quartzmonzonite=sum(quartzmonzonite),
                   quartzmonzodiorite=sum(quartzmonzodiorite),
                   sandstone=sum(sandstone)) %>% as.data.frame()
geol_prepped<-(rbind(one,two))

categoricals0<-merge(hydrocats_prepped,salmocats_prepped,by="park_code")
categoricals<-t(merge(categoricals0,geol_prepped,by="park_code"))

write.csv(categoricals,"../figures/analysispowers/CountUpHydroTypesSalmosGeol.csv")


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
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnodas_plot.png",
       plot = iceout_SWE_plot, width = 8, height = 5, units = "in")

iceout_SWE_plot<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot.png",
       plot = iceout_SWE_plot, width = 8, height = 5, units = "in")

dataplot<-bigjoin.stats %>% dplyr::select(park_code,short_code,park_site,year, SWE_May_snotel,days_since_iceout,ice_free_days,ice_out_doy) %>% unique() %>% as.data.frame() %>% na.omit()
iceout_SWE_scatterplot<-ggplot(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel,color=park_code,label=short_code))+
  geom_point(size=3,alpha=0.5)+
  geom_smooth(method="lm",aes(y=ice_free_days,x=SWE_May_snotel,color="black"))+
  ylab("Ice free days")+xlab("May SWE (cm)")+
#  geom_text(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel),color="black",size=3)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)
lm<-lm(ice_free_days~SWE_May_snotel,data=dataplot)
summary(lm)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_scatterplot.png",
       plot = iceout_SWE_scatterplot, width = 5, height = 4, units = "in")

iceoutdoy_SWE_scatterplot<-ggplot(data=dataplot,aes(y=ice_out_doy,x=SWE_May_snotel,color=park_code,label=short_code))+
  geom_point(size=3,alpha=0.5)+
  geom_smooth(method="lm",aes(y=ice_out_doy,x=SWE_May_snotel,color="black"))+
  ylab("Ice out day of year")+xlab("May SWE (cm)")+
  #  geom_text(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel),color="black",size=3)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)
lm<-lm(ice_out_doy~SWE_May_snotel,data=dataplot)
summary(lm)

dataplot<-bigjoin.stats %>% dplyr::select(park_code,short_code,park_site,year, SWE_May_snotel,Elevation_m,elevmean_wshed,days_since_iceout,ice_free_days,ice_out_doy) %>% unique() %>% as.data.frame() %>% na.omit()
SWE_elev_scatterplot<-ggplot(data=dataplot,aes(x=Elevation_m,y=SWE_May_snotel,color=park_code,label=short_code))+
  geom_point(size=3,alpha=0.5)+
  #geom_smooth(method="lm",aes(x=Elevation_m,y=SWE_May_snotel,color="black"))+
  xlab("Lake elevation (m)")+ylab("May SWE (cm)")+
  #  geom_text(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel),color="black",size=3)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)
lm<-lm(SWE_May_snotel~Elevation_m,data=dataplot)
summary(lm)

SWE_elevwshed_scatterplot<-ggplot(data=dataplot,aes(x=elevmean_wshed,y=SWE_May_snotel,color=park_code,label=short_code))+
  geom_point(size=3,alpha=0.5)+
  #geom_smooth(method="lm",aes(x=elevmean_wshed,y=SWE_May_snotel,color="black"))+
  xlab("Mean wshed elevation (m)")+ylab("May SWE (cm)")+
  #  geom_text(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel),color="black",size=3)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)
lm<-lm(SWE_May_snotel~elevmean_wshed,data=dataplot)
summary(lm)


SWE_days_since_iceout_scatterplot<-ggplot(data=dataplot,aes(y=days_since_iceout,x=SWE_May_snotel,color=park_code,label=short_code))+
  geom_point(size=3,alpha=0.5)+
  geom_smooth(method="lm",aes(y=days_since_iceout,x=SWE_May_snotel,color="black"))+
  ylab("Days since iceout (m)")+xlab("May SWE (cm)")+
  #  geom_text(data=dataplot,aes(y=ice_free_days,x=SWE_May_snotel),color="black",size=3)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

png(filename = "../figures/analysispowers/iceandsnow_multi.png",
  width = 7, height = 8, units = "in",res=300)
grid.arrange(iceout_SWE_scatterplot,iceoutdoy_SWE_scatterplot,
  SWE_days_since_iceout_scatterplot,SWE_elev_scatterplot,
  SWE_elevwshed_scatterplot)
dev.off()


snodas_snotel_compare<-bigjoin.stats %>% dplyr::select(park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel) %>% as.data.frame()
snodas_snotel_compare<-unique(snodas_snotel_compare)
snodas_snotel_compare$year_label<-as.character(substr(snodas_snotel_compare$year,3,4))

snotel_wtemp_compare<-bigjoin.stats  %>% filter(variable=="ProfTemp_top2m") %>% dplyr::select(axis_label,park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel,value) %>% as.data.frame()
snotel_wtemp_compare<-unique(snotel_wtemp_compare)
snotel_wtemp_compare$year_label<-as.character(substr(snotel_wtemp_compare$year,3,4))

snodas_wtemp_compare<-bigjoin.stats  %>% filter(variable=="ProfTemp_top2m") %>% dplyr::select(axis_label,park_code,park_site,short_code,year,SWE_May_snodas,SWE_May_snotel,value) %>% as.data.frame()
snodas_wtemp_compare<-unique(snodas_wtemp_compare)
snodas_wtemp_compare$year_label<-as.character(substr(snodas_wtemp_compare$year,3,4))

snodas_snotel_plot<-ggplot(snodas_snotel_compare,aes(y=SWE_May_snodas,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_text(size=2.5)+
  ylab("SNODAS May SWE (cm)")+xlab("SNOTEL May SWE (cm)")+
  facet_wrap(~substr(park_site,1,15),ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snodas_snotel_compare.png",
       plot = snodas_snotel_plot, width = 8.25, height = 5.75, units = "in")

snodas_wtemp_plot<-ggplot(snodas_wtemp_compare,aes(x=SWE_May_snodas,y=value,color=park_code,label=year_label))+
  geom_text(size=2.5)+
  xlab("SNODAS May SWE (cm)")+ylab(snodas_wtemp_compare$axis_label)+
  facet_wrap(~substr(park_site,1,15),ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snodas_wtemp.png",
       plot = snodas_wtemp_plot, width = 8.25, height = 5.75, units = "in")

snotel_wtemp_plot<-ggplot(snotel_wtemp_compare,aes(x=SWE_May_snotel,y=value,color=park_code,label=year_label))+
  geom_text(size=2.5)+
  xlab("SNOTEL May SWE (cm)")+ylab(snotel_wtemp_compare$axis_label)+
  facet_wrap(~substr(park_site,1,15),ncol=6)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/snotel_wtemp.png",
       plot = snotel_wtemp_plot, width = 8.25, height = 5.75, units = "in")

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
                    "Na",
                    "CLAD","ROT","COPE")) %>% 
 select(variable,var_abbrev1,var_abbrev2,axis_label,year,short_code,park_code,park_site,park_siteshort,Depth_mean_m,Volume_m3,Surface_area_ha,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,elevmean_wshed,
         parklakenum,#Asp,
         solar_jas,solar_dec,
         barren,forest,snowice,#shrub,meadow,
         value,
         ice_out_doy,ice_in_doy,ice_free_days,days_since_iceout,
        SWE_May_snodas,SWE_May_snotel,flush_index_SWE_May_snodas,flush_index_SWE_May_snotel,BlueLineInlet,BlueLineOutlet,hydro,
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
minyears_ice<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
 select(year,short_code,park_code,park_site,park_siteshort,value,ice_out_doy) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(min_iceoutdoy=min(ice_out_doy,na.rm=TRUE),minYear_iceoutdoy=year[which(ice_out_doy == min(ice_out_doy,na.rm=TRUE))][1]) %>% as.data.frame()
maxyears_ice<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2009) %>% 
 select(year,short_code,park_code,park_site,park_siteshort,value,ice_out_doy) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(max_iceoutdoy=max(ice_out_doy,na.rm=TRUE),maxYear_iceoutdoy=year[which(ice_out_doy == max(ice_out_doy,na.rm=TRUE))][1]) %>% as.data.frame()

# Merge the min/max snow year tags with data_forstats
data_forstats<-merge(data_forstats,minyears_snotel,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears_snotel,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,minyears_snodas,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears_snodas,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,minyears_ice,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears_ice,by=c("park_site","short_code"))
# assign snow year dummies
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
data_forstats$snowyr<-"mid"
data_forstats$snowyr[which(data_forstats$snowyrlo_snotel==1)]<-"lo"
data_forstats$snowyr[which(data_forstats$snowyrhi_snotel==1)]<-"hi"
data_forstats$snowyr<-factor(data_forstats$snowyr,levels=c("mid","lo","hi"))

# assign iceout_doy dummies
data_forstats$iceyrlo<-0;data_forstats$iceyrmid<-0;data_forstats$iceyrhi<-0
data_forstats$iceyrlo[which(data_forstats$year==data_forstats$minYear_iceoutdoy)]<-1
data_forstats$iceyrhi[which(data_forstats$year==data_forstats$maxYear_iceoutdoy)]<-1

data_forstats$BlueLineInlet<-dplyr::recode(data_forstats$BlueLineInlet, F = 0, T=1)
data_forstats$BlueLineOutlet<-dplyr::recode(data_forstats$BlueLineOutlet, F = 0, T=1)

SWEz_df<-data_forstats %>% dplyr::select(park_site,year,SWE_May_snotel) %>% unique()
SWEz_df$SWE_May_snotel_z<-SWEz_df$SWE_May_snotel
for(j in 1:length(unique(SWEz_df$park_site))){
  lakej<-unique(SWEz_df$park_site)[j]
  scaledij<-as.numeric(scale(SWEz_df$SWE_May_snotel[which(SWEz_df$park_site==lakej)]))
  SWEz_df$SWE_May_snotel_z[which(SWEz_df$park_site==lakej)]<-scaledij
}
iceoutz_df<-data_forstats %>% dplyr::select(park_site,year,ice_out_doy) %>% unique()
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


data_forstats %>% dplyr::select(park_code,park_site,year,ice_out_doy,SWE_May_snotel) %>% unique() %>%
  dplyr::summarize(mean_ice=mean(ice_out_doy,na.rm=TRUE),
                   count_ice=length(ice_out_doy[which(is.na(ice_out_doy)==FALSE)]),
                   mean_swe=mean(SWE_May_snotel,na.rm=TRUE),
                   count_swe=length(SWE_May_snotel[which(is.na(SWE_May_snotel)==FALSE)]))
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

bp_varorder<-c("Watertemp","DO","pH","Secchi","Chl","TDN","TDP","SO4","Cond","clad","rot","cope","Ca","Cl","K","Mg","Na")

# factorize and re-order axis labels into desired sort order
data_forstats$axis_label<-factor(data_forstats$axis_label, levels=unique(data_forstats$axis_label)[match(bp_varorder,data_forstats$var_abbrev1 %>% unique())])
data_forstats$axis_label_nounits<-gsub("\\s*\\([^\\)]+\\)","",as.character(data_forstats$axis_label))

plotit<-ggplot(data = data_forstats %>% filter(snowyr =="mid")) +
#  coord_flip()+
  geom_boxplot(aes(x = axis_label_nounits, y = value_z), alpha = 0.3)+
#  geom_boxplot(aes(x = variable, y = value_z), alpha = 0.7)+
  geom_jitter(data = filter(data_forstats, snowyr %in% c("lo","hi")),
              aes(x = axis_label_nounits, y = value_z, color = snowyr)) + 
#              aes(x = axis_label, y = value_z, color = snowyr)) + 
  scale_x_discrete(limits = rev(levels(factor(data_forstats$axis_label_nounits))))+
  xlab("")+
  ylab("z scored value")+
  coord_flip()+
  scale_colour_viridis_d(end = 0.8,direction=-1,name="Snow year",labels=c("low","high")) +
  theme_bw() +
  ylim(c(-4, 4))

png(filename = "../figures/analysispowers/bplot.png",
#    width = 12, height = 6, units = "in",res=300)
    width = 8, height = 6, units = "in",res=300)
plotit
dev.off()

# group the data for lme()
by_variablei <- group_by(data_forstats, variable)
by_variablei$variable<-as.factor(by_variablei$variable)
by_variablei$park_site<-as.factor(by_variablei$park_site)

prep1<-data_forstats[which(data_forstats$variable=="ProfTemp_top2m"),]
prep1$value<-prep1$ice_out_doy
prep1$variable<-"ice_out_doy"
by_variableice <- group_by(prep1, variable)
by_variableice$variable<-as.factor(by_variableice$variable)
by_variableice$park_site<-as.factor(by_variableice$park_site)

by_variablei<-rbind(by_variablei,by_variableice)

# fit lme models, no covariates
mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)

# get the model stats (AIC,loglik) from lme models of each variable 
#mi_glance<-do(by_variablei, glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
# Started producing this error on Sep 8, 2020 (despite running previously)
#Error: Can't combine `..1` <logLik> and `..2` <logLik>.
#x Some attributes are incompatible.
#ℹ The author of the class should implement vctrs methods.
#ℹ See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# Yet, these stil work on Sep 8, 2020. No idea after SP spent much time.  
#mi_glance_iris<-do(group_by(iris,Species), glance(lme(na.action=na.omit,Petal.Width ~ Petal.Length+Sepal.Length, random = ~ 1|Sepal.Width, method="ML",data = .))) %>% as.data.frame() #%>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
#mi_glance_iris<-do(group_by(iris,Species), glance(lme(na.action=na.omit,Petal.Width ~ Petal.Length+Sepal.Length, random = ~ 1|as.factor(Sepal.Width), method="ML",data = .))) %>% as.data.frame() #%>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)

# Fine. One by one then :(
mi_glance1<-do(by_variablei %>% filter(variable=="ProfTemp_top2m"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance2<-do(by_variablei %>% filter(variable=="SO4"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance3<-do(by_variablei %>% filter(variable=="secchi_value_m"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance4<-do(by_variablei %>% filter(variable=="Na"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance5<-do(by_variablei %>% filter(variable=="Total P"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance6<-do(by_variablei %>% filter(variable=="Total N"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance7<-do(by_variablei %>% filter(variable=="Cl"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance8<-do(by_variablei %>% filter(variable=="K"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance9<-do(by_variablei %>% filter(variable=="Chlorophyll"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance10<-do(by_variablei %>% filter(variable=="SpCond_top2m"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance11<-do(by_variablei %>% filter(variable=="pH_top2m"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance12<-do(by_variablei %>% filter(variable=="Ca"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance13<-do(by_variablei %>% filter(variable=="DO_top2m"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance14<-do(by_variablei %>% filter(variable=="Mg"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance15<-do(by_variablei %>% filter(variable=="CLAD"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance16<-do(by_variablei %>% filter(variable=="ROT"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
mi_glance17<-do(by_variablei %>% filter(variable=="COPE"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)

mi_glance18<-do(by_variablei %>% filter(variable=="ice_out_doy"), glance(lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)


mi_glance<-rbind(mi_glance1,mi_glance2,mi_glance3,mi_glance4,mi_glance5,mi_glance6,mi_glance7,mi_glance8,mi_glance9,mi_glance10,
      mi_glance11,mi_glance12,mi_glance13,mi_glance14,
      mi_glance15,mi_glance16,mi_glance17,mi_glance18)

mi<-merge(mi,mi_glance,by=c("variable","model"))
m_0covar<-mi

detailedvar<-"ProfTemp_top2m"
data_forstats_detail<-data_forstats %>% filter(variable==detailedvar)
data_forstats_detail$yearlab<-substr(data_forstats_detail$year,3,4)
lme_detail<- lme(na.action=na.omit,value ~ snowyrlo_snotel+snowyrhi_snotel, random = ~ 1|park_site, method="ML",data=data_forstats_detail)
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
m_0covar_intonly<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=0)

# dangit vctrs again!
# m_0covar_intonly_glance<-do(by_variablei, glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance1<-do(by_variablei %>% filter(variable=="ProfTemp_top2m"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance2<-do(by_variablei %>% filter(variable=="SO4"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance3<-do(by_variablei %>% filter(variable=="secchi_value_m"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance4<-do(by_variablei %>% filter(variable=="Na"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance5<-do(by_variablei %>% filter(variable=="Total P"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance6<-do(by_variablei %>% filter(variable=="Total N"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance7<-do(by_variablei %>% filter(variable=="Cl"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance8<-do(by_variablei %>% filter(variable=="K"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance9<-do(by_variablei %>% filter(variable=="Chlorophyll"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance10<-do(by_variablei %>% filter(variable=="SpCond_top2m"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance11<-do(by_variablei %>% filter(variable=="pH_top2m"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance12<-do(by_variablei %>% filter(variable=="Ca"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance13<-do(by_variablei %>% filter(variable=="DO_top2m"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance14<-do(by_variablei %>% filter(variable=="Mg"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance15<-do(by_variablei %>% filter(variable=="CLAD"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance16<-do(by_variablei %>% filter(variable=="ROT"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance17<-do(by_variablei %>% filter(variable=="COPE"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly_glance18<-do(by_variablei %>% filter(variable=="ice_out_doy"), glance(lme(na.action=na.omit,value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)


m_0covar_intonly_glance<-rbind(m_0covar_intonly_glance1,m_0covar_intonly_glance2,m_0covar_intonly_glance3,m_0covar_intonly_glance4,m_0covar_intonly_glance5,
                               m_0covar_intonly_glance6,m_0covar_intonly_glance7,m_0covar_intonly_glance8,m_0covar_intonly_glance9,m_0covar_intonly_glance10,
                               m_0covar_intonly_glance11,m_0covar_intonly_glance12,m_0covar_intonly_glance13,m_0covar_intonly_glance14,
                               m_0covar_intonly_glance15,m_0covar_intonly_glance16,m_0covar_intonly_glance17,
                              m_0covar_intonly_glance18)

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
m_0covar_allvar<-m_0covar_mainint %>% dplyr::select(-df,-statistic,-effect,-group,variable,term,val=estimate,se=std.error,pval=p.value) %>% as.data.frame()
m_0covar_allvar_int<-subset(m_0covar_allvar,m_0covar_allvar$term=="(Intercept)") %>% dplyr::select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval) %>% dplyr::rename(val.int=val,se.int=se,sig.int=sig)
m_0covar_allvar_slope_snowyrlo<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrlo_snotel") %>% dplyr::select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)
m_0covar_allvar_slope_snowyrhi<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrhi_snotel") %>% dplyr::select(-term,-model,-sigma,-logLik,-AIC,-BIC,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)

# recombine stats parts and reformat the way we want
m_0covar_allvar_slope<-merge(m_0covar_allvar_slope_snowyrlo,m_0covar_allvar_slope_snowyrhi,by=c("variable"),suffixes=c(".lo",".hi"))
m_0covar_allvar_out0<- merge(m_0covar_allvar_int,m_0covar_allvar_slope,by=c("variable"),suffixes=c(".int",""))
m_0covar_allvar_out<-m_0covar_allvar_out0
m_0covar_allvar_out<-m_0covar_allvar_out %>% mutate(b0=paste(val.int," ± ",se.int,sep=""),
                                                    Mlo=paste(val.lo," ± ",se.lo,sig.lo,sep=""), Mhi=paste(val.hi," ± ",se.hi,sig.hi,sep="")) %>% 
 select(variable,b0,Mlo,Mhi)

desired_order <- data.frame(variable=c("ProfTemp_top2m","DO_top2m","pH_top2m","secchi_value_m","Chlorophyll","Total N","Total P","SO4","SpCond_top2m","Ca","Cl",
                                  "K","Mg","Na","CLAD","ROT","COPE","ice_out_doy"))
desired_order$order<-1:length(desired_order[,1])
m_0covar_allvar_out<-merge(desired_order,m_0covar_allvar_out,by="variable") 
m_0covar_allvar_out<-m_0covar_allvar_out %>% arrange(order) %>% dplyr::select(-order)

write.csv(m_0covar_allvar_out,file="../figures/analysispowers/LimnoCoefficients.csv",fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)
################################3
# hilomid snow dotplot 
desired_order <- data.frame(variable=c("ProfTemp_top2m","DO_top2m","pH_top2m","secchi_value_m","Chlorophyll","Total N","Total P","SO4","SpCond_top2m","Ca","Cl",
                                       "K","Mg","Na","CLAD","ROT","COPE"))
desired_order$order<-1:length(desired_order[,1])
dataplot0<-merge(desired_order,m_0covar,by="variable") 
dataplot0<-dataplot0 %>% arrange(order) %>% dplyr::select(-order)

dataplot0$term[which(dataplot0$term=="snowyrhi_snotel")]<-"hi"
dataplot0$term[which(dataplot0$term=="(Intercept)")]<-"mi"
dataplot0$term[which(dataplot0$term=="snowyrlo_snotel")]<-"lo"

dataplot0<-dataplot0 %>% arrange(variable,desc(term))

dataplot0$estimate[which(dataplot0$term=="hi")]<-
  dataplot0$estimate[which(dataplot0$term=="hi")]+
  dataplot0$estimate[-2+which(dataplot0$term=="hi")]
dataplot0$estimate[which(dataplot0$term=="lo")]<-
  dataplot0$estimate[which(dataplot0$term=="lo")]+
  dataplot0$estimate[-1+which(dataplot0$term=="lo")]

dataplot<-merge(dataplot0,labels_tbl,by="variable")
#dataplot$term<-factor(dataplot$term,levels=c("lo","mi","hi"))
dataplot$term[which(dataplot$term=="lo")]<-"Lo"
dataplot$term[which(dataplot$term=="mi")]<-"Int"
dataplot$term[which(dataplot$term=="hi")]<-"Hi"
dataplot$term<-factor(dataplot$term,levels=c("Lo","Int","Hi"))

dataplot$axis_label<-gsub(", top 2m","",dataplot$axis_label)
dataplot$axis_label<-gsub("ind/m3","1000 ind/m3",dataplot$axis_label)

hilo_varorder<-c("Watertemp","DO","pH","Secchi","Chl","TDN","TDP","SO4","Cond","Ca","Cl","K","Mg","Na","clad","rot","cope")

# factorize and re-order axis labels into desired sort order
dataplot$axis_label<-factor(dataplot$axis_label, levels=unique(dataplot$axis_label)[match(hilo_varorder,dataplot$var_abbrev1 %>% unique())])

dataplot<-dataplot %>% filter(!variable %in% c("Mg","Na","Ca","Cl","K"))
dataplot$estimate[which(dataplot$variable %in% c("CLAD","ROT","COPE"))]<-(1/1000)*(dataplot$estimate[which(dataplot$variable %in% c("CLAD","ROT","COPE"))])
dataplot$std.error[which(dataplot$variable %in% c("CLAD","ROT","COPE"))]<-(1/1000)*(dataplot$std.error[which(dataplot$variable %in% c("CLAD","ROT","COPE"))])

#dataplot$estimate[which(dataplot$variable %in% c("CLAD","ROT","COPE"))]<-log10(dataplot$estimate[which(dataplot$variable %in% c("CLAD","ROT","COPE"))])
#dataplot$std.error[which(dataplot$variable %in% c("CLAD","ROT","COPE"))]<-log10(dataplot$std.error[which(dataplot$variable %in% c("CLAD","ROT","COPE"))])

dotplot<-ggplot(dataplot,aes(x=estimate,y=term))+
  geom_point()+
  geom_errorbar(aes(xmin=estimate-std.error, xmax=estimate+std.error), width=.2,
                position=position_dodge(0.05))+
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
  facet_wrap(~axis_label,scales="free",nrow=3)+
  theme_bw()+
  xlab("value")+ylab("")#+
#  theme(axis.text.x = element_text(size=7))
dotplot

do<-0

if(do==1){
ggplot(dataplot,aes(x=estimate,y=term))+
  geom_point()+
  geom_errorbar(aes(xmin=estimate-std.error, xmax=estimate+std.error), width=.2,
                position=position_dodge(0.05))+
  facet_wrap(~axis_label,scales="free")+
  theme_bw()

ggplot(dataplot, aes(y=term, x=estimate)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
#  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
#                position=position_dodge(.9))  +
  facet_wrap(~axis_label,scales="free")+
  theme_bw()
}

#png(file="../figures/analysispowers/dotplot_hilomid.png",width=7.75,height=3.75,units="in",res=600)
png(file="../figures/analysispowers/dotplot_hilomid.png",width=6,height=3.5,units="in",res=600)
dotplot
dev.off()
################################3
# do plots 
dataplot<-data_forstats %>% filter((snowyrlo_snotel==1 | snowyrhi_snotel==1), variable %in% c("ProfTemp_top2m"))
dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"

plot_wtempSWE_snotel<-ggplot(dataplot, aes(x=SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  xlab("May SWE (cm), SNOTEL")+
  ylab(paste0("Water temp. (", intToUtf8(176), "C), top 2m"))+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85) +
  xlim(x=c(0,325))

plot_wtempSWE_snodas<-ggplot(dataplot, aes(x=SWE_May_snodas, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  xlab("May SWE (cm), SNODAS")+
  ylab(paste0("Water temp. (", intToUtf8(176), "C), top 2m"))+
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
  ylab(paste0("Water temp. (", intToUtf8(176), "C), top 2m"))+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/wtemp_snow.png",width=6,height=2.8,units="in",res=600)
grid.arrange(plot_wtempSWE_snotel,plot_wtempSWE_snodas,ncol=2)
dev.off()

dataplot<-unique(subset(bigjoin.stats,bigjoin.stats$variable=="SWE_May_snotel") %>% dplyr::select(park_code,Snowsite,year,value))
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
                                                                                              "secchi_value_m" , "pH_top2m",
                                                                                              "SO4","Total N"))
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
  facet_wrap(~axis_label,ncol=3,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/limnoSWEhighlow.png",width=6.5,height=4,units="in",res=600)
plot_limno
dev.off()

dataplot<-data_forstats %>% filter((year==minYear_SWE_May_snotel | snowyrhi_snotel==1), variable %in% c("SpCond_top2m","Ca","Cl","K","Mg","Na"))
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
  facet_wrap(~substr(axis_label,1,24),ncol=3,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/chemSWEhighlow.png",width=6.5,height=4,units="in",res=600)
plot_chems
dev.off()

dataplot<-data_forstats %>% filter((year==minYear_SWE_May_snotel | snowyrhi_snotel==1), variable %in% c("CLAD","ROT","COPE"))
checktiedyears<- dataplot %>% filter(snowyrlo_snotel==1) %>% group_by(variable,short_code) %>% unique() %>%
  dplyr::summarize(minyr=min(year),maxyr=max(year)) %>% as.data.frame()
tiedyears<-checktiedyears[which(checktiedyears$minyr!=checktiedyears$maxyr),]

# for plot, use only first low year if two low years share the same value
#dataplot<-dataplot[-which(dataplot$variable %in% tiedyears$variable & dataplot$short_code %in% tiedyears$short_code & 
#                            dataplot$year %in% tiedyears$maxyr),]
dataplot$xaxis_label<-"May SWE (cm)"

plot_zoops<-ggplot(dataplot, aes(x=SWE_May_snotel, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,box.padding=0.1,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo_snotel==1)) + 
  scale_y_log10() +
  xlab(dataplot$xaxis_label)+ylab("Value")+
  annotation_logticks(sides="l",color="gray",size=0.3)+
  xlim(c(-30,max(dataplot$SWE_May_snotel)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~substr(axis_label,1,24),ncol=3,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+ 
  scale_colour_viridis_d(end = 0.85)

png(file="../figures/analysispowers/zoopSWEhighlow.png",width=6.5,height=2,units="in",res=600)
plot_zoops
dev.off()

############################### 
# do cross correlation plots
#data_forstats_cross<-data_forstats
#data_forstats
#labels_tbl


crosscorr<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$snowyrhi_snotel==1) %>% 
 select(park_site,Elev_lake=Elevation_m,Elev_wshed=elevmean_wshed,
         Area_lake=Surface_area_ha,Area_wshed=Watershed_area_ha,WaLa_ratio=Watershed_area_to_lake_area_ratio,
        Depth_mean=Depth_mean_m,Vol_m3=Volume_m3,
         SWE_May_snotel,#SWE_May_snodas,
         Solar_dec=solar_dec,Solar_jas=solar_jas,Aspect_ns=northface,
#         flush_index_SWE_May_snotel,flush_index_SWE_May_snodas,hydro
         Forest_pct=forest,Snowice_pct=snowice,Barren_pct=barren) %>%                                                                                              
#         BlueLineInlet,BlueLineOutlet,salmo_adult_ever) %>% 
  group_by(park_site) %>%
  summarise_all(list(~mean(.))) %>% 
 select(-park_site) %>% as.data.frame()

#crosscorr<-crosscorr %>% select()

#crosscorr<-crosscorr %>% dplyr::rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio)
crosscorr.m<-as.matrix(crosscorr)
res1 <- cor.mtest(crosscorr, conf.level = 0.95)

corrs_physio<-cor(crosscorr.m)
png(filename = "../figures/analysispowers/corrplot_landscape.png", width = 6, height = 6, units = "in",res=300)
corrplot_physio<-corrplot(cor(crosscorr.m), method = "ellipse", tl.col = "black", p.mat = res1$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()

#check correlations
predictor_corrs<-cor(crosscorr.m) %>% as.data.frame() %>% 
  mutate_if(is.numeric, signif,digits=2)
predictor_corrs<-predictor_corrs
predictor_corrs$var<-row.names(predictor_corrs)
predictor_corrs_long<-melt(predictor_corrs)
predictor_corrs_long[which(abs(predictor_corrs_long$value)>0.5),]
  
crosscorr_hiy<-subset(data_forstats,data_forstats$snowyrhi_snotel==1) %>% dplyr::select(park_site,var_abbrev1,value) %>% spread(var_abbrev1,value) %>% dplyr::select(-park_site)
crosscorr_loy<-subset(data_forstats,data_forstats$year==data_forstats$minYear_SWE_May_snotel) %>% dplyr::select(park_site,var_abbrev1,value) %>% spread(var_abbrev1,value) %>% dplyr::select(-park_site)
crosscorr_allyr<-data_forstats %>% group_by(park_site,var_abbrev1) %>% dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% spread(var_abbrev1,value) %>% as.data.frame() %>% dplyr::select(-park_site)
crosscorr_allyr<-data_forstats %>% dplyr::select(park_site,var_abbrev1,value,year) %>% group_by(park_site,var_abbrev1,year) %>% dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% spread(var_abbrev1,value) %>% as.data.frame() %>% dplyr::select(-park_site,-year) %>% na.omit()

cross_varorder<-c("Watertemp","DO","pH","Secchi","Chl","TDN","TDP","SO4","Cond","clad","rot","cope","Ca","Cl","K","Mg","Na")

crosscorr_hiy<-crosscorr_hiy[,match(cross_varorder,names(crosscorr_hiy))]
crosscorr_loy<-crosscorr_loy[,match(cross_varorder,names(crosscorr_loy))]
crosscorr_allyr<-crosscorr_allyr[,match(cross_varorder,names(crosscorr_allyr))]
names(crosscorr_allyr)[which(names(crosscorr_allyr)=="clad")]<-"Clad"
names(crosscorr_allyr)[which(names(crosscorr_allyr)=="rot")]<-"Rot"
names(crosscorr_allyr)[which(names(crosscorr_allyr)=="cope")]<-"Cope"
names(crosscorr_allyr)[which(names(crosscorr_allyr)=="Watertemp")]<-"H2Otemp"

#names(crosscorr_allyr)[match(names(crosscorr_allyr),labels_tbl$var_abbrev1)]<-
#  labels_tbl$var_abbrev1[match(names(crosscorr_allyr),labels_tbl$var_abbrev1)]

#match(labels_tbl$variable,names(crosscorr_allyr))

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

variable_corrs<-cor(crosscorr_allyr.m) %>% as.data.frame() %>% 
  mutate_if(is.numeric, signif,digits=2)
variable_corrs<-variable_corrs
variable_corrs$var<-row.names(variable_corrs)
variable_corrs_long<-melt(variable_corrs)
variable_corrs_long[which(abs(variable_corrs_long$value)>0.5 & abs(variable_corrs_long$value)<1),]


varscheck<-c("variable","value","ice_out_doy","days_since_iceout","SWE_May_snotel","solar_jas","solar_dec","Volume_m3","hydro",
  "N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009",
  "SO4_dep_2000_2009",
  "andesite","basalt","biotitegneiss","granodiorite","graywacke")

data_forcorrs<-data_forstats[,which(names(data_forstats) %in% varscheck)]
data_forcorrs<-data_forcorrs %>% pivot_wider(names_from = variable, values_from = value)

data_forcorrs_m<-as.matrix(data_forcorrs)
data_forcorrs_m_test <- cor(na.omit(data_forcorrs_m))
data_forcorrs_m_test_long<-melt(data_forcorrs_m_test)
correlated<-data_forcorrs_m_test_long[which(abs(data_forcorrs_m_test_long$value)>0.5 &  abs(data_forcorrs_m_test_long$value)<1),]
correlated



###########################

make_stars <- function(vector,pval) {
  pval<-replace_na(pval,99)
  for (i in 1:length(vector)){
    stars = ""
    if(pval[i] <= 0.001){
      stars = "***"
    }
    if(pval[i] > 0.001 & pval[i] <= 0.01){
      stars = "**"
    }
    if(pval[i] > 0.01 & pval[i] <= 0.05){
      stars = "*"
    }
    if(pval[i] > 0.05 & pval[i] <= 0.1){
      stars = ". "
    #stars
    }
    starredi<-paste(vector[i],stars,sep="")
    if(i==1){starred<-starredi}
    if(i>1){starred<-c(starred,starredi)}
  }
  starred
}

make_stars_term <- function(vector,term,pval) {
  pval<-replace_na(pval,99)
  for (i in 1:length(vector)){
    starsi = ""
    termi = ""
    if(pval[i] <= 0.001){
      starsi = "***"
    termi = term[i]
    }
    if(pval[i] > 0.001 & pval[i] <= 0.01){
      starsi = "**"
    termi = term[i]
    }
    if(pval[i] > 0.01 & pval[i] <= 0.05){
      starsi = "*"
    termi = term[i]
    }
    if(pval[i] > 0.05 & pval[i] <= 0.1){
      starsi = ". "
    termi = term[i]
    }
    #stars
    starredi<-paste(vector[i],termi,starsi,sep="")
    if(i==1){starred<-starredi}
    if(i>1){starred<-c(starred,starredi)}
  }
  starred
}
#########################################

file_models_zscore<-"../figures/analysispowers/modelouts_zscore"
file_models_nozscore<-"../figures/analysispowers/modelouts"

fixedpart1<-"vari1"
fixedpart2<-"vari1+vari2"
fixedpart2b<-"vari1*vari2"
fixedpart3<-"vari1+vari2+vari3"
fixedpart3b<-"(vari1+vari2+vari3)^2"
randompart<-list(~1|park_site)

fileout<-file_models_nozscore
#data_forstats$value_model<-data_forstats$value
valuecol_set<-"value"
swecol_set<-"SWE_May_snotel"
iceoutdcol_set<-"ice_out_doy"
doplots_set<-0
usenlme<-1
source("models_hypoth.R")

models_df$varname<-as.character(models_df$varname)
models_top10_df<-models_df %>% filter(AICcdiff<=10) %>% arrange(varname,AICcdiff)
write.csv(models_df,file=paste(fileout,".csv",sep=""),row.names=FALSE)
write.csv(models_top10_df,file=paste(fileout,"_top10AIC.csv",sep=""),row.names=FALSE)


# adjust next line to include/exclude days_since_ice_out
models_df<-models_df[-grep("days_since_iceout",models_df)]
models_top_df<-models_df %>% filter(AICcdiff<=2) %>% arrange(varname,AICcdiff)
write.csv(models_top_df,file=paste(fileout,"_top.csv",sep=""),row.names=FALSE)

r2mr2c<-models_top_df %>% select(modname,modelformula,R2m,R2c,AICc) %>% unique()
r2mr2c$r2mcdiff<-r2mr2c$R2c-r2mr2c$R2m
r2mr2c %>% dplyr::summarize(meanr2m=mean(R2m,na.rm=TRUE),
                            meanr2c=mean(R2m,na.rm=TRUE),
                            meanR2mcdiff=mean(r2mcdiff,na.rm=TRUE))
min(r2mr2c$R2m)
mean(r2mr2c$R2m)
max(r2mr2c$R2m)
min(r2mr2c$R2c)
mean(r2mr2c$R2c)
max(r2mr2c$R2c)
min(r2mr2c$r2mcdiff)
mean(r2mr2c$r2mcdiff)
max(r2mr2c$r2mcdiff)

#models_df$varname<-as.character(models_df$varname)
models_top_signif_df<-models_df %>% filter(term!= "(Intercept)" & p.value<=0.05) %>% dplyr::select(varname,modname,modname2) %>% unique()
models_1signif<-models_df %>% filter(term != "SWE_May_snotel" & nparam==min(models_df$nparam) & term!= "(Intercept)" & p.value<=0.05) %>% dplyr::select(modname) %>% unique()
models_2signif<-models_df %>% filter(term != "SWE_May_snotel" & nparam>min(models_df$nparam) & term!= "(Intercept)" & p.value<=0.05) %>% dplyr::select(modname) %>% unique()

#write.csv(models_df,file=paste(fileout,".csv",sep=""),row.names=FALSE)
#write.csv(models_top_df,file=paste(fileout,"_top.csv",sep=""),row.names=FALSE)


covartbl<-
  models_top_df %>% filter(modname %in% models_top_signif_df$modname) %>% dplyr::select(varname,term,AICc,R2m,R2c,estimate,std.error,p.value,nparam,modname,modname2,modname3,var0,var1,var2,var3,varint1,varint2,varint3) %>%
  unique() %>%
  mutate(coef=paste(estimate," ± ",signif(std.error,1),sep="")) %>%
  mutate(modname2=str_replace(modname2,":","_")) %>%
  rename(p=p.value) %>%
  select(varname,term,AICc,R2m,R2c,coef,p,nparam,modname,modname2,modname3,var0,var1,var2,var3,varint1,varint2,varint3) %>%
  pivot_wider(names_from=c(var0,var1,var2,var3,varint1,varint2,varint3),values_from=c(term,coef,p)) %>%
  as.data.frame()
names(covartbl)<-gsub("_","",names(covartbl))

covartbl<-covartbl %>% dplyr::select(-term0)
covartbl$coef4sign<-covartbl$coef5sign<-covartbl$coef6sign<-""
covartbl$coef4sign[which(substr(covartbl$coef4,1,1)>=0)]<-"+"
covartbl$coef4sign[which(substr(covartbl$coef4,1,1)=="-")]<-"-"
covartbl$coef5sign[which(substr(covartbl$coef5,1,1)>=0)]<-"+"
covartbl$coef5sign[which(substr(covartbl$coef5,1,1)=="-")]<-"-"
covartbl$coef6sign[which(substr(covartbl$coef6,1,1)>=0)]<-"+"
covartbl$coef6sign[which(substr(covartbl$coef6,1,1)=="-")]<-"-"

covartbl$coef0star<-make_stars(covartbl$coef0,covartbl$p0)
covartbl$coef1star<-make_stars(covartbl$coef1,covartbl$p1)
covartbl$coef2star<-make_stars(covartbl$coef2,covartbl$p2)
covartbl$coef3star<-make_stars(covartbl$coef3,covartbl$p3)
covartbl$coef4star<-make_stars(covartbl$coef4,covartbl$p4)
covartbl$coef5star<-make_stars(covartbl$coef5,covartbl$p5)
covartbl$coef6star<-make_stars(covartbl$coef6,covartbl$p6)

covartbl[which(covartbl$p4>0.05),]$term4<-""
covartbl[which(covartbl$p4>0.05),]$coef4<-""
covartbl[which(covartbl$p5>0.05),]$term5<-""
covartbl[which(covartbl$p5>0.05),]$coef5<-""
covartbl[which(covartbl$p6>0.05),]$term6<-""
covartbl[which(covartbl$p6>0.05),]$coef6<-""

#covartbl[which(covartbl$p4<=0.05),]$term4<-paste(covartbl[which(covartbl$p4<=0.05),]$term4," ")
#covartbl[which(covartbl$p4<=0.05),]$coef4<-paste(covartbl[which(covartbl$p4<=0.05),]$coef4," ")
#covartbl[which(covartbl$p5<=0.05),]$term5<-paste(covartbl[which(covartbl$p5<=0.05),]$term5," ")
#covartbl[which(covartbl$p5<=0.05),]$coef5<-paste(covartbl[which(covartbl$p5<=0.05),]$coef5," ")
#covartbl[which(covartbl$p6<=0.05),]$term6<-paste(covartbl[which(covartbl$p6<=0.05),]$term6," ")
#covartbl[which(covartbl$p6<=0.05),]$coef6<-paste(covartbl[which(covartbl$p6<=0.05),]$coef6," ")

covartbl<-covartbl %>% dplyr::select(-c(p0,p1,p2,p3,p4,p5,p6))

covartbl[is.na(covartbl)] <- ""
covartbl$coef2<-gsub("NA","",covartbl$coef2)
covartbl$coef3<-gsub("NA","",covartbl$coef3)
covartbl$coef4<-gsub("NA","",covartbl$coef4)
covartbl$coef5<-gsub("NA","",covartbl$coef5)
covartbl$coef6<-gsub("NA","",covartbl$coef6)

covartbl$term4sign<-paste(covartbl$coef4sign,covartbl$term4,sep="")
covartbl$term5sign<-paste(covartbl$coef5sign,covartbl$term5,sep="")
covartbl$term6sign<-paste(covartbl$coef6sign,covartbl$term6,sep="")
covartbl$term4sign[which(covartbl$term4sign=="-")]<-""
covartbl$term5sign[which(covartbl$term5sign=="-")]<-""
covartbl$term6sign[which(covartbl$term6sign=="-")]<-""
covartbl$term4sign[which(covartbl$term4sign=="+")]<-""
covartbl$term5sign[which(covartbl$term5sign=="+")]<-""
covartbl$term6sign[which(covartbl$term6sign=="+")]<-""

covartbl<-covartbl %>% mutate(interactions=paste(term4sign,term5sign,term6sign))
covartbl$interactions<-gsub("  "," ",covartbl$interactions)
covartbl$interactions<-gsub("  "," ",covartbl$interactions)
covartbl$interactions<-gsub("  "," ",covartbl$interactions)
covartbl$interactions<-gsub("\\*","",covartbl$interactions)
covartbl$interactions<-paste(" ",covartbl$interactions)
covartbl$interactions<-gsub("  "," ",covartbl$interactions)
covartbl$interactions<-gsub("  "," ",covartbl$interactions)
#covartbl$interactions[which(covartbl$interactions==" ")]<-"-"
#covartbl$term3[which(covartbl$term3=="")]<-"-"

covartbl<-covartbl %>% dplyr::select(var=varname,model=modname2,k=nparam,AICc,R2m,R2c,
  coef0,term1,coef1,
  term2,coef2,
  term3,coef3,
  interactions,
  coef0star,coef1star,
  coef2star,coef3star,
  coef4star,coef5star,
  coef6star,
  term4,term5,term6,
  -modname)

covartbl$R2m<-signif(covartbl$R2m,digits=2)
covartbl$R2c<-signif(covartbl$R2c,digits=2)

covartbl$model[which(covartbl$model=="vari1")]<-"1"
covartbl$model[which(covartbl$model=="vari1+vari2")]<-"2"
covartbl$model[which(covartbl$model=="vari1*vari2")]<-"2x"
covartbl$model[which(covartbl$model=="vari1+vari2+vari3")]<-"3"
covartbl$model[which(covartbl$model=="(vari1+vari2+vari3)^2")]<-"3x"

covartbl <- covartbl %>% mutate_all(funs(str_replace(., "quartzmonzonite", "qzm"))) %>% 
  mutate_all(funs(str_replace(., "graywacke", "grw"))) %>%
  mutate_all(funs(str_replace(., "granodiorite", "grd"))) %>%
  mutate_all(funs(str_replace(., "biotitegneiss", "btg"))) %>%
  mutate_all(funs(str_replace(., "basalt", "bst"))) %>%
  mutate_all(funs(str_replace(., "Watertemp", "Temp"))) %>%
  mutate_all(funs(str_replace(., "_dep_2000_2009", "dep"))) %>%
  mutate_all(funs(str_replace(., "_dep_2000_2009", "dep"))) %>%
  mutate_all(funs(str_replace(., "Watertemp", "Temp"))) %>%
  mutate_all(funs(str_replace(., "iceout_doy", "iceoutd"))) %>%
  mutate_all(funs(str_replace(., "iceout_doy", "iceoutd"))) %>%
  mutate_all(funs(str_replace(., "ice_out_doy", "iceoutd"))) %>%
  mutate_all(funs(str_replace(., "ice_out_doy", "iceoutd"))) %>%
  mutate_all(funs(str_replace(., "solar_jas", "solarjas"))) %>%
  mutate_all(funs(str_replace(., "solar_jas", "solarjas"))) %>%
  mutate_all(funs(str_replace(., "solar_dec", "solardec"))) %>%
  mutate_all(funs(str_replace(., "solar_dec", "solardec"))) %>%
  mutate_all(funs(str_replace(., "Elevation_m", "elev"))) %>%
  mutate_all(funs(str_replace(., "Elevation_m", "elev"))) %>%
  mutate_all(funs(str_replace(., "volume", "vol")))

#desired_order <- data.frame(var=c("Temp","DO","pH","Secchi","Chl","TDN","SO4","Cond","SWE","iceoutd","Ca","Cl",
#                                  "K","Mg","Na"))
#desired_order <- data.frame(var=c("Temp","DO","pH","Secchi","Chl","TDN","SO4","Clad","Rot","Cope","SWE","iceoutd",
#  "Cond","Ca","Cl","K","Mg","Na"))
#desired_order <- data.frame(var=c("Temp","DO","pH","Secchi","Chl","TDN","SO4","log10Clad","log10Rot","log10Cope","SWE","iceoutd",
#  "Cond","Ca","Cl","K","Mg","Na","Clad","Rot","Cope"))
#desired_order <- data.frame(var=c("Temp","DO","pH","Secchi","Chl","log10Chl","TDN","SO4","log10Clad","log10Rot","log10Cope","SWE","iceoutd",
#  "Cond","Ca","Cl","K","Mg","Na","Clad","Rot","Cope"))
desired_order <- data.frame(var=c("Temp","DO","pH","Secchi","Chl","TDN","SO4","log10Clad","log10Rot","log10Cope","SWE","iceoutd",
  "Cond","Ca","Cl","K","Mg","Na","Clad","Rot","Cope"))


desired_order$order<-1:length(desired_order[,1])
covartbl_main<-merge(desired_order,covartbl,by="var") 
covartbl_main<-covartbl_main %>% arrange(order,k,(AICc)) %>% 
  dplyr::select(var,model,k,AICc,R2m,R2c,b0=coef0,
    term1,m1=coef1,
    term2,m2=coef2,
    term3,m3=coef3,
    interactions)
write.csv(covartbl_main,file=paste(fileout,"_covar_sig_main.csv",sep=""),fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)


covartbl_supp0<-covartbl %>% arrange(order,k,(AICc)) %>% 
  dplyr::select(var,model,k,AICc,R2m,R2c,b0=coef0star,
    term1,m1=coef1star,
    term2,m2=coef2star,
    term3,m3=coef3star,
    term4,coef4star,
    term5,coef5star,
    term6,coef6star)

covartbl_supp0<-covartbl_supp0 %>% mutate(interaction1=paste(term4,coef4star))
covartbl_supp0<-covartbl_supp0 %>% mutate(interaction2=paste(term5,coef5star))
covartbl_supp0<-covartbl_supp0 %>% mutate(interaction3=paste(term6,coef6star))
#which(covartbl_supp0$interaction1==" NA" & covartbl_supp0$interaction2!=" NA")
#which(covartbl_supp0$interaction2==" NA" & covartbl_supp0$interaction3!=" NA")
covartbl_supp<-covartbl_supp0 %>% select(var,model,k,AICc,R2m,R2c,b0,
  term1,m1,
  term2,m2,
  term3,m3,
  interaction1,interaction2,interaction3)
#covartbl_supp<-covartbl_supp0
covartbl_supp<-covartbl_supp %>% mutate_all(funs(str_replace(., " NA", "-")))
covartbl_supp<-covartbl_supp %>% mutate_all(funs(str_replace(., "NA", "-")))
covartbl_supp$term2[which(covartbl_supp$term2=="")]<-"-"
covartbl_supp$term3[which(covartbl_supp$term3=="")]<-"-"
write.csv(covartbl_supp,file=paste(fileout,"_covar_sig_supp.csv",sep=""),fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)

#######################################

file_models_nozscore<-"../figures/analysispowers/modelouts"

fileout<-file_models_nozscore
#data_forstats$value_model<-data_forstats$value
valuecol_set<-"value"
swecol_set<-"SWE_May_snotel"
iceoutdcol_set<-"ice_out_doy"
doplots_set<-0
usenlme<-0
source("models_hypoth.R")

onevar<-merge(
  models_df %>% filter(modname %in% models_1signif$modname) %>% dplyr::select(varname,term,estimate,std.error,p.value,modname,R2m,AICc) %>%
    filter(term=="(Intercept)") %>% dplyr::select(-term),
  models_df %>% filter(modname %in% models_1signif$modname) %>% dplyr::select(varname,term,estimate,std.error,p.value,modname,R2m,AICc) %>%
    filter(term!="(Intercept)"),
  by=c("modname","varname"),
  suffixes=c("_int","_slope")
) %>% 
  select(-modname) %>% 
  mutate(varname,#relate=paste(varname,"~",term),
    term,coef=paste(estimate_slope," ± ",std.error_slope,sep=""),
    int=paste(estimate_int," ± ",std.error_int,sep="")) %>%
  select(varname,term,coef,int,p.value_slope,p.value_int,R2=R2m_slope,AICc=AICc_slope)#,std.error_int,p.value_int)

onevar$coef<-make_stars(onevar$coef,onevar$p.value_slope)
onevar$int<-make_stars(onevar$int,onevar$p.value_int)

#main_varorder<-c("Watertemp","DO","pH","Secchi","Chl","TDN","TDP","SO4","Cond","SWE","ice_out_doy")
#main_varorder<-c("Watertemp","DO","pH","Secchi","Chl","TDN","TDP","SO4","Cond","Clad","Rot","Cope","SWE","ice_out_doy")
main_varorder<-c("Watertemp","DO","pH","Secchi","Chl","log10Chl","TDN","TDP","SO4","Cond","log10Clad","log10Rot","log10Cope","SWE","ice_out_doy","Clad","Rot","Cope")

second_varorder<-c("Ca","Cl","K","Mg","Na")

onevar_main<-onevar %>% filter(varname %in% main_varorder)
onevar_supp<-onevar %>% filter(varname %in% second_varorder)

write.csv(onevar_main,file=paste(fileout,"_onevar_sig.csv",sep=""),fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)
write.csv(onevar_supp,file=paste(fileout,"_onevar_supp_sig.csv",sep=""),fileEncoding = "UTF-16LE",row.names=FALSE,quote=FALSE)

#############################################################
