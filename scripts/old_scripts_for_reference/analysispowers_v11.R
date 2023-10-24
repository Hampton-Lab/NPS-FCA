library(nlme) # needed for mixed model
library(broom)
library(tidyr)
library(tidyverse)
library(broom.mixed)
library(car)
library(reshape2)
library(corrplot)
library(ggplot2)
library(ggrepel)
#library(rpart)
library(gridExtra)

#options(digits = 3, scipen = -2)
#options(scipen = 999)

bigjoin <- readRDS(file = file.path("..",
                                    "data",
                                    "analysis_outputs",
                                    "bigjoin.rds"))

#short_codes <- tribble(
#  ~site_code, ~short_code,
#  "LH14",     "PA",
#  "LH15",     "15",
#  "LN03",     "AL",
#  "LP19",     "19",
#  "LW32",     "DW",
#  "LZ35",     "BL",
#  "LS-07-01",     "LB",
#  "MA-03-01",     "SI",
#  "MC-03-01",     "ER",
#  "MC-14-02",     "LE",
#  "MR-12-01",     "BO",
#  "SM-02-02",     "TR",
#  "106",     "GL",
#  "138",     "FE",
#  "263",     "HE",
#  "426",     "CR",
#  "498",     "MI",
#  "520",     "LC",
#  "623",     "SU",
#  "627",     "CO",
#  "Hoh",     "HO"
#)

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
  "Total N", "Total dissolved N (mg/L)",
  "ice_out_doy","Ice out day of year",
  "flush_index_SWE_May","Catchment snow vol : lake vol"
)


#find minimum nonzero value
min_nonzero<-function(x){
  x_sort<-sort(unique(x))
  x_sort2<-x_sort[which(x_sort!=0)]
  xmin<-min(x_sort2)
  return(xmin)
}

bigjoin.stats <- full_join(x = bigjoin, y = short_codes, by = c("Lake"))
bigjoin.stats$park_siteshort<-paste(bigjoin.stats$park_code,"-",bigjoin.stats$short_code,sep="")

bigjoin.stats <- full_join(x = bigjoin.stats, y = axis_label_tbl, by = c("variable"))


bigjoin.stats$hydro<-""
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
bigjoin.stats$inlet<-0
bigjoin.stats$inlet[which(bigjoin.stats$BlueLineInlet=="T")]<-1
bigjoin.stats$outlet<-0
bigjoin.stats$outlet[which(bigjoin.stats$BlueLineOutlet=="T")]<-1
bigjoin.stats$northface<-0
bigjoin.stats$northface[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-1
######################################
# corrections? 

bigjoin.stats$value[which(bigjoin.stats$variable=="ProfTemp_top2m" & bigjoin.stats$value<0)]<-0
  
######################################
# summaries

bigjoin.stats.summ<-bigjoin.stats %>% group_by(variable,Lake) %>%
  dplyr::summarize(CV=abs(sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE)),
                   CV_SWE_May=abs(sd(SWE_May,na.rm=TRUE)/mean(SWE_May,na.rm=TRUE)),
                   corr=cor(value,SWE_May)) %>% as.data.frame()

bigjoin.stats.summ %>% filter(variable %in% 
                                c("ProfTemp_top2m",
                                  "DO_top2m", "DOsat_top2m",#"DO_below2m",
                                  "Chlorophyll", 
                                  "secchi_value_m" , "pH_top2m",
                                  #                                "pH_below2m",
                                  "SpCond_top2m",
                                  "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>%
  arrange(-CV) %>% as.data.frame()

######################################
# sites table

sitestable0<-bigjoin.stats %>% select(Park,Lake,Elevation_m,Watershed_area_ha,Surface_area_ha,Volume_m3,Depth_max,Depth_mean_m,Shoreline_length_m,Watershed_area_to_lake_area_ratio,Aspect,solar_dec,solar_jas,snowice,forest,barren,hydro,Snowsite)
sitestable<-unique(sitestable0)
sitestable<-sitestable[order(sitestable$Park,sitestable$Elevation_m),]
sitestable<-sitestable %>% mutate_if(is.numeric, signif,digits=2)
sitestable$Park[which(sitestable$Park=="North Cascades National Park Service Complex")]<-"N. Cascades"
sitestable$Park[which(sitestable$Park=="Olympic National Park")]<-"Olympic"
sitestable$Park[which(sitestable$Park=="Mount Rainier National Park")]<-"Rainier"
sitestable$Aspect[which(sitestable$Aspect=="East")]<-"E"
sitestable$Aspect[which(sitestable$Aspect=="West")]<-"W"
sitestable$Aspect[which(sitestable$Aspect=="North")]<-"N"
sitestable$Aspect[which(sitestable$Aspect=="South")]<-"S"
sitestable$Aspect[which(sitestable$Aspect=="Southeast")]<-"SE"
sitestable$Aspect[which(sitestable$Aspect=="Southwest")]<-"SW"
sitestable$Aspect[which(sitestable$Aspect=="Northeast")]<-"NE"
sitestable$Aspect[which(sitestable$Aspect=="Northwest")]<-"NW"
which_end<-grep(" Lake",sitestable$Lake)
sitestable$Lake[which_end]<-substr(sitestable$Lake[which_end],1,nchar(sitestable$Lake[which_end])-5)
which_first<-grep("Lake",sitestable$Lake)
sitestable$Lake[which_first]<-substr(sitestable$Lake[which_first],6,nchar(sitestable$Lake[which_first]))

sitestable<-sitestable %>% dplyr::rename(Elev_m=Elevation_m,Depthmax_m=Depth_max,Depthmean_m=Depth_mean_m,
                                         Shorelength_m=Shoreline_length_m,SurfArea_ha=Surface_area_ha,
                                         ShedArea_ha=Watershed_area_ha,Vol_m3=Volume_m3,
                                         ShedAreaLakeArea_ratio=Watershed_area_to_lake_area_ratio,
                                         SolarJAS_Whrperm2=solar_jas,SolarDec_Whrperm2=solar_dec)

write.csv(sitestable,"../figures/analysispowers/sitestable.csv")

###################################

iceout_SWE<-data_forstats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank = dense_rank((SWE_May))/max(dense_rank((SWE_May)))) %>% as.data.frame()


# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","HE","HO","LC"))

iceout_SWE_plot<-ggplot(data=iceout_SWE,aes(y=SWE_rank,x=SWE_May,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank,x=SWE_May),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank,x=ice_out_doy,color=park_code),shape=0,size=2)+
  theme_bw()+
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~park_siteshort,nrow=3)

ggsave(filename = "../figures/analysispowers/iceout_SWE_plot.png",
       plot = iceout_SWE_plot, width = 8, height = 5, units = "in")

########################################


data_forstats<- bigjoin.stats %>% #mutate(snowhi=year==2011,snowlo=year==2015,snowrest=!year %in% c(2011,2015)) %>% 
  dplyr::filter(#year %in% c(2011, 2015),
    variable %in% c("ProfTemp_top2m",
                    "DO_top2m", "DOsat_top2m",#"DO_below2m",
                    "Chlorophyll", 
                    "secchi_value_m" , "pH_top2m",
                    #                                "pH_below2m",
                    "SpCond_top2m",
                    "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>% #,"SurfTemp_8","MidTemp_8","BotTemp_8")) %>%
  select(variable,axis_label,year,short_code,park_code,park_site,park_siteshort,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,#Asp,
         solar_jas,solar_dec,
         barren,forest,#shrub,meadow,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro,
         salmo_adult_ever,
         northface,inlet,outlet) %>%
  unique() %>% as.data.frame()

# [1] takes the first year of minimum, if two years had the same minimum (zero!)
# this omits min/max swe years that lacked temp profile data, aka the lake was not / could not be visited
# year>2008 omits 2008, which had incomplete data for some NOCA sites, though may need to use 2010?

minyears<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99,year>2008) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(min_SWE_May=min(SWE_May),minYear_SWE_May=year[which(SWE_May == min(SWE_May))][1]) %>% as.data.frame()

maxyears<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99) %>% 
  select(year,short_code,park_code,park_site,park_siteshort,value,SWE_May) %>%
  group_by(park_site,short_code) %>%
  dplyr::summarize(max_SWE_May=max(SWE_May),maxYear_SWE_May=year[which(SWE_May == max(SWE_May))][1]) %>% as.data.frame()


data_forstats<-merge(data_forstats,minyears,by=c("park_site","short_code"))
data_forstats<-merge(data_forstats,maxyears,by=c("park_site","short_code"))

data_forstats$snowyrlo<-0
data_forstats$snowyrlo[which(data_forstats$year==data_forstats$minYear_SWE_May)]<-1
data_forstats$snowyrlo[which(data_forstats$SWE_May==data_forstats$min_SWE_May)]<-1

data_forstats$snowyrhi<-0
data_forstats$snowyrhi[which(data_forstats$year==data_forstats$maxYear_SWE_May)]<-1
data_forstats$snowyrhi[which(data_forstats$SWE_May==data_forstats$max_SWE_May)]<-1

data_forstats$snowyrmid<-0
data_forstats$snowyrmid[which(data_forstats$SWE_May!=data_forstats$min_SWE_May & data_forstats$SWE_May!=data_forstats$max_SWE_May)]<-1


by_variable <- group_by(data_forstats, variable)

vars<-c("snowyrlo","snowyrhi","Depth_mean_m","Volume_m3","Watershed_area_ha","Elevation_m",
        "SWE_May",#"SWE_Apr",
        "Watershed_area_to_lake_area_ratio","northface",#"aspectns_lake","aspectns_wshed","northface_wshed",
        "flush_index_SWE_May",#"flush_index_SWE_Apr",#"park_code",
        "salmo_adult_ever",
        "solar_jas","solar_dec","barren","forest",#"shrub","meadow",
        "inlet","outlet")
vars_1covar<-vars[-which(vars %in% c("snowyrlo","snowyrmid","snowyrhi"))] 
vars_2covar<-combn(vars[-which(vars %in% c("yr","park_code","snowyrlo","snowyrmid","snowyrhi","SWE_May","flush_index_SWE_May"))],2)

# do z scores? 
#for (i in 1:length(vars)){
#  vari<-vars[i]
#  scaledi<-scale(data_forstats[,which(names(data_forstats)==vari)])
#  data_forstats[,which(names(data_forstats)==vari)]<-scaledi
#}

by_variablei <- group_by(data_forstats, variable)

mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ snowyrlo+snowyrhi, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
mi_glance<-do(by_variablei, glance(lme(value ~ snowyrlo+snowyrhi, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=1) %>% select(variable,sigma,logLik,AIC,BIC,model)
mi<-merge(mi,mi_glance,by=c("variable","model"))
m_0covar<-mi

m_0covar_intonly<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ 1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=0)
m_0covar_intonly_glance<-do(by_variablei, glance(lme(value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_0covar_intonly<-merge(m_0covar_intonly,m_0covar_intonly_glance,by=c("variable","model"))
#m_0covar<-rbind(m_0covar)
nparam<-length(unique(m_0covar$term))
nparam<-1

m_0covar<-m_0covar[order(m_0covar$variable,m_0covar$model),]
m_0covar$sig<-""
m_0covar$sig[which(m_0covar$p.value<=0.05/nparam)]<-"*"
m_0covar$sig[which(m_0covar$p.value<=0.01/nparam)]<-"**"
m_0covar$sig[which(m_0covar$p.value<=0.005/nparam)]<-"***"

m_0covar <- m_0covar %>% mutate_if(is.numeric, signif, digits=2) %>% arrange(variable)
m_0covar_mainint<-subset(m_0covar,m_0covar$term!="(Intercept)"|(m_0covar$term=="(Intercept)" & m_0covar$model==1))
m_0covar_allvar<-subset(m_0covar,m_0covar$model==1) %>% arrange(variable,model,term) %>% rename(val=effect)

m_0covar_out<-m_0covar_mainint %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value) %>% as.data.frame()
#m_0covar_temper_out<-m_0covar_out %>% filter(variable=="ProfTemp_top2m" & term!="salmo_adult_ever" & nparam<=3)

m_0covar_allvar<-subset(m_0covar_out,m_0covar_out$model==1)
m_0covar_allvar_int<-subset(m_0covar_allvar,m_0covar_allvar$term=="(Intercept)") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-nparam,-pval) %>% dplyr::rename(val.int=val,se.int=se,sig.int=sig)
m_0covar_allvar_slope_snowyrlo<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrlo") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-nparam,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)
m_0covar_allvar_slope_snowyrhi<-subset(m_0covar_allvar,m_0covar_allvar$term=="snowyrhi") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-nparam,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)
m_0covar_allvar_slope<-merge(m_0covar_allvar_slope_snowyrlo,m_0covar_allvar_slope_snowyrhi,by=c("variable"),suffixes=c(".lo",".hi"))
m_0covar_allvar_out<- merge(m_0covar_allvar_int,m_0covar_allvar_slope,by=c("variable"),suffixes=c(".int",""))
write.csv(m_0covar_allvar_out,"../figures/analysispowers/allvar_out.csv",row.names=FALSE)

for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-data_forstats %>% filter(snowyrmid==1)
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
#  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar_mid<-mi}
  if(i>1){m_1covar_mid<-rbind(m_1covar_mid,mi)}
}

for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-data_forstats %>% filter(snowyrhi==1)
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
#  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar_hi<-mi}
  if(i>1){m_1covar_hi<-rbind(m_1covar_hi,mi)}
}

for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-data_forstats %>% filter(snowyrlo==1)
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
#  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar_lo<-mi}
  if(i>1){m_1covar_lo<-rbind(m_1covar_lo,mi)}
}
m_1covar_mid$snowyr<-"mid"
m_1covar_hi$snowyr<-"hi"
m_1covar_lo$snowyr<-"lo"

m_1covar_mid$nparam<-3
m_1covar_hi$nparam<-3
m_1covar_lo$nparam<-3

m_1covar_mid$nobs<-170
m_1covar_hi$nobs<-21
m_1covar_lo$nobs<-21

m_1covar_mid<-m_1covar_mid %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
m_1covar_hi<-m_1covar_hi %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
m_1covar_lo<-m_1covar_lo %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))

m_1covar<-rbind(m_1covar_mid,m_1covar_hi,m_1covar_lo) %>% select(snowyr,variable,model,term,val=estimate,std.error,pval=p.value,AICc)
m_1covar_minAICc<-m_1covar %>% group_by(variable,snowyr) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_1covar<-merge(m_1covar,m_1covar_minAICc,by=c("variable","snowyr"))
m_1covar$AICcdiff<-m_1covar$AICc-m_1covar$minAICc

longstats_slopes<-m_1covar
longstats_slopes <-longstats_slopes %>% mutate_if(is.numeric, signif, digits=3) %>% select(-model) %>% arrange(variable,AICcdiff) %>% filter(AICcdiff<=6)
write.csv(longstats_slopes,"../figures/analysispowers/longstats_slopes.csv")

longstats_slopes_temper<-subset(longstats_slopes,longstats_slopes$variable=="ProfTemp_top2m")



#longstats_slopes_temper_int<-subset(longstats_slopes_temper,longstats_slopes_temper$term=="(Intercept)") %>% select(-term,-model,-sigma,-logLik,-AIC,-BIC,-nparam,-pval) %>% dplyr::rename(val.int=val,se.int=se,sig.int=sig)
longstats_slopes_temper_snowyrlo<-subset(longstats_slopes_temper,longstats_slopes_temper$snowyr=="lo") #%>% select(-term,-sigma,-logLik,-AIC,-BIC)# %>% dplyr::rename(val.lo=val,se.lo=se)
longstats_slopes_temper_snowyrhi<-subset(longstats_slopes_temper,longstats_slopes_temper$snowyr=="hi") #select(-term,-model,-sigma,-logLik,-AIC,-BIC,-nparam,-pval)# %>% dplyr::rename(val.lo=val,se.lo=se)
longstats_slopes_temper_snowyr<-merge(longstats_slopes_temper_snowyrlo,longstats_slopes_temper_snowyrhi,by=c("term"),suffixes=c(".lo",".hi"))
m_0covar_allvar_out<- merge(m_0covar_allvar_int,m_0covar_allvar_slope,by=c("variable"),suffixes=c(".int",""))







longstats_slopes<-subset(longstats_slopes,!longstats_slopes$term %in% c("yr","(Intercept)"))

nterms<-length(unique(longstats_slopes$variable))
longstats_slopes_sig<-subset(longstats_slopes,longstats_slopes$pval<=(0.05/nterms))
#longstats_slopes_sig<-longstats_slopes
longstats_slopes_sig$dir<-"0"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val>0)]<-' +'
longstats_slopes_sig$dir[which(longstats_slopes_sig$val<=0)]<-' -'

#longstats_slopes_sig_trunc<-longstats_slopes_sig %>% select(variable,term,val,dir)
#longstats_slopes_sig_trunc$snowyr<-substr(longstats_slopes_sig_trunc$term,7,8)
#longstats_slopes_sig_trunc$term<-substr(longstats_slopes_sig_trunc$term,10,nchar(longstats_slopes_sig_trunc$term))

longstats_slopes_sig$signvar<-paste(longstats_slopes_sig$dir,longstats_slopes_sig$variable,sep="")

widestats_snowyrhi<-longstats_slopes_sig %>% filter(snowyr=="hi") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()
widestats_snowyrlo<-longstats_slopes_sig %>% filter(snowyr=="lo") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()
widestats_snowyrmid<-longstats_slopes_sig %>% filter(snowyr=="mid") %>%
  group_by(term) %>% dplyr::summarise(vars = paste(signvar, collapse=", ")) %>% as.data.frame()

widestats_out<-merge(widestats_snowyrlo,widestats_snowyrmid,by=c("term"),all=TRUE,suffixes=c("_lo",""))
widestats_out<-merge(widestats_out,widestats_snowyrhi,by=c("term"),all=TRUE,suffixes=c("_mid","_hi"))
widestats_out$vars_hi<-as.character(widestats_out$vars_hi)
widestats_out$vars_lo<-as.character(widestats_out$vars_lo)
widestats_out$vars_mid<-as.character(widestats_out$vars_mid)

widestats_out
write.csv(widestats_out,"../figures/analysispowers/widestats.csv")

#stats<-stats[order(stats$variable,stats$model),]
#stats$sig<-""
#stats$sig[which(stats$pval<=0.05/nparam)]<-"*"
#stats$sig[which(stats$pval<=0.01/nparam)]<-"**"
#stats$sig[which(stats$pval<=0.005/nparam)]<-"***"

#############################

for (i in 1:length(vars_2covar[1,])){
  vari<-vars_2covar[,i]
  daterr<-data_forstats %>% filter(snowyrmid==1)
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  daterr$var2<-(daterr[,which(names(daterr)==vari[2])])
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_2covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_2covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"var1",vari[1])
  mi$term<-str_replace(mi$term,"var2",vari[2])
  mi$term<-str_replace(mi$term,"var1","")  
  if(i==1){m_2covar_mid<-mi}
  if(i>1){m_2covar_mid<-rbind(m_2covar_mid,mi)}
}

for (i in 1:length(vars_2covar[1,])){
  vari<-vars_2covar[,i]
  daterr<-data_forstats %>% filter(snowyrhi==1)
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  daterr$var2<-(daterr[,which(names(daterr)==vari[2])])
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_2covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_2covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"var1",vari[1])
  mi$term<-str_replace(mi$term,"var2",vari[2])
  mi$term<-str_replace(mi$term,"var1","")  
  if(i==1){m_2covar_hi<-mi}
  if(i>1){m_2covar_hi<-rbind(m_2covar_hi,mi)}
}

for (i in 1:length(vars_2covar[1,])){
  vari<-vars_2covar[,i]
  daterr<-data_forstats %>% filter(snowyrlo==1)
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  daterr$var2<-(daterr[,which(names(daterr)==vari[2])])
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_2covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ var1+var2, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_2covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"var1",vari[1])
  mi$term<-str_replace(mi$term,"var2",vari[2])
  mi$term<-str_replace(mi$term,"var1","")  
  if(i==1){m_2covar_lo<-mi}
  if(i>1){m_2covar_lo<-rbind(m_2covar_lo,mi)}
}
m_2covar_mid$snowyr<-"mid"
m_2covar_hi$snowyr<-"hi"
m_2covar_lo$snowyr<-"lo"

m_2covar_mid$nparam<-3
m_2covar_hi$nparam<-3
m_2covar_lo$nparam<-3

m_2covar_mid$nobs<-170
m_2covar_hi$nobs<-21
m_2covar_lo$nobs<-21

m_2covar_mid<-m_2covar_mid %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
m_2covar_hi<-m_2covar_hi %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
m_2covar_lo<-m_2covar_lo %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))

m_2covar<-rbind(m_2covar_mid,m_2covar_hi,m_2covar_lo) %>% select(snowyr,variable,model,term,val=estimate,std.error,pval=p.value,AICc)
#m_2covar<-rbind(m_2covar_mid,m_2covar_hi,m_2covar_lo) %>% select(snowyr,variable,model,term,val=estimate,std.error,pval=p.value,AICc)
m_2covar_minAICc<-m_2covar %>% group_by(variable,snowyr) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_2covar<-merge(m_2covar,m_1covar_minAICc,by=c("variable","snowyr"))
m_2covar$AICcdiff<-m_2covar$AICc-m_2covar$minAICc

m_2covar <-m_2covar %>% mutate_if(is.numeric, signif, digits=3) %>% filter(AICcdiff<=6) %>% arrange(variable,AICcdiff)
write.csv(m_2covar,"../figures/analysispowers/longstats_2covar.csv")

#m_2covar_slopes<-subset(m_2covar_slopes,!m_2covar_slopes$term %in% c("yr","(Intercept)"))


###########################


#dataplot<-bigjoin.stats %>% filter(year %in% c(2011,2015), variable %in% c("ProfTemp_top2m"))
dataplot<-data_forstats %>% filter((year==minYear_SWE_May | snowyrhi==1), variable %in% c("ProfTemp_top2m"))

dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"

plot_wtempSWE<-ggplot(dataplot, aes(x=SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo==1)) + 
  #  scale_x_log10() +
  xlab("May snow water equivalent-SWE (cm)")+
  ylab("Value")+
  theme_bw()+
  #  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))

plot_wtemp_flush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2.5,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo==1)) + 
  scale_x_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  #  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+
  ylab(paste0("Water temperature (", intToUtf8(176), "C), top 2m"))


png(file="../figures/analysispowers/wtemp_snow.png",width=6,height=2.8,units="in",res=600)
grid.arrange(plot_wtempSWE,plot_wtemp_flush,ncol=2)
dev.off()

dataplot<-unique(subset(bigjoin.stats,bigjoin.stats$variable=="SWE_May") %>% select(park_code,Snowsite,year,value))
SWE_timeseries <- 
  ggplot(dataplot,aes(x = year, y = value, color = park_code,group=Snowsite,label=Snowsite)) +
  geom_point()+#aes(x = year, y = value, color = park_code,group=Snowsite)) +
  geom_line()+#aes(x = year, y = value, color = park_code,group=Snowsite)) +
  geom_text_repel(size=4,segment.color="black",segment.size = 0.5,box.padding=0.5,#alpha=1,
                  data=subset(dataplot,dataplot$year==2018)) + 
  #  facet_grid(rows = vars(Snowsite)) +
  ylab("May snowpack - SWE (cm)")+
  xlab("Year") +
  #  xlim(2011,2019)+
  theme_bw() +
  scale_color_discrete(name = "Park")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=c(2011:2019),limits=c(2011,2019))

png(file="../figures/analysispowers/swe_timeseries.png",width=6,height=2.8,units="in",res=300)
SWE_timeseries
dev.off()



#dataplot<-bigjoin.stats %>% filter(year %in% c(2011,2015), variable %in% c("DO_top2m","Chlorophyll", #"Chlorophyll_log10", 
#                                                                     "secchi_value_m" , "pH_top2m"))
dataplot<-data_forstats %>% filter((snowyrlo==1 | snowyrhi==1), variable %in% c("DO_top2m","Chlorophyll", #"Chlorophyll_log10", 
                                                                           "secchi_value_m" , "pH_top2m"))

dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"
#dataplot$value[which(dataplot$short_code=="TR" & dataplot$snowyrlo==1 & 
#                       dataplot$variable=="Chlorophyll")]<-2.5 
#dataplot$short_code[which(dataplot$short_code=="TR" &  
#                            dataplot$variable=="Chlorophyll")]<-"TR*"

plot_limnoflush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo==1)) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))

png(file="../figures/analysispowers/limnoflush.png",width=4.5,height=4,units="in",res=600)
plot_limnoflush
dev.off()

#dataplot<-bigjoin.stats %>% filter(year %in% c(2011,2015), variable %in% c("Na", "Mg", "SO4","Total N"))
dataplot<-data_forstats %>% filter((year==minYear_SWE_May | snowyrhi==1), variable %in% c("Cl", "Mg", "SO4","Total N"))

dataplot$xaxis_label<-"Log10 Catchment snow vol : lake vol"
#dataplot$value[which(dataplot$short_code=="EA" & dataplot$year==2011 & 
#                       dataplot$variable=="SO4")]<-8.5 
#dataplot$short_code[which(dataplot$short_code=="EA" &  
#                            dataplot$variable=="SO4")]<-"EA*"

plot_chemsflush<-ggplot(dataplot, aes(x=flush_index_SWE_May, y=value, group="black", color=(park_code),label=short_code)) +
  geom_line(aes(group=short_code),alpha=0.5)+
  geom_text_repel(size=2,segment.color="black",alpha=1,segment.size = 0.2,#segment.size = 2,box.padding=0.1,alpha=1,
                  data=subset(dataplot,dataplot$snowyrlo==1)) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab(dataplot$xaxis_label)+
  ylab("Value")+
  theme_bw()+
  facet_wrap(~axis_label,ncol=2,scales="free_y") +
  theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))


png(file="../figures/analysispowers/chemsflush.png",width=4.5,height=4,units="in",res=600)
plot_chemsflush
dev.off()

############################### 


crosscorr1<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$snowyrhi==1) %>% select(-variable,-year,-short_code,-park_code,-snowyrlo,-snowyrhi,-snowyrmid,-value,-BlueLineInlet,-BlueLineOutlet,-ice_out_doy,-hydro,-park_siteshort) %>% as.data.frame() #-ice_out_doy,
crosscorr2<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$year==data_forstats$minYear_SWE_May) %>% select(park_site,SWE_May,flush_index_SWE_May) %>% as.data.frame()
crosscorr<-merge(crosscorr1,crosscorr2, by=c("park_site"),suffixes=c(".hi",".lo")) %>% select(-park_site,-axis_label,-minYear_SWE_May,-maxYear_SWE_May) 
crosscorr<-crosscorr %>% dplyr::rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio,flush_index.hi=flush_index_SWE_May.hi,flush_index.lo=flush_index_SWE_May.lo)
#crosscorr$hydro[which(crosscorr$hydro=="iso")]<-0
#crosscorr$hydro[which(crosscorr$hydro=="out")]<-1
#crosscorr$hydro[which(crosscorr$hydro=="io")]<-2
#crosscorr$hydro<-as.numeric(crosscorr$hydro)
crosscorr.m<-as.matrix(crosscorr)

res1 <- cor.mtest(crosscorr, conf.level = 0.95)

corrs_physio<-cor(crosscorr.m)
png(filename = "../figures/analysispowers/corrplot_landscape.png", width = 6, height = 6, units = "in",res=300)
corrplot_physio<-corrplot(cor(crosscorr.m), method = "ellipse", tl.col = "black", p.mat = res1$p, sig.level = 0.1,insig="blank",order = "AOE") #order = "FPC")
dev.off()

crosscorr_hiy<-subset(data_forstats,data_forstats$snowyrhi==1) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)
#prep_crosscorr_loy<-subset(data_forstats,data_forstats$year==data_forstats$minYear_SWE_May) %>% select(park_site,variable,value)
crosscorr_loy<-subset(data_forstats,data_forstats$year==data_forstats$minYear_SWE_May) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)

crosscorr_allyr<-data_forstats %>% group_by(park_site,variable) %>% dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% spread(variable,value) %>% as.data.frame() %>% select(-park_site)

for(i in 1:ncol(crosscorr_loy)){
  crosscorr_loy[is.na(crosscorr_loy[,i]), i] <- mean(crosscorr_loy[,i], na.rm = TRUE)
}

crosscorr_hiy$secchi_value_m[is.na(crosscorr_hiy$secchi_value_m)]<-mean(crosscorr_hiy$secchi_value_m,na.rm=TRUE)
crosscorr_loy$secchi_value_m[is.na(crosscorr_loy$secchi_value_m)]<-mean(crosscorr_loy$secchi_value_m,na.rm=TRUE)

crosscorr_hiy.m<-as.matrix(crosscorr_hiy)
crosscorr_loy.m<-as.matrix(crosscorr_loy)
crosscorr_allyr.m<-as.matrix(crosscorr_allyr)

corrs_hiy<-cor(crosscorr_hiy.m)
corrs_loy<-cor(crosscorr_loy.m)
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