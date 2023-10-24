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
library(rpart)

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

#find minimum nonzero value
min_nonzero<-function(x){
  x_sort<-sort(unique(x))
  x_sort2<-x_sort[which(x_sort!=0)]
  xmin<-min(x_sort2)
  return(xmin)
}

bigjoin.stats <- full_join(x = bigjoin, y = short_codes, by = c("Lake"))
bigjoin.stats$park_siteshort<-paste(bigjoin.stats$park_code,"-",bigjoin.stats$short_code,sep="")

bigjoin.stats$hydro<-""
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
bigjoin.stats$inlet<-0
bigjoin.stats$inlet[which(bigjoin.stats$BlueLineInlet=="T")]<-1
bigjoin.stats$outlet<-0
bigjoin.stats$outlet[which(bigjoin.stats$BlueLineOutlet=="T")]<-1
#bigjoin.stats$Aspect2<-""
#bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"Northy"
#bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("South","Southeast","Southwest"))]<-"Southy"
#bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("East","West"))]<-"E-W"
#bigjoin.stats$Asp<-""
#bigjoin.stats$Asp[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"Northy"
#bigjoin.stats$Asp[which(!bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"EWSouthy"
bigjoin.stats$northface<-0
bigjoin.stats$northface[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-1
######################################
# summaries

bigjoin.stats.summ<-bigjoin.stats %>% group_by(variable,Lake) %>%
  dplyr::summarize(CV=abs(sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE)),
                   CV_SWE_May=abs(sd(SWE_May,na.rm=TRUE)/mean(SWE_May,na.rm=TRUE)),
                   corr=cor(value,SWE_May)) %>% as.data.frame()
#  dplyr::summarize(CV=mean(CV,na.rm=TRUE),CV_SWE_May=mean(CV_SWE_May,na.rm=TRUE),
#                   corr_value_SWE=mean(corr,na.rm=TRUE),ratio=mean(ratio,na.rm=TRUE))

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
  select(variable,year,short_code,park_code,park_site,park_siteshort,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,#Asp,
         solar_jas,solar_dec,
         barren,forest,#shrub,meadow,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro,
         salmo_adult_ever,
         northface,inlet,outlet) %>%
  unique() %>% as.data.frame()



# hehe

# [1] takes the first year of minimum, if two years had the same minimum (zero!)
# this omits min/max swe years that lacked temp profile data, aka the lake was not / could not be visited

minyears<-data_forstats %>% filter(variable=="ProfTemp_top2m",value>-99) %>% 
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
m_0covar<-rbind(m_0covar,m_0covar_intonly)

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


longstats_slopes<-subset(m_1covar,!m_1covar$term %in% c("yr","(Intercept)"))
longstats_slopes_sig<-subset(longstats_slopes,longstats_slopes$pval<=0.05)
longstats_slopes_sig$dir<-"0"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val>0)]<-"+"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val<=0)]<-"-"

#longstats_slopes_sig_trunc<-longstats_slopes_sig %>% select(variable,term,val,dir)
#longstats_slopes_sig_trunc$snowyr<-substr(longstats_slopes_sig_trunc$term,7,8)
#longstats_slopes_sig_trunc$term<-substr(longstats_slopes_sig_trunc$term,10,nchar(longstats_slopes_sig_trunc$term))


widestats_snowyrhi<-longstats_slopes_sig %>% filter(snowyr=="hi") %>%
  group_by(term,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()
widestats_snowyrlo<-longstats_slopes_sig %>% filter(snowyr=="lo") %>%
  group_by(term,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()
widestats_snowyrmid<-longstats_slopes_sig %>% filter(snowyr=="mid") %>%
  group_by(term,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()

widestats_out<-merge(widestats_snowyrlo,widestats_snowyrhi,by=c("term","dir"),all=TRUE,suffixes=c("_lo","_hi"))
widestats_out<-merge(widestats_out,widestats_snowyrmid,by=c("term","dir"),all=TRUE)

widestats_out


m_1covar_topmodels<-m_1covar %>% filter(AICcdiff<=6,term!="(Intercept)") %>% 
  select(-AICc,-minAICc) %>% arrange(variable,snowyr,AICcdiff,term) %>% as.data.frame()







# no need to report duplicate rows
#widestats_out$vars_lo[which(widestats_out %>% select(term,snowyr_lo,dir_lo,vars_lo) %>% duplicated()==TRUE)]<-""
#widestats_out$vars_hi[which(widestats_out %>% select(term,snowyr_hi,dir_hi,vars_hi) %>% duplicated()==TRUE)]<-""
widestats_out$vars_lo[which(widestats_out %>% select(term,dir_lo,vars_lo) %>% duplicated()==TRUE)]<-""
widestats_out$vars_hi[which(widestats_out %>% select(term,dir_hi,vars_hi) %>% duplicated()==TRUE)]<-""
widestats_out<-unique(widestats_out %>% select(-snowyr_lo,-snowyr_hi))



for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-data_forstats
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ snowyrlo:vari+snowyrhi:vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ snowyrlo:vari+snowyrhi:vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar<-mi}
  if(i>1){m_1covar<-rbind(m_1covar,mi)}
}

for (i in 1:length(vars_2covar[1,])){
  vari<-vars_2covar[,i]
  daterr<-data_forstats
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  daterr$var2<-(daterr[,which(names(daterr)==vari[2])])
  if(vari[1] %in% c("snowyrlo")){daterr$snowyrlo<-1}
  if(vari[1] %in% c("snowyrhi")){daterr$snowyrhi<-1}
  if(vari[1] %in% c("snowyrmid")){daterr$snowyrmid<-1}
  if(vari[2] %in% c("snowyrlo")){daterr$snowyrlo<-1}
  if(vari[2] %in% c("snowyrhi")){daterr$snowyrhi<-1}
  if(vari[2] %in% c("snowyrmid")){daterr$snowyrmid<-1}
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ snowyrlo:var1+snowyrhi:var1+snowyrlo:var2+snowyrhi:var2, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_2covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ snowyrlo:var1+snowyrhi:var1+snowyrlo:var2+snowyrhi:var2, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_2covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"var1",vari[1])
  mi$term<-str_replace(mi$term,"var2",vari[2])
  mi$term<-str_replace(mi$term,"var1","")  
  if(i==1){m_2covar<-mi}
  if(i>1){m_2covar<-rbind(m_2covar,mi)}
}

m_1covar$nparam<-3
m_0covar$nparam<-3
m_2covar$nparam<-5
m_0covar$nparam[which(m_0covar$model==0)]<-1

m_0covar$nobs<-21
m_1covar$nobs<-21
m_2covar$nobs<-21

m_0covar$AICc<-(-2*m_0covar$logLik)+(2*m_0covar$nparam)+(2*m_0covar$nparam*(m_0covar$nparam+1)/(m_0covar$nobs-m_0covar$nparam-1))
m_1covar$AICc<-(-2*m_1covar$logLik)+(2*m_1covar$nparam)+(2*m_1covar$nparam*(m_1covar$nparam+1)/(m_1covar$nobs-m_1covar$nparam-1))
m_2covar$AICc<-(-2*m_2covar$logLik)+(2*m_2covar$nparam)+(2*m_2covar$nparam*(m_2covar$nparam+1)/(m_2covar$nobs-m_2covar$nparam-1))

# list model terms in consistent order
m_1covar$term[grep(":snowyrhi",m_1covar$term)]<-
  paste("snowyrhi:",substr(m_1covar$term[grep(":snowyrhi",m_1covar$term)],1,-9+nchar(m_1covar$term[grep(":snowyrhi",m_1covar$term)])),sep="")
m_1covar$term[grep(":snowyrlo",m_1covar$term)]<-
  paste("snowyrlo:",substr(m_1covar$term[grep(":snowyrlo",m_1covar$term)],1,-9+nchar(m_1covar$term[grep(":snowyrlo",m_1covar$term)])),sep="")

m_2covar$term[grep(":snowyrhi",m_2covar$term)]<-
  paste("snowyrhi:",substr(m_2covar$term[grep(":snowyrhi",m_2covar$term)],1,-9+nchar(m_2covar$term[grep(":snowyrhi",m_2covar$term)])),sep="")
m_2covar$term[grep(":snowyrlo",m_2covar$term)]<-
  paste("snowyrlo:",substr(m_2covar$term[grep(":snowyrlo",m_2covar$term)],1,-9+nchar(m_2covar$term[grep(":snowyrlo",m_2covar$term)])),sep="")



#m_1covar
#m_1covar_minAICc<-m_1covar %>% group_by(variable) %>% 
#  dplyr::summarize(minAICc=min(AICc))
#m_1covar<-merge(m_1covar,m_1covar_minAICc,by.x=c("variable"),by.y=c("variable"))
#m_1covar$AICcdiff<-m_1covar$AICc-m_1covar$minAICc
#m_1covar_topmodels<-m_1covar %>% filter(AICcdiff<=6) %>% arrange(variable,AICc,term) %>% as.data.frame()


stats<-rbind(m_1covar,m_0covar,m_2covar)

stats_minAICc<-stats %>% group_by(variable) %>% 
  dplyr::summarize(minAICc=min(AICc))
stats<-merge(stats,stats_minAICc,by.x=c("variable"),by.y=c("variable"))
stats$AICcdiff<-stats$AICc-stats$minAICc

stats<-stats %>% arrange(variable,AICc)
stats<-stats %>% mutate_if(is.numeric, signif,digits=3) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value) %>% mutate(AICc=round(AICc,0),minAICc=round(minAICc,0),AICcdiff=round(AICcdiff,0))
m_1covar<-m_1covar %>% mutate_if(is.numeric, signif,digits=3) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)



#m_0covar_topmodels<-m_0covar %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()
stats_topmodels<-stats %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()

stats_topmodels_out<-stats_topmodels %>% select(-minAICc) %>% arrange(variable,nparam,AICc,model,term)

# any models equivalent to the simple intercept model ? (model=0)
#m_0covar_topmodels %>% filter(model==0)
#m_0covar_topmodels %>% filter(variable=="ProfTemp_top2m")
stats_topmodels %>% filter(model==0)

# number of tested params, for calculating bonferoni corrected p-values
nparam<-length(unique(stats$term))-1

stats<-stats[order(stats$variable,stats$model),]
stats$sig<-""
stats$sig[which(stats$pval<=0.05/nparam)]<-"*"
stats$sig[which(stats$pval<=0.01/nparam)]<-"**"
stats$sig[which(stats$pval<=0.005/nparam)]<-"***"

stats_summ<-stats %>% select(variable,sigma,logLik,AICc) %>% as.data.frame()
longstats_out<-unique(m_1covar %>% arrange(variable,AICc,model,term))

# discard all intercept values in reporting, except the intercept of model = 1
# for each variable, these intercepts were generally very similar across models
stats_mainint<-subset(stats,stats$term!="(Intercept)"|(stats$term=="(Intercept)" & stats$model==1))
stats_allvar<-subset(stats,stats$model==1) %>% arrange(variable,AICc,model,term)

stats_out<-stats_mainint %>% select(variable,model,term,val,se,sig,sigma,logLik,AICc,nparam) %>% as.data.frame()
stats_temper_out<-stats_out %>% filter(variable=="ProfTemp_top2m" & term!="salmo_adult_ever" & nparam<=3)

stats_allvar<-subset(stats_out,stats_out$model==1)
stats_allvar_int<-subset(stats_allvar,stats_allvar$term=="(Intercept)") %>% select(-term,-sigma,-logLik,-AICc,-nparam) %>% dplyr::rename(val.int=val,se.int=se,sig.int=sig)
stats_allvar_slope_snowyrlo<-subset(stats_allvar,stats_allvar$term=="snowyrlo") %>% select(-term,-model,-sigma,-logLik,-AICc,-nparam)# %>% dplyr::rename(val.lo=val,se.lo=se)
stats_allvar_slope_snowyrhi<-subset(stats_allvar,stats_allvar$term=="snowyrhi") %>% select(-term,-model,-sigma,-logLik,-AICc,-nparam)# %>% dplyr::rename(val.lo=val,se.lo=se)
stats_allvar_slope<-merge(stats_allvar_slope_snowyrlo,stats_allvar_slope_snowyrhi,by=c("variable"),suffixes=c(".lo",".hi"))
stats_allvar_out<- merge(stats_allvar_int,stats_allvar_slope,by=c("variable"),suffixes=c(".int",""))

longstats_slopes<-subset(longstats_out,!longstats_out$term %in% c("yr","(Intercept)"))
longstats_slopes_sig<-subset(longstats_slopes,longstats_slopes$pval<=0.05)
longstats_slopes_sig$dir<-"0"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val>0)]<-"+"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val<=0)]<-"-"

longstats_slopes_sig_trunc<-longstats_slopes_sig %>% select(variable,term,val,dir)
longstats_slopes_sig_trunc$snowyr<-substr(longstats_slopes_sig_trunc$term,7,8)
longstats_slopes_sig_trunc$term<-substr(longstats_slopes_sig_trunc$term,10,nchar(longstats_slopes_sig_trunc$term))

widestats_snowyrhi<-longstats_slopes_sig_trunc %>% filter(snowyr=="hi") %>%
  group_by(term,snowyr,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()
widestats_snowyrlo<-longstats_slopes_sig_trunc %>% filter(snowyr=="lo") %>%
  group_by(term,snowyr,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()
widestats_out<-merge(widestats_snowyrlo,widestats_snowyrhi,by=c("term","dir"),all=TRUE,suffixes=c("_lo","_hi"))


# no need to report duplicate rows
#widestats_out$vars_lo[which(widestats_out %>% select(term,snowyr_lo,dir_lo,vars_lo) %>% duplicated()==TRUE)]<-""
#widestats_out$vars_hi[which(widestats_out %>% select(term,snowyr_hi,dir_hi,vars_hi) %>% duplicated()==TRUE)]<-""
widestats_out$vars_lo[which(widestats_out %>% select(term,dir_lo,vars_lo) %>% duplicated()==TRUE)]<-""
widestats_out$vars_hi[which(widestats_out %>% select(term,dir_hi,vars_hi) %>% duplicated()==TRUE)]<-""
widestats_out<-unique(widestats_out %>% select(-snowyr_lo,-snowyr_hi))



#widestats_out<-longstats_slopes_sig_trunc %>% 
#  group_by(term,dir,snowyr) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()

crosscorr1<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$year==2011) %>% select(-variable,-year,-short_code,-park_code,-snowyrlo,-snowyrhi,-snowyrmid,-value,-BlueLineInlet,-BlueLineOutlet,-ice_out_doy,-hydro,-park_siteshort) %>% as.data.frame() #-ice_out_doy,
crosscorr2<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$year==2015) %>% select(park_site,SWE_May,flush_index_SWE_May) %>% as.data.frame()
crosscorr<-merge(crosscorr1,crosscorr2, by=c("park_site"),suffixes=c(".2011",".2015")) %>% select(-park_site) 
crosscorr<-crosscorr %>% dplyr::rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio,flush_index.2011=flush_index_SWE_May.2011,flush_index.2015=flush_index_SWE_May.2015)
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

crosscorr_2011y<-subset(data_forstats,data_forstats$year==2011) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)
crosscorr_2015y<-subset(data_forstats,data_forstats$year==2015) %>% select(park_site,variable,value) %>% spread(variable,value) %>% select(-park_site)
crosscorr_2011y$secchi_value_m[is.na(crosscorr_2011y$secchi_value_m)]<-mean(crosscorr_2011y$secchi_value_m,na.rm=TRUE)
crosscorr_2015y$secchi_value_m[is.na(crosscorr_2015y$secchi_value_m)]<-mean(crosscorr_2015y$secchi_value_m,na.rm=TRUE)

crosscorr_2011y.m<-as.matrix(crosscorr_2011y)
crosscorr_2015y.m<-as.matrix(crosscorr_2015y)

corrs_2011y<-cor(crosscorr_2011y.m)
corrs_2015y<-cor(crosscorr_2015y.m)

res_2011y <- cor.mtest(crosscorr_2011y, conf.level = 0.95)
res_2015y <- cor.mtest(crosscorr_2015y, conf.level = 0.95)

png(filename = "../figures/analysispowers/corrplot2011y.png", width = 6, height = 6, units = "in",res=300)
corrplot2011y<-corrplot(cor(crosscorr_2011y.m), method = "ellipse", tl.col = "black", p.mat = res_2011y$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()
png(filename = "../figures/analysispowers/corrplot2015y.png", width = 6, height = 6, units = "in",res=300)
corrplot2015y<-corrplot(cor(crosscorr_2015y.m), method = "ellipse", tl.col = "black", p.mat = res_2015y$p, sig.level = 0.1,insig="blank",order = "original") #order = "FPC")
dev.off()


write.csv(stats_temper_out,"../figures/analysispowers/temper_out.csv",row.names=FALSE)
write.csv(stats_allvar_out,"../figures/analysispowers/allvar_out.csv",row.names=FALSE)
write.csv(longstats_out,"../figures/analysispowers/longstats.csv",row.names=FALSE)
write.csv(longstats_out %>% filter(pval<=0.05),"../figures/analysispowers/longstats_signif.csv",row.names=FALSE)
write.csv(widestats_out,"../figures/analysispowers/widestats.csv",row.names=FALSE)
write.csv(stats_topmodels_out,"../figures/analysispowers/topmodels_out.csv",row.names=FALSE)

######################################

midyears<-data_forstats %>% filter(snowyrmid==1)
hiyears<-data_forstats %>% filter(snowyrhi==1)
loyears<-data_forstats %>% filter(snowyrlo==1)

for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-midyears
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar<-mi}
  if(i>1){m_1covar<-rbind(m_1covar,mi)}
}
snowyearmid_mod<-m_1covar

for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-hiyears
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar<-mi}
  if(i>1){m_1covar<-rbind(m_1covar,mi)}
}
snowyearhi_mod<-m_1covar


for (i in 1:length(vars_1covar)){
  vari<-vars_1covar[i]
  daterr<-loyears
  daterr$vari<-(daterr[,which(names(daterr)==vari)])
  daterr$vari[which(!daterr$year %in% c(2011,2015))]<-replace_na(daterr$vari,mean(daterr$vari,na.rm=TRUE))[which(!daterr$year %in% c(2011,2015))]
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ vari, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars_1covar))
  mi_glance<-do(by_variablei, glance(lme(value ~ vari, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars_1covar)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"vari",vari)
  mi$term<-str_replace(mi$term,"vari","")  
  if(i==1){m_1covar<-mi}
  if(i>1){m_1covar<-rbind(m_1covar,mi)}
}
snowyearlo_mod<-m_1covar

snowyearmid_mod$snowyear<-"mid"
snowyearhi_mod$snowyear<-"hi"
snowyearlo_mod$snowyear<-"lo"
snowyearsmod<-rbind(snowyearmid_mod,snowyearhi_mod,snowyearlo_mod)

snowyearsmod<-snowyearsmod %>% arrange(variable,model,snowyear)


write.csv(snowyearsmod,"../figures/analysispowers/snowyearsmodels_out.csv",row.names=FALSE)
          

dataplot<-data_forstats


ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(SWE_May,value,color=park_code)) +
  geom_point() 

ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(SWE_May,value,color=Elevation_m)) +
  geom_point() +
  scale_color_viridis_c()+
  facet_wrap(~park_code)

ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(Elevation_m,value,color=SWE_May)) +
  geom_point() +
  scale_color_viridis_c()+
  facet_wrap(~park_code)

ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(SWE_May,value,color=(solar_jas))) +
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~park_code)


ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(SWE_May,value,color=((1/solar_jas)*Elevation_m))) +
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~park_code)

ggplot(dataplot %>% filter(variable=="ProfTemp_top2m"),aes(Elevation_m,solar_jas)) +
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~park_code)

mod<-glm(value~Depth_mean_m+Elevation_m+solar_jas+SWE_May+hydro+Watershed_area_to_lake_area_ratio+park_code+year+forest,data=dataplot %>% filter(variable=="ProfTemp_top2m"))
mod<-glm(value~Depth_mean_m+Elevation_m+solar_jas+SWE_May+hydro+flush_index_SWE_May+forest,data=dataplot %>% filter(variable=="ProfTemp_top2m"))

dataplot<-mod$data#,fitted.values=mod$fitted.values)

ggplot(dataplot, aes(x = value, y = fitted.values,color=hydro) ) +
  geom_point()+
  facet_wrap(~park_site)
  
+
  geom_line(aes(y = predlm), size = 1)

ggplot() +
  geom_point(data=dataplot,aes(SWE_May,value))+
  geom_line(data=dataplot,aes(x=SWE_May,y=fitted.values))+
#  geom_point(mod$data,aes(Elevation_m,fitted.values))+
  scale_color_viridis_c()+
  facet_wrap(~park_code)

ggplot(dataplot,aes(solar_jas,SWE_May)) +
  geom_point() 


ggplot(dataplot,aes(solar_jas,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(solar_dec,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(Elevation_m,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(forest,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(log10(flush_index_SWE_May),value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(northface,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(Watershed_area_ha,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(Watershed_area_to_lake_area_ratio,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(Volume_m3,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")

ggplot(dataplot,aes(hydro,value)) +
  geom_point() +
  facet_wrap(~variable,scales="free")





data_forstats$snowyr<-1
data_forstats$snowyr[which(data_forstats$snowyrhi==1)]<-2
data_forstats$snowyr[which(data_forstats$snowyrlo==1)]<-0

treedata<-data_forstats %>% filter(variable=="ProfTemp_top2m") %>% select(value,vars,snowyr)


fit <- rpart(value ~ .,
             method="class", data=treedata)
fit <- rpart(value ~ solar_jas+Elevation_m,
             method="class", data=treedata)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


model <- rpart(Species ~., data = iris)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)


model <- rpart(value ~., data = treedata)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)


model <- rpart(value ~., data = treedata %>% filter(snowyr==0))
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)


data_forstats %>% group_by(variable,snowyr) %>%
  dplyr::summarize(cv=sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE)) %>% data.frame()


data_spread<-data_forstats %>% spread(variable,value)
man<-manova(cbind(ProfTemp_top2m,DO_top2m,secchi_value_m+SWE_May)~snowyr,data=data_spread)
summary.aov(man)

Grazed.lda1 <- lda(snowyr ~ ProfTemp_top2m+DO_top2m+secchi_value_m+Cl+Mg+SO4+Ca+Chlorophyll+solar_jas+Elevation_m+forest, method="moment",data=data_spread)
Grazed.lda1 <- lda(snowyr ~ scale(ProfTemp_top2m)+scale(DO_top2m)+scale(secchi_value_m)+
                     scale(Cl)+scale(Mg)+scale(SO4)+scale(Ca)+scale(Chlorophyll)+
                     scale(solar_jas)+scale(Elevation_m)+scale(forest), method="moment",data=data_spread)
Grazed.lda1 <- lda(snowyr ~ scale(ProfTemp_top2m)+scale(DO_top2m)+scale(secchi_value_m)+
                     scale(Cl)+scale(Mg)+scale(SO4)+scale(Ca)+scale(Chlorophyll), method="moment",data=data_spread)

Grazed.lda1


fit <- lda(G ~ x1 + x2 + x3, data=mydata, 
           na.action="na.omit", CV=TRUE)
fit # show results


mod<-glm(snowyr ~ ProfTemp_top2m+DO_top2m+secchi_value_m+Cl,data=data_spread)
mod<-glm(factor(snowyr) ~ scale(ProfTemp_top2m)+scale(DO_top2m)+scale(secchi_value_m)+
           scale(Cl)+scale(Mg)+scale(SO4)+scale(Ca)+scale(Chlorophyll),data=data_spread)
mod<-glm(factor(snowyrhi)+factor(snowyrlo) ~ scale(ProfTemp_top2m)+scale(DO_top2m)+scale(secchi_value_m)+
           scale(Cl)+scale(Mg)+scale(SO4)+scale(Ca)+scale(Chlorophyll),data=data_spread)

summary(mod)


