
library(nlme) # needed for mixed model
library(broom)
library(tidyr)
library(tidyverse)
library(broom.mixed)
library(car)
library(reshape2)
library(corrplot)

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

bigjoin.stats$hydro<-""
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
bigjoin.stats$Aspect2<-""
bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"Northy"
bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("South","Southeast","Southwest"))]<-"Southy"
bigjoin.stats$Aspect2[which(bigjoin.stats$Aspect %in% c("East","West"))]<-"E-W"
bigjoin.stats$Asp<-""
bigjoin.stats$Asp[which(bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"Northy"
bigjoin.stats$Asp[which(!bigjoin.stats$Aspect %in% c("North","Northeast","Northwest"))]<-"EWSouthy"
######################################
# summaries

bigjoin.stats.summ<-bigjoin.stats %>% group_by(variable,Lake) %>%
  dplyr::summarize(CV=abs(sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE)),
                   CV_SWE_May=abs(sd(SWE_May,na.rm=TRUE)/mean(SWE_May,na.rm=TRUE)),
                                  corr=cor(value,SWE_May)) %>% as.data.frame()
  dplyr::summarize(CV=mean(CV,na.rm=TRUE),CV_SWE_May=mean(CV_SWE_May,na.rm=TRUE),
                   corr_value_SWE=mean(corr,na.rm=TRUE),ratio=mean(ratio,na.rm=TRUE))

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

sitestable0<-bigjoin.stats %>% select(Park,Lake,Elevation_m,Aspect,Watershed_area_ha,Surface_area_ha,Volume_m3,Depth_max,Depth_mean_m,Shoreline_length_m,Watershed_area_to_lake_area_ratio,solar_jas,snowice,forest,meadow,hydro,Snowsite)
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
                                         solarjas_Whrperm2=solar_jas)

write.csv(sitestable,"../figures/analysispowers/sitestable.csv")

###################################

data_forstats<- bigjoin.stats %>% #mutate(snowhi=year==2011,snowlo=year==2015,snowrest=!year %in% c(2011,2015)) %>% 
  dplyr::filter(#year %in% c(2011, 2015),
                variable %in% c("ProfTemp_top2m",
                                "DO_top2m", "DOsat_top2m",#"DO_below2m",
                                "Chlorophyll", 
                                "secchi_value_m" , "pH_top2m",
                                #                                "pH_below2m",
                                "SpCond_top2m",
                                "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>% #,"SurfTemp_8","MidTemp_8","BotTemp_8")) %>%
  select(variable,year,short_code,park_code,park_site,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,Asp,
         solar_jas,barren,forest,shrub,meadow,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro,salmo_adult_ever,) %>%
  unique() %>% as.data.frame()

data_forstats$snowyrlo<-0
data_forstats$snowyrlo[data_forstats$year==2015]<-1
data_forstats$snowyrmid<-0
data_forstats$snowyrmid[!data_forstats$year %in% c(2011,2015)]<-1
data_forstats$snowyrhi<-0
data_forstats$snowyrhi[data_forstats$year==2011]<-1

by_variable <- group_by(data_forstats, variable)

vars<-c("snowyrlo","snowyrhi","Depth_mean_m","Volume_m3","Watershed_area_ha","Elevation_m",
        "Watershed_area_to_lake_area_ratio","Asp","SWE_May",
        "flush_index_SWE_May","park_code","salmo_adult_ever",
        "solar_jas","barren","forest","shrub","meadow")
vars_1covar<-vars[-which(vars %in% c("snowyrlo","snowyrmid","snowyrhi"))] 
vars_2covar<-combn(vars[-which(vars %in% c("yr","park_code","snowyrlo","snowyrmid","snowyrhi","SWE_May","flush_index_SWE_May"))],2)
# drop combo that causes singularity in backsolve, and/or are pseudo-correlated
vars_2covar<-vars_2covar[,-which(vars_2covar[1,]=="Asp" & vars_2covar[2,]=="salmo_adult_ever")]

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

stats<-rbind(m_1covar,m_0covar,m_2covar)

stats_minAICc<-stats %>% group_by(variable) %>% 
  dplyr::summarize(minAICc=min(AICc))
stats<-merge(stats,stats_minAICc,by.x=c("variable"),by.y=c("variable"))
stats$AICcdiff<-stats$AICc-stats$minAICc

stats<-stats %>% arrange(variable,AICc)
stats<-stats %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)
m_1covar<-m_1covar %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)

m_0covar_topmodels<-m_0covar %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()
stats_topmodels<-stats %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()

stats_topmodels_out<-stats_topmodels %>% select(-minAICc) %>% arrange(variable,nparam,AICc,model,term)

# any models equivalent to the simple intercept model ? (model=0)
m_0covar_topmodels %>% filter(model==0)
m_0covar_topmodels %>% filter(variable=="ProfTemp_top2m")
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

widestats_out<-longstats_slopes_sig_trunc %>% 
  group_by(term,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()

crosscorr1<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$year==2011) %>% select(-variable,-year,-short_code,-park_code,-snowyrlo,-snowyrhi,-snowyrmid,-value,-ice_out_doy,-BlueLineInlet,-BlueLineOutlet) %>% as.data.frame()
crosscorr2<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$year==2015) %>% select(park_site,SWE_May,flush_index_SWE_May) %>% as.data.frame()
crosscorr<-merge(crosscorr1,crosscorr2, by=c("park_site"),suffixes=c(".2011",".2015")) %>% select(-park_site) 
crosscorr<-crosscorr %>% dplyr::rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio,flush_index.2011=flush_index_SWE_May.2011,flush_index.2015=flush_index_SWE_May.2015)
crosscorr$Asp[which(crosscorr$Asp=="Northy")]<-1
crosscorr$Asp[which(crosscorr$Asp=="EWSouthy")]<-0
crosscorr$hydro[which(crosscorr$hydro=="iso")]<-0
crosscorr$hydro[which(crosscorr$hydro=="out")]<-1
crosscorr$hydro[which(crosscorr$hydro=="io")]<-2
crosscorr$Asp<-as.numeric(crosscorr$Asp)
crosscorr$hydro<-as.numeric(crosscorr$hydro)
crosscorr.m<-as.matrix(crosscorr)

res1 <- cor.mtest(crosscorr, conf.level = 0.95)

corrs_physio<-cor(crosscorr.m)
png(filename = "../figures/analysispowers/corrplot_physio.png", width = 6, height = 6, units = "in",res=300)
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
write.csv(widestats_out,"../figures/analysispowers/widestats.csv",row.names=FALSE)
write.csv(stats_topmodels_out,"../figures/analysispowers/topmodels_out.csv",row.names=FALSE)

######################################



