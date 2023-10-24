
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

short_codes <- tribble(
  ~site_code, ~short_code,
  "LH14",     "PA",
  "LH15",     "15",
  "LN03",     "AL",
  "LP19",     "19",
  "LW32",     "DW",
  "LZ35",     "BL",
  "LS-07-01",     "LB",
  "MA-03-01",     "SI",
  "MC-03-01",     "ER",
  "MC-14-02",     "EA",
  "MR-12-01",     "BO",
  "SM-02-02",     "TR",
  "106",     "GL",
  "138",     "FE",
  "263",     "HE",
  "426",     "CR",
  "498",     "MI",
  "520",     "LC",
  "623",     "SU",
  "627",     "CO",
  "Hoh",     "HO"
)

#find minimum nonzero value
min_nonzero<-function(x){
  x_sort<-sort(unique(x))
  x_sort2<-x_sort[which(x_sort!=0)]
  xmin<-min(x_sort2)
  return(xmin)
}

bigjoin.stats <- full_join(x = bigjoin, y = short_codes, by = c("site_code"))

bigjoin.stats$hydro<-""
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T")]<-"in"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineOutlet=="T")]<-"out"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="T" & bigjoin.stats$BlueLineOutlet=="T")]<-"io"
bigjoin.stats$hydro[which(bigjoin.stats$BlueLineInlet=="F" & bigjoin.stats$BlueLineOutlet=="F")]<-"iso"
bigjoin.stats$yr<-0
bigjoin.stats$yr[which(bigjoin.stats$year==2015)]<-1
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
                                  "DO_top2m", #"DO_below2m",
                                  "Chlorophyll", 
                                  "secchi_value_m" , "pH_top2m",
                                  #                                "pH_below2m",
                                  "SpCond_top2m",
                                  "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>%
  arrange(-CV) %>% as.data.frame()

######################################
# sites table

sitestable0<-bigjoin.stats %>% select(Park,Lake,Elevation_m,Aspect,Watershed_area_ha,Surface_area_ha,Volume_m3,Depth_max,Depth_mean_m,Shoreline_length_m,Watershed_area_to_lake_area_ratio,hydro,Snowsite)
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

sitestable<-sitestable %>% dplyr::rename(Elev_m=Elevation_m,Depthmax_m=Depth_max,Depthmean_m=Depth_mean_m,Shorelength_m=Shoreline_length_m,SurfArea_ha=Surface_area_ha,ShedArea_ha=Watershed_area_ha,Vol_m3=Volume_m3,ShedAreaLakeArea_ratio=Watershed_area_to_lake_area_ratio)

write.csv(sitestable,"../figures/analysispowers/sitestable.csv")

###################################

data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("ProfTemp_top2m",
                                "DO_top2m", #"DO_below2m",
                                "Chlorophyll", 
                                "secchi_value_m" , "pH_top2m",
                                #                                "pH_below2m",
                                "SpCond_top2m",
                                "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>% #,"SurfTemp_8","MidTemp_8","BotTemp_8")) %>%
  select(variable,year,yr,short_code,park_code,park_site,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,Asp,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro,salmo_adult_ever) %>%
  unique() %>% as.data.frame()

by_variable <- group_by(data_forstats, variable)

vars<-c("yr","Depth_mean_m","Volume_m3","Watershed_area_ha","Elevation_m",
        "Watershed_area_to_lake_area_ratio","Asp","SWE_May",
        "flush_index_SWE_May","park_code","salmo_adult_ever")
combos<-combn(vars[-which(vars %in% c("yr","park_code"))],2)
# drop combo that causes singularity in backsolve, and/or are pseudo-correlated
#combos<-combos[,-which(combos[1,]=="Asp" & combos[2,]=="park_code")]
combos<-combos[,-which(combos[1,]=="SWE_May" & combos[2,]=="flush_index_SWE_May")]


for (i in 1:length(vars)){
  vari<-vars[i]
  daterr<-data_forstats
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  if(vari[1] %in% c("yr","flush_index_SWE_May","SWE_May")){daterr$yr1<-1}
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ var1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i)
  mi_glance<-do(by_variablei, glance(lme(value ~ var1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term[which(mi$term=="var1")]<-vari[1]
  mi$term<-str_replace(mi$term,"var1","")
  if(i==1){m_single<-mi}
  if(i>1){m_single<-rbind(m_single,mi)}
}
m_single_intonly<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ 1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=0)
m_single_intonly_glance<-do(by_variablei, glance(lme(value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_single_intonly<-merge(m_single_intonly,m_single_intonly_glance,by=c("variable","model"))
m_single<-rbind(m_single,m_single_intonly)

for (i in 1:length(combos[1,])){
  vari<-combos[,i]
  daterr<-data_forstats
  daterr$var1<-(daterr[,which(names(daterr)==vari[1])])
  daterr$var2<-(daterr[,which(names(daterr)==vari[2])])
  daterr$yr1<-daterr$yr
  daterr$yr2<-daterr$yr
  if(vari[1] %in% c("yr","flush_index_SWE_May","SWE_May")){daterr$yr1<-1}
  if(vari[2] %in% c("yr","flush_index_SWE_May","SWE_May")){daterr$yr2<-1}
  by_variablei <- group_by(daterr, variable)
  mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ yr1:var1+yr2:var2, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars))
  mi_glance<-do(by_variablei, glance(lme(value ~ yr1:var1+yr2:var2, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars)) %>% select(variable,sigma,logLik,AIC,BIC,model)
  mi<-merge(mi,mi_glance,by=c("variable","model"))
  mi$term<-str_replace(mi$term,"yr1:var1",vari[1])
  mi$term<-str_replace(mi$term,"yr2:var2",vari[2])
  mi$term<-str_replace(mi$term,"var1","")  
  if(i==1){m_combo<-mi}
  if(i>1){m_combo<-rbind(m_combo,mi)}
}
#m_combo_intonly<-by_variablei %>% group_by(variable) %>% do(tidy(lme(value ~ 1, random = ~ 1|park_site,  method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=0)
#m_combo_intonly_glance<-do(by_variablei, glance(lme(value ~ 1, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=0) %>% select(variable,sigma,logLik,AIC,BIC,model)
#m_combo_intonly<-merge(m_combo_intonly,m_combo_intonly_glance,by=c("variable","model"))
#m_combo<-rbind(m_combo,m_combo_intonly)


m_single$nparam<-2
m_combo$nparam<-3
m_single$nparam[which(m_single$model==0)]<-1

m_single$nobs<-21
m_combo$nobs<-21

m_single$AICc<-(-2*m_single$logLik)+(2*m_single$nparam)+(2*m_single$nparam*(m_single$nparam+1)/(m_single$nobs-m_single$nparam-1))
m_combo$AICc<-(-2*m_combo$logLik)+(2*m_combo$nparam)+(2*m_combo$nparam*(m_combo$nparam+1)/(m_combo$nobs-m_combo$nparam-1))

m_single_minAICc<-m_single %>% group_by(variable) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_combo_minAICc<-m_combo %>% group_by(variable) %>% 
  dplyr::summarize(minAICc=min(AICc))
m_single<-merge(m_single,m_single_minAICc,by.x=c("variable"),by.y=c("variable"))
m_single$AICcdiff<-m_single$AICc-m_single$minAICc
m_combo<-merge(m_combo,m_combo_minAICc,by.x=c("variable"),by.y=c("variable"))
m_combo$AICcdiff<-m_combo$AICc-m_combo$minAICc

stats<-rbind(m_single,m_combo)
stats<-stats %>% arrange(variable,AICc)
stats<-stats %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)
m_single<-m_single %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)

m_single_topmodels<-m_single %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()
stats_topmodels<-stats %>% filter(AICcdiff<=2) %>% arrange(variable,AICc,term) %>% as.data.frame()

stats_topmodels_int<-subset(stats_topmodels,stats_topmodels$term=="(Intercept)") %>% select(variable,model,term,val,se,pval,nparam,AICc,AICcdiff)
stats_topmodels_slope<-subset(stats_topmodels,stats_topmodels$term!="(Intercept)") %>% select(variable,model,term,val,se,pval,nparam,AICc,AICcdiff)
stats_topmodels_out<-merge(stats_topmodels_int,stats_topmodels_slope,by=c("variable","model","nparam","AICc","AICcdiff"),suffixes=c(".int",".slope")) %>% 
  select(variable,model,nparam,val.int,se.int,pval.int,predictor=term.slope,val.slope,se.slope,pval.slope,AICc,AICcdiff)


stats_topmodels_out<-stats_topmodels %>% select(-minAICc) %>% arrange(variable,nparam,AICc,model,term)

# any models equivalent to the simple intercept model ? (model=0)
m_single_topmodels %>% filter(model==0)

m_single_topmodels %>% filter(variable=="ProfTemp_top2m")
m_single_topmodels %>% filter(model==0)

stats_topmodels %>% filter(model==0)


# number of tested params, for calculating bonferoni corrected p-values
nparam<-length(unique(stats$term))-1

#stats$interactTF<-0
#stats$interactTF[grep(":",stats$term)]<-1
stats<-stats[order(stats$variable,stats$model),]
stats$sig<-""
stats$sig[which(stats$pval<=0.05/nparam)]<-"*"
stats$sig[which(stats$pval<=0.01/nparam)]<-"**"
stats$sig[which(stats$pval<=0.005/nparam)]<-"***"

stats_summ<-stats %>% select(variable,sigma,logLik,AICc) %>% as.data.frame()
longstats_out<-unique(m_single %>% arrange(variable,AICc,model,term))

# discard all intercept values in reporting, except the intercept of model = 1
# for each variable, these intercepts were generally very similar across models
stats_mainint<-subset(stats,stats$term!="(Intercept)"|(stats$term=="(Intercept)" & stats$model==1))
stats_nontemp<-subset(stats,stats$variable!="ProfTemp_top2m" & stats$model==1 & stats$term %in% c("(Intercept)","yr") | 
                        stats$variable!="ProfTemp_top2m" & stats$model==0 & stats$term %in% c("(Intercept)","yr"))

stats_nontemp_int<-subset(stats_nontemp,stats_nontemp$term=="(Intercept)" & stats_nontemp$model==1)
stats_nontemp_slope<-subset(stats_nontemp,stats_nontemp$term=="yr")
stats_nontemp_out<-merge(stats_nontemp_int,stats_nontemp_slope,by=c("variable","model"),suffixes=c(".int",".slope"))

stats_out<-stats_mainint %>% select(variable,model,term,val,se,sig,sigma,logLik,AICc,nparam) %>% as.data.frame()
#stats_temper_out<-subset(stats_out,stats_out$variable=="ProfTemp_top2m" & stats_out$term!="salmo_adult_ever")
stats_temper_out<-stats_out %>% filter(variable=="ProfTemp_top2m" & term!="salmo_adult_ever" & nparam<=2)

stats_nontemper<-subset(stats_out,stats_out$variable!="ProfTemp_top2m" & stats_out$term %in% c("(Intercept)","yr"))
stats_nontemper_int<-subset(stats_nontemper,stats_nontemper$term=="(Intercept)")
stats_nontemper_slope<-subset(stats_nontemper,stats_nontemper$term=="yr")
stats_nontemper_out<-merge(stats_nontemper_int %>% select(-term,-model,-sigma,-logLik,-AICc),stats_nontemper_slope %>% select(-term,-model,-sigma,-logLik,-AICc),by=c("variable"),suffixes=c(".int",".slope"))

longstats_slopes<-subset(longstats_out,!longstats_out$term %in% c("yr","(Intercept)"))
longstats_slopes_sig<-subset(longstats_slopes,longstats_slopes$pval<=0.05)
longstats_slopes_sig$dir<-"0"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val>0)]<-"+"
longstats_slopes_sig$dir[which(longstats_slopes_sig$val<=0)]<-"-"

longstats_slopes_sig_trunc<-longstats_slopes_sig %>% select(variable,term,val,dir)

widestats_out<-longstats_slopes_sig_trunc %>% 
  group_by(term,dir) %>% dplyr::summarise(vars = paste(variable, collapse=", ")) %>% as.data.frame()

crosscorr1<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$yr==0) %>% select(-variable,-year,-yr,-short_code,-park_code,-value,-ice_out_doy,-BlueLineInlet,-BlueLineOutlet) %>% as.data.frame()
crosscorr2<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m" & data_forstats$yr==1) %>% select(park_site,SWE_May,flush_index_SWE_May) %>% as.data.frame()
crosscorr<-merge(crosscorr1,crosscorr2, by=c("park_site"),suffixes=c(".2011",".2015")) %>% select(-park_site) 
crosscorr<-crosscorr %>% rename(WA_ha=Watershed_area_ha,Wala=Watershed_area_to_lake_area_ratio,flush_index.2011=flush_index_SWE_May.2011,flush_index.2015=flush_index_SWE_May.2015)
#crosscorr$Asp<-as.numeric(as.factor(crosscorr$Asp))-1
#crosscorr$BlueLineInlet<-as.numeric(as.factor(crosscorr$BlueLineInlet))-1
#crosscorr$BlueLineOutlet<-as.numeric(as.factor(crosscorr$BlueLineOutlet))-1
#crosscorr$hydro<-as.numeric(as.factor(crosscorr$hydro))-1
crosscorr$Asp[which(crosscorr$Asp=="Northy")]<-1
crosscorr$Asp[which(crosscorr$Asp=="EWSouthy")]<-0
crosscorr$hydro[which(crosscorr$hydro=="iso")]<-0
crosscorr$hydro[which(crosscorr$hydro=="out")]<-1
crosscorr$hydro[which(crosscorr$hydro=="io")]<-2
crosscorr$Asp<-as.numeric(crosscorr$Asp)
crosscorr$hydro<-as.numeric(crosscorr$hydro)
crosscorr.m<-as.matrix(crosscorr)
#row.names(crosscorr)<-crosscorr$park_site
#crosscorr<-crosscorr %>% select(-park_site) %>% as.matrix()

res1 <- cor.mtest(crosscorr, conf.level = 0.95)
#res2 <- cor.mtest(crosscorr, conf.level = 0.99)

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
write.csv(stats_nontemper_out,"../figures/analysispowers/nontemp_out.csv",row.names=FALSE)
write.csv(longstats_out,"../figures/analysispowers/longstats.csv",row.names=FALSE)
write.csv(widestats_out,"../figures/analysispowers/widestats.csv",row.names=FALSE)
write.csv(stats_topmodels_out,"../figures/analysispowers/topmodels_out.csv",row.names=FALSE)

######################################



