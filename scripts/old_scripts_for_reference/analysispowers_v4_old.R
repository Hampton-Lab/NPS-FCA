
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
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro) %>%
  unique() %>% as.data.frame()

by_variable <- group_by(data_forstats, variable)

m_yr<-by_variable %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
m_yr_glance<-do(by_variable, glance(lme(value ~ yr, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=1) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yr<-merge(m_yr,m_yr_glance,by=c("variable","model"))

m_yrdepth<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Depth_mean_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2)
m_yrdepth_glance<-do(by_variable, glance(lme(value ~ yr:Depth_mean_m, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=2) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrdepth<-merge(m_yrdepth,m_yrdepth_glance,by=c("variable","model"))

m_yrvol<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3)
m_yrvol_glance<-do(by_variable, glance(lme(value ~ yr:Volume_m3, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=3) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrvol<-merge(m_yrvol,m_yrvol_glance,by=c("variable","model"))

m_yrwa<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4)
m_yrwa_glance<-do(by_variable, glance(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=4) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrwa<-merge(m_yrwa,m_yrwa_glance,by=c("variable","model"))

m_yrelev<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5)
m_yrelev_glance<-do(by_variable, glance(lme(value ~ yr:Elevation_m, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=5) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrelev<-merge(m_yrelev,m_yrelev_glance,by=c("variable","model"))

m_yrwala<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6)
m_yrwala_glance<-do(by_variable, glance(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=6) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrwala<-merge(m_yrwala,m_yrwala_glance,by=c("variable","model"))

m_yrasp<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Asp, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7)
m_yrasp_glance<-do(by_variable, glance(lme(value ~ yr:Asp, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=7) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrasp<-merge(m_yrasp,m_yrasp_glance,by=c("variable","model"))

m_swe<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8)
m_swe_glance<-do(by_variable, glance(lme(value ~ SWE_May, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=8) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_swe<-merge(m_swe,m_swe_glance,by=c("variable","model"))

m_flush<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9)
m_flush_glance<-do(by_variable, glance(lme(value ~ flush_index_SWE_May, random = ~ 1|park_site, data = .))) %>% as.data.frame() %>% mutate(model=9) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_flush<-merge(m_flush,m_flush_glance,by=c("variable","model"))

m_yrpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:park_code, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=10)
m_yrpark_glance<-do(by_variable, glance(lme(value ~ yr:park_code, random = ~ 1|park_site,data = .))) %>% as.data.frame() %>% mutate(model=10) %>% select(variable,sigma,logLik,AIC,BIC,model)
m_yrpark<-merge(m_yrpark,m_yrpark_glance,by=c("variable","model"))

stats<-rbind(m_yr,m_yrdepth,m_yrvol,m_yrwa,m_yrelev,m_yrwala,m_yrasp,m_swe,m_flush,m_yrpark)

nparam<-length(unique(stats$term))-1

stats<-stats %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)
stats$interactTF<-0
stats$interactTF[grep(":",stats$term)]<-1
stats<-stats[order(stats$variable,stats$model),]
stats$sig<-""
stats$sig[which(stats$pval<=0.05/nparam)]<-"*"
stats$sig[which(stats$pval<=0.01/nparam)]<-"**"
stats$sig[which(stats$pval<=0.005/nparam)]<-"***"

stats_summ<-stats %>% select(variable,sigma,logLik,AIC) %>% as.data.frame()
longstats_out<-unique(stats)

stats_mainint<-subset(stats,stats$term!="(Intercept)"|(stats$term=="(Intercept)" & stats$model==1))
stats_nontemp<-subset(stats,stats$variable!="ProfTemp_top2m" & stats$model==1 & stats$term %in% c("(Intercept)","yr") | 
                        stats$variable!="ProfTemp_top2m" & stats$model==0 & stats$term %in% c("(Intercept)","yr"))

stats_nontemp_int<-subset(stats_nontemp,stats_nontemp$term=="(Intercept)" & stats_nontemp$model==1)
stats_nontemp_slope<-subset(stats_nontemp,stats_nontemp$term=="yr")
stats_nontemp_out<-merge(stats_nontemp_int,stats_nontemp_slope,by=c("variable","model"),suffixes=c(".int",".slope"))

#stats_temper<-subset(stats_mainint,stats_mainint$variable=="ProfTemp_top2m")
stats_out<-stats_mainint %>% select(variable,model,term,val,se,sig,sigma,logLik,AIC) %>% as.data.frame()
stats_temper_out<-subset(stats_out,stats_out$variable=="ProfTemp_top2m")

stats_nontemper<-subset(stats_out,stats_out$variable!="ProfTemp_top2m" & stats_out$term %in% c("(Intercept)","yr"))
stats_nontemper_int<-subset(stats_nontemper,stats_nontemper$term=="(Intercept)")
stats_nontemper_slope<-subset(stats_nontemper,stats_nontemper$term=="yr")
stats_nontemper_out<-merge(stats_nontemper_int %>% select(-term,-model,-sigma,-logLik,-AIC),stats_nontemper_slope %>% select(-term,-model,-sigma,-logLik,-AIC),by=c("variable"),suffixes=c(".int",".slope"))

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

######################################



