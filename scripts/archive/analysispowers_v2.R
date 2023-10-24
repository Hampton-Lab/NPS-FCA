
library(nlme) # needed for mixed model
library(broom)
library(tidyr)
library(tidyverse)
library(broom.mixed)
library(car)
library(reshape2)

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

write.csv(sitestable,"sitestable.csv")

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

m_yr<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
m_yrdepth<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Depth_mean_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2)
m_yrvol<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3)
m_yrwa<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4)
m_yrelev<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5)
m_yrwala<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6)
m_yrasp<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Asp, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7)
m_swe<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8)
m_flush<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9)

m_yrpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:park_code, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
m_yrdepthpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Depth_mean_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2)
m_yrvolpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Volume_m3:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3)
m_yrwapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_ha:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4)
m_yrelevpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Elevation_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5)
m_yrwalapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_to_lake_area_ratio:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6)
m_yrasppark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Asp:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7)
m_swepark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8)
m_flushpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9)


stats1<-rbind(m_yr,m_yrdepth,m_yrvol,m_yrwa,m_yrelev,m_yrwala,m_yrasp,m_swe,m_flush)
#                       m_yrdepthpark,m_yrvolpark,m_yrwapark,m_yrelevpark,m_yrwalapark,m_yrasppark,m_swepark,m_flushpark)
stats2<-rbind(m_yrpark,m_yrdepthpark,m_yrvolpark,m_yrwapark,m_yrelevpark,m_yrwalapark,m_yrasppark,m_swepark,m_flushpark)
stats1$submodel<-0
stats2$submodel<-1

stats<-rbind(stats1,stats2)

stats<-stats %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)
stats$parkTF<-0
stats$parkTF[grep("MORA|NOCA|OLYM",stats$term)]<-1
stats$interactTF<-0
stats$interactTF[grep(":",stats$term)]<-1
stats$parkintTF<-0
stats$parkintTF[which(stats$parkTF==1 & stats$interactTF==0)]<-1
stats$parkslopeTF<-0
stats$parkslopeTF[which(stats$parkTF==1 & stats$interactTF==1)]<-1
stats<-stats[order(stats$variable,stats$model,stats$submodel,stats$parkslopeTF),]

stats_parkint<-subset(stats,stats$parkintTF==1)
stats_noparkint<-subset(stats,stats$parkintTF==0)
stats_noparkint_simpleint<-subset(stats_noparkint,!(stats_noparkint$term=="(Intercept)" & stats_noparkint$submodel==1))

stats_noparkint_simpleint_sig<-subset(stats_noparkint_simpleint,stats_noparkint_simpleint$parkslopeTF==0 | (stats_noparkint_simpleint$parkslopeTF==1 & stats_noparkint_simpleint$pval<=0.05))

stats_noparkint_simpleint

stats_main<-stats_noparkint_simpleint
stats_main_temper<-subset(stats_main,stats_main$variable=="ProfTemp_top2m")

stats_main_temper_out<-stats_main_temper %>% select(variable,term,val,se,pval,model,submodel) %>% as.data.frame()


######################################



do<-0

if(do==1){


# temperature stats

data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("ProfTemp_top2m")) %>% #,"SurfTemp_8","MidTemp_8","BotTemp_8")) %>%
  select(variable,year,yr,short_code,park_code,park_site,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,Asp,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro) %>%
  unique() %>% as.data.frame()

m_yr<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
m_yrdepth<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Depth_mean_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2)
m_yrvol<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3)
m_yrwa<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4)
m_yrelev<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5)
m_yrwala<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6)
m_yrasp<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Asp, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7)
m_swe<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8)
m_flush<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9)

m_yrpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:park_code, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1)
m_yrdepthpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Depth_mean_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2)
m_yrvolpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Volume_m3:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3)
m_yrwapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_ha:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4)
m_yrelevpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Elevation_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5)
m_yrwalapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_to_lake_area_ratio:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6)
m_yrasppark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Asp:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7)
m_swepark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8)
m_flushpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9)

#m_yr$sig<-""
#m_yr$sig[which(m_yr$p.value<=0.05)]<-"*"
#m_yr$sig[which(m_yr$p.value<=0.01)]<-"**"
#m_yr$sig[which(m_yr$p.value<=0.005)]<-"***"

temper_main<-m_yr %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)#,sig)
temper_main$term[which(temper_main$term=="(Intercept)")]<-"Intercept"

temper_interact1<-rbind(m_yr,m_yrdepth,m_yrvol,m_yrwa,m_yrelev,m_yrwala,m_yrasp,m_swe,m_flush)
#                       m_yrdepthpark,m_yrvolpark,m_yrwapark,m_yrelevpark,m_yrwalapark,m_yrasppark,m_swepark,m_flushpark)
temper_interact2<-rbind(m_yrpark,m_yrdepthpark,m_yrvolpark,m_yrwapark,m_yrelevpark,m_yrwalapark,m_yrasppark,m_swepark,m_flushpark)
temper_interact1$submodel<-0
temper_interact2$submodel<-1

temper_interact<-rbind(temper_interact1,temper_interact2)

temper_interact<-temper_interact %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value)
temper_interact$parkTF<-0
temper_interact$parkTF[grep("MORA|NOCA|OLYM",temper_interact$term)]<-1
temper_interact$interactTF<-0
temper_interact$interactTF[grep(":",temper_interact$term)]<-1
temper_interact$parkintTF<-0
temper_interact$parkintTF[which(temper_interact$parkTF==1 & temper_interact$interactTF==0)]<-1
temper_interact$parkslopeTF<-0
temper_interact$parkslopeTF[which(temper_interact$parkTF==1 & temper_interact$interactTF==1)]<-1
temper_interact<-temper_interact[order(temper_interact$variable,temper_interact$model,temper_interact$submodel,temper_interact$parkslopeTF),]


temper_interact$term[grep("boxCox",temper_interact$term)]<-"yr:flush_index_boxcox"

acast(temper_interact,model+variable~term)

temper_interact_nosubmodel<-temper_interact[which(temper_interact$submodel==0),]
#[-grep("park_codeMORA|park_codeNOCA|park_codeOLYM",temper_interact$term),]
temper_interact_mora<-temper_interact[grep("park_codeMORA",temper_interact$term),]
temper_interact_noca<-temper_interact[grep("park_codeNOCA",temper_interact$term),]
temper_interact_olym<-temper_interact[grep("park_codeOLYM",temper_interact$term),]
merge1<-merge(temper_interact_mora,temper_interact_noca,by=c("variable","model","submodel"),suffixes=c(".mora",".noca"))
merge2<-merge(merge1,temper_interact_olym,by=c("variable","model","submodel"),all=TRUE)
temper_interact_parkpart<-merge2 %>% dplyr::rename(term.olym=term,val.olym=val,se.olym=se,pval.olym=pval)
temper_interact_wparks<-merge(temper_interact_nopark,temper_interact_parkpart,by=c("variable","model"))
temper_interact_wparks<-temper_interact_wparks[-which(temper_interact_wparks$term=="(Intercept)"),]
temper_interact_wparks<-temper_interact_wparks %>% select(-submodel.x,-submodel.y) %>% as.data.frame()
duplicated(temper_interact_wparks)
temper_interact_wparks[order(temper_interact_wparks$model),]



yrcoef<-subset(temper_main,temper_main$term=="yr")
intcoef<-subset(temper_main,temper_main$term=="Intercept") %>% select(-term)
temper_main_wide<-merge(intcoef,yrcoef,by=c("variable","model"),suffixes=c(".int",".coef"))



which_int0<-grep("yr",temper_interact$term)
which_submodelF<-which(temper_interact$submodel==0)
which_submodelT<-which(temper_interact$submodel==1)
which_int<-c(which_int0,which_submodelF)[duplicated(c(which_int0,which_submodelF))]

yr_interact_coef<-temper_interact[which_int,]
int_interact_coef<-subset(temper_interact,substr(temper_interact$term,1,3)=="Int") %>% select(-term)
temper_interact_wide<-merge(int_interact_coef,yr_interact_coef,by=c("variable","model"),suffixes=c(".int",".coef"),all=TRUE)

temper_out<-rbind(temper_main_wide,temper_interact_wide)
temper_out<-temper_out[order(temper_out$model),]
write.csv(temper_out,"temper_out.csv",row.names=FALSE)

###############################
# stats for vars other than temperature

data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("DO_top2m", #"DO_below2m",
                                "Chlorophyll", 
                                "secchi_value_m" , "pH_top2m",
#                                "pH_below2m",
                                "SpCond_top2m",
                                "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>% #,"SurfTemp_8","MidTemp_8","BotTemp_8")) %>%
  select(variable,year,yr,short_code,park_code,park_site,Depth_mean_m,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,Asp,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro) %>%
  unique() %>% as.data.frame()


m_yr<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1.1)
m_yrdepth<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Depth_mean_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2.1)
m_yrvol<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3.1)
m_yrwa<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4.1)
m_yrelev<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5.1)
m_yrwala<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6.1)
m_yrasp<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Asp, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7.1)
m_swe<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8.1)
m_flush<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9.1)

m_yrpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=1.2)
m_yrdepthpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Depth_mean_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=2.2)
m_yrvolpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Volume_m3:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=3.2)
m_yrwapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_ha:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=4.2)
m_yrelevpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Elevation_m:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=5.2)
m_yrwalapark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Watershed_area_to_lake_area_ratio:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=6.2)
m_yrasppark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+yr:Asp:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=7.2)
m_swepark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=8.2)
m_flushpark<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ park_code+flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=9.2)

m_yr$sig<-""
m_yr$sig[which(m_yr$p.value<=0.05)]<-"*"
m_yr$sig[which(m_yr$p.value<=0.01)]<-"**"
m_yr$sig[which(m_yr$p.value<=0.005)]<-"***"

nontemp_main<-m_yr %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value,sig)
nontemp_main$term[which(nontemp_main$term=="(Intercept)")]<-"Intercept"

nontemp_interact<-rbind(m_yrdepth,m_yrvol,m_yrwa,m_yrelev,m_yrwala,m_yrasp,m_swe,m_flush)
nontemp_interact$term[which(nontemp_interact$term=="(Intercept)")]<-"Intercept"
nontemp_interact$sig<-""
nontemp_interact$sig[which(nontemp_interact$p.value<=0.05)]<-"*"
nontemp_interact$sig[which(nontemp_interact$p.value<=0.01)]<-"**"
nontemp_interact$sig[which(nontemp_interact$p.value<=0.005)]<-"***"
#nontemp_interact<-nontemp_interact[which(nontemp_interact$term!="(Intercept)"),]

nontemp_interact<-nontemp_interact %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value,sig)
nontemp_interact<-nontemp_interact[order(nontemp_interact$variable,nontemp_interact$term),]
nontemp_interact$term[grep("boxCox",nontemp_interact$term)]<-"yr:flush_index_boxcox"

yrcoef<-subset(nontemp_main,nontemp_main$term=="yr")
intcoef<-subset(nontemp_main,nontemp_main$term=="Intercept") %>% select(-term)
nontemp_main_wide<-merge(intcoef,yrcoef,by=c("variable","model"),suffixes=c(".int",".coef"))

yr_interact_coef<-subset(nontemp_interact,substr(nontemp_interact$term,1,2)=="yr")
int_interact_coef<-subset(nontemp_interact,substr(nontemp_interact$term,1,3)=="Int") %>% select(-term)
nontemp_interact_wide<-merge(int_interact_coef,yr_interact_coef,by=c("variable","model"),suffixes=c(".int",".coef"))
nontemp_interact_wide_signif<-subset(nontemp_interact_wide,substr(nontemp_interact_wide$term,1,3)=="yr:" & nontemp_interact_wide$p.value.coef<=0.05)

nontemp_out<-rbind(nontemp_main_wide,nontemp_interact_wide)
nontemp_out<-nontemp_out[order(nontemp_out$variable,nontemp_out$model),]

write.csv(nontemp_out,"nontemp_out.csv",row.names=FALSE)
write.csv(nontemp_main_wide,"nontemp_main.csv",row.names=FALSE)
write.csv(nontemp_interact_wide,"nontemp_interact.csv",row.names=FALSE)

allstats<-rbind(temper_out,nontemp_out)
write.csv(allstats,"allstats.csv",row.names=FALSE)

############################


one<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=1)
two<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:hydro, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=2)
three<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=3)
four<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=4)
five<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=5)
six<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=6)
#seven<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:flush_index_SWE_May, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
# https://rdrr.io/cran/car/man/boxCoxVariable.html
seven<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:boxCoxVariable(flush_index_SWE_May+1), random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame() %>% mutate(model=7)

one$sig<-""
one$sig[which(one$p.value<=0.05)]<-"*"
one$sig[which(one$p.value<=0.01)]<-"**"
one$sig[which(one$p.value<=0.005)]<-"***"

nontemp_main<-one %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value,sig)
nontemp_main$term[which(nontemp_main$term=="(Intercept)")]<-"Intercept"

nontemp_interact<-rbind(two,three,four,five,six,seven)
nontemp_interact$term[which(nontemp_interact$term=="(Intercept)")]<-"Intercept"
nontemp_interact$sig<-""
nontemp_interact$sig[which(nontemp_interact$p.value<=0.05)]<-"*"
nontemp_interact$sig[which(nontemp_interact$p.value<=0.01)]<-"**"
nontemp_interact$sig[which(nontemp_interact$p.value<=0.005)]<-"***"
#nontemp_interact<-nontemp_interact[which(nontemp_interact$term!="(Intercept)"),]

nontemp_interact<-nontemp_interact %>% mutate_if(is.numeric, signif,digits=2) %>% select(-df,-statistic,-effect,-group,variable,term,model,val=estimate,se=std.error,pval=p.value,sig)
nontemp_interact<-nontemp_interact[order(nontemp_interact$variable,nontemp_interact$term),]
nontemp_interact$term[grep("boxCox",nontemp_interact$term)]<-"yr:flush_index_boxcox"

yrcoef<-subset(nontemp_main,nontemp_main$term=="yr")
intcoef<-subset(nontemp_main,nontemp_main$term=="Intercept") %>% select(-term)
nontemp_main_wide<-merge(intcoef,yrcoef,by=c("variable","model"),suffixes=c(".int",".coef"))

yr_interact_coef<-subset(nontemp_interact,substr(nontemp_interact$term,1,2)=="yr")
int_interact_coef<-subset(nontemp_interact,substr(nontemp_interact$term,1,2)=="In") %>% select(-term)
nontemp_interact_wide<-merge(int_interact_coef,yr_interact_coef,by=c("variable","model"),suffixes=c(".int",".coef"))
nontemp_interact_wide_signif<-subset(nontemp_interact_wide,substr(nontemp_interact_wide$term,1,3)=="yr:" & nontemp_interact_wide$p.value.coef<=0.05)

nontemp_out<-rbind(nontemp_main_wide,nontemp_interact_wide)
nontemp_out<-nontemp_out[order(nontemp_out$variable,nontemp_out$model),]

write.csv(nontemp_out,"nontemp_out.csv",row.names=FALSE)
write.csv(nontemp_main_wide,"nontemp_main.csv",row.names=FALSE)
write.csv(nontemp_interact_wide,"nontemp_interact.csv",row.names=FALSE)

allstats<-rbind(temper_out,nontemp_out)
write.csv(allstats,"allstats.csv",row.names=FALSE)

##############################



##############################


temper_stats_wide



melt(temper_out %>% select(-variable,-df,-stat),id.vars=c("term","estimate","se","p.value","sig"))
  
dcast(melt(temper_out),format="wide")

melt(gather(temper_out %>% select(-variable,-df)))

gather(temper_out %>% select(-variable,-df)) %>% separate(key,c("term","(Intercept)"))

gather(temper_out %>% select(-variable,-df)) %>% spread(key)

melted<-melt(temper_out,variable)

dcast(melted, term+sig-variable~value)
dcast(melted,value~term+variable+sig-variable)
  
write.csv(temper_out,"temper_out.csv",row.names=FALSE)
write.csv(temper_interact_out,"temper_interact_out.csv",row.names=FALSE)

data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("DO_top2m", "DO_below2m",
                                "Chlorophyll", 
                                "secchi_value_m" , "pH_top2m",
                                "pH_below2m",
                                "SpCond_top2m",
                                "Ca","Cl","K","Mg","SO4","Total N","Total P")) %>%
  select(variable,year,yr,short_code,park_code,park_site,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro) %>%
  unique() %>% as.data.frame()

one<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
two<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:hydro, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
three<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Volume_m3, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
four<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_ha, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
five<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Elevation_m, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
six<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:Watershed_area_to_lake_area_ratio, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
#seven<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:flush_index_SWE_May, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
seven<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ yr:boxCoxVariable(flush_index_SWE_May+1), random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()

one$sig<-""
one$sig[which(one$p.value<=0.05)]<-"*"
one$sig[which(one$p.value<=0.01)]<-"**"
one$sig[which(one$p.value<=0.005)]<-"***"

nontemper_main<-one %>% mutate_if(is.numeric, signif,digits=2) %>% select(variable,term,estimate,se=std.error,df,stat=statistic,p.value,sig)

nontemper_interact<-rbind(two,three,four,five,six,seven)
nontemper_interact$sig<-""
nontemper_interact$sig[which(nontemper_interact$p.value<=0.05)]<-"*"
nontemper_interact$sig[which(nontemper_interact$p.value<=0.01)]<-"**"
nontemper_interact$sig[which(nontemper_interact$p.value<=0.005)]<-"***"
nontemper_interact<-nontemper_interact[which(nontemper_interact$term!="(Intercept)"),]

nontemper_interact_out<-nontemper_interact %>% mutate_if(is.numeric, signif,digits=2) %>% select(variable,term,estimate,se=std.error,df,stat=statistic,p.value,sig)
nontemper_interact_out$term[grep("boxCox",nontemper_interact_out$term)]<-"yr:flush_index_boxcox"
nontemper_interact_out<-nontemper_interact_out[order(nontemper_interact_out$variable,nontemper_interact_out$term),]
write.csv(nontemper_interact_out,"nontemper_interact.csv",row.names=FALSE)


########################################



#########################################

one %>% rename(signif(estimate,digits=3))

signif(one,digits=3)

data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable %in% c("DO_top2m", "Chlorophyll", 
                                "secchi_value_m" , "pH_top2m",
                                "pH_below2m",
                                "ANC","Ca","Cl","K","Mg","SO4","TDS","Total N","Total P","")) %>%
  select(variable,year,lowsnowyear,short_code,park_code,park_site,Volume_m3,Watershed_area_ha,Watershed_area_to_lake_area_ratio,Elevation_m,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet,hydro) %>%
  unique() %>% as.data.frame()

one<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
two<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:flush_index_SWE_May, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
three<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:hydro, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
four<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:Volume_m3, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
five<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:Watershed_area_ha, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
six<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:Watershed_area_to_lake_area_ratio, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
seven<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ lowsnowyear:Elevation_m, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()


################
one<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10, data=.))) %>% as.data.frame()
two<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:flush_index_SWE_May, data=.))) %>% as.data.frame()
three<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:hydro, data=.))) %>% as.data.frame()
four<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Volume_m3, data=.))) %>% as.data.frame()
five<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Watershed_area_ha, data=.))) %>% as.data.frame()
six<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Watershed_area_to_lake_area_ratio, data=.))) %>% as.data.frame()
seven<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Elevation_m, data=.))) %>% as.data.frame()
eight<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:park_code, data=.))) %>% as.data.frame()

nine<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10+park_code, data=.))) %>% as.data.frame()
ten<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:flush_index_SWE_May+park_code, data=.))) %>% as.data.frame()
eleven<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:hydro+park_code, data=.))) %>% as.data.frame()
twelve<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Volume_m3+park_code, data=.))) %>% as.data.frame()
thirteen<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Watershed_area_ha+park_code, data=.))) %>% as.data.frame()
fourteen<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Watershed_area_to_lake_area_ratio+park_code, data=.))) %>% as.data.frame()
fifteen<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Elevation_m+park_code, data=.))) %>% as.data.frame()


#############################
data<-subset(data_forstats,data_forstats$variable %in% c("ProfTemp_top2m"))
model<-lme(value ~ year10, random = ~ 1|park_code/park_site, data=wtemp)# %>% as.data.frame()
summary(model)


wtemp %>% group_by(variable) %>% do(tidy(lme(value ~ year10*Elevation_m, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()
poop<-data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ year10*Elevation_m, random = ~ 1|park_code/park_site, data=.))) #%>% as.data.frame()


poop<-lme(value ~ year10*Elevation_m, random = ~ 1|park_code/park_site, data=wtemp)# %>% as.data.frame()
poop<-lme(value ~ year10*Elevation_m, random = ~ 1|park_code/park_site, data=wtemp)# %>% as.data.frame()


poop<-wtemp %>% group_by(variable) %>% do(tidy(lme(value ~ year10:Elevation_m, random = ~ 1|park_code/park_site, data=.)))# %>% as.data.frame()



wtemp %>% group_by(variable) %>% do(tidy(lme(value ~ year10:park_code, random = ~ 1|park_site, data=.))) %>% as.data.frame()

wtemp %>% group_by(variable) %>% do(tidy(lme(value ~ year10, random = ~ 1|park_code/park_site, data=.))) %>% as.data.frame()


data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:hydro+park_code, data=.))) %>% as.data.frame()

data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:flush_index_SWE_May+park_code, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Volume_m3+park_code, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:Volume_m3+park_code, data=.))) %>% as.data.frame()



data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:log10(flush_index_SWE_May+0.03), data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:log10(flush_index_SWE_May+0.03)+park_code, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ year10:flush_index_SWE_May+park_code, data=.))) %>% as.data.frame()



data_forstats %>% group_by(variable) %>% do(tidy(glm(value ~ year10:flush_index_SWE_May+park_code, data=.,family="gamma"))) %>% as.data.frame()

data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ year10, random = ~ 1|park_code, data=.))) %>% as.data.frame()

data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ year10:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame()


subset(data_forstats, data_forstats$variable=="Chlorophyll") %>% group_by(variable,hydro) %>% tally() %>% as.data.frame()





data_models<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:hydro, data=.))) %>% as.data.frame()

######################




blueline_outlet<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineOutlet, data=.))) %>% as.data.frame()
blueline_inlet<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineInlet, data=.))) %>% as.data.frame()
blueline_outlet$signif<-""
blueline_inlet$signif<-""
blueline_outlet[blueline_outlet$p.value<=0.05,]$signif<-"*"
blueline_inlet[blueline_inlet$p.value<=0.05,]$signif<-"*"
  

###################################


data_forstats<- bigjoin.stats %>%
  dplyr::filter(year %in% c(2011, 2015)) %>%
  select(variable,year,short_code,park_code,park_site,
         value,
         ice_out_doy,SWE_May,flush_index_SWE_May,BlueLineInlet,BlueLineOutlet) %>%
  unique() %>% as.data.frame()



blueline_outlet<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineOutlet, data=.))) %>% as.data.frame()
blueline_inlet<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineInlet, data=.))) %>% as.data.frame()

blueline_outlet %>% as.data.frame()


data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:park_code, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineOutlet, data=.))) %>% as.data.frame()



########################





flush_lm_log10<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ log10(flush_index_SWE_May+0.01), data=.))) %>% as.data.frame()
flush_lm<-data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May, data=.))) %>% as.data.frame()

options(scipen = 999,digits=3)
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May, data=.))) %>% as.data.frame()


data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:park_code, data=.))) %>% as.data.frame()
data_forstats %>% group_by(variable) %>% do(tidy(lm(value ~ flush_index_SWE_May:BlueLineOutlet, data=.))) %>% as.data.frame()



#write.csv(flush_lm, "flush.lm.csv")

#write.csv(flush_lm_log10, "flush.lm.log10.csv")

#flush_lm %>% mutate_if(is.numeric,list(signif(digits=3)))

#signif(flush_lm,digits=3)


data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame()



####################### 

data_forstats %>% group_by(variable) %>% do(tidy(lme(value ~ flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data=.))) %>% as.data.frame()


test<-lme(value ~ flush_index_SWE_May:park_code-1, random = ~ 1|park_site, data_forstats)


my_lme <- lme(value ~ flush_index_SWE_May:variable-1, random = ~ 1|park_code, data_forstats)
summary(my_lme)

data_forstats_groups<-groupedData( value ~ flush_index_SWE_May|dummy,
             data = as.data.frame( data_forstats ))
data_forstats_groups<-groupedData( value ~ flush_index_SWE_May|short_code,data_forstats )


my_lme <- lme(value ~ flush_index_SWE_May:variable, random = ~ 1, data=data_forstats)

my_lme <- lme(value ~ log10(flush_index_SWE_May+0.01):variable-1, random = ~ 1+1|dummy, data=data_forstats)
summary(my_lme)


data_forstats2<-subset(data_forstats,data_forstats$variable != "Chlorophyll")
my_lme2 <- lme(value ~ log10(flush_index_SWE_May+0.01):variable-1, random = ~ 1|short_code, data=data_forstats2)
summary(my_lme2)

data_forstats3<-subset(data_forstats2,data_forstats2$variable != "DO_top2m")
my_lme3 <- lme(value ~ log10(flush_index_SWE_May+0.01):variable-1, random = ~ 1|short_code, data=data_forstats3)
summary(my_lme3)


data_forstats4<-subset(data_forstats3,data_forstats3$variable != "pH_top2m")
my_lme4 <- lme(value ~ log10(flush_index_SWE_May+0.01):variable-1, random = ~ 1|short_code, data=data_forstats4)
summary(my_lme4)

data_forstats5<-subset(data_forstats4,data_forstats4$variable != "SpCond_top2m")
my_lme5 <- lme(value ~ log10(flush_index_SWE_May+0.01):variable, random = ~ 1+1|dummy, data=data_forstats5)
summary(my_lme5)

fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
lme(value ~ flush_index_SWE_May, data = data_forstats, random = ~ 1)

data_forstats2<-data_forstats %>% select(value,flush_index_SWE_May,variable)
lmlist<-lmList(value~log10(flush_index_SWE_May+0.01)|variable,data_forstats2)


datax<-subset(data_forstats,data_forstats$variable %in% c("ProfTemp_top2m","Chlorophyll"))
datax<-subset(data_forstats,data_forstats$variable=="ProfTemp_top2m")
datax<-subset(data_forstats,data_forstats$variable=="Chlorophyll")
lmx<-lm(value~log10(flush_index_SWE_May+0.01),data=datax)
summary(lmx)
}


