zuse_set<-1
#zuse_set<-1

# choose variables for testing interactions
vars<-c("snowyrlo_snotel","snowyrhi_snotel",
        "Depth_mean_m","Volume_m3","Watershed_area_ha","Elevation_m","elevmean_wshed",
        "SWE_May_snotel","SWE_May_snodas",#"SWE_Apr",
        "Watershed_area_to_lake_area_ratio","northface",#"aspectns_lake","aspectns_wshed","northface_wshed",
        "flush_index_SWE_May_snodas",#"flush_index_SWE_Apr",#"park_code",
        "salmo_adult_ever",
        "solar_jas","solar_dec","barren","forest",#"shrub","meadow",
        "inlet","outlet",
        dep_names,
        "andesite","basalt","biotitegneiss","granodiorite","graywacke","quartzmonzodiorite","quartzmonzonite","sandstone")
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
data_forstats$SWE_May_snotel_z<-data_forstats$SWE_May_snotel
#for (i in 1:length(unique(data_forstats$variable))){
  for(j in 1:length(unique(data_forstats$park_site))){
#    vari<-unique(data_forstats$variable)[i]
    lakej<-unique(data_forstats$park_site)[j]
    #  vari<-vars[i]
    scaledij<-as.numeric(scale(data_forstats$SWE_May_snotel[which(data_forstats$park_site==lakej)]))
    #  scaledi<-scale(data_forstats[,which(names(data_forstats)==vari)])
    data_forstats$SWE_May_snotel_z[which(data_forstats$park_site==lakej)]<-scaledij
  }
#}


##################
# variance parts

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

##################
# function to fit mixed models using 1 predictor
mod_1covar<-function(covars,data,zuse){
  for (i in 1:length(covars)){
    vari<-covars[i]
    daterr<-data
    daterr$vari<-daterr[,which(names(daterr)==vari)]
    if(zuse==0){daterr$val<-daterr$value}
    if(zuse==1){daterr$val<-daterr$value_z}
    by_variablei <- group_by(daterr, variable)
    mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit,val ~ snowyr:vari-1, random = ~ 1|park_site/snowyr, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=i+length(vars))
    mi_glance<-do(by_variablei, glance(lme(na.action=na.omit,val ~ snowyr:vari-1, random = ~ 1|park_site/snowyr, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=i+length(vars)) %>% select(variable,sigma,logLik,AIC,BIC,model)
    mi<-merge(mi,mi_glance,by=c("variable","model"))
    mi$snowyr<-substr(mi$term,7,nchar(mi$term)-5)
    mi$term<-vari 
    if(i==1){m_1covar<-mi}
    if(i>1){m_1covar<-rbind(m_1covar,mi)}
  }
  return(m_1covar)
}
# fit lme models, 1 covariate
m_1covar<-mod_1covar(covars=vars_1covar,data=data_forstats,zuse=zuse_set)
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
mod_2covar<-function(covars,data,zuse){
  for (i in 1:length(covars[1,])){
    var1i<-covars[,i][1];var2i<-covars[,i][2]
    daterr<-data
    daterr$vari1<-daterr[,which(names(daterr)==var1i[1])]
    daterr$vari2<-daterr[,which(names(daterr)==var2i[1])]
    if(zuse==0){daterr$val<-daterr$value}
    if(zuse==1){daterr$val<-daterr$value_z}
    by_variablei <- group_by(daterr, variable)
    if(daterr$vari1==daterr$vari2){
      print("perfectly correlated vars...")
      print(c(var1i,var2i))
      print("on to the next set...")
      next()}
    mi<-by_variablei %>% group_by(variable) %>% do(tidy(lme(na.action=na.omit, val ~ snowyr:vari1+snowyr:vari2-1, random = ~ 1|park_site/snowyr, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
    mi_glance<-do(by_variablei, glance(lme(na.action=na.omit, val ~ snowyr:vari1+snowyr:vari2-1, random = ~ 1|park_site/snowyr, method="ML",data = .))) %>% as.data.frame() %>% mutate(model=paste("2_",i,sep="")) %>% select(variable,sigma,logLik,AIC,BIC,model)
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
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_forstats,zuse=zuse_set)
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