#temperature model
data_formodel<- data_forstats %>% filter(variable=="ProfTemp_top2m")
lmetemp<-lme(na.action=na.omit,value_model ~ snowyr:SWE_May_snotel+snowyr:solar_jas+snowyr:Volume_m3-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel)
sink(fileout,append=TRUE)
print("############  temperature  ####################################################################################################")
print(summary(lmetemp)$tTable)
print("####################################################################")
sink()

data_formodel<- data_forstats %>% filter(variable %in% c("Chlorophyll","secchi_value_m","Total N","Total P","ProfTemp_top2m")) %>% 
  select(variable,park_site,snowyr,solar_jas,forest,N_dep_1985_2015,SO4_dep_1985_2015,value_model) %>% unique() %>% 
  tidyr::pivot_wider(names_from=variable,values_from=value_model,values_fn = list(value_model=mean)) %>% rename(TotalN="Total N",TotalP="Total P") %>% as.data.frame()

#secchi models
lmesecchi<-lme(na.action=na.omit,secchi_value_m ~ snowyr:Chlorophyll-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 

sink(fileout,append=TRUE)
print("############  secchi  #################################")
print(summary(lmesecchi)$tTable)
print("####################################################################")
sink()

#chlorophyll models
lmechl1<-lme(na.action=na.omit,Chlorophyll ~ snowyr:TotalN+snowyr:TotalP+snowyr:solar_jas+snowyr:ProfTemp_top2m-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
lmechl2<-lme(na.action=na.omit,Chlorophyll ~ snowyr:TotalN+snowyr:solar_jas+snowyr:ProfTemp_top2m-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
lmechl3<-lme(na.action=na.omit,Chlorophyll ~ snowyr:TotalP+snowyr:solar_jas+snowyr:ProfTemp_top2m-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
lmechl4<-lme(na.action=na.omit,Chlorophyll ~ snowyr:TotalN+snowyr:TotalP-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 

sink(fileout,append=TRUE)
print("############  chlorophyll  #################################")
print(summary(lmechl1)$tTable)
print(summary(lmechl2)$tTable)
print(summary(lmechl3)$tTable)
print(summary(lmechl4)$tTable)
print("####################################################################")
sink()

# nitrogen model
lmen<-lme(na.action=na.omit,TotalN ~ snowyr:forest+snowyr:N_dep_1985_2015-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
sink(fileout,append=TRUE)
print("############  nitrogen  #################################")
print(summary(lmen)$tTable)
print("####################################################################")
sink()

# phosphorus model
lmep<-lme(na.action=na.omit,TotalP ~ snowyr:forest-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
print(sink(fileout,append=TRUE))
print("############  phosphorus  #################################")
print(summary(lmep)$tTable)
print("####################################################################")
sink()

# sulfate model
data_formodel<- data_forstats %>% filter(variable=="SO4")
lmeSO4<-lme(na.action=na.omit,value_model ~ snowyr:SO4_dep_1985_2015-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
sink(fileout,append=TRUE)
print("############  SO4  #################################")
print(summary(lmeSO4)$tTable)
print("####################################################################")
sink()

# chloride model
data_formodel<- data_forstats %>% filter(variable=="Cl")
lmeCl<-lme(na.action=na.omit,value_model ~ snowyr:Cl_dep_1985_2015-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
sink(fileout,append=TRUE)
print("############  Cl  #################################")
print(summary(lmeCl)$tTable)
print("####################################################################")
sink()


# pH
data_formodel<- data_forstats %>% filter(variable=="pH_top2m") 
lmepH1<-lme(na.action=na.omit,value_model ~ snowyr:hydro+snowyr:Volume_m3-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
lmepH2<-lme(na.action=na.omit,value_model ~ snowyr:andesite+snowyr:basalt+snowyr:biotitegneiss+snowyr:granodiorite+snowyr:graywacke+snowyr:quartzmonzodiorite+snowyr:quartzmonzonite+snowyr:sandstone-1, random = ~ 1|park_site/snowyr, method="ML",data=data_formodel) 
sink(fileout,append=TRUE)
print("############  pH  #################################")
print(summary(lmepH1)$tTable)
print(summary(lmepH2)$tTable)
print("####################################################################")
sink()

#cations/ations
data_formodel<- data_forstats %>% filter(variable %in% c("Ca","Mg","K","Na")) %>% 
  select(variable,park_site,snowyr,solar_jas,forest,hydro,Volume_m3,BlueLineInlet,BlueLineOutlet,
         andesite,basalt,biotitegneiss,granodiorite,graywacke,quartzmonzodiorite,quartzmonzonite,sandstone,
         value_model) %>% unique() #%>% 

by_variablei <- group_by(data_formodel, variable)
lmeions<-by_variablei %>% do(tidy(lme(na.action=na.omit,value_model ~ snowyr:andesite+snowyr:basalt+snowyr:biotitegneiss+snowyr:granodiorite+snowyr:graywacke+snowyr:quartzmonzodiorite+snowyr:quartzmonzonite+snowyr:sandstone-1, random = ~ 1|park_site/snowyr, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars") %>% as.data.frame()
lmeions_glance<-do(by_variablei, glance(lme(na.action=na.omit,value_model ~ snowyr:andesite+snowyr:basalt+snowyr:biotitegneiss+snowyr:granodiorite+snowyr:graywacke+snowyr:quartzmonzodiorite+snowyr:quartzmonzonite+snowyr:sandstone-1, random = ~ 1|park_site/snowyr, method="ML",data = .))) %>% as.data.frame() %>% select(variable,sigma,logLik,AIC,BIC)
sink(fileout,append=TRUE)
print("############  ions  #################################")
print(lmeions)
print("####################################################################")
sink()
