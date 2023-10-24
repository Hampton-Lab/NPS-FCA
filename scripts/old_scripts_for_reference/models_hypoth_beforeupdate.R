#temperature model
vars_2covar<-combn(c("SWE_May_snotel_model","solar_jas","Volume_m3"),2)
vars_3covar<-combn(c("SWE_May_snotel_model","solar_jas","Volume_m3"),3)
data_formodel<- data_forstats %>% filter(variable=="ProfTemp_top2m")
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="ProfTemp_top2m")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_3covar0<-mod_3covar(covars=vars_3covar,data=data_formodel,varname="ProfTemp_top2m")
m_3covar0<-m_3covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_temp<-rbind(m2_covar,m3_covar)  %>% arrange(AICc)

#mi<-data_formodel %>% do(tidy(lme(na.action=na.omit, value_model ~ SWE_May_snotel_model*solar_jas*Volume_m3, random = ~ 1|park_site, method="ML",data=.))) %>% as.data.frame() %>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
#mi_glance<-do(daterr, glance(lme(na.action=na.omit, value_model ~ vari1*vari2*vari3, random = ~ 1|park_site, method="ML",data = .))) %>% as.data.frame() #%>% mutate(model=paste("2_",i,sep="")) %>% select(variable,sigma,logLik,AIC,BIC,model)


data_formodel<- data_forstats %>% filter(variable %in% c("Chlorophyll","secchi_value_m","Total N","Total P","ProfTemp_top2m")) %>% 
  select(variable,park_site,solar_jas,forest,hydro,N_dep_1985_2015,SO4_dep_1985_2015,value_model,SWE_May_snotel_model) %>% unique() %>% 
  tidyr::pivot_wider(names_from=variable,values_from=value_model,values_fn = list(value_model=mean)) %>% rename(TotalN="Total N",TotalP="Total P") %>% as.data.frame()

#secchi models
vars_2covar<-combn(c("SWE_May_snotel_model","Chlorophyll","hydro"),2)
data_formodel$value_model<-data_formodel$secchi_value_m
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="secchi_value_m")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_secchi<-m_2covar0 %>% arrange(AICc)

#chlorophyll models
vars_2covar<-combn(c("SWE_May_snotel_model","TotalN","TotalP","solar_jas","ProfTemp_top2m"),2)
data_formodel$value_model<-data_formodel$Chlorophyll
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="Chlorophyll")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_chl<-m_2covar0 %>% arrange(AICc)

#N models
vars_2covar<-combn(c("SWE_May_snotel_model","forest","N_dep_1985_2015"),2)
data_formodel$value_model<-data_formodel$TotalN
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="TotalN")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_N<-m_2covar0 %>% arrange(AICc)

#P models
vars_2covar<-combn(c("SWE_May_snotel_model","forest","hydro"),2)
data_formodel$value_model<-data_formodel$TotalP
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="TotalP")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_P<-m_2covar0 %>% arrange(AICc)

#SO4 models
vars_2covar<-combn(c("SWE_May_snotel_model","SO4_dep_1985_2015","hydro"),2)
data_formodel<- data_forstats %>% filter(variable=="SO4")
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="SO4")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_SO4<-m_2covar0 %>% arrange(AICc)

#Cl models
vars_2covar<-combn(c("SWE_May_snotel_model","Cl_dep_1985_2015","hydro"),2)
data_formodel<- data_forstats %>% filter(variable=="Cl")
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="Cl")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_Cl<-m_2covar0 %>% arrange(AICc)

#pH models
vars_2covar<-combn(c("SWE_May_snotel_model","hydro","Volume_m3",
                     "andesite"),2)#,"basalt","biotitegneiss","granodiorite","graywacke","quartzmonzodiorite","quartzmonzonite","sandstone"),2)
#vars_2covar<-vars_2covar[,-c(17,23)]
data_formodel<- data_forstats %>% filter(variable=="pH_top2m") 
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="pH_top2m")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_pH<-m_2covar0 %>% arrange(AICc)

vars_2covar0<-combn(c("SWE_May_snotel_model","hydro","Volume_m3","andesite","basalt","biotitegneiss","granodiorite","graywacke","quartzmonzodiorite","quartzmonzonite","sandstone"),2)

#ions models
vars_2covar<-vars_2covar0[,c(1:16,18,19,20,21,22,23,24,26,27)]
data_formodel<- data_forstats %>% filter(variable=="Ca") 
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="Ca")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_Ca<-m_2covar0 %>% arrange(AICc)

vars_2covar<-vars_2covar0[,c(1:16,18,19,20,21,22,23,24,26,27)]
data_formodel<- data_forstats %>% filter(variable=="Mg") 
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="Mg")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_Mg<-m_2covar0 %>% arrange(AICc)

vars_2covar<-vars_2covar0[,c(1:16,18,19,20,21,22,23,24,26,27)]
data_formodel<- data_forstats %>% filter(variable=="K") 
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="K")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_K<-m_2covar0 %>% arrange(AICc)

vars_2covar<-vars_2covar0[,c(1:16,18,19,20,21,22,23,24,26,27)]
data_formodel<- data_forstats %>% filter(variable=="Na") 
m_2covar0<-mod_2covar(covars=vars_2covar,data=data_formodel,varname="Na")
m_2covar0<-m_2covar0 %>% mutate_if(is.numeric, signif,digits=3)
m_2covar_Na<-m_2covar0 %>% arrange(AICc)


sink(fileout)
print(m_2covar_temp)
print(m_2covar_secchi)
print(m_2covar_chl)
print(m_2covar_N)
print(m_2covar_P)
print(m_2covar_SO4)
print(m_2covar_Cl)
print(m_2covar_pH)
print(m_2covar_Ca)
print(m_2covar_Mg)
print(m_2covar_K)
print(m_2covar_Na)
sink()
