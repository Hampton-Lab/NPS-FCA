#fixedpart1<-"vari1"
#fixedpart2<-"vari1+vari2"
#fixedpart2b<-"vari1*vari2"
#fixedpart3<-"vari1+vari2+vari3"
#fixedpart3b<-"(vari1+vari2+vari3)^2"
#randompart<-list(~1|park_site)
#fixedpart3b<-"vari1*vari2*vari3"
#fixedpart1<-"vari1:snowyr"
#fixedpart2<-"vari1:snowyr+vari2:snowyr"
#fixedpart2b<-"vari1:snowyr*vari2:snowyr"
#fixedpart3<-"vari1:snowyr*vari2:snowyr*vari3:snowyr"
#randompart<-list(~1|nothing)
#randompart<-"(1|nothing)"
#randompart<-"(1|park_site)"
#randompart<-list(~1|park_site)
#doplots<-1
#usenlme<-1
#    mi<-lme(value_model~ice_out_doy+SWE_May_snotel+, random = randompart, method="ML",data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))

ndigits<-4

#SWE model
vars_1covar<-combn(c("ice_out_doy","solar_jas","solar_dec"),1)
vars_2covar<-combn(c("ice_out_doy","solar_jas","solar_dec"),2)
vars_3covar<-combn(c("ice_out_doy","solar_jas","solar_dec"),3)
data_formodel<- data_forstats
data_formodel$value<-data_formodel$SWE_May_snotel
covars=vars_1covar;data=data_formodel;varname="SWE";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=1
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="SWE";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="SWE";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=1
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="SWE";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="SWE";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3b;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar

m_covar_SWE<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_SWE<-m_covar_SWE %>% mutate_if(is.numeric, signif,digits=ndigits)


#ice_out_doy model
vars_1covar<-combn(c("SWE_May_snotel","solar_jas","solar_dec","Volume_m3","Elevation_m"),1)
vars_2covar<-combn(c("SWE_May_snotel","solar_jas","solar_dec","Volume_m3","Elevation_m"),2)
vars_3covar<-combn(c("SWE_May_snotel","solar_jas","solar_dec","Volume_m3","Elevation_m"),3)
data_formodel<- data_forstats
data_formodel$value<-data_formodel$ice_out_doy
covars=vars_1covar;data=data_formodel;varname="ice_out_doy";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="ice_out_doy";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="ice_out_doy";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="ice_out_doy";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="ice_out_doy";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3b;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_ice_out<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_ice_out<-m_covar_ice_out %>% mutate_if(is.numeric, signif,digits=ndigits)

#temperature model
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","solar_jas","solar_dec","Volume_m3","hydro"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","solar_jas","solar_dec","Volume_m3","hydro"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","solar_jas","solar_dec","Volume_m3","hydro"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="Watertemp")
covars=vars_1covar;data=data_formodel;varname="Watertemp";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Watertemp";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Watertemp";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2b;
                     randompart=randompart;doplots=1
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Watertemp";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3;
                      randompart=randompart;doplots=doplots_set
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Watertemp";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3b;
                     randompart=randompart;doplots=1
source("mod_covar.R")
m_3covar0b<-m_covar

m_covar_temp<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_temp<-m_covar_temp %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel_multi<- data_forstats %>% filter(var_abbrev1 %in% c("Chl","Secchi","TDN","TDP","Watertemp","SO4","DO","pH","Cond")) %>% 
  select(var_abbrev1,park_site,park_code,year,snowyr,
         parklakenum,short_code,
         solar_jas,solar_dec,forest,hydro,Depth_mean_m,Volume_m3,ice_out_doy,
         N_dep_2000_2009,NH4_dep_2000_2009,NO3_dep_2000_2009,
         SO4_dep_2000_2009,value,SWE_May_snotel,SWE_May_snotel_z,ice_out_doy,ice_out_doy_z) %>% unique() %>% 
  tidyr::pivot_wider(names_from=var_abbrev1,values_from=value,values_fn = list(value=mean)) %>% as.data.frame()
data_formodel<-data_formodel_multi
#secchi models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","Chl","hydro"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","Chl","hydro"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","Chl","hydro"),3)
data_formodel$value<-data_formodel$Secchi#_value_m
covars=vars_1covar;data=data_formodel;varname="Secchi";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Secchi";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Secchi";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Secchi";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Secchi";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=1
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_secchi<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_secchi<-m_covar_secchi %>% mutate_if(is.numeric, signif,digits=ndigits)

#chlorophyll models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),3)
data_formodel$value<-data_formodel$Chl
covars=vars_1covar;data=data_formodel;varname="Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Chl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Chl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_chl<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_chl<-m_covar_chl %>% mutate_if(is.numeric, signif,digits=ndigits)

#log10 chlorophyll models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","TDN","TDP","solar_jas","solar_dec","Watertemp"),3)
data_formodel$value<-log10(data_formodel$Chl+1.5*abs(min(data_formodel$Chl[which(data_formodel$Chl!=0)],na.rm=TRUE)))
covars=vars_1covar;data=data_formodel;varname="log10Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Chl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Chl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Chl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_chllog10<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_chllog10<-m_covar_chllog10 %>% mutate_if(is.numeric, signif,digits=ndigits)


#oxygen models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl"),3)
data_formodel$value<-data_formodel$DO
covars=vars_1covar;data=data_formodel;varname="DO";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="DO";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="DO";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="DO";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="DO";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_do<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_do<-m_covar_do %>% mutate_if(is.numeric, signif,digits=ndigits)

# Conductivity models
#vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3","Chl"),1)
#vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3","Chl"),2)
#vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3","Chl"),3)
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="Cond")
#data_formodel$value<-data_formodel$Cond
covars=vars_1covar;data=data_formodel;varname="Cond";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cond";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cond";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cond";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cond";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_Cond<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_Cond<-m_covar_Cond %>% mutate_if(is.numeric, signif,digits=ndigits)

#N models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009"),3)
#vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009","Watertemp"),1)
#vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009","Watertemp"),2)
#vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","N_dep_2000_2009","NH4_dep_2000_2009","NO3_dep_2000_2009","Watertemp"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="TDN")
#data_formodel$value<-data_formodel$TDN
covars=vars_1covar;data=data_formodel;varname="TDN";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="TDN";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="TDN";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="TDN";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="TDN";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_N<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_N<-m_covar_N %>% mutate_if(is.numeric, signif,digits=ndigits)

#P models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","hydro","Depth_mean_m"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","hydro","Depth_mean_m"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","forest","hydro","Depth_mean_m"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="TDP")
#data_formodel$value<-data_formodel$TDP
covars=vars_1covar;data=data_formodel;varname="TDP";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="TDP";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="TDP";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="TDP";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="TDP";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_P<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_P<-m_covar_P %>% mutate_if(is.numeric, signif,digits=ndigits)

#SO4 models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro"),3)
#vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN"),1)
#vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN"),2)
#vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN"),3)
#vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN","Watertemp"),1)
#vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN","Watertemp"),2)
#vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","SO4_dep_2000_2009","hydro","TDN","Watertemp"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="SO4")
#data_formodel$value<-data_formodel$SO4
covars=vars_1covar;data=data_formodel;varname="SO4";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="SO4";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="SO4";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="SO4";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="SO4";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_SO4<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_SO4<-m_covar_SO4 %>% mutate_if(is.numeric, signif,digits=ndigits)

#Cl models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","Cl_dep_2000_2009","hydro"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","Cl_dep_2000_2009","hydro"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","Cl_dep_2000_2009","hydro"),3)
data_formodel<- data_forstats %>% filter(var_abbrev1=="Cl")
covars=vars_1covar;data=data_formodel;varname="Cl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cl";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cl";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_Cl<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_Cl<-m_covar_Cl %>% mutate_if(is.numeric, signif,digits=ndigits)

geolvars<-c("andesite","graywacke")#,"quartzmonzonite")#,"sandstone")
#geolvars<-c("andesite","basalt","granodiorite","graywacke")#,"quartzmonzonite")#,"sandstone")
#geolvars<-c("andesite","basalt","biotitegneiss","granodiorite","graywacke")#,"quartzmonzonite")#,"sandstone")
#geolvars<-c("andesite","basalt","biotitegneiss","granodiorite","graywacke","quartzmonzodiorite","quartzmonzonite","sandstone")


for (i in 1:length(geolvars)){
  geolvari<-geolvars[i]
  combparti<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3","ice_out_doy",geolvari),2)
  if(i==1){vars_2covar0<-combparti}
  if(i>1){vars_2covar0<-cbind(vars_2covar0,combparti)}
}

for (i in 1:length(geolvars)){
  geolvari<-geolvars[i]
  combparti<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3",geolvari),3)
  combparti<-combparti[,-c(2,4)]
  if(i==1){vars_3covar0<-combparti}
  if(i>1){vars_3covar0<-cbind(vars_3covar0,combparti)}
}

vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","hydro","Volume_m3",geolvari),1)
vars_2covar<-vars_2covar0[,-which(duplicated(t(vars_2covar0)))]
vars_3covar<-vars_3covar0[,-which(duplicated(t(vars_3covar0)))]

# pH models  
data_formodel<- data_forstats %>% filter(var_abbrev1=="pH") 
covars=vars_1covar;data=data_formodel;varname="pH";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="pH";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="pH";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="pH";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="pH";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar

m_covar_pH<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_pH<-m_covar_pH %>% mutate_if(is.numeric, signif,digits=ndigits)

#ions models
data_formodel<- data_forstats %>% filter(var_abbrev1=="Ca") 
covars=vars_1covar;data=data_formodel;varname="Ca";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Ca";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Ca";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Ca";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Ca";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_Ca<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_Ca<-m_covar_Ca %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel<- data_forstats %>% filter(var_abbrev1=="Mg") 
covars=vars_1covar;data=data_formodel;varname="Mg";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Mg";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Mg";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Mg";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Mg";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_Mg<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_Mg<-m_covar_Mg %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel<- data_forstats %>% filter(var_abbrev1=="K") 
covars=vars_1covar;data=data_formodel;varname="K";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="K";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="K";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="K";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="K";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_K<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_K<-m_covar_K %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel<- data_forstats %>% filter(var_abbrev1=="Na") 
covars=vars_1covar;data=data_formodel;varname="Na";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Na";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Na";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Na";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Na";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_Na<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_Na<-m_covar_Na %>% mutate_if(is.numeric, signif,digits=ndigits)

#    mi$modname3<-gsub("*|+","",fixedpart)


#zoop models
vars_1covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl","Ca"),1)
vars_2covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl","Ca"),2)
vars_3covar<-combn(c("ice_out_doy","SWE_May_snotel","Watertemp","Chl","Ca"),3)
#data_formodel<-data_forstats %>% filter(var_abbrev1=="clad") 

data_formodel_multi<- data_forstats %>% filter(var_abbrev1 %in% c("clad","rot","cope","Watertemp","Chl","Ca")) %>% 
  select(var_abbrev1,park_site,park_code,year,snowyr,
         parklakenum,short_code,
         solar_jas,solar_dec,forest,hydro,Depth_mean_m,Volume_m3,ice_out_doy,
         value,SWE_May_snotel,SWE_May_snotel_z,ice_out_doy,ice_out_doy_z) %>% unique() %>% 
  tidyr::pivot_wider(names_from=var_abbrev1,values_from=value,values_fn = list(value=mean)) %>% as.data.frame()
data_formodel<-data_formodel_multi
data_formodel$value<-data_formodel$clad
covars=vars_1covar;data=data_formodel;varname="Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Clad";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Clad";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_CLAD<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_CLAD<-m_covar_CLAD %>% mutate_if(is.numeric, signif,digits=ndigits)


data_formodel$value<-data_formodel$rot
covars=vars_1covar;data=data_formodel;varname="Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Rot";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Rot";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_ROT<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_ROT<-m_covar_ROT %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel$value<-data_formodel$cope
covars=vars_1covar;data=data_formodel;varname="Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="Cope";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="Cope";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_COPE<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_COPE<-m_covar_COPE %>% mutate_if(is.numeric, signif,digits=ndigits)


# now log10 trans

data_formodel$value<-log10(data_formodel$clad+min(data_formodel$clad[which(data_formodel$clad>0)],na.rm=TRUE))
covars=vars_1covar;data=data_formodel;varname="log10Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Clad";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Clad";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Clad";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_CLADlog10<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_CLADlog10<-m_covar_CLADlog10 %>% mutate_if(is.numeric, signif,digits=ndigits)


data_formodel$value<-log10(data_formodel$rot+min(data_formodel$rot[which(data_formodel$rot>0)],na.rm=TRUE))
covars=vars_1covar;data=data_formodel;varname="log10Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Rot";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Rot";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Rot";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_ROTlog10<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_ROTlog10<-m_covar_ROTlog10 %>% mutate_if(is.numeric, signif,digits=ndigits)

data_formodel$value<-log10(data_formodel$cope+min(data_formodel$cope[which(data_formodel$cope>0)],na.rm=TRUE))
covars=vars_1covar;data=data_formodel;varname="log10Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart1;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_1covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart2;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0<-m_covar
covars=vars_2covar;data=data_formodel;varname="log10Cope";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart2b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_2covar0b<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Cope";
                     valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                     fixedpart=fixedpart3;
                     randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0<-m_covar
covars=vars_3covar;data=data_formodel;varname="log10Cope";
                      valuecol=valuecol_set;swecol=swecol_set;iceoutdcol=iceoutdcol_set;
                      fixedpart=fixedpart3b;
                      randompart=randompart;doplots=doplots_set 
source("mod_covar.R")
m_3covar0b<-m_covar
m_covar_COPElog10<-rbind(m_1covar0,m_2covar0,m_2covar0b,m_3covar0,m_3covar0b)  %>% arrange(AICc)
m_covar_COPElog10<-m_covar_COPElog10 %>% mutate_if(is.numeric, signif,digits=ndigits)



models_df<-rbind(m_covar_SWE,m_covar_ice_out,
                 m_covar_temp,m_covar_secchi,m_covar_chl,m_covar_do,
                 m_covar_N,m_covar_P,m_covar_SO4,
                 m_covar_Cl,m_covar_pH,m_covar_Ca,m_covar_Mg,m_covar_K,m_covar_Na,
                 m_covar_Cond,
                 m_covar_CLAD,m_covar_ROT,m_covar_COPE,
                 m_covar_chllog10,
                 m_covar_CLADlog10,m_covar_ROTlog10,m_covar_COPElog10)
models_df$modname3<-gsub("[*]|[+]","",models_df$modname2)

# drop models that have interactions between iceoutd and SWE

models_df_checks<-models_df
models_df_checks$hasiceoutd<-0
models_df_checks$hasiceoutd<-grepl("ice_out_doy",models_df_checks$modname) %>% as.numeric()
models_df_checks$hasSWE<-0
models_df_checks$hasSWE<-grepl("SWE_May_snotel",models_df_checks$modname) %>% as.numeric()
#models_df_checks$hasinteract2<-grepl("vari1\\*vari2",models_df_checks$modname2) %>% as.numeric()
#models_df_checks$hasinteract3<-grepl("\\^2",models_df_checks$modname2) %>% as.numeric()

#which_drop2interact<-which((models_df_checks$hasiceoutd+models_df_checks$hasSWE+models_df_checks$hasinteract2)==3)
#which_drop3interact<-which((models_df_checks$hasiceoutd+models_df_checks$hasSWE+models_df_checks$hasinteract3)==3)

#which_drop<-c(which_drop2interact,which_drop3interact) %>% unique()

which_drop2<-which((models_df_checks$hasiceoutd+models_df_checks$hasSWE)==2)
which_drop<-which_drop2

models_df_checks<-models_df_checks[-which_drop,]

models_df<-models_df_checks
models_df<-models_df %>% select(-hasiceoutd,-hasSWE)

#models_df<-models_df %>% select(-hasiceoutd,-hasSWE,-hasinteract2,-hasinteract3)


# include or exclude interactions
which_interact<-c(grep("\\*",models_df$modname),grep("\\^",models_df$modname)) %>% unique()
if(interact==0){models_df<-models_df[-which_interact,]}

# limit interactions to those involving iceout date or SWE
#c(grep("\\*",models_df$modname),grep("\\^",models_df$modname)) %>% unique()
if(interact==1){
  
  which_interact<-grep("\\:",models_df$term)
#  which_iceoutd<-grep("iceout_doy",models_df$term)
#  which_SWE<-grep("SWE",models_df$term)
  
  one<-models_df[which_interact,]
  two<-one[-grep("iceout_doy",one$term),]
  three<-one[-grep("SWE",one$term),]
  four<-rbind(two,three)
  modnames_drop<-four$modname %>% unique()
  
  models_df_dropinteracts<-models_df[-which(models_df$modname %in% modnames_drop),]
  models_df<-models_df_dropinteracts
  }


models_df<-models_df %>% select(-c(minAICc,AIC))
minAICc<-models_df %>% group_by(varname) %>% dplyr::summarize(minAICc=min(AICc,na.rm=TRUE))
models_df<-merge(models_df,minAICc,by="varname")

models_df<-models_df %>% mutate(estimate=signif(estimate,digits=2),
                                std.error=signif(std.error,digits=2),
                                p.value=signif(p.value,digits=2))


models_df$AICcdiff<-models_df$AICc-models_df$minAICc
models_df<-models_df %>% select(-minAICc) 

models_df<-models_df %>% mutate(term = str_replace(term, "SWE_May_snotel", "SWE"),
                     term = str_replace(term, "Volume_m3", "volume"), 
                     term = str_replace(term, "Depth_mean_m", "depthmean"), 
                     term = str_replace(term, "ice_out_doy", "iceout_doy"),
                     varname = str_replace(varname, "SWE_May_snotel", "SWE"))

models_df$varname<-as.character(models_df$varname)




#models_top_df<-models_df %>% filter(AICcdiff<=1.99) %>% arrange(varname,AICcdiff)
#models_top_df<-models_df %>% filter(AICcdiff<=1.94) %>% arrange(varname,AICcdiff)



#write.csv(models_df,file=paste(fileout,".csv",sep=""),row.names=FALSE)
#write.csv(models_top_df,file=paste(fileout,"_top.csv",sep=""),row.names=FALSE)

#models_top_df_part1<- models_top_df %>% filter(varname %in% c("Watertemp","Secchi","Chl","DO","TDN","TDP","pH"))
#models_top_df_part2<- models_top_df %>% filter(varname %in% c("SO4","Cl","Ca","Mg","K","Na","Cond"))

#models_top_signif_df<-models_top_df %>% filter(term!= "(Intercept)" & p.value<=0.05) %>% select(varname,modname,modname2) %>% unique()
#models_1signif<-models_df %>% filter(term != "SWE_May_snotel" & nparam==min(models_df$nparam) & term!= "(Intercept)" & p.value<=0.05) %>% select(modname) %>% unique()
#models_2signif<-models_df %>% filter(term != "SWE_May_snotel" & nparam>min(models_df$nparam) & term!= "(Intercept)" & p.value<=0.05) %>% select(modname) %>% unique()


#write.csv(models_top_df_part1,file=paste(fileout,"_top_part1.csv",sep=""),row.names=FALSE)
#write.csv(models_top_df_part2,file=paste(fileout,"_top_part2.csv",sep=""),row.names=FALSE)
