  for (i in 1:length(covars[1,])){
    print("###############################################")
    print(i)
    daterr<-data
    daterr$value_formodel<-daterr[,which(names(daterr)==valuecol)]
    daterr$swe_formodel<-daterr[,which(names(daterr)==swecol)]
    daterr$iceoutd_formodel<-daterr[,which(names(daterr)==iceoutdcol)]
    
    covarsi<-covars[,i];
    daterr<-daterr %>% dplyr::select(value_formodel,swe_formodel,iceoutd_formodel,covarsi,park_site,park_code,year,snowyr) %>% na.omit()
    daterr<-daterr %>% unique()
    daterr$nothing<-1
    
    print(c(paste(i,varname,covars[,i])))
    
    fixedpart_named<-fixedpart
    for (j in 1:length(covarsi)){
      if(j==1){fixedpart_named<-gsub("vari1",covarsi[1],fixedpart_named)}
      if(j==2){fixedpart_named<-gsub("vari2",covarsi[2],fixedpart_named)} 
      if(j==3){fixedpart_named<-gsub("vari3",covarsi[3],fixedpart_named)} 
    }
    
    modelformula<-formula(paste("value_formodel~",fixedpart_named,sep=""))
    
    if(length(covarsi)==2 & 
       identical(daterr[,which(names(daterr)==covarsi[1])],daterr[,which(names(daterr)==covarsi[2])])==TRUE){
      print("perfectly correlated vars...");print(covarsi);print("on to the next set...")
      next()}
    
      mi<-glm(na.action=na.omit, modelformula, data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
      mi_glance<-glance(glm(na.action=na.omit, modelformula, data = daterr)) %>% as.data.frame() #%>% mutate(model=paste("2_",i,sep="")) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
      covars_ordered<-names(mi$coefficients)[1+c(1:length(covarsi))]
      #miR<-1-mi$deviance/mi$null.deviance
#      miR2<-r2beta(mi)$Rsq[1] # the marginal r2, Nakagawa and Schielzeth
      miR2m<-r.squaredGLMM(mi)[1,1] %>% as.numeric() # the marginal r2
      miR2c<-r.squaredGLMM(mi)[1,2] %>% as.numeric() # the conditional r2
      mi0<-mi
      
      mi.relaimpo<-c(1)
      class(mi.relaimpo)<-"try-error"
      if(length(covarsi)>1){
        mi.relaimpo<-try(calc.relimp(mi),TRUE)
        if(class(mi.relaimpo)!="try-error"){
          mi.relaimpo.lmg<-sort(mi.relaimpo$lmg,decreasing=TRUE)
          termorder<-c("(Intercept)",names(mi.relaimpo.lmg))
          covars_ordered<-termorder[1+c(1:length(covarsi))]
          if(length(grep("\\:",termorder))>0){
            covars_ordered<-termorder[-grep("\\:",termorder)][1+c(1:length(covarsi))]
          }
        }
      }
      mi<-summary(mi)$coefficients %>% as.data.frame()
      names(mi)<-c("Value",  "Std.Error", "t-value",      "p-value")
      mi<-data.frame(varname=rep(varname,length(mi[,1])),term=row.names(mi),mi,row.names=NULL)
      
    if(usenlme==1){
    mi<-lme(na.action=na.omit, modelformula, random = randompart, method="ML",data=daterr) #%>% as.data.frame() #%>% filter(effect!="ran_pars")# %>% as.data.frame() %>% mutate(model=paste("2_",i,sep=""))
    mi_coef<-summary(mi)$tTable %>% as.data.frame()    
    mi_glance<-glance(lme(na.action=na.omit, modelformula, random = randompart, method="ML",data=daterr)) %>% as.data.frame() #%>% mutate(model=paste("2_",i,sep="")) %>% dplyr::select(variable,sigma,logLik,AIC,BIC,model)
  
    miR2m<-r.squaredGLMM(mi)[1,1] %>% as.numeric() # the marginal r2
    miR2c<-r.squaredGLMM(mi)[1,2] %>% as.numeric() # the conditional r2
    
     
 #   return(mi)
#    getmi
    
#    rbeta(lme(na.action=na.omit, modelformula, random = randompart, method="ML",data=daterr))
    
#    rbetai<-r2beta(lme(na.action=na.omit, modelformula, random = randompart, method="ML",data=daterr))
    rbetai<-try(r2beta(mi),TRUE)
    if(class(rbetai)!="try-error"){
    rbetai<-r2beta(mi) %>% as.data.frame()
    
    rbetai<-rbetai[order(rbetai$F)]
    termorder<-as.character(rbetai$Effect) 
    rbetai_main<-rbetai[-grep(":",rbetai$Effect),]
    
    covars_ordered<-as.character(rbetai_main$Effect)
    termorder<-termorder[-which(termorder=="Model")]
    covars_ordered<-covars_ordered[-which(covars_ordered=="Model")]
    
    termorder<-c("(Intercept)",termorder)
    covars_ordered<-termorder[-grep("(Intercept)",termorder)]
    
    if(length(grep("\\:",termorder))>0){
      covars_ordered<-covars_ordered[-grep("\\:",covars_ordered)]
    }
    }


    
 #   return(mi)
 #   print(c("rbetai ",rbetai))
    
#    %>% as.data.frame()
    
#    vars <- insight::get_variance(mi)
#    vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual)
#    miR<-r.squaredGLMM(mi)[1]
#    mi<-rsquared(mi)
    mi0<-mi
    mi<-summary(mi)$tTable %>% as.data.frame()
    names(mi)<-c("Value",  "Std.Error", "df","t-value",      "p-value")
    mi<-data.frame(varname=rep(varname,length(mi[,1])),term=row.names(mi),mi,row.names=NULL)
    }
    if(length(covarsi)>1 & class(mi.relaimpo)!="try-error"){
    mi<-mi[match(mi$term,termorder),]
    }
      
    mi$sigma<- mi_glance$sigma
    mi$logLik<- mi_glance$logLik
    mi$AIC<- mi_glance$AIC
    mi$BIC<- mi_glance$BIC
    mi$deviance<- mi_glance$deviance
    mi$R2m<-miR2m
    mi$R2c<-miR2c
    mi$nfix<-length(mi[,1])
    mi<-mi %>% rename(std.error=Std.Error,estimate=Value)#,df=DF)

    mi$nparam<-mi$nfix#+2
    mi$modname<-paste(varname,"_",fixedpart_named,sep="")
    mi$modname2<-fixedpart
    mi$modelformula<-paste(as.character(modelformula)[2],as.character(modelformula)[1],as.character(modelformula)[3])
    mi$randompart<-as.character(randompart)
    mi$valuecol<-valuecol
    mi$var0<-""
    mi$var0[which(mi$term =="(Intercept)")]<-0
    mi$var1<-""
    mi$var1[which(mi$term %in% covars_ordered[1])]<-1
    mi$var2<-""
    mi$var2[which(mi$term %in% covars_ordered[2])]<-2
    mi$var3<-""
    mi$var3[which(mi$term %in% covars_ordered[3])]<-3
    
    varint_length<-length(grep(":",mi$term))
    mi$varint1<-""
    mi$varint2<-""
    mi$varint3<-""
    mi$varint1[grep(":",mi$term)[1]]<-4
    mi$varint2[grep(":",mi$term)[2]]<-5
    mi$varint3[grep(":",mi$term)[3]]<-6

    if(i==1){m_covar<-mi}
    if(i>1){m_covar<-rbind(m_covar,mi)}
  }
  
  m_covar$nobs<-length(is.na(daterr$value_formodel)==FALSE)
  m_covar<-m_covar %>% mutate(AICc=(-2*logLik)+(2*nparam)+(2*nparam*(nparam+1)/(nobs-nparam-1)))
  minAICc<-m_covar %>% dplyr::summarize(miAICc=min(AICc,na.rm=TRUE))
  m_covar$minAICc<-rep(minAICc,length(m_covar[,1])) %>% as.numeric()
  
  bestmodel<-m_covar %>% filter(AICc==minAICc) %>% dplyr::select(varname,modname,modelformula,valuecol) %>% unique()
  bestmodel$fixedpart<-substr(bestmodel$modname,nchar(varname)+2,nchar(bestmodel$modname))
  bestmodel_covars<-as.character(strsplit(bestmodel$modelformula,c("\\:|\\+|\\*|\\~"))[[1]])
  bestmodel_covars<-gsub(" ","",bestmodel_covars)
  bestmodel_covars<-gsub("\\(|\\)| |\\^2","",bestmodel_covars)
  bestmodel_covars<-bestmodel_covars[-c(1)]
  bestmodel$ncovars<-length(bestmodel_covars)
  
  daterr<-data
  daterr$value_formodel<-daterr[,which(names(daterr)==bestmodel$valuecol)]
  daterr<-daterr[,which(names(daterr) %in% c("value_formodel","swe_formodel","iceoutd_formodel","park_site","park_code","year","snowyr",
                                             "parklakenum","short_code",bestmodel_covars))]
  daterr<-daterr %>% na.omit() %>% unique()
  daterr$nothing<-1
  bestmodelformula<-formula(bestmodel$modelformula)
  
  daterr$var1<-daterr[,which(names(daterr) == bestmodel_covars[1])]
  if(length(bestmodel_covars)==2){
    daterr$var2<-daterr[,which(names(daterr) == bestmodel_covars[2])]
  }
  if(length(bestmodel_covars)==3){
    daterr$var2<-daterr[,which(names(daterr) == bestmodel_covars[2])]
    daterr$var3<-daterr[,which(names(daterr) == bestmodel_covars[3])]
  }
  
  mi<-glm(na.action=na.omit, bestmodelformula, data=daterr) 
  mi_1stvar<-glm(na.action=na.omit, value_formodel~var1, data=daterr) 
  daterr$fitted_1stvar<-as.numeric(predict(mi_1stvar))
  daterr$resid_1stvar<-as.numeric(residuals(mi_1stvar))
  
  if(usenlme==1){
    mi<-lme(na.action=na.omit, bestmodelformula, random = randompart, method="ML",data=daterr) 
    mi_1stvar<-lme(na.action=na.omit, value_formodel~var1, random = randompart, method="ML",data=daterr) 
#    daterr$fitted_1stvar<-as.numeric(mi_1stvar$fitted[,1])#as.numeric(predict(mi_1stvar))
#    daterr$resid_1stvar<-mi_1stvar$data$value_formodel-as.numeric(mi_1stvar$fitted[,1])#as.numeric(residuals(mi_1stvar))
    daterr$fitted_1stvar<-summary(mi_1stvar)$tTable[1,1]+summary(mi_1stvar)$tTable[2,1]*mi_1stvar$data$var1
    daterr$fitted_1stvar<-daterr$value_formodel-daterr$fitted_1stvar
  }
  daterr$fitted<-as.numeric(predict(mi))
  daterr$resid<-as.numeric(residuals(mi))

  
  
  if(length(bestmodel_covars)>=2){
  mi_2ndvar<-glm(na.action=na.omit, resid_1stvar~var2, data=daterr) 
  if(usenlme==1){
  mi_2ndvar<-lme(na.action=na.omit, resid_1stvar~var2, random = randompart, method="ML",data=daterr) 
  }
  daterr$fitted_2ndvar<-as.numeric(predict(mi_2ndvar))
  daterr$resid_2ndvar<-as.numeric(residuals(mi_2ndvar))
  }
  
  daterr$parklakenum<-as.factor(daterr$parklakenum)
  
  row.names(daterr)<-NULL
  daterr_wallgrp<-daterr
  daterr_wallgrp$park_site<-"all data"
  daterr_parkgrp<-daterr
  daterr_parkgrp$park_site<-paste("all",daterr_parkgrp$park_code)
  daterr_wallgrp<-rbind(daterr,daterr_wallgrp,daterr_parkgrp)
  
  daterr_wallgrp$park_site_<-substr(daterr_wallgrp$park_site,1,15)
  daterr_wallgrp$park_site2<-substr(daterr_wallgrp$park_site,1,15)
  daterr_wallgrp$park_site2[which(daterr_wallgrp$park_site2=="all data")]<-NA
  daterr_wallgrp$park_site2[which(daterr_wallgrp$park_site2=="all OLYM")]<-NA
  daterr_wallgrp$park_site2[which(daterr_wallgrp$park_site2=="all NOCA")]<-NA
  daterr_wallgrp$park_site2[which(daterr_wallgrp$park_site2=="all MORA")]<-NA
  
  maxxy<-max(c(daterr_wallgrp$value_formodel,daterr_wallgrp$fitted),na.rm=TRUE)
  minxy<-min(c(daterr_wallgrp$value_formodel,daterr_wallgrp$fitted),na.rm=TRUE)
  
  bestmodel_covars<-gsub("ice_out_doy","Iceout DOY", bestmodel_covars)

if(doplots==1) {
  bestmodel$fixedpart<-gsub("\\*","X",bestmodel$fixedpart)
  bestmodel$fixedpart<-gsub("\\^","",bestmodel$fixedpart)
  
  bestmodelfile_preds<-paste("../figures/analysispowers/best_fitval_",varname,"_",bestmodel$ncovars,"var_",
                             bestmodel$fixedpart,".png",sep="")
  
  plotit<-ggplot(daterr_wallgrp, aes(x = fitted, y = value_formodel, col=park_site2))+#,group=nothing)) +
    geom_point(alpha = 0.5,daterr_wallgrp, mapping=aes(x = fitted, y = value_formodel,shape=snowyr))+
    facet_wrap(~park_site_,ncol=5)+
    guides(shape=FALSE)+
    scale_color_viridis_d(end = 0.85,na.value="dark gray")+
    theme_bw()+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab(paste("Observed",varname))+xlab(paste("Predicted",varname))+
    xlim(c(minxy,maxxy))+ylim(c(minxy,maxxy))+
    geom_abline(slope=1,intercept=0,linetype="dotted")
    
  png(filename = bestmodelfile_preds, width = 7, height = 7, units = "in",res=300)
  print(plotit)
  dev.off()
  
  for(j in 1:length(bestmodel_covars)){
  
    if(j==1){daterr$var<-daterr$var1}
    if(j==2){daterr$var<-daterr$var2}
    if(j==3){daterr$var<-daterr$var3}
    if(j==1){daterr_wallgrp$var<-daterr_wallgrp$var1}
    if(j==2){daterr_wallgrp$var<-daterr_wallgrp$var2}
    if(j==3){daterr_wallgrp$var<-daterr_wallgrp$var3}
    
    bestmodelfile1<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"var_",
                          j,bestmodel_covars[1],"_",bestmodel$fixedpart,".png",sep="")
    
    plotit1<-ggplot(daterr, aes(x = var, y = value_formodel, col=park_site))+#,shape=snowyr)) +
      geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var, y = y.hat),alpha=0.5) + 
      geom_point(alpha = 0.5,aes(x = var, y = value_formodel, col=park_site,shape=snowyr))+
      guides(shape=FALSE)+
      theme_bw()+
      xlab(bestmodel_covars[j])+ylab(varname)
    png(filename = bestmodelfile1, width = 9, height = 4, units = "in",res=300)
    print(plotit1)
    dev.off()  
    
    
  bestmodelfile1_bylake<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"varbylake_",
                        j,bestmodel_covars[j],"_",bestmodel$fixedpart,".png",sep="")
  
  plotit1b<-ggplot(daterr_wallgrp, aes(x = var, y = value_formodel, col=park_site))+#,shape=snowyr)) +
    geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var, y = y.hat),alpha=0.5) + 
    geom_point(alpha = 0.5,aes(x = var, y = value_formodel, col=park_site2))+#,shape=snowyr))+
    facet_wrap(~substr(park_site,1,15)) +
    scale_colour_viridis_d(end = 0.85,na.value="dark gray")+
        theme(legend.position="none")+
    guides(shape=FALSE,color=FALSE)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab(bestmodel_covars[j])+ylab(varname)
  png(filename = bestmodelfile1_bylake, width = 7, height = 7, units = "in",res=300)
  print(plotit1b)
  dev.off()
  
  bestmodelfile1_bypark<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"varbypark_",
                               j,bestmodel_covars[j],"_",bestmodel$fixedpart,".png",sep="")
  
  plotit1c<-ggplot(daterr, aes(x = var, y = value_formodel,col=park_site,label=short_code))+#, col=park_site))+#,shape=snowyr)) +
    geom_line(data = cbind(daterr, y.hat = predict(mi)), aes(x = var, y = y.hat),alpha=0.5) + 
    geom_text(aes(x = var, y = value_formodel),size=2,alpha=1)+#, col=park_site))+#,shape=snowyr))+
    facet_wrap(~substr(park_code,1,4)) +
    scale_colour_viridis_d(end = 0.85)+
    theme(legend.position="none")+
    guides(shape=FALSE,color=FALSE)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab(bestmodel_covars[j])+
    ylab(varname)
  png(filename = bestmodelfile1_bypark, width = 10, height = 4, units = "in",res=300)
  print(plotit1c)
  dev.off()
  

  
  if(j>1){
#    mi_simpler_resid2<-glm(na.action=na.omit, residuals~var2, data=mi_simpler$model) 
#    if(usenlme==1){
#    mi_simpler_resid2<-lme(na.action=na.omit, residuals~var2, random = randompart, method="ML",data=mi_simpler$model) 
#    }

  if(j==2){daterr$varA<-daterr$var1
    daterr$varB<-daterr$var2
    bestmodelfile1<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"resid_var_",
                          j,bestmodel_covars[1],"_",bestmodel$fixedpart,".png",sep="")
    }
  if(j==3){daterr$varA<-daterr$var1
    daterr$varB<-daterr$var3
    bestmodelfile1<-paste("../figures/analysispowers/best_",varname,bestmodel$ncovars,"resid_var_",
                          j,bestmodel_covars[2],"_",bestmodel$fixedpart,".png",sep="")
    }
  
  daterr0<-daterr
  daterr0$fitted<-fitted(mi_1stvar)
  daterr0$resid<-resid(mi_1stvar)
  daterrA<-daterr0
  daterrB<-daterr0
  daterrC<-daterr0
  
  daterrA$X<-daterr0$varA
  daterrA$Y<-daterr0$value_formodel
  daterrA$panel<-"A.  Y ~ X1"
  daterrB$X<-daterr0$varB
  daterrB$Y<-daterr0$resid_1stvar
  daterrB$panel<-"B.  Residuals ~ X2"
#  daterrC$X<-daterr0$varB
#  daterrC$Y<-daterr0$resid
 # daterrC$panel<-"C.  residuals ~ X2"
  daterrABC<-rbind(daterrA,daterrB)#,daterrC)
#  daterrABC$panel<-factor(daterrABC$panel,levels=c("y~varA","y~varB","resid~varB"))

#  mi_2ndvarplot<-glm(na.action=na.omit, Y~X, data=daterrB) 
#  if(usenlme==1){
#    mi_2ndvarplot<-lme(na.action=na.omit, Y~X, random = randompart, method="ML",data=daterrB) 
#  }
  
  plotitABC<-ggplot(daterrABC, aes(x = X, y = Y, col=park_site))+#,shape=snowyr)) ++ 
#    stat_smooth(geom='line', method="lm",alpha=alph, se=FALSE)+
    geom_line(data = cbind(daterrA, y.hat = predict(mi_1stvar)), aes(x = X, y = y.hat),size=0.3)+
    geom_line(data = cbind(daterrB, y.hat = predict(mi_2ndvar)), aes(x = X, y = y.hat),size=0.3)+
    geom_smooth(method="lm",se=FALSE,data = daterrA, aes(x = X, y = Y),size=1.5,color="dark gray")+
    geom_smooth(method="lm",se=FALSE,data = daterrB, aes(x = X, y = Y),size=1.5,color="dark gray")+   
#    geom_smooth(method="lm",se=FALSE,data=daterrA,aes(x=X,y=resid_1stvar),size=0.3)+#,alpha=0.5)+        
#    geom_smooth(method="lm",se=FALSE,data=daterrB,aes(x=X,y=resid_1stvar),size=0.3)+#,alpha=0.5)+        
#    geom_line(data = daterrB,aes(x = X, y = resid_1stvar),size=0.3)+#alpha=0.5) + 
#    geom_smooth(method="lm",se=FALSE,data=daterrB,aes(x=X,y=resid_1stvar),size=0.3)+#,alpha=0.5)+    
#    geom_line(data = cbind(daterrB, y.hat = predict(mi_2ndvar)), aes(x = X, y = y.hat),size=1.5,color="dark gray")+#alpha=0.5) + 
    geom_point(alpha = 0.5)+
    facet_wrap(~panel,scales = "free",nrow=21)+
    scale_colour_viridis_d(end = 0.85)+
    guides(shape=FALSE,col = guide_legend(ncol = 1,title ="site"))+
    theme_bw()+
    xlab("X1 or X2")+ylab("Y or Residuals")
#    xlab("")+ylab("")

  png(filename = bestmodelfile1, width = 6, height = 7, units = "in",res=300)
  print(plotitABC)
  dev.off()  
  
  }
  
  }
  
}
  
#m_covar<-m_covar %>% select(-df,-sigma)

