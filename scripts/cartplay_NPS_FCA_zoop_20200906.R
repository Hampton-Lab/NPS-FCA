
library(rpart)

# import data, created a file outside of R with env variables
# and aggreaggated zooplankton data - needs to be re=created

dat1 <- read.csv("../data/bigjoin_taxon_varying_variables_wide.csv", header= TRUE, sep= ",");
head(dat1)
length(dat1$site_code.of.bigjoin_wide_varying_variables)

clad.tree = rpart(log.clad. ~  Chlorophyll + DOC +SurfTemp +ice_out_doy
                  + pH_below2m,
                 data = dat1);

printcp(clad.tree) ; plot(clad.tree); text(clad.tree, use.n= TRUE);
#cp.val.clad <-clad.tree$cptable[which.min(clad.tree$cptable[,"xerror"]),"CP"]
pclad.tree <- prune(clad.tree, cp = 0.031053); #from cptable
plotcp(pclad.tree)
rsq.rpart(pclad.tree)
printcp(pclad.tree); plot(pclad.tree); text(pclad.tree, use.n= TRUE);
png("clad.tree.png", width = 900, height = 900);
post(pclad.tree, file = "", title. = "pruned cladoceran tree",
     bp = 18); 
dev.off();
#####
cope.tree = rpart(log.copepod. ~  Chlorophyll + DOC +SurfTemp +ice_out_doy
                  + pH_below2m,
                  data = dat1);

printcp(cope.tree) ; plot(cope.tree); text(cope.tree, use.n= TRUE);
#cp.val.cope <-cope.tree$cptable[which.min(cope.tree$cptable[,"xerror"]),"CP"]
pcope.tree <- prune(cope.tree, cp = 0.040086);
plotcp(pcope.tree)
rsq.rpart(pcope.tree)
printcp(pcope.tree); plot(pcope.tree); text(pcope.tree, use.n= TRUE);
png("cope.tree.png", width = 900, height = 900);
post(pcope.tree, file = "", title. = "pruned copepod tree",
     bp = 18); 
dev.off();

#####
rot.tree = rpart(log.rotifer. ~  Chlorophyll + DOC +SurfTemp +ice_out_doy
                 + pH_below2m,
                  data = dat1);

printcp(rot.tree) ; plot(rot.tree); text(rot.tree, use.n= TRUE);
#cp.val.rot <-rot.tree$cptable[which.min(rot.tree$cptable[,"xerror"]),"CP"]
prot.tree <- prune(rot.tree, cp = 0.050843);
plotcp(prot.tree)
rsq.rpart(prot.tree)
printcp(prot.tree); plot(prot.tree); text(prot.tree, use.n= TRUE);
png("rot.tree.png", width = 900, height = 900);
post(prot.tree, file = "", title. = "pruned rotifer tree",
     bp = 18); 
dev.off();


library(randomForest)
randomForest.model<-randomForest(log.rotifer.~ Chlorophyll 
                                 + DOC +SurfTemp +ice_out_doy
                                 + pH_below2m + Ca +Cl + K + Mg
                                 + Na +Total.N + Total.P, 
                                 ntree=5000, 
                                 data=dat1,  na.action = na.omit)
varImpPlot(randomForest.model,
           main="Relative Importance of Predictors")
