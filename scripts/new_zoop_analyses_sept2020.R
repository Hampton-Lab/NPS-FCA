library(tidyverse)
library(readxl)
library(vegan)
library(labdsv)
library(visdat)
library(corrplot)
library(psych)
library(ggvegan)
library(janitor)


# 1. Load and prep data ---------------------------------------------------

bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")
zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")

#  Prep data
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))

# Crosswalk common names for sites to site_codes
zoop <- left_join(x = zoop, y = match_sites,
                  by = c("Lake" = "old_name")) %>%
  clean_names()

zoop_taxa <- zoop %>%
  group_by(park, year, lake, site_code, taxa) %>%
  summarise(density = sum(density)) %>%
  filter(taxa %in% c("COPE", "CLAD", "ROT"))

# Add a row identifier column, turn into matrix
zoop_comm_matrix <- zoop_taxa %>%
  mutate(park_site_year = paste0(park, "_", site_code, "_", year)) %>%
  ungroup() %>%
  select(park_site_year, taxa, density) %>%
  data.frame() %>%
  matrify()

head(zoop_comm_matrix)

zoops_comm_ids <- rownames_to_column(zoop_comm_matrix) %>%
  separate(col = rowname, into = c("park", "site", "year"), sep = "_")

zoops_comm_ids <- zoops_comm_ids %>%
  mutate(year = as.numeric(year))

# Take a subset of bigjoin
yearly_bigjoin <- bigjoin %>%
  filter(variable %in% c("secchi_value_m", "AirTemp", "SurfTemp", "DOC",
                         "Chlorophyll", "ice_out_doy", "Total N", "Total P",
                         "Ca")) %>%
  select(park_code, site_code, event_year, variable, value) %>%
  unique() %>%
  spread(key = variable, value = value) %>%
  mutate(n_p_ratio = (`Total N` / 14) / (`Total P` / 31))

# Add elevation to yearly_bigjoin. Don't spread to do this, because that results
# in empty rows for years where no data except elevation exists
bigjoin_elev <- bigjoin %>%
  select(park_code, site_code, Elevation_m, Volume_m3, solar_jas, 
         quartzmonzonite, salmo_adult_ever, flush_index_noSWE) %>%
  unique()

yearly_bigjoin <- left_join(x = yearly_bigjoin, y = bigjoin_elev,
                            by = c("park_code", "site_code"))

#rename columns
names(yearly_bigjoin)[names(yearly_bigjoin) == "Total P"] <- "total_P"
names(yearly_bigjoin)[names(yearly_bigjoin) == "Total N"] <- "total_N"
head(yearly_bigjoin)

# Join the zoops to yearly_bigjoin bc some data will be NA: both datasets
# should have corresponding rows
yearly_bigjoin_zoop <- inner_join(x = zoops_comm_ids, y = yearly_bigjoin,
                                  by = c("park" = "park_code",
                                         "site" = "site_code",
                                         "year" = "event_year"))

vis_dat(x = yearly_bigjoin_zoop)
summary(yearly_bigjoin_zoop)


# 2. Capscale -------------------------------------------------------------

# Now look at env variable correlations, but first check for and remove NAs
# Down some rows after removing NAs...
yearly_bigjoin_zoop <- na.omit(yearly_bigjoin_zoop)

corrplot(cor(select(yearly_bigjoin_zoop, AirTemp:SurfTemp, n_p_ratio, Elevation_m)),
         type = "upper", tl.col = "black", tl.srt = 45)

# How about keep SurfTemp, drop other temps?
corrplot(cor(select(yearly_bigjoin_zoop, SurfTemp, DOC,
                    secchi_value_m, ice_out_doy)),
         type = "upper", tl.col = "black", tl.srt = 45)

corr.test(x = select(yearly_bigjoin_zoop, SurfTemp, DOC, secchi_value_m,
                     Chlorophyll, ice_out_doy))


zoop_cap <- capscale(formula = select(yearly_bigjoin_zoop, CLAD:ROT) ~
                       SurfTemp + Chlorophyll + ice_out_doy + Elevation_m +
                       Volume_m3 + solar_jas + quartzmonzonite + salmo_adult_ever +
                       flush_index_noSWE,
                     data = yearly_bigjoin_zoop, distance = "bray")

# Capscale results
zoop_cap
vif.cca(zoop_cap)
# Greater detail
#summary(zoop_cap)

# Plot it:
# Have to combine the site scores with the original dataset so that it's plot-able
custom_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),
                                yearly_bigjoin_zoop)

autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = custom_site_points,
             aes(x = CAP1, y = CAP2, color = park, size = Elevation_m))

RsquareAdj(zoop_cap)

# https://sites.ualberta.ca/~ahamann/teaching/renr690/Lab9b.pdf
anova(zoop_cap)
# anova(zoop_dbrda, by = "axis", perm.max = 999) # Won't run bc negative eigenvalues
anova(zoop_cap, by = "margin", perm.max = 999)
RsquareAdj(zoop_cap)
#zoops<-select(zoops_comm_ids, Ascomorpha:Trichotria)
#head(zoops)
#PERMANOVA
#zoop_perm <- adonis(zoops ~ park * year, data=zoops_comm_ids)
#zoop_perm

zoop_cca <- cca(formula = select(yearly_bigjoin_zoop, CLAD:ROT) ~
                  SurfTemp + Chlorophyll + ice_out_doy + Elevation_m,
                data = yearly_bigjoin_zoop, distance = "bray")

# Capscale results
zoop_cca
vif.cca(zoop_cca)
RsquareAdj(zoop_cca)
plot(zoop_cca)
anova(zoop_cca, by="margin")


# 3. Linear modeling ------------------------------------------------------



# 3.1 log10 + 1 transformation on the response variable: ------------------

# SurfTemp + Chlorophyll + Ca + DOC + ice_out_day (ice_out_doy)

clad_l10_lm <- lm(formula = log10(CLAD + 1) ~ SurfTemp + Chlorophyll  + DOC + ice_out_doy,
                  data = yearly_bigjoin_zoop)

par(mfrow = c(2, 2))
plot(clad_l10_lm)

summary(clad_l10_lm)


cope_l10_lm <- lm(formula = log10(COPE + 1) ~ SurfTemp + Chlorophyll + DOC + ice_out_doy,
                  data = yearly_bigjoin_zoop)

par(mfrow = c(2, 2))
plot(cope_l10_lm)

summary(cope_l10_lm)


rot_l10_lm <- lm(formula = log10(ROT + 1) ~ SurfTemp + Chlorophyll + DOC + ice_out_doy,
                 data = yearly_bigjoin_zoop)

par(mfrow = c(2, 2))
plot(rot_l10_lm)

summary(rot_l10_lm)

# Quick visual of CLAD ~ SurfTemp
ggplot(data = yearly_bigjoin_zoop, aes(x = SurfTemp, y = log10(CLAD + 1))) +
  geom_point() +
  geom_smooth(method = "lm")


# 3.2 z-score the response variable: --------------------------------------

# How many records per lake?
yearly_bigjoin_zoop %>%
  count(site)

# Add z-scores
yearly_bigjoin_zoop_z <- yearly_bigjoin_zoop %>%
  group_by(site) %>%
  mutate(CLAD_z = scale(CLAD),
         COPE_z = scale(COPE),
         ROT_z = scale(ROT))

# Run the models
clad_z_lm <- lm(formula = CLAD_z ~ SurfTemp + Chlorophyll + DOC + ice_out_doy,
                data = yearly_bigjoin_zoop_z)

par(mfrow = c(2, 2))
plot(clad_z_lm)

summary(clad_z_lm)


cope_z_lm <- lm(formula = COPE_z ~ SurfTemp + Chlorophyll +  DOC + ice_out_doy,
                data = yearly_bigjoin_zoop_z)

par(mfrow = c(2, 2))
plot(cope_z_lm)

summary(cope_z_lm)


rot_z_lm <- lm(formula = ROT_z ~ SurfTemp + Chlorophyll +  DOC + ice_out_doy,
               data = yearly_bigjoin_zoop_z)

par(mfrow = c(2, 2))
plot(rot_z_lm)

summary(rot_z_lm)

library(rpart)

# import data, created a file outside of R with env variables
# and aggreaggated zooplankton data - needs to be re=created

head(yearly_bigjoin_zoop)

clad.tree = rpart((log10(CLAD + 1)) ~  Chlorophyll + DOC +SurfTemp +
                    ice_out_doy,
                  data = yearly_bigjoin_zoop);

printcp(clad.tree) ; plot(clad.tree); text(clad.tree, use.n= TRUE);
#cp.val.clad <-clad.tree$cptable[which.min(clad.tree$cptable[,"xerror"]),"CP"]
pclad.tree <- prune(clad.tree, cp = 0.014583); #from cptable
plotcp(pclad.tree)
rsq.rpart(pclad.tree)
printcp(pclad.tree); plot(pclad.tree); text(pclad.tree, use.n= TRUE);
png("clad.tree.png", width = 900, height = 900);
post(pclad.tree, file = "", title. = "pruned cladoceran tree",
     bp = 18); 
dev.off();
#####
cope.tree = rpart((log10(COPE + 1)) ~  Chlorophyll + DOC +SurfTemp +
                    ice_out_doy,
                  data = yearly_bigjoin_zoop);

printcp(cope.tree) ; plot(cope.tree); text(cope.tree, use.n= TRUE);
#cp.val.cope <-cope.tree$cptable[which.min(cope.tree$cptable[,"xerror"]),"CP"]
pcope.tree <- prune(cope.tree, cp = 0.029914); #from cptable
plotcp(pcope.tree)
rsq.rpart(pcope.tree)
printcp(pcope.tree); plot(pcope.tree); text(pcope.tree, use.n= TRUE);
png("cope.tree.png", width = 900, height = 900);
post(pcope.tree, file = "", title. = "pruned copepod tree",
     bp = 18); 
dev.off();

#####
rot.tree = rpart((log10(ROT + 1)) ~  Chlorophyll + DOC +SurfTemp 
                 +ice_out_doy ,
                 data = yearly_bigjoin_zoop);

printcp(rot.tree) ; plot(rot.tree); text(rot.tree, use.n= TRUE);
#cp.val.rot <-rot.tree$cptable[which.min(rot.tree$cptable[,"xerror"]),"CP"]
prot.tree <- prune(rot.tree, cp = 0.034435); #from randomforest result
plotcp(prot.tree)
rsq.rpart(prot.tree)
printcp(prot.tree); plot(prot.tree); text(prot.tree, use.n= TRUE);
png("rot.tree.png", width = 900, height = 900);
post(prot.tree, file = "", title. = "pruned rotifer tree",
     bp = 18); 
dev.off();
par(mfrow = c(1, 1))
randomForest.model<-randomForest((log10(CLAD + 1))~ Chlorophyll 
                                 + DOC +SurfTemp +ice_out_doy
                                 +total_N + total_P, 
                                 ntree=5000, 
                                 data=yearly_bigjoin_zoop,  
                                 na.action = na.omit)
varImpPlot(randomForest.model,
           main="Relative Importance of Predictors")

library(randomForest)
randomForest.model<-randomForest((log10(COPE + 1))~ Chlorophyll 
                                 + DOC +SurfTemp +ice_out_doy
                                 +total_N + total_P, 
                                 ntree=5000, 
                                 data=yearly_bigjoin_zoop,  
                                 na.action = na.omit)
varImpPlot(randomForest.model,
           main="Relative Importance of Predictors")


randomForest.model<-randomForest((log10(ROT + 1))~ Chlorophyll 
                                 + DOC +SurfTemp +ice_out_doy
                                   +total_N + total_P, 
                                 ntree=5000, 
                                 data=yearly_bigjoin_zoop,  
                                 na.action = na.omit)
varImpPlot(randomForest.model,
           main="Relative Importance of Predictors")


