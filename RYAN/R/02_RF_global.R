#Random Forest Script
set.seed(71)

directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(directory,"/config/zoop_RF_config.yml")))

zoop_data <- read_csv(config$file_path$zoop_data)%>%
             mutate(event_year = as.character(event_year))%>%
   mutate(RAP = log10(RAP+1),
          MICRO = log10(MICRO+1),
          CLAD = log10(CLAD+1),
          COPE = log10(COPE+1))

env_zoop_data <- read_csv(config$file_path$environment_data)%>%
  mutate(event_year = as.character(event_year))%>%
  left_join(., zoop_data, by = c("park_code","site_code","event_year"))

write_csv(env_zoop_data, "ZoopData.csv")

park = config$attributes$park_code
taxa = config$attributes$taxa

RF_data <- env_zoop_data %>%
  mutate(light_attenuation = 1.7/(secchi_value_m))%>%
  select(-secchi_value_m,-event_year)


for (i in colnames(RF_data[,c(1:23)])) {
  RF_data[,i] <- imputeTS::na_interpolation(RF_data[,i],option = "linear")
}

RF_data <- RF_data %>% group_by(site_code, year, park_code)%>%
  #summarize_all(funs(mean))%>%
  mutate(fish = ifelse((CCT_ever+TSS_ever+WCT_ever+RBT_ever+BRK_ever)>0,1,0))%>%
  select(-CCT_ever, -TSS_ever, -WCT_ever, -RBT_ever, -BRK_ever)%>%
  mutate(lake_temp = (SurfTemp+BotTemp+MidTemp)/3)%>%
  select(-SurfTemp,-BotTemp,-MidTemp)%>%
  ungroup(.)

com = RF_data[,11:14]
m_com = as.matrix(com)

set.seed(123)
ord = metaMDS(m_com, distance = "bray", trace = F, autotransform = F)

fit <- envfit(ord, RF_data[,-c(1,2,3,4,5,6,8,9,10,11,12,13,14)], perm = 999)
scores(fit, "vectors")
plot(ord)
plot(fit)
plot(fit, p.max = 0.05, col = "red")



# loading the vegan library in order to calculate distance matrices
library(vegan)

DATA1 <- data.frame(read.table(file='SSMDS.csv', sep=',', header=TRUE))
DATA <- DATA1[,c(2:14)]
#  Rename the non-ID part of the matrix;  it will save you some typing later
X <- DATA[c(1:28),c(2:14)]
Y <- DATA[c(28:56),c(2:14)]
Z <- DATA[c(56:84),c(2:14)]

# Running the NMDS

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including our old friend 'vegdist' frm the vegan package
par(ask=TRUE)
NMDS <- metaMDS(DATA, distance='bray', k=3, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
NMDS$stress
names(NMDS$ndis)
summary(NMDS)
NMDS$points
NMDS$species
#  Basic plot of all of the points
plot(NMDS,type='p')

#  Points divided into day vs. night
ordiplot(NMDS, display=c('sites','species'),choices=c(1,2), type='n')
points(NMDS$points[DATA$depth=='0.1',1], NMDS$points[DATA$depth=='0.1',2], pch=21,bg='purple')
points(NMDS$points[DATA$depth=='5',1], NMDS$points[DATA$depth=='5',2], pch=21,bg='sienna')
points(NMDS$points[DATA$depth=='9',1], NMDS$points[DATA$depth=='9',2], pch=21,bg='pink')

legend('bottomright',min(DATA$points[,1]), cex = 0.5, max(DATA$points[,2]), legend=c('0.1m', '5m', '9m'), pch=21, pt.bg=c('purple', 'sienna', 'pink'))
ordihull(NMDS, DATA$depth, lwd = 2)
ordiellipse(NMDS, DATA$depth, lwd = 4)

ordiplot(X, display=c('sites','species'),choices=c(1,3), type='n')
points(X$points[DATA$time=='night',1], X$points[DATA$time=='night',3], pch=21,bg='purple')
points(X$points[DATA$time=='day',1], X$points[DATA$time=='day',3], pch=21,bg='sienna')
legend('bottomright',min(X$points[,1]), max(X$points[,3]), legend=c('Night', 'Day'), pch=21, pt.bg=c('purple', 'sienna'))
ordihull(X, DATA$time, lwd = 2)
ordiellipse(X, DATA$time, lwd = 4)
#  Basic outline for making your own Scree plot

k <- c(1:6)
Stress <- c(0.2197126,0.2245967,0.2197362,0.225493,0.2275461,0.2192167)
plot(k, Stress, type='b', xlab='Dimensions', ylab='Stress', pch=21, bg='grey75', cex=1.3)

## Adding fitted arrows to CCA. We use "lc" scores, and hope
## that arrows are scaled similarly in cca and envfit plots
ord <- cca(m_com ~ Al + P + K, m_com)
plot(ord, type="p")
fit <- envfit(ord, varechem, perm = 999, display = "lc")
plot(fit, p.max = 0.05, col = "red")
## 'scaling' must be set similarly in envfit and in ordination plot
plot(ord, type = "p", scaling = "sites")
fit <- envfit(ord, varechem, perm = 0, display = "lc", scaling = "sites")
plot(fit, col = "red")


scree_plot(m_com)

plot(nmds)
envfit()

nmds_scores <- cbind(as_tibble(scores(nmds)),
                     RF_data[,-c(11,12,13,14)])

park_nmds_plot <- ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2,
                                     fill = park_code), size = 3, pch = 21) +
  scale_fill_manual(values = magma(15)[c(3, 8, 11)],
                    name = "Park") +
  annotate(geom = "label", x = -0.30, y = 1.25, size = 5,
           label = paste("Stress: ", round(nmds$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(size = 24))

RF_data <- RF_data %>% select(-park_code, -site_code)

zoop_dbrda <- dbrda(formula = select(RF_data, CLAD:RAP) ~
                      Chlorophyll + DO_below2m +
                      ice_free_days +
                      light_attenuation+
                      fish + lake_temp, data = RF_data,
                    distance = "bray")

summary(zoop_dbrda)

plot(zoop_dbrda)




#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))
data.scores$site_code = RF_data$site_code
data.scores$park_code = RF_data$park_code



plot(nmds)
envfit(nmds, RF_data[,-c(11,12,13,14)], add=TRUE)

subset_nmds <- metaMDS(comm = select(RF_data, CLAD:RAP),
                       distance = "bray", k = 2, trymax = 100)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(subset_nmds))


################################################################################
### RF_data$XXX NEEDS TO BE UPDATED BETWEEN EACH RUN. STILL WORKING ON IT ######

mtry <- tuneRF(RF_data[-c(1,2)],RF_data$RAP, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

################################################################################


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
rf <-randomForest(unlist(RF_data[1]) ~., data = RF_data[-c(1:2)], mtry = best.m, importance=TRUE, ntree=1000)

importance(rf)
varImpPlot(rf)




pred = as.data.frame(unlist(predict(rf), recursive = T))

model_compare <- cbind(env_zoop_data, pred)%>%
  dplyr::rename(predicted_density = `unlist(predict(rf), recursive = T)`,
                obs_density = taxa)

eval <- model_compare %>%
  select(park_code, site_code, event_year, obs_density, predicted_density)%>%
  na.omit(.)%>%
  mutate(zoop_tested = taxa)

saveRDS(eval, paste0("./output/",park,"_global_random_forest_",taxa,".rds"))

fig <- ggplot()+
  geom_line(data = eval, aes(event_year,obs_density, group = site_code), color = "blue")+
  geom_line(data = eval, aes(event_year,predicted_density, group = site_code), color = "red")+
  facet_wrap(~site_code, scales = "free_y")+
  theme_bw()

ggsave(path = ".", filename = paste0("./figures/",taxa,"_predictions_RF",park[s],".jpg"),
       width = 20, height = 16, device='jpg', dpi=400)

