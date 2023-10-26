#Random Forest Script
set.seed(71)

directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(directory,"/config/zoop_RF_config.yml")))

zoop_data <- read_csv(config$file_path$zoop_data)%>%
  mutate(event_year = as.character(event_year))%>%
  mutate(RAP = sqrt(RAP),
         MICRO = sqrt(MICRO),
         COPE = sqrt(COPE),
         CLAD = sqrt(CLAD))

env_zoop_data <- read_csv(config$file_path$environment_data)%>%
  mutate(event_year = as.character(event_year))%>%
  select(-site_code)%>%
  rename(site_code = Lake)%>%
  left_join(., zoop_data, by = c("park_code","site_code","event_year"))

taxa = config$attributes$taxa

RF_data <- env_zoop_data %>% rename(NH4=`NH4-N`,
                                    NO3=`NO3-N`,
                                    TN=`Total N`,
                                    TP=`Total P`)%>%
  select(taxa,park_code,site_code,event_year,AirTemp,BotTemp,Ca,Chlorophyll,
         DO_top2m,DOC,K,MidTemp,NH4,NO3,TN,TP,PO4,secchi_value_m,SO4,SurfTemp,
         TDS)%>%
  mutate(light_attenuation = 1.7/(secchi_value_m))%>%
  select(-secchi_value_m, -site_code,-event_year, -park_code)

#mutate(noquote(taxa[s]) = ifelse(noquote(taxa[s]) == 0, 0.01, noquote(taxa[s])))

for (i in colnames(RF_data[,c(1:18)])) {
  RF_data[,i] <- imputeTS::na_interpolation(RF_data[,i],option = "linear")
}



################################################################################
### RF_data$XXX NEEDS TO BE UPDATED BETWEEN EACH RUN. STILL WORKING ON IT ######

mtry <- tuneRF(RF_data[-1],RF_data$COPE, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

################################################################################


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
rf <-randomForest(unlist(RF_data[1]) ~., data = RF_data[-1], mtry = best.m, importance=TRUE, ntree=1000)

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

saveRDS(eval, paste0("./output/global_random_forest_",taxa,".rds"))

fig <- ggplot()+
  geom_line(data = eval, aes(event_year,obs_density, group = site_code), color = "blue")+
  geom_line(data = eval, aes(event_year,predicted_density, group = site_code), color = "red")+
  facet_wrap(~site_code, scales = "free_y")+
  theme_bw()

ggsave(path = ".", filename = paste0("./figures/",taxa,"_predictions_RF.jpg"),
       width = 20, height = 16, device='jpg', dpi=400)
}
