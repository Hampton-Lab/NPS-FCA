#Random Forest Script
set.seed(71)

directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(directory,"/config/zoop_RF_config.yml")))

zoop_data <- read_csv(config$file_path$zoop_data)%>%
  mutate(event_year = as.character(event_year))%>%
  mutate(RAP = log(RAP+0.1),
         MICRO = log(MICRO+0.1),
         CLAD = log(CLAD+0.1),
         COPE = log(COPE+0.1))

env_zoop_data <- read_csv(config$file_path$environment_data)%>%
  mutate(event_year = as.character(event_year))%>%
  select(-site_code)%>%
  rename(site_code = Lake)%>%
  left_join(., zoop_data, by = c("park_code","site_code","event_year"))

park = config$attributes$park_code
taxa = config$attributes$taxa
site = config$attributes$site_code

RF_data <- env_zoop_data %>% rename(TN=`Total N`,
                                    TP=`Total P`)%>%
  select(taxa, site_code,event_year, park_code, BotTemp, MidTemp, SurfTemp, SWE_May,
         ice_free_days, DO_below2m, Chlorophyll,
         secchi_value_m)%>%
  filter(park_code == park)%>%
  mutate(light_attenuation = 1.7/(secchi_value_m),
         ice_days = 365 - ice_free_days)%>%
  select(-secchi_value_m,-ice_free_days)


for (i in colnames(RF_data[,c(1:12)])) {
  RF_data[,i] <- imputeTS::na_interpolation(RF_data[,i],option = "linear")
}



################################################################################
### RF_data$XXX NEEDS TO BE UPDATED BETWEEN EACH RUN. STILL WORKING ON IT ######

mtry <- tuneRF(RF_data[-c(1,2)],RF_data$MICRO, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

################################################################################

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
rf <-randomForest(unlist(RF_data[1]) ~., data = RF_data[-c(1,2)], mtry = best.m, importance=TRUE, ntree=1000)

importantce <- as.data.frame(importance(rf))%>%
  arrange(-IncNodePurity)%>%
  top_n(2)

saveRDS(importantce, paste0("./output/",park,"_node_purity_",taxa,".rds"))


pred = as.data.frame(unlist(predict(rf), recursive = T))

model_compare <- cbind(RF_data, pred)%>%
  dplyr::rename(predicted_density = `unlist(predict(rf), recursive = T)`,
                obs_density = taxa)%>%
  mutate(park_code = park)

eval <- model_compare %>%
  select(park_code, site_code, event_year, obs_density, predicted_density)%>%
  na.omit(.)%>%
  mutate(zoop_tested = taxa)

saveRDS(eval, paste0("./output/",park,"_global_random_forest_",taxa,".rds"))

fig <- ggplot()+
  geom_line(data = eval, aes(event_year,obs_density, group = site_code), color = "blue")+
  geom_line(data = eval, aes(event_year,predicted_density, group = site_code), color = "red")+
  facet_wrap(~site_code, scales = "free_y")+
  labs(title = paste0(park," ", taxa, " Predictions"))+
  ylab("ln(Density)+0.1")+
  xlab("Year")+
  theme_bw()

fig

ggsave(path = ".", filename = paste0("./figures/",taxa,"_predictions_RF_",park,".jpg"),
       width = 15, height = 7, device='jpg', dpi=200)
