rmse = function(m, o){
  sqrt(mean((m - o)^2))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PATH NEEDS TO BE CHANGED to ./data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_path <- "./output"


site_prediction_jags <- list.files(data_path, pattern = "MAIN_MCMC_JAGS_PREDICTION")%>%
  map(~ readRDS(file.path(data_path, .))) %>%
  data.table::rbindlist(fill = T)%>%
  na.omit(.)%>%
  left_join(., env_zoop_data, by = c("site_code","event_year", "park_code", "fish"))

evals <- site_prediction_jags %>%
  group_by(site_code, taxa)%>%
  summarize(RMSE = rmse(mean, value),
            var = mean(var))%>%
  left_join(., env_zoop_data, by = "site_code")

ggplot(evals, aes(taxa, var))+
  geom_boxplot(aes(fill=park_code))

ggplot(evals, aes(var, RMSE, group = taxa))+
  geom_point(aes(color = park_code))+
  facet_wrap(~taxa)

ggplot(site_prediction_jags, aes(mean, value, group = taxa))+
  geom_point(aes(color = site_code))+
  facet_wrap(~taxa)



CLAD_TS <- site_prediction_jags%>%filter(taxa == "CLAD")%>%
           ggplot(.)+
             geom_line(aes(x = event_year, y = mean, group = park_code, color = as.character(fish)), lwd = 1, color = "midnightblue")+
             geom_ribbon(aes(x = event_year, ymin = lower_95, ymax = upper_95, group = park_code), alpha = 0.2, fill = "midnightblue") +
             geom_point(aes(x = event_year, y = value, group = park_code), cex=4, color = "black", pch = 21, bg = "grey70")+
             geom_point(aes(x = event_year, y = mean, fill = as.character(fish)), cex = 4, color = "black", pch = 21)+
             labs(y = expression(paste("log_10(Abundance)")), x = "", title = "CLAD predictions")+
             theme_bw()+
             theme(text = element_text(size=15, color = "black"),
                   axis.text = element_text(size = 15, color = "black"),
                   axis.text.x = element_text(angle = 45, hjust = 1))+
             facet_wrap(~site_code)

site_prediction_jags %>% filter(taxa == "COPE")%>%
  ggplot(.)+
  geom_line(aes(x = event_year, y = mean, group = park_code, color = as.character(fish)), lwd = 1, color = "midnightblue")+
  geom_ribbon(aes(x = event_year, ymin = lower_95, ymax = upper_95, group = park_code), alpha = 0.2, fill = "midnightblue") +
  geom_point(aes(x = event_year, y = value, group = park_code), cex=4, color = "black", pch = 21, bg = "grey70")+
  geom_point(aes(x = event_year, y = mean, fill = as.character(fish)), cex = 4, color = "black", pch = 21)+
  labs(y = expression(paste("log_10(Abundance)")), x = "", title = "COPE predictions")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~site_code)

site_prediction_jags %>% filter(taxa == "RAP")%>%
  ggplot(.)+
  geom_line(aes(x = event_year, y = mean, group = park_code, color = as.character(fish)), lwd = 1, color = "midnightblue")+
  geom_ribbon(aes(x = event_year, ymin = lower_95, ymax = upper_95, group = park_code), alpha = 0.2, fill = "midnightblue") +
  geom_point(aes(x = event_year, y = value, group = park_code), cex=4, color = "black", pch = 21, bg = "grey70")+
  geom_point(aes(x = event_year, y = mean, fill = as.character(fish)), cex = 4, color = "black", pch = 21)+
  labs(y = expression(paste("log_10(Abundance)")), x = "", title = "RAP predictions")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~site_code)

site_prediction_jags %>% filter(taxa == "MICRO")%>%
  ggplot(.)+
  geom_line(aes(x = event_year, y = mean, group = park_code, color = as.character(fish)), lwd = 1, color = "midnightblue")+
  geom_ribbon(aes(x = event_year, ymin = lower_95, ymax = upper_95, group = park_code), alpha = 0.2, fill = "midnightblue") +
  geom_point(aes(x = event_year, y = value, group = park_code), cex=4, color = "black", pch = 21, bg = "grey70")+
  geom_point(aes(x = event_year, y = mean, fill = as.character(fish)), cex = 4, color = "black", pch = 21)+
  labs(y = expression(paste("log_10(Abundance)")), x = "", title = "MICRO predictions")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~site_code)


##### UNCERTAINTY PARTITIONING DATA ####

taxa <- c("COPE", "CLAD", "MICRO", "RAP")
for(i in 1:length(taxa)){

  TEMP <- list.files(data_path, pattern = paste0("TEMPERATURE_",taxa[i]))%>%
    map(~ readRDS(file.path(data_path, .))) %>%
    data.table::rbindlist(fill = T)

  CA <- list.files(data_path, pattern = paste0("CALCIUM_",taxa[i]))%>%
    map(~ readRDS(file.path(data_path, .))) %>%
    data.table::rbindlist(fill = T)

  CHLOROPHYLL <- list.files(data_path, pattern = paste0("CHLOROPHYLL_",taxa[i]))%>%
    map(~ readRDS(file.path(data_path, .))) %>%
    data.table::rbindlist(fill = T)

  INTERCEPT <- list.files(data_path, pattern = paste0("INTERCEPT_",taxa[i]))%>%
    map(~ readRDS(file.path(data_path, .))) %>%
    data.table::rbindlist(fill = T)

  partition <- cbind(TEMP[,c(1,2,3,4,11)], CA[,11], CHLOROPHYLL[,11], INTERCEPT[,11])
  names(partition) <- c("event_year", "site_code", "park_code","fish", "var_temp", "var_ca", "var_chla", "var_int")

  all_partitioned <- partition %>%
    mutate(sum_var = var_temp+var_ca+var_chla+var_int)%>%
    mutate(`Temperature` = var_temp/sum_var)%>%
    mutate(`Calcium` = var_ca/sum_var)%>%
    mutate(`Chlorophyll` = var_chla/sum_var)%>%
    mutate(`Intercept` = var_int/sum_var)%>%
    mutate(sum_check = `Temperature`+`Calcium`+`Chlorophyll`+`Intercept`)%>%
    select(event_year,site_code, park_code, fish, `Temperature`, `Calcium`, `Chlorophyll`, `Intercept`, sum_check)%>%
    ungroup(.)

  all_partitioned_melt <- all_partitioned%>%
    select(-sum_check)%>%
    melt(., id = c("event_year","site_code","park_code","fish"))%>%
    na.omit(.)

  c <- all_partitioned_melt%>%
    group_by(variable)%>%
    ggplot(., aes(x = event_year, y = value, group=variable, fill = variable))+
    geom_area(aes(fill = variable))+
    scale_fill_manual(values = c("#E69F00", "#D55E00", "#CC79A7", "#56B4E9"))+
    theme_bw()+
    labs(title = paste0("Partitioned ",taxa[i]," uncertainty of each lake"))+
    ylab("Proportion of total variance")+
    xlab("")+
    theme(axis.text=element_text(size=15, color = "black"),
          axis.text.x=element_text(size=15, angle = 45, hjust = 1),
          axis.title=element_text(size=15, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(),
          title = element_text(size = 15), legend.position = "top",
          legend.text = element_text(size = 10, color = "black"))+
    facet_wrap(~site_code)

  c

    ggsave(paste0("./figures/partitioned_covariate_paramaters",taxa[i],".jpg"), width = 10, height = 8, units = "in", dpi = 1000)

}


