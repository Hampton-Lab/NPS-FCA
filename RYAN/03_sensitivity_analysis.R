set.seed(42)

ZOOP_all <- env_zoop_data %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll, fish)%>%
  mutate(site_code = case_when(
    site_code == "LH15" ~ 1,
    site_code == "Allen" ~ 2,
    site_code == "LP19" ~ 3,
    site_code == "Deadwood" ~ 4,
    site_code == "Blue" ~ 5,
    site_code == "Blum" ~ 6,
    site_code == "Silent" ~ 7,
    site_code == "EasyRidge" ~ 8,
    site_code == "East" ~ 9,
    site_code == "Bowan" ~ 10,
    site_code == "Triplet" ~ 11,
    site_code == "Gladys" ~ 12,
    site_code == "Ferry" ~ 13,
    site_code == "Heather" ~ 14,
    site_code == "Crazy" ~ 15,
    site_code == "Milk" ~ 16,
    site_code == "LaCrosse" ~ 17,
    site_code == "Sunup" ~ 18,
    site_code == "Connie" ~ 19))%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish"))


ZOOP_fishless <- env_zoop_data %>% filter(fish == 0) %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
  mutate(site_code = case_when(
    site_code == "Allen" ~ 1,
    site_code == "Bowan" ~ 2,
    site_code == "Milk" ~ 3,
    site_code == "Silent" ~ 4,
    site_code == "Triplet" ~ 5,
    site_code == "Sunup" ~ 6,
    site_code == "Blum" ~ 7,
    site_code == "Connie" ~ 8,
    site_code == "Crazy" ~ 9,
    site_code == "East" ~ 10,
    site_code == "EasyRidge" ~ 11,
    site_code == "Ferry" ~ 12,
    site_code == "LaCrosse" ~ 13))%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))


ZOOP_fish <- env_zoop_data %>% filter(fish == 1) %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
  mutate(site_code = case_when(
    site_code == "Blue" ~ 1,
    site_code == "Gladys" ~ 2,
    site_code == "Heather" ~ 3,
    site_code == "LP19" ~ 4,
    site_code == "Deadwood" ~ 5,
    site_code == "LH15" ~ 6))%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))


# GLOBAL ZOOPLANKTON MODEL
#
zoop_taxa <- c("COPE", "CLAD", "MICRO", "RAP")
for(s in 1:length(zoop_taxa)){

  ZOOP_jags <- ZOOP_all %>% filter(variable == zoop_taxa[s])
  sites <- length(levels(as.factor(ZOOP_jags$site_code)))

  jagsdata <- with(ZOOP_jags, list(value = value, lake_temp = lake_temp, Ca = Ca, site_code = site_code,
                              Chlorophyll = Chlorophyll, fish = fish,  N = length(ZOOP_jags), sites = sites))

  init_values <- function(){
    list(alpha = rnorm(4), beta = rnorm(4), omega = rnorm(4), phi = rnorm(4), sigma = runif(1), sigma_a = runif(1))
  }

  params <- c("alpha", "beta", "omega", "phi", "sigma", "sigma_a")

  fit_lm_global <- jags(data = jagsdata, inits = init_values,
                      parameters.to.save = params, model.file = global_lme_jags,
                      n.chains = 3, n.iter = 200000, n.burnin = 2000, n.thin = 10,
                      DIC = F)

  fit_lm_global <- as.mcmc(fit_lm_global)
  gelman.diag(fit_lm_global)

  global_parameters <- fit_lm_global %>%
    spread_draws(sigma, sigma_a, `alpha[1]`, `alpha[2]`, `alpha[3]`, `alpha[4]`,
                 `beta[1]`, `beta[2]`, `beta[3]`, `beta[4]`,
                 `omega[1]`, `omega[2]`, `omega[3]`, `omega[4]`,
                 `phi[1]`, `phi[2]`, `phi[3]`, `phi[4]`) %>%
    rename(ensemble = .iteration)

  saveRDS(global_parameters, paste0("./output/global_parameters",zoop_taxa[s],".rds"))
}

# GLOBAL FISHLESS ZOOPLANKTON MODEL
#
zoop_taxa <- c("COPE", "CLAD", "MICRO", "RAP")
for(s in 1:length(zoop_taxa)){

  ZOOP_jags <- ZOOP_fishless %>% filter(variable == zoop_taxa[s])
  sites <- length(levels(as.factor(ZOOP_jags$site_code)))

  jagsdata <- with(ZOOP_jags, list(value = value, lake_temp = lake_temp, Ca = Ca, site_code = site_code,
                                   Chlorophyll = Chlorophyll,  N = length(ZOOP_jags), sites = sites))

  init_values <- function(){
    list(alpha = rnorm(1), beta = rnorm(1), omega = rnorm(1), phi = rnorm(1), sigma = runif(1), sigma_a = runif(1))
  }

  params <- c("alpha", "beta", "omega", "phi", "sigma", "sigma_a")

  fit_lm_fishless <- jags(data = jagsdata, inits = init_values,
                        parameters.to.save = params, model.file = global_fish_lme_jags,
                        n.chains = 3, n.iter = 200000, n.burnin = 2000, n.thin = 10,
                        DIC = F)

  fit_lm_fishless <- as.mcmc(fit_lm_fishless)
  plot(fit_lm_fishless)
  gelman.diag(fit_lm_fishless)

  fishless_parameters <- fit_lm_fishless %>%
    spread_draws(sigma, sigma_a, `alpha`,`beta`,`omega`,`phi`) %>%
    rename(ensemble = .iteration)

  saveRDS(fishless_parameters, paste0("./output/fishless_parameters",zoop_taxa[s],".rds"))
}

# GLOBAL FISH ZOOPLANKTON MODEL
#
zoop_taxa <- c("COPE", "CLAD", "MICRO", "RAP")
for(s in 1:length(zoop_taxa)){

  ZOOP_jags <- ZOOP_fish %>% filter(variable == zoop_taxa[s])
  sites <- length(levels(as.factor(ZOOP_jags$site_code)))

  jagsdata <- with(ZOOP_jags, list(value = value, lake_temp = lake_temp, Ca = Ca, site_code = site_code,
                                   Chlorophyll = Chlorophyll,  N = length(ZOOP_jags), sites = sites))

  init_values <- function(){
    list(alpha = rnorm(1), beta = rnorm(1), omega = rnorm(1), phi = rnorm(1), sigma = runif(1), sigma_a = runif(1))
  }

  params <- c("alpha", "beta", "omega", "phi", "sigma", "sigma_a")

  fit_lm_fish <- jags(data = jagsdata, inits = init_values,
                          parameters.to.save = params, model.file = global_fish_lme_jags,
                          n.chains = 3, n.iter = 200000, n.burnin = 2000, n.thin = 10,
                          DIC = F)

  fit_lm_fish <- as.mcmc(fit_lm_fish)
  plot(fit_lm_fish)
  gelman.diag(fit_lm_fish)

  fish_parameters <- fit_lm_fish %>%
    spread_draws(sigma, sigma_a, `alpha`,`beta`,`omega`,`phi`) %>%
    rename(ensemble = .iteration)

  saveRDS(fish_parameters, paste0("./output/fish_parameters",zoop_taxa[s],".rds"))
}




ZOOP_fishless_by_site <- env_zoop_data %>% filter(fish == 0) %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
  melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))

zoop_taxa <- c("COPE", "CLAD", "MICRO", "RAP")
sites <- c("Allen","Bowan","Milk","Silent","Triplet","Sunup","Blum","Connie","Crazy","East","EasyRidge",
           "Ferry","LaCrosse")

RMSE <- list()

for(g in 1:length(sites)){
 for(s in 1:length(zoop_taxa)){

  ZOOP_jags <- ZOOP_fishless_by_site %>% filter(variable == zoop_taxa[s]) %>%
    filter(site_code == sites[g])

  jagsdata <- with(ZOOP_jags, list(value = value, lake_temp = lake_temp, Ca = Ca,
                                   Chlorophyll = Chlorophyll,  N = length(ZOOP_jags)))

  init_values <- function(){
    list(alpha = rnorm(1), beta = rnorm(1), omega = rnorm(1), phi = rnorm(1), sigma = runif(1))
  }

  params <- c("alpha", "beta", "omega", "phi", "sigma")

  fit_lm_fish <- jags(data = jagsdata, inits = init_values,
                      parameters.to.save = params, model.file = site_specific_lme_jags,
                      n.chains = 3, n.iter = 200000, n.burnin = 2000, n.thin = 10,
                      DIC = F)

  traceplot(fit_lm_fish, mfrow = c(2, 3), ask = F)

  fit_lm_fish <- as.mcmc(fit_lm_fish)

  site_zoop_parameters <- fit_lm_fish %>%
    spread_draws(sigma,`alpha`,`beta`,`omega`,`phi`) %>%
    rename(ensemble = .iteration)

  predict_function <- function(alpha, beta, phi, omega, TEMP, Ca, Chlorophyll, Q){
    est <- alpha + (beta * TEMP) + (omega * Ca) + (phi * Chlorophyll) + rnorm(10000, 0, sd = Q)
    return(est)
  }

  pred_data_dist <- data.frame(matrix(NA, nrow = 10, ncol = 1000))
  event_year <- c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")

      for(h in 1:length(event_year)){

         parms <- sample_n(site_zoop_parameters, 1000, replace=TRUE)
         prediction_zoops <- ZOOP_jags %>% filter(event_year == event_year[h])

         pred_data_dist[h,] <- predict_function(alpha = parms$`alpha`, ## sample IC
                                                beta = parms$`beta`,
                                                omega = parms$`omega`,
                                                phi = parms$`phi`,
                                                TEMP = prediction_zoops$lake_temp,
                                                Ca = prediction_zoops$Ca,
                                                Chlorophyll = prediction_zoops$Chlorophyll,
                                                Q = parms$`sigma`)

      }

  pred_data_dist <- cbind(as.data.frame(event_year), pred_data_dist)%>%
    melt(., id.vars = "event_year")%>%
    group_by(event_year)%>%
    summarise(mean = mean(value),
              max = max(value),
              min = min(value),
              upper_95 = quantile(value, 0.95, na.rm = T),
              lower_95 = quantile(value, 0.05, na.rm = T),
              var = var(value))

  data_compare <- as.data.frame(left_join(ZOOP_jags[,c(1,2,3,8)], pred_data_dist, by = "event_year"))%>%ungroup(.)

  ggplot(data_compare)+
    geom_line(aes(x = event_year, y = mean, group = park_code), lwd = 2)+
    geom_point(aes(x = event_year, y = value, group = park_code), cex=7, color = "black", pch = 21, bg = "grey70")+
    geom_point(aes(x = event_year, y = mean, group = park_code), cex = 7, color = "black", fill = "blue", pch = 21)+
    labs(y = expression(paste("log_10(Abundance)")), x = "", title = paste0(sites[g],"_",zoop_taxa[s]))+
    theme_bw()+
    theme(text = element_text(size=15, color = "black"),
          axis.text = element_text(size = 15, color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(".figures/",sites[g],"_",zoop_taxa[s],".jpg"), width = 4, height = 4, units = "in", dpi = 300)

  RMSE[g[s]] <- rmse(data_compare$value, data_compare$mean)


 }
}
