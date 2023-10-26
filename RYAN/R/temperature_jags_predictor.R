
model.ar = ("ar_model.txt")
jagsscript = cat("
model {

   #priors===================================================

   mu2 ~ dnorm(0,1e-6)
   sd.pro ~ dunif(0, 1000)
   phi ~ dnorm(0,1e-6)
   omega ~ dnorm(0,1e-6)

   #Informative priors on initial conditions based on first observation
   X[1] ~ dnorm(x_init, tau.obs[1])

   #end priors===============================================

   for(i in 2:N) {

      #process model=============================================

      tau.pro[i] <- 1/((sd.pro)*(sd.pro))
      predX[i] <- mu2 + phi*X[i-1] + omega*D[i]
      X[i] ~ dnorm(predX[i],tau.pro[i])


      #end of process model======================================

      #data model================================================

      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model.ar)

# GET SITE NAMES

# FORECAST HORIZON
for(s in 1:length(lakes)){
  # Select site
  site_data <- data_join %>%
    filter(file_name == lakes[s])%>%
    mutate(bottom_temp_se = ifelse(bottom_temp_se==0,NA,bottom_temp_se))

  for (i in colnames(site_data[,c(3:6)])) {
    site_data[,i] <- imputeTS::na_interpolation(site_data[,i],option = "linear")
  }

  jags.data.ar = list(x_init = site_data$bottom_temp_mean[1],
                      Y = site_data$bottom_temp_mean,
                      tau.obs = 1/((site_data$bottom_temp_se)) ^ 2,
                      N = nrow(site_data),
                      D = site_data$air_temp_mean)

  nchain = 3
  chain_seeds <- c(200,800,1400)
  init <- list()
  for(i in 1:nchain){
    init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                      mu2 = runif(1, -20,0),
                      omega = runif(1, 0.3,1),
                      phi = runif(1, -0.2, 0.3),
                      .RNG.name = "base::Wichmann-Hill",
                      .RNG.seed = chain_seeds[i])
  }

  j.model   <- jags.model(file = model.ar,
                          data = jags.data.ar,
                          inits = init,
                          n.chains = 3)

  evaluation  <- coda.samples(model = j.model,
                            variable.names = c("predX", "sd.pro", "mu2", "phi", "omega"),
                            n.iter = 10000, n.burnin = 1000, thin = 10)

  # #plot(eval_ebu)
  # print("SS MODEL DIAGNOSTICS")
  # print(gelman.diag(evaluation))
  # plot(evaluation)

  parameters <- evaluation %>%
    spread_draws(sd.pro, mu2, phi, omega) %>%
    filter(.chain == 1)

  confidence_interval_prediction <- evaluation %>%
    spread_draws(predX[date]) %>%
    filter(.chain == 1) %>%
    rename(ensemble = .iteration) %>%
    mutate(time = site_data$date[date]) %>%
    ungroup() %>%
    select(time, predX, ensemble)%>%
    group_by(time) %>%
    summarise(mean = mean(predX),
              upper_90 = quantile(predX, 0.90),
              lower_90 = quantile(predX, 0.10),
              upper_80 = quantile(predX, 0.80),
              lower_80 = quantile(predX, 0.20),
              upper_70 = quantile(predX, 0.70),
              lower_70 = quantile(predX, 0.30),
              upper_60 = quantile(predX, 0.60),
              lower_60 = quantile(predX, 0.40),
              var = var(predX),
              sd = sd(predX),.groups = "drop")

  saveRDS(confidence_interval_prediction, paste0("temperature_prediction_",lakes[s],"_.rds"))

  ggplot(confidence_interval_prediction, aes(x = time, y = mean))+
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.2, fill = "midnightblue") +
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.2, fill = "midnightblue") +
    geom_ribbon(aes(ymin = lower_70, ymax = upper_70), alpha = 0.2, fill = "midnightblue") +
    geom_ribbon(aes(ymin = lower_60, ymax = upper_60), alpha = 0.2, fill = "midnightblue") +
    geom_line(color = "black")+
    geom_point(data = site_data, aes(x = date, y = bottom_temp_mean), color = "red") +
    labs(title = lakes[s])+
    coord_cartesian(ylim = c(0,20))+
    theme_bw()+
    labs(x = "Date", y = "Bottom Temperature")

  ggsave(paste0("bottom_temperature_",lakes[s],"__figure.jpg"), device = "jpg")

}
