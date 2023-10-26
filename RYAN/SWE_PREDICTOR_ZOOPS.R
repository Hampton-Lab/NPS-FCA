# CH4cast
# Ryan McClure
# TRAINING MODELS WITH 2017 DATA

# 2017 MODEL TRAINING ----

#* TEMPERATURE SCALING MODEL ----

model.ar.train = ("model.ar.train.txt")
jagsscript = cat("
model {

   #priors===================================================

   mu2 ~ dnorm(0,1e-6)
   sd.pro ~ dunif(0, 1000)
   phi ~ dnorm(0,1e-6)
   omega ~ dnorm(0,1e-6)
   tau.pro <-  pow(sd.pro, -2)

   #Informative priors on initial conditions based on first observation
   X[1] ~ dnorm(x_init, tau.obs[1])

   #end priors===============================================

   for(i in 2:N) {

      #process model=============================================

      predX[i] <- mu2 + phi*X[i-1] + omega*D[i]
      X[i] ~ dnorm(predX[i],tau.pro)


      #end of process model======================================

      #data model================================================

      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model.ar.train)

CLAD_NOCA_train <- CLAD_NOCA %>%
  group_by(site_code)%>%
  mutate(CLAD_se = standard_error(CLAD),
         CLAD_se = ifelse(CLAD_se == 0, 0.1, CLAD_se))%>%
  arrange(event_year)

jags.data.ar = list(x_init = CLAD_NOCA_train$CLAD[1],
                    Y = CLAD_NOCA_train$CLAD,
                    tau.obs = 1/((CLAD_NOCA_train$CLAD_se)) ^ 2,
                    N = nrow(CLAD_NOCA_train),
                    D = CLAD_NOCA_train$SWE_May_snotel)

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

j.model   <- jags.model(file = model.ar.train,
                        data = jags.data.ar,
                        inits = init,
                        n.chains = 3)

eval_clad_noca  <- coda.samples(model = j.model,
                          variable.names = c("sd.pro", "mu2", "phi", "omega"),
                          n.iter = 200000, n.burnin = 20000, thin = 200)

print(gelman.diag(eval_clad_noca))
plot(eval_clad_noca)

clad_out_parms_noca <- eval_clad_noca %>%
  spread_draws(sd.pro, mu2, phi, omega) %>%
  filter(.chain == 1)

model.ar.forecast = ("model.ar.forecast.txt")
jagsscript = cat("
model {

   #priors===================================================
   pars ~ dmnorm(prior_mean,prior_inv_cov)
   sd.pro ~ dlnorm(proc_mean, proc_prec)
   tau.pro <-  pow(sd.pro, -2)

   #Informative priors on initial conditions based on first observation
   X[1] ~ dnorm(x_init, tau.obs[1])

   #end priors===============================================

   for(i in 2:N) {

      #process model=============================================

      predX[i] <- pars[1] + pars[2]*X[i-1] + pars[3]*D[i]
      X[i] ~ dnorm(predX[i],tau.pro)

      #end of process model======================================

      #data model================================================

      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model.ar.forecast)

years <- c("2008", "2009", "2010", "2011", "2012",
           "2013", "2014", "2015", "2016", "2017", "2018")

for(s in 1:length(years)){

  CLAD_NOCA_forecast <- CLAD_NOCA %>%
    select(event_year, SWE_May_snotel, lag_CLAD, CLAD)%>%
    filter(event_year <= years[s])%>%
    group_by(event_year)%>%
    mutate(CLAD_se = standard_error(CLAD),
           CLAD_se = ifelse(is.na(CLAD_se),0.5,CLAD_se))

  jags.data.ar = list(x_init = CLAD_NOCA_forecast$CLAD[1],
                      Y = CLAD_NOCA_forecast$CLAD,
                      tau.obs = 1/((CLAD_NOCA_forecast$CLAD_se)) ^ 2,
                      N = nrow(CLAD_NOCA_forecast),
                      D = CLAD_NOCA_forecast$SWE_May_snotel,
                      prior_mean = colMeans(clad_out_parms_noca[,5:7]),
                      prior_inv_cov = solve(cov(clad_out_parms_noca[,5:7])),
                      proc_prec = 1 / log(1 + (var(clad_out_parms_noca$sd.pro)/(mean(clad_out_parms_noca$sd.pro)^2))),
                      proc_mean = log((mean(clad_out_parms_noca$sd.pro)^2)/sqrt(mean(clad_out_parms_noca$sd.pro)^2 + var(clad_out_parms_noca$sd.pro))))

  nchain = 3
  chain_seeds <- c(200,800,1400)
  init <- list()
  for(i in 1:nchain){
    init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                      pars = c(runif(1, -20,0),runif(1, 0.3,1),runif(1, -0.2, 0.3)),
                      .RNG.name = "base::Wichmann-Hill",
                      .RNG.seed = chain_seeds[i])
  }

  j.model   <- jags.model(file = model.ar.forecast,
                          data = jags.data.ar,
                          inits = init,
                          n.chains = 3)

  jags.out  <- coda.samples(model = j.model,
                            variable.names = c("sd.pro", "pars[1]", "pars[2]", "pars[3]"),
                            n.iter = 200000, n.burnin = 20000, thin = 200)

  gelman.diag(jags.out)
  plot(jags.out)

  clad_noca_forecast_parms <- jags.out %>%
    spread_draws(sd.pro, `pars[1]`, `pars[2]`, `pars[3]`) %>%
    rename(ensemble = .iteration)


  forecast_function <- function(IC, mu2, phi, omega, SWE, Q){
    est <- mu2 + (phi * IC) + (omega * SWE) + rnorm(1000, 0, sd = Q)
    return(est)
  }


  parms <- sample_n(clad_noca_forecast_parms, 1000, replace=TRUE)

  IC <- CLAD_NOCA_forecast %>% filter(event_year < years[s]) %>%
    ungroup(.) %>%
    filter(event_year == max(event_year))%>%
    summarize_all(funs(mean))%>%
    select(CLAD, CLAD_se)

  SWE = CLAD_NOCA_forecast %>% filter(event_year == years[s]) %>%
    select(SWE_May_snotel)%>%
    ungroup(.)%>%
    summarise(SWE_mean = mean(SWE_May_snotel),
              SWE_sd = sd(SWE_May_snotel))

  observed_year <- CLAD_NOCA_forecast %>% filter(event_year < years[s]) %>%
    ungroup(.) %>%
    filter(event_year == max(event_year))%>%
    select(event_year)%>%
    mutate(event_year = as.numeric(event_year))%>%
    summarize_all(funs(mean))%>%
    mutate(event_year = as.character(event_year))

  forecast_year <- CLAD_NOCA_forecast %>% filter(event_year == years[s]) %>%
    select(event_year)%>%
    summarize_all(funs(mean))

  # All uncertainty on
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, IC$CLAD_se), ## sample IC
                                             SWE = rnorm(1000, SWE$SWE_mean, SWE$SWE_sd),
                                             mu2 = parms$`pars[1]`,
                                             phi = parms$`pars[2]`,
                                             omega = parms$`pars[3]`,
                                             Q = parms$sd.pro)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_forecast_wDA_",years[s],".rds"))


  # IC on
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, IC$CLAD_se), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, 0),
                                              mu2 = mean(parms$`pars[1]`),
                                              phi = mean(parms$`pars[2]`),
                                              omega = mean(parms$`pars[3]`),
                                              Q = 0)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_IC_UNC_",years[s],".rds"))

  # SWE DRIVER ON
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, 0), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, SWE$SWE_sd),
                                              mu2 = mean(parms$`pars[1]`),
                                              phi = mean(parms$`pars[2]`),
                                              omega = mean(parms$`pars[3]`),
                                              Q = 0)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_SWE_UNC_",years[s],".rds"))

  # Parameter uncertainty on
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, 0), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, 0),
                                              mu2 = parms$`pars[1]`,
                                              phi = parms$`pars[2]`,
                                              omega = parms$`pars[3]`,
                                              Q = 0)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_Parameter_UNC_",years[s],".rds"))

  # Model PROCESS
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, 0), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, 0),
                                              mu2 = mean(parms$`pars[1]`),
                                              phi = mean(parms$`pars[2]`),
                                              omega = mean(parms$`pars[3]`),
                                              Q = parms$sd.pro)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_PROCESS_UNC_",years[s],".rds"))


  # Model SWE parm
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, 0), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, 0),
                                              mu2 = mean(parms$`pars[1]`),
                                              phi = mean(parms$`pars[2]`),
                                              omega = parms$`pars[3]`,
                                              Q = 0)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_SWE_PARM_",years[s],".rds"))

  # Model AR parm
  clad_noca_forecast_1yr <- forecast_function(IC = rnorm(1000, IC$CLAD, 0), ## sample IC
                                              SWE = rnorm(1000, SWE$SWE_mean, 0),
                                              mu2 = mean(parms$`pars[1]`),
                                              phi = parms$`pars[2]`,
                                              omega = mean(parms$`pars[3]`),
                                              Q = 0)


  clad_noca_forecast <- cbind(forecast_year, clad_noca_forecast_1yr)%>%
    mutate(type = "forecast")%>%
    rename(CLAD = clad_noca_forecast_1yr)

  prior_year <- cbind(observed_year, rnorm(1000, IC$CLAD, IC$CLAD_se))%>%
    mutate(type = "initial_condition")%>%
    rename(CLAD = `rnorm(1000, IC$CLAD, IC$CLAD_se)`)

  clad_noca_forecast <- bind_rows(prior_year, clad_noca_forecast)

  forecast_sumarized <- clad_noca_forecast %>%
    group_by(event_year) %>%
    summarise(mean = mean(CLAD),
              max = max(CLAD),
              min = min(CLAD),
              upper_95 = quantile(CLAD, 0.95, na.rm = T),
              lower_95 = quantile(CLAD, 0.05, na.rm = T),
              var = var(CLAD))

  saveRDS(forecast_sumarized, paste0("./data/CLAD_NOCA_AR_PARM_",years[s],".rds"))
}
