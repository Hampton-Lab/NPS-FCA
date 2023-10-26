cca_df_ts <- cca_df %>%
  group_by(site_code)%>%
  mutate(RAP_lag = lag(RAP),
         MICRO_lag = lag(MICRO),
         CLAD_lag = lag(CLAD),
         COPE_lag = lag(COPE))%>%
  na.omit(.)

sites <- c(unique(cca_df_ts$site_code))

CLAD_drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = 2))
CLAD_model_output <- data.frame(matrix(NA, nrow = length(sites),ncol = 16))

for(s in 1:length(sites)){

  site_ts <- cca_df_ts %>%
    filter(site_code == sites[s])%>%
    select(CLAD,CLAD_lag,lake_temp,stability,ice_out_doy,`Total P`,`Total N`,Ca)%>%
    rename(TP = `Total P`,
           TN = `Total N`)

  all <- glm(CLAD ~ CLAD_lag+
               lake_temp+stability+ice_out_doy+
               TN+TP+Ca,
               na.action = "na.fail",
               data = site_ts)

  all_models <- dredge(all, extra = c("R^2","adjR^2"),
                       fixed = "CLAD_lag", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  CLAD_model_output[s,] <- as.data.frame(cbind(best_model,sites[s]))

  best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
    summarise_all(funs(mean))%>%
    rename(Intercept = `(Intercept)`)

  best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  CLAD_drivers[s,] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", "), sites[s]))

}

CLAD_model_output <- CLAD_model_output %>% rename(Intercept = `X1`,
                                        Ca = `X2`,
                                        CLAD_lag = `X3`,
                                        ice_out = `X4`,
                                        lake_temp = `X5`,
                                        stability = `X6`,
                                        TN= `X7`,
                                        TP = `X8`,
                                        r_square = `X9`,
                                        adj_r_square = `X10`,
                                        df = `X11`,
                                        logLik = `X12`,
                                        AICc = `X13`,
                                        delta = `X14`,
                                        weight = `X15`,
                                        site_code = `X16`)%>%
  mutate(site_code = as.character(site_code))

CLAD_drivers <- CLAD_drivers %>% rename(Drivers = `X1`,
                                        site_code = `X2`)

CLAD_drivers <- left_join(CLAD_model_output, CLAD_drivers, by = "site_code")%>%
  left_join(., mapping_zoop, by = "site_code")%>%
  filter(r_square >=0.2)
CLAD_drivers$Drivers  <- gsub("Intercept, ","" , CLAD_drivers$Drivers ,ignore.case = TRUE)
CLAD_drivers$Drivers  <- gsub(", CLAD_lag","" , CLAD_drivers$Drivers ,ignore.case = TRUE)
CLAD_drivers$Drivers  <- gsub(", CLAD_lag, ",", " , CLAD_drivers$Drivers ,ignore.case = TRUE)
CLAD_drivers$Drivers  <- gsub("CLAD_lag, ","" , CLAD_drivers$Drivers ,ignore.case = TRUE)
CLAD_drivers$Drivers  <- gsub("CLAD_lag","None" , CLAD_drivers$Drivers ,ignore.case = TRUE)

CLAD_drivers <- CLAD_drivers %>%
  filter(Drivers != "None")

# COPEPODS
COPE_drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = 2))
COPE_model_output <- data.frame(matrix(NA, nrow = length(sites),ncol = 16))

for(s in 1:length(sites)){

  site_ts <- cca_df_ts %>%
    filter(site_code == sites[s])%>%
    select(COPE,COPE_lag,lake_temp,stability,ice_out_doy,`Total P`,`Total N`,Ca)%>%
    rename(TP = `Total P`,
           TN = `Total N`)

  all <- glm(COPE ~ COPE_lag+
               lake_temp+stability+ice_out_doy+
               TN+TP+Ca,
             na.action = "na.fail",
             data = site_ts)

  all_models <- dredge(all, extra = c("R^2","adjR^2"),
                       fixed = "COPE_lag", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  COPE_model_output[s,] <- as.data.frame(cbind(best_model,sites[s]))

  best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
    summarise_all(funs(mean))%>%
    rename(Intercept = `(Intercept)`)

  best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  COPE_drivers[s,] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", "), sites[s]))

}

COPE_model_output <- COPE_model_output %>% rename(Intercept = `X1`,
                                                  Ca = `X2`,
                                                  COPE_lag = `X3`,
                                                  ice_out = `X4`,
                                                  lake_temp = `X5`,
                                                  stability = `X6`,
                                                  TN= `X7`,
                                                  TP = `X8`,
                                                  r_square = `X9`,
                                                  adj_r_square = `X10`,
                                                  df = `X11`,
                                                  logLik = `X12`,
                                                  AICc = `X13`,
                                                  delta = `X14`,
                                                  weight = `X15`,
                                                  site_code = `X16`)%>%
  mutate(site_code = as.character(site_code))

COPE_drivers <- COPE_drivers %>% rename(Drivers = `X1`,
                                        site_code = `X2`)

COPE_drivers <- left_join(COPE_model_output, COPE_drivers, by = "site_code")%>%
  left_join(., mapping_zoop, by = "site_code")%>%
  filter(r_square >=0.2)
COPE_drivers$Drivers  <- gsub("Intercept, ","" , COPE_drivers$Drivers ,ignore.case = TRUE)
COPE_drivers$Drivers  <- gsub(", COPE_lag","" , COPE_drivers$Drivers ,ignore.case = TRUE)
COPE_drivers$Drivers  <- gsub(", COPE_lag, ",", " , COPE_drivers$Drivers ,ignore.case = TRUE)
COPE_drivers$Drivers  <- gsub("COPE_lag, ","" , COPE_drivers$Drivers ,ignore.case = TRUE)
COPE_drivers$Drivers  <- gsub("COPE_lag","None" , COPE_drivers$Drivers ,ignore.case = TRUE)

COPE_drivers <- COPE_drivers %>%
  filter(Drivers != "None")

MICRO_drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = 2))
MICRO_model_output <- data.frame(matrix(NA, nrow = length(sites),ncol = 16))

for(s in 1:length(sites)){

  site_ts <- cca_df_ts %>%
    filter(site_code == sites[s])%>%
    select(MICRO,MICRO_lag,lake_temp,stability,ice_out_doy,`Total P`,`Total N`,Ca)%>%
    rename(TP = `Total P`,
           TN = `Total N`)

  all <- glm(MICRO ~ MICRO_lag+
               lake_temp+stability+ice_out_doy+
               TN+TP+Ca,
             na.action = "na.fail",
             data = site_ts)

  all_models <- dredge(all, extra = c("R^2","adjR^2"),
                       fixed = "MICRO_lag", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  MICRO_model_output[s,] <- as.data.frame(cbind(best_model,sites[s]))

  best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
    summarise_all(funs(mean))%>%
    rename(Intercept = `(Intercept)`)

  best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  MICRO_drivers[s,] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", "), sites[s]))

}

MICRO_model_output <- MICRO_model_output %>% rename(Intercept = `X1`,
                                                  Ca = `X2`,
                                                  ice_out = `X3`,
                                                  lake_temp = `X4`,
                                                  MICRO_lag = `X5`,
                                                  stability = `X6`,
                                                  TN= `X7`,
                                                  TP = `X8`,
                                                  r_square = `X9`,
                                                  adj_r_square = `X10`,
                                                  df = `X11`,
                                                  logLik = `X12`,
                                                  AICc = `X13`,
                                                  delta = `X14`,
                                                  weight = `X15`,
                                                  site_code = `X16`)%>%
  mutate(site_code = as.character(site_code))

MICRO_drivers <- MICRO_drivers %>% rename(Drivers = `X1`,
                                        site_code = `X2`)

MICRO_drivers <- left_join(MICRO_model_output, MICRO_drivers, by = "site_code")%>%
  left_join(., mapping_zoop, by = "site_code")%>%
  filter(r_square >=0.2)
MICRO_drivers$Drivers  <- gsub("Intercept, ","" , MICRO_drivers$Drivers ,ignore.case = TRUE)
MICRO_drivers$Drivers  <- gsub(", MICRO_lag","" , MICRO_drivers$Drivers ,ignore.case = TRUE)
MICRO_drivers$Drivers  <- gsub(", MICRO_lag, ",", " , MICRO_drivers$Drivers ,ignore.case = TRUE)
MICRO_drivers$Drivers  <- gsub("MICRO_lag, ","" , MICRO_drivers$Drivers ,ignore.case = TRUE)
MICRO_drivers$Drivers  <- gsub("MICRO_lag","None" , MICRO_drivers$Drivers ,ignore.case = TRUE)

MICRO_drivers <- MICRO_drivers %>%
  filter(Drivers != "None")

RAP_drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = 2))
RAP_model_output <- data.frame(matrix(NA, nrow = length(sites),ncol = 16))

for(s in 1:length(sites)){

  site_ts <- cca_df_ts %>%
    filter(site_code == sites[s])%>%
    select(RAP,RAP_lag,lake_temp,stability,ice_out_doy,`Total P`,`Total N`,Ca)%>%
    rename(TP = `Total P`,
           TN = `Total N`)

  all <- glm(RAP ~ RAP_lag+
               lake_temp+stability+ice_out_doy+
               TN+TP+Ca,
             na.action = "na.fail",
             data = site_ts)

  all_models <- dredge(all, extra = c("R^2","adjR^2"),
                       fixed = "RAP_lag", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  RAP_model_output[s,] <- as.data.frame(cbind(best_model,sites[s]))

  best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
    summarise_all(funs(mean))%>%
    rename(Intercept = `(Intercept)`)

  best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  RAP_drivers[s,] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", "), sites[s]))

}

RAP_model_output <- RAP_model_output %>% rename(Intercept = `X1`,
                                                    Ca = `X2`,
                                                    ice_out = `X3`,
                                                    lake_temp = `X4`,
                                                    RAP_lag = `X5`,
                                                    stability = `X6`,
                                                    TN= `X7`,
                                                    TP = `X8`,
                                                    r_square = `X9`,
                                                    adj_r_square = `X10`,
                                                    df = `X11`,
                                                    logLik = `X12`,
                                                    AICc = `X13`,
                                                    delta = `X14`,
                                                    weight = `X15`,
                                                    site_code = `X16`)%>%
  mutate(site_code = as.character(site_code))

RAP_drivers <- RAP_drivers %>% rename(Drivers = `X1`,
                                          site_code = `X2`)

RAP_drivers <- left_join(RAP_model_output, RAP_drivers, by = "site_code")%>%
  left_join(., mapping_zoop, by = "site_code")%>%
  filter(r_square >=0.2)
RAP_drivers$Drivers  <- gsub("Intercept, ","" , RAP_drivers$Drivers ,ignore.case = TRUE)
RAP_drivers$Drivers  <- gsub(", RAP_lag","" , RAP_drivers$Drivers ,ignore.case = TRUE)
RAP_drivers$Drivers  <- gsub(", RAP_lag, ",", " , RAP_drivers$Drivers ,ignore.case = TRUE)
RAP_drivers$Drivers  <- gsub("RAP_lag, ","" , RAP_drivers$Drivers ,ignore.case = TRUE)
RAP_drivers$Drivers  <- gsub("RAP_lag","None" , RAP_drivers$Drivers ,ignore.case = TRUE)

RAP_drivers <- RAP_drivers %>%
  filter(Drivers != "None")
