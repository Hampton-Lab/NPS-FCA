fully_ts_MORA <- fully_aquatic %>%
  select(-variable, -site_code)%>%
  arrange(park_code, event_year)%>%
  group_by(park_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_value = as.numeric(lag(value)))%>%
  arrange(park_code, event_year)%>%
  ungroup(.)%>%
  select(-site_code)%>%
  mutate_at(vars(-park_code, -event_year),
            funs(imputeTS::na_interpolation(., option = "spline")))%>%
  filter(park_code == "MORA")%>%
  select(event_year, park_code, value, lag_value, lake_temp,`Total P`,`Total N`, Ca, Chlorophyll)%>%
  rename(TP = `Total P`,
         TN = `Total N`)%>%
  mutate(NtoP = TN/TP)%>%
  select(-TP, -TN)%>%
  mutate(lake_temp_lag = lag(lake_temp),
         Ca_lag = lag(Ca),
         Chlorophyll_lag = lag(Chlorophyll),
         NtoP_lag = lag(NtoP))%>%
  mutate_at(vars(-park_code),funs(imputeTS::na_interpolation(., option = "spline")))



  t <- glm(value~lag_value+lake_temp+Ca+NtoP, data = fully_ts_MORA, na.action = "na.fail")

  all_models <- dredge(t, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  fully_aic_t_MORA <- best_model %>% select(AICc)%>% mutate(park_code = "MORA")
  ### best predicted by AR term and temerature


  t_1 <- glm(value~lag_value+lake_temp_lag+Ca+NtoP, data = fully_ts_MORA, na.action = "na.fail")

  all_models <- dredge(t_1, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  fully_aic_t_1_MORA <- best_model %>% select(AICc)%>% mutate(park_code = "MORA")



  fully_ts_NOCA <- fully_aquatic %>%
    select(-variable, -site_code)%>%
    arrange(park_code, event_year)%>%
    group_by(park_code, event_year)%>%
    summarize_all(funs(mean), na.rm = F)%>%
    mutate(lag_value = as.numeric(lag(value)))%>%
    arrange(park_code, event_year)%>%
    ungroup(.)%>%
    select(-site_code)%>%
    mutate_at(vars(-park_code, -event_year),
              funs(imputeTS::na_interpolation(., option = "spline")))%>%
    filter(park_code == "NOCA")%>%
    select(event_year, park_code, value, lag_value, lake_temp,`Total P`,`Total N`, Ca, Chlorophyll)%>%
    rename(TP = `Total P`,
           TN = `Total N`)%>%
    mutate(NtoP = TN/TP)%>%
    select(-TP, -TN)%>%
    mutate(lake_temp_lag = lag(lake_temp),
           Ca_lag = lag(Ca),
           Chlorophyll_lag = lag(Chlorophyll),
           NtoP_lag = lag(NtoP))%>%
    mutate_at(vars(-park_code),funs(imputeTS::na_interpolation(., option = "spline")))



  t <- glm(value~lag_value+lake_temp+Ca+NtoP, data = fully_ts_NOCA, na.action = "na.fail")

  all_models <- dredge(t, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  fully_aic_t_NOCA <- best_model %>% select(AICc)%>% mutate(park_code = "NOCA")
  ### best predicted by AR term and temperature


  t_1 <- glm(value~lag_value+lake_temp_lag+Ca+NtoP, data = fully_ts_NOCA, na.action = "na.fail")

  all_models <- dredge(t_1, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  fully_aic_t_1_NOCA <- best_model %>% select(AICc)%>% mutate(park_code = "NOCA")




  semi_ts_MORA <- semi_aquatic %>%
    select(-variable, -site_code)%>%
    arrange(park_code, event_year)%>%
    group_by(park_code, event_year)%>%
    summarize_all(funs(mean), na.rm = F)%>%
    mutate(lag_value = as.numeric(lag(value)))%>%
    arrange(park_code, event_year)%>%
    ungroup(.)%>%
    select(-site_code)%>%
    mutate_at(vars(-park_code, -event_year),
              funs(imputeTS::na_interpolation(., option = "spline")))%>%
    filter(park_code == "MORA")%>%
    select(event_year, park_code, value, lag_value, lake_temp,`Total P`,`Total N`, Ca, Chlorophyll)%>%
    rename(TP = `Total P`,
           TN = `Total N`)%>%
    mutate(NtoP = TN/TP)%>%
    select(-TP, -TN)%>%
    mutate(lake_temp_lag = lag(lake_temp),
           Ca_lag = lag(Ca),
           Chlorophyll_lag = lag(Chlorophyll),
           NtoP_lag = lag(NtoP))%>%
    mutate_at(vars(-park_code),funs(imputeTS::na_interpolation(., option = "spline")))



  t <- glm(value~lag_value+lake_temp+Ca+NtoP, data = semi_ts_MORA, na.action = "na.fail")

  all_models <- dredge(t, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  semi_aic_t_MORA <- best_model %>% select(AICc)%>% mutate(park_code = "MORA")
  ### best predicted by AR term and temerature


  t_1 <- glm(value~lag_value+lake_temp_lag+Ca+NtoP, data = semi_ts_MORA, na.action = "na.fail")

  all_models <- dredge(t_1, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  semi_aic_t_1_MORA <- best_model %>% select(AICc)%>% mutate(park_code = "MORA")

  ggplot(semi_ts_MORA, aes(event_year, value))+geom_point()
  ggplot(fully_ts_MORA, aes(event_year, value))+geom_point()


  semi_ts_NOCA <- semi_aquatic %>%
    select(-variable, -site_code)%>%
    arrange(park_code, event_year)%>%
    group_by(park_code, event_year)%>%
    summarize_all(funs(mean), na.rm = F)%>%
    mutate(lag_value = as.numeric(lag(value)))%>%
    arrange(park_code, event_year)%>%
    ungroup(.)%>%
    select(-site_code)%>%
    mutate_at(vars(-park_code, -event_year),
              funs(imputeTS::na_interpolation(., option = "spline")))%>%
    filter(park_code == "NOCA")%>%
    select(event_year, park_code, value, lag_value, lake_temp,`Total P`,`Total N`, Ca, Chlorophyll)%>%
    rename(TP = `Total P`,
           TN = `Total N`)%>%
    mutate(NtoP = TN/TP)%>%
    select(-TP, -TN)%>%
    mutate(lake_temp_lag = lag(lake_temp),
           Ca_lag = lag(Ca),
           Chlorophyll_lag = lag(Chlorophyll),
           NtoP_lag = lag(NtoP))%>%
    mutate_at(vars(-park_code),funs(imputeTS::na_interpolation(., option = "spline")))



  t <- glm(value~lag_value+lake_temp+Ca+NtoP, data = semi_ts_NOCA, na.action = "na.fail")

  all_models <- dredge(t, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  semi_aic_t_NOCA <- best_model %>% select(AICc)%>% mutate(park_code = "NOCA")
  ### best predicted by AR term and temperature


  t_1 <- glm(value~lag_value+lake_temp_lag+Ca+NtoP, data = semi_ts_NOCA, na.action = "na.fail")

  all_models <- dredge(t_1, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta <= 2))

  semi_aic_t_1_NOCA <- best_model %>% select(AICc)%>% mutate(park_code = "NOCA")





  #
# macro_join_ts_test <- macro_join %>%
#   group_by(site_code)%>%
#   mutate(Amphipoda_lag = lag(Amphipoda),
#          Ntoda_lag = lag(Ntoda),
#          Ephemeroptera_lag = lag(Ephemeroptera),
#          Nmorpha_lag = lag(Nmorpha),
#          Veneroida_lag = lag(Veneroida),
#          Diptera_lag = lag(Diptera),
#          Megaloptera_lag = lag(Megaloptera),
#          Trichoptera_lag = lag(Trichoptera),
#          Basommatophora_lag = lag(Basommatophora),
#          Hemiptera_lag = lag(Hemiptera),
#          Platy_lag = lag(Platy),
#          Coleoptera_lag = lag(Coleoptera),
#          Hirudinida_lag = lag(Hirudinida),
#          Ntopda_lag = lag(Ntopda),
#          Plecoptera_lag = lag(Plecoptera),
#          Acari_lag = lag(Acari),
#          Diplostraca_lag = lag(Diplostraca),
#          Isopoda_lag = lag(Isopoda),
#          Odonata_lag = lag(Odonata),
#          Poridera_lag = lag(Poridera),
#          diff_Amphipoda  = Amphipoda_lag - Amphipoda,
#          diff_Ntoda  = Ntoda_lag - Ntoda,
#          diff_Ephemeroptera  = Ephemeroptera_lag -Ephemeroptera,
#          diff_Nmorpha  = Nmorpha_lag -Nmorpha,
#          diff_Veneroida  = Veneroida_lag -Veneroida,
#          diff_Diptera  = Diptera_lag -Diptera,
#          diff_Megaloptera  = Megaloptera_lag -Megaloptera,
#          diff_Trichoptera  = Trichoptera_lag -Trichoptera,
#          diff_Basommatophora  = Basommatophora_lag -Basommatophora,
#          diff_Hemiptera  = Hemiptera_lag -Hemiptera,
#          diff_Platy  = Platy_lag -Platy,
#          diff_Coleoptera  = Coleoptera_lag -Coleoptera,
#          diff_Hirudinida  = Hirudinida_lag -Hirudinida,
#          diff_Ntopda  = Ntopda_lag -Ntopda,
#          diff_Plecoptera  = Plecoptera_lag -Plecoptera,
#          diff_Acari  = Acari_lag -Acari,
#          diff_Diplostraca  = Diplostraca_lag -Diplostraca,
#          diff_Isopoda  = Isopoda_lag -Isopoda,
#          diff_Odonata  = Odonata_lag -Odonata,
#          diff_Poridera  = Poridera_lag -Poridera)%>%
#   mutate_at(vars(-site_code, -park_code, -event_year),
#             funs(imputeTS::na_interpolation(., option = "spline")))%>%
#   select(site_code, park_code, event_year, starts_with('diff'))%>%
#   melt(., id.vars = c("site_code","park_code","event_year"))%>%
#   ggplot(., aes(variable, value, group = variable, fill = variable))+
#   geom_boxplot()+
#   facet_wrap(~site_code, ncol = 1)+
#   geom_hline(yintercept = 0)+
#   theme_classic()+
#   theme(axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12))
# macro_join_ts_test
# ggsave("./figures/test_lags_BMI.jpg", width = 30, height = 18, units = "in")

sites <- c(unique(macro_join_ts$site_code))

taxa <- c("Amphipoda",
          "Ntoda",
          "Ephemeroptera",
          "Nmorpha",
          "Veneroida",
          "Diptera",
          "Megaloptera",
          "Trichoptera",
          "Basommatophora",
          "Hemiptera",
          "Platy",
          "Coleoptera",
          "Hirudinida",
          "Ntopda",
          "Plecoptera",
          "Acari",
          "Diplostraca",
          "Isopoda",
          "Odonata",
          "Poridera")


drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
drivers2 <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
AIC <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
AIC2 <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))

for(h in 1:length(taxa)){
for(s in 1:length(sites)){

  ts <- macro_join_ts %>%
    filter(site_code == sites[s])%>%
    select(taxa[h],paste0(taxa[h],"_lag"), lake_temp,`Total P`,`Total N`, Ca, Chlorophyll, ice_free_days)%>%
    rename(TP = `Total P`,
           TN = `Total N`,
           macro = taxa[h],
           macro_lag = paste0(taxa[h],"_lag"))%>%
    mutate(NtoP = TN/TP)%>%
    select(-TP, -TN)%>%
    mutate(lake_temp_lag = lag(lake_temp),
           Ca_lag = lag(Ca),
           Chlorophyll_lag = lag(Chlorophyll),
           ice_free_days_lag = lag(ice_free_days),
           NtoP_lag = lag(NtoP))%>%
    mutate_at(vars(-site_code),
              funs(imputeTS::na_interpolation(., option = "spline")))

  if(sum(ts$macro != 0)){

  t <- gls(macro~lake_temp+Ca+Chlorophyll+ice_free_days+NtoP, correlation = corAR1(form= ~1), data = ts)

  all_models <- dredge(t, extra = c("R^2","adjR^2"), rank = "AICc")

  best_model <- as.data.frame(subset(all_models, delta == 0))

  best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
    summarise_all(funs(mean))

  aic <- best_fits %>% select(`R^2`)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(value)

  best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  drivers[s,h] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", ")))
  AIC[s,h] <- as.data.frame(cbind(paste(aic$value,collapse=", ")))


  p <- gls(macro~lake_temp_lag+Ca_lag+Chlorophyll_lag+ice_free_days_lag+NtoP_lag, correlation = corAR1(form= ~1), data = ts)
  all_models2 <- dredge(p, extra = c("R^2","adjR^2"), rank = "AICc")

  best_model2 <- as.data.frame(subset(all_models2, delta == 0))

  best_fits2 <- best_model2[,colSums(is.na(best_model2))<nrow(best_model2)] %>%
    summarise_all(funs(mean))

  aic2 <- best_fits2 %>% select(`R^2`)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(value)

  best_fits2 <- best_fits2 %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
    cbind(., sites[s])%>%
    reshape2::melt(., id.vars = "sites[s]")%>%
    select(variable)

  drivers2[s,h] <- as.data.frame(cbind(paste(best_fits2$variable,collapse=", ")))
  AIC2[s,h] <- as.data.frame(cbind(paste(aic2$value,collapse=", ")))

}
}
}

drivers <- drivers %>% rename(Amphipoda = `X1`,
                              Ntoda = `X2`,
                              Ephemeroptera = `X3`,
                              Nmorpha = `X4`,
                              Veneroida = `X5`,
                              Diptera = `X6`,
                              Megaloptera = `X7`,
                              Trichoptera = `X8`,
                              Basommatophora = `X9`,
                              Hemiptera = `X10`,
                              Platy = `X11`,
                              Coleoptera = `X12`,
                              Hirudinida = `X13`,
                              Ntopda = `X14`,
                              Plecoptera = `X15`,
                              Acari = `X16`,
                              Diplostraca = `X17`,
                              Isopoda = `X18`,
                              Odonata = `X19`,
                              Poridera = `X20`)%>%
  mutate(site_code = as.character(sites))

drivers2 <- drivers2 %>% rename(Amphipoda = `X1`,
                              Ntoda = `X2`,
                              Ephemeroptera = `X3`,
                              Nmorpha = `X4`,
                              Veneroida = `X5`,
                              Diptera = `X6`,
                              Megaloptera = `X7`,
                              Trichoptera = `X8`,
                              Basommatophora = `X9`,
                              Hemiptera = `X10`,
                              Platy = `X11`,
                              Coleoptera = `X12`,
                              Hirudinida = `X13`,
                              Ntopda = `X14`,
                              Plecoptera = `X15`,
                              Acari = `X16`,
                              Diplostraca = `X17`,
                              Isopoda = `X18`,
                              Odonata = `X19`,
                              Poridera = `X20`)%>%
  mutate(site_code = as.character(sites))


AIC2 <- AIC2 %>% rename(Amphipoda = `X1`,
                              Ntoda = `X2`,
                              Ephemeroptera = `X3`,
                              Nmorpha = `X4`,
                              Veneroida = `X5`,
                              Diptera = `X6`,
                              Megaloptera = `X7`,
                              Trichoptera = `X8`,
                              Basommatophora = `X9`,
                              Hemiptera = `X10`,
                              Platy = `X11`,
                              Coleoptera = `X12`,
                              Hirudinida = `X13`,
                              Ntopda = `X14`,
                              Plecoptera = `X15`,
                              Acari = `X16`,
                              Diplostraca = `X17`,
                              Isopoda = `X18`,
                              Odonata = `X19`,
                              Poridera = `X20`)%>%
  mutate(site_code = as.character(sites),
         mod_type = "lagged")

AIC <- AIC %>% rename(Amphipoda = `X1`,
                                Ntoda = `X2`,
                                Ephemeroptera = `X3`,
                                Nmorpha = `X4`,
                                Veneroida = `X5`,
                                Diptera = `X6`,
                                Megaloptera = `X7`,
                                Trichoptera = `X8`,
                                Basommatophora = `X9`,
                                Hemiptera = `X10`,
                                Platy = `X11`,
                                Coleoptera = `X12`,
                                Hirudinida = `X13`,
                                Ntopda = `X14`,
                                Plecoptera = `X15`,
                                Acari = `X16`,
                                Diplostraca = `X17`,
                                Isopoda = `X18`,
                                Odonata = `X19`,
                                Poridera = `X20`)%>%
  mutate(site_code = as.character(sites),
         mod_type = "not_lagged")

AIC_compare <- bind_rows(AIC, AIC2)%>%
  melt(., id.vars = c("site_code", "mod_type"))%>%
  na.omit(.)%>%
  mutate(value = as.numeric(value))

ggplot(AIC_compare, aes(x = mod_type, y = value, group = mod_type, fill = mod_type))+
  geom_boxplot()+
  geom_jitter(width = 0.05)+
  facet_wrap(~variable, scales = "free_y")+
  stat_compare_means(label = "p.signif", method = "t.test",label.x = 1.5, size = 3, paired = T)


map_drivers <- left_join(drivers, mapping_zoop, by = "site_code")
map_drivers2 <- left_join(drivers2, mapping_zoop, by = "site_code")


cca_df_ts <- cca_df %>%
  group_by(site_code)%>%
  mutate(RAP_lag = lag(RAP),
         MICRO_lag = lag(MICRO),
         CLAD_lag = lag(CLAD),
         COPE_lag = lag(COPE),
         diff_RAP  = RAP_lag - RAP,
         diff_MICRO  = MICRO_lag - MICRO,
         diff_CLAD  = CLAD_lag - CLAD,
         diff_COPE  = COPE_lag - COPE)%>%
  mutate_at(vars(-site_code, -park_code, -event_year),
            funs(imputeTS::na_interpolation(., option = "spline")))
#   select(site_code, park_code, event_year, starts_with('diff'))%>%
#   melt(., id.vars = c("site_code","park_code","event_year"))%>%
#   ggplot(., aes(variable, value, group = variable, fill = variable))+
#   geom_boxplot()+
#   facet_wrap(~site_code, ncol = 3)+
#   geom_hline(yintercept = 0)+
#   theme_classic()+
#   theme(axis.text.x = element_text(size = 15),
#         axis.text.y = element_text(size = 15))
# cca_df_ts_test
# ggsave("./figures/test_lags_zoop.jpg", width = 18, height = 12, units = "in")


sites <- c(unique(cca_df_ts$site_code))

taxa <- c("CLAD",
          "COPE",
          "MICRO",
          "RAP")

drivers <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
drivers2 <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
AIC <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))
AIC2 <- data.frame(matrix(NA, nrow = length(sites),ncol = length(taxa)))

for(h in 1:length(taxa)){
  for(s in 1:length(sites)){

    ts <- cca_df_ts %>%
      filter(site_code == sites[s])%>%
      select(taxa[h],paste0(taxa[h],"_lag"), lake_temp,`Total P`,`Total N`, Ca, Chlorophyll, ice_free_days)%>%
      rename(TP = `Total P`,
             TN = `Total N`,
             macro = taxa[h],
             macro_lag = paste0(taxa[h],"_lag"))%>%
      mutate(NtoP = TN/TP)%>%
      select(-TP, -TN)%>%
      mutate(lake_temp_lag = lag(lake_temp),
             Ca_lag = lag(Ca),
             Chlorophyll_lag = lag(Chlorophyll),
             ice_free_days_lag = lag(ice_free_days),
             NtoP_lag = lag(NtoP))%>%
      mutate_at(vars(-site_code),
                funs(imputeTS::na_interpolation(., option = "spline")))

    if(sum(ts$macro != 0)){

      t <- gls(macro~lake_temp+Ca+Chlorophyll+ice_free_days+NtoP, correlation = corAR1(form= ~1), data = ts)

      all_models <- dredge(t, extra = c("R^2","adjR^2"), rank = "AICc")

      best_model <- as.data.frame(subset(all_models, delta == 0))

      best_fits <- best_model[,colSums(is.na(best_model))<nrow(best_model)] %>%
        summarise_all(funs(mean))

      aic <- best_fits %>% select(`R^2`)%>%
        cbind(., sites[s])%>%
        reshape2::melt(., id.vars = "sites[s]")%>%
        select(value)

      best_fits <- best_fits %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
        cbind(., sites[s])%>%
        reshape2::melt(., id.vars = "sites[s]")%>%
        select(variable)

      drivers[s,h] <- as.data.frame(cbind(paste(best_fits$variable,collapse=", ")))
      AIC[s,h] <- as.data.frame(cbind(paste(aic$value,collapse=", ")))


      p <- gls(macro~lake_temp_lag+Ca_lag+Chlorophyll_lag+ice_free_days_lag+NtoP_lag, correlation = corAR1(form= ~1), data = ts)
      all_models2 <- dredge(p, extra = c("R^2","adjR^2"), rank = "AICc")

      best_model2 <- as.data.frame(subset(all_models2, delta == 0))

      best_fits2 <- best_model2[,colSums(is.na(best_model2))<nrow(best_model2)] %>%
        summarise_all(funs(mean))

      aic2 <- best_fits2 %>% select(`R^2`)%>%
        cbind(., sites[s])%>%
        reshape2::melt(., id.vars = "sites[s]")%>%
        select(value)

      best_fits2 <- best_fits2 %>% select(-`R^2`, -`adjR^2`, -df, -logLik, -AICc, -delta, -weight)%>%
        cbind(., sites[s])%>%
        reshape2::melt(., id.vars = "sites[s]")%>%
        select(variable)

      drivers2[s,h] <- as.data.frame(cbind(paste(best_fits2$variable,collapse=", ")))
      AIC2[s,h] <- as.data.frame(cbind(paste(aic2$value,collapse=", ")))

    }
  }
}

drivers <- drivers %>% rename(CLAD = `X1`,
                              COPE = `X2`,
                              MICRO = `X3`,
                              RAP = `X4`)%>%
  mutate(site_code = as.character(sites))

drivers2 <- drivers2 %>% rename(CLAD = `X1`,
                              COPE = `X2`,
                              MICRO = `X3`,
                              RAP = `X4`)%>%
  mutate(site_code = as.character(sites))

AIC <- AIC %>% rename(CLAD = `X1`,
                              COPE = `X2`,
                              MICRO = `X3`,
                              RAP = `X4`)%>%
  mutate(site_code = as.character(sites),
         mod_type = "not_lagged")

AIC2 <- AIC2 %>% rename(CLAD = `X1`,
                              COPE = `X2`,
                              MICRO = `X3`,
                              RAP = `X4`)%>%
  mutate(site_code = as.character(sites),
         mod_type = "lagged")


AIC_compare <- bind_rows(AIC, AIC2)%>%
  melt(., id.vars = c("site_code", "mod_type"))%>%
  na.omit(.)%>%
  mutate(value = as.numeric(value))

ggplot(AIC_compare, aes(x = mod_type, y = value, group = mod_type, fill = mod_type))+
  geom_boxplot()+
  geom_jitter(width = 0.05)+
  facet_wrap(~variable, scales = "free_y")+
  stat_compare_means(label = "p.signif", method = "t.test",label.x = 1.5, size = 3, paired = T)

zoop_map_drivers <- left_join(drivers, mapping_zoop, by = "site_code")
zoop_map_drivers2 <- left_join(drivers2, mapping_zoop, by = "site_code")


# species_t = (specties_t-1)+ error + change_temp_t +TN_t + TP_t + chla_t+change_temp_t-1 +TN_t-1- + TP_t-1 + chla_t-1
#
# Test the idea of fully aquatic vs.
#
# Semi aquatic are more influenced by (t)
#
# species_t = (specties_t-1)+ error + change_temp_t +TN_t + TP_t + chla_t
# species_t = (specties_t-1)+ error + change_temp_t-1 +TN_t-1- + TP_t-1 + chla_t-1
#
# AICc rank
#
# Fully aquatic we might expect that more influcend by (T-1)
# species_t = (specties_t-1)+ error + change_temp_t +TN_t + TP_t + chla_t+change_temp_t-1 +TN_t-1- + TP_t-1 + chla_t-1
