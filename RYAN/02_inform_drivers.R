###############################
# Linear Mixed Effects Modeling
###############################

set.seed(71)


### Perform an RDA on the Zooplankton in Fishless lakes
zoop_cap <- capscale(formula = select(env_zoop_data, CLAD:RAP) ~
                       lake_temp + Ca + Chlorophyll, data = env_zoop_data, distance = "bray")

zoop_null <- capscale(formula = select(env_zoop_data, CLAD:RAP) ~ 1,
                      data = env_zoop_data, distance = "bray")

mods <- ordiR2step(zoop_null, scope = formula(zoop_cap), trace = 0, direction = c("forward"),
                 permutations = how(nperm = 999), steps = 100)
mods$anova
model_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),env_zoop_data)

all_data_model <- autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = model_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(event_year)), size = 4, alpha = 0.5)+
  labs(title = "Zooplankton Abundance - Fishless Lakes")+
  theme_classic()

all_data_model
ggsave("./figures/CAPSCALE_model_output.jpg", width = 10, height = 10, units = "in")

zoop_taxa <- c("COPE", "CLAD", "MICRO", "RAP")
sites <- c("Allen","Blue","Bowan","Gladys","Heather","LP19","Milk","Silent","Triplet","Deadwood","LH15",
           "Sunup","Blum","Connie","Crazy","East","EasyRidge","Ferry","LaCrosse")

model_predict <- list()
model_coef <- list()
model_eval <- data.frame(matrix(NA, nrow = length(zoop_taxa),ncol = 5))

for(s in 1:length(zoop_taxa)){

  ZOOP <- ZOOP_by_site %>% filter(variable == zoop_taxa[s]) %>%
          mutate(fish = as.character(fish)) %>%
          lme(na.action=na.omit,value ~ lake_temp + Ca + Chlorophyll + fish, random = ~ 1|site_code, method="ML",data = .) %>%
          predict(., interval = "prediction") %>% as.data.frame(.)
  event_year <- ZOOP_by_site %>% filter(variable == zoop_taxa[s]) %>% select(event_year, site_code, variable)
  predict <- bind_cols(ZOOP, event_year)
  names(predict) <- c("abundance", "event_year", "site_code", "taxa" )

  model_predict[[s]] <- predict

  mod <- ZOOP_by_site %>% filter(variable == zoop_taxa[s]) %>%
    mutate(fish = as.character(fish)) %>%
    lme(na.action=na.omit,value ~ lake_temp + Ca + Chlorophyll + fish, random = ~ 1|site_code, method="REML",data = .)
summary(mod)

  coef <- ZOOP_by_site %>% filter(variable == zoop_taxa[s]) %>%
    mutate(fish = as.character(fish)) %>%
    lme(na.action=na.omit,value ~ lake_temp + Ca + Chlorophyll + fish, random = ~ 1|site_code, method="ML",data = .) %>%
    coef(.) %>% as.data.frame(.) %>% mutate(taxa = zoop_taxa[s])

  model_coef[[s]] <- coef

  site_summary <- ZOOP_by_site %>% filter(variable == zoop_taxa[s]) %>% mutate(fish = as.character(fish)) %>%
                  do(glance(lme(na.action=na.omit,value ~ lake_temp + Ca + Chlorophyll + fish, random = ~ 1|site_code,
                                method="ML",data = .)))%>%
                  as.data.frame() %>% mutate(taxa = zoop_taxa[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,taxa)

  model_eval[s,] <- as.data.frame(rbind(site_summary))
}

model_predict = do.call(rbind, model_predict)
model_coef = do.call(rbind, model_coef)
names(model_eval) <- c("sigma","logLik","AIC","BIC","taxa")

CLAD_compare <- ZOOP %>% select(event_year, site_code, park_code, CLAD)%>%
  left_join(., model_predict, by = c("event_year", "site_code"))

RMSE <- CLAD_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(CLAD, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(CLAD_compare, aes(x = abundance, y = CLAD, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed CLAD")),
       x = expression(paste("predicted CLAD")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(CLAD_compare, aes(x = event_year, y = CLAD))+
  geom_point(aes(x = event_year, y = CLAD, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(CLAD Abundance)")), x = "", title = "CLAD lme models (no fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_COPE <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_COPE <- list()

for(s in 1:length(sites)){

  COPE_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,COPE ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(COPE_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  COPE_model_eval <- cbind(COPE_model_eval=COPE_model_eval, event_year)
  names(COPE_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_COPE[[s]] <- COPE_model_eval

  COPE_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,COPE ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_COPE[s,] <- as.data.frame(rbind(COPE_site_summary))

}

model_predict_COPE = do.call(rbind, model_predict_COPE)
names(model_eval_COPE) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_COPE$zoop_taxa <- "COPE"

COPE_compare <- ZOOP %>% select(event_year, site_code, park_code, COPE)%>%
  left_join(., model_predict_COPE, by = c("event_year", "site_code"))

RMSE <- COPE_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(COPE, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(COPE_compare, aes(x = abundance, y = COPE, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed COPE")),
       x = expression(paste("predicted COPE")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(COPE_compare, aes(x = event_year, y = COPE))+
  geom_point(aes(x = event_year, y = COPE, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(COPE Abundance)")), x = "", title = "COPE lme models (no fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_MICRO <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_MICRO <- list()

for(s in 1:length(sites)){

  MICRO_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,MICRO ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(MICRO_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  MICRO_model_eval <- cbind(MICRO_model_eval=MICRO_model_eval, event_year)
  names(MICRO_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_MICRO[[s]] <- MICRO_model_eval

  MICRO_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,MICRO ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_MICRO[s,] <- as.data.frame(rbind(MICRO_site_summary))

}

model_predict_MICRO = do.call(rbind, model_predict_MICRO)
names(model_eval_MICRO) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_MICRO$zoop_taxa <- "MICRO"

MICRO_compare <- ZOOP %>% select(event_year, site_code, park_code, MICRO)%>%
  left_join(., model_predict_MICRO, by = c("event_year", "site_code"))

RMSE <- MICRO_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(MICRO, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(MICRO_compare, aes(x = abundance, y = MICRO, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed MICRO")),
       x = expression(paste("predicted MICRO")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(MICRO_compare, aes(x = event_year, y = MICRO))+
  geom_point(aes(x = event_year, y = MICRO, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(MICRO Abundance)")), x = "", title = "MICRO lme models (no fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_RAP <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_RAP <- list()

for(s in 1:length(sites)){

  RAP_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,RAP ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(RAP_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  RAP_model_eval <- cbind(RAP_model_eval=RAP_model_eval, event_year)
  names(RAP_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_RAP[[s]] <- RAP_model_eval

  RAP_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,RAP ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_RAP[s,] <- as.data.frame(rbind(RAP_site_summary))

}

model_predict_RAP = do.call(rbind, model_predict_RAP)
names(model_eval_RAP) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_RAP$zoop_taxa <- "RAP"

RAP_compare <- ZOOP %>% select(event_year, site_code, park_code, RAP)%>%
  left_join(., model_predict_RAP, by = c("event_year", "site_code"))

RMSE <- RAP_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(RAP, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(RAP_compare, aes(x = abundance, y = RAP, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed RAP")),
       x = expression(paste("predicted RAP")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(RAP_compare, aes(x = event_year, y = RAP))+
  geom_point(aes(x = event_year, y = RAP, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(RAP Abundance)")), x = "", title = "RAP lme models (no fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


### Perform an RDA on the Zooplankton in Fish lakes
zoop_cap <- capscale(formula = select(zoop_fish, CLAD:RAP) ~
                       lake_temp + Ca + Chlorophyll, data = zoop_fish, distance = "bray")

zoop_null <- capscale(formula = select(zoop_fish, CLAD:RAP) ~ 1,
                      data = zoop_fish, distance = "bray")

mods <- ordiR2step(zoop_null, scope = formula(zoop_cap), trace = 0, direction = c("forward"),
                   permutations = how(nperm = 999), steps = 100)
mods$anova
model_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),zoop_fish)

all_data_model <- autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = model_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(park_code)), size = 4, alpha = 0.5)+
  labs(title = "Zooplankton Abundance - Fishless Lakes")+
  theme_classic()

all_data_model
ggsave("./figures/CAPSCALE_model_output.jpg", width = 10, height = 10, units = "in")

ZOOP <- zoop_fish %>%
  group_by(site_code, event_year, park_code)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_CLAD = as.numeric(lag(CLAD)),
         lag_COPE = as.numeric(lag(COPE)),
         lag_RAP = as.numeric(lag(RAP)),
         lag_MICRO = as.numeric(lag(MICRO)))%>%
  arrange(event_year)%>%
  ungroup(.)

sites <- c("LH15","LP19","Deadwood","Blue","Gladys","Heather")

model_eval_CLAD <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_CLAD <- list()

for(s in 1:length(sites)){

  CLAD_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,CLAD ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(., interval = "prediction") %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(CLAD_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  CLAD_model_eval <- cbind(CLAD_model_eval=CLAD_model_eval, event_year)
  names(CLAD_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_CLAD[[s]] <- CLAD_model_eval

  CLAD_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,CLAD ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_CLAD[s,] <- as.data.frame(rbind(CLAD_site_summary))

}

model_predict_CLAD = do.call(rbind, model_predict_CLAD)
names(model_eval_CLAD) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_CLAD$zoop_taxa <- "CLAD"

CLAD_compare <- ZOOP %>% select(event_year, site_code, park_code, CLAD)%>%
  left_join(., model_predict_CLAD, by = c("event_year", "site_code"))

RMSE <- CLAD_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(CLAD, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(CLAD_compare, aes(x = abundance, y = CLAD, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed CLAD")),
       x = expression(paste("predicted CLAD")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(CLAD_compare, aes(x = event_year, y = CLAD))+
  geom_point(aes(x = event_year, y = CLAD, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(CLAD Abundance)")), x = "", title = "CLAD lme models (fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_COPE <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_COPE <- list()

for(s in 1:length(sites)){

  COPE_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,COPE ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(COPE_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  COPE_model_eval <- cbind(COPE_model_eval=COPE_model_eval, event_year)
  names(COPE_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_COPE[[s]] <- COPE_model_eval

  COPE_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,COPE ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_COPE[s,] <- as.data.frame(rbind(COPE_site_summary))

}

model_predict_COPE = do.call(rbind, model_predict_COPE)
names(model_eval_COPE) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_COPE$zoop_taxa <- "COPE"

COPE_compare <- ZOOP %>% select(event_year, site_code, park_code, COPE)%>%
  left_join(., model_predict_COPE, by = c("event_year", "site_code"))

RMSE <- COPE_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(COPE, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(COPE_compare, aes(x = abundance, y = COPE, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed COPE")),
       x = expression(paste("predicted COPE")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(COPE_compare, aes(x = event_year, y = COPE))+
  geom_point(aes(x = event_year, y = COPE, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(COPE Abundance)")), x = "", title = "COPE lme models (fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_MICRO <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_MICRO <- list()

for(s in 1:length(sites)){

  MICRO_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,MICRO ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(MICRO_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  MICRO_model_eval <- cbind(MICRO_model_eval=MICRO_model_eval, event_year)
  names(MICRO_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_MICRO[[s]] <- MICRO_model_eval

  MICRO_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,MICRO ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_MICRO[s,] <- as.data.frame(rbind(MICRO_site_summary))

}

model_predict_MICRO = do.call(rbind, model_predict_MICRO)
names(model_eval_MICRO) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_MICRO$zoop_taxa <- "MICRO"

MICRO_compare <- ZOOP %>% select(event_year, site_code, park_code, MICRO)%>%
  left_join(., model_predict_MICRO, by = c("event_year", "site_code"))

RMSE <- MICRO_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(MICRO, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(MICRO_compare, aes(x = abundance, y = MICRO, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed MICRO")),
       x = expression(paste("predicted MICRO")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(MICRO_compare, aes(x = event_year, y = MICRO))+
  geom_point(aes(x = event_year, y = MICRO, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(MICRO Abundance)")), x = "", title = "MICRO lme models (fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")


model_eval_RAP <- data.frame(matrix(NA, nrow = length(sites),ncol = 5))
model_predict_RAP <- list()

for(s in 1:length(sites)){

  RAP_model_eval <- ZOOP %>% filter(site_code == sites[s]) %>%
    lme(na.action=na.omit,RAP ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .) %>%
    predict(.) %>% as.data.frame(.) %>% mutate(site_code = sites[s])
  a <- rownames(RAP_model_eval)
  event_year <- ZOOP %>% filter(site_code == sites[s]) %>% select(event_year)
  RAP_model_eval <- cbind(RAP_model_eval=RAP_model_eval, event_year)
  names(RAP_model_eval) <- c("abundance", "site_code", "event_year")

  model_predict_RAP[[s]] <- RAP_model_eval

  RAP_site_summary <- ZOOP %>% filter(site_code == sites[s]) %>%
    do(glance(lme(na.action=na.omit,RAP ~ lake_temp + Ca + Chlorophyll, random = ~ 1|park_code, method="ML",data = .)))%>%
    as.data.frame() %>% mutate(model=sites[s]) %>% dplyr::select(sigma,logLik,AIC,BIC,model)

  model_eval_RAP[s,] <- as.data.frame(rbind(RAP_site_summary))

}

model_predict_RAP = do.call(rbind, model_predict_RAP)
names(model_eval_RAP) <- c("sigma","logLik","AIC","BIC","site_code")
model_eval_RAP$zoop_taxa <- "RAP"

RAP_compare <- ZOOP %>% select(event_year, site_code, park_code, RAP)%>%
  left_join(., model_predict_RAP, by = c("event_year", "site_code"))

RMSE <- RAP_compare%>%
  group_by(site_code, park_code)%>%
  summarise(RMSE = rmse(RAP, abundance))

ggplot(RMSE, aes(park_code, RMSE))+
  geom_boxplot(aes(fill = park_code))+
  geom_jitter(aes(color = site_code), size = 3, width = 0.05)

ggplot(RAP_compare, aes(x = abundance, y = RAP, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed RAP")),
       x = expression(paste("predicted RAP")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()

ggplot(RAP_compare, aes(x = event_year, y = RAP))+
  geom_point(aes(x = event_year, y = RAP, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(RAP Abundance)")), x = "", title = "RAP lme models (fish)")+
  geom_line(aes(x = event_year, y = abundance, group = site_code, color = park_code), lwd = 1)+
  geom_point(aes(x = event_year, y = abundance, group = site_code, bg = park_code), size=3, color = "black", pch = 21)+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")



### Perform an RDA on the Macro invertebrates
macro_cap <- capscale(formula = select(macro_aggregate, FCR:SHR) ~
                        Chlorophyll + Ca + lake_temp, data = macro_aggregate, distance = "bray")

macro_null <- capscale(formula = select(macro_aggregate, FCR:SHR) ~ 1,
                      data = macro_aggregate, distance = "bray")

mods_macro <- ordiR2step(macro_null, scope = formula(macro_cap), trace = 0, direction = c("forward"),
                   permutations = how(nperm = 999), steps = 100)
mods_macro$anova
model_site_points <- bind_cols(data.frame(scores(macro_cap)$sites),macro_aggregate)

all_data_model <- autoplot(macro_cap, layers = c("biplot", "species")) +
  geom_point(data = model_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(park_code), alpha = 0.5), size = 2.5)+
  theme_classic()+
  labs(title = "Aquatic Macroinvertebrate RDA by Taxon")

all_data_model
ggsave("./figures/CAPSCALE_model_output_macros.jpg", width = 10, height = 10, units = "in")
