set.seed(71)


### Perform an RDA on the Zooplankton
zoop_cap <- capscale(formula = select(zoop_fish, CLAD:RAP) ~
                       Chlorophyll + Elevation_m + SWE_May_snotel + secchi_value_m + flush_index_SWE_May_snotel + DOC + TDS +
                       Ca + solar_jas, data = zoop_fish, distance = "bray")

zoop_null <- capscale(formula = select(zoop_fish, CLAD:RAP) ~ 1,
                      data = zoop_fish, distance = "bray")

mods <- ordiR2step(zoop_null, scope = formula(zoop_cap), trace = 0, direction = c("forward"),
                   permutations = how(nperm = 999), steps = 100)
mods$anova
model_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),zoop_fish)

all_data_model <- autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = model_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(park_code), alpha = 0.5), size = 4)+
  theme_classic()

all_data_model
ggsave("./figures/CAPSCALE_model_output.jpg", width = 10, height = 10, units = "in")


library(PerformanceAnalytics)

chart.Correlation(zoop_fish[,c(4:27)], histogram = TRUE, method = "pearson")

CLAD_OLY <- zoop_fish %>% filter(park_code == "MORA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_CLAD = as.numeric(lag(CLAD)),
         k = 1.7/secchi_value_m)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

OLY_CLAD_MODEL <- glm(CLAD~k+lag_CLAD, data = CLAD_OLY, na.action = "na.fail")
summary(OLY_CLAD_MODEL)
OLY_CLAD_PREDICT <- data.frame(CLAD_OLY$site_code,CLAD_OLY$event_year,
                               predict(OLY_CLAD_MODEL, se.fit = T),CLAD_OLY$CLAD)
names(OLY_CLAD_PREDICT) <- c("site_code", "event_year",
                             "predicted_CLAD", "se_fit", "resid_scale", "observed_CLAD")
OLY_CLAD_PREDICT$park_code <- "OLYM"

CLAD_NOCA <- zoop_fishless %>% filter(park_code == "NOCA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_CLAD = as.numeric(lag(CLAD)),
         k = 1.7/secchi_value_m)%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

NOCA_CLAD_MODEL <- glm(CLAD~SWE_May_snotel+lag_CLAD, data = CLAD_NOCA, na.action = "na.fail")
summary(NOCA_CLAD_MODEL)
NOCA_CLAD_PREDICT <- data.frame(CLAD_NOCA$site_code,CLAD_NOCA$event_year,
                                predict(NOCA_CLAD_MODEL, se.fit = T), CLAD_NOCA$CLAD)
names(NOCA_CLAD_PREDICT) <- c("site_code", "event_year",
                              "predicted_CLAD", "se_fit", "resid_scale", "observed_CLAD")
NOCA_CLAD_PREDICT$park_code <- "NOCA"

# Now make one big DF ###
mod_pre <- rbind(NOCA_CLAD_PREDICT, OLY_CLAD_PREDICT, deparse.level = 1)

### plotting of STATS ###

a <- ggplot(mod_pre, aes(x = event_year, y = observed_CLAD))+
  geom_point(aes(x = event_year, y = observed_CLAD, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(CLAD Abundance)")), x = "", title = "")+
  geom_line(aes(x = event_year, y = predicted_CLAD, group = site_code, color = park_code), lwd = 1)+
  geom_ribbon(aes(ymin = predicted_CLAD - se_fit, ymax = predicted_CLAD + se_fit, group = site_code, fill = park_code), alpha = 0.4)+
  geom_point(aes(x = event_year, y = predicted_CLAD, group = site_code), size=3, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")

a

b <- ggplot(mod_pre, aes(x = predicted_CLAD, y = observed_CLAD, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  #geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed CLAD")),
       x = expression(paste("predicted CLAD")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()
b


RAP_OLY <- zoop_fishless %>% filter(park_code == "OLYM")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_RAP = as.numeric(lag(RAP)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)
OLY_RAP_MODEL <- glm(RAP~SWE_May_snotel+lag_RAP, data = RAP_OLY, na.action = "na.fail")
summary(OLY_RAP_MODEL)
OLY_RAP_PREDICT <- data.frame(RAP_OLY$site_code,RAP_OLY$event_year,
                              predict(OLY_RAP_MODEL, se.fit = T),RAP_OLY$RAP)
names(OLY_RAP_PREDICT) <- c("site_code", "event_year",
                            "predicted_RAP", "se_fit", "resid_scale", "observed_RAP")
OLY_RAP_PREDICT$park_code <- "OLYM"

RAP_NOCA <- zoop_fishless %>% filter(park_code == "NOCA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_RAP = as.numeric(lag(RAP)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

NOCA_RAP_MODEL <- glm(RAP~SWE_May_snotel+lag_RAP, data = RAP_NOCA, na.action = "na.fail")
summary(NOCA_RAP_MODEL)
NOCA_RAP_PREDICT <- data.frame(RAP_NOCA$site_code,RAP_NOCA$event_year,
                               predict(NOCA_RAP_MODEL, se.fit = T),RAP_NOCA$RAP)
names(NOCA_RAP_PREDICT) <- c("site_code", "event_year",
                             "predicted_RAP", "se_fit", "resid_scale", "observed_RAP")
NOCA_RAP_PREDICT$park_code <- "NOCA"

# Now make one big DF ###
mod_pre <- rbind(NOCA_RAP_PREDICT, OLY_RAP_PREDICT, deparse.level = 1)

### plotting of STATS ###

a <- ggplot(mod_pre, aes(x = event_year, y = observed_RAP))+
  geom_point(aes(x = event_year, y = observed_RAP, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(RAP Abundance)")), x = "", title = "")+
  geom_line(aes(x = event_year, y = predicted_RAP, group = site_code), lwd = 1, color = "blue")+
  geom_ribbon(aes(ymin = predicted_RAP - se_fit, ymax = predicted_RAP + se_fit, group = site_code), fill = "blue", alpha = 0.4)+
  geom_point(aes(x = event_year, y = predicted_RAP, group = site_code), size=3, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")

a

b <- ggplot(mod_pre, aes(x = predicted_RAP, y = observed_RAP, fill = site_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  #geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed RAP")),
       x = expression(paste("predicted RAP")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()
b


COPE_OLY <- zoop_fishless %>% filter(park_code == "OLYM")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_COPE = as.numeric(lag(COPE)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

OLY_COPE_MODEL <- glm(COPE~SWE_May_snotel+lag_COPE, data = COPE_OLY, na.action = "na.fail")
summary(OLY_COPE_MODEL)
OLY_COPE_PREDICT <- data.frame(COPE_OLY$site_code,COPE_OLY$event_year,
                               predict(OLY_COPE_MODEL, se.fit = T),COPE_OLY$COPE)
names(OLY_COPE_PREDICT) <- c("site_code", "event_year",
                             "predicted_COPE", "se_fit", "resid_scale", "observed_COPE")
OLY_COPE_PREDICT$park_code <- "OLYM"

COPE_NOCA <- zoop_fishless %>% filter(park_code == "NOCA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_COPE = as.numeric(lag(COPE)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

t <- glm(COPE~lag_value+lake_temp+Ca+NtoP, data = fully_ts_MORA, na.action = "na.fail")

all_models <- dredge(t, extra = c("R^2","adjR^2"), fixed = "lag_value", rank = "AICc")

best_model <- as.data.frame(subset(all_models, delta <= 2))

NOCA_COPE_MODEL <- glm(COPE~ice_out_doy+lag_COPE+solar_jas+, data = COPE_NOCA, na.action = "na.fail")
summary(NOCA_COPE_MODEL)
NOCA_COPE_PREDICT <- data.frame(COPE_NOCA$site_code,COPE_NOCA$event_year,
                                predict(NOCA_COPE_MODEL, se.fit = T),COPE_NOCA$COPE)
names(NOCA_COPE_PREDICT) <- c("site_code", "event_year",
                              "predicted_COPE", "se_fit", "resid_scale", "observed_COPE")
NOCA_COPE_PREDICT$park_code <- "NOCA"

# Now make one big DF ###
mod_pre <- rbind(NOCA_COPE_PREDICT, OLY_COPE_PREDICT, deparse.level = 1)

### plotting of STATS ###

a <- ggplot(mod_pre, aes(x = event_year, y = observed_COPE))+
  geom_point(aes(x = event_year, y = observed_COPE, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(COPE Abundance)")), x = "", title = "")+
  geom_line(aes(x = event_year, y = predicted_COPE, group = site_code), lwd = 1, color = "blue")+
  geom_ribbon(aes(ymin = predicted_COPE - se_fit, ymax = predicted_COPE + se_fit, group = site_code), fill = "blue", alpha = 0.4)+
  geom_point(aes(x = event_year, y = predicted_COPE, group = site_code), size=3, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")

a

b <- ggplot(mod_pre, aes(x = predicted_COPE, y = observed_COPE, fill = site_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed COPE")),
       x = expression(paste("predicted COPE")), title = "")+
  ylim(c(1,5))+
  xlim(c(1,5))+
  facet_wrap(~site_code)+
  theme_bw()
b

MICRO_OLY <- zoop_fishless %>% filter(park_code == "OLYM")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_MICRO = as.numeric(lag(MICRO)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

OLY_MICRO_MODEL <- glm(MICRO~lake_temp+lag_MICRO, data = MICRO_OLY, na.action = "na.fail")
summary(OLY_MICRO_MODEL)
OLY_MICRO_PREDICT <- data.frame(MICRO_OLY$site_code,MICRO_OLY$event_year,
                                predict(OLY_MICRO_MODEL, se.fit = T),MICRO_OLY$MICRO)
names(OLY_MICRO_PREDICT) <- c("site_code", "event_year",
                              "predicted_MICRO", "se_fit", "resid_scale", "observed_MICRO")
OLY_MICRO_PREDICT$park_code <- "OLYM"

MICRO_NOCA <- zoop_fishless %>% filter(park_code == "NOCA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(lag_MICRO = as.numeric(lag(MICRO)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)
NOCA_MICRO_MODEL <- glm(MICRO~flush_index_SWE_May_snotel+lag_MICRO, data = MICRO_NOCA, na.action = "na.fail")
summary(NOCA_MICRO_MODEL)
NOCA_MICRO_PREDICT <- data.frame(MICRO_NOCA$site_code,MICRO_NOCA$event_year,
                                 predict(NOCA_MICRO_MODEL, se.fit = T),MICRO_NOCA$MICRO)
names(NOCA_MICRO_PREDICT) <- c("site_code", "event_year",
                               "predicted_MICRO", "se_fit", "resid_scale", "observed_MICRO")
NOCA_MICRO_PREDICT$park_code <- "NOCA"

# Now make one big DF ###
mod_pre <- rbind(NOCA_MICRO_PREDICT, OLY_MICRO_PREDICT, deparse.level = 1)

### plotting of STATS ###

a <- ggplot(mod_pre, aes(x = event_year, y = observed_MICRO))+
  geom_point(aes(x = event_year, y = observed_MICRO, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(MICRO Abundance)")), x = "", title = "")+
  geom_line(aes(x = event_year, y = predicted_MICRO, group = site_code), lwd = 1, color = "blue")+
  geom_ribbon(aes(ymin = predicted_MICRO - se_fit, ymax = predicted_MICRO + se_fit, group = site_code), fill = "blue", alpha = 0.4)+
  geom_point(aes(x = event_year, y = predicted_MICRO, group = site_code), size=3, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")

a

b <- ggplot(mod_pre, aes(x = predicted_MICRO, y = observed_MICRO, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed MICRO")),
       x = expression(paste("predicted MICRO")), title = "")+
  ylim(c(1,5))+
  xlim(c(1,5))+
  facet_wrap(~site_code)+
  theme_bw()
b


GR_OLY <- zoop_fishless %>% filter(park_code == "OLYM")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(GR = (RAP+MICRO),
         lag_GR = as.numeric(lag(GR)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

OLY_GR_MODEL <- glm(GR~SWE_May_snotel+lag_GR, data = GR_OLY, na.action = "na.fail")
summary(OLY_GR_MODEL)
OLY_GR_PREDICT <- data.frame(GR_OLY$site_code,GR_OLY$event_year,
                             predict(OLY_GR_MODEL, se.fit = T),GR_OLY$GR)
names(OLY_GR_PREDICT) <- c("site_code", "event_year",
                           "predicted_GR", "se_fit", "resid_scale", "observed_GR")
OLY_GR_PREDICT$park_code <- "OLYM"

GR_NOCA <- zoop_fishless %>% filter(park_code == "NOCA")%>%
  group_by(site_code, event_year)%>%
  summarize_all(funs(mean), na.rm = F)%>%
  mutate(GR = (RAP+MICRO),
         lag_GR = as.numeric(lag(GR)))%>%
  arrange(event_year)%>%
  ungroup(.)%>%
  select(-park_code)%>%
  na.omit(.)

NOCA_GR_MODEL <- glm(GR ~ lake_temp + lag_GR, data = GR_NOCA, na.action = "na.fail")
summary(NOCA_GR_MODEL)
NOCA_GR_PREDICT <- data.frame(GR_NOCA$site_code,GR_NOCA$event_year,
                              predict(NOCA_GR_MODEL, se.fit = T),GR_NOCA$GR)
names(NOCA_GR_PREDICT) <- c("site_code", "event_year",
                            "predicted_GR", "se_fit", "resid_scale", "observed_GR")
NOCA_GR_PREDICT$park_code <- "NOCA"

# Now make one big DF ###
mod_pre <- rbind(NOCA_GR_PREDICT, OLY_GR_PREDICT, deparse.level = 1)

### plotting of STATS ###

a <- ggplot(mod_pre, aes(x = event_year, y = observed_GR))+
  geom_point(aes(x = event_year, y = observed_GR, group = site_code), size=3, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("log_10(GR Abundance)")), x = "", title = "")+
  geom_line(aes(x = event_year, y = predicted_GR, group = site_code, color = park_code), lwd = 1)+
  geom_ribbon(aes(ymin = predicted_GR - se_fit, ymax = predicted_GR + se_fit, group = site_code, fill = park_code), alpha = 0.4)+
  geom_point(aes(x = event_year, y = predicted_GR, group = site_code), size=3, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_bw()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 45))+
  facet_wrap(~site_code,scales = "free")

a

b <- ggplot(mod_pre, aes(x = predicted_GR, y = observed_GR, fill = park_code))+
  geom_point(size = 3, pch = 21)+
  geom_abline(intercept = 0, slope = 1, lwd = 1)+
  #geom_smooth(method = lm, se = T)+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))+
  labs(y = expression(paste("observed GR")),
       x = expression(paste("predicted GR")), title = "")+
  ylim(c(0,5))+
  xlim(c(0,5))+
  theme_bw()
b

### Perform an RDA on the Macro invertebrates
macro_cap <- capscale(formula = select(macro_fishless, Acari:Veneroida) ~
                        Chlorophyll + Elevation_m + SWE_May_snotel +
                        Ca + solar_jas, data = macro_fishless, distance = "bray")

macro_null <- capscale(formula = select(macro_fishless, Acari:Veneroida) ~ 1,
                       data = macro_fishless, distance = "bray")

mods_macro <- ordiR2step(macro_null, scope = formula(macro_cap), trace = 0, direction = c("forward"),
                         permutations = how(nperm = 999), steps = 100)
mods_macro$anova
model_site_points <- bind_cols(data.frame(scores(macro_cap)$sites),macro_join)

all_data_model <- autoplot(macro_cap, layers = c("biplot", "species")) +
  geom_point(data = model_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(park_code), alpha = 0.5), size = 2.5)+
  theme_classic()+
  labs(title = "Aquatic Macroinvertebrate RDA by Taxon")

all_data_model
ggsave("./figures/CAPSCALE_model_output_macros.jpg", width = 10, height = 10, units = "in")
