
env_dat_yr_swe <- env_dat_yr %>% select(park_code, site_code, event_year, SWE_May_snotel, ice_out_doy) %>%
  mutate(event_year = as.numeric(event_year))

########

zoop_dens_year <- read_csv("./data/zoop_dens_genus.csv") %>%
  group_by(year, lake, park) %>%
  rename(event_year = year, site_code = lake, park_code = park) %>%
  melt(., id.vars = c("event_year","site_code", "park_code")) %>%
  group_by(event_year, site_code) %>%
  summarize(simpson = vegan::diversity(value, "shannon"))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code"))%>%
  group_by(event_year)%>%
  summarise(simpson_div = mean(simpson),
            simpson_sd = std.error(simpson),
            SWE = mean(SWE_May_snotel, na.rm = T),
            sd_SWE = std.error(SWE_May_snotel, na.rm = T))%>%
  filter(event_year>2007)

ggplot(zoop_dens_year, aes(SWE,simpson_div))+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
                    ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
                    xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  labs(title = "Zooplankton Evenness", y = "Species Evenness (shannon index)", x = "May SWE depth (cm)")+
  theme_classic()+
  theme(legend.title = element_blank())

# ggplot(zoop_dens_year, aes(event_year,simpson_div))+
#   geom_point(aes(fill = SWE),color = "black", pch = 21, size = 5)+
#   geom_errorbar(aes(x=event_year, ymin = simpson_div - simpson_sd,
#                     ymax = simpson_div + simpson_sd), color="grey80", width=1)+
#   theme_classic()

##########

zoop_dens_site <- read_csv("./data/zoop_dens_genus.csv") %>%
  group_by(year, lake, park) %>%
  rename(event_year = year, site_code = lake, park_code = park) %>%
  melt(., id.vars = c("event_year","site_code", "park_code")) %>%
  group_by(site_code, event_year, park_code) %>%
  summarize(simpson = vegan::diversity(value, "shannon"))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code"))%>%
  group_by(site_code, event_year, park_code)%>%
  summarise(simpson_div = mean(simpson),
            simpson_sd = std.error(simpson),
            SWE = mean(SWE_May_snotel, na.rm = T),
            sd_SWE = std.error(SWE_May_snotel, na.rm = T))%>%
  filter(event_year>2007)

ggplot(zoop_dens_site, aes(SWE,simpson_div))+
  geom_point(aes(fill = park_code),color = "black", pch = 21, size = 5)+
  # geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
  #                   ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  # geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
  #                   xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  labs(title = "Zooplankton Evenness", y = "Species Evenness (shannon index)", x = "May SWE depth (cm)")+
  theme_classic()+
  theme(legend.title = element_blank())+
  facet_wrap(~site_code)

########

macro_aggregate_dens_all <- macro_aggregate %>%
  group_by(variable, site_code, event_year, park_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  summarise(sum_species = sum(sum_species))%>%
  group_by(site_code, event_year, park_code) %>%
  summarize(simpson = vegan::diversity(sum_species,
                                       index = "shannon"))%>%
  mutate(event_year = as.numeric(event_year))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code")) %>%
  filter(park_code != "OLYM")  %>%
  filter(event_year>2007)

ggplot(macro_aggregate_dens_all, aes(SWE_May_snotel,simpson))+
  geom_point(aes(fill = park_code),color = "black", pch = 21, size = 5)+
  # geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
  #                   ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  # geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
  #                   xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  labs(title = "BMI Evenness", y = "Species Evenness (shannon index)", x = "May SWE depth (cm)")+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  theme_classic()+
  facet_wrap(~site_code)+
  theme(legend.title = element_blank())

#########

diversity_BMI <- macro_aggregate %>%
  group_by(variable, event_year, site_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  group_by(event_year, park_code, site_code) %>%
  summarize(simpson = vegan::diversity(sum_species,
                                     index = "shannon"))%>%
  mutate(event_year = as.numeric(event_year))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code")) %>%
  filter(park_code != "OLYM") %>%
  group_by(event_year)%>%
  summarise(simpson_div = mean(simpson),
            simpson_sd = std.error(simpson),
            SWE = mean(SWE_May_snotel, na.rm = T),
            sd_SWE = std.error(SWE_May_snotel, na.rm = T))%>%
  filter(event_year>2007)

ggplot(diversity_BMI, aes(SWE,simpson_div))+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
                    ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
                    xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  labs(title = "BMI Evenness", y = "Species Evenness (shannon index)", x = "May SWE depth (cm)")+
  theme_classic()+
 theme(legend.title = element_blank())


ggplot(diversity_BMI, aes(event_year,simpson_div))+
  geom_point(aes(fill = SWE),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=event_year, ymin = simpson_div - simpson_sd,
                    ymax = simpson_div + simpson_sd), color="grey80", width=1)+
  theme_classic()


#########

diversity_BMI_site <- macro_aggregate %>%
  group_by(variable, site_code, event_year, park_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  mutate(sum_species = ifelse(sum_species>0,1,0)) %>%
  group_by(site_code, park_code, event_year) %>%
  summarize(evenness = sum(sum_species))%>%
  mutate(event_year = as.numeric(event_year))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code")) %>%
  filter(park_code != "OLYM")%>%
  filter(event_year>2008)

ggplot(diversity_BMI_site, aes(SWE_May_snotel,evenness))+
  geom_point(aes(fill = park_code),color = "black", pch = 21, size = 5)+
  # geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
  #                   ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  # geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
  #                   xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  labs(title = "BMI Richness", y = "Species Richness (total # species)", x = "May SWE depth (cm)")+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  theme_classic()+
  facet_wrap(~site_code)


########

diversity_BMI <- macro_aggregate %>%
  group_by(variable, event_year, site_code, park_code) %>%
  mutate(sum_species = ifelse(sum_species<=0,0,sum_species)) %>%
  mutate(sum_species = ifelse(sum_species>0,1,0)) %>%
  group_by(event_year, park_code, site_code) %>%
  summarize(evenness = vegan::diversity(sum_species, "shannon")/log(length(unique(variable))))%>%
  mutate(event_year = as.numeric(event_year))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code")) %>%
  filter(park_code != "OLYM") %>%
  group_by(event_year)%>%
  summarise(evenness2 = mean(evenness),
            evenness_se2 = std.error(evenness, na.rm = T),
            SWE = mean(SWE_May_snotel, na.rm = T),
            se_SWE = std.error(SWE_May_snotel, na.rm = T)) %>%
  filter(event_year>2008)



ggplot(diversity_BMI, aes(SWE,evenness2))+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=SWE, ymin = evenness2 - evenness_se2,
                    ymax = evenness2 + evenness_se2), color="grey80", width=1)+
  geom_errorbar(aes(y=evenness2, xmin = SWE - se_SWE,
                    xmax = SWE + se_SWE), color = "grey80", width=0.05)+
  labs(title = "BMI Richness", y = "Species Richness (total # species)", x = "May SWE depth (cm)")+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  theme_classic()+
  theme(legend.title = element_blank())


ggplot(diversity_BMI, aes(event_year,evenness2))+
  geom_line(color = "grey40")+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=event_year, ymin = evenness2 - evenness_se2,
                    ymax = evenness2 + evenness_se2), color="grey80", width=0.5)+
  labs(title = "BMI Richness", y = "Species Richness (total # species)", x = "Year")+
  theme_classic()+
  theme(legend.title = element_blank())









zoop_evenness <- read_csv("./data/zoop_dens_genus.csv") %>%
  group_by(year, lake, park) %>%
  rename(site_code = lake, event_year = year, park_code = park) %>%
  melt(., id.vars = c("event_year","site_code","park_code")) %>%
  mutate(value = ifelse(value > 0, 1, 0)) %>%
  group_by(site_code, park_code, event_year) %>%
  summarize(evenness = vegan::diversity(value, "shannon")/log(length(unique(variable))))%>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code"))%>%
  filter(event_year>2008)

ggplot(zoop_evenness, aes(SWE_May_snotel,evenness))+
  geom_point(aes(fill = park_code),color = "black", pch = 21, size = 5)+
  # geom_errorbar(aes(x=SWE, ymin = simpson_div - simpson_sd,
  #                   ymax = simpson_div + simpson_sd), color="grey80", width=10)+
  # geom_errorbar(aes(y=simpson_div, xmin = SWE - sd_SWE,
  #                   xmax = SWE + sd_SWE), color = "grey80", width=0.05)+
  labs(title = "Zooplankton Richness", y = "Species Richness (total # species)", x = "May SWE depth (cm)")+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  theme_classic()+
  facet_wrap(~site_code)







zoop_evenness2 <- read_csv("./data/zoop_dens_genus.csv") %>%
  group_by(year, lake, park) %>%
  rename(site_code = lake, event_year = year, park_code = park) %>%
  melt(., id.vars = c("event_year","site_code","park_code")) %>%
  mutate(value = ifelse(value > 0, 1, 0)) %>%
  group_by(event_year, site_code, park_code) %>%
  summarize(evenness = sum(value)) %>%
  left_join(., env_dat_yr_swe, by = c("event_year", "site_code", "park_code")) %>%
  group_by(event_year)%>%
  summarise(evenness2 = mean(evenness),
            evenness_se2 = std.error(evenness, na.rm = T),
            SWE = mean(SWE_May_snotel, na.rm = T),
            se_SWE = std.error(SWE_May_snotel, na.rm = T)) %>%
  filter(event_year>2008)

ggplot(zoop_evenness2, aes(SWE,evenness2))+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=SWE, ymin = evenness2 - evenness_se2,
                    ymax = evenness2 + evenness_se2), color="grey80", width=1)+
  geom_errorbar(aes(y=evenness2, xmin = SWE - se_SWE,
                    xmax = SWE + se_SWE), color = "grey80", width=0.05)+
  labs(title = "Zooplankton Richness", y = "Species Richness (total # species)", x = "May SWE depth (cm)")+
  geom_smooth(method = "lm", se = T, alpha = 0.2, color = "black")+
  theme_classic()+
  theme(legend.title = element_blank())


ggplot(zoop_evenness2, aes(event_year,evenness2))+
  geom_line(color = "grey40")+
  geom_point(aes(fill = as.character(event_year)),color = "black", pch = 21, size = 5)+
  geom_errorbar(aes(x=event_year, ymin = evenness2 - evenness_se2,
                    ymax = evenness2 + evenness_se2), color="grey80", width=0.5)+
  labs(title = "Zooplankton Richness", y = "Species Richness (total # species)", x = "Year")+
  theme_classic()+
  theme(legend.title = element_blank())
