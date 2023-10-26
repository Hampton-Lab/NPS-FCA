#Random Forest Script
set.seed(71)

directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(directory,"/config/zoop_RF_config.yml")))

zoop_data <- read_csv(config$file_path$zoop_data)%>%
  mutate(event_year = as.character(event_year))
  # mutate(RAP = log10(RAP+1),
  #        MICRO = log10(MICRO+1),
  #        CLAD = log10(CLAD+1),
  #        COPE = log10(COPE+1))

env_zoop_data <- read_csv(config$file_path$environment_data)%>%
  mutate(event_year = as.character(event_year))%>%
  left_join(., zoop_data, by = c("park_code","site_code","event_year"))

write_csv(env_zoop_data, "ZoopData.csv")

park = config$attributes$park_code
taxa = config$attributes$taxa


com = env_zoop_data[,22:25]
m_com = as.matrix(com)

ord = metaMDS(m_com, distance = "bray", trace = F, autotransform = T)

scores <- as.data.frame(scores(ord))
scores <- cbind(scores, env_zoop_data[, c(1:3,30)])
colnames(scores)[3] <- "Year"
colnames(scores)[4] <- "Site"
colnames(scores)[5] <- "Park"
colnames(scores)[6] <- "May Snowpack"
species_scores <- as.data.frame(scores(ord, "species")) %>% na.omit(.)
species_scores$species <- rownames(species_scores)

ggplot() +
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
  #           alpha = 0.5) +
  geom_point(data = scores, aes(x = NMDS1, y = NMDS2,
                                color = `May Snowpack`), size = 3) +
  scale_color_manual(values = inferno(15)[c(3, 8, 11)],
                     name = "High vs. Low SWE") +
  annotate(geom = "label", x = -0.5, y = 0.5,
           label = paste("Stress: ", round(ord$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right")






nmds_scores <- cbind(as_tibble(scores(ord)),
                     RF_data[1:2])

NMDS = data.frame(MDS1 = ord$points[,1], MDS2 = ord$points[,2], lake = RF_data[,2], park = RF_data[,1])
simp <- simper(m_com, RF_data$park_code)
summary(simp)
vec.sp<-envfit(ord$points, RF_data[,-c(1,2,3,13,14,15,16)], strata = RF_data$site_code, perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)

zoop_cap <- capscale(formula = select(RF_data, CLAD:RAP) ~
                       stability + Chlorophyll + ice_out_doy + Elevation_m +
                       Ca + solar_jas + DOC + lake_temp +`Total N` +
                       `Total P` + Depth_max,
                     data = RF_data, distance = "bray")

zoop_null <- capscale(formula = select(RF_data, CLAD:RAP) ~ 1,
                      data = RF_data, distance = "bray")

mods <- ordistep(zoop_null, scope = formula(zoop_cap), trace = 0)

mods$anova

vif.cca(zoop_cap)



custom_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),
                                RF_data)
library(ggvegan)

all_data_model <- autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = custom_site_points,
             aes(x = CAP1, y = CAP2, color = as.character(site_code), alpha = 0.5), size = 2)




plot_test <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
  geom_point(data = nmds_scores, aes(NMDS1, NMDS2, color = site_code ,inherit_aes=F), size = 3)+
  #stat_ellipse(geom = "polygon", aes(group = park_code, color = park_code, fill = park_code), alpha = 0.1) +
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=F) +
  geom_text(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=3)+
  xlim(c(-1,1.1))+
  ylim(c(-0.6,0.6))+
  theme_bw()

plot_test <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
  geom_point(data = nmds_scores, aes(NMDS1, NMDS2, color = park_code ,inherit_aes=F), size = 3)+
  #stat_ellipse(geom = "polygon", aes(group = park_code, color = park_code, fill = park_code), alpha = 0.1) +
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=F) +
  geom_text(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=3)+
  xlim(c(-1,1.1))+
  ylim(c(-0.6,0.6))+
  theme_bw()

#stress 0.128



#
# ZOOP_all <- env_zoop_data %>%
#   group_by(site_code, event_year, park_code)%>%
#   summarize_all(funs(mean), na.rm = F)%>%
#   arrange(event_year)%>%
#   ungroup(.)%>%
#   select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll, fish)%>%
#   mutate(site_code = case_when(
#     site_code == "LH15" ~ 1,
#     site_code == "Allen" ~ 2,
#     site_code == "LP19" ~ 3,
#     site_code == "Deadwood" ~ 4,
#     site_code == "Blue" ~ 5,
#     site_code == "Blum" ~ 6,
#     site_code == "Silent" ~ 7,
#     site_code == "EasyRidge" ~ 8,
#     site_code == "East" ~ 9,
#     site_code == "Bowan" ~ 10,
#     site_code == "Triplet" ~ 11,
#     site_code == "Gladys" ~ 12,
#     site_code == "Ferry" ~ 13,
#     site_code == "Heather" ~ 14,
#     site_code == "Crazy" ~ 15,
#     site_code == "Milk" ~ 16,
#     site_code == "LaCrosse" ~ 17,
#     site_code == "Sunup" ~ 18,
#     site_code == "Connie" ~ 19))%>%
#   melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish"))
#
#
#
#
# alpha_diversity_ZOOP <- ZOOP_all %>%
#   group_by(variable, event_year, site_code) %>%
#   mutate(value = ifelse(value<=0,0,value)) %>%
#   summarise(value = sum(value))%>%
#   group_by(site_code, event_year) %>%
#   summarize(alpha = vegan::diversity(value,
#                                      index = "simpson"))
#
# beta_diversity_ZOOP <- ZOOP_all %>%
#   group_by(variable, event_year, park_code) %>%
#   mutate(value = ifelse(value<=0,0,value)) %>%
#   summarise(value = sum(value))%>%
#   group_by(park_code, event_year) %>%
#   summarize(alpha = vegan::diversity(value,
#                                      index = "simpson"))
#
# gamma_diversity_ZOOP <- ZOOP_all %>%
#   group_by(variable, event_year) %>%
#   mutate(value = ifelse(value<=0,0,value)) %>%
#   summarise(value = sum(value))%>%
#   group_by(event_year) %>%
#   summarize(alpha = vegan::diversity(value,
#                                      index = "simpson"))
#
#
# ZOOP_fishless <- env_zoop_data %>% filter(fish == 0) %>%
#   group_by(site_code, event_year, park_code)%>%
#   summarize_all(funs(mean), na.rm = F)%>%
#   arrange(event_year)%>%
#   ungroup(.)%>%
#   select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
#   mutate(site_code = case_when(
#     site_code == "Allen" ~ 1,
#     site_code == "Bowan" ~ 2,
#     site_code == "Milk" ~ 3,
#     site_code == "Silent" ~ 4,
#     site_code == "Triplet" ~ 5,
#     site_code == "Sunup" ~ 6,
#     site_code == "Blum" ~ 7,
#     site_code == "Connie" ~ 8,
#     site_code == "Crazy" ~ 9,
#     site_code == "East" ~ 10,
#     site_code == "EasyRidge" ~ 11,
#     site_code == "Ferry" ~ 12,
#     site_code == "LaCrosse" ~ 13))%>%
#   melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))
#
#
# ZOOP_fish <- env_zoop_data %>% filter(fish == 1) %>%
#   group_by(site_code, event_year, park_code)%>%
#   summarize_all(funs(mean), na.rm = F)%>%
#   arrange(event_year)%>%
#   ungroup(.)%>%
#   select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll)%>%
#   mutate(site_code = case_when(
#     site_code == "Blue" ~ 1,
#     site_code == "Gladys" ~ 2,
#     site_code == "Heather" ~ 3,
#     site_code == "LP19" ~ 4,
#     site_code == "Deadwood" ~ 5,
#     site_code == "LH15" ~ 6))%>%
#   melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll"))
#
# ZOOP_by_site <- env_zoop_data %>%
#   group_by(site_code, event_year, park_code)%>%
#   summarize_all(funs(mean), na.rm = F)%>%
#   arrange(event_year)%>%
#   ungroup(.)%>%
#   select(event_year, site_code, park_code, COPE, CLAD, MICRO, RAP, Ca, lake_temp, Chlorophyll, fish)%>%
#   melt(., id.vars = c("event_year","site_code","park_code","Ca","lake_temp","Chlorophyll","fish"))
