##############################
# REQUIRE PACKAGES WITH PACMAN
##############################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,PerformanceAnalytics,reshape2,ISLR,rpart,rpart.plot,
               zoo,tidyverse,rjags,tidybayes,scales,gridExtra,readxl,vegan,
               labdsv,viridis,patchwork,plotly,janitor,ggpubr,visdat,corrplot,
               randomForest,data.table)

source("./R/functions/ScreePlotFunction.R")
##############################

# Compile data as though we are moving
# from physical, to chemical, and finally biological

###################################
# PHYSICAL AND CHEMICAL COMPILATION
###################################
env_dat_all <- readRDS(file = "./data/bigjoin.rds")

env_dat_yr <- env_dat_all %>%
  filter(variable %in% c("secchi_value_m", "AirTemp", "BotTemp",
                         "MidTemp", "SurfTemp", "DO_top2m","Ca","Chlorophyll",
                         "DOC","K","NH4-N","NO3-N","PO4","SO4","TDS","Total N","Total P")) %>%
  select(park_code, Lake, site_code, event_year, variable, value) %>%
  unique() %>%
  spread(key = variable, value = value)%>%
  mutate(event_year = as.character(event_year))

write_csv(env_dat_yr, "dat_temp.csv")

#########################
# ZOOPLANKTON COMPILATION
#########################

zoop <- read_excel("./data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5
zoop_no_rotifers <- zoop %>%
  filter(Taxa != "ROT")%>%
  filter(Taxa != "INS")

#Partition Rotifers into functional feeding groups
rot_wFFG <- zoop %>% filter(Taxa == "ROT")
rot_wFFG$GenSp <- sub(" .*", "", rot_wFFG$GenSp)

rot_wFFG <- rot_wFFG %>%
  mutate(trophi = case_when(
    GenSp == "Conochilius" ~ "malleoramate",
    GenSp == "Kellicottia" ~ "malleate",
    GenSp == "Keratella" ~ "malleate",
    GenSp == "Lecane" ~ "malleate",
    GenSp == "Monostyla" ~ "malleate",
    GenSp == "Polyarthra" ~ "virgate",
    GenSp == "Trichotria" ~ "malleate",
    GenSp == "Synchaeta" ~ "virgate",
    GenSp == "Collotheca" ~ "uncinate",
    GenSp == "Notholca" ~ "malleate",
    GenSp == "Brachionus" ~ "malleate",
    GenSp == "Gastropus" ~ "virgate",
    GenSp == "Philodina" ~ "malleate",
    GenSp == "Ascomorpha" ~ "virgate",
    GenSp == "Filinia" ~ "malleoramate",
    GenSp == "Trichocerca" ~ "virgate",
    GenSp == "Ploesoma" ~ "virgate",
    GenSp == "Conochiloides" ~ "malleoramate",
    TRUE ~ NA_character_),
    feed_type = case_when(
      trophi == "uncinate" ~ "RAP",
      trophi == "virgate" ~ "RAP",
      trophi == "malleate" ~ "MICRO",
      trophi == "malleoramate" ~ "MICRO",
      TRUE ~ NA_character_))%>%
  select(`Park&Year&Lake`,Park, Year, Lake, Code, Density, GenSp, feed_type, Taxonid)%>%
  rename(Taxa = feed_type)

match_sites <- readRDS(file = "./data/name_site_matches.rds")

# Matrify the DF
zoop <- bind_rows(zoop_no_rotifers, rot_wFFG)%>%
  left_join(x = ., y = match_sites, by = c("Lake" = "old_name"))%>%
  clean_names()%>%
  group_by(park, site_code, lake, year, taxa) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  filter(prop_density >= .05)%>%
  mutate(park_site_year = paste0(park, "_", lake, "_", year)) %>%
  select(park_site_year, taxa, prop_density) %>%
  data.frame() %>%
  matrify()

zoop_scomm_ids <- rownames_to_column(zoop) %>%
  separate(col = rowname, into = c("park_code", "site_code", "event_year"), sep = "_")%>%
  mutate(event_year = as.character(event_year))

# Determine the proportion
zoop_props <- as.data.frame(zoop) %>%
  bind_cols(park_site_year = row.names(.), .) %>%
  separate(data = ., col = park_site_year, into = c("park", "site", "year"),
           sep = "_") %>%
  gather(key = taxa, value = proportion, -park, -site, -year)

NOCA_zoop_prop_plot <- zoop_props %>%
  filter(park == "NOCA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "NOCA")

OLYM_zoop_prop_plot <- zoop_props %>%
  filter(park == "OLYM") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "OLYM")

MORA_zoop_prop_plot <- zoop_props %>%
  filter(park == "MORA") %>%
  ggplot() +
  geom_bar(aes(x = year, y = proportion, fill = taxa),
           stat = "identity", position = "fill", color = "gray48") +
  facet_wrap(. ~ site)+
  geom_vline(xintercept = "2015", lwd = 1, color = "white", lty = "dotted")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")+
  ggtitle(label = "MORA")

zoop_proportions <- (OLYM_zoop_prop_plot+MORA_zoop_prop_plot)/
                    (NOCA_zoop_prop_plot+plot_spacer())
zoop_proportions

ggsave(path = ".", filename = "./figures/Zooplankton Proportions.jpg",
       width = 20, height = 12, device='jpg', dpi=500)

##############################
# BIND ENV DATA WITH ZOOP DATA
##############################

data <- read_csv("./data/dat_temp.csv")%>%
  mutate(event_year = as.character(event_year))%>%
  select(-site_code)%>%
  rename(site_code = Lake)



envs_zoops <- left_join(data, zoop_scomm_ids,
                        by = c("park_code","site_code","event_year"))%>%
  select(-`NH4-N`,-`NO3-N`,-TDS)

vis_dat(x = envs_zoops)


## ---------------------------------------------------------------------
# Down some rows after removing NAs...
envs_zoops <- na.omit(envs_zoops)

corrplot(cor(select(envs_zoops, AirTemp:RAP)), type = "upper",
         tl.col = "black", tl.srt = 45)

subset_nmds <- metaMDS(comm = select(envs_zoops, CLAD:RAP),
                       distance = "bray", k = 3, trymax = 100)
## ----echo=FALSE-------------------------------------------------------
subset_nmds

plot(subset_nmds)

nmds_scores_subset <- cbind(as_tibble(scores(subset_nmds)),
                            select(envs_zoops, park_code:event_year))

library(plotly)
fig <- plot_ly(nmds_scores_subset, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~park_code, colors = c('#BF382A', '#0C4B8E','green'))
fig <- fig %>% add_markers()
fig
