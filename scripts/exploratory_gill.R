
library(readxl)
library(tidyverse)
library(lubridate)

# Bumbling around:

# Site = physical location in park?
site_meta <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b014_Sites_export_20180917_144805.xlsx"))

# Location = type of sampling that occurred? 
loc_meta <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b024_Locations_export_20180917_144833.xlsx"))

head(site_meta)
head(loc_meta)

# Something with more direction: 

# Take a look at fish
gill <- read_excel(
  path = file.path("..",
                   "data",
                   "NPS_NCCN_Mtn_Lakes_Exports_20190923",
                   "qs_b244_Fish_Gill_Net_Event_Specimens_20190923_121812.xlsx"))

unique(data.frame(gill %>% select(Park_code,Site_name,Fish_taxon_code)))

data.frame(gill$Park_code,Site_name,gill)

# How sampling many events per location?
gill %>%
  select(Park_code, Site_name, Location_code, tbl_Gill_Net_Event_Start_date) %>%
  group_by(Park_code, Site_name, Location_code) %>%
  unique() %>%
  tally()

# Number of fish by species by lake?
n_fish_plot <- gill %>%
  group_by(Park_code, Site_name, tbl_Gill_Net_Event_Start_date, Fish_taxon_code) %>%
  summarize(n_fish = n()) %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = n_fish,
             color = Fish_taxon_code)) +
  geom_point() +
  facet_wrap(~Site_name)

ggsave(plot = n_fish_plot,
       filename = "../figures/total-fish-by-lake_gill.png")

# Number of fish by species?
n_fish_agg_plot <- gill %>%
  group_by(tbl_Gill_Net_Event_Start_date, Fish_taxon_code) %>%
  summarize(n_fish = n()) %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = n_fish)) +
  geom_point()

ggsave(plot = n_fish_agg_plot,
       filename = "../figures/total-fish_gill.png")

# Fish length by species by lake?
length_lake_plot <- gill %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = Total_length_mm,
             color = Fish_taxon_code)) +
  geom_point() +
  facet_wrap(~Site_name)

ggsave(plot = length_lake_plot,
       filename = "../figures/total-length-by-lake_gill.png")

# Fish weight by species over time, across lakes?
weight_species_plot <- gill %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = Weight_g)) +
  geom_point() +
  facet_wrap(~Fish_taxon_code)

ggsave(plot = weight_species_plot,
       filename = "../figures/weight-by-species_gill.png")


# Fish weight:length over time
weight_to_Flength_plot <- gill %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = Weight_g / Fork_length_mm,
             color = Fish_taxon_code)) +
  geom_point()

ggsave(plot = weight_to_Flength_plot,
       filename = "../figures/weight-to-Flength_gill.png")

# Fish weight:length by lake over time
weight_to_Flength_lake_plot <- gill %>%
  ggplot(aes(x = tbl_Gill_Net_Event_Start_date,
             y = Weight_g / Fork_length_mm)) +
  geom_point() +
  facet_wrap(~Site_name)

ggsave(plot = weight_to_Flength_lake_plot,
       filename = "../figures/weight-to-Flength_by_lake_gill.png")





# Other exploration:

# Mean weight by species and location
gill %>%
  group_by(Park_code, Site_name, Location_code, Fish_taxon_code) %>%
  summarise(mean_weight = mean(Weight_g),
            mean_length = mean(Total_length_mm),
            mean_fork = mean(Fork_length_mm)) %>%
  ggplot() + 
  geom_point(aes(x = Site_name, y = mean_weight, color = Fish_taxon_code))

# Fish over time
gill %>%
  ggplot() +
  geom_point(aes(x = tbl_Gill_Net_Event_Start_date,
                 y = Weight_g,
                 shape = Fish_taxon_code,
                 fill = Site_code),
             pch = 21,
             color = "black", size = 3) + 
  scale_fill_viridis_d() 

gill %>%
  ggplot() +
  geom_point(aes(x = tbl_Gill_Net_Event_Start_date,
                 y = Total_length_mm,
                 shape = Fish_taxon_code,
                 fill = Site_code),
             pch = 21,
             color = "black", size = 3) + 
  scale_fill_viridis_d() 

gill %>%
  ggplot() +
  geom_point(aes(x = month(gill$tbl_Gill_Net_Event_Start_date),
                 y = Fork_length_mm,
                 shape = Fish_taxon_code,
                 color = Site_code),
             size = 3) + 
  scale_color_viridis_d() 


