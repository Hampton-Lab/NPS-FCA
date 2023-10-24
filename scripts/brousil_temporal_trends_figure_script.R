library(tidyverse)
library(ggrepel)
library(janitor)
library(readxl)


# 1. Plot bigjoin variables -----------------------------------------------

bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

head(bigjoin)

bigjoin_classify <- bigjoin %>%
  mutate(variable_group = case_when(
    grepl(pattern = "flush", x = variable) ~ "Flush",
    grepl(pattern = "DO", x = variable) ~ "DO",
    grepl(pattern = "BRK", x = variable) ~ "Fish",
    grepl(pattern = "CCT", x = variable) ~ "Fish",
    grepl(pattern = "RBT", x = variable) ~ "Fish",
    grepl(pattern = "WCT", x = variable) ~ "Fish",
    grepl(pattern = "TSS", x = variable) ~ "Fish",
    grepl(pattern = "SpCond", x = variable) ~ "Sp Cond",
    grepl(pattern = "SWE", x = variable) ~ "SWE",
    grepl(pattern = "Depth", x = variable) ~ "Physical Characteristics",
    grepl(pattern = "lake", x = variable) ~ "Physical Characteristics",
    grepl(pattern = "Depth", x = variable) ~ "Physical Characteristics",
    grepl(pattern = "ice", x = variable) ~ "Ice Phenology",
    grepl(pattern = "AirTemp", x = variable) ~ "Air Temp",
    grepl(pattern = "BotTemp", x = variable) ~ "Bottom Temp",
    grepl(pattern = "MidTemp", x = variable) ~ "Middle Temp",
    grepl(pattern = "ProfTemp", x = variable) ~ "Profile Temp",
    grepl(pattern = "SurfTemp", x = variable) ~ "Surface Temp",
    grepl(pattern = "pH", x = variable) ~ "pH & Salinity",
    grepl(pattern = "Salinity", x = variable) ~ "pH, Salinity, ANC",
    grepl(pattern = "ANC", x = variable) ~ "pH, Salinity, ANC",
    grepl(pattern = "-N", x = variable) ~ "Nitrogen",
    grepl(pattern = "Total N", x = variable) ~ "Nitrogen",
    grepl(pattern = "Total P", x = variable) ~ "Phosphorus",
    grepl(pattern = "PO4", x = variable) ~ "Phosphorus",
    grepl(pattern = "Ca", x = variable) ~ "Other Ions",
    grepl(pattern = "Cl", x = variable) ~ "Other Ions",
    grepl(pattern = "SO4", x = variable) ~ "Other Ions",
    grepl(pattern = "K", x = variable) ~ "Other Ions",
    grepl(pattern = "Mg", x = variable) ~ "Other Ions",
    grepl(pattern = "Na", x = variable) ~ "Other Ions",
    TRUE ~ "Secchi, Chlorophyll, Pheophytin"))  %>%
  group_by(park_code, site_code, variable) %>%
  mutate(scaled_value = scale(value)) %>%
  ungroup()


bigjoin_figure_list <- map(.x = unique(bigjoin_classify$variable_group),
                           .f = ~ bigjoin_classify %>%
                             filter(variable_group == .x) %>%
                             ggplot(data = ., aes(x = event_year, y = value)) +
                             geom_line(aes(group = Lake, color = Lake)) +
                             scale_x_discrete(limits = c(seq(from = 2007,
                                                             to = 2018,
                                                             by = 1))) +
                             theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
                             facet_grid(variable ~ park_code) +
                             ggtitle(label = paste0("Temporal trends for ",
                                                    .x,
                                                    " variable group")))

names(bigjoin_figure_list) <- paste0(make_clean_names(unique(bigjoin_classify$variable_group)),
                                     "_temporal_plot")
map2(.x = bigjoin_figure_list,
     .y = names(bigjoin_figure_list),
     .f = ~ ggsave(filename = paste0("../figures/temporal_trends/raw_values/", .y, ".png"),
                   plot = .x, device = "png", width = 13, height = 12,
                   units = "in"))

scaled_bigjoin_figure_list <- map(.x = unique(bigjoin_classify$variable_group),
                                  .f = ~ bigjoin_classify %>%
                                    filter(variable_group == .x) %>%
                                    ggplot(data = ., aes(x = event_year, y = scaled_value)) +
                                    geom_line(aes(group = Lake, color = Lake)) +
                                    scale_x_discrete(limits = c(seq(from = 2007,
                                                                    to = 2018,
                                                                    by = 1))) +
                                    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
                                    facet_grid(variable ~ park_code) +
                                    ggtitle(label = paste0("Z-scored temporal trends for ",
                                                           .x,
                                                           " variable group")))

names(scaled_bigjoin_figure_list) <- paste0(make_clean_names(unique(bigjoin_classify$variable_group)),
                                            "_temporal_plot")

map2(.x = scaled_bigjoin_figure_list,
     .y = names(scaled_bigjoin_figure_list),
     .f = ~ ggsave(filename = paste0("../figures/temporal_trends/z_scored/", .y, ".png"),
                   plot = .x, device = "png", width = 13, height = 12,
                   units = "in"))


# 2. Plot zooplankton data ------------------------------------------------


zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")

#  Prep data
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))

# Crosswalk common names for sites to site_codes
zoop <- left_join(x = zoop, y = match_sites,
                  by = c("Lake" = "old_name")) %>%
  clean_names()

# Remove copepodites and nauplii from the dataset per MM suggestion
zoop_orig <- zoop

zoop <- zoop %>%
  filter(!grepl(pattern = "copepodite", x = gen_sp),
         !grepl(pattern = "nauplii", x = gen_sp),
         taxa != "INS", taxa != "HAR")

zoop_sum <- zoop %>%
  group_by(park, lake, year, taxa) %>%
  summarize(total_density = sum(density)) %>%
  group_by(park, lake, taxa) %>%
  mutate(scaled_density = scale(total_density))
#   spread(key = taxa, value = total_density)

zoop_raw_temporal <- zoop_sum %>%
  ggplot(data = ., aes(x = year, y = total_density)) +
  geom_line(aes(group = lake, color = lake)) +
  facet_grid(taxa ~ park) +
  scale_x_discrete(limits = c(seq(from = 2008,
                                  to = 2018,
                                  by = 1))) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggtitle(label = paste0("Temporal trends for zoop taxa"))

ggsave(filename = "../figures/temporal_trends/raw_values/zoop_temporal_plot.png",
       plot = zoop_raw_temporal, device = "png", width = 13, height = 12,
       units = "in")

zoop_scaled_temporal <- zoop_sum %>%
  ggplot(data = ., aes(x = year, y = scaled_density)) +
  geom_line(aes(group = lake, color = lake)) +
  facet_grid(taxa ~ park) +
  scale_x_discrete(limits = c(seq(from = 2009,
                                  to = 2018,
                                  by = 1))) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggtitle(label = paste0("Z-scored temporal trends for zoop taxa"))

ggsave(filename = "../figures/temporal_trends/z_scored/zoop_temporal_plot.png",
       plot = zoop_scaled_temporal, device = "png", width = 13, height = 12,
       units = "in")


# 3. Plot macroinvert data ------------------------------------------------

# Macroinvert data
macro_counts <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "BMI_Counts")

head(macro_counts)

# Lookup table for macro taxonomy
macro_lookup <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "Taxon_Lookup")

head(macro_lookup)

unique_counts <- unique(macro_counts$Taxon)
unique_lookup <- unique(macro_lookup$Taxon)

unique_counts[!(unique_counts %in% unique_lookup)]
unique_lookup[!(unique_lookup %in% unique_counts)]

full_macro <- inner_join(x = macro_counts, y = macro_lookup, by = c("Taxon"))

# summary(full_macro)

full_macro <- full_macro %>%
  filter(!is.na(Count))

macro_sum <- full_macro %>%
  group_by(Park, Site_ID, Year, Order) %>%
  summarize(total_count = sum(Count)) %>%
  group_by(Park, Site_ID, Order) %>%
  mutate(scaled_count = scale(total_count))

# For now doesn't plot the macros because not yet sure how best to break them up
# (i.e., which orders make sense to plot in same figure, like variable_group)

macro_grouped <- macro_sum %>%
  mutate(order_group = case_when(
    grepl(pattern = "_", x = Order) ~ "Incomplete ID",
    Order %in% c("Coleoptera", "Diptera", "Ephemeroptera", "Hemiptera",
                 "Megaloptera", "Odonata", "Plecoptera", "Trichoptera") ~ "Insect",
    TRUE ~ "Non-Insect"))

macro_raw_temporal <- map(.x = unique(macro_grouped$order_group),
                          .f = ~ macro_grouped %>%
                            filter(order_group == .x) %>%
                            ggplot(data = ., aes(x = Year, y = total_count)) +
                            geom_line(aes(group = Site_ID, color = Site_ID)) +
                            facet_grid(Order ~ Park) +
                            scale_x_discrete(limits = c(seq(from = 2008,
                                                            to = 2018,
                                                            by = 1))) +
                            theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
                            ggtitle(label = paste0("Temporal trends for ",
                                                   .x,
                                                   " macroinvert orders")))

names(macro_raw_temporal) <- paste0(make_clean_names(unique(macro_grouped$order_group)),
                                    "_temporal_plot")

map2(.x = macro_raw_temporal,
     .y = names(macro_raw_temporal),
     .f = ~ ggsave(filename = paste0("../figures/temporal_trends/raw_values/macro_", .y, ".png"),
                   plot = .x, device = "png", width = 13, height = 12,
                   units = "in"))


macro_std_temporal <- map(.x = unique(macro_grouped$order_group),
                          .f = ~ macro_grouped %>%
                            filter(order_group == .x) %>%
                            ggplot(data = ., aes(x = Year, y = scaled_count)) +
                            geom_line(aes(group = Site_ID, color = Site_ID)) +
                            facet_grid(Order ~ Park) +
                            scale_x_discrete(limits = c(seq(from = 2008,
                                                            to = 2018,
                                                            by = 1))) +
                            theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
                            ggtitle(label = paste0("Z-scored temporal trends for ",
                                                   .x,
                                                   " macroinvert orders")))


names(macro_std_temporal) <- paste0(make_clean_names(unique(macro_grouped$order_group)),
                                    "_temporal_plot")

map2(.x = macro_std_temporal,
     .y = names(macro_std_temporal),
     .f = ~ ggsave(filename = paste0("../figures/temporal_trends/z_scored/macro_", .y, ".png"),
                   plot = .x, device = "png", width = 13, height = 12,
                   units = "in"))













