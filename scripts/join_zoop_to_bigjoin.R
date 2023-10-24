library(tidyverse)
library(readxl)
library(janitor)


# 1. Load datasets --------------------------------------------------------

bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")

# Name matching table
match_sites <- readRDS(file = file.path("..",
                                        "data",
                                        "name_site_matches.rds"))

genus_biomass <- read.csv(file = "../data/analysis_outputs/zoop_dry_weight_estimates.csv",
                          stringsAsFactors = FALSE)


# 2. Prep/clean -----------------------------------------------------------

#  Prep zoop data
zoop[1487, "Taxa"] <- "ROT"
zoop[1487, "Taxonid"] <- 5

# Crosswalk common names for sites to consistent site_codes
zoop <- left_join(x = zoop, y = match_sites,
                  by = c("Lake" = "old_name")) %>%
  clean_names()

# Remove copepodites and nauplii from the dataset per MM suggestion
zoop <- zoop %>%
  filter(!grepl(pattern = "copepodite", x = gen_sp),
         !grepl(pattern = "nauplii", x = gen_sp),
         taxa != "INS", taxa != "HAR",
         gen_sp != "No zooplankton detected",
         gen_sp != "immature Daphnia")

# Remove things that don't have genus level ID. Then create genus column
zoop <- zoop %>%
  filter(!(gen_sp %in% c("small sp.", "Chaoborus", "mosquito pupae",
                         "harpacticoid", "chydorid"))) %>%
  separate(col = gen_sp, into = c("genus", "species"), sep = " ") %>%
  mutate(genus = if_else(condition = genus == "Conochilius",
                         true = "Conochilus", false = genus))

zoop_five <- zoop %>%
  # Dropping Code for now b/c unclear what it means. Code != Taxa though.
  # Also dropping taxonid
  group_by(park, lake, site_code, year, genus) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  arrange(prop_density) %>%
  filter(prop_density <= 0.05)

# Use that dataset to filter out things that don't match up and sum to genus
zoop_reduced <- anti_join(x = zoop, y = zoop_five,
                          by = c("park","site_code", "year", "genus")) %>%
  group_by(park, site_code, year, genus) %>%
  summarise(density = sum(density))


# 3. Start joining datasets -----------------------------------------------

# Join the zoop dataset (with <=5% samples removed & summarized to genus) with
# SH biomass data
zoop_with_biomass <- left_join(x = zoop_reduced, y = genus_biomass,
                               by = c("genus" = "Genus")) %>%
  mutate(total_biomass_mg = (density * ug.dry.weight.ind) / 1000)

# Which zoop genera from NPS don't have biomass data?
zoop_without_biomass <- anti_join(x = zoop_reduced, y = genus_biomass,
                                  by = c("genus" = "Genus")) %>%
  pull(genus) %>%
  unique()

zoop_without_biomass

# Which biomass data doesn't have an application with NPS zoops?
biomass_without_match <- anti_join(x = genus_biomass, y = zoop_reduced,
                                   by = c("Genus" = "genus")) %>%
  pull(Genus) %>%
  unique()

biomass_without_match

# Join biomass level data with bigjoin

biomass_bigjoin <- inner_join(x = zoop_with_biomass, y = bigjoin,
                              by = c("park" = "park_code",
                                     "site_code",
                                     "year" = "event_year"))

