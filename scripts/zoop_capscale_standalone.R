library(tidyverse)
library(readxl)
library(vegan)
library(labdsv)
library(visdat)
library(corrplot)
library(psych)
library(ggvegan)
library(janitor)

# 1. Load and prep data ---------------------------------------------------

bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")

#  Prep data. A row needs correcting
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

# Drop genera that are less or equal to than 5% of EVERY sample. So if they ever
# are a large enough proportion of a sample that genera is kept through the
# entire dataset.
zoop_five <- zoop %>%
  # Dropping Code for now b/c unclear what it means. Code != Taxa though.
  # Also dropping taxonid
  group_by(park, lake, site_code, year, genus) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  group_by(genus) %>%
  summarize(max_prop = max(prop_density)) %>%
  filter(max_prop <= 0.05)

zoop_ten <- zoop %>%
  # Dropping Code for now b/c unclear what it means. Code != Taxa though.
  # Also dropping taxonid
  group_by(park, lake, site_code, year, genus) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  group_by(genus) %>%
  summarize(max_prop = max(prop_density)) %>%
  filter(max_prop <= 0.10)
unique(zoop_ten)

# Use that dataset to filter out things that don't match up
zoop_reduced <- anti_join(x = zoop, y = zoop_five, by = c("genus")) %>%
  group_by(park, site_code, year, genus) %>%
  summarise(density = sum(density))

# Add a row identifier column, then turn into matrix
zoop_comm_matrix <- zoop_reduced %>%
  mutate(park_site_year = paste0(park, "_", site_code, "_", year)) %>%
  ungroup() %>%
  select(park_site_year, genus, density) %>%
  data.frame() %>%
  matrify()

zoops_comm_ids <- rownames_to_column(zoop_comm_matrix) %>%
  separate(col = rowname, into = c("park", "site", "year"), sep = "_")

# Make genera with 0s have small random numbers added. Make year col numeric
zoops_comm_ids <- zoops_comm_ids %>%
  rowwise() %>%
  mutate_if(.predicate = is_numeric,
            .funs = ~ ifelse(test = . == 0.000000,
                             yes = runif(1, min = 0, max = 0.000001),
                             no = .)) %>%
  mutate(year = as.numeric(year))


# Take a subset of bigjoin
yearly_bigjoin <- bigjoin %>%
  filter(variable %in% c("secchi_value_m", "AirTemp", "SurfTemp", "DOC",
                         "Chlorophyll", "ice_out_doy", "Total N", "Total P")) %>%
  select(park_code, site_code, event_year, variable, value) %>%
  unique() %>%
  spread(key = variable, value = value) %>%
  mutate(n_p_ratio = (`Total N` / 14) / (`Total P` / 31))

# Add elevation to yearly_bigjoin. Don't spread to do this, because that results
# in empty rows for years where no data except elevation exists
bigjoin_elev <- bigjoin %>%
  select(park_code, site_code, Elevation_m) %>%
  unique()

yearly_bigjoin <- left_join(x = yearly_bigjoin, y = bigjoin_elev,
                            by = c("park_code", "site_code"))

#rename columns
names(yearly_bigjoin)[names(yearly_bigjoin) == "Total P"] <- "total_P"
names(yearly_bigjoin)[names(yearly_bigjoin) == "Total N"] <- "total_N"
head(yearly_bigjoin)

# Join the zoops to yearly_bigjoin bc some data will be NA: both datasets
# should have corresponding rows
yearly_bigjoin_zoop <- inner_join(x = zoops_comm_ids, y = yearly_bigjoin,
                                  by = c("park" = "park_code",
                                         "site" = "site_code",
                                         "year" = "event_year"))

# Inspect the dataset a little bit to get a sense of how much is NAs
vis_dat(x = yearly_bigjoin_zoop)


# 2. Capscale -------------------------------------------------------------


# 2a. Full capscale -------------------------------------------------------

# Now look at env variable correlations, but first check for and remove NAs
# Down some rows after removing NAs...
yearly_bigjoin_zoop <- na.omit(yearly_bigjoin_zoop)

corrplot(cor(select(yearly_bigjoin_zoop, AirTemp:SurfTemp, n_p_ratio, Elevation_m)),
         type = "upper", tl.col = "black", tl.srt = 45)

# How about keep SurfTemp, drop other temps?
corrplot(cor(select(yearly_bigjoin_zoop, SurfTemp, DO_top2m,
                    secchi_value_m, ice_out_doy)),
         type = "upper", tl.col = "black", tl.srt = 45)

corr.test(x = select(yearly_bigjoin_zoop, SurfTemp, DO_top2m, secchi_value_m,
                     Chlorophyll, ice_out_doy))


zoop_cap <- capscale(formula = select(yearly_bigjoin_zoop, Ascomorpha:Trichotria) ~
                       SurfTemp + Chlorophyll + ice_out_doy + Elevation_m + DOC,
                     data = yearly_bigjoin_zoop, distance = "bray")

# Capscale results
zoop_cap

# Greater detail
#summary(zoop_cap)

# Plot it:
# Have to combine the site scores with the original dataset so that it's plot-able
custom_site_points <- bind_cols(data.frame(scores(zoop_cap)$sites),
                                yearly_bigjoin_zoop)

autoplot(zoop_cap, layers = c("biplot", "species")) +
  geom_point(data = custom_site_points,
             aes(x = CAP1, y = CAP2, color = park, size = Elevation_m))

RsquareAdj(zoop_cap)

# https://sites.ualberta.ca/~ahamann/teaching/renr690/Lab9b.pdf
anova(zoop_cap)
# anova(zoop_dbrda, by = "axis", perm.max = 999) # Won't run bc negative eigenvalues
anova(zoop_cap, by = "terms", perm.max = 999)

#zoops<-select(zoops_comm_ids, Ascomorpha:Trichotria)
#head(zoops)
#PERMANOVA
#zoop_perm <- adonis(zoops ~ park * year, data=zoops_comm_ids)
#zoop_perm

# 2b. Crustaceans ---------------------------------------------------------

zoop_reduced_crust <- anti_join(x = zoop, y = zoop_five, by = c("genus")) %>%
  filter(taxa %in% c("CLAD", "COPE")) %>%
  group_by(park, site_code, year, genus) %>%
  summarise(density = sum(density))

# Add a row identifier column, split by park, then turn into matrix
zoop_comm_matrix_crust <- zoop_reduced_crust %>%
  mutate(park_site_year = paste0(park, "_", site_code, "_", year)) %>%
  ungroup() %>%
  select(park, park_site_year, genus, density) %>%
  data.frame() %>%
  split(x = ., f = .$park) %>%
  map(.x = (.),
      .f = ~ .x %>%
        select(-park) %>%
        matrify(.))

zoops_comm_ids_crust <- map(.x = zoop_comm_matrix_crust,
                            .f = ~ rownames_to_column(.x) %>%
                              separate(col = rowname,
                                       into = c("park", "site", "year"),
                                       sep = "_") %>%
                              mutate(year = as.numeric(year)))

# Join the zoops to yearly_bigjoin bc some data will be NA: both datasets
# should have corresponding rows
yearly_bigjoin_zoop_crust <- map(.x = zoops_comm_ids_crust,
                                 .f = ~ inner_join(x = .x,
                                                   y = yearly_bigjoin,
                                                   by = c("park" = "park_code",
                                                          "site" = "site_code",
                                                          "year" = "event_year")))

# Inspect the dataset a little bit to get a sense of how much is NAs
map(.x = yearly_bigjoin_zoop_crust,
    .f = ~ vis_dat(x = .x))

yearly_bigjoin_zoop_crust <- map(.x = yearly_bigjoin_zoop_crust,
                                 .f = ~ na.omit(.x))

map(.x = yearly_bigjoin_zoop_crust,
    .f = ~ corrplot(cor(select(.x, SurfTemp,
                               secchi_value_m, ice_out_doy)),
                    type = "upper", tl.col = "black", tl.srt = 45))

map(.x = yearly_bigjoin_zoop_crust,
    .f = ~ corr.test(x = select(.x, SurfTemp, secchi_value_m,
                                Chlorophyll, ice_out_doy)))

zoop_cap_crust <- map(.x = yearly_bigjoin_zoop_crust,
                      .f = ~ capscale(formula = select(.x,
                                                       Bosmina:Microcyclops) ~
                                        SurfTemp + Chlorophyll + ice_out_doy + total_P + 
                                        Elevation_m,
                                      data = .x, distance = "bray"))

# Capscale results
zoop_cap_crust

# Greater detail
#summary(zoop_cap_crust)

# Plot it
# This doesn't work and I don't understand why:
# map(.x = zoop_cap_crust,
#     .y = yearly_bigjoin_zoop_crust,
#     .f = ~ autoplot(.x) +
#       geom_point(data = bind_cols(data.frame(scores(.x)$sites),
#                                   .y),
#                  aes(x = CAP1, y = CAP2, size = Elevation_m)))

autoplot(zoop_cap_crust[[1]], layers = c("biplot", "species")) +
  geom_point(data = bind_cols(data.frame(scores(zoop_cap_crust[[1]])$sites),
                              yearly_bigjoin_zoop_crust[[1]]),
             aes(x = CAP1, y = CAP2, size = Elevation_m))

autoplot(zoop_cap_crust[[2]], layers = c("biplot", "species")) +
  geom_point(data = bind_cols(data.frame(scores(zoop_cap_crust[[2]])$sites),
                              yearly_bigjoin_zoop_crust[[2]]),
             aes(x = CAP1, y = CAP2, size = Elevation_m))

autoplot(zoop_cap_crust[[3]], layers = c("biplot", "species")) +
  geom_point(data = bind_cols(data.frame(scores(zoop_cap_crust[[3]])$sites),
                              yearly_bigjoin_zoop_crust[[3]]),
             aes(x = CAP1, y = CAP2, size = Elevation_m))


# R squared
map(.x = zoop_cap_crust,
    .f = ~ RsquareAdj(.x))

# https://sites.ualberta.ca/~ahamann/teaching/renr690/Lab9b.pdf
map(.x = zoop_cap_crust,
    .f = ~ anova(.x))

# anova(zoop_dbrda, by = "axis", perm.max = 999) # Won't run bc negative eigenvalues
map(.x = zoop_cap_crust,
    .f = ~ anova(.x, by = "terms", perm.max = 999))

