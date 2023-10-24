library(tidyverse)
library(readxl)
library(janitor)
library(visdat)


# Load data
bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

zoop <- read_excel("../data/NCCN_Zoops_Combined_For_FCA_May_2019.xlsx")


# Create basic zoop dataset for MM ----------------------------------------

# Recreate the dataset SH made (external to R) for building regression trees
# modeling zooplankton

# SH's dataset
sh_df <- read.csv(file = "../data/bigjoin_taxon_varying_variables_wide.csv",
                  stringsAsFactors = FALSE)

head(sh_df)

names(bigjoin)

# sh_df names in bigjoin names:
names(sh_df)[names(sh_df) %in% names(bigjoin)]

# "event_year"          "SWE_May_snotel"      "ice_out_doy"         "flush_index_noSWE"  
# "flush_index_SWE_May" "WRT_index_SWE_May"   "SWE_May"             "SWE_Apr"            
# "ice_in_doy"          "year"  


# sh_df names in bigjoin variable column:
names(sh_df)[names(sh_df) %in% unique(bigjoin$variable)]
# Many came from this column

# Filter bigjoin for relevant vars
bigjoin_sh <- bigjoin %>%
  filter(variable %in% c(names(sh_df),
                         # Some vars whose names don't transfer from cols well
                         "NH3-N", "NH4-N", "NO3-N", "NO3-N + NO2-N", "Total N",
                         "Total P"))

# Are the variables from the variable column redundant with any columns in bigjoin?
unique(bigjoin_sh$variable)[unique(bigjoin_sh$variable) %in% names(bigjoin)]
# Yes:
# [1] "SWE_May"             "SWE_Apr"             "SWE_May_snotel"      "ice_out_doy"        
# [5] "ice_in_doy"          "flush_index_noSWE"   "flush_index_SWE_May" "WRT_index_SWE_May" 


# Before pivoting to wider version we need to figure out which vars have multiple
# values per park*site*year, because not all = 1
map(.x = unique(bigjoin_sh$variable),
    .f = ~  bigjoin_sh %>%
      select(park_code, site_code, event_year, variable, value) %>%
      filter(variable == .x) %>% 
      count(park_code, site_code, event_year, variable) %>%
      select(variable,  n) %>%
      unique())

# lakelevelcm, ice_out_doy 

# lakelevelcm is because of multiple measures in different locations
bigjoin_sh %>%
  filter(variable == "lakelevelcm",  site_code == "LH14") %>%
  select(site_code, variable, event_year, event_month, value) %>%
  arrange(site_code, event_year) %>%
  unique()

# ice_out_doy appears to be an error originating on Oct 6, 2019 in bigjoin
bigjoin_sh %>%
  filter(variable == "ice_out_doy",  site_code == "LH14") %>%
  arrange(site_code, event_year) %>%
  select(site_code, variable, event_year, event_month, value) %>%
  unique()

# Take the vars we need
bigjoin_sh_drop <- bigjoin_sh %>%
  select(park_code, site_code, event_year, variable, value) %>%
  unique() %>%
  # values_fn = mean because of duplicate rows in lakelevel, ice_out_doy
  pivot_wider(names_from = variable, values_from =  value, values_fn = mean) %>%
  clean_names() %>%
  rename(ph_below2m = p_h_below2m,
         ph_top2m = p_h_top2m,
         phmv_below2m = p_hm_v_below2m,
         phmv_top2m = p_hm_v_top2m)

# Now match up zoops to this dataset
bigjoin_sh_zoop <- 
  # inner_join(x = bigjoin_sh_drop, y = zoops_comm_ids,
  #                             by = c("park_code" = "park",
  #                                    "site_code" = "site",
  #                                    "event_year" = "year")) %>%
  bigjoin_sh_drop %>%
  clean_names() %>%
  mutate(ln_clad = log(clad),
         ln_cope = log(cope),
         ln_rot = log(rot)) %>%
  group_by(site_code) %>%
  # These values don't match SH's. I'm not sure what she did for hers
  mutate(ln_cent_clad = log(scale(clad, center = TRUE, scale = FALSE)),
         ln_cent_cope = log(scale(cope, center = TRUE, scale = FALSE)),
         ln_cent_rot = log(scale(rot, center = TRUE, scale = FALSE)))

# Compare: 
bigjoin_sh_zoop %>%
  select(site_code, event_year, ln_clad, ln_cent_clad) %>%
  arrange(site_code) %>%
  head()

sh_df %>%
  select(site_code.of.bigjoin_wide_varying_variables, event_year,
         log.clad.,
         log.clad..centered.by.site_code.of.bigjoin_wide_varying_variables) %>%
  unique() %>%
  arrange(site_code.of.bigjoin_wide_varying_variables) %>%
  head() 


# What cols are missing?
names(clean_names(sh_df))[!(names(clean_names(sh_df)) %in% names(bigjoin_sh_zoop))]
names(bigjoin_sh_zoop)[!(names(bigjoin_sh_zoop) %in% names(clean_names(sh_df)))]

# I'm not sure that it's important to match the response variables? MM didn't ask
# for them, just the explanatory ones. Maybe don't worry for now.


# I think that we're good on columns included now:

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[1:10]
sort(names(bigjoin_sh_zoop))[1:10]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[11:20]
sort(names(bigjoin_sh_zoop))[11:20]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[21:30]
sort(names(bigjoin_sh_zoop))[21:30]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[31:40]
sort(names(bigjoin_sh_zoop))[31:40]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[41:50]
sort(names(bigjoin_sh_zoop))[41:50]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[51:60]
sort(names(bigjoin_sh_zoop))[51:60]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[61:70]
sort(names(bigjoin_sh_zoop))[61:70]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[71:80]
sort(names(bigjoin_sh_zoop))[71:80]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[81:90]
sort(names(bigjoin_sh_zoop))[81:90]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[91:100]
sort(names(bigjoin_sh_zoop))[91:100]

sort(names(clean_names(sh_df) %>%
             select(-c(label, year, site_code_of_zoop_taxon_wide))))[101:108]
sort(names(bigjoin_sh_zoop))[101:107]



names(sh_df)
names(bigjoin_sh_zoop)
bind_cols(sh = sh_df %>% clean_names() %>%
            select(-c(label, year, site_code_of_zoop_taxon_wide)) %>%
            names() %>%
            sort(),
          mrb = bigjoin_sh_zoop %>%
            names() %>%
            sort()) %>%
  mutate(test = if_else(condition = .[1] == .[2],
                        true = "", false = "NO MATCH")) %>%
  View

# Test datasets for equality
sh_df_clean <- sh_df %>%
  clean_names() %>%
  select(-c(label, year, site_code_of_zoop_taxon_wide,
            contains("centered"))) %>%
  rename(ln_clad = log_clad,
         ln_cope = log_copepod,
         ln_rot = log_rotifer,
         park_code = park,
         site_code = site_code_of_bigjoin_wide_varying_variables,
         ph_below2m = p_h_below2m,
         ph_top2m = p_h_top2m,
         phmv_below2m = p_hm_v_below2m,
         phmv_top2m = p_hm_v_top2m)

bigjoin_sh_zoop <- bigjoin_sh_zoop %>%
  select(-contains("cent"))

all_equal(
  sh_df_clean,
  bigjoin_sh_zoop,
  ignore_col_order = T, ignore_row_order = T, convert = T)

# Difference of 3 rows
# Also noticed in exploring this that the park/park_code column in sh_df
# has a lot of incomplete data. Must have been taken from zoops in the join

# O rows:
anti_join(x = bigjoin %>%
            select(site_code, event_year) %>%
            unique(),
          y = sh_df_clean %>%
            select(site_code, event_year) %>%
            unique())

# Row mismatches come from sites 426, 520, MC-03-01
bind_cols(
  sh_df_clean %>%
    count(site_code),
  
  bigjoin_sh_zoop %>%
    data.frame() %>%
    count(site_code)
) %>%
  mutate(.[[2]] - .[[4]])

filter(bigjoin_sh_zoop, site_code %in% c("426", "520", "MC-03-01")) %>%
  select(site_code, event_year) %>%
  unique()

filter(sh_df_clean, site_code %in% c("426", "520", "MC-03-01")) %>%
  select(site_code, event_year) %>%
  unique()

# Seems to trace back to the error mentioned above from Oct 6, 2019 in bigjoin
bigjoin %>%
  filter(site_code == "426", event_year == 2009) %>%
  select(park_code, site_code, event_year, variable)

# For now, take care of it by removing NA rows for park_code. I think this will
# do it safely. Asked SP to review the Oct 06 change

bigjoin_sh_zoop <- bigjoin_sh_zoop %>%
  filter(!is.na(park_code))

# Still not equivalent:
all_equal(
  sh_df_clean %>% select(-park_code),
  bigjoin_sh_zoop %>% select(-park_code),
  ignore_col_order = T, ignore_row_order = T, convert = T)

# Let's figure out why
full_join(x = bigjoin_sh_zoop %>% select(-park_code),
          y = sh_df_clean %>% select(-park_code),
          by = c("site_code", "event_year"),
          suffix = c("mb", "sh")) %>%
  select(site_code, event_year, sort(colnames(.))) %>%
  View()

mb_sh_compare <- compareDF::compare_df(sh_df_clean %>%
                                         select(-park_code),
                                       bigjoin_sh_zoop %>%
                                         select(-park_code),
                                       group_col = c("site_code", "event_year"),
                                       tolerance = 0.01)

compareDF::create_output_table(mb_sh_compare)

# Findings: 
# - lakelevelcm measurements differ between between our datasets
#     - SH's values match one of the two for each year, so she must have
#       selected one of the two values per year and dropped the second.
#       Mine are averaged
# - zoop densities differ too, not hugely, but maybe around 6%?

# Ok, we can export this first version

write.csv(x = bigjoin_sh_zoop,
          file = "../data/analysis_outputs/zoop_tree_data_taxa_w_env.csv",
          row.names = FALSE)

# Next versions to  make:

# 1. Same as bigjoin_sh_zoop but with cols for cal & cyc replacing COPE
# 2. Same as bigjoin_sh_zoop but replace all taxa cols with cols for genera, but
#    only those >= 2% of a lake's genera historically


# Create dataset with columns for genera, 2% cutoff -----------------------

# Note that these zoop data are going to be processed differently than those
# from the dataset in the previous section, which was taken from bigjoin.
# Worth just confirming that they agree once analyses start getting in-depth.

#  Prep zoop data
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

# Drop genera that don't have a lake-level average of 2% over the time series
# Note that dropping on a lake-by-lake basis, not across the whole study.
prop <- 0.0025

zoop_min <- zoop %>%
  group_by(park, lake, site_code, year, genus) %>%
  summarise(density = sum(density)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_density = sum(density, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_density = density / sum_density) %>%
  group_by(lake, genus) %>%
  summarize(mean_prop = mean(prop_density)) %>%
  filter(mean_prop < prop) %>%
  ungroup()

# Use that dataset to filter out things that don't match up
zoop_reduced <- anti_join(x = zoop, y = zoop_min, by = c("lake", "genus")) %>%
  select(park, lake, site_code, year, density, genus, species, taxa) %>%
  group_by(park, lake, site_code, year, genus) %>%
  summarise(density = sum(density))

zoop_wide <- zoop_reduced %>%
  pivot_wider(names_from = genus, values_from = density, values_fill = 0) %>%
  ungroup() %>%
  select(-park)

# Join genera counts to SH-selected bigjoin env vars
bigjoin_genus <- left_join(x = bigjoin_sh_zoop %>%
                             select(!contains(c("clad", "cope", "rot"))),
                           y = zoop_wide,
                           by = c("site_code", "event_year" = "year"))

write.csv(x = bigjoin_genus,
          file = "../data/analysis_outputs/zoop_tree_data_genera_w_env.csv",
          row.names = FALSE)


# Create dataset with cal & cyc distinctions ------------------------------

mm_classify <- read.csv(file = "../data/zoops_unique_gensp.csv",
                        stringsAsFactors = FALSE)

zoop_classify <- zoop_orig %>%
  filter(!grepl(pattern = "copepodite", x = gen_sp),
         # !grepl(pattern = "nauplii", x = gen_sp),
         taxa != "INS", taxa != "HAR",
         gen_sp != "No zooplankton detected",
         gen_sp != "immature Daphnia") %>%
  left_join(x = ., y = mm_classify, by = c("gen_sp" = "GenSp")) %>%
  mutate(Type = case_when(
    Type == "cylcopoid" ~ "cyclopoid",
    Type == "cyclopod" ~ "cyclopoid",
    (gen_sp == "nauplii") ~ "nauplii",
    TRUE ~ Type
  )) %>%
  # Remove remaining data that aren't in one of MM's classifications
  filter(!is.na(Type), Type != "")

zoop_classify_wide <- zoop_classify %>%
  select(-park_year_lake, -park) %>%
  group_by(lake, site_code, year, Type) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>%
  pivot_wider(names_from = Type, values_from = density, values_fill = 0)

bigjoin_classify <- inner_join(x = bigjoin_sh_zoop %>%
                                 select(!contains(c("clad", "cope", "rot"))),
                               y = zoop_classify_wide,
                               by = c("site_code", "event_year" = "year")) %>%
  ungroup()

write.csv(x = bigjoin_classify,
          file = "../data/analysis_outputs/zoop_tree_data_calcyc_w_env.csv",
          row.names = FALSE)


# Create macro dataset at class level -------------------------------------

# Name standardization table
match_sites <- read_rds(file = "../data/name_site_matches.rds")

# Macros
macro_counts <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "BMI_Counts") %>%
  left_join(x = ., y = match_sites, by = c("Site_ID" = "old_name")) %>%
  mutate(site_code = case_when(
    is.na(site_code) ~ Site_ID,
    !is.na(site_code) ~ site_code))

head(macro_counts)

# Lookup table for macro taxonomy
macro_lookup <- read_excel(path = "../data/NCCN_ML_BMI_Counts_FCA_2019JUL09.xlsx",
                           sheet = "Taxon_Lookup") %>%
  mutate(Class = if_else(condition = Class == "Malocostraca",
                         "Malacostraca", Class))

head(macro_lookup)

full_macro <- inner_join(x = macro_counts, y = macro_lookup, by = c("Taxon"))

full_macro <- full_macro %>%
  filter(!is.na(Count)) %>%
  clean_names()

# Drop classes that don't have a lake-level average of 2% over the time series
# Note that dropping on a lake-by-lake basis, not across the whole study.
prop <- 0.0025

macro_min <- full_macro %>%
  group_by(park, site_code, year, class) %>%
  summarise(count = sum(count)) %>%
  group_by(park, site_code, year) %>%
  mutate(sum_count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_count = count / sum_count) %>%
  group_by(site_code, class) %>%
  summarize(mean_prop = mean(prop_count)) %>%
  filter(mean_prop < prop) %>%
  ungroup()

# Use that dataset to filter out things that don't match up
macro_reduced <- anti_join(x = full_macro, y = macro_min,
                           by = c("site_code", "class")) %>%
  select(park, site_code, year, count, class) %>%
  filter(!is.na(class)) %>%
  group_by(park, site_code, year, class) %>%
  summarise(count = sum(count))

macro_wide <- macro_reduced %>%
  pivot_wider(names_from = class, values_from = count, values_fill = 0) %>%
  ungroup() %>%
  select(-park)

# Join genera counts to SH-selected bigjoin env vars
bigjoin_class <- left_join(x = bigjoin_sh_zoop %>%
                             select(!contains(c("clad", "cope", "rot"))),
                           y = macro_wide,
                           by = c("site_code", "event_year" = "year"))

# Filter out places and times where macro data weren't collected
bigjoin_class_filter <- bigjoin_class %>%
  semi_join(x = .,
            y = macro_counts,
            by = c("park_code" = "Park",
                   "site_code",
                   "event_year" = "Year")) %>%
  # Make a column for all macros summed
  mutate(all_macros = rowSums(across(c(Arachnida:Trepaxonemata))))

vis_dat(bigjoin_class_filter)

# Export
write.csv(x = bigjoin_class_filter,
          file = "../data/analysis_outputs/macro_tree_data_class_w_env.csv",
          row.names = FALSE)


# Create macro dataset by feeding and mobility groups ---------------------


# Feeding guild:

feeding_lookup <- read_csv(file = "../data/nps_macro_feeding_guilds.csv")

head(feeding_lookup)

feeding_macro <- inner_join(x = macro_counts, y = feeding_lookup, by = c("Taxon"))

feeding_macro <- feeding_macro %>%
  filter(!is.na(Count)) %>%
  clean_names()


# Create cols for each feeding guild
feeding_macro_wide <- feeding_macro %>%
  select(park, site_code, site_code, year, count, feeding_guild) %>%
  # filter(!is.na(class)) %>%
  group_by(park, site_code, year, feeding_guild) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = feeding_guild, values_from = count, values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  rename(unlabeled = na) %>%
  select(-park)

# Summary counts:
feeding_macro_wide %>%
  summarise(across(.cols = filter_feeder:shredder, .f = sum))

# Join genera counts to SH-selected bigjoin env vars
feeding_bigjoin <- left_join(x = bigjoin_sh_zoop %>%
                               select(!contains(c("clad", "cope", "rot"))),
                             y = feeding_macro_wide,
                             by = c("site_code", "event_year" = "year"))

vis_dat(feeding_bigjoin)

# Filter out places and times where macro data weren't collected
feeding_bigjoin_filter <- feeding_bigjoin %>%
  semi_join(x = .,
            y = macro_counts,
            by = c("park_code" = "Park",
                   "site_code",
                   "event_year" = "Year"))

vis_dat(feeding_bigjoin_filter)

# Export
write.csv(x = feeding_bigjoin_filter,
          file = "../data/analysis_outputs/macro_tree_data_feeding_w_env.csv",
          row.names = FALSE)


# Mobility:

mobility_lookup <- read_csv(file = "../data/nps_macro_mobility.csv")

head(mobility_lookup)

clean_mobility_lookup <- mobility_lookup %>%
  rename(tax_group = taxon) %>%
  mutate(
    # Where to find it in the taxon lookup table
    match_taxon = case_when(
      
      tax_group == "Acari" ~ "taxon",
      tax_group == "Ntoda" ~ "taxa_code",
      tax_group == "Ntpoda" ~ "order",
      tax_group == "Oligochaeta" ~ "taxon",
      tax_group == "Ostracods" ~ "taxon",
      tax_group == "Platyhelminths" ~ "phylum",
      tax_group == "Amphipods" ~ "order",
      tax_group == "Coleoptera" ~ "order",
      tax_group == "Diplostraca" ~ "order",
      tax_group == "Diptera" ~ "order",
      tax_group == "Ephemeroptera" ~ "order",
      tax_group == "Megaloptera" ~ "order",
      tax_group == "Odonata" ~ "order",
      tax_group == "Trichoptera" ~ "order",
      tax_group == "Veneroida" ~ "order",
      tax_group == "Gastropoda" ~ "class"
    ),
    
    # What it's called in the tax_group lookup table
    name_fix = case_when(
      
      tax_group == "Ntpoda" ~ "_Ntopda",
      tax_group == "Ostracods" ~ "Ostracoda",
      tax_group == "Platyhelminths" ~ "Platyhelminthes",
      tax_group == "Amphipods" ~ "Amphipoda",
      TRUE ~ tax_group
      
    ))


mobility_macro <- full_join(x = full_macro,
                            y = clean_mobility_lookup,
                            by = character()) %>%
  filter_at(vars(eval(.$match_taxon)),
            any_vars(. == name_fix)) %>%
  filter(!is.na(count))

# Create cols for each mobility group
mobility_macro_wide <- mobility_macro %>%
  select(park, site_code, site_code, year, count, mobility) %>%
  # filter(!is.na(class)) %>%
  group_by(park, site_code, year, mobility) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = mobility, values_from = count, values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  # rename(unlabeled = na) %>%
  select(-park)

# Summary counts:
mobility_macro_wide %>%
  summarise(across(.cols = -c(site_code, year), .f = sum))

# Join genera counts to SH-selected bigjoin env vars
mobility_bigjoin <- left_join(x = bigjoin_sh_zoop %>%
                                select(!contains(c("clad", "cope", "rot"))),
                              y = mobility_macro_wide,
                              by = c("site_code", "event_year" = "year"))

vis_dat(mobility_bigjoin)

# Filter out places and times where macro data weren't collected
mobility_bigjoin_filter <- mobility_bigjoin %>%
  semi_join(x = .,
            y = macro_counts,
            by = c("park_code" = "Park",
                   "site_code",
                   "event_year" = "Year"))

vis_dat(mobility_bigjoin_filter)

# Export
write.csv(x = mobility_bigjoin_filter,
          file = "../data/analysis_outputs/macro_tree_data_mobility_w_env.csv",
          row.names = FALSE)
