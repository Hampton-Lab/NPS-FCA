# This script calculates Sen's slopes for each of the variables contained
# within the bigjoin$variable column (created by bigjoin.R). All are calculated
# simultaneously using purrr. A couple are excluded because of incomplete data.
# The calculations are currently done (2020-01-21) by calculating the sen's slope
# estimation for each lake and variable, then taking the mean of the slopes
# by variable and park.

# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(trend) # One package with Sens
library(openair) # The package SH has published Sens with previously
library(janitor)
library(lubridate)
library(ggrepel)
library(ggpubr)

# openair manual: http://www.openair-project.org/PDF/OpenAir_Manual.pdf


# 2. Read in revelant data files ------------------------------------------

# Load the data
bigjoin <- readRDS(file = "../data/analysis_outputs/bigjoin.rds")

# Z-score each variable for easier comparison
bigjoin_filter <- bigjoin %>%
  group_by(park_code, site_code, variable) %>%
  mutate(value = scale(value)) %>%
  # Remove NA rows
  filter(!is.na(value)) %>%
  ungroup()

# A quick sidebar: Create and export a plot showing the number of rows of data
# present for each park*site*variable combo after filtering out NAs
post_na_rowcount <- bigjoin_filter %>%
  count(park_code, site_code, variable) %>%
  ggplot() +
  geom_point(aes(y = variable, x = site_code, color = park_code, size = n),
             position = position_jitter(width = 0, height = 0.1)) +
  geom_text(aes(x = site_code, y = variable, label = n)) + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

ggsave(filename = "../figures/sen_narm_bigjoin_rowcount.png",
       plot = post_na_rowcount,
       device = "png", width = 10, height = 16, units = "in")

# Create list of 89 dataframes...each is one bigjoin variable
split_vars <- bigjoin_filter %>%
  select(park_code:Lake, units, units_label) %>%
  # Remove some vars bc of data issues
  filter(!grepl(pattern = "salinity", x = .$variable, ignore.case = TRUE),
         variable != "TSS", variable != "TSS_ever",
         variable != "WCT", variable != "WCT_ever",
         variable != "CCT", variable != "CCT_ever",
         variable != "BRK", variable != "BRK_ever",
         variable != "RBT", variable != "RBT_ever",
         variable != "lakelevelcm",
         variable != "flush_index_noSWE") %>%
  # Standardize the ice units..they have multiple values for ice_out_doy
  mutate(units = case_when(variable == "ice_out_doy" ~ "day of year",
                           variable == "ice_in_doy" ~ "day of year",
                           TRUE ~ units)) %>%
  # I use base::split bc it names list items. unique() to remove duplicated
  # rows
  split(x = ., list(.$variable)) %>%
  map(.x = .,
      .f = ~ unique(.x) %>%
        as_tibble())

# List, 88 vars long with each var having a sublist of 3 parks' data
split_parks <- map(.x = split_vars, .f = ~ .x %>%
                     split(x = ., list(.$park_code)))

# Spread each dataframe at the variable*park level to wide version, split it
# by lake, then calculate Sen's slope.
# safely() to prevent erroring out if some vars have data issues
safe_sen <- safely(.f = ~ spread(data = .x,
                                 key = "variable",
                                 value = "value") %>%
                     clean_names() %>%
                     split(x = ., list(.$site_code)) %>%
                     map(.x = .,
                         .f = ~ .x %>%
                           mutate(date = ymd(event_year,
                                             truncated = 2L,
                                             tz = "UTC")) %>%
                           # Confirm this is the setting we want to use
                           TheilSen(mydata = .,
                                    pollutant = names(.[, 9]),
                                    plot = FALSE,
                                    avg.time = "year")))

# Apply the above method at the variable*park*lake level.
safe_parks <- map(.x = split_parks,
                  .f = ~ map(.x = .,
                             .f = ~ safe_sen(.x)))

# Output is nested lists: variable > park > result/error > lake (openair object).
# Isolate only the successful slope data:
safe_results <- safe_parks %>%
  map(.x = .,
      .f = ~ .x %>%
        transpose() %>%
        pluck("result") %>%
        # This doesn't appear to be working for some things (e.g. NO3) but does
        # work for others (e.g. PO4)
        discard(.x = .,
                .p = function(x) all(is.null(x))))

# Function to filter out rows that are NA for p-value stars.Sometimes TheilSen
# inserts NA rows and this column is one way to detect it. 
# Using safely() because some variables don't run successfully on some lakes
# & thus don't have a data frame that can be filtered.
safe_filter <- safely(.f = ~ map(.x = .x,
                                 .f = ~ map_df(.x = .x,
                                               .f = ~ .x$data$res2 %>%
                                                 filter(!is.na(p.stars)))))

# Apply the above function. Basic output is nested lists:
# variable > result/error > park > data frame of one slope per lake. From this I
# isolate only the successfully filtered data and then remove null vals.
safe_sens_results <- map(.x = safe_results,
                         .f = ~ safe_filter(.x)) %>%
  transpose() %>%
  pluck("result") %>%
  compact()

# Add park name col to each variable*park level data frame & collapse into one
# data frame per variable. Then, add variable name col to each variable level
# data frame and collapse list into a single data frame.
sens_by_var <- map(.x = safe_sens_results,
                   .f = ~ map2_df(.x = .x,
                                  .y = names(.x),
                                  .f = ~ .x %>% mutate(park = .y))) %>%
  map2_df(.x = .,
          .y = names(.),
          .f = ~ .x %>% mutate(variable_name = .y))

# Lots of variables...Create some rough categories here to to facilitate viz.
# Next calculate mean, sd of slope for each variable*park combination and make
# labels for slope averages.
categorized_sens <- sens_by_var %>%
  mutate(variable_group = case_when(
    grepl(pattern = "Temp", x = variable_name) ~ "temperature",
    grepl(pattern = "flush", x = variable_name) ~ "flush",
    grepl(pattern = "DO", x = variable_name) ~ "DO",
    grepl(pattern = "BRK", x = variable_name) ~ "fish",
    grepl(pattern = "CCT", x = variable_name) ~ "fish",
    grepl(pattern = "RBT", x = variable_name) ~ "fish",
    grepl(pattern = "WCT", x = variable_name) ~ "fish",
    grepl(pattern = "SpCond", x = variable_name) ~ "sp_cond",
    grepl(pattern = "SWE", x = variable_name) ~ "SWE",
    grepl(pattern = "Depth", x = variable_name) ~ "physio",
    grepl(pattern = "lake", x = variable_name) ~ "physio",
    grepl(pattern = "Depth", x = variable_name) ~ "physio",
    grepl(pattern = "ice", x = variable_name) ~ "ice",
    TRUE ~ "water_quality_or_chem")) %>%
  group_by(variable_name, park) %>%
  summarize(mean_slope = mean(slope),
            sd_slope = sd(slope),
            variable_group = unique(variable_group)) %>%
  ungroup() %>%
  group_by(variable_group) %>%
  mutate(n_tile = ntile(x = mean_slope, n = 10),
         # Create a label denoting the data in the top decile of values
         label = if_else(condition = n_tile %in% c(1, 10),
                         true = variable_name, false = ""))


# Quick check: Which vars from bigjoin didn't end up getting Sens treatment?
unique(bigjoin$variable)[!(unique(bigjoin$variable) %in%
                             unique(categorized_sens$variable_name))]

# Export slopes (for reference if nothing else)
write.csv(x = categorized_sens %>% select(variable_name, park, mean_slope, sd_slope),
          file = "../data/analysis_outputs/sens_slopes_na_rm.csv",
          row.names = FALSE)


# Make a list of figures. Reduce number of labels if in a group with many
# slope points to plot. Each fig = mean slopes for one variable category.
fig_list <- map(.x = unique(categorized_sens$variable_group),
                .f = ~ 
                  
                  if(.x %in% c("DO", "temperature", "water_quality_or_chem",
                               "fish")){
                    
                    categorized_sens %>%
                      filter(variable_group == .x) %>%
                      ggplot(data = ., aes(x = park, y = mean_slope)) +
                      geom_point() +
                      facet_wrap(. ~ variable_group) +
                      geom_text_repel(aes(label = label))  
                    
                  } else{
                    categorized_sens %>%
                      filter(variable_group == .x) %>%
                      ggplot(data = ., aes(x = park, y = mean_slope)) +
                      geom_point() +
                      facet_wrap(. ~ variable_group) +
                      geom_text_repel(aes(label = variable_name))  
                    
                  })


# Arrange into single figure
all_sens_figs <- ggarrange(plotlist = fig_list)

# Export the figure
ggsave(filename = "../figures/sens_plots_labeled_averaged_slopes.png",
       plot = all_sens_figs, device = "png", width = 18, height = 12,
       units = "in")

sens_by_park <- sens_by_var %>%
  mutate(variable_group = case_when(
    grepl(pattern = "Temp", x = variable_name) ~ "temperature",
    grepl(pattern = "flush", x = variable_name) ~ "flush",
    grepl(pattern = "DO", x = variable_name) ~ "DO",
    grepl(pattern = "BRK", x = variable_name) ~ "fish",
    grepl(pattern = "CCT", x = variable_name) ~ "fish",
    grepl(pattern = "RBT", x = variable_name) ~ "fish",
    grepl(pattern = "WCT", x = variable_name) ~ "fish",
    grepl(pattern = "SpCond", x = variable_name) ~ "sp_cond",
    grepl(pattern = "SWE", x = variable_name) ~ "SWE",
    grepl(pattern = "Depth", x = variable_name) ~ "physio",
    grepl(pattern = "lake", x = variable_name) ~ "physio",
    grepl(pattern = "Depth", x = variable_name) ~ "physio",
    grepl(pattern = "ice", x = variable_name) ~ "ice",
    TRUE ~ "water_quality_or_chem")) %>%
  group_by(variable_name, park) %>%
  summarize(mean_slope = mean(slope),
            sd_slope = sd(slope),
            variable_group = unique(variable_group)) %>%
  ungroup() %>%
  group_by(park) %>%
  mutate(n_tile = ntile(x = mean_slope, n = 10),
         # Create a label denoting the data in the top decile of values
         label = if_else(condition = n_tile %in% c(1, 10),
                         true = variable_name, false = "")) %>%
  ggplot(data = ., aes(x = variable_group, y = mean_slope)) +
  geom_point() +
  facet_wrap(. ~ park) +
  geom_text_repel(aes(label = label)) +
  theme(axis.text.x = element_text(angle = 270))

# Export the figure
ggsave(filename = "../figures/sens_by_park_labeled_averaged_slopes.png",
       plot = sens_by_park, device = "png", width = 18, height = 8,
       units = "in")
