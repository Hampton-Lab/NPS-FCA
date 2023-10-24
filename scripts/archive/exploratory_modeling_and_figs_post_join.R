# This script follows up on bigjoin.R by taking its output and using it
# for modeling and plotting purposes

# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(nlme) # needed for mixed model
library(janitor)
library(ggrepel)


# 2. Load bigjoin ---------------------------------------------------------

bigjoin <- readRDS(file = file.path("..",
                                    "data",
                                    "analysis_outputs",
                                    "bigjoin.rds"))

# Add a column of shortened names for each lake for labels on plots
short_codes <- tribble(
  ~site_code, ~short_code,
  "LH14",     "MPA",
  "LH15",     "M15",
  "LN03",     "MAL",
  "LP19",     "M19",
  "LW32",     "MDW",
  "LZ35",     "MBL",
  "LS-07-01",     "NLB",
  "MA-03-01",     "NSI",
  "MC-03-01",     "NER",
  "MC-14-02",     "NEA",
  "MR-12-01",     "NBO",
  "SM-02-02",     "NTR",
  "106",     "OGL",
  "138",     "OFE",
  "263",     "OHE",
  "426",     "OCR",
  "498",     "OMI",
  "520",     "OLC",
  "623",     "OSU",
  "627",     "OSO",
  "Hoh",     "OHO"
)

bigjoin <- full_join(x = bigjoin, y = short_codes, by = c("site_code"))


# 3. Initial modeling of all variables ~ time + figs ----------------------


# 3a. The modeling approach -----------------------------------------------

# Fit simple change model for each variable with time at each lake

# Is this modeling approach accurate? I'm surprised to see 13k degrees of
# freedom for each interaction. Also, running the below commented out code
# chunk doesn't match the output for year:variableBRK in the original
# model summary.
# lme(value ~ year, random = ~1|park_site,
#     data = filter(bigjoin, variable == "BRK")) %>%
#   summary()

# If this modeling approach isn't working as intended, we could just include
# it as part of the figure generation for() loop for now. We are already
# subsetting the data in the for loop by variable, so it would be easy to
# just run a model on that data subset.

my_lme <- lme(value ~ year:variable - 1, random = ~ 1|park_site, bigjoin)

summary(my_lme)

# Save summary table
tTable <- summary(my_lme)$tTable %>% signif(digits = 3)

write.csv(file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "lme_tTable_univar.csv"),
          x = tTable)

unique_variables <- unique(bigjoin$variable)


# 3b. Generate plots ------------------------------------------------------

for (i in 1:length(unique_variables)){

  variable_i <- unique_variables[i]
  print(variable_i)
  data_subset <- dplyr::filter(bigjoin, variable == variable_i)

  facet_by_lake_name <- paste0("../figures/paneled_by_lake/by_lake_",
                               variable_i,
                               ".png")
  one_panel_name <- paste0("../figures/single_panel/single_panel_",
                           variable_i,
                           ".png")

  # Make a scatterplot faceted by lake
  facet_by_lake_plot <- ggplot(data = data_subset,
                               aes(x = year, y = value, color = park_code)) +
    geom_point() +
    ylab(paste0(data_subset$variable[1], " ", data_subset$units_label[1])) +
    scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
    facet_wrap(~park_site) +
    theme_bw() +
    theme(legend.position = "none")

  ggsave(plot = facet_by_lake_plot, width = 11, height = 6, units = "in",
         filename = facet_by_lake_name)

  # Make a scatterplot with all lakes in the same panel
  one_panel_plot <- ggplot(data = data_subset,
                           aes(x = year, y = value, color = park_site)) +
    geom_point() +
    ylab(paste0(data_subset$variable[1], " ", data_subset$units_label[1])) +
    scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
    theme_bw() +
    guides(col = guide_legend(ncol = 2))

  ggsave(plot = one_panel_plot, width = 9, height = 4, units = "in",
         filename = one_panel_name)
}


# 4. Cross variables to create paired comparisons -------------------------

temperature_vars <- grep(pattern = "temp", x = unique_variables,
                         ignore.case = TRUE)

non_temperature <- unique_variables[-temperature_vars]

# Some of these don't seem to exist in bigjoin...intentional?
temperature_names <- c("BottomTemp_mean_value_6", "SurfaceTemp_mean_value_6",
                       "MidTemp_mean_value_6", "AirTemp_mean_value_6",
                       "BottomTemp_mean_value", "SurfaceTemp_mean_value",
                       "MidTemp_mean_value", "AirTemp_mean_value",
                       "ProfTemp_top2m", "ProfTemp_below2m")

selected_variables <- c(non_temperature, temperature_names)

simplify_bigjoin <- bigjoin %>%
  select(park_code, park_site, year, variable, value, units_label) %>%
  as.data.frame()

simplify_bigjoin <- filter(.data = simplify_bigjoin,
                          variable %in% selected_variables)

bigjoin_cross <- inner_join(x = simplify_bigjoin, y = simplify_bigjoin,
                               by = c("park_code", "park_site", "year")) %>%
  mutate(variable_pair = paste0(variable.x, "_", variable.y),
         variable_pair_park_site = paste0(variable_pair, "_", park_site),
         variable_pair_park_code = paste0(variable_pair, "_", park_code),
         all = 1) %>% # I think this column can go...
  filter(variable.x != variable.y)

temp_vars_x <- grep("Temp", bigjoin_cross$variable.x)
temp_vars_y <- grep("Temp", bigjoin_cross$variable.y)

# Remove all temperature-to-temperature comparisons
cross_no_temp <- bigjoin_cross[-temp_vars_x[temp_vars_x %in% temp_vars_y], ]
cross_no_temp <- unique(cross_no_temp) # I don't think this is needed


# Fit model comparing variable pairs. This is a highly memory intensive model

# my_lme_pairs <-  lme(value.y ~ value.x:variable_pair-1, random = ~1|park_site,
#                      cross_no_temp)
# summary(my_lme_pairs)
# 
# tTable0 <- summary(my_lme_pairs)$tTable
# tTable <- signif(tTable0, digits = 3)
# write.csv(file = file.path("..",
#                            "data",
#                            "analysis_outputs",
#                            "tTable.csv"),
#           tTable)


# 5. Plot SWE and pH relationships ----------------------------------------

# Pull out snow water equiv and ph data
ph_swe <- cross_no_temp %>%
  filter(variable.x %in% c("SWE_May", "SWE_April") &
           variable.y %in% c("pH_top2m", "pH_below2m"))

plotdata <- ph_swe

ph_swe_pairs <- unique(ph_swe$variable_pair)

# Plots for pH and SWE relationships
for (i in 1:length(ph_swe_pairs)){

  var_pair_i <- ph_swe_pairs[i]
  print(var_pair_i)

  ph_swe_subset <- filter(.data = ph_swe, variable_pair == var_pair_i)

  facet_by_lake_name <- paste0("../figures/paneled_by_lake/by_lake_",
                               var_pair_i,
                               ".png")

  one_panel_name <- paste0("../figures/single_panel/single_panel_",
                           var_pair_i,
                           ".png")

  # Make a scatterplot faceted by lake
  facet_by_lake_plot <- ggplot(data = ph_swe_subset,
                               aes(x = value.x, y = value.y, color = park_site)) +
    geom_point(size = 1) +
    ylab(paste0(ph_swe_subset$variable.y, " ", ph_swe_subset$units_label.y)) +
    xlab(paste0(ph_swe_subset$variable.x[1], " ", ph_swe_subset$units_label.x[1])) +
    facet_wrap(~park_site) +
    theme_bw() +
    guides(col = guide_legend(ncol = 1))

  ggsave(plot = facet_by_lake_plot, width = 11, height = 6, units = "in",
         filename = facet_by_lake_name)

  # Make a scatterplot with all lakes in the same panel
  one_panel_plot <- ggplot(data = ph_swe_subset,
                           aes(x = value.x, y = value.y, color = park_site)) +
    geom_point(size = 1) +
    ylab("value") +
    xlab(paste0(ph_swe_subset$variable.x[1],
                " ",
                ph_swe_subset$units_label.x[1])) +
    ylab(paste0(ph_swe_subset$variable.y[1],
                " ",
                ph_swe_subset$units_label.y[1])) +
    scale_x_continuous(breaks = c(2004, 2006, 2008, 2010,
                                  2012, 2014, 2016, 2018)) +
    theme_bw() +
    guides(col = guide_legend(ncol = 2))

  ggsave(plot = one_panel_plot, width = 9, height = 4, units = "in",
         filename = one_panel_name)

}


# 6. Create multi-panel bivariate relationship plots ----------------------

# Plots showing multi-paneled bivariate relationships controlling for one
# axis at a time

unique_variables_cross <- unique(cross_no_temp$variable.x) # Take variables

for (i in 1:length(unique_variables_cross)){

  variable_i <- unique_variables_cross[i]
  print(variable_i)

  # Subset the data for variable pairs containing variable_i

  data_subset <- filter(cross_no_temp,
                        variable.x == variable_i)

  paneled_bivariate_name <- paste0("../figures/paneled_bivariate/panel_bivar_",
                                   variable_i,
                                   ".png")

  # A paneled bivariate plot where the x axis is the current variable_i
  paneled_bivariate_plot <- ggplot(data = data_subset,
                                   aes(x = value.x, y = value.y,
                                       color = park_site)) +
    geom_point(size = 1) +
    ylab("value") +
    xlab(paste0(data_subset$variable.x, " ", data_subset$units_label.x)) +
    facet_wrap(~paste0(data_subset$variable.y, " ", data_subset$units_label.y),
               scales = "free") +
    theme_bw() +
    guides(col = guide_legend(ncol = 1))

  ggsave(plot = paneled_bivariate_plot, width = 16, height = 10, units = "in",
         dpi = 150, filename = paneled_bivariate_name)
}


# 7. Plot & summarize 2011/2015 comparisons -------------------------------


# 7a. Plot selected vars summarized by year w/ facet ----------------------

variables_choose <- c("SWE_May",
                    "ice_out_doy",
                    "ProfTemp_below2m","ProfTemp_top2m",
                    "pH_below2m","pH_top2m",
                    "SpCond_below2m","SpCond_top2m",
                    "secchi_value_m",
                    "TDS",
                    "DO_below2m","DO_top2m",
                    "Total N","Total P",
                    "SO4",
                    "K","Mg","Na","Ca","Cl")

bigjoin_1115 <- filter(.data = bigjoin,
                       event_year %in% c(2011, 2015),
                       variable %in% variables_choose)

dataplot <- bigjoin_1115

paneled_twoyear_plot <- ggplot(data = dataplot,
                               aes(x = as.factor(event_year), y = value,
                                   color = park_site)) +
  geom_point(size = 1) +
  xlab("year") +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  guides(col = guide_legend(ncol = 1))

paneled_twoyear_plot

ggsave(plot = paneled_twoyear_plot, width = 10, height = 8, units = "in",
       filename = "../figures/twoyear.png")


# 7b. Plot all vars ~ year (2011/2015) ------------------------------------

# A function to make and export plots with variable ~ year
two_yr_plot_fun <- function(variable_name) {
  
  filtered_data <- filter(bigjoin, variable == variable_name,
                          year %in% c(2011, 2015))
  
  two_yr_plot <-  ggplot(data = filtered_data,
                         aes(x = year, y = value,
                             color = park_code, group = park_site)) +
    geom_point(size = 1) +
    geom_text_repel(
      aes(label = park_site),
      data = . %>% filter(year == 2015,
                          variable == variable_name),
      nudge_x = 1.15,
      nudge_y = 0, 
      segment.size = 0.5,
      segment.color = "gray",
      direction = "y",
      hjust = 0,
      size = 3) +
    geom_line() +
    ylab(paste0(filtered_data$variable[1], " ", filtered_data$units_label[1])) +
    scale_x_continuous(breaks = c(2011, 2015),
                       limits = c(2011, 2017)) +
    theme_bw()
  
  ggsave(plot = two_yr_plot, width = 9, height = 4, units = "in",
         filename = paste0("../figures/two_yr/two_yr_", variable_name, ".png"))
}

# Run the make/export function for each variable in bigjoin
map(.x = unique(bigjoin$variable),
    .f = ~ two_yr_plot_fun(variable_name = .x))


# 7c. Plot all vars ~ specific vars (2011/2015) ---------------------------

# A function to allow plotting of any two vars from bigjoin$variables
# against each other for the years 2011 & 2015 only
two_yr_plot_SWE_fun <- function(y_variable_name, x_variable_name) {

  # Subset bigjoin to vars & time of interest
  filtered_data <- dplyr::filter(.data = bigjoin,
                          variable %in% c(y_variable_name, x_variable_name),
                          year %in% c(2011, 2015)) %>%
    # We don't need most of the remaining columns
    select(park_code, site_code, short_code, year, variable, value)
  
  # Separate into different dfs using variable col, so each variable can be its
  # own column for plotting purposes. Makes a list of dfs.
  separated_dfs <-  map(.x = c(unique(filtered_data$variable)),
                        .f = ~ filter(.data = filtered_data, variable == .x))
  
  # Spread each df in above list so that variable is its own column, not stored
  # under generic "variable" key column. Note that spread() has been replaced
  # with pivot_wider(), which allows to average rows that aren't unique in ID.
  separated_dfs_wide <- map(.x = separated_dfs,
                            .f = ~ pivot_wider(data = .x,
                                               names_from = variable,
                                               values_from =  value,
                                               values_fn = list(value = mean)))
  
  # Join the two dfs into one with both vars as their own columns
  joined_dfs <- full_join(x = separated_dfs_wide[[1]],
                          y = separated_dfs_wide[[2]],
                          by = c("park_code", "site_code",
                                 "short_code", "year")) %>%
    clean_names()
  
  # Construct variable-unit axis labels
  x_label <- paste0(x_variable_name,
                    " ",
                    unique(filter(filtered_data,
                                  variable == x_variable_name)$units_label))
  y_label <- paste0(y_variable_name,
                    " ",
                    unique(filter(filtered_data,
                                  variable == y_variable_name)$units_label))  
  
  # Create the plot with y_var ~ x_var
  two_yr_SWE_plot <-  ggplot(data = joined_dfs,
                             aes_string(x = make_clean_names(x_variable_name),
                                        y = make_clean_names(y_variable_name),
                                        color = "park_code",
                                        group = "short_code")) +
    geom_point(size = 1) +
    geom_text_repel(
      aes(label = short_code),
      data = . %>% filter(year == 2011),
      segment.size = 0.5,
      segment.color = "gray",
      size = 3) +
    geom_line() +
    xlab(x_label) +
    ylab(y_label) +
    theme_bw() +
    theme(panel.background = element_blank(),
          legend.position = "none")
  
  ggsave(plot = two_yr_SWE_plot, width = 4, height = 4, units = "in",
         filename = paste0("../figures/two_yr/two_yr_",
                           y_variable_name, "_vs_",
                           x_variable_name, ".png"))
  
}

# Run the make/export function for each variable in bigjoin
vars_of_2011_2015 <- bigjoin %>%
  dplyr::filter(year %in% c(2011, 2015),
                variable != "SWE_May") %>%
  select(variable) %>%
  unique() %>%
  # Make it a vector
  .$variable

# Plot variable combos vs SWE_May
map(.x = vars_of_2011_2015,
    .f = ~ two_yr_plot_SWE_fun(x_variable_name = "SWE_May",
                               y_variable_name = .x))

# Plot Total P ~ Total N
map(.x = "Total P",
    .f = ~ two_yr_plot_SWE_fun(x_variable_name = "Total N",
                               y_variable_name = .x))


# Ice out ~ SWE plot 2011/2015
iceout_swe_scatter <- bigjoin %>%
  filter(variable %in% c("ice_out_doy", "SWE_May"),
         year %in% c(2011, 2015)) %>%
  select(park_code, site_code, short_code, year, variable, value) %>%
  group_split(.tbl = ., variable) %>%
  map(.x = .,
      .f = ~ spread(data = .x,
                    key = variable,
                    value = value)) %>%
  inner_join(x = .[[1]], y = .[[2]],
             by = c("park_code", "site_code", "short_code", "year")) %>%
  ggplot() +
  geom_point(aes(x = SWE_May, y = ice_out_doy)) +
  theme_bw() +
  ylab("Ice out (day of year)") +
  xlab("SWE (May)")

ggsave(filename = "../figures/two_yr/ice_out_swe_no_lines.png",
       plot = iceout_swe_scatter, device = "png",
       width = 7, height = 4)

# Total N ~ Total P plot 2011/2015
N_P_scatter <- bigjoin %>%
  filter(variable %in% c("Total N", "Total P"),
         year %in% c(2011, 2015)) %>%
  select(park_code, site_code, short_code, year, variable, value) %>%
  group_split(.tbl = ., variable) %>%
  map(.x = .,
      .f = ~ spread(data = .x,
                    key = variable,
                    value = value)) %>%
  inner_join(x = .[[1]], y = .[[2]],
             by = c("park_code", "site_code", "short_code", "year")) %>%
  clean_names() %>%
  ggplot() +
  geom_point(aes(x = total_p, y = total_n)) +
  theme_bw() +
  ylab("Total N") +
  xlab("Total P")

ggsave(filename = "../figures/two_yr/total_N_total_P_no_lines.png",
       plot = N_P_scatter, device = "png",
       width = 7, height = 4)


# 7e. Summary statistics --------------------------------------------------

snowice_1115 <- filter(.data = bigjoin,
         event_year %in% c(2011, 2015),
         variable %in% c("SWE_May", "ice_out_doy"))

swe_unique <- snowice_1115 %>%
  filter(variable == "SWE_May") %>%
  select(park_code, variable, event_year, value) %>%
  unique()

swe_unique %>%
  group_by(event_year) %>%
  dplyr::summarize(SWEmean = mean(value,na.rm = TRUE),
                   SWEmin = min(value, na.rm = TRUE), 
                   SWEmax = max(value, na.rm = TRUE)) %>%
  as.data.frame()

snowice_1115 %>%
  filter(variable == "SWE_May") %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year) %>%
  dplyr::summarize(SWEmean = 2.54 * mean(value, na.rm = TRUE),
                   SWEmin = 2.54 * min(value, na.rm = TRUE), 
                   SWEmax = 2.54 * max(value, na.rm = TRUE),
                   SWEmedian = 2.54 * median(value, na.rm = TRUE))

bigjoin %>% filter(event_year %in% c(2011, 2015),
                   variable %in% c("SurfTemp", "MidTemp", "BotTemp")) %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year, variable) %>%
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE), 
                   median = median(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE))

bigjoin %>%
  filter(event_year %in% c(2011, 2015),
         variable == "ProfTemp_top2m") %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year, variable) %>%
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE), 
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   count = sum(!is.na(value)))

bigjoin %>%
  filter(event_year %in% c(2011, 2015),
         variable == "ProfTemp_below2m") %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year, variable) %>%
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE), 
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   count = sum(!is.na(value)))

bigjoin %>% filter(event_year %in% c(2011, 2015),
                   variable %in% c("SurfTemp_8", "MidTemp_8", "BotTemp_8")) %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year, variable) %>%
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE), 
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   count = sum(!is.na(value)))

bigjoin %>% filter(event_year %in% c(2011, 2015),
                   variable %in% c("secchi_value_m", "pH_top2m", "DO_top2m",
                                   "Total N", "SpCond_top2m", "SpCond_below2m",
                                   "Chlorophyll")) %>%
  select(park_code, variable, event_year, value) %>%
  group_by(event_year, variable) %>%
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE), 
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   count = sum(!is.na(value)))

