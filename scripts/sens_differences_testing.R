# The goal for this script is to check differences in the outputs of the Sen's
# script by removing NAs before running the measure or not.


library(tidyverse)

na_rm_slopes <- read.csv(file = "../data/analysis_outputs/sens_slopes_na_rm.csv")
na_incl_slopes <- read.csv(file = "../data/analysis_outputs/sens_slopes_na_incl.csv")

joined_measures <- full_join(x = na_rm_slopes,
                             y = na_incl_slopes,
                             by = c("variable_name", "park"))

joined_measures %>%
  select(park, variable_name, slope.x, slope.y) %>%
  mutate(slope_diff = slope.x - slope.y,
         slope_avg = (slope.x + slope.y) / 2,
         (slope_diff / slope_avg) * 100)
         #slope_percent_diff = slope)
