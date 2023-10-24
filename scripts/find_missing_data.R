# This script works with the "bigjoin" dataset to document which data are
# currently missing.

library(tidyverse)

# Bring in bigjoin data
bigjoin <- readRDS(file = file.path("..",
                                    "data",
                                    "analysis_outputs",
                                    "bigjoin.rds"))

# Make a data frame of the unique park, lake, site code, year combinations
lake_year_combos <- bigjoin %>%
  select(park_code, Lake, site_code, event_year) %>%
  arrange(park_code, site_code, event_year) %>%
  unique() %>%
  as.data.frame()

# Export this
write.csv(file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "lake_year_combos.csv"),
          x = lake_year_combos, row.names = F)


# Put together a data frame showing the existing and missing data:

data_structure <- expand.grid(park_site = sort(unique(bigjoin$park_site)),
                              event_year = sort(unique(bigjoin$event_year)),
                              variable = sort(unique(bigjoin$variable)))

# Join, document missing values
expose_missing <- full_join(x = data_structure, y = bigjoin,
                            by = c("park_site", "event_year", "variable")) %>%
  mutate(has_data = 1,
         has_data = case_when(
           is.na(value) ~ 0,
           TRUE ~ has_data))

# Break down data presence by variable, location, time
data_presence <- expose_missing %>%
  group_by(park_site, event_year, variable) %>%
  dplyr::summarize(has_data = max(has_data)) %>%
  arrange(park_site, variable, event_year)

data_presence_wide <- data_presence %>%
  spread(key = variable, value = has_data, fill = 0)

write.csv(file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "data1_missing0_long.csv"),
          x = data_presence,
          row.names = FALSE, quote = FALSE)

write.csv(file = file.path("..",
                           "data",
                           "analysis_outputs",
                           "data1_missing0_wide.csv"),
          x = data_presence_wide,
          row.names = FALSE, quote = FALSE)